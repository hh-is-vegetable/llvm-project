// WebAssemblyInstrMemory.td-WebAssembly Memory codegen support -*- tablegen -*-
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// WebAssembly Memory operand code-gen constructs.
///
//===----------------------------------------------------------------------===//

// TODO:
//  - WebAssemblyTargetLowering having to do with atomics
//  - Each has optional alignment.
//  - Addr64 

// WebAssembly has i8/i16/i32/i64/f32/f64/memref memory types, but doesn't have i8/i16
// local types. These memory-only types instead zero- or sign-extend into local
// types when loading, and truncate when storing.

// WebAssembly constant offsets are performed as unsigned with infinite
// precision, so we need to check for NoUnsignedWrap so that we don't fold an
// offset for an add that needs wrapping.

def SDTMrefBinary : SDTypeProfile<1, 2, [    
  SDTCisVT<0, memref>, SDTCisVT<1, memref>, SDTCisInt<2>
]>;

def SDTMrefAlloc : SDTypeProfile<1, 3, [
  SDTCisVT<0, memref>, 
  SDTCisInt<1>, SDTCisInt<2>, SDTCisInt<3>
]>;

def SDTMrefFree : SDTypeProfile<0, 1, [
  SDTCisVT<0, memref>
]>;

def SDTMrefNarrow : SDTypeProfile<1, 3, [
  SDTCisVT<0, memref>,
  SDTCisVT<1, memref>, SDTCisInt<2>, SDTCisInt<3>
]>;

def SDTMrefField : SDTypeProfile<1, 2, [
  SDTCisInt<0>,
  SDTCisInt<1>, SDTCisVT<2, memref>
]>;

def SDTMrefNull : SDTypeProfile<1, 0, [
  SDTCisVT<0, memref>
]>;


def SDT_WebAssemblyWrapper_Memref    : SDTypeProfile<1, 1, [SDTCisSameAs<0, 1>,
                                                     SDTCisPtrTy<0>]>;
def WebAssemblyWrapper_Memref  : SDNode<"WebAssemblyISD::Wrapper_Memref",
                                 SDT_WebAssemblyWrapper_Memref>;

// mrefadd
def mrefadd : SDNode<"ISD::WASM_MEMREF_ADD", SDTMrefBinary, []>;

// mrefand
def mrefand : SDNode<"ISD::WASM_MEMREF_AND", SDTMrefBinary, []>;

def mrefalloc : SDNode<"ISD::WASM_MEMREF_ALLOC", SDTMrefAlloc, []>;

def mreffree : SDNode<"ISD::WASM_MEMREF_FREE", SDTMrefFree, []>;

def mrefnarrow : SDNode<"ISD::WASM_MEMREF_NARROW", SDTMrefNarrow, []>;

def mreffield : SDNode<"ISD::WASM_MEMREF_FIELD", SDTMrefField, []>;

def mrefnull : SDNode<"ISD::WASM_MEMREF_NULL", SDTMrefNull, []>;

// this is for memref plus i32
def regMemrefPlusImm : PatFrag<(ops node:$mref, node:$off),
                               (mrefadd node:$mref, node:$off)>;

// i32 immediate
def SDTInt32Leaf : SDTypeProfile<1, 0, [SDTCisVT<0, i32>]>;
def imm_i32_target   : SDNode<"ISD::TargetConstant"  , SDTInt32Leaf , [], "ConstantSDNode">;
def imm_i32          : SDNode<"ISD::Constant"  , SDTInt32Leaf , [], "ConstantSDNode">;
// i64 immediate
def SDTInt64Leaf : SDTypeProfile<1, 0, [SDTCisVT<0, i64>]>;
def imm_i64      : SDNode<"ISD::TargetConstant"  , SDTInt64Leaf , [], "ConstantSDNode">;

// We don't need a regPlusES because external symbols never have constant
// offsets folded into them, so we can just use add.

// Defines atomic and non-atomic loads, regular and extending.
multiclass WebAssemblyMSLoad<WebAssemblyRegClass rc, string Name, int Opcode,
                           list<Predicate> reqs = []> {
  let mayLoad = 1, UseNamedOperandTable = 1 in {
  defm "_A32": I<(outs rc:$dst),
                 (ins P2Align:$p2align, offset32_op:$off, MEMREF:$mref),
                 (outs), (ins P2Align:$p2align, offset32_op:$off),
                 [], !strconcat(Name, "\t$dst, ${off}(${mref})${p2align}"),
                 !strconcat(Name, "\t${off}${p2align}"), Opcode, "false">,
               Requires<reqs>;
  defm "_A64": I<(outs rc:$dst),
                 (ins P2Align:$p2align, offset64_op:$off, MEMREF:$mref),
                 (outs), (ins P2Align:$p2align, offset64_op:$off),
                 [], !strconcat(Name, "\t$dst, ${off}(${mref})${p2align}"),
                 !strconcat(Name, "\t${off}${p2align}"), Opcode, "false">,
               Requires<reqs>;
  }
}

// msload basic load
defm MSLOAD_I32 : WebAssemblyMSLoad<I32, "i32.msload", 0xe0, []>;
defm MSLOAD_I64 : WebAssemblyMSLoad<I64, "i64.msload", 0xe1, []>;
defm MSLOAD_F32 : WebAssemblyMSLoad<F32, "f32.msload", 0xe2, []>;
defm MSLOAD_F64 : WebAssemblyMSLoad<F64, "f64.msload", 0xe3, []>;
defm MSLOAD_MEMREF : WebAssemblyMSLoad<MEMREF, "memref.msload", 0xe4, []>;

// Select loads with no constant offset.
multiclass MSLoadPatNoOffset<ValueType ty, SDPatternOperator kind, string inst> {
  def : Pat<(ty (kind MEMREF:$mref)), (!cast<NI>(inst # "_A32") 0, 0, MEMREF:$mref)>,
        Requires<[HasAddr32]>;
  def : Pat<(ty (kind MEMREF:$mref)), (!cast<NI>(inst # "_A64") 0, 0, MEMREF:$mref)>,
        Requires<[HasAddr64]>;
}

defm : MSLoadPatNoOffset<i32, load, "MSLOAD_I32">;
defm : MSLoadPatNoOffset<i64, load, "MSLOAD_I64">;
defm : MSLoadPatNoOffset<f32, load, "MSLOAD_F32">;
defm : MSLoadPatNoOffset<f64, load, "MSLOAD_F64">;
defm : MSLoadPatNoOffset<memref, load, "MSLOAD_MEMREF">;

// Select loads with a constant offset.

// Pattern with address + immediate offset
multiclass MSLoadPatImmOff<ValueType ty, SDPatternOperator kind, PatFrag operand,
                         string inst> {
  def : Pat<(ty (kind (operand MEMREF:$mref, imm_i32:$off))),
            (!cast<NI>(inst # "_A32") 0, imm:$off, MEMREF:$mref)>,
        Requires<[HasAddr32]>;
  def : Pat<(ty (kind (operand MEMREF:$mref, imm_i64:$off))),
            (!cast<NI>(inst # "_A64") 0, imm:$off, MEMREF:$mref)>,
        Requires<[HasAddr64]>;
}

defm : MSLoadPatImmOff<i32, load, regMemrefPlusImm, "MSLOAD_I32">;
defm : MSLoadPatImmOff<i64, load, regMemrefPlusImm, "MSLOAD_I64">;
defm : MSLoadPatImmOff<f32, load, regMemrefPlusImm, "MSLOAD_F32">;
defm : MSLoadPatImmOff<f64, load, regMemrefPlusImm, "MSLOAD_F64">;
defm : MSLoadPatImmOff<memref, load, regMemrefPlusImm, "MSLOAD_MEMREF">;

// no need CONST_MEMREF
// Select loads with just a constant offset.
// multiclass MSLoadPatOffsetOnly<ValueType ty, SDPatternOperator kind, string inst> {
//   def : Pat<(ty (kind imm:$off)),
//             (!cast<NI>(inst # "_A32") 0, imm:$off, (CONST_I32 0))>,
//         Requires<[HasAddr32]>;
//   def : Pat<(ty (kind imm:$off)),
//             (!cast<NI>(inst # "_A64") 0, imm:$off, (CONST_I64 0))>,
//         Requires<[HasAddr64]>;
// }

// no need CONST_MEMREF
// multiclass LoadPatGlobalAddrOffOnly<ValueType ty, SDPatternOperator kind, string inst> {
//   def : Pat<(ty (kind (WebAssemblyWrapper tglobaladdr:$off))),
//             (!cast<NI>(inst # "_A32") 0, tglobaladdr:$off, (CONST_I32 0))>,
//         Requires<[IsNotPIC, HasAddr32]>;
//   def : Pat<(ty (kind (WebAssemblyWrapper tglobaladdr:$off))),
//             (!cast<NI>(inst # "_A64") 0, tglobaladdr:$off, (CONST_I64 0))>,
//         Requires<[IsNotPIC, HasAddr64]>;
// }

// for global. so normal global will be replaced by load instead of global_get
multiclass LoadPatGlobalAddr<ValueType ty, SDPatternOperator kind, string inst> {
  def : Pat<(ty (kind (WebAssemblyWrapper_Memref tglobaladdr:$mref))),
            (!cast<NI>(inst # "_A32") 0, 0, tglobaladdr:$mref)>,
        Requires<[IsNotPIC, HasAddr32]>;
}
defm : LoadPatGlobalAddr<i32, load, "MSLOAD_I32">;
defm : LoadPatGlobalAddr<i64, load, "MSLOAD_I64">;
defm : LoadPatGlobalAddr<f32, load, "MSLOAD_F32">;
defm : LoadPatGlobalAddr<f64, load, "MSLOAD_F64">;
defm : LoadPatGlobalAddr<memref, load, "MSLOAD_MEMREF">;

// Extending msload
defm MSLOAD8_S_I32 : WebAssemblyMSLoad<I32, "i32.msload8_s", 0xe5, []>;
defm MSLOAD8_U_I32 : WebAssemblyMSLoad<I32, "i32.msload8_u", 0xe6, []>;
defm MSLOAD16_S_I32 : WebAssemblyMSLoad<I32, "i32.msload16_s", 0xe7, []>;
defm MSLOAD16_U_I32 : WebAssemblyMSLoad<I32, "i32.msload16_u", 0xe8, []>;
defm MSLOAD8_S_I64 : WebAssemblyMSLoad<I64, "i64.msload8_s", 0xe9, []>;
defm MSLOAD8_U_I64 : WebAssemblyMSLoad<I64, "i64.msload8_u", 0xea, []>;
defm MSLOAD16_S_I64 : WebAssemblyMSLoad<I64, "i64.msload16_s", 0xeb, []>;
defm MSLOAD16_U_I64 : WebAssemblyMSLoad<I64, "i64.msload16_u", 0xec, []>;
defm MSLOAD32_S_I64 : WebAssemblyMSLoad<I64, "i64.msload32_s", 0xed, []>;
defm MSLOAD32_U_I64 : WebAssemblyMSLoad<I64, "i64.msload32_u", 0xee, []>;

// Select extending msloads with no constant offset.
defm : MSLoadPatNoOffset<i32, sextloadi8, "MSLOAD8_S_I32">;
defm : MSLoadPatNoOffset<i32, zextloadi8, "MSLOAD8_U_I32">;
defm : MSLoadPatNoOffset<i32, sextloadi16, "MSLOAD16_S_I32">;
defm : MSLoadPatNoOffset<i32, zextloadi16, "MSLOAD16_U_I32">;
defm : MSLoadPatNoOffset<i64, sextloadi8, "MSLOAD8_S_I64">;
defm : MSLoadPatNoOffset<i64, zextloadi8, "MSLOAD8_U_I64">;
defm : MSLoadPatNoOffset<i64, sextloadi16, "MSLOAD16_S_I64">;
defm : MSLoadPatNoOffset<i64, zextloadi16, "MSLOAD16_U_I64">;
defm : MSLoadPatNoOffset<i64, sextloadi32, "MSLOAD32_S_I64">;
defm : MSLoadPatNoOffset<i64, zextloadi32, "MSLOAD32_U_I64">;

// Select extending msloads with a constant offset.
defm : MSLoadPatImmOff<i32, sextloadi8, regMemrefPlusImm, "MSLOAD8_S_I32">;
defm : MSLoadPatImmOff<i32, zextloadi8, regMemrefPlusImm, "MSLOAD8_U_I32">;
defm : MSLoadPatImmOff<i32, sextloadi16, regMemrefPlusImm, "MSLOAD16_S_I32">;
defm : MSLoadPatImmOff<i32, zextloadi16, regMemrefPlusImm, "MSLOAD16_U_I32">;
defm : MSLoadPatImmOff<i64, sextloadi8, regMemrefPlusImm, "MSLOAD8_S_I64">;
defm : MSLoadPatImmOff<i64, zextloadi8, regMemrefPlusImm, "MSLOAD8_U_I64">;
defm : MSLoadPatImmOff<i64, sextloadi16, regMemrefPlusImm, "MSLOAD16_S_I64">;
defm : MSLoadPatImmOff<i64, zextloadi16, regMemrefPlusImm, "MSLOAD16_U_I64">;
defm : MSLoadPatImmOff<i64, sextloadi32, regMemrefPlusImm, "MSLOAD32_S_I64">;
defm : MSLoadPatImmOff<i64, zextloadi32, regMemrefPlusImm, "MSLOAD32_U_I64">;

// Resolve "don't care" extending loads to zero-extending loads. This is
// somewhat arbitrary, but zero-extending is conceptually simpler.

// Select "don't care" extending msloads with no constant offset.
defm : MSLoadPatNoOffset<i32, extloadi8, "MSLOAD8_U_I32">;
defm : MSLoadPatNoOffset<i32, extloadi16, "MSLOAD16_U_I32">;
defm : MSLoadPatNoOffset<i64, extloadi8, "MSLOAD8_U_I64">;
defm : MSLoadPatNoOffset<i64, extloadi16, "MSLOAD16_U_I64">;
defm : MSLoadPatNoOffset<i64, extloadi32, "MSLOAD32_U_I64">;

// Select "don't care" extending loads with a constant offset.
defm : MSLoadPatImmOff<i32, extloadi8, regMemrefPlusImm, "MSLOAD8_U_I32">;
defm : MSLoadPatImmOff<i32, extloadi16, regMemrefPlusImm, "MSLOAD16_U_I32">;
defm : MSLoadPatImmOff<i64, extloadi8, regMemrefPlusImm, "MSLOAD8_U_I64">;
defm : MSLoadPatImmOff<i64, extloadi16, regMemrefPlusImm, "MSLOAD16_U_I64">;
defm : MSLoadPatImmOff<i64, extloadi32, regMemrefPlusImm, "MSLOAD32_U_I64">;

// for global
defm : LoadPatGlobalAddr<i32, sextloadi8, "MSLOAD8_S_I32">;
defm : LoadPatGlobalAddr<i32, zextloadi8, "MSLOAD8_U_I32">;
defm : LoadPatGlobalAddr<i32, sextloadi16, "MSLOAD16_S_I32">;
defm : LoadPatGlobalAddr<i32, zextloadi16, "MSLOAD16_U_I32">;
defm : LoadPatGlobalAddr<i64, sextloadi8, "MSLOAD8_S_I64">;
defm : LoadPatGlobalAddr<i64, zextloadi8, "MSLOAD8_U_I64">;
defm : LoadPatGlobalAddr<i64, sextloadi16, "MSLOAD16_S_I64">;
defm : LoadPatGlobalAddr<i64, zextloadi16, "MSLOAD16_U_I64">;
defm : LoadPatGlobalAddr<i64, sextloadi32, "MSLOAD32_S_I64">;
defm : LoadPatGlobalAddr<i64, zextloadi32, "MSLOAD32_U_I64">;

// msstore
// Defines atomic and non-atomic stores, regular and truncating
// Basic store.
// Note: WebAssembly inverts SelectionDAG's usual operand order.
multiclass WebAssemblyMSStore<WebAssemblyRegClass rc, string Name, int Opcode,
                            list<Predicate> reqs = []> {
  let mayStore = 1, UseNamedOperandTable = 1 in
  defm "_A32" : I<(outs),
                  (ins P2Align:$p2align, offset32_op:$off, MEMREF:$mref, rc:$val),
                  (outs),
                  (ins P2Align:$p2align, offset32_op:$off), [],
                  !strconcat(Name, "\t${off}(${mref})${p2align}, $val"),
                  !strconcat(Name, "\t${off}${p2align}"), Opcode, "false">,
                Requires<reqs>;
  defm "_A64" : I<(outs),
                  (ins P2Align:$p2align, offset64_op:$off, MEMREF:$mref, rc:$val),
                  (outs),
                  (ins P2Align:$p2align, offset64_op:$off), [],
                  !strconcat(Name, "\t${off}(${mref})${p2align}, $val"),
                  !strconcat(Name, "\t${off}${p2align}"), Opcode, "false">,
                Requires<reqs>;
}

defm MSSTORE_I32  : WebAssemblyMSStore<I32, "i32.msstore", 0xf0>;
defm MSSTORE_I64  : WebAssemblyMSStore<I64, "i64.msstore", 0xf1>;
defm MSSTORE_F32  : WebAssemblyMSStore<F32, "f32.msstore", 0xf2>;
defm MSSTORE_F64  : WebAssemblyMSStore<F64, "f64.msstore", 0xf3>;
defm MSSTORE_MEMREF : WebAssemblyMSStore<MEMREF, "memref.msstore", 0xf4>;

// Select stores with no constant offset.
multiclass MSStorePatNoOffset<ValueType ty, PatFrag node, string inst> {
  def : Pat<(node ty:$val, MEMREF:$mref),
            (!cast<NI>(inst # "_A32") 0, 0, MEMREF:$mref, ty:$val)>,
        Requires<[HasAddr32]>;
  def : Pat<(node ty:$val, MEMREF:$mref),
            (!cast<NI>(inst # "_A64") 0, 0, MEMREF:$mref, ty:$val)>,
        Requires<[HasAddr64]>;
}

defm : MSStorePatNoOffset<i32, store, "MSSTORE_I32">;
defm : MSStorePatNoOffset<i64, store, "MSSTORE_I64">;
defm : MSStorePatNoOffset<f32, store, "MSSTORE_F32">;
defm : MSStorePatNoOffset<f64, store, "MSSTORE_F64">;
defm : MSStorePatNoOffset<memref, store, "MSSTORE_MEMREF">;

// Select stores with a constant offset.
multiclass MSStorePatImmOff<ValueType ty, PatFrag kind, PatFrag operand,
                          string inst> {
  def : Pat<(kind ty:$val, (operand MEMREF:$mref, imm_i32:$off)),
            (!cast<NI>(inst # "_A32") 0, imm:$off, MEMREF:$mref, ty:$val)>,
        Requires<[HasAddr32]>;
  def : Pat<(kind ty:$val, (operand MEMREF:$mref, imm_i64:$off)),
            (!cast<NI>(inst # "_A64") 0, imm:$off, MEMREF:$mref, ty:$val)>,
        Requires<[HasAddr64]>;
}

defm : MSStorePatImmOff<i32, store, regMemrefPlusImm, "MSSTORE_I32">;
defm : MSStorePatImmOff<i64, store, regMemrefPlusImm, "MSSTORE_I64">;
defm : MSStorePatImmOff<f32, store, regMemrefPlusImm, "MSSTORE_F32">;
defm : MSStorePatImmOff<f64, store, regMemrefPlusImm, "MSSTORE_F64">;
defm : MSStorePatImmOff<memref, store, regMemrefPlusImm, "MSSTORE_MEMREF">;

def : Pat<(store (WebAssemblyWrapper_Memref tglobaladdr:$val), (regMemrefPlusImm MEMREF:$mref, imm_i32:$off)),
           (MSSTORE_MEMREF_A32 0, imm:$off, MEMREF:$mref, tglobaladdr:$val)>,
        Requires<[HasAddr32]>;

// no need CONST_MEMREF
// Select stores with just a constant offset.
// multiclass MSStorePatOffsetOnly<ValueType ty, PatFrag kind, string inst> {
//   def : Pat<(kind ty:$val, imm:$off),
//             (!cast<NI>(inst # "_A32") 0, imm:$off, (CONST_I32 0), ty:$val)>,
//         Requires<[HasAddr32]>;
//   def : Pat<(kind ty:$val, imm:$off),
//             (!cast<NI>(inst # "_A64") 0, imm:$off, (CONST_I64 0), ty:$val)>,
//         Requires<[HasAddr64]>;
// }

// multiclass StorePatGlobalAddrOffOnly<ValueType ty, PatFrag kind, string inst> {
//   def : Pat<(kind ty:$val, (WebAssemblyWrapper tglobaladdr:$off)),
//             (!cast<NI>(inst # "_A32") 0, tglobaladdr:$off, (CONST_I32 0),
//              ty:$val)>,
//         Requires<[IsNotPIC, HasAddr32]>;
//   def : Pat<(kind ty:$val, (WebAssemblyWrapper tglobaladdr:$off)),
//             (!cast<NI>(inst # "_A64") 0, tglobaladdr:$off, (CONST_I64 0),
//              ty:$val)>,
//         Requires<[IsNotPIC, HasAddr64]>;
// }

// for global
multiclass StorePatGlobalAddr<ValueType ty, PatFrag kind, string inst> {
  def : Pat<(kind ty:$val, (WebAssemblyWrapper_Memref tglobaladdr:$mref)),
            (!cast<NI>(inst # "_A32") 0, 0, tglobaladdr:$mref,
             ty:$val)>,
        Requires<[HasAddr32]>;
}

defm : StorePatGlobalAddr<i32, store, "MSSTORE_I32">;
defm : StorePatGlobalAddr<i64, store, "MSSTORE_I64">;
defm : StorePatGlobalAddr<f32, store, "MSSTORE_F32">;
defm : StorePatGlobalAddr<f64, store, "MSSTORE_F64">;
defm : StorePatGlobalAddr<memref, store, "MSSTORE_MEMREF">;

def : Pat<(store (WebAssemblyWrapper_Memref tglobaladdr:$val), memref:$mref),
          (MSSTORE_MEMREF_A32 0, 0, memref:$mref, tglobaladdr:$val)>,
        Requires<[HasAddr32]>;
def : Pat<(store (WebAssemblyWrapper_Memref tglobaladdr:$val), (WebAssemblyWrapper_Memref tglobaladdr:$mref)),
          (MSSTORE_MEMREF_A32 0, 0, tglobaladdr:$mref, tglobaladdr:$val)>,
        Requires<[HasAddr32]>;

// Truncating store.
defm MSSTORE8_I32 : WebAssemblyMSStore<I32, "i32.msstore8", 0xf5>;
defm MSSTORE16_I32 : WebAssemblyMSStore<I32, "i32.msstore16", 0xf6>;
defm MSSTORE8_I64 : WebAssemblyMSStore<I64, "i64.msstore8", 0xf7>;
defm MSSTORE16_I64 : WebAssemblyMSStore<I64, "i64.msstore16", 0xf8>;
defm MSSTORE32_I64 : WebAssemblyMSStore<I64, "i64.msstore32", 0xf9>;

// Select truncating stores with no constant offset.
defm : MSStorePatNoOffset<i32, truncstorei8, "MSSTORE8_I32">;
defm : MSStorePatNoOffset<i32, truncstorei16, "MSSTORE16_I32">;
defm : MSStorePatNoOffset<i64, truncstorei8, "MSSTORE8_I64">;
defm : MSStorePatNoOffset<i64, truncstorei16, "MSSTORE16_I64">;
defm : MSStorePatNoOffset<i64, truncstorei32, "MSSTORE32_I64">;

// Select truncating stores with a constant offset.
defm : MSStorePatImmOff<i32, truncstorei8, regMemrefPlusImm, "MSSTORE8_I32">;
defm : MSStorePatImmOff<i32, truncstorei16, regMemrefPlusImm, "MSSTORE16_I32">;
defm : MSStorePatImmOff<i64, truncstorei8, regMemrefPlusImm, "MSSTORE8_I64">;
defm : MSStorePatImmOff<i64, truncstorei16, regMemrefPlusImm, "MSSTORE16_I64">;
defm : MSStorePatImmOff<i64, truncstorei32, regMemrefPlusImm, "MSSTORE32_I64">;

// for global
defm : StorePatGlobalAddr<i32, truncstorei8, "MSSTORE8_I32">;
defm : StorePatGlobalAddr<i32, truncstorei16, "MSSTORE16_I32">;
defm : StorePatGlobalAddr<i64, truncstorei8, "MSSTORE8_I64">;
defm : StorePatGlobalAddr<i64, truncstorei16, "MSSTORE16_I64">;
defm : StorePatGlobalAddr<i64, truncstorei32, "MSSTORE32_I64">;

// defm MEMREF_CONST : I<(outs MEMREF:$res), 
//                       (ins i32imm_op:$base, i32imm_op:$size, i32imm_op:$attr),
//                       (outs), (ins i32imm_op:$base, i32imm_op:$size, i32imm_op:$attr),
//                       [],
//                       "memref.const\t$res, $base, $size, $attr",
//                       "memref.const\t$base, $size, $attr", 0xda>;

defm MEMREF_ADD : I<(outs MEMREF:$res), (ins MEMREF:$mref, I32:$val), (outs), (ins),
                [(set MEMREF:$res, (mrefadd MEMREF:$mref, I32:$val))],
                "memref.add\t$res, $mref, $val",
                "memref.add\t", 0xdb>;

defm MEMREF_AND : I<(outs MEMREF:$res), (ins MEMREF:$mref, I32:$val), (outs), (ins),
                [(set MEMREF:$res, (mrefand MEMREF:$mref, I32:$val))],
                "memref.and\t$res, $mref, $val",
                "memref.and\t", 0xdc>;

defm MEMREF_ALLOC : I<(outs MEMREF:$res), (ins I32:$addr, I32:$size, I32:$attr),
                  (outs), (ins),
                  [(set MEMREF:$res, (mrefalloc I32:$addr, I32:$size, I32:$attr))],
                  "memref.alloc\t$res, $addr, $size, $attr",
                  "memref.alloc\t", 0xdd>;

defm MEMREF_NARROW : I<(outs MEMREF:$res), (ins MEMREF:$mref, I32:$base, I32:$size), 
                (outs), (ins),
                [(set MEMREF:$res, (mrefnarrow MEMREF:$mref, I32:$base, I32:$size))],
                "memref.narrow\t$res, $mref, $base, $size",
                "memref.narrow\t", 0xde>;

def : Pat<(memref (mrefnarrow (WebAssemblyWrapper_Memref tglobaladdr:$mref), I32:$base, I32:$size)),
          (MEMREF_NARROW tglobaladdr:$mref, I32:$base, I32:$size)>,
      Requires<[IsNotPIC, HasAddr32]>;

defm MEMREF_FIELD : I<(outs I32:$res), (ins i32imm_op:$idx, MEMREF:$mref),
                (outs), (ins i32imm_op:$idx),
                [],//[(set I32:$res, (mreffield imm_i32:$idx, MEMREF:$mref))],
                "memref.field\t$res, $idx, $mref",
                "memref.field\t$idx", 0xdf>;

def : Pat<(mreffield imm_i32:$idx, (WebAssemblyWrapper_Memref tglobaladdr:$mref)),
          (MEMREF_FIELD imm:$idx, tglobaladdr:$mref)>,
      Requires<[IsNotPIC, HasAddr32]>;
def : Pat<(mreffield imm_i32_target:$idx, (WebAssemblyWrapper_Memref tglobaladdr:$mref)),
          (MEMREF_FIELD imm:$idx, tglobaladdr:$mref)>,
      Requires<[IsNotPIC, HasAddr32]>;
def : Pat<(mreffield imm_i32:$idx, MEMREF:$mref),
          (MEMREF_FIELD imm:$idx, MEMREF:$mref)>,
      Requires<[IsNotPIC, HasAddr32]>;

def : Pat<(memref (mrefand (WebAssemblyWrapper_Memref tglobaladdr:$mref), I32:$val)),
          (MEMREF_AND tglobaladdr:$mref, I32:$val)>,
      Requires<[IsNotPIC, HasAddr32]>;

def : Pat<(memref (mrefadd (WebAssemblyWrapper_Memref tglobaladdr:$mref), I32:$val)),
          (MEMREF_ADD tglobaladdr:$mref, I32:$val)>,
      Requires<[IsNotPIC, HasAddr32]>;

defm MEMREF_EQ : I<(outs I32:$res), (ins MEMREF:$lhs, MEMREF:$rhs), (outs), (ins),
                [(set I32:$res, (setcc MEMREF:$lhs, MEMREF:$rhs, SETEQ))],
                "memref.eq\t$res, $lhs, $rhs",
                "memref.eq", 0xc5>;
def : Pat<(i32 (setcc (WebAssemblyWrapper_Memref tglobaladdr:$lhs), 
            MEMREF:$rhs, SETEQ)),
          (MEMREF_EQ tglobaladdr:$lhs, MEMREF:$rhs)>;
def : Pat<(i32 (setcc MEMREF:$lhs, 
            (WebAssemblyWrapper_Memref tglobaladdr:$rhs), SETEQ)),
          (MEMREF_EQ MEMREF:$lhs, tglobaladdr:$rhs)>;
def : Pat<(i32 (setcc (WebAssemblyWrapper_Memref tglobaladdr:$lhs), 
            (WebAssemblyWrapper_Memref tglobaladdr:$rhs), SETEQ)),
          (MEMREF_EQ tglobaladdr:$lhs, tglobaladdr:$rhs)>;

defm MEMREF_NE : I<(outs I32:$res), (ins MEMREF:$lhs, MEMREF:$rhs), (outs), (ins),
                [(set I32:$res, (setcc MEMREF:$lhs, MEMREF:$rhs, SETNE))],
                "memref.ne\t$res, $lhs, $rhs",
                "memref.ne", 0xc6>;
def : Pat<(i32 (setcc (WebAssemblyWrapper_Memref tglobaladdr:$lhs), 
            MEMREF:$rhs, SETNE)),
          (MEMREF_NE tglobaladdr:$lhs, MEMREF:$rhs)>;
def : Pat<(i32 (setcc MEMREF:$lhs, 
            (WebAssemblyWrapper_Memref tglobaladdr:$rhs), SETNE)),
          (MEMREF_NE MEMREF:$lhs, tglobaladdr:$rhs)>;
def : Pat<(i32 (setcc (WebAssemblyWrapper_Memref tglobaladdr:$lhs), 
            (WebAssemblyWrapper_Memref tglobaladdr:$rhs), SETNE)),
          (MEMREF_NE tglobaladdr:$lhs, tglobaladdr:$rhs)>;

// c5 ~ cf is available
defm MEMREF_NULL : I<(outs MEMREF:$res), (ins), 
                (outs), (ins),
                [(set MEMREF:$res, mrefnull)],
                "memref.null\t$res",
                "memref.null\t", 0xc7>;

defm SELECT_MEMREF : I<(outs MEMREF:$dst), (ins MEMREF:$lhs, MEMREF:$rhs, I32:$cond),
                    (outs), (ins),
                    [(set MEMREF:$dst, (select I32:$cond, MEMREF:$lhs, MEMREF:$rhs))],
                    "memref.select\t$dst, $lhs, $rhs, $cond", "memref.select", 0xc8>;

def : Pat<(select (i32 (setne I32:$cond, 0)), MEMREF:$lhs, MEMREF:$rhs),
          (SELECT_MEMREF MEMREF:$lhs, MEMREF:$rhs, I32:$cond)>;

def : Pat<(select (i32 (seteq I32:$cond, 0)), MEMREF:$lhs, MEMREF:$rhs),
          (SELECT_MEMREF MEMREF:$rhs, MEMREF:$lhs, I32:$cond)>;

def : Pat<(select I32:$cond, 
              (WebAssemblyWrapper_Memref tglobaladdr:$lhs), MEMREF:$rhs),
              (SELECT_MEMREF tglobaladdr:$lhs, MEMREF:$rhs, I32:$cond)>;
def : Pat<(select I32:$cond, 
              MEMREF:$lhs, (WebAssemblyWrapper_Memref tglobaladdr:$rhs)),
              (SELECT_MEMREF MEMREF:$lhs, tglobaladdr:$rhs, I32:$cond)>;
def : Pat<(select I32:$cond, 
              (WebAssemblyWrapper_Memref tglobaladdr:$lhs), 
              (WebAssemblyWrapper_Memref tglobaladdr:$rhs)),
              (SELECT_MEMREF tglobaladdr:$lhs, tglobaladdr:$rhs, I32:$cond)>;

def : Pat<(memref (WebAssemblyWrapper_Memref tglobaladdr:$addr)),
          (GLOBAL_GET_Addr tglobaladdr:$addr)>, Requires<[HasAddr32]>;

defm MEMREF_FREE : I<(outs), (ins MEMREF:$ref),
                  (outs), (ins), [],
                  "memref.free\t$ref",
                  "memref.free\t", 0xc9>;
// def : Pat<(mreffree MEMREF:$ref),
//           (MEMREF_FREE MEMREF:$ref)>;