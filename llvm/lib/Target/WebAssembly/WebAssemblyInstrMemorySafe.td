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

// mrefadd
def mrefadd : SDNode<"ISD::WASM_MEMREF_ADD", SDTMrefBinary, []>;

// mrefand
def mrefand : SDNode<"ISD::WASM_MEMREF_AND", SDTMrefBinary, []>;

def mrefalloc : SDNode<"ISD::WASM_MEMREF_ALLOC", SDTMrefAlloc, []>;

// this is for memref plus i32
def regMemrefPlusImm : PatFrag<(ops node:$mref, node:$off),
                               (mrefadd node:$mref, node:$off)>;

// i32 immediate
def SDTInt32Leaf : SDTypeProfile<1, 0, [SDTCisVT<0, i32>]>;
def imm_i32      : SDNode<"ISD::TargetConstant"  , SDTInt32Leaf , [], "ConstantSDNode">;
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
  def : Pat<(ty (kind (WebAssemblyWrapper tglobaladdr:$mref))),
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
  def : Pat<(kind ty:$val, (WebAssemblyWrapper tglobaladdr:$mref)),
            (!cast<NI>(inst # "_A32") 0, 0, tglobaladdr:$mref,
             ty:$val)>,
        Requires<[IsNotPIC, HasAddr32]>;
}

defm : StorePatGlobalAddr<i32, store, "MSSTORE_I32">;
defm : StorePatGlobalAddr<i64, store, "MSSTORE_I64">;
defm : StorePatGlobalAddr<f32, store, "MSSTORE_F32">;
defm : StorePatGlobalAddr<f64, store, "MSSTORE_F64">;
defm : StorePatGlobalAddr<memref, store, "MSSTORE_MEMREF">;


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

// defm MEMREF_NULL : I<(outs MEMREF:$dst), (ins),
//                         (outs), (ins),
//                         [(set MEMREF:$dst, (!cast<Intrinsic>("int_wasm_memref_null")))],
//                         "memref.null\t$dst",
//                         "memref.null\t", 0xfa>,
//                       Requires<[HasAddr32]>;

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

def : Pat<(memref (mrefand (WebAssemblyWrapper tglobaladdr:$mref), I32:$val)),
          (MEMREF_AND tglobaladdr:$mref, I32:$val)>,
      Requires<[IsNotPIC, HasAddr32]>;

def : Pat<(memref (mrefadd (WebAssemblyWrapper tglobaladdr:$mref), I32:$val)),
          (MEMREF_ADD tglobaladdr:$mref, I32:$val)>,
      Requires<[IsNotPIC, HasAddr32]>;


// defm MEMREF_NARROW : I<(outs MEMREF:$res), (ins MEMREF:$mref, I32:$base, I32:$size, I32:$info), 
//                 (outs), (ins),
//                 [(set MEMREF:$res, (node MEMREF:$mref, I32:$base, I32:$size, I32:$info))],
//                 "memref.narrow\t$res, $mref, $base, $size, $info",
//                 "memref.narrow\t", 0xf6>;

// defm MEMREF_IS_NULL : I<(outs MEMREF:$res), (ins MEMREF:$mref), 
//                 (outs), (ins),
//                 [(set MEMREF:$res, (node MEMREF:$mref))],
//                 "memref.narrow\t$res, $mref",
//                 "memref.narrow\t", 0xf7>;
