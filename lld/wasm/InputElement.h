//===- InputElement.h ----------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLD_WASM_INPUT_ELEMENT_H
#define LLD_WASM_INPUT_ELEMENT_H

#include "Config.h"
#include "InputFiles.h"
#include "WriterUtils.h"
#include "lld/Common/LLVM.h"
#include "llvm/Object/Wasm.h"

namespace lld {
namespace wasm {

// Represents a single element (Global, Tag, Table, etc) within an input
// file.
class InputElement {
protected:
  InputElement(StringRef name, ObjFile *f)
      : file(f), live(!config->gcSections), name(name) {}

public:
  StringRef getName() const { return name; }
  uint32_t getAssignedIndex() const { return assignedIndex.getValue(); }
  bool hasAssignedIndex() const { return assignedIndex.hasValue(); }
  void assignIndex(uint32_t index) {
    assert(!hasAssignedIndex());
    assignedIndex = index;
  }

  ObjFile *file;
  bool live = false;

protected:
  StringRef name;
  llvm::Optional<uint32_t> assignedIndex;
};

inline WasmInitExpr intConst(uint64_t value, bool is64) {
  WasmInitExpr ie;
  if (is64) {
    ie.Opcode = llvm::wasm::WASM_OPCODE_I64_CONST;
    ie.Value.Int64 = static_cast<int64_t>(value);
  } else {
    ie.Opcode = llvm::wasm::WASM_OPCODE_I32_CONST;
    ie.Value.Int32 = static_cast<int32_t>(value);
  }
  return ie;
}

inline WasmInitExpr memrefAlloc(uint64_t addr, uint64_t size, uint64_t attr, uint64_t info, bool is64) {
  assert(!is64 && "memref does not support 64bit address now");
  WasmInitExpr ie;
  ie.Opcode = llvm::wasm::WASM_OPCODE_MEMREF_ALLOC;
  ie.Value.Memref.addr = addr;
  ie.Value.Memref.size = size;
  ie.Value.Memref.attr = attr;
  ie.Value.Memref.info = info;
  return ie;
}

class InputGlobal : public InputElement {
public:
  InputGlobal(const WasmGlobal &g, ObjFile *f)
      : InputElement(g.SymbolName, f), type(g.Type), initExpr(g.InitExpr) {}

  const WasmGlobalType &getType() const { return type; }
  const WasmInitExpr &getInitExpr() const { return initExpr; }

  void setPointerValue(uint64_t value) {
    bool is64 = config->is64.getValueOr(false);
    initExpr = is64 ? intConst(value, config->is64.getValueOr(false)) : memrefAlloc(value, 0, 0, 0, is64);
  }

  void setMemrefVlaue(uint64_t addr, uint64_t size , uint64_t attr, uint64_t info) {
    initExpr = memrefAlloc(addr, size, attr, info, config->is64.getValueOr(false));
  }

private:
  WasmGlobalType type;
  WasmInitExpr initExpr;
};

class InputTag : public InputElement {
public:
  InputTag(const WasmSignature &s, const WasmTag &t, ObjFile *f)
      : InputElement(t.SymbolName, f), signature(s) {}

  const WasmSignature &signature;
};

class InputTable : public InputElement {
public:
  InputTable(const WasmTable &t, ObjFile *f)
      : InputElement(t.SymbolName, f), type(t.Type) {}

  const WasmTableType &getType() const { return type; }
  void setLimits(const WasmLimits &limits) { type.Limits = limits; }

private:
  WasmTableType type;
};

} // namespace wasm

inline std::string toString(const wasm::InputElement *d) {
  return (toString(d->file) + ":(" + d->getName() + ")").str();
}

} // namespace lld

#endif // LLD_WASM_INPUT_ELEMENT_H
