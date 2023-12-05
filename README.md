# Compiler for MemS-Wasm

This is a branch of the LLVM project, a compiler for translating C language into MemS-Wasm. The main content is on the branch `release14-memswasm`, and all modifications are based on branch `c12386a` (i.e., `release/14.x`).

## Getting the Source Code and Building
```
git clone -b release14-memswasm https://github.com/huangh-git/llvm-project-memswasm.git
cd llvm-project-memswasm
mkdir build && cd build
cmake ../llvm -DLLVM_EXPERIMENTAL_TARGETS_TO_BUILD=WebAssembly -DCMAKE_BUILD_TYPE=Release -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DLLVM_ENABLE_PROJECTS="clang;lld" -DCMAKE_INSTALL_PREFIX=./install/llvm/ -DLLVM_INCLUDE_EXAMPLES=OFF -DLLVM_INCLUDE_TESTS=OFF -DLLVM_ENABLE_ASSERTIONS=1 -DLLVM_TARGETS_TO_BUILD="WebAssembly"
make -j8
```

You can replace `-DCMAKE_BUILD_TYPE=Release` with `-DCMAKE_BUILD_TYPE=Debug` if you need to perform debugging. Additionally, you can use `make -jn` instead of `make -j8` to speed up your compilation if you have more CPU cores available, where `n` is greater than 8.

## Before compiling C to MemS-Wasm
If your C code does not contain libc library function calls, you can skip this section and look directly at [Compiling C to MemS-Wasm](#compile-without-std-lib).

### ms-wasi-libc
To compile C code that contains a `main` function or makes libc calls, you should first build the [ms-wasi-libc](https://github.com/huangh-git/ms-wasi-libc) library.
### libclang_rt.builtins-wasm32.a
We need `libclang_rt.builtins-wasm32.a` as well, you can get it from [ms-wasi-sdk](https://github.com/huangh-git/ms-wasi-sdk). 
```
git clone -b ms-clang https://github.com/huangh-git/ms-wasi-sdk.git
mkdir -p /path/to/mems-wasm-llvm/build/lib/clang/14.0.5/lib/wasi/
cp /path/to/ms-wasi-sdk/libclang_rt.builtins-wasm32.a /path/to/mems-wasm-llvm/build/lib/clang/14.0.5/lib/wasi/
```


## Compiling C to MemS-Wasm
### compile without std lib

```
/path/to/mems-wasm-llvm/build/bin/clang --target=wasm32-unknown-wasi -Wl,--no-entry -Wl,--export-all  -o test.wasm test.c -nostdlib         
```

### compile with std lib
```
/path/to/mems-wasm-llvm/build/bin/clang --target=wasm32-unknown-wasi -Wl,--no-entry -Wl,--export-all -o test.wasm test.c --sysroot=/path/to/ms-wasi-libc/sysroot/
```

