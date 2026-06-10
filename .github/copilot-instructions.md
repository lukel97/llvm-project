# Copilot Instructions

## Repository Context

In this repository, the upstream is at [llvm/llvm-project](https://github.com/llvm/llvm-project). Most of the time if a commit or pull request is mentioned, it will be at that repository, not lukel97/llvm-project.

## llvm-test-suite

If llvm-test-suite is mentioned, often you want to clone it from [llvm/llvm-test-suite](https://github.com/llvm/llvm-test-suite) and build it with a locally built copy of Clang.

## Cross Compilation

If a target is mentioned that needs cross compiled like RISC-V or AArch64, create a CMake toolchain file to help cross compile. An example for RISC-V is:

```cmake
# rva23u64.cmake
set(CMAKE_SYSTEM_NAME Linux)

set(CMAKE_C_COMPILER path-to-llvm-build/bin/clang)
set(CMAKE_CXX_COMPILER path-to-llvm-build/clang++)

set(CMAKE_C_COMPILER_TARGET riscv64-linux-gnu)
set(CMAKE_CXX_COMPILER_TARGET riscv64-linux-gnu)
set(CMAKE_C_FLAGS_INIT "-march=rva23u64 -save-temps=obj")
set(CMAKE_CXX_FLAGS_INIT "-march=rva23u64 -save-temps=obj")
set(CMAKE_SYSTEM_PROCESSOR riscv64)

set(CMAKE_LINKER_TYPE LLD)

set(ARCH riscv64)
```

You can then create a cross-compiling CMake build for llvm-test-suite like:

```sh
cmake -B build.riscv64 --toolchain rva23u64.cmake -C cmake/caches/ReleaseLTO.cmake -GNinja <other flags as needed...>
```

If cross compiling, make sure to also have built LLD in your LLVM build. Also, if you want to see the assembly of the generated binaries, make sure to have the `-save-temps=obj` flag set in the toolchain file.
