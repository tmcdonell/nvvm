Haskell FFI Bindings to NVVM
============================

[![Build status](https://travis-ci.org/tmcdonell/nvvm.svg?branch=master)](https://travis-ci.org/tmcdonell/nvvm)

The NVVM library compiles NVVM IR (a subset of LLVM IR) into PTX code which can
then be executed on NVIDIA GPUs. In contrast to the standard NVPTX target built
in to the LLVM toolchain, NVVM includes a set of proprietary optimisations which
are otherwise only available by compiling CUDA code with the `nvcc` compiler.

The resulting PTX code can be loaded onto the GPU and executed using the 'cuda'
package:

  <https://github.com/tmcdonell/cuda>

The NVVM library is a compiler component available a part of the CUDA toolkit:

  <https://developer.nvidia.com/cuda-toolkit>

The configure step will look for your CUDA installation in the standard places,
and if the `nvcc` compiler is found in your `PATH`, relative to that.

