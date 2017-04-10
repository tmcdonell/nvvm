Haskell FFI Bindings to NVVM
============================

[![Build status](https://travis-ci.org/tmcdonell/nvvm.svg?branch=master)](https://travis-ci.org/tmcdonell/nvvm)
[![Hackage](https://img.shields.io/hackage/v/nvvm.svg)](https://hackage.haskell.org/package/nvvm)

The NVVM library compiles [NVVM IR][nvvm-ir-spec] (a subset of LLVM IR) into PTX code which can
then be executed on NVIDIA GPUs.

In contrast to the standard [NVPTX][nvptx-spec] target built in to the LLVM
toolchain, NVVM includes a set of proprietary optimisations which are otherwise
only available by compiling CUDA code with the `nvcc` compiler. On the other
hand, the version of LLVM that NVVM is internally based on typically lags the
public release by several generations (years), so these secret optimisations may
or may not be worthwhile to your application.

The resulting PTX code can be loaded onto the GPU and executed using the [cuda
package][hs-cuda].

The NVVM library is a compiler component available a part of the CUDA toolkit:

  <https://developer.nvidia.com/cuda-toolkit>

The configure step will look for your CUDA installation in the standard places,
and if the `nvcc` compiler is found in your `PATH`, relative to that.


[nvptx-spec]:     http://llvm.org/docs/NVPTXUsage.html
[nvvm-ir-spec]:   http://docs.nvidia.com/cuda/nvvm-ir-spec/index.html
[hs-cuda]:        https://github.com/tmcdonell/cuda

