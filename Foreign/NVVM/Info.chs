{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module    : Foreign.NVVM.Info
-- Copyright : [2016] Trevor L. McDonell
-- License   : BSD
--
-- General information query
--
--------------------------------------------------------------------------------

module Foreign.NVVM.Info (

  nvvmVersion,
  nvvmIRVersion,

) where

import Foreign.NVVM.Error
import Foreign.NVVM.Internal.C2HS
import Foreign.Marshal
import Foreign.C
import Foreign.Ptr

import Data.Version
import System.IO.Unsafe


#include "cbits/stubs.h"
{# context lib="nvvm" #}


-- | Get the version of NVVM IR supported by this library. The first component
-- is the NVVM IR version, and the second the version of the debug metadata.
--
-- Requires: CUDA-7.0
--
-- <http://docs.nvidia.com/cuda/libnvvm-api/group__query.html#group__query_1g0894677934db095b3c40d4f8e2578cc5>
--
nvvmIRVersion :: (Version, Version)
#if CUDA_VERSION < 7000
nvvmIRVersion = requireSDK 'nvvmIRVersion 7.0
#else
nvvmIRVersion = unsafePerformIO $ do
  (status, r1, r2, d1, d2) <- c_nvvmIRVersion
  resultIfOk (status, (makeVersion [r1,r2], makeVersion [d1,d2]))

{#
  fun unsafe nvvmIRVersion as c_nvvmIRVersion
    { alloca- `Int' peekIntConv*
    , alloca- `Int' peekIntConv*
    , alloca- `Int' peekIntConv*
    , alloca- `Int' peekIntConv*
    }
    -> `Status' cToEnum
#}
#endif


-- | Get the version of the NVVM library
--
-- <http://docs.nvidia.com/cuda/libnvvm-api/group__query.html#group__query_1gcdd062f26078d20ded68f1017e999246>
--
nvvmVersion :: Version
nvvmVersion = unsafePerformIO $ do
  (status, v1, v2) <- c_nvvmVersion
  resultIfOk (status, makeVersion [v1,v2])

{#
  fun unsafe nvvmVersion as c_nvvmVersion
    { alloca- `Int' peekIntConv*
    , alloca- `Int' peekIntConv*
    }
    -> `Status' cToEnum
#}

#if __GLASGOW_HASKELL__ <= 708
makeVersion :: [Int] -> Version
makeVersion vs = Version vs []
#endif

