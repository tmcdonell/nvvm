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

import Data.Version
import System.IO.Unsafe


#include "cbits/stubs.h"
{# context lib="nvvm" #}


-- | Get the version of NVVM IR supported by this library. The first component
-- is the NVVM IR version, and the second the version of the debug metadata.
--
nvvmIRVersion :: (Version, Version)
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


-- | Get the version of the NVVM library
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

