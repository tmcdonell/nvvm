{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module    : Foreign.NVVM.Path
-- Copyright : [2020] Trevor L. McDonell
-- License   : BSD
--
--------------------------------------------------------------------------------

module Foreign.NVVM.Path (

  nvvmDeviceLibraryPath

) where

import Foreign.CUDA.Path

import Language.Haskell.TH
import System.Directory
import System.FilePath
import Data.List


-- | Location of the libdevice bitcode file(s)
--
-- @since 0.10.0.0
--
nvvmDeviceLibraryPath :: FilePath
nvvmDeviceLibraryPath = $(do
  path <- runIO $ do
    let ubuntu = return "/usr/lib/nvidia-cuda-toolkit/libdevice"
        nvidia = return (cudaInstallPath </> "nvvm" </> "libdevice")
        --
        orElse this other = do
          path <- this
          yes  <- doesDirectoryExist path
          if yes
             then do
               files <- getDirectoryContents path
               let bc = filter (\f -> "libdevice" `isPrefixOf` f && takeExtension f == ".bc") files
               if null bc
                  then other
                  else return path
             else other
    --
    nvidia `orElse` ubuntu `orElse` error "could not locate libdevice directory"
  --
  stringE path
  )

