
import Foreign.CUDA.Driver                        as CUDA
import Foreign.NVVM                               as NVVM

import Data.ByteString                            as B
import Data.ByteString.Char8                      as B8
import Prelude                                    as P


main :: IO ()
main = do
  -- Initialise the CUDA driver in the usual way.
  -- See the 'cuda' package for details.
  --
  CUDA.initialise []
  dev <- CUDA.device 0
  prp <- CUDA.props dev
  ctx <- CUDA.create dev []

  -- Read the NVVM IR
  --
  ll  <- B.readFile "vector-add.ll"

  -- Compile the NVVM IR and load into a CUDA module
  --
  prg <- NVVM.compileModule "vector-add" ll
           [ NVVM.Target (CUDA.computeCapability prp)
           , NVVM.OptimisationLevel 3
           ]

  mdl <- CUDA.loadData (NVVM.compileResult prg)
  fun <- CUDA.getFun mdl "kernel"

  -- We can also display the generated PTX and show any compilation messages
  --
  P.putStrLn "NVVM compilation result:"
  B8.putStrLn (NVVM.compileLog prg)
  B8.putStrLn (NVVM.compileResult prg)

  -- Generate some data and copy to the GPU for processing by the kernel. Lists
  -- are easy for this simple example, but for performance you would prefer
  -- unboxed arrays.
  --
  let n  = 128
      xs = P.take n [0..]     :: [ Float ]
      ys = P.take n [0,2..]   :: [ Float ]

  zs <- CUDA.withListArray xs $ \d_xs -> do
        CUDA.withListArray ys $ \d_ys -> do
        CUDA.allocaArray n    $ \d_zs -> do
          CUDA.launchKernel fun (1,1,1) (n,1,1) 0 Nothing [CUDA.VArg d_xs, CUDA.VArg d_ys, CUDA.VArg d_zs]
          CUDA.peekListArray n d_zs

  -- Print and check the results
  --
  P.putStrLn ("got result: " ++ show zs)
  if (zs == P.zipWith (+) xs ys)
    then P.putStrLn "OK!"
    else P.putStrLn "FAILED!!"

  -- Final cleanup
  --
  CUDA.unload mdl
  CUDA.destroy ctx

