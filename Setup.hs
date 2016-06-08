
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Simple
import Distribution.Simple.BuildPaths
import Distribution.Simple.Command
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PreProcess                               hiding ( ppC2hs )
import Distribution.Simple.Program
import Distribution.Simple.Program.Db
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.System
import Distribution.Verbosity

import Control.Applicative
import Control.Exception
import Control.Monad
import System.Directory
import System.Environment
import System.FilePath
import System.IO.Error
import Text.Printf
import Prelude


-- Configuration
-- -------------

customBuildInfoFilePath :: FilePath
customBuildInfoFilePath = "nvvm" <.> "buildinfo"

generatedBuldInfoFilePath :: FilePath
generatedBuldInfoFilePath = customBuildInfoFilePath <.> "generated"


-- Build setup
-- -----------

main :: IO ()
main = defaultMainWithHooks customHooks
  where
    readHook get_verbosity args flags = do
        noExtraFlags args
        getHookedBuildInfo (fromFlag (get_verbosity flags))

    -- Our readHook implementation uses our getHookedBuildInfo. We can't rely on
    -- cabal's autoconfUserHooks since they don't handle user overwrites to
    -- buildinfo like we do.
    --
    customHooks =
      simpleUserHooks
        { preBuild            = preBuildHook -- not using 'readHook' here because 'build' takes extra args
        , preClean            = readHook cleanVerbosity
        , preCopy             = readHook copyVerbosity
        , preInst             = readHook installVerbosity
        , preHscolour         = readHook hscolourVerbosity
        , preHaddock          = readHook haddockVerbosity
        , preReg              = readHook regVerbosity
        , preUnreg            = readHook regVerbosity
        , postConf            = postConfHook
        , postBuild           = postBuildHook
        , hookedPreProcessors = ("chs", ppC2hs) : filter (\x -> fst x /= "chs") (hookedPreProcessors simpleUserHooks)
        }

    -- The hook just loads the HookedBuildInfo generated by postConfHook, unless
    -- there is user-provided info that overwrites it.
    --
    preBuildHook :: Args -> BuildFlags -> IO HookedBuildInfo
    preBuildHook _ flags = getHookedBuildInfo $ fromFlag $ buildVerbosity flags

    -- The hook scans system in search for CUDA Toolkit. If the toolkit is not
    -- found, an error is raised. Otherwise the toolkit location is used to
    -- create a `nvvm.buildinfo.generated` file with all the resulting flags.
    postConfHook :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
    postConfHook args flags pkg_descr lbi = do
      let
          verbosity       = fromFlag (configVerbosity flags)
          currentPlatform = hostPlatform lbi
          compilerId_     = compilerId (compiler lbi)
      --
      noExtraFlags args
      generateAndStoreBuildInfo verbosity currentPlatform compilerId_ generatedBuldInfoFilePath
      validateLinker verbosity currentPlatform $ withPrograms lbi
      --
      actualBuildInfoToUse <- getHookedBuildInfo verbosity
      let pkg_descr' = updatePackageDescription actualBuildInfoToUse pkg_descr
      postConf simpleUserHooks args flags pkg_descr' lbi

    -- This hook fixes the embedded LC_RPATHs in the generated .dylib on OSX.
    postBuildHook :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
    postBuildHook args flags pkg_descr lbi = do
      let
          verbosity           = fromFlag (buildVerbosity flags)
          platform            = hostPlatform lbi
          cid                 = compilerId (compiler lbi)
          uid                 = localUnitId lbi
          sharedLib           = buildDir lbi </> mkSharedLibName cid uid
          Just extraLibDirs'  = extraLibDirs . libBuildInfo <$> library pkg_descr
      --
      noExtraFlags args
      updateLibraryRPATHs verbosity platform sharedLib extraLibDirs'


-- It seems that the GHC and/or Cabal developers don't quite understand how
-- dynamic linking works on OSX. Even though we have specified
-- '-optl-Wl,-rpath,...' as part of the configuration, this (sometimes?) gets
-- filtered out somewhere, and the resulting .dylib that is generated does not
-- have this path embedded as an LC_RPATH. The result is that the NVVM library
-- will not be found, resulting in a link-time error.
--
-- On *nix (and versions of OSX previous to El Capitan 10.11), we could use
-- [DY]LD_LIBRARY_PATH to specify where to resolve @rpath locations, but that is
-- no longer an option on OSX due to System Integrity Protection.
--
-- An alternate argument is that the CUDA installer should have updated the
-- install name (LC_ID_DYLIB) of its dynamic libraries to include the full
-- absolute path, rather than relying on @rpath in the first place, which is
-- what Apple's system libraries do for example.
--
updateLibraryRPATHs :: Verbosity -> Platform -> FilePath -> [FilePath] -> IO ()
updateLibraryRPATHs verbosity (Platform _ os) sharedLib extraLibDirs' =
  when (os == OSX) $ do
    exists <- doesFileExist sharedLib
    unless exists $ die $ printf "Unexpected failure: library does not exist: %s" sharedLib
    --
    mint   <- findProgram verbosity "install_name_tool"
    case mint of
      Nothing                -> notice verbosity $ "Could not locate 'install_name_tool' in order to update LC_RPATH entries. This is likely to cause problems later on."
      Just install_name_tool ->
        forM_ extraLibDirs' $ \libDir ->
          runProgramInvocation verbosity $ simpleProgramInvocation install_name_tool ["-add_rpath", libDir, sharedLib]


-- Reads user-provided `nvvm.buildinfo` if present, otherwise loads `nvvm.buildinfo.generated`
-- Outputs message informing about the other possibility.
-- Calls die when neither of the files is available.
-- (generated one should be always present, as it is created in the post-conf step)
--
getHookedBuildInfo :: Verbosity -> IO HookedBuildInfo
getHookedBuildInfo verbosity = do
  customBuildInfoExists <- doesFileExist customBuildInfoFilePath
  if customBuildInfoExists
    then do
      notice verbosity $ printf "The user-provided buildinfo from file '%s' will be used. To use default settings, delete this file." customBuildInfoFilePath
      readHookedBuildInfo verbosity customBuildInfoFilePath
    else do
      generatedBuildInfoExists <- doesFileExist generatedBuldInfoFilePath
      if generatedBuildInfoExists
        then do
          notice verbosity $ printf "Using build information from '%s'" generatedBuldInfoFilePath
          notice verbosity $ printf "Provide a '%s' file to override this behaviour" customBuildInfoFilePath
          readHookedBuildInfo verbosity generatedBuldInfoFilePath
        else
          die $ printf "Unexpected failure: neither the default '%s' nor custom '%s' exist" generatedBuldInfoFilePath customBuildInfoFilePath


-- Runs CUDA detection procedure and stores .buildinfo to a file.
--
generateAndStoreBuildInfo :: Verbosity -> Platform -> CompilerId -> FilePath -> IO ()
generateAndStoreBuildInfo verbosity platform (CompilerId _ghcFlavor ghcVersion) path = do
  installPath <- findCUDAInstallPath verbosity platform
  hbi         <- libraryBuildInfo installPath platform ghcVersion
  storeHookedBuildInfo verbosity path hbi


-- Try to locate CUDA installation by checking (in order):
--
--  1. CUDA_PATH environment variable
--  2. Looking for `nvcc` in `PATH`
--  3. Checking /usr/local/cuda
--
-- In case of failure, calls die with the pretty long message from below.
--
findCUDAInstallPath :: Verbosity -> Platform -> IO FilePath
findCUDAInstallPath verbosity platform = do
  result <- findFirstValidLocation verbosity platform (candidateCUDAInstallPaths verbosity platform)
  case result of
    Just installPath -> do
      notice verbosity $ printf "Found CUDA toolkit at: %s" installPath
      return installPath
    Nothing -> die cudaNotFoundMsg


-- Function iterates over action yielding possible locations, evaluating them
-- and returning the first valid one. Returns Nothing if no location matches.
--
findFirstValidLocation :: Verbosity -> Platform -> [(IO FilePath, String)] -> IO (Maybe FilePath)
findFirstValidLocation verbosity platform = go
  where
    go :: [(IO FilePath, String)] -> IO (Maybe FilePath)
    go []     = return Nothing
    go (x:xs) = do
      let (path,desc) = x
      info verbosity $ printf "checking for %s" desc
      found <- validateIOLocation verbosity platform path
      if found
        then Just `fmap` path
        else go xs


-- Evaluates IO to obtain the path, handling any possible exceptions.
-- If path is evaluable and points to valid CUDA toolkit returns True.
--
validateIOLocation :: Verbosity -> Platform -> IO FilePath -> IO Bool
validateIOLocation verbosity platform iopath =
  let handler :: IOError -> IO Bool
      handler err = do
        info verbosity (show err)
        return False
  in
  (iopath >>= validateLocation verbosity platform) `catch` handler


-- Checks whether given location looks like a valid CUDA toolkit directory by
-- testing whether the 'nvvm.h' file exists in the expected place.
--
-- TODO: Ideally this should check also for 'libnvvm' and whether the library
-- exports relevant symbols. This should be achievable with some `nm` trickery.
--
validateLocation :: Verbosity -> Platform -> FilePath -> IO Bool
validateLocation verbosity platform path = do
  let nvvmHeader = nvvmIncludePath platform path </> "nvvm.h"
      cudaHeader = cudaIncludePath platform path </> "cuda.h"
  --
  exists <- (&&) <$> doesFileExist nvvmHeader <*> doesFileExist cudaHeader
  info verbosity $
    if exists
      then printf "Path accepted: %s" path
      else printf "Path rejected: %s\nDoes not exist: %s or %s" path cudaHeader nvvmHeader
  return exists


-- Returns pairs of (action yielding candidate path, String description of that location)
--
candidateCUDAInstallPaths :: Verbosity -> Platform -> [(IO FilePath, String)]
candidateCUDAInstallPaths verbosity platform =
  [ (getEnv "CUDA_PATH", "environment variable CUDA_PATH")
  , (findInPath,         "nvcc compiler executable in PATH")
  , (return defaultPath, printf "default install location (%s)" defaultPath)
  ]
  where
    findInPath :: IO FilePath
    findInPath = do
      nvccPath <- findProgramLocationOrError verbosity "nvcc"
      -- The obtained path is likely TOOLKIT/bin/nvcc. We want to extract the
      -- TOOLKIT part
      return (takeDirectory $ takeDirectory nvccPath)

    defaultPath :: FilePath
    defaultPath = defaultCUDAInstallPath platform


defaultCUDAInstallPath :: Platform -> FilePath
defaultCUDAInstallPath _ = "/usr/local/cuda"  -- windows?


-- NOTE: this function throws an exception when there is no `nvcc` in PATH.
-- The exception contains a meaningful message.
--
findProgramLocationOrError :: Verbosity -> String -> IO FilePath
findProgramLocationOrError verbosity execName = do
  location <- findProgram verbosity execName
  case location of
    Just path -> return path
    Nothing   -> ioError $ mkIOError doesNotExistErrorType ("not found: " ++ execName) Nothing Nothing

findProgram :: Verbosity -> FilePath -> IO (Maybe FilePath)
findProgram verbosity prog = do
  result <- findProgramOnSearchPath verbosity defaultProgramSearchPath prog
  case result of
    Nothing       -> return Nothing
    Just (path,_) -> return (Just path)


cudaNotFoundMsg :: String
cudaNotFoundMsg = unlines
  [ "********************************************************************************"
  , ""
  , "The configuration process failed to locate your CUDA installation. Ensure that you have installed both the developer driver and toolkit, available from:"
  , ""
  , "> http://developer.nvidia.com/cuda-downloads"
  , ""
  , "and make sure that `nvcc` is available in your PATH, or set the CUDA_PATH environment variable appropriately. Check the above output log and run the command directly to ensure it can be located."
  , ""
  , "If you have a non-standard installation, you can add additional search paths using --extra-include-dirs and --extra-lib-dirs. Note that 64-bit Linux flavours often require both `lib64` and `lib` library paths, in that order."
  , ""
  , "********************************************************************************"
  ]


-- Generates build info with flags needed for CUDA Toolkit to be properly
-- visible to underlying build tools.
--
libraryBuildInfo :: FilePath -> Platform -> Version -> IO HookedBuildInfo
libraryBuildInfo installPath platform@(Platform arch os) ghcVersion = do
  let
      libraryPaths      = [nvvmLibraryPath platform installPath]
      includePaths      = [nvvmIncludePath platform installPath, cudaIncludePath platform installPath]

      -- options for GHC
      extraLibDirs'     = libraryPaths
      ccOptions'        = map ("-I"++) includePaths
      ldOptions'        = map ("-L"++) extraLibDirs'
      ghcOptions        = map ("-optc"++) ccOptions'
                       ++ map ("-optl"++) ldOptions'
                       ++ if os /= Windows
                            then map ("-optl-Wl,-rpath,"++) extraLibDirs'
                            else []
      extraLibs'        = nvvmLibrary platform

      -- options for C2HS
      archFlag          = case arch of
                            I386   -> "-m32"
                            X86_64 -> "-m64"
                            _      -> ""
      emptyCase         = ["-DUSE_EMPTY_CASE" | versionBranch ghcVersion >= [7,8]]
      c2hsOptions       = unwords $ map ("--cppopts="++) ("-E" : archFlag : emptyCase)
      c2hsExtraOptions  = ("x-extra-c2hs-options", c2hsOptions)

      addSystemSpecificOptions :: BuildInfo -> IO BuildInfo
      addSystemSpecificOptions bi =
        case os of
          -- In the CUDA package this is used to populate the extraGHCiLibs
          -- field with the mangled .dll names. I'm not sure what those are for
          -- this library, so left out for the time being.
          Windows -> return bi

          -- In the CUDA package used to add the framework option (not needed
          -- here) and extra CPP flags to deal with the Blocks extension (taken
          -- care of by newer versions of c2hs).
          OSX     -> return bi
          _       -> return bi

  buildInfo' <- addSystemSpecificOptions $ emptyBuildInfo
    { ccOptions      = ccOptions'
    , ldOptions      = ldOptions'
    , extraLibs      = extraLibs'
    , extraLibDirs   = extraLibDirs'
    , options        = [(GHC, ghcOptions) | os /= Windows]
    , customFieldsBI = [c2hsExtraOptions]
    }

  return (Just buildInfo', [])


-- Location of the NVVM library relative to the base CUDA installation
--
nvvmPath :: Platform -> FilePath -> FilePath
nvvmPath _ base = base </> "nvvm"

nvvmIncludePath :: Platform -> FilePath -> FilePath
nvvmIncludePath platform base = nvvmPath platform base </> "include"

cudaIncludePath :: Platform -> FilePath -> FilePath
cudaIncludePath _ base = base </> "include"

nvvmLibraryPath :: Platform -> FilePath -> FilePath
nvvmLibraryPath platform@(Platform arch os) base = nvvmPath platform base </> libpath
  where
    libpath =
      case (os, arch) of
        (Windows, I386)   -> "Win32"
        (Windows, X86_64) -> "x64"
        (OSX,     _)      -> "lib"    -- MacOS does not distinguish 32- vs. 64-bit paths
        (_,       X86_64) -> "lib64"  -- treat all others similarly
        _                 -> "lib"

nvvmLibrary :: Platform -> [String]
nvvmLibrary _ = ["nvvm"]

storeHookedBuildInfo :: Verbosity -> FilePath -> HookedBuildInfo -> IO ()
storeHookedBuildInfo verbosity path hbi = do
    notice verbosity $ "Storing parameters to " ++ path
    writeHookedBuildInfo path hbi


-- On Windows platform the binutils linker targeting x64 is bugged and cannot
-- properly link with import libraries generated by MS compiler (like the CUDA ones).
-- The programs would correctly compile and crash as soon as the first FFI call is made.
--
-- Therefore we fail configure process if the linker is too old and provide user
-- with guidelines on how to fix the problem.
--
validateLinker :: Verbosity -> Platform -> ProgramDb -> IO ()
validateLinker verbosity (Platform X86_64 Windows) db = do
  let say msg = printf "%s. If generated executables crash when making calls to NVVM please see: %s" msg windowsHelpPage
  --
  maybeLdPath <- getRealLdPath verbosity db
  case maybeLdPath of
    Nothing     -> warn verbosity $ say "Cannot find ld.exe to check if it is new enough"
    Just ldPath -> do
      debug verbosity $ "Checking if ld.exe at " ++ ldPath ++ " is new enough"
      maybeVersion <- getLdVersion verbosity ldPath
      case maybeVersion of
        Nothing        -> warn verbosity $ say "Unknown ld.exe version"
        Just ldVersion -> do
          debug verbosity $ "Found ld.exe version: " ++ show ldVersion
          when (ldVersion < [2,25,1]) $ die (windowsLinkerBugMsg ldPath)
validateLinker _ _ _ = return () -- The linker bug is present only on Win64 platform


-- On Windows GHC package comes with two copies of ld.exe.
--
-- ProgramDb knows about the first one: ghcpath\mingw\bin\ld.exe
-- This function returns the other one: ghcpath\mingw\x86_64-w64-mingw32\bin\ld.exe
--
-- The second one is the one that does actual linking and code generation.
-- See: https://github.com/tmcdonell/cuda/issues/31#issuecomment-149181376
--
-- The function is meant to be used only on 64-bit GHC distributions.
--
getRealLdPath :: Verbosity -> ProgramDb -> IO (Maybe FilePath)
getRealLdPath verbosity programDb =
  -- This should ideally work `programFindVersion ldProgram` but for some reason
  -- it does not. The issue should be investigated at some time.
  case lookupProgram ghcProgram programDb of
    Nothing            -> return Nothing
    Just configuredGhc -> do
      let ghcPath        = locationPath $ programLocation configuredGhc
          presumedLdPath = (takeDirectory . takeDirectory) ghcPath </> "mingw" </> "x86_64-w64-mingw32" </> "bin" </> "ld.exe"
      info verbosity $ "Presuming ld location" ++ presumedLdPath
      presumedLdExists <- doesFileExist presumedLdPath
      return $ if presumedLdExists
                 then Just presumedLdPath
                 else Nothing

-- Tries to obtain the version `ld`. Throws an exception on failure.
--
getLdVersion :: Verbosity -> FilePath -> IO (Maybe [Int])
getLdVersion verbosity ldPath = do
  -- Examples of version string format:
  --  * GNU ld (GNU Binutils) 2.25.1
  --  * GNU ld (GNU Binutils) 2.20.51.20100613
  --
  ldVersionString <- getProgramInvocationOutput normal (simpleProgramInvocation ldPath ["-v"])

  let versionText   = last $ words ldVersionString -- takes e.g. "2.25.1"
      versionParts  = splitOn (== '.') versionText
      versionParsed = Just $ map read versionParts

  -- last and read above may throw and message would be not understandable for user,
  -- so we'll intercept exception and rethrow it with more useful message.
  let handleError :: SomeException -> IO (Maybe [Int])
      handleError e = do
          warn verbosity $ printf "cannot parse ld version string: '%s'. Parsing exception: %s" ldVersionString (show e)
          return Nothing

  evaluate versionParsed `catch` handleError

splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s =
  case dropWhile p s of
    [] -> []
    ss -> let (w,s') = break p ss in w : splitOn p s'


windowsHelpPage :: String
windowsHelpPage = "https://github.com/tmcdonell/nvvm/blob/master/WINDOWS.md"

windowsLinkerBugMsg :: FilePath -> String
windowsLinkerBugMsg ldPath = printf (unlines msg) windowsHelpPage ldPath
  where
    msg =
      [ "********************************************************************************"
      , ""
      , "The installed version of `ld.exe` has version < 2.25.1. This version has known bug on Windows x64 architecture, making it unable to correctly link programs using CUDA. The fix is available and MSys2 released fixed version of `ld.exe` as part of their binutils package (version 2.25.1)."
      , ""
      , "To fix this issue, replace the `ld.exe` in your GHC installation with the correct binary. See the following page for details:"
      , ""
      , "  %s"
      , ""
      , "The full path to the outdated `ld.exe` detected in your installation:"
      , ""
      , "> %s"
      , ""
      , "Please download a recent version of binutils `ld.exe`, from, e.g.:"
      , ""
      , "  http://repo.msys2.org/mingw/x86_64/mingw-w64-x86_64-binutils-2.25.1-1-any.pkg.tar.xz"
      , ""
      , "********************************************************************************"
      ]


-- Replicate the default C2HS preprocessor hook here, and inject a value for
-- extra-c2hs-options, if it was present in the buildinfo file
--
-- Everything below copied from Distribution.Simple.PreProcess
--
ppC2hs :: BuildInfo -> LocalBuildInfo -> PreProcessor
ppC2hs bi lbi =
  PreProcessor
    { platformIndependent = False
    , runPreProcessor     = \(inBaseDir, inRelativeFile) (outBaseDir, outRelativeFile) verbosity ->
          rawSystemProgramConf verbosity c2hsProgram (withPrograms lbi) . filter (not . null) $
            maybe [] words (lookup "x-extra-c2hs-options" (customFieldsBI bi))
            ++ ["--include=" ++ outBaseDir]
            ++ ["--cppopts=" ++ opt | opt <- getCppOptions bi lbi]
            ++ ["--output-dir=" ++ outBaseDir,
                "--output=" ++ outRelativeFile,
                inBaseDir </> inRelativeFile]
    }

getCppOptions :: BuildInfo -> LocalBuildInfo -> [String]
getCppOptions bi lbi
    = hcDefines (compiler lbi)
   ++ ["-I" ++ dir | dir <- includeDirs bi]
   ++ [opt | opt@('-':c:_) <- ccOptions bi, c `elem` "DIU"]

hcDefines :: Compiler -> [String]
hcDefines comp =
  case compilerFlavor comp of
    GHC  -> ["-D__GLASGOW_HASKELL__=" ++ versionInt version]
    JHC  -> ["-D__JHC__=" ++ versionInt version]
    NHC  -> ["-D__NHC__=" ++ versionInt version]
    Hugs -> ["-D__HUGS__"]
    _    -> []
  where version = compilerVersion comp

-- TODO: move this into the compiler abstraction
-- FIXME: this forces GHC's crazy 4.8.2 -> 408 convention on all the other
-- compilers. Check if that's really what they want.
versionInt :: Version -> String
versionInt (Version { versionBranch = [] })      = "1"
versionInt (Version { versionBranch = [n] })     = show n
versionInt (Version { versionBranch = n1:n2:_ }) = printf "%d%02d" n1 n2

