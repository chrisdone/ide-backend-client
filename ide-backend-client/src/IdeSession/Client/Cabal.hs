module IdeSession.Client.Cabal (
    listTargets
  , initCabalSession
  ) where

import Control.Exception
import Data.List
import Data.Maybe
import Data.Monoid
import System.Directory
import System.FilePath

import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.BuildTarget
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import qualified Distribution.ModuleName      as C
import qualified Distribution.Simple.Compiler as C
import Language.Haskell.Extension

import IdeSession
import IdeSession.Client.CmdLine
import IdeSession.Client.JsonAPI

{-------------------------------------------------------------------------------
  Top-level API
-------------------------------------------------------------------------------}

listTargets :: FilePath -> IO [String]
listTargets cabalRoot = do
    lbi <- getPersistBuildConfig (cabalRoot </> "dist")
    return $ availableTargets lbi

initCabalSession :: ClientIO -> Options -> CabalOptions -> IO IdeSession
initCabalSession ClientIO{..} Options{..} CabalOptions{..} = do
    lbi <- getPersistBuildConfig (cabalRoot </> "dist")
    (comp, clbi, exts) <- resolveBuildTarget lbi cabalTarget
    mods <- componentModules cabalRoot comp
    let opts = buildInfoGhcOpts lbi (componentBuildInfo comp) clbi
        initParams = optInitParams {
            sessionInitGhcOptions = opts
          }
        config = optConfig {
            configPackageDBStack = unlessOverwritten configPackageDBStack
                                 $ map translatePackageDB (withPackageDB lbi)
          }
    session <- initSession initParams config
    setGhcOpts session exts
    let loadModules = mconcat $ map updateSourceFileFromFile mods
    updateSession session loadModules (sendResponse . ResponseUpdateSession . Just)
    sendResponse $ ResponseUpdateSession Nothing
    -- dumpIdInfo session
    return session
  where
    unlessOverwritten :: Eq a => (SessionConfig -> a) -> a -> a
    unlessOverwritten f x = if f optConfig == f defaultSessionConfig
                               then x
                               else f optConfig

{-------------------------------------------------------------------------------
  Mapping from cabal package info to ide-backend or ghc settings
-------------------------------------------------------------------------------}

-- | Translate build info to ghc options
buildInfoGhcOpts :: LocalBuildInfo -> BuildInfo -> ComponentLocalBuildInfo
                 -> [String]
buildInfoGhcOpts lbi bi clbi = concat [
      C.extensionsToFlags (compiler lbi) (defaultExtensions bi)
    , excludeOpts (fromMaybe [] (lookup (C.compilerFlavor (compiler lbi)) (options bi)))
    , "-hide-all-packages" : map (packageIdFlag . fst) (componentPackageDeps clbi)
    ]

-- | Exclude optimization flags (-O*) because they conflict with the
-- GHC interpreter mode.
--
-- Currently the error from GHC 7.8 is rather misleading (\"bytecode
-- compiler can't handle unboxed tuple\"). In GHC 7.10 the message
-- will be improved with an explicit exception from GHC describing the
-- problem, but in any case we filter out @-O@ to avoid this problem.
excludeOpts :: [String] -> [String]
excludeOpts = filter (not . isPrefixOf "-O")

componentModules :: FilePath -> Component -> IO [FilePath]
componentModules cabalRoot comp = do
    let (files, mods) = aux comp
    files' <- mapM (locateFile cabalRoot hsSourceDirs) files
    mods'  <- mapM (moduleToFile cabalRoot hsSourceDirs) (mods ++ otherModules)
    return $ files' ++ mods'
  where
    aux :: Component -> ([FilePath], [C.ModuleName])
    aux (CLib  Library{..})    = ([], exposedModules)
    aux (CExe  Executable{..}) = ([modulePath], [])
    aux (CTest TestSuite{..})  =
      case testInterface of
        TestSuiteExeV10 _ fp   -> ([fp], [])
        TestSuiteLibV09 _ md   -> ([], [md])
        TestSuiteUnsupported _ -> throw $ userError "Unsupported test suite"
    aux (CBench Benchmark{..}) =
      case benchmarkInterface of
        BenchmarkExeV10 _ fp   -> ([fp], [])
        BenchmarkUnsupported _ -> throw $ userError "Unsupported benchmark"

    BuildInfo{..} = componentBuildInfo comp

-- | Translate installed package ID to ghc flag
packageIdFlag :: InstalledPackageId -> String
packageIdFlag (InstalledPackageId ipid) = "-package-id " ++ ipid

-- | Translate Cabal's package DB to ide-backend's
--
-- (This is only necessary because ide-backend currently uses Cabal-ide-backend,
-- a private copy of Cabal. Once that is gone this function is no longer
-- required).
translatePackageDB :: C.PackageDB -> PackageDB
translatePackageDB C.GlobalPackageDB        = GlobalPackageDB
translatePackageDB C.UserPackageDB          = UserPackageDB
translatePackageDB (C.SpecificPackageDB fp) = SpecificPackageDB fp

-- | Translate module name to filepath
--
-- (ide-backend deals exclusively in filenames)
moduleToFile :: FilePath -> [FilePath] -> C.ModuleName -> IO FilePath
moduleToFile cabalRoot srcDirs nm =
    go [cabalRoot </> dir </> C.toFilePath nm <.> ext
       | dir <- srcDirs, ext <- exts
       ]
  where
    go :: [FilePath] -> IO FilePath
    go []     = throwIO $ userError $ "Could not find module " ++ show nm
    go (p:ps) = do exists <- doesFileExist p
                   if exists then return p
                             else go ps

    exts :: [String]
    exts = ["hs", "lhs"]

-- | Find modules specified as files (for executables/tests/benchmarks)
locateFile :: FilePath -> [FilePath] -> FilePath -> IO FilePath
locateFile cabalRoot srcDirs fp =
    go [cabalRoot </> dir </> fp
       | dir <- srcDirs
       ]
  where
    go :: [FilePath] -> IO FilePath
    go []     = throwIO $ userError $ "Could not find file " ++ show fp
    go (p:ps) = do exists <- doesFileExist p
                   if exists then return p
                             else go ps


-- | Set GHC options.
setGhcOpts  :: IdeSession -> [Extension] ->  IO ()
setGhcOpts sess exts =
  updateSession sess (updateGhcOpts (map showExt exts)) (const (return ()))
  where showExt :: Extension -> String
        showExt g =
          case g of
            EnableExtension e -> "-X" <> show e
            DisableExtension e -> "-XNo" <> show e
            UnknownExtension e -> "-X" <> show e

{-------------------------------------------------------------------------------
  Compute available build targets

  (TODO: I would have expected this to be available in Cabal somewhere
  but I cannot find it.)

  This is based on the parsing code in Cabal.Distribution.Simple.BuildTarget,
  in particular matchComponentKind.
-------------------------------------------------------------------------------}

availableTargets :: LocalBuildInfo -> [String]
availableTargets LocalBuildInfo{..} = go localPkgDescr
  where
    go :: PackageDescription -> [String]
    go PackageDescription{..} = concat [
        [ "library"                      | Just _         <- [library]   ]
      , [ "executable:" ++ exeName       | Executable{..} <- executables ]
      , [ "test-suite:" ++ testName      | TestSuite{..}  <- testSuites  ]
      , [ "benchmark:"  ++ benchmarkName | Benchmark{..}  <- benchmarks  ]
      ]

-- | Resolve a build target
--
-- This mostly relies on standard Cabal infrastructure, but additionally
-- supports the "library" target to refer to the library compnent of the package
-- (Cabal only supports the 'lib:package' syntax).
resolveBuildTarget :: LocalBuildInfo -> String -> IO (Component, ComponentLocalBuildInfo, [Extension])
resolveBuildTarget lbi@LocalBuildInfo{..} target = do
    buildTargets <- case target of
                      "library"  -> return [BuildTargetComponent CLibName]
                      _otherwise -> readBuildTargets localPkgDescr [target]
    case buildTargets of
      []    -> throwIO $ userError "Invalid target"
      _:_:_ -> throwIO $ userError "Ambiguous target"
      [BuildTargetComponent component] -> do
        let !comp = getComponent               localPkgDescr component
            !clbi = getComponentLocalBuildInfo lbi           component
        exts <- getExtensions localPkgDescr target
        return (comp, clbi, exts)
      [BuildTargetModule _component _module] ->
        throwIO $ userError "Unsupported target"
      [BuildTargetFile _component _file] ->
        throwIO $ userError "Unsupported target"

-- | Resolve language extensions for a given buiold target
--
-- Loads the list of langauge extensions declared in the `extensions` or
-- `default-extensions` field of the cabal file for a given build target.
getExtensions :: PackageDescription -> String -> IO [Extension]
getExtensions PackageDescription{..} target =
  let (targetType, targetRest) = break (== ':') target
      _:targetName = targetRest
      targetBuildInfo =
        case targetType of
          "library" -> fmap libBuildInfo library
          "executable" -> fmap buildInfo (find ((== targetName) . exeName) executables)
          "test-suite" -> fmap testBuildInfo (find ((== targetName) . testName) testSuites)
          "benchmark"  -> fmap benchmarkBuildInfo (find ((== targetName) . benchmarkName) benchmarks)
          _  -> return mempty
  in case targetBuildInfo of
       Just !bi -> return $ usedExtensions bi
       Nothing  -> throwIO $ userError "Unsupported target"
