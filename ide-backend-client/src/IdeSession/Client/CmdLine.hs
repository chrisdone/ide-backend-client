{-# LANGUAGE OverloadedStrings #-}
module IdeSession.Client.CmdLine (
    -- * Types
    Options(..)
  , Command(..)
  , EmptyOptions(..)
  , InterfaceConfig(..)
  , CabalOptions(..)
    -- * Parsing
  , getCommandLineOptions
  ) where

import Data.Monoid
import Options.Applicative
import System.Directory (getHomeDirectory)
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import qualified Options.Applicative.Help.Chunk as Chunk
import qualified Text.PrettyPrint.ANSI.Leijen   as Doc

import IdeSession

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

data Options = Options {
    optInitParams :: SessionInitParams
  , optConfig     :: SessionConfig
  , optIFConfig   :: InterfaceConfig
  , optCommand    :: Command
  }
  deriving Show

data Command =
    StartEmptySession EmptyOptions
  | StartCabalSession CabalOptions
  | ListTargets FilePath
  | ShowAPI
  deriving Show

data EmptyOptions = EmptyOptions
  deriving Show

data InterfaceConfig = InterfaceConfig {
    bufferIO :: Bool
  } deriving Show

data CabalOptions = CabalOptions {
    cabalTarget :: String
  , cabalRoot   :: FilePath
  }
  deriving Show

{-------------------------------------------------------------------------------
  Parsers
-------------------------------------------------------------------------------}

parseOptions :: Parser Options
parseOptions = Options
  <$> parseInitParams
  <*> parseConfig
  <*> parseIFConfig
  <*> parseCommand

parseCommand :: Parser Command
parseCommand = subparser $ mconcat [
      command "empty" $ info
        (helper <*> (StartEmptySession <$> parseEmptyOptions))
        (progDesc "Start an empty session")
    , command "cabal" $ info
        (helper <*> (StartCabalSession <$> parseCabalOptions))
        (progDesc "Start a Cabal session in directory DIR")
    , command "targets" $ info
        (helper <*> (ListTargets <$> argument str (metavar "DIR")))
        (progDesc "List the available build targets in directory DIR")
    , command "show-api" $ info
        (helper <*> pure ShowAPI)
        (progDesc "Show the JSON API documentation")
    ]

parseIFConfig :: Parser InterfaceConfig
parseIFConfig = InterfaceConfig
    <$> (flag False True $ mconcat [
          long "unbuffered"
        , help "Do not buffer stdout (useful for piping with interactive environments)"
        ])

parseEmptyOptions :: Parser EmptyOptions
parseEmptyOptions = pure EmptyOptions

parseCabalOptions :: Parser CabalOptions
parseCabalOptions = CabalOptions
    <$> (strOption $ mconcat [
            long "target"
          , metavar "NAME"
          , value "library"
          , showDefault
          , help "Target in the Cabal file"
          ])
    <*> argument str (metavar "DIR")

{-------------------------------------------------------------------------------
  Parsers for ide-backend types
-------------------------------------------------------------------------------}

parseInitParams :: Parser SessionInitParams
parseInitParams = SessionInitParams
    <$> useDefault sessionInitCabalMacros
    <*> (defaultTo sessionInitGhcOptions . many . strOption $ mconcat [
            long "ghc-option"
          , metavar "OPT"
          , helpDoc . Just $ Doc.vcat [
                "GHC option to use when initializing the session."
              , "Can be used multiple times; overrides default if used."
              ]
          ])
    <*> (defaultTo sessionInitRelativeIncludes . fmap splitSearchPath' . strOption $ mconcat [
            long "include"
          , metavar "PATH"
          , value ""
          , help "Include path (relative to session directory)."
          ])
    <*> useDefault sessionInitTargets
    <*> (defaultTo sessionInitRtsOpts . many . strOption $ mconcat [
            long "rts-option"
          , metavar "OPT"
          , helpDoc . Just $ Doc.vcat [
                "RTS option to use when initializing the session."
              , "Can be used multiple times; overrides default if used."
              ]
          ])
  where
    useDefault :: (SessionInitParams -> a) -> Parser a
    useDefault fld = pure (fld defaultSessionInitParams)

    defaultTo :: (SessionInitParams -> [a]) -> Parser [a] -> Parser [a]
    defaultTo fld = fmap $ \xs ->
      case xs of
        [] -> fld defaultSessionInitParams
        _  -> xs

parseConfig :: Parser SessionConfig
parseConfig = SessionConfig
    <$> (strOption $ mconcat [
            long "dir"
          , metavar "DIR"
          , value (configDir defaultSessionConfig)
          , showDefault
          , help "Directory to use to store the session files"
          ])
    <*> (defaultTo configExtraPathDirs . fmap splitSearchPath' . strOption $ mconcat [
            long "path"
          , metavar "PATH"
          , value ""
          , help "Additional search path to add to search for tools (ghc, ide-backend-server, etc.)."
          ])
    <*> useDefault configInProcess
    <*> useDefault configGenerateModInfo
    <*> (fmap mkPackageDBStack . strOption $ mconcat [
            long "package-db"
          , metavar "DIR"
          , value ""
          , helpDoc . Just $ Doc.vcat [
                "Package DBs to use for this session."
              , autoWrap "This takes the form of a search path; the resulting DB stack will consist of the global DB and the specified specific DBs."
              ]
          ])
    <*> useDefault configLicenseExc
    <*> useDefault configLicenseFixed
    <*> useDefault configLog
    <*> (fmap not . switch $ mconcat [
            long "keep-tmp-files"
          , help "Do not delete the session directory on termination"
          ])
  where
    useDefault :: (SessionConfig -> a) -> Parser a
    useDefault fld = pure (fld defaultSessionConfig)

    defaultTo :: (SessionConfig -> [a]) -> Parser [a] -> Parser [a]
    defaultTo fld = fmap $ \xs ->
      case xs of
        [] -> fld defaultSessionConfig
        _  -> xs

    mkPackageDBStack :: String -> PackageDBStack
    mkPackageDBStack ""   = configPackageDBStack defaultSessionConfig
    mkPackageDBStack path = GlobalPackageDB
                          : map SpecificPackageDB (splitSearchPath' path)

{-------------------------------------------------------------------------------
  Top-level API
-------------------------------------------------------------------------------}

getCommandLineOptions :: IO Options
getCommandLineOptions = execParser opts
  where
    opts = info
      (helper <*> parseOptions)
      (mconcat [
          fullDesc
        , progDesc "Start a new session"
        , header "ide-backend-client: JSON interface to ide-backend"
        ])

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

deriving instance Show SessionConfig

instance Show (a -> b) where
  show _ = "<<function>>"

-- | Version of splitSearchPath that returns the empty list on empty string
-- and expands home directories.
splitSearchPath' :: String -> [FilePath]
splitSearchPath' = map expandHomeDir . split
  where
    split "" = []
    split xs = splitSearchPath xs

-- | Expand home directory
expandHomeDir :: FilePath -> FilePath
expandHomeDir path = unsafePerformIO $ do
    home <- getHomeDirectory

    let expand :: FilePath -> FilePath
        expand []       = []
        expand ('~':xs) = home ++ expand xs
        expand (x:xs)   = x     : expand xs

    return $ expand path

-- | Insert automatic linebreaks
autoWrap :: String -> Doc.Doc
autoWrap = Chunk.extractChunk . Chunk.paragraph
