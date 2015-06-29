{-# LANGUAGE CPP #-}

module Main where

import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 (hPutStrLn)
import IdeSession.Client
import IdeSession.Client.CmdLine
import IdeSession.Client.JsonAPI (apiDocs, toJSON, fromJSON)
import IdeSession.Client.Util.ValueStream (newStream, nextInStream)
import System.IO (stdin, stdout)

main :: IO ()
main = do
  input <- newStream stdin
  -- We separate JSON values in the output by newlines, so that
  -- editors have a means to split the input into separate
  -- values. (The parser on the Haskell side is a lot more
  -- sophisticated and deals with whitespace properly)
  let clientIO = ClientIO
        { sendResponse = hPutStrLn stdout . encode . toJSON
        , receiveRequest = fmap fromJSON $ nextInStream input
        }
  opts <- getCommandLineOptions
  case optCommand opts of
    ShowAPI -> putStrLn apiDocs
    StartEmptySession opts' -> startEmptySession clientIO opts opts'
#ifdef USE_CABAL
    StartCabalSession opts' -> startCabalSession clientIO opts opts'
    ListTargets filepath -> sendTargetsList clientIO filepath
#else
    StartCabalSession _ -> notBuiltWithCabal
    ListTargets _ -> notBuiltWithCabal
#endif

notBuiltWithCabal :: IO ()
notBuiltWithCabal = putStrLn $
    "ide-backend-client not built with Cabal, so it cannot be used with cabal projects.  " ++
    "Build it with -fuse-cabal in order to use this functionality."
