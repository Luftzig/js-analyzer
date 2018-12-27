{-# LANGUAGE OverloadedStrings #-}
module JsAnalyze (analyze, processAst) where

import Conduit
import Data.Aeson
import Data.ByteString.Lazy (ByteString, unpack, hGetContents, hPutStr, putStr, toStrict)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as SBS
import Data.ByteString.Char8 (pack)
import Data.Conduit.Process
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Vector as V
import GHC.IO.Handle (hClose, hFlush)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (createProcess, proc, waitForProcess, CreateProcess, StdStream (..))

import Control.Lens
import Data.Aeson.Lens

import Debug.Trace

import Data


-- Maybe call esprima or acorn parser and parse the result JSON?

analyze :: FilePath -> SBS.ByteString -> IO (Either ParseError FileStats)
analyze path content = do
  liftIO $ putStrLn $ "Processing file " ++ path
  astText <- extractAndParserFileContent content
  return $ do
     rawJson <- astText
     astResult <- processAst rawJson
     Right $ astResult {fileName=path}
--     (\stats -> stats {fileName=path}) $ fmap processAst $ rawJson


extractAndParserFileContent :: SBS.ByteString -> IO (Either ParseError ByteString)
extractAndParserFileContent content = do
  (Just inStream, Just outStream, Just errorStream, handle) <- createProcess (esprima { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe })
  hPutStr inStream $ LBS.fromStrict content
  hFlush inStream
  hClose inStream
  result <- hGetContents outStream
  errors <- hGetContents errorStream
  exitCode <- waitForProcess handle
  return $ if exitCode == ExitSuccess then Right result else Left $ decodeUtf8 $ toStrict errors


esprima :: CreateProcess
esprima =
  proc "node" ["js-parser" </> "index.js"]


processAst :: ByteString -> Either ParseError FileStats
processAst json =
  let
    value = decode json :: Maybe Value
  in
    case value of
      Just ast -> Right $ countStructures ast
      Nothing -> Left "Failed to read AST"


countStructures :: Value -> FileStats
countStructures val = do
  let nType = val ^? key "type"._String
--  obj <- val ^? _Object
--  nodeType <- val ^? key "type" . _String
--  containedArray <- obj ^.. _2 . _Array
--  containedObjs <- obj ^.. _2 . _Object
  let cs = val ^.. members
  let memberObjects = val ^.. members . _Object
  let memberArrays = val ^.. members . _Array
  let fromMembers = trace ("members" ++ (show memberArrays) ++ (show memberObjects)) $ map countStructures cs
  case nType of
    Nothing -> emptyStats
    Just typeName -> trace ("Found type " ++ (T.unpack typeName)) $ foldl sumFileStats emptyStats $ fromMembers


