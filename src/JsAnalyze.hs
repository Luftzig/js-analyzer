{-# LANGUAGE OverloadedStrings #-}
module JsAnalyze (analyze, processAst) where

import Conduit
import qualified Data.Conduit.Combinators as CC
import Data.Aeson
import Data.ByteString (ByteString, unpack, hGetContents, hPutStr, putStr)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Char8 (pack)
import Data.Conduit.Process
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Vector as V
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process.Typed (ProcessConfig, byteStringInput, proc, readProcess, setStdin)

import Control.Lens
import Data.Aeson.Lens

import Debug.Trace

import Data
import Analyze.WalkAst


-- Maybe call esprima or acorn parser and parse the result JSON?

analyze :: FilePath -> ByteString -> IO (Either ParseError FileStats)
analyze path content = do
  let numLines = toInteger . length . BC.lines
--  liftIO $ putStrLn $ "Processing file " ++ path
  astText <- extractAndParseFileContent content
  return $ either
    (Left . (ParseError path))
    Right (do
     rawJson <- astText
     astResult <- processAst rawJson
     Right $ astResult {fileName=path, linesOfCode=numLines content}
     )


extractAndParseFileContent :: SBS.ByteString -> IO (Either Text LBS.ByteString)
extractAndParseFileContent content = do
  let inputStream = byteStringInput $ LBS.fromStrict content
  (exitCode, result, errors) <- System.Process.Typed.readProcess $ setStdin inputStream esprima
  return $ if exitCode == ExitSuccess then Right result else Left $ decodeUtf8 $ LBS.toStrict errors


esprima :: ProcessConfig () () ()
esprima =
  System.Process.Typed.proc "node" ["js-parser" </> "index.js"]


processAst :: LBS.ByteString -> Either Text FileStats
processAst json =
  let
    value = eitherDecode json :: Either String Value
  in
    case value of
      Right ast -> Right $ countStructures ast
      Left e -> trace ("Failed to decode AST: " ++ e ) $ Left $ T.pack ("Failed to decode AST: " ++ e)


countStructures :: Value -> FileStats
countStructures =
  (foldl sumFileStats emptyStats) . (visitNode countPatterns)


