module JsAnalyze (analyze) where

import Conduit
import Data.ByteString (ByteString, unpack, hGetContents, hPutStr, putStr)
import Data.ByteString.Char8 (pack)
import Data.Conduit.Process
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import GHC.IO.Handle (hClose, hFlush)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (createProcess, proc, waitForProcess, CreateProcess, StdStream (..))

import Data


-- Maybe call esprima or acorn parser and parse the result JSON?

analyze :: FilePath -> ByteString -> IO (Either ParseError FileStats)
analyze path content = do
  liftIO $ putStrLn $ "Processing file " ++ path
  processFileContent content
  return $ Right $ statsForPath path


processFileContent :: ByteString -> IO (Either ParseError ByteString)
processFileContent content = do
  (Just inStream, Just outStream, Just errorStream, handle) <- createProcess (esprima { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe })
  hPutStr inStream content
  hFlush inStream
  hClose inStream
  result <- hGetContents outStream
  errors <- hGetContents errorStream
  exitCode <- waitForProcess handle
  return $ if exitCode == ExitSuccess then Right result else Left $ decodeUtf8 errors


esprima :: CreateProcess
esprima =
  proc "node" ["js-parser" </> "index.js"]


