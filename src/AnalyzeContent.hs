{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module AnalyzeContent (analyze) where

import Control.Monad (when)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans.Resource (MonadResource, ResourceT)

import Data.Semigroup ((<>))

import Data.ByteString (ByteString)
import qualified  Data.Text as T

import Data.Conduit
import Data.Conduit.Zlib (ungzip)
import qualified Data.Conduit.Tar as CT
import qualified Data.Conduit.List as CL

import Network.HTTP.Simple
import System.IO (FilePath, IO)

import Data
import qualified JsAnalyze as JS

import Debug.Trace


analyze :: ProjectInfo -> IO ()
analyze repo = do
    print $ "Processing " <> projectName repo <> " at " <> T.pack ( show (archiveUrl repo)) <> ": "
    count <- runConduitRes (
      getArchiveContent (archiveUrl repo)
      .| filterFiles
      .| analyzeFiles
      .| printErrors
      .| countJSFiles
      )
    print count


getArchiveContent :: Maybe URL -> ConduitT () CT.TarChunk (ResourceT IO) ()
getArchiveContent (Just uri) =
    pipeline
    where
      response =
        httpSource ( parseRequest_ uri) getResponseBody
      pipeline =
          response
          .| ungzip
          .| CT.untarChunks
getArchiveContent Nothing = mempty


filterFiles :: MonadThrow m => ConduitT CT.TarChunk (FilePath, ByteString) m ()
filterFiles =
  CT.withEntries getJSFile


analyzeFiles :: MonadThrow m => ConduitT (FilePath, ByteString) (Either ParseError FileStats) m ()
analyzeFiles =
  CL.map (uncurry JS.analyze)


printErrors :: MonadIO m => ConduitT (Either ParseError FileStats) FileStats m ()
printErrors =
  CL.mapM printErrors .| CL.catMaybes
  where
    printErrors (Left e) = liftIO $ print e >> return Nothing
    printErrors (Right s) = liftIO $ return $ Just s


countJSFiles :: ConduitT a Void (ResourceT IO) Integer
countJSFiles =
  CL.map (const (1 :: Integer))
    .| CL.fold (+) 0


getJSFile :: Monad m => CT.Header -> ConduitT ByteString (FilePath, ByteString) m ()
getJSFile header = when (isJsFile header) $ do
    content <- CL.fold (<>) mempty
    yield (CT.headerFilePath header, content)
    where
      isJsFile header = ".js" `T.isSuffixOf` (T.toCaseFold . T.pack . CT.headerFilePath) header
