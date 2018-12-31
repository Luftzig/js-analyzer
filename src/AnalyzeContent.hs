{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module AnalyzeContent (analyze) where

import Conduit
import Control.Monad (when, forM_)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans.Resource (MonadResource, MonadUnliftIO, ResourceT)

import Data.Aeson (encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as B
import Data.ByteString.Lazy (toStrict)
import qualified Data.Text as T

import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import Data.Conduit.Zlib (ungzip)
import qualified Data.Conduit.Tar as CT
import qualified Data.Conduit.List as CL

import Data.Semigroup ((<>))

import Network.HTTP.Simple
import System.Directory (createDirectoryIfMissing)
import System.IO (FilePath, IO)
import System.FilePath ((</>), (<.>))

import Data
import qualified JsAnalyze as JS


analyze :: FilePath -> ProjectInfo -> IO ()
analyze outDir repo = do
    print $ "Processing " <> projectName repo <> ": "
    createDirectoryIfMissing True $ outDir </> (T.unpack $ projectOwner repo)
    forM_ (revisions repo) (runConduitRes . processRevision)
    where
      processRevision (Revision{archiveUrl=archive, commitId=cId}) =
        getArchiveContent (archive)
          .| filterFiles
          .| analyzeFiles
          .| convertErrors
          .| writeResults outDir repo cId


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


getJSFile :: Monad m => CT.Header -> ConduitT ByteString (FilePath, ByteString) m ()
getJSFile header = when (isJsFile header && (not . isMinJs) header) $ do
    content <- CL.fold (<>) mempty
    yield (CT.headerFilePath header, content)
    where
      isJsFile header = ".js" `T.isSuffixOf` (T.toCaseFold . T.pack . CT.headerFilePath) header
      isMinJs header = ".min.js" `T.isSuffixOf` (T.toCaseFold . T.pack . CT.headerFilePath) header
analyzeFiles :: (MonadThrow m, MonadUnliftIO m) => ConduitT (FilePath, ByteString) (Either ParseError FileStats) m ()
analyzeFiles =
  CL.mapM (liftIO . uncurry JS.analyze)


convertErrors :: MonadIO m => ConduitT (Either ParseError FileStats) FileStats m ()
convertErrors =
  CL.map (either (\(ParseError file e) -> FailedStats file e) id)


writeResults :: MonadResource m => FilePath -> ProjectInfo -> T.Text -> ConduitT (FileStats) o m ()
writeResults outDir repo commitId =
    let pipe = yield "[\n"
              *> (CL.map (toStrict . encode)
              .| CC.intersperse ",\n")
              *> yield "\n]"

    in
      pipe .| sinkFile (outDir </> (T.unpack $ projectId repo) <> "-" <> (T.unpack commitId) <.> "json")
