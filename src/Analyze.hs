{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Analyze (analyze) where

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

import Debug.Trace


analyze :: ProjectInfo -> IO ()
analyze repo = do
    print $ "Processing " <> projectName repo <> " at " <> T.pack ( show (archiveUrl repo)) <> ": "
    count <- runConduitRes (
      getArchiveContent (archiveUrl repo)
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


countJSFiles :: ConduitT CT.TarChunk Void (ResourceT IO) Integer
countJSFiles =  CT.withEntries getJSFile
                .| CL.map (const (1 :: Integer))
                .| CL.fold (+) 0


getJSFile :: Monad m => CT.Header -> ConduitT ByteString (FilePath, ByteString) m ()
getJSFile header = when (isJsFile header) $ do
    content <- CL.fold (<>) mempty
    yield (CT.headerFilePath header, content)
    where
      isJsFile header = ".js" `T.isSuffixOf` (T.pack . CT.headerFilePath) header
