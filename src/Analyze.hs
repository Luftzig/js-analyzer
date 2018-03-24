{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Analyze (analyze) where

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans.Resource (MonadResource, ResourceT)

import Data.ByteString (ByteString)

import Data.Conduit
import Data.Conduit.Zlib (ungzip)
import qualified Data.Conduit.Tar as CT
import qualified Data.Conduit.List as CL

import Network.HTTP.Simple
import System.IO (IO)

import Data


analyze :: ProjectInfo -> IO ()
analyze repo =
    runConduitRes (getArchiveContent (archiveUrl repo) .| CL.mapM_ (liftIO . print))


getArchiveContent :: Maybe URL -> ConduitM () CT.TarChunk (ResourceT IO) ()
getArchiveContent (Just uri) =
    pipeline
    where
      response =
        httpSource ( parseRequest_ $ show uri) getResponseBody
      pipeline =
          response
          .| ungzip
          .| CT.untarChunks
getArchiveContent Nothing = mempty


-- printFileNames :: ConduitM CT.TarChunk ()
