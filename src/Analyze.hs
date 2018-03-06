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

import GitHub.Data.Definitions (simpleOwnerLogin)
import qualified GitHub.Auth as Auth
import qualified GitHub.Data.Repos as Github
import qualified GitHub.Data.Name as Github
import qualified GitHub.Endpoints.Repos.Contents as Github

import Network.HTTP.Simple
import Network.URI (URI)
import System.IO (IO)

import Data


analyze ::  Maybe Auth.Auth -> ProjectInfo -> IO ()
analyze auth repo = do
    (Right uri) <- Github.archiveFor' auth
                      (Github.N $ projectOwner repo)
                      (Github.N $ projectName repo)
                      Github.ArchiveFormatTarball
                      Nothing
    runConduitRes (getArchiveContent uri .| CL.mapM_ (liftIO . print))


getArchiveContent :: URI -> ConduitM () CT.TarChunk (ResourceT IO) ()
getArchiveContent uri =
    pipeline
    where
      response =
        httpSource ( parseRequest_ $ show uri) getResponseBody
      pipeline =
          response
          .| ungzip
          .| CT.untarChunks


-- printFileNames :: ConduitM CT.TarChunk ()
