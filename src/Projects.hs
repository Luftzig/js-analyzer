{-# LANGUAGE OverloadedStrings #-}
module Projects (toProjectInfo) where


import qualified GitHub.Auth as Auth
import qualified GitHub.Data as Github
import qualified GitHub.Data.Name as Github
import qualified GitHub.Data.Repos as Github
import qualified GitHub.Endpoints.Search as Github
import qualified GitHub.Endpoints.Repos.Contents as Github
import qualified GitHub.Endpoints.Repos.Collaborators as Github
import qualified GitHub.Request as Github
import GitHub.Data.Definitions (simpleOwnerLogin)
import GitHub.Data.Request (query)

import Data.Aeson.Lens
import Control.Lens

import Network.URI (URI, uriToString)
import qualified Data.ByteString.Base64 as Base64
import Data.Char (isSpace)
import qualified Data.Text as T
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V

import Debug.Trace

import Data
import Projects.GraphQL

toProjectInfo :: Maybe Auth.Auth -> Github.Repo -> IO ProjectInfo
toProjectInfo auth repo = do
--  revision <- getLatestRevision auth repo
--  archiveUri <- getArchiveUri auth repo
--  dependencies <- getProjectDependencies auth repo
-- auth repo removed as there are no guarantees we'll get the real number. Should probably collect this manually...
--  contributors <- getContributorsCount
--  return ProjectInfo
--    { projectName = (Github.untagName . Github.repoName) repo
--    , projectOwner = (Github.untagName . Github.simpleOwnerLogin . Github.repoOwner) repo
--    , repoUrl = unpack $ (Github.getUrl . Github.repoUrl) repo
--    , projectRevision = revision
--    , archiveUrl =  toMaybe archiveUri
--    , dependencies = dependencies
--    , stars = toInteger . Github.repoStargazersCount $ repo
--    , contributors = Nothing
--    , commits = Nothing
--    , forks = fmap toInteger $ Github.repoForks repo
--    , createdAt = Github.repoCreatedAt repo
--    }
  let owner = (Github.untagName . Github.simpleOwnerLogin . Github.repoOwner) repo
  let name =  (Github.untagName . Github.repoName) repo
  result <- queryProjectInfo auth owner name
  case result of
    Right project -> do
      withDeps <- mapM (getRevisionDependencies auth owner name) (revisions project)
      return $ project { revisions = withDeps }
    Left e -> do
      print e
      error "Damn"


toUrl :: URI -> Maybe URL
toUrl u = Just $ uriToString id u ""


toMaybe :: Either a URI -> Maybe URL
toMaybe = either (const Nothing) toUrl


getArchiveUri :: Maybe Auth.Auth -> Github.Repo -> IO (Either Github.Error URI)
getArchiveUri auth repo =
     Github.archiveFor' auth
                  (Github.simpleOwnerLogin $ Github.repoOwner repo)
                  (Github.repoName repo)
                  Github.ArchiveFormatTarball
                  Nothing


ownerLogin = Github.simpleOwnerLogin . Github.repoOwner

getRevisionDependencies :: Maybe Auth.Auth -> Text -> Text -> Revision -> IO Revision
getRevisionDependencies auth owner name rev = do
  result <- Github.contentsFor' auth (Github.N owner) (Github.N name) "package.json" (Just $ commitId rev)
  case result of
    Left e -> do print e
                 return rev
    Right content -> return $ rev {dependencies = getDependencies $ getFileContent content }


getContributorsCount :: Maybe Auth.Auth -> Github.Repo -> IO (Maybe Integer)
getContributorsCount auth repo = do
  result <- Github.collaboratorsOn' auth (ownerLogin repo) (Github.repoName repo)
  case result of
    Right users -> return $ Just $ toInteger $ V.length users
    Left e -> do
      print ("Failed to get contributors" ++ show e)
      return $ Nothing


getFileContent :: Github.Content -> Maybe Text
getFileContent (Github.ContentFile fileData) = Just $ Github.contentFileContent fileData
getFileContent _ = Nothing


getDependencies :: Maybe Text -> [Text]
getDependencies (Just encoded) =
  case Base64.decode $ encodeUtf8 $ (T.filter (not . isSpace)) encoded of
    Left e -> trace ("Error with decoding: " ++ e) $ []
    Right packageJson ->
      (getKeysOf "dependencies" packageJson) ++ (getKeysOf "devDependencies" packageJson)
  where
    getKeysOf k input =
      ifoldl (\i a _ -> i:a) [] $ input ^? key k . _Object ^. folded
getDependencies _ = []
