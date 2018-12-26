{-# LANGUAGE OverloadedStrings #-}
module Projects (toProjectInfo) where


import qualified GitHub.Auth as Auth
import qualified GitHub.Data as Github
import qualified GitHub.Data.Name as Github
import qualified GitHub.Data.Repos as Github
import qualified GitHub.Endpoints.Search as Github
import qualified GitHub.Endpoints.Repos.Contents as Github
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

import Debug.Trace

import Data

toProjectInfo :: Maybe Auth.Auth -> Github.Repo -> IO ProjectInfo
toProjectInfo auth repo = do
  revision <- getLatestRevision auth repo
  archiveUri <- getArchiveUri auth repo
  dependencies <- getProjectDependencies auth repo
  return ProjectInfo
    { projectName = (Github.untagName . Github.repoName) repo
    , projectOwner = (Github.untagName . Github.simpleOwnerLogin . Github.repoOwner) repo
    , repoUrl = unpack $ (Github.getUrl . Github.repoUrl) repo
    , projectRevision = revision
    , archiveUrl =  toMaybe archiveUri
    , dependencies = dependencies
    , stars = toInteger . Github.repoStargazersCount $ repo
    , contributors = Nothing
    , commits = Nothing
    , forks = fmap toInteger $ Github.repoForks repo
    , createdAt = Github.repoCreatedAt repo
    }


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

getLatestRevision :: Maybe Auth.Auth -> Github.Repo -> IO Revision
getLatestRevision auth repo =
    return DefaultBranchHead


getProjectDependencies :: Maybe Auth.Auth -> Github.Repo -> IO [Text]
getProjectDependencies auth repo = do
  result <- Github.contentsFor' auth (Github.simpleOwnerLogin $ Github.repoOwner repo) (Github.repoName repo) "package.json" Nothing
  case result of
    Left e -> do print e
                 return []
    Right content -> return $ getDependencies $ getFileContent content


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
