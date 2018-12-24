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

import Network.URI (URI, uriToString)
import Data.Text (Text, unpack)

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


getProjectDependencies :: Maybe Auth.Auth -> Github.Repo -> IO [String]
getProjectDependencies auth repo =
  return []
