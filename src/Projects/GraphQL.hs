{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Projects.GraphQL (queryProjectInfo, makeRequestBody) where


import Control.Lens
import Data.Aeson (Value, decode, object, toJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Lens
import qualified Data.ByteString.Char8 as S8
import GitHub.Auth (Auth(BasicAuth, OAuth))
import Network.Wreq
import Data.Maybe (maybe, mapMaybe)
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Format (parseTimeOrError, defaultTimeLocale, iso8601DateFormat)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy as LT
import Data.ByteString as S
import NeatInterpolation

import Data

queryProjectInfo :: Maybe GitHub.Auth.Auth -> Text -> Text -> IO (Either String ProjectInfo)
queryProjectInfo githubAuth owner name = do
  let opts = maybe defaults (\authVal -> defaults & auth ?~ authVal) (fmap getAuthSettings githubAuth)
  response <- asJSON =<< postWith opts githubGraphQLApi (makeRequestBody owner name)
  return $ toProjectInfo $ response ^? responseBody


getAuthSettings :: GitHub.Auth.Auth -> Network.Wreq.Auth
getAuthSettings (BasicAuth u p) = basicAuth u p
getAuthSettings (OAuth t) = oauth2Token t


toProjectInfo :: Maybe Value -> Either String ProjectInfo
toProjectInfo (Just v) =
  let
    failed :: Text -> Maybe a -> Either Text a
    failed msg x =
      case x of
        Nothing -> Left msg
        Just x' -> Right x'
    pretty = LT.toStrict $ LT.decodeUtf8 $ encodePretty v :: Text
    go = do
      repository <- failed "repository" $ v ^? key "data" . key "repository"
      projectName <- failed "repository.name" $ repository ^? key "name" . _String
      projectOwner <- failed "repository.owner" $ repository ^? key "owner" . key "login" . _String
      createdAt <- failed "repository.createdAt" $ repository ^? key "createdAt" . _String . to _Date
      repoUrl <- failed "repository.url" $ repository ^? key "url" . _String . to T.unpack
      stars <- failed "repository.stargazers.totalCount" $ repository ^? key "stargazers" . key "totalCount" . _Integer
      forks <- failed "repository.forkCount.totalCount" $ repository ^? key "forkCount" . _Integer
      contributors <- Right $ Nothing
      branch <- failed "repository.defaultBranchRef" $ repository ^? key "defaultBranchRef"
      commits <- failed "branch.target.count.totalCount" $ branch ^? key "target" . key "count" . key "totalCount" . _Integer
      revisions <- failed "branch.target -> parseRevisions" $ branch ^? key "target" . to parseRevisions
      return ProjectInfo{..}
  in
    case go of
      Right p -> Right p
      Left msg -> Left $ T.unpack $ [text|Failed to parse $msg of $pretty|]
toProjectInfo Nothing = Left $ "Got empty response"


_Date :: Text -> UTCTime
_Date = (parseTimeOrError False defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S%Z"))) . T.unpack


parseRevisions :: Value -> [Revision]
parseRevisions v =
  let
    allNodes = v ^.. members . key "nodes" . _Array . ix 0
    makeRevision r = do
        commitId <- r ^? key "oid" . _String
        committedDate <- r ^? key "committedDate" . _String . to _Date
        archiveUrl <- Just $ r ^? key "tarballUrl" . _String . to T.unpack
        dependencies <- Just []
        return Revision{..}
  in
    mapMaybe makeRevision allNodes


makeRequestBody :: Text -> Text -> Value
makeRequestBody owner name =
  object [("query",
  toJSON [text|
    {
      repository(owner: "$owner", name: "$name") {
        ...RepoFragment
      }
    }

    fragment RepoFragment on Repository {
      name
      owner {
        login
      }
      url
      createdAt
      forkCount
      stargazers {
        totalCount
      }
      watchers {
        totalCount
      }
      defaultBranchRef {
        name
        target {
          ... on Commit {
            committedDate
            head: oid
            count: history(first: 1) {
              totalCount
            }
            $quarters
          }
        }
      }
    }

    fragment commits on CommitHistoryConnection {
      nodes {
        committedDate
        oid
        tarballUrl
      }
    }
  |]
  )]
  where
    quarters = T.unlines [ commitsForQuarters (T.pack $ show year) | year <- [2008..2019]]
    commitsForQuarters year = [text|
            year_${year}_q1: history(since: "$year-01-01T00:00:00+00:00", until: "$year-03-31T23:59:59+00:00", first: 1) {
              ...commits
            }
            year_${year}_q2: history(since: "$year-04-01T00:00:00+00:00", until: "$year-06-31T23:59:59+00:00", first: 1) {
              ...commits
            }
            year_${year}_q3: history(since: "$year-07-01T00:00:00+00:00", until: "$year-09-30T23:59:59+00:00", first: 1) {
              ...commits
            }
            year_${year}_q4: history(since: "$year-10-01T00:00:00+00:00", until: "$year-12-31T23:59:59+00:00", first: 1) {
              ...commits
            }
    |]

githubGraphQLApi = "https://api.github.com/graphql"
