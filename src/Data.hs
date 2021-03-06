{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Data where

import Control.Applicative

import Data.Text (Text, intercalate, breakOn, tail, null)
import Data.Traversable (Traversable)
import Data.Time.Clock (UTCTime)
import GHC.Generics
import Data.Aeson

type Name = Text

type URL = String

data Revision = Revision
                  { commitId :: Text
                  , committedDate :: UTCTime
                  , archiveUrl :: Maybe URL
                  , dependencies :: [Text]
                  } deriving (Show, Generic)

instance ToJSON Revision where

instance FromJSON Revision where


data ProjectInfo = ProjectInfo
  { projectName :: Name
  , projectOwner :: Name
  , revisions :: [Revision]
  , repoUrl :: URL
  , stars :: Integer
  , contributors :: Maybe Integer
  , commits :: Integer
  , forks :: Integer
  , createdAt :: UTCTime
  } deriving (Show, Generic)


projectId :: ProjectInfo -> Text
projectId p = intercalate "/" [projectOwner p, projectName p]


splitId :: Text -> (Text, Text)
splitId id =
  let (owner, name) = breakOn "/" id
  in
    (owner, if Data.Text.null name then "" else Data.Text.tail name)

instance FromJSON ProjectInfo where
  parseJSON (Object o) = do
    id <- o .: "id"
    let (projectName, projectOwner) = splitId id
    repoUrl <- o .: "url"
    revisions <- o .: "revisions"
    stars <- o .: "stars"
    contributors <- o .: "contributors"
    commits <- o .: "commits"
    forks <- o .: "forks"
    createdAt <- o .: "createdAt"
    return $ ProjectInfo {..}


instance ToJSON ProjectInfo where
  toJSON p =
    object [ "id" .= projectId p
           , "url" .= repoUrl p
           , "revisions" .= revisions p
           , "stars" .= stars p
           , "contributors" .= contributors p
           , "commits" .= commits p
           , "forks" .= forks p
           , "createdAt" .= createdAt p
           ]


data ProjectStats = ProjectStats
  { projectInfo :: ProjectInfo
  , filesStats :: [FileStats]
  } deriving Show


data FileStats = FileStats
      { fileName :: FilePath
      , linesOfCode :: Integer
      , forCount :: Integer
      , forInCount :: Integer
      , forOfCount :: Integer
      , whileCount :: Integer
      , forEachCount :: Integer
      , mapCount :: Integer
      , filterCount :: Integer
      , reduceCount :: Integer
      }
    | FailedStats
      { fileName :: FilePath
      , failReason :: Text
      }
    deriving (Show, Generic)


instance ToJSON FileStats

instance FromJSON FileStats


emptyStats :: FileStats
emptyStats =
  FileStats
    { fileName = "Unknown"
    , linesOfCode = 0
    , forCount = 0
    , forInCount = 0
    , forOfCount = 0
    , whileCount = 0
    , forEachCount = 0
    , mapCount = 0
    , filterCount = 0
    , reduceCount = 0
    }


sumFileStats :: FileStats -> FileStats -> FileStats
sumFileStats s1 s2 =
  s1 { forCount = forCount s1 + forCount s2
    , forInCount = forInCount s1 + forInCount s2
    , forOfCount = forOfCount s1 + forOfCount s2
    , whileCount = whileCount s1 + whileCount s2
    , forEachCount = forEachCount s1 + forEachCount s2
    , mapCount = mapCount s1 + mapCount s2
    , filterCount = filterCount s1 + filterCount s2
    , reduceCount = reduceCount s1 + reduceCount s2
  }


statsForPath :: FilePath -> FileStats
statsForPath path =
  emptyStats { fileName = path }

type AnalyzeResult error = Either error FileStats

data ParseError = ParseError FilePath Text deriving (Show)
