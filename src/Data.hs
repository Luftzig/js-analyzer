{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Data where

import Data.Text (Text, intercalate)
import Data.Traversable (Traversable)
import Data.Time.Clock (UTCTime)
import GHC.Generics
import Data.Aeson

import qualified Text.Parsec.Error as Parsec

type Name = Text

type URL = String

data Revision = Commit Text
              | Tag Name
              | DefaultBranchHead


instance Show Revision where
  show (Commit t) = "#" ++ (show t)
  show (Tag t) = "tag " ++ (show t)
  show DefaultBranchHead = "HEAD"


instance ToJSON Revision where
  toJSON (Commit t) = object ["commit" .= t]
  toJSON (Tag t) = object ["tag" .= t]
  toJSON (DefaultBranchHead) = object ["tag" .= ("HEAD" :: Text)]


data ProjectInfo = ProjectInfo
  { projectName :: Name
  , projectOwner :: Name
  , repoUrl :: URL
  , projectRevision :: Revision
  , archiveUrl :: Maybe URL
  , dependencies :: [String]
  , stars :: Integer
  , contributors :: Maybe Integer
  , commits :: Maybe Integer
  , forks :: Maybe Integer
  , createdAt :: Maybe UTCTime
  } deriving (Show, Generic)


projectId :: ProjectInfo -> Text
projectId p = intercalate "/" [projectOwner p, projectName p]


instance ToJSON ProjectInfo where
  toJSON p =
    object ["id" .= projectId p
           , "url" .= repoUrl p
           , "revision" .= projectRevision p
           , "dependencies" .= dependencies p
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
    } deriving Show


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

statsForPath :: FilePath -> FileStats
statsForPath path =
  emptyStats { fileName = path }

type AnalyzeResult error = Either error FileStats

type ParseError = Text
