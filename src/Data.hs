{-# LANGUAGE OverloadedStrings #-}
module Data where

import Data.Text (Text)
import Data.Traversable (Traversable)

import qualified Text.Parsec.Error as Parsec

type Name = Text

type URL = String

data Revision = Commit Text
              | Tag Name
              | DefaultBranchHead

data ProjectInfo = ProjectInfo
  { projectName :: Name
  , projectOwner :: Name
  , repoUrl :: URL
  , projectRevision :: Revision
  , archiveUrl :: Maybe URL
  }


data ProjectStats = ProjectStats
  { projectInfo :: ProjectInfo
  , filesStats :: [FileStats]
  }


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


type AnalyzeResult error = Either error FileStats

type ParseError = Parsec.ParseError
