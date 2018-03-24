{-# LANGUAGE OverloadedStrings #-}
module Data where

import Data.Text (Text)
import Data.Traversable (Traversable)

type Name = Text

type URL = Text

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
    , filter :: Integer
    , reduce :: Integer
    }


type AnalyzeResult error = Either error FileStats
