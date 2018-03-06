{-# LANGUAGE OverloadedStrings #-}
module Data where

import Data.Text (Text)

type Name = Text

type URL = Text

data Revision = Commit Text
              | Tag Name

data ProjectInfo = ProjectInfo
  { projectName :: Name
  , projectOwner :: Name
  , repoUrl :: URL
  , projectRevision :: Revision
  }


data FileStatistics = FileStatistics
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


type AnalyzeResult error = Either error FileStatistics
