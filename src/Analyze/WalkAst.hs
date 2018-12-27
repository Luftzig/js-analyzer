{-# LANGUAGE OverloadedStrings #-}
module Analyze.WalkAst (visitNode, countPatterns) where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.HashMap.Strict as M
import Data.Text (Text)
import qualified Data.Text as T

import Data
import Debug.Trace


visitNode :: (Maybe Text -> Object -> a) -> Value -> [a]
visitNode f n =
  case n of
    Object o ->
      let
        t = nodeType o
        res = f t o
      in
        res : concatMap (visitNode f) (M.elems o)
    Array a -> concatMap (visitNode f) a
    _ -> []

  where
    nodeType o =
      case M.lookup "type" o of
        Just (String s) -> Just s
        _ -> Nothing


countPatterns :: Maybe Text -> Object -> FileStats
countPatterns nodeType node =
  case nodeType of
    Just "ForStatement" -> emptyStats { forCount = 1 }
    Just "ForInStatement" -> emptyStats { forInCount = 1 }
    Just "ForOfStatement" -> emptyStats { forOfCount = 1 }
    Just "WhileStatement" -> emptyStats { whileCount = 1 }
    Just "DoWhileStatement" -> emptyStats { whileCount = 1 }
    Just "CallExpression" -> countCall node
    _ -> emptyStats


countCall :: Object -> FileStats
countCall node =
  case Object node ^? key "callee" . key "type" . _String of
    Just "MemberExpression" -> countByFnName (Object node ^? key "callee" . key "property" . key "name" . _String)
    Just "Identifier" -> countByFnName (Object node ^? key "callee" . key "name" . _String)
    _ -> emptyStats


countByFnName :: Maybe Text -> FileStats
countByFnName (Just name)
  | name `elem` ["map", "pluck", "flatMap", "collect"] = emptyStats { mapCount = 1 }
  | name `elem` ["reduce", "reduceRight", "foldl", "foldr"] = emptyStats { reduceCount = 1 }
  | name `elem` ["filter", "select", "reject", "where"] = emptyStats { filterCount = 1 }
  | name `elem` ["forEach", "each"] = emptyStats { forEachCount = 1 }
  | otherwise = emptyStats
countByFnName Nothing = emptyStats

