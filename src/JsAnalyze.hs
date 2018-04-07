module JsAnalyze (analyze) where

import Data.ByteString (ByteString, unpack)

import qualified Language.ECMAScript3.Parser as JS
import qualified Language.ECMAScript3.Syntax as JS
import qualified Text.Parsec as P
import qualified Text.Parsec.Error as P

import Data

analyze :: FilePath -> ByteString -> Either P.ParseError FileStats
analyze path content =
  extractStats $ JS.parse JS.program path content


extractStats :: Either P.ParseError (JS.JavaScript JS.SourcePos) -> Either P.ParseError FileStats
extractStats (Left error) = Left error
extractStats (Right code) = Right emptyStats

