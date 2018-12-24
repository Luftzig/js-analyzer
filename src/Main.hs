{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified GitHub.Auth as Auth
import qualified GitHub.Endpoints.Search as Github
import qualified GitHub.Request as Github

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson.Text as A
import Data.List (intercalate)
import Data.ByteString.Char8 (ByteString, pack)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.String (fromString)
import Data.Text (Text, unpack)
import Data.Vector (Vector)
import qualified Data.Text.Lazy.IO as TextIO
import Data.Semigroup ((<>))
import qualified Data.Text.Encoding as TE
import Network.URI (URI, uriToString)
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv, getArgs)
import System.FilePath.Posix ((</>))
import System.IO (FilePath)
import Options.Applicative

import AnalyzeContent (analyze)
import Projects (toProjectInfo)
import Data

import Debug.Trace


data Options = Options
    { searchQuery :: String
    , perPage :: Int
    , page :: Int
    , sort :: String
    , order :: String
    , outputDir :: String
    }


main :: IO ()
main = do
    token <- lookupEnv "GITHUB_TOKEN"
    args <- parseArgs
    let auth = fmap (Auth.OAuth . pack) token
    let search = fromString $ searchQuery args
    createDirectoryIfMissing True $ outputDir args
    putStrLn $ "Searching " ++ unpack search
    result <- searchRepos auth search
      [ ("sort", fromString $ sort args)
      , ("order", fromString $ order args)
      , ("per_page", fromString . show $ perPage args)
      , ("page", fromString . show $ page args)
      ]
    case result of
      Left e -> putStrLn $ "Error: " ++ show e
      Right r -> analyzeRepos auth (outputDir args) r


parseArgs :: IO Options
parseArgs =
    execParser args'
    where
      args' = info (args <**> helper)
                ( fullDesc
                <> progDesc "Query Github repositories and analyze their content"
                )


args :: Parser Options
args = Options
  <$> strOption
    ( long "query"
    <> short 'q'
    <> metavar "SEARCH"
    <> value "language:javascript"
    <> help "Github query")
  <*> option auto
    ( long "per-page"
    <> value 100
    )
  <*> option auto
    ( long "page"
    <> short 'p'
    <> value 1
    )
  <*> strOption
    ( long "sort-by"
    <> short 's'
    <> value "stars"
    )
  <*> strOption
    ( long "order"
    <> short 'o'
    <> value "desc"
    )
  <*> strOption
    ( long "outputDir"
    <> short 'd'
    <> value "out"
    )



searchRepos :: Maybe Github.Auth -> Text -> [(ByteString, Text)] -> IO (Either Github.Error (Github.SearchResult Github.Repo))
searchRepos auth search queryParams =
    let
      params = map (\(a, b) -> (a, Just $ TE.encodeUtf8 b)) queryParams
    in
      Github.executeRequestMaybe auth $ Github.query ["search", "repositories"] (("q", Just $ TE.encodeUtf8 search):params)


analyzeRepos :: Maybe Auth.Auth -> FilePath -> Github.SearchResult Github.Repo -> IO ()
analyzeRepos auth outputDir result = do
    repos <- sequence $ toProjectInfo auth <$> Github.searchResultResults result
    writeProjectsData outputDir repos
    forM_ repos analyze


writeProjectsData :: FilePath -> Vector ProjectInfo -> IO ()
writeProjectsData outputDir projects =
  TextIO.writeFile (outputDir </> "projects.json") (A.encodeToLazyText $ projects)

