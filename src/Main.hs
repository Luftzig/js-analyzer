{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified GitHub.Endpoints.Search as Github
import qualified GitHub.Auth as Auth
import qualified GitHub.Data as Github
import qualified GitHub.Data.Name as Github
import qualified GitHub.Request as Github
import GitHub.Data.Request (query)

import Control.Monad (forM_)
import Data.List (intercalate)
import Data.ByteString.Char8 (ByteString, pack)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.String (fromString)
import Data.Text (Text, unpack)
import Data.Semigroup ((<>))
import qualified Data.Text.Encoding as TE
import System.Environment (lookupEnv, getArgs)
import Options.Applicative

import Analyze (analyze)
import Data


data Options = Options
    { searchQuery :: String
    , perPage :: Int
    , page :: Int
    , sort :: String
    , order :: String
    }


main :: IO ()
main = do
    token <- lookupEnv "GITHUB_TOKEN"
    args <- parseArgs
    let auth = fmap (Auth.OAuth . pack) token
    let search = fromString $ searchQuery args
    putStrLn $ "Searching " ++ unpack search
    result <- searchRepos auth search
      [ ("sort", fromString $ sort args)
      , ("order", fromString $ order args)
      , ("per_page", fromString . show $ perPage args)
      , ("page", fromString . show $ page args)
      ]
    case result of
      Left e -> putStrLn $ "Error: " ++ show e
      Right r -> analyzeRepos auth r


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



searchRepos :: Maybe Github.Auth -> Text -> [(ByteString, Text)] -> IO (Either Github.Error ProjectInfo)
searchRepos auth search queryParams =
    let
      params = map (\(a, b) -> (a, Just $ TE.encodeUtf8 b)) queryParams
    in
      Github.executeRequestMaybe auth $ Github.query ["search", "repositories"] (("q", Just $ TE.encodeUtf8 search):params)


analyzeRepos :: Maybe Auth.Auth -> Github.SearchResult Github.Repo -> IO ()
analyzeRepos auth result = do
    let repos = toProjectInfo auth <$> Github.searchResultResults result
    forM_ repos (analyze auth)


toProjectInfo :: Maybe Auth.Auth -> Github.Repo -> IO ProjectInfo
toProjectInfo auth repo = do
  revision <- getLatestRevision auth repo
  return ProjectInfo
    { projectName = (Github.untagName . Github.repoName) repo
    , projectOwner = (Github.untagName . Github.simpleOwnerLogin . Github.repoOwner) repo
    , repoUrl = (Github.getUrl . Github.repoUrl) repo
    , projectRevision = revision
    }


getLatestRevision :: Maybe Auth.Auth -> Github.Repo -> IO Revision
getLatestRevision auth repo =
    return


-- formatRepo :: Github.Repo -> String
-- formatRepo r =
--     let fields =
--             [ ("Name", show . Github.repoName)
--             , ("URL", show . Github.repoHtmlUrl)
--             , ("Created-At", show . Github.repoCreatedAt)
--             , ("Stars", show . Github.repoStargazersCount)
--             ]
--     in intercalate "\n" $ map fmt fields
--         where fmt (s, f) = s ++ ": " ++ f r
--

