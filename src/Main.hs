{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified GitHub.Auth as Auth
import qualified GitHub.Data as Github
import qualified GitHub.Data.Name as Github
import qualified GitHub.Endpoints.Search as Github
import qualified GitHub.Endpoints.Repos as Github
import qualified GitHub.Request as Github

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import qualified Data.Aeson.Text as A
import qualified Data.Aeson.Encode.Pretty as A
import Data.List (intercalate)
import Data.ByteString.Char8 (ByteString, pack)
import qualified Data.ByteString.Lazy as LBS
import Data.Either (lefts, rights)
import Data.Either.Combinators (mapLeft)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.String (fromString)
import Data.Text (Text, breakOn, intercalate, unpack, lines, splitOn, strip, tail)
import qualified Data.Text.IO as T
import Data.Vector (Vector, fromList)
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


data Options = Options
    { searchQuery :: String
    , page :: Int
    , numResults :: Int
    , sort :: String
    , order :: String
    , outputDir :: String
    , reposFile :: Maybe String
    , process :: Bool
    }


data Error = GithubError Github.Error
           | ReadReposFileError
           deriving (Show)


main :: IO ()
main = do
    token <- lookupEnv "GITHUB_TOKEN"
    args <- parseArgs
    let auth = fmap (Auth.OAuth . pack) token
    createDirectoryIfMissing True $ outputDir args
    result <- getReposToProcess args auth
    case (process args, result) of
      (_, Left e) -> putStrLn $ "Error: " ++ show e
      (True, Right r) -> analyzeRepos auth (outputDir args) r
      (False, Right rs) -> putStrLn $ show $ fmap repoId rs
    where
      repoId r = Data.Text.intercalate "/" [Github.untagName $ Github.simpleOwnerLogin $ Github.repoOwner r, Github.untagName $ Github.repoName r]


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
    <> showDefault
    <> help "Github query")
  <*> option auto
    ( long "page"
    <> value 1
    )
  <*> option auto
    ( long "num-results"
    <> short 'n'
    <> value 100
    <> showDefault
    <> help "limit search to that number of results"
    )
  <*> strOption
    ( long "sort-by"
    <> short 's'
    <> value "stars"
    <> showDefault
    )
  <*> strOption
    ( long "order"
    <> short 'o'
    <> value "desc"
    <> showDefault
    )
  <*> strOption
    ( long "outputDir"
    <> short 'd'
    <> value "out"
    <> showDefault
    )
  <*> repos
  <*> switch
    ( long "process"
    <> short 'p'
    <> help "if set, will run the processing chain on the input (file or search) repositories. Otherwise will just print repositories metadata"
    <> showDefault
    )


reposFileOption :: Parser (Maybe FilePath)
reposFileOption = Just <$> strOption
    ( long "repos"
    <> short 'r'
    <> help "path to a comma or new-line delimited list of repositories to process. Implies `process`"
    )


searchReposOption :: Parser (Maybe FilePath)
searchReposOption= const Nothing <$> switch
  ( long "search"
  <> short 'S'
  <> help "Search github for repositories to process"
  )


repos :: Parser (Maybe FilePath)
repos = reposFileOption <|> searchReposOption


getReposToProcess :: Options -> Maybe Github.Auth -> IO (Either Error (Vector Github.Repo))
getReposToProcess args@(Options {reposFile=Nothing}) auth = do
    let search = fromString $ searchQuery args
    putStrLn $ "Searching " ++ unpack search
    result <- searchRepos auth search
      [ ("sort", fromString $ sort args)
      , ("order", fromString $ order args)
      , ("per_page", fromString . show $ numResults args)
      , ("page", fromString . show $ page args)
      ]
    return $ mapLeft GithubError result


getReposToProcess args@(Options {reposFile=(Just file)}) auth = do
    resolveRepositoriesFromList auth file


resolveRepositoriesFromList :: Maybe Github.Auth -> FilePath -> IO (Either Error (Vector Github.Repo))
resolveRepositoriesFromList auth file = do
  fileContent <- T.readFile file
  names <- return $ concatMap (splitOn ",") $ Data.Text.lines fileContent
  repos <- mapM (getRepoInfo auth) names
  failures <- return $ lefts repos
  successes <- return $ rights repos
  case failures of
    []   -> return $ Right $ fromList successes
    f:fs -> return $ Left $ GithubError f


getRepoInfo :: Maybe Github.Auth -> Text -> IO (Either Github.Error Github.Repo)
getRepoInfo auth fullName = do
  let (ownerT, repoT) = Data.Text.breakOn "/" fullName
  let (owner, repo) = (Github.N $ Data.Text.strip $ ownerT, Github.N $ Data.Text.strip $ Data.Text.tail repoT)
  Github.repository' auth owner repo


searchRepos :: Maybe Github.Auth -> Text -> [(ByteString, Text)] -> IO (Either Github.Error (Vector Github.Repo))
searchRepos auth search queryParams = do
    let params = map (\(a, b) -> (a, Just $ TE.encodeUtf8 b)) queryParams
    response <- Github.executeRequestMaybe auth $ Github.query ["search", "repositories"] (("q", Just $ TE.encodeUtf8 search):params)
    return $ Github.searchResultResults <$> response


analyzeRepos :: Maybe Auth.Auth -> FilePath -> Vector Github.Repo -> IO ()
analyzeRepos auth outputDir result = do
    repos <- sequence $ toProjectInfo auth <$> result
    writeProjectsData outputDir repos
    forM_ repos (analyze outputDir)


writeProjectsData :: FilePath -> Vector ProjectInfo -> IO ()
writeProjectsData outputDir projects = do
  let projectsFile = outputDir </> "projects.json"
  oldProjects <- A.decodeFileStrict projectsFile :: IO (Maybe (Vector ProjectInfo))
  LBS.writeFile projectsFile (A.encodePretty $ (projects <> fromMaybe mempty oldProjects))
