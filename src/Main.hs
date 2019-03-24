{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified GitHub.Auth              as Auth
import qualified GitHub.Data              as Github
import qualified GitHub.Data.Name         as Github
import qualified GitHub.Endpoints.Repos   as Github
import qualified GitHub.Endpoints.Search  as Github
import qualified GitHub.Request           as Github

import           Control.Monad            (when, unless, forM_)
import           Control.Monad.IO.Class   (liftIO)
import qualified Data.Aeson               as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.Aeson.Text          as A
import           Data.ByteString.Char8    (ByteString, pack)
import qualified Data.ByteString.Lazy     as LBS
import           Data.Either              (either, fromRight, isLeft, lefts,
                                           rights)
import           Data.Either.Combinators  (mapLeft)
import qualified Data.HashMap.Strict      as HM
import           Data.List                (intercalate)
import           Data.Maybe               (fromMaybe, isNothing, listToMaybe)
import           Data.Semigroup           ((<>))
import           Data.String              (fromString)
import           Data.Text                (Text, breakOn, intercalate, lines,
                                           splitOn, strip, tail, unpack)
import qualified Data.Text.Encoding       as TE
import qualified Data.Text.IO             as T
import qualified Data.Text.Lazy.IO        as TextIO
import           Data.Vector              (Vector, fromList)
import           Network.URI              (URI, uriToString)
import           Options.Applicative
import           System.Directory         (doesFileExist, createDirectoryIfMissing)
import           System.Environment       (getArgs, lookupEnv)
import           System.FilePath.Posix    ((</>))
import           System.IO                (FilePath)

import           AnalyzeContent           (analyze)
import           Data
import           Projects                 (toProjectInfo)


data Options = Options
    { searchQuery   :: String
    , page          :: Int
    , numResults    :: Int
    , sort          :: String
    , order         :: String
    , outputDir     :: String
    , reposFile     :: Maybe String
    , process       :: Bool
    , metadataOnly  :: Bool
    , noUpdateRepos :: Bool
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
      (_, Left e)       -> putStrLn $ "Error: " ++ show e
      (True, Right r)   -> processRepos auth args r
      (False, Right rs) -> print $ fmap repoId rs
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
    <> help "if set, will run the processing chain on the input (file or search) repositories. Otherwise will just print repositories IDs"
    <> showDefault
    )
  <*> switch
    ( long "metadata-only"
    <> short 'm'
    <> help "If used will only fetch metadata on the projects, but not process project files"
    <> showDefault
    )
  <*> switch
    ( long "no-update-projects"
    <> short 'U'
    <> help "If `OUTPUT_DIR/projects.json` exists, do not update or fetch projects that are already listed"
    )


reposFileOption :: Parser (Maybe FilePath)
reposFileOption = Just <$> strOption
    ( long "repos"
    <> short 'r'
    <> help "path to a comma or new-line delimited list of repositories to process. Implies `process`"
    )


searchReposOption :: Parser (Maybe FilePath)
searchReposOption= Nothing <$ switch
  ( long "search"
  <> short 'S'
  <> help "Search github for repositories to process"
  )


repos :: Parser (Maybe FilePath)
repos = reposFileOption <|> searchReposOption


getReposToProcess :: Options -> Maybe Github.Auth -> IO (Either Error (Vector Github.Repo))
getReposToProcess args@Options {reposFile=Nothing} auth = do
    let search = fromString $ searchQuery args
    putStrLn $ "Searching " ++ unpack search
    result <- searchRepos auth search
      [ ("sort", fromString $ sort args)
      , ("order", fromString $ order args)
      , ("per_page", fromString . show $ numResults args)
      , ("page", fromString . show $ page args)
      ]
    return $ mapLeft GithubError result


getReposToProcess args@Options {reposFile=(Just file), noUpdateRepos=noUpdate} auth =
    resolveRepositoriesFromList auth args


resolveRepositoriesFromList :: Maybe Github.Auth -> Options -> IO (Either Error (Vector Github.Repo))
resolveRepositoriesFromList auth Options {reposFile=(Just file), noUpdateRepos=noUpdate, outputDir=outDir} = do
  fileContent <- T.readFile file
  let names = concatMap (splitOn ",") $ Data.Text.lines fileContent
  repos <- mapM (getRepoInfo auth) names
  let failures = lefts repos
  let successes = rights repos
  case failures of
    []   -> return $ Right $ fromList successes
    f:fs -> return $ Left $ GithubError f


getRepoInfo :: Maybe Github.Auth -> Text -> IO (Either Github.Error Github.Repo)
getRepoInfo auth fullName = do
  let (ownerT, repoT) = Data.Text.breakOn "/" fullName
  let (owner, repo) = (Github.N $ Data.Text.strip ownerT, Github.N $ Data.Text.strip $ Data.Text.tail repoT)
  Github.repository' auth owner repo


searchRepos :: Maybe Github.Auth -> Text -> [(ByteString, Text)] -> IO (Either Github.Error (Vector Github.Repo))
searchRepos auth search queryParams = do
    let params = map (\(a, b) -> (a, Just $ TE.encodeUtf8 b)) queryParams
    response <- Github.executeRequestMaybe auth $ Github.query ["search", "repositories"] (("q", Just $ TE.encodeUtf8 search):params)
    return $ Github.searchResultResults <$> response


processRepos :: Maybe Auth.Auth -> Options -> Vector Github.Repo -> IO ()
processRepos auth options repoNames = do
  repos <- getProjectsMetadata auth (outputDir options) repoNames
  unless (metadataOnly options) $ forM_ repos (analyze $ outputDir options)


getProjectsMetadata :: Maybe Auth.Auth -> FilePath -> Vector Github.Repo -> IO (Vector ProjectInfo)
getProjectsMetadata auth outputDir result = do
    repos <- sequence $ toProjectInfo auth <$> result
    writeProjectsData outputDir repos
    return repos


writeProjectsData :: FilePath -> Vector ProjectInfo -> IO ()
writeProjectsData outputDir projects = do
    let projectsFile = outputDir </> "projects.json"
    oldProjects <- getExistingProjectsList outputDir
    let projectsMap = foldl (\m p -> HM.insert (projectId p) p m) HM.empty projects
    let allProjects = HM.union projectsMap oldProjects
    LBS.writeFile projectsFile (A.encodePretty (HM.elems allProjects))


getExistingProjectsList :: FilePath -> IO (HM.HashMap Text ProjectInfo)
getExistingProjectsList outDir = do
  let path = outDir </> "projects.json"
  fileExists <- doesFileExist path
  if fileExists then do
    oldProjects <- A.decodeFileStrict path :: IO (Maybe (Vector ProjectInfo))
    return $ case oldProjects of
              Just ps -> foldl (\m p -> HM.insert (projectId p) p m) HM.empty ps
              Nothing -> HM.empty
    else return HM.empty

