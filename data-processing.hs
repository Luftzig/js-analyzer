#!/usr/bin/env stack
-- stack --resolver lts-12.25
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import           Conduit                  (Conduit, (.|))
import qualified Conduit                  as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.List        as CL
import           Data.List                (isInfixOf, or)
import qualified Data.Text                as T
import qualified Data.Vector              as V
import           System.FilePath.Posix    (takeExtension, takeFileName)
import qualified Text.Printf              as T

import           Data.Aeson
import           NeatInterpolation

import           Data


showT :: Show a => a -> T.Text
showT = T.pack . show


main :: IO ()
main = do
  let isLastJson p = takeExtension p == ".json" && takeFileName p /= "projects.json"
  let isInLastQuarter p = Data.List.or $ ($ p) <$>
                          [isInfixOf "2018-10-"
                          , isInfixOf "2018-11"
                          , isInfixOf "2018-12"]
  let lastFiles = C.sourceDirectoryDeep False "out"
                  .| C.filter isLastJson
                  .| C.filter (isInLastQuarter . takeFileName)
  files <- C.runConduitRes $ lastFiles .| C.sinkList
  let readStatsFile = decodeFileStrict :: FilePath -> IO (Maybe (V.Vector FileStats))
  let perProject = lastFiles
                   .| CL.mapMaybeM (\p -> do
                                       stats <- C.liftIO $ readStatsFile p
                                       return $ fmap (\x -> (p, x)) stats
                                   )
  let isFailed s= case s of { FailedStats{} -> True ; FileStats{} -> False}
  let countFailed = V.length . (V.filter isFailed) :: V.Vector FileStats -> Int
  let percent a b = ((fromIntegral $ a ) / (fromIntegral $ b)) * 100 :: Float
  failureCount <- C.runConduitRes $ perProject
                  .| C.map (\(path, fs) ->
                              let
                                fileName = T.pack $ takeFileName path
                                numFiles = showT $ V.length fs
                                numFailures = showT $ countFailed fs
                                percentText = T.pack $ T.printf "%.2f" (percent (countFailed fs) (V.length fs)) :: T.Text
                              in
                                [text|$fileName failures: $numFailures out of $numFiles (${percentText}%)|]
                            )
                  .| C.sinkList
  mapM print failureCount
  allV <- C.runConduitRes $ lastFiles .| CL.mapMaybeM (C.liftIO . readStatsFile) .| C.fold
  let totalFiles = V.length allV
  let totalFailures = countFailed allV
  let totalPercent = percent totalFailures totalFiles
  let totalFilesT = showT totalFiles
  let totalFailuresT = showT totalFailures
  let totalPercentT = showT$ (T.printf "%.2f" totalPercent :: String)
  print $ [text|Total failed files: ${totalFailuresT} out of ${totalFilesT} (${totalPercentT}%)|]
  return ()
