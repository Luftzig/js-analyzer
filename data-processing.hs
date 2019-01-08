#!/usr/bin/env stack
-- stack --resolver lts-12.25
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Conduit (Conduit, (.|))
import qualified Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.List as CL
import Data.List (isInfixOf, or)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import System.FilePath.Posix (takeExtension, takeFileName)
import qualified Text.Printf as T

import Graphics.EasyPlot
import Graphics.Gnuplot.Simple

import Data.Aeson
import NeatInterpolation

import Data

showT :: Show a => a -> T.Text
showT = T.pack . show

countImperative (FileStats { whileCount = wc
                           , forCount = fc
                           , forInCount = fic
                           , forOfCount = foc
                           }) = wc + fc + fic + foc

countFunctional (FileStats { mapCount = mc
                           , filterCount = fc
                           , forEachCount = fec
                           , reduceCount = rc
                           }) = mc + fc + fec + rc

allFileStats files =
  C.runConduitRes $ files .| CL.mapMaybeM (C.liftIO . readStatsFile) .| C.fold

isLastJson p = takeExtension p == ".json" && takeFileName p /= "projects.json"

isInLastQuarter p =
  Data.List.or $
  ($ p) <$> [isInfixOf "2018-10-", isInfixOf "2018-11", isInfixOf "2018-12"]

lastFiles =
  C.sourceDirectoryDeep False "out" .| C.filter isLastJson .|
  C.filter (isInLastQuarter . takeFileName)

readStatsFile = decodeFileStrict :: FilePath -> IO (Maybe (V.Vector FileStats))

perProject =
  lastFiles .|
  CL.mapMaybeM
    (\p -> do
       stats <- C.liftIO $ readStatsFile p
       return $ fmap (\x -> (p, x)) stats)

isFailed s =
  case s of
    FailedStats {} -> True
    FileStats {} -> False

countFailed = V.length . (V.filter isFailed) :: V.Vector FileStats -> Int

percent a b = ((fromIntegral $ a) / (fromIntegral $ b)) * 100 :: Float

scatterAllFiles title source f =
  Data2D [Title title, Style Points] [] $
  V.toList $ V.map f $ V.filter (not . isFailed) source

main :: IO ()
main = do
  files <- C.runConduitRes $ lastFiles .| C.sinkList
  failureCount <-
    C.runConduitRes $
    perProject .|
    C.map
      (\(path, fs) ->
         let fileName = T.pack $ takeFileName path
             numFiles = showT $ V.length fs
             numFailures = showT $ countFailed fs
             percentText =
               T.pack $ T.printf "%.2f" (percent (countFailed fs) (V.length fs)) :: T.Text
         in [text|$fileName failures: $numFailures out of $numFiles (${percentText}%)|]) .|
    C.sinkList
  mapM T.putStr failureCount
  allV <- allFileStats lastFiles
  let totalFiles = V.length allV
  let totalFailures = countFailed allV
  let totalPercent = percent totalFailures totalFiles
  let totalFilesT = showT totalFiles
  let totalFailuresT = showT totalFailures
  let totalPercentT = showT $ (T.printf "%.2f" totalPercent :: String)
  T.putStr $
    [text|Total failed files: ${totalFailuresT} out of ${totalFilesT} (${totalPercentT}%)|]
  plot (PNG "total_scatter.png") $
    scatterAllFiles
      "Control Structures per File Size (log LoC)"
      allV
      (\fs ->
         ( log $ fromIntegral $ linesOfCode fs
         , log $ fromIntegral $ countImperative fs + countFunctional fs))
  plot
    (PNG "imp_vs_fun_scatter.png")
    [ scatterAllFiles
        "Imperative Structures per File Size (log LoC)"
        allV
        (\fs ->
           ( log $ fromIntegral $ linesOfCode fs
           , log $ fromIntegral $ countImperative fs))
    , scatterAllFiles
        "Functional Structures per File Size (log LoC)"
        allV
        (\fs ->
           ( log $ fromIntegral $ linesOfCode fs
           , log $ fromIntegral $ countFunctional fs))
    ]
  return ()
