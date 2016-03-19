{-# LANGUAGE BangPatterns #-}
module Main where

import Solver

import Test.Hspec (Spec, hspec, describe, it, shouldBe, shouldReturn)
import Test.HUnit (assertBool, assertFailure, Assertion)
import Control.Applicative
import Control.Monad
import qualified Data.Char         as Char
import qualified System.IO         as IO
import qualified System.Posix.Temp as Temp
import qualified System.Directory  as Directory
import System.FilePath.Posix (takeExtension, replaceExtension, joinPath)

rstrip :: String -> String
rstrip = reverse . dropWhile Char.isSpace . reverse
execMain :: IO.FilePath -> IO.FilePath -> IO Bool
execMain inf exf = do
  input    <- IO.openFile inf IO.ReadMode
  expf     <- IO.openFile exf IO.ReadMode
  -- TODO: Fix for Mac and Windows
  (op, output)   <- Temp.mkstemps "/tmp/" "hs_"
  solve input output
  -- FIXME: this is strange, but Haskell require me to close file to write file contents
  -- IO.hFlush does not work.
  IO.hClose output
  output   <- IO.openFile op IO.ReadMode
  expected <- IO.hGetContents expf
  outs     <- IO.hGetContents output
  let !success = (rstrip expected) == (rstrip outs)
  IO.hClose input
  IO.hClose output
  IO.hClose expf
  return success

-- IO [FilePath]
getTestCaseForMain :: IO.FilePath -> IO [(IO.FilePath,IO.FilePath)]
getTestCaseForMain path = do
  rfiles <- map (\i -> joinPath ["cases", i]) <$> Directory.getDirectoryContents path
  vfiles <- filterM Directory.doesFileExist rfiles -- filter out directory.
  let inputs  = filter (\i -> (takeExtension i) == ".in") vfiles
  let outputs = [replaceExtension i ".out" | i <- inputs]
  return $ zip inputs outputs

-- Main.~~~
mainSpec :: [(IO.FilePath, IO.FilePath)] -> Spec
mainSpec testcases =
  describe "main" $ do
    forM_ testcases $ \(input, expected) ->
      it (input ++ " => " ++ expected) $ do
        (execMain input expected) `shouldReturn` True

doMainSpec :: IO ()
doMainSpec = do
  testcases <- getTestCaseForMain "cases"
  putStrLn $ show $ length testcases
  hspec $ mainSpec testcases

main :: IO ()
main = doMainSpec
