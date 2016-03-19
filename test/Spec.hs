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
execMain :: IO.FilePath -> IO.FilePath -> IO (String, String)
execMain inf exf = do
  input    <- IO.openFile inf IO.ReadMode
  expf     <- IO.openFile exf IO.ReadMode
  -- TODO: Fix for Mac and Windows
  (op, output)   <- Temp.mkstemps "/tmp/" "hs_"
  solve input output
  -- FIXME: this is strange, but Haskell require me to close file to write file contents
  -- IO.hFlush does not work.
  IO.hClose output
  -- FIXME: I don't know, but just !ret does not work.
  !output   <- IO.openFile op IO.ReadMode
  !expected <- IO.hGetContents expf
  !outs     <- IO.hGetContents output
  let !ret = ((rstrip expected), (rstrip outs))
  -- ignore newlines.
  let !_ = IO.hClose input
  let !_ = IO.hClose output
  let !_ = IO.hClose expf
  return ret

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
        (expected, output) <- (execMain input expected)
        output `shouldBe` expected

doMainSpec :: IO ()
doMainSpec = do
  testcases <- getTestCaseForMain "cases"
  hspec $ mainSpec testcases

main :: IO ()
main = doMainSpec
