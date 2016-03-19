{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}

module Solver where

import Control.Applicative
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Maybe as Maybe
import qualified Data.List  as List
import qualified Data.Set   as Set
import qualified Data.Char  as Char
import qualified System.IO  as IO

strip :: String -> String
strip = lstrip . rstrip
   where
     lstrip = dropWhile Char.isSpace
     rstrip = reverse . lstrip . reverse

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p l = splitWhenAux l []
  where
    splitWhenAux []    []            = []
    splitWhenAux []    b@(_:_)       = [b]
    splitWhenAux (h:t) b | (p h)     = (reverse b) : (splitWhenAux t [])
                         | otherwise = splitWhenAux t (h:b)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn n l = splitWhen (n ==) l

readInt :: ByteString.ByteString -> Int
readInt = fromIntegral . Maybe.fromJust . fmap fst . ByteString.readInt

readInts :: IO.Handle -> IO [Int]
readInts x = map readInt . ByteString.words <$> ByteString.hGetLine x
--------------------------------------------------------------------------------

solve :: IO.Handle -> IO.Handle -> IO ()
solve cin cout =
  do
    [n] <- getInts
    putStrLn $ show $ (n * n)
  where
    -- shadowing stdio.
    putChar     = IO.hPutChar     cout
    putStr      = IO.hPutStr      cout
    putStrLn    = IO.hPutStrLn    cout
    print :: Show a => a -> IO ()
    print       = IO.hPrint       cout
    getChar     = IO.hGetChar     cin
    getLine     = IO.hGetLine     cin
    getContents = IO.hGetContents cin
    getInts     = readInts        cin

--------------------------------------------------------------------------------

-- When you submit file to online judge, uncomment below.
-- main :: IO ()
-- main = solve IO.stdin IO.stdout
