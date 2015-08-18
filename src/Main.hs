{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}

import Control.Applicative
import qualified Data.List as List
import qualified Data.Set  as Set
import qualified Data.Char as Char

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

--------------------------------------------------------------------------------


main :: IO ()
main = return ()
