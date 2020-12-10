module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import Data.Set
import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

--
-- Diagonal diference' function below.
--

diag :: [[Int]] -> Int
diag arr =
  let diagLeft [] p c = c
      diagLeft (x:xs) p c = diagLeft xs (p + 1) (c + x !! p)
      diagRight [] p c = c
      diagRight (x:xs) p c = diagRight xs (p - 1) (c + x !! p)
  in abs((diagLeft arr 0 0) - (diagRight arr (Data.List.length arr - 1) 0))

main :: IO ()
main = do
  n <- readLn :: IO Int
  str <- replicateM n getLine
  let ans = Data.List.map (Data.List.map (read :: String -> Int) . Data.List.words) str
  print (diag ans)
