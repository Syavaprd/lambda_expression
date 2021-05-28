module Main where

import Lib
import System.IO

run :: IO ()
run = do
  putStr "λ "
  str <- getLine
  putStrLn $ ">>> " ++ reduce str
  run

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  run