module Main where

import Lib

import System.Random

main :: IO ()
main = do
    gen <- getStdGen    
    run gen
