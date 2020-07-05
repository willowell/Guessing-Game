module Main where

import Lib

import System.Random

main :: IO ()
main = do
    let turnLimit = 10
    gen <- getStdGen    
    runGame gen turnLimit
