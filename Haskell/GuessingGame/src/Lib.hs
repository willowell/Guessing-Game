module Lib where

import Data.Char
import System.IO
import System.Random
import Text.Read


promptLine :: String -> IO String
promptLine msg = do
    putStr msg
    hFlush stdout
    getLine

input :: Read a => String -> IO a
input msg = do
    str <- promptLine msg
    case (readMaybe :: Read a => String -> Maybe a) str of
        Just x -> return x
        Nothing -> do
            putStrLn "Invalid input."
            input msg

inputInt :: String -> IO Int
inputInt msg = (input :: String -> IO Int) msg

inputDouble :: String -> IO Double
inputDouble msg = (input :: String -> IO Double) msg

yesno :: String -> IO Bool
yesno prompt = do
    str <- promptLine $ prompt ++ " (y/n): "
    case map toLower str of
        "y" -> return True
        "n" -> return False
        _   -> do
            putStrLn "Invalid input."
            yesno prompt

doYouLikeCats :: IO ()
doYouLikeCats = do
    res <- yesno "Do you like cats?"
    if res
        then putStrLn "Yay!!"
        else putStrLn "Awww :-("
    continue <- yesno "Continue?"
    if continue then doYouLikeCats else putStrLn "Okay, goodbye!"

askForNumber :: Int -> IO ()  
askForNumber answer = do  

    number <- inputInt "Please enter your guess: "
    
    putStrLn $ "You guessed: " ++ show number
    
    case compare number answer of  
        LT -> do
            putStrLn "Too low!"
            askForNumber answer
        
        GT -> do
            putStrLn "Too high!"
            askForNumber answer

        EQ -> putStrLn "You are correct!"


run :: StdGen -> IO ()
run gen = do
    putStrLn "I'm thinking of a number between 1 and 100! Can you guess which one?"    

    let (randomNumber, newGen) = randomR (1, 100) gen :: (Int, StdGen)
  
    askForNumber randomNumber
    
    continue <- yesno "Do you want to play again?"
    if continue 
        then do
            putStrLn "Okay, give me a moment to think of a new number!"
            run newGen 
        else putStrLn "Okay, thank you for playing!"
    