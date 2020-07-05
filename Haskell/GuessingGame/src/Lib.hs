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

input :: Read a => String -> (a -> Bool) -> IO a
input msg validator = do
    str <- promptLine msg
    case (readMaybe :: Read a => String -> Maybe a) str of
        Just x -> case validator x of
            True -> return x
            False -> do
                putStrLn "Invalid input."
                input msg validator
        Nothing -> do
            putStrLn "Invalid input."
            input msg validator


inputInt :: String -> (Int -> Bool) -> IO Int
inputInt msg validator = (input :: String -> (Int -> Bool) -> IO Int) msg validator

inputDouble :: String -> (Double -> Bool) -> IO Double
inputDouble msg validator = (input :: String -> (Double -> Bool) -> IO Double) msg validator

yesOrNo :: String -> IO Bool
yesOrNo prompt = do
    str <- promptLine $ prompt ++ " (y/n): "
    case map toLower str of
        "y" -> return True
        "n" -> return False
        _   -> do
            putStrLn "Invalid input."
            yesOrNo prompt

askForNumber :: Int -> Int -> IO ()  
askForNumber answer totalTurns = go totalTurns  
    where 
        go turnsLeft = 
            if turnsLeft == 0
                then putStrLn "You lose!"
                else do
                    putStrLn $ "You have " ++ show turnsLeft ++ " turn(s) left.\n" 
                                ++ (if turnsLeft == 1 then "Uh oh! Make it count!!" else [])

                    number <- inputInt "Please enter your guess: " (\x -> 1 <= x && x <= 100)
                    
                    putStrLn $ "You guessed: " ++ show number
                    
                    case compare number answer of  
                        LT -> do
                            putStrLn "Too low!"
                            go (turnsLeft - 1)
                        
                        GT -> do
                            putStrLn "Too high!"
                            go (turnsLeft - 1)

                        EQ -> putStrLn "You win!"


runGame :: StdGen -> Int -> IO ()
runGame gen totalTurns = do
        putStrLn "I'm thinking of a number between 1 and 100! Can you guess which one?"    

        let (randomNumber, newGen) = randomR (1, 100) gen :: (Int, StdGen)
    
        askForNumber randomNumber totalTurns
        
        continue <- yesOrNo "Do you want to play again?"
        if continue 
            then do
                putStrLn "Okay, give me a moment to think of a new number!"
                runGame newGen totalTurns
            else putStrLn "Okay, thank you for playing!"
    