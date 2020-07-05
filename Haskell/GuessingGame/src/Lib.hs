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

                    number <- inputInt "Please enter your guess: "
                    
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
    