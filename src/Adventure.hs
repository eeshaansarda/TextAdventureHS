module Main where

import World
import Actions
import WorldParser
import DataDecl

import Control.Monad
import System.IO
import System.Exit
import Data.Char

winmessage = "Congratulations, you have made it outside.\n"

{- Given a game state, and user input (as a list of words) return a 
   new game state and a message for the user. -}

{-
process :: GameData -> [String] -> (GameData, String)
process state [cmd,arg] = case actions cmd of
                            Just fn -> fn arg state
                            Nothing -> (state, "I don't understand")
process state [cmd]     = case commands cmd of
                            Just fn -> fn state
                            Nothing -> (state, "I don't understand")
process state _ = (state, "I don't understand")
-}

fuzz :: Char -> Char
fuzz x | not (isAlphaNum x) = x
       | even (ord x)       = '▒'
       | otherwise          = x

putStrFuzzy :: GameData -> String -> IO ()
putStrFuzzy state string | blind state = putStr (map (fuzz) string)
                         | otherwise   = putStr string
                         
putStrLnFuzzy :: GameData -> String -> IO ()
putStrLnFuzzy state string = putStrFuzzy state (string ++ "\n")


repl :: GameData -> IO GameData
repl state | finished state = return state
repl state = do putStrFuzzy state (show state)
                putStr "\nWhat now? "
                hFlush stdout
                cmd <- getLine
                let (state', msg) = process state cmd
                putStrLnFuzzy state ("\n\n" ++ msg ++ "\n")
                if (won state') then do putStrLn winmessage
                                        return state'
                               else repl state'

main :: IO ()
main = do repl initState
          return ()
