module Main where

import World
import Actions
import WorldParser
import DataDecl

import Control.Monad
import System.IO
import System.Exit
import Data.Char
import Test.QuickCheck

winmessage = "Congratulations, you have made it outside.\n"

{-| For a given character, return either the character or a fuzzy nonsense character.
    A given character will always return the same result. Punctuation will never be fuzzy. -}
fuzz :: Char -> Char
fuzz x | not (isAlphaNum x) = x
       | even (ord x)       = '▒'
       | otherwise          = x

{-| Calls putStr with the provided String. If the GameData shows that the player is
       currently blind, the characters are fuzzed using `fuzz` first. -}
putStrFuzzy :: GameData -> String -> IO ()
putStrFuzzy state string | blind state = putStr (map (fuzz) string)
                         | otherwise   = putStr string
                         
{- | Calls putStrFuzzy after appending a line break to the string. -}
putStrLnFuzzy :: GameData -> String -> IO ()
putStrLnFuzzy state string = putStrFuzzy state (string ++ "\n")

{- | Main game loop: returning a response to an action, prompting for a command, 
     parsing input, then executing the command. -}
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

{-| Starts the main game loop. -}
main :: IO ()
main = do repl initState
          return ()

{-
  ___       _    _    ___ _           _     _____       _      
 / _ \ _  _(_)__| |__/ __| |_  ___ __| |__ |_   _|__ __| |_ ___
| (_) | || | / _| / / (__| ' \/ -_) _| / /   | |/ -_|_-<  _(_-<
 \__\_\\_,_|_\__|_\_\\___|_||_\___\__|_\_\   |_|\___/__/\__/__/
-}
prop_fuzz  :: Char -> Bool
prop_fuzz x = (x == x) || (x == '▒')
              where fuzzed = fuzz x