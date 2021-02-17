module Main where

import World
import Actions
import WorldParser
import DataDecl

import Control.Monad
import System.IO
import System.Console.Haskeline
import System.Exit
import Data.Char
import Data.List



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
putStrFuzzy :: GameData -> String -> InputT IO ()
putStrFuzzy state string | blind state = outputStr (map (fuzz) string)
                         | otherwise   = outputStr string

{- | Calls putStrFuzzy after appending a line break to the string. -}
putStrLnFuzzy :: GameData -> String -> InputT IO ()
putStrLnFuzzy state string = putStrFuzzy state (string ++ "\n")

-- Completion code based on example by C. A. McCann (https://stackoverflow.com/a/6165913/10664143)
commandList = ["go", "get", "drop", "pour", "examine", "drink", "open", "unlock", "wear", "remove", "apply", "brush", "?", "inventory", "quit","save","load"]

commandSearch :: String -> [Completion]
commandSearch str = map simpleCompletion $ filter (str `isPrefixOf`) commandList

settings :: Settings IO
settings  = Settings { historyFile = Just "", -- don't save history
                       complete = completeWord Nothing " \t" $ return . commandSearch,
                       autoAddHistory = True
                      }
-- End completion code

{-| Starts the main game loop. -}
main :: IO ()
main = runInputT settings (loop initState)

loop :: GameData -> InputT IO ()
loop state  = do
                         putStrLnFuzzy state (show state)
                         input <- getInputLine "What now? "
                         case input of
                              Nothing  -> loop state
                              Just cmd -> do let (state', msg) = process state cmd
                                             putStrLnFuzzy state ("\n\n" ++ msg ++ "\n")
                                             if (won state') then do outputStrLn winmessage
                                                                     return ()
                                             else if (finished state') then return ()
                                             else loop state'           
           


--https://www.reddit.com/r/haskell/comments/3wjddo/can_someone_help_me_with_aeson/
{-
load::Maybe String-> GameData-> IO()
load path state =case path of
                     Nothing -> do
                                      outputStrLn "Please enter valid path"
                                      loop state
                     Just path -> do 
                                lState <- decode <$> B.readFile path
                                case lState :: Maybe GameData of
                                  Nothing -> do
                                              outputStrLn "Load failed"
                                              loop state
                                  Just lState -> loop lState
-}

{-
save::String -> GameData ->IO()
save path state = case path of
                      Nothing -> do
                                      print("Please enter valid path")
                      Just path  -> do 
                                      json <- B.writeFile path (encode state) 
                                      print("ok")

-}

{-
  ___       _    _    ___ _           _     _____       _      
 / _ \ _  _(_)__| |__/ __| |_  ___ __| |__ |_   _|__ __| |_ ___
| (_) | || | / _| / / (__| ' \/ -_) _| / /   | |/ -_|_-<  _(_-<
 \__\_\\_,_|_\__|_\_\\___|_||_\___\__|_\_\   |_|\___/__/\__/__/
-}
prop_fuzz  :: Char -> Bool
prop_fuzz x = (x == x) || (x == '▒')
              where fuzzed = fuzz x

prop_objHere1 :: Object ->Room-> Bool
prop_objHere1 item room = (objectHere item room) == True || (objectHere item room) == False

prop_objHere2 :: Object ->Room-> Bool
prop_objHere2 item room | (objectHere item room) && (elem item (objects room))            = True
                        | (not (objectHere item room)) && (not (elem item (objects room))) = True -- does not contain
                        | otherwise                                                       = False

prop_RemoveObj ::Object -> Room-> Bool
prop_RemoveObj item room = (not (elem item (objects room))) == elem item (objects (removeObject item room)) 

prop_RemoveObjDesc ::Object -> Room-> Bool
prop_RemoveObjDesc item room = room_desc room == room_desc (removeObject item room)

prop_RemoveObjExit ::Object -> Room-> Bool
prop_RemoveObjExit item room = length (exits room) == length (exits (removeObject item room)) 