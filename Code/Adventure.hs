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

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Control.Monad.IO.Class

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

{-| User input processing tool. Requests to load or save are handled directly, and other
    commands are interpreted by the parser and the game loop is returned to. -}
control :: GameData -> String -> InputT IO ()
control state cmd | isPrefixOf "load " cmd = load (drop 5 cmd) state
                  | isPrefixOf "save " cmd = save (drop 5 cmd) state
                  | otherwise              = do let (state', msg) = process state cmd
                                                putStrLnFuzzy state ("\n" ++ msg ++ "\n")
                                                if (won state') then do outputStrLn winmessage
                                                                        return ()
                                                else if (finished state') then return ()
                                                else loop state' 

{-| Main game loop, capturing user input and passing it off to be processed. -}
loop :: GameData -> InputT IO ()
loop state  = do putStrLnFuzzy state (show state)
                 input <- getInputLine "What now? "
                 case input of
                      Nothing  -> loop state
                      Just cmd -> control state cmd
                              
--https://www.reddit.com/r/haskell/comments/3wjddo/can_someone_help_me_with_aeson/
load :: String-> GameData-> InputT IO()
load path state = do lState <- liftIO (decode <$> B.readFile ("saved/" ++ path))
                     case lState :: Maybe GameData of
                          Nothing     -> do outputStrLn "Load failed"
                                            loop state
                          Just lState -> loop lState


save :: String -> GameData -> InputT IO()
save path state = do json <- liftIO (B.writeFile ("saved/" ++ path) (encode state))
                     outputStrLn "Saved!"
                     
{-
  ___       _    _    ___ _           _     _____       _      
 / _ \ _  _(_)__| |__/ __| |_  ___ __| |__ |_   _|__ __| |_ ___
| (_) | || | / _| / / (__| ' \/ -_) _| / /   | |/ -_|_-<  _(_-<
 \__\_\\_,_|_\__|_\_\\___|_||_\___\__|_\_\   |_|\___/__/\__/__/
-}


instance Arbitrary DataDecl.Object where
       arbitrary = oneof [return mug, return coffeepot,
                          return key, return mask,
                          return glasses, return toothbrush,
                          return toothpaste]

instance Arbitrary DataDecl.Room where
       arbitrary = oneof [return bedroom, return kitchen,
                          return hall, return lounge,
                          return porch, return street,
                          return bathroom]

instance Arbitrary DataDecl.Direction' where
       arbitrary = oneof[return North, return South,
                         return East, return West,
                         return Outside, return Inside]



-----------------------------------------------------------------------------
prop_fuzz  :: Char -> Bool
prop_fuzz x = (x == x) || (x == '▒')
              where fuzzed = fuzz x
-----------------------------------------------------------------------------
prop_objHere1 :: DataDecl.Object -> Room -> Bool
prop_objHere1 item room = (objectHere item room) == True || (objectHere item room) == False

prop_objHere2 :: DataDecl.Object -> Room -> Bool
prop_objHere2 item room | (objectHere item room) && (elem item (objects room))            = True
                        | (not (objectHere item room)) && (not (elem item (objects room))) = True -- does not contain
                        | otherwise                                                       = False
---------------------------------------------------------------------------------
prop_RemoveObj :: DataDecl.Object -> Room -> Bool
prop_RemoveObj item room | objectHere item room = not (elem item (objects (removeObject item room)))
                         | otherwise            = True

prop_RemoveObjExit :: DataDecl.Object -> Room -> Bool
prop_RemoveObjExit item room = length (exits room) == length (exits (removeObject item room))
-------------------------------------------------------------------------------------
prop_AddObj :: DataDecl.Object -> Room -> Bool
prop_AddObj item room | not (objectHere item room) = (not (elem item (objects room))) == elem item (objects (addObject item room))
                      | otherwise                  = True

prop_AddObjDesc :: DataDecl.Object -> Room -> Bool
prop_AddObjDesc item room = room_desc room == room_desc (addObject item room)

prop_AddObjExit :: DataDecl.Object -> Room -> Bool
prop_AddObjExit item room = length (exits room) == length (exits (addObject item room))
----------------------------------------------------------------------------------

-- prop_findObj         :: String -> [DataDecl.Object] -> Bool
-- prop_findObj obj objs | elem obj [x | y <- objs, x <- [obj_name y]] = obj_name (findObj obj objs) == obj
                      -- | otherwise                                   = True
                          
----------------------------------------------------------------------------
prop_move :: Direction' -> Room -> Bool
prop_move dir room | elem dir [x | y <- (exits room), x <- [exit_dir y]] = case move dir room of
                                                                                Just a -> True
                                                                                _      -> False
                   | otherwise                                           = move dir room == Nothing
----------------------------------------------------------------------------
{-
*************The following Tests have been commented out as QuickTests were not deemed apt for them
----------------------------------------------------------------------------------------
prop_AddInv :: GameData -> DataDecl.Object -> Bool
prop_AddInv gm obj = length (inventory gm) + 1 == length (inventory (addInv gm obj))

prop_AddInvUnchanged1 :: DataDecl.Object -> GameData -> Bool
prop_AddInvUnchanged1 obj gm = (checkUnlock gm newState') && (checkBrush gm newState') && (checkCaf gm newState') && (checkApplied gm newState')
                                    && (checkMask gm newState') && (checkPour gm newState') && (checkFinish gm newState') && (checkBlind gm newState')
                                          && (checkLoc gm newState') && (checkWorld gm newState')

                            where newState' = addInv gm obj

-----------------------------------------------------------------
prop_removeInv :: GameData -> DataDecl.Object -> Bool
prop_removeInv gm obj = length (inventory gm) - 1 == length (inventory (removeInv gm obj))

prop_RemoveInvUnchanged1 :: DataDecl.Object -> GameData -> Bool
prop_RemoveInvUnchanged1 obj gm = (checkUnlock gm newState') && (checkBrush gm newState') && (checkCaf gm newState') && (checkApplied gm newState')
                                    && (checkMask gm newState') && (checkPour gm newState') && (checkFinish gm newState') && (checkBlind gm newState')
                                          && (checkLoc gm newState') && (checkWorld gm newState')

                            where newState' = removeInv gm obj


------------------------------------------------------------------------
--not sure**********
prop_Get :: DataDecl.Object -> GameData -> Bool--object being added to inv
prop_Get obj gm| elem obj (objects (getRoomData gm))          =
                            length (inventory gm) + 1 == length (inventory (fst (get obj gm))) -- object has been added to inv
               | elem obj (objects (getRoomData gm)) == False =
                            length (inventory gm) == length (inventory (fst (get obj gm)))
               | otherwise                                    = False

prop_GetRmChange :: DataDecl.Object -> GameData -> Bool--object being removed from the room
prop_GetRmChange obj gm| elem obj (objects (getRoomData gm))          = elem obj (objects cRoom') == False
                       | elem obj (objects (getRoomData gm)) == False = elem obj (objects cRoom') == True
                       | otherwise                                    = False
                            where cRoom' = getRoomData (fst (get obj gm))

prop_GetStr :: DataDecl.Object -> GameData -> Bool
prop_GetStr obj gm| elem obj (objects (getRoomData gm))          = snd (put obj gm) == (obj_name obj) ++ " is put in inventory"
                  | elem obj (objects (getRoomData gm)) == False = snd (put obj gm) == "No such object here"
                  | otherwise                                    = False

prop_GetUnchanged1 :: DataDecl.Object -> GameData -> Bool
prop_GetUnchanged1 obj gm = (checkUnlock gm newState') && (checkBrush gm newState') && (checkCaf gm newState') && (checkApplied gm newState')
                                    && (checkMask gm newState') && (checkPour gm newState') && (checkFinish gm newState') && (checkBlind gm newState')
                                          && (checkLoc gm newState') && (checkWorld gm newState')

                            where newState' = (fst (get obj gm))

---------------------------------------------------------------------------
--not sure**********
prop_Put :: DataDecl.Object -> GameData -> Bool
prop_Put obj gm| elem obj (inventory gm)          =(length (inventory gm) - 1) == length (inventory (fst (put obj gm)))
               | elem obj (inventory gm) == False = length (inventory gm) == length (inventory (fst (put obj gm)))
               | otherwise                        = False

prop_PutRmChange :: DataDecl.Object -> GameData -> Bool
prop_PutRmChange obj gm| elem obj (inventory gm)          = elem obj (objects cRoom') == True
                       | elem obj (inventory gm) == False = elem obj (objects cRoom') == False
                       | otherwise                                    = False
                            where cRoom' = getRoomData (fst (put obj gm))

prop_PutStr :: DataDecl.Object -> GameData -> Bool
prop_PutStr obj gm| elem obj (inventory gm)          = snd (put obj gm) == (obj_name obj) ++ " has been dropped"
                  | elem obj (inventory gm) == False = snd (put obj gm) == "No such item"
                  | otherwise                        = False

prop_PutUnchanged1 :: DataDecl.Object -> GameData -> Bool
prop_PutUnchanged1 obj gm = (checkUnlock gm newState') && (checkBrush gm newState') && (checkCaf gm newState') && (checkApplied gm newState')
                                    && (checkMask gm newState') && (checkPour gm newState') && (checkFinish gm newState') && (checkBlind gm newState')
                                          && (checkLoc gm newState') && (checkWorld gm newState')
                            where newState' = (fst (put obj gm))
---------------------------------------------------------------------------
--not sure*******

prop_Go :: Direction' -> GameData -> Bool
prop_Go dir gm | validDir && (snd result == "Moved") && (location_id (fst result) /= location_id gm)                  = True
               | (not validDir) && (snd result == "Unknown location") && (location_id (fst result) == location_id gm) = True
               | otherwise                                                                                        = False
                     where result =  go dir gm
                           validDir = dir == North || dir == South || dir == East|| dir == West|| dir == Outside|| dir == Inside

prop_GoUnchanged1 :: Direction'-> GameData -> Bool
prop_GoUnchanged1 dir gm = (checkUnlock gm newState') && (checkBrush gm newState') && (checkCaf gm newState') && (checkApplied gm newState')
                                    && (checkMask gm newState') && (checkPour gm newState') && (checkFinish gm newState') && (checkBlind gm newState')
                                          && (checkInv gm newState') && (checkWorld gm newState')

                            where newState' = fst (go dir gm)
---------------------------------------------------------------------------
prop_remove :: DataDecl.Object -> GameData -> Bool
prop_remove obj gm |obj == mask    = not (checkMask gm newState')
                   |obj == glasses = not (checkBlind gm newState')
                   |otherwise      = checkBlind gm newState' && checkMask gm newState'
                                     
                            where newState' = (fst (remove obj gm))
prop_removetoInv :: DataDecl.Object -> GameData -> Bool
prop_removetoInv obj gm| elem obj (inventory gm)          =(length (inventory gm) - 1) == length (inventory (fst (remove obj gm))) 
                       | elem obj (inventory gm) == False = length (inventory gm) == length (inventory (fst (put obj gm)))
                       | otherwise                        = False

prop_removeStr :: DataDecl.Object -> GameData -> Bool
prop_removeStr obj gm| elem obj (inventory gm)          = snd (remove obj gm) == "You're not carrying that at the moment"
                     | elem obj (inventory gm) == False = snd (put obj gm) == "No such item"
                     | otherwise                        = False

prop_removeUnchanged1 :: DataDecl.Object -> GameData -> Bool
prop_removeUnchanged1 obj gm |obj == mask    = (checkBlind gm newState') && checks
                             |obj == glasses = (checkMask gm newState') && checks
                             |otherwise      = (checkMask gm newState') && (checkBlind gm newState') && checks
                                     
                            where  checks = (checkUnlock gm newState') && (checkBrush gm newState') && (checkCaf gm newState') && (checkApplied gm newState')
                                    && (checkMask gm newState') && (checkPour gm newState') && (checkFinish gm newState') && (checkInv gm newState')
                                          && (checkLoc gm newState') && (checkWorld gm newState')
                                   newState' = (fst (remove obj gm))

checkUnlock:: GameData-> GameData -> Bool
checkUnlock gm1 gm2 = unlocked gm1 == unlocked gm2

checkBrush:: GameData-> GameData -> Bool
checkBrush gm1 gm2 = brushed gm1 == brushed gm2

checkPour:: GameData->GameData -> Bool
checkPour gm1 gm2 = poured gm1 == poured gm2

checkApplied:: GameData->GameData -> Bool
checkApplied gm1 gm2 = pasteApplied gm1 == pasteApplied gm2

checkFinish:: GameData->GameData -> Bool
checkFinish gm1 gm2 = finished gm1 == finished gm2

checkBlind:: GameData->GameData -> Bool
checkBlind gm1 gm2 = blind gm1 == blind gm2

checkCaf:: GameData->GameData -> Bool
checkCaf gm1 gm2 = caffeinated gm1 == caffeinated gm2

checkLoc:: GameData->GameData -> Bool
checkLoc gm1 gm2 = location_id gm1 == location_id gm2

checkMask:: GameData->GameData -> Bool
checkMask gm1 gm2 = masked gm1 == masked gm2

checkWorld :: GameData->GameData -> Bool
checkWorld gm1 gm2 = length (world gm1) == length (world gm2) 

checkInv :: GameData->GameData -> Bool
checkInv gm1 gm2 = length (inventory gm1) == length (inventory gm2)
-}

