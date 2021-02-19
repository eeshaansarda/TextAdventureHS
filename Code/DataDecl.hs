{-# LANGUAGE DeriveGeneric #-}
module DataDecl where

import GHC.Generics

-- Data Declaration -------------------------------------------

data Command' = Quit | Inventory | Help
   deriving Show
-------------------------------------------
data Direction' = North | East | South | West | Inside | Outside
   deriving (Eq,Generic)
-------------------------------------------
data Action' = Go Direction' | Get DataDecl.Object | Put DataDecl.Object | Pour DataDecl.Object
             | Examine DataDecl.Object | Drink DataDecl.Object | Open String| Wear DataDecl.Object
             | Remove DataDecl.Object | Unlock String | Apply DataDecl.Object | Brush String
   deriving Show
-------------------------------------------
data Object = Obj { obj_name :: String,
                    obj_longname :: String,
                    obj_desc :: String }
   deriving (Eq,Generic)
-------------------------------------------
data Exit = Exit { exit_dir :: Direction',
                   exit_desc :: String,
                   room :: String } 
   deriving (Eq,Generic)
-------------------------------------------
data Room = Room { room_desc :: String,
                   exits :: [Exit],
                   objects :: [DataDecl.Object] }
   deriving (Eq,Generic)
-------------------------------------------
data GameData = GameData { location_id :: String, -- where player is
                           world :: [(String, Room)],
                           inventory :: [DataDecl.Object], -- objects player has
                           poured :: Bool, -- coffee is poured
                           caffeinated :: Bool, -- coffee is drunk
                           blind :: Bool, -- true while glasses are not on
                           finished :: Bool, -- is game finished
                           masked :: Bool, -- is the mask being put on
                           pasteApplied :: Bool,-- paste has been applied to the tooth-brush
                           brushed :: Bool, -- is teeth brushed
                           unlocked :: Bool -- is the front door locked
                         }  deriving Generic
------------------------------------------------------------------

-- Show ---------------------------------------------------------
-- for testing purposes
instance Show DataDecl.Object where
   show obj = obj_longname obj

instance Show Room where
    show (Room desc exits objs) = desc ++ "\n" ++ concatMap exit_desc exits ++
                                  showInv objs
       where showInv [] = ""
             showInv xs = "\n\nYou can see: " ++ showInv' xs
             showInv' [x] = show x
             showInv' (x:xs) = show x ++ ", " ++ showInv' xs

instance Show GameData where
    show gd = show (getRoomData gd)

instance Show Direction' where
   show dir = case dir of
                North -> "north"
                South -> "south"
                East -> "east"
                West -> "west"
                Outside -> "out"
                Inside -> "in"

{- Return the room the player is currently in. -}

getRoomData :: GameData -> Room
getRoomData gd = maybe undefined id (lookup (location_id gd) (world gd))

------------------------------------------------------------------
-- Function types

-- actions
type Action    = String -> GameData -> (GameData, String)
type ActionDir = Direction' -> GameData -> (GameData, String)
type ActionObj = DataDecl.Object -> GameData -> (GameData, String)

-- commands
type Command = GameData -> (GameData, String)
------------------------------------------------------------------
