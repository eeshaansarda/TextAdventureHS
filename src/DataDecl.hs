module DataDecl where

-- Data Declaration -------------------------------------------

data Command' = Quit | Inventory | Help
   deriving Eq
--x---------------------x------------------x--
data Direction' = North | East | South | West | Inside | Outside
   deriving Eq
--x---------------------x------------------x--
data Action' = Go Direction' | Get Object | Put Object | Pour Object
             | Examine Object | Drink Object | Open String| Wear Object
             | Unlock String | Apply Object | Brush String
   deriving Eq
--x---------------------x------------------x--

data Object = Obj { obj_name :: String,
                    obj_longname :: String,
                    obj_desc :: String }
   deriving Eq
--x---------------------x------------------x--

data Exit = Exit { exit_dir :: Direction',
                   exit_desc :: String,
                   room :: String }
   deriving Eq
--x---------------------x------------------x--

data Room = Room { room_desc :: String,
                   exits :: [Exit],
                   objects :: [Object] }
   deriving Eq
--x---------------------x------------------x--

data GameData = GameData { location_id :: String, -- where player is
                           world :: [(String, Room)],
                           inventory :: [Object], -- objects player has
                           poured :: Bool, -- coffee is poured
                           caffeinated :: Bool, -- coffee is drunk
                           blind :: Bool, -- true while glasses are not on
                           finished :: Bool, -- is game finished
                           masked :: Bool, -- is the mask being put on
                           pasteApplied :: Bool,-- paste has been applied to the tooth-brush
                           brushed :: Bool, -- is teeth brushed
                           unlocked :: Bool -- is the front door locked
                         }
------------------------------------------------------------------

-- Show ---------------------------------------------------------
instance Show Object where
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

{- Return the room the player is currently in. -}

getRoomData :: GameData -> Room
getRoomData gd = maybe undefined id (lookup (location_id gd) (world gd))

------------------------------------------------------------------

-- Things which do something to an object and update the game state
type Action    = String -> GameData -> (GameData, String)
type ActionDir = Direction' -> GameData -> (GameData, String)
type ActionObj = Object -> GameData -> (GameData, String)

-- Things which just update the game state
type Command = GameData -> (GameData, String)
------------------------------------------------------------------
