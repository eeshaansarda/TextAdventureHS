module World where

data Object = Obj { obj_name :: String,
                    obj_longname :: String,
                    obj_desc :: String }
   deriving Eq

instance Show Object where
   show obj = obj_longname obj

data Exit = Exit { exit_dir :: String,
                   exit_desc :: String,
                   room :: String }
   deriving Eq

data Room = Room { room_desc :: String,
                   exits :: [Exit],
                   objects :: [Object] }
   deriving Eq

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

won :: GameData -> Bool
won gd = location_id gd == "street"

instance Show Room where
    show (Room desc exits objs) = desc ++ "\n" ++ concatMap exit_desc exits ++
                                  showInv objs
       where showInv [] = ""
             showInv xs = "\n\nYou can see: " ++ showInv' xs
             showInv' [x] = show x
             showInv' (x:xs) = show x ++ ", " ++ showInv' xs
                                  

instance Show GameData where
    show gd = show (getRoomData gd)

-- Things which do something to an object and update the game state
type Action  = String -> GameData -> (GameData, String)

-- Things which just update the game state
type Command = GameData -> (GameData, String)

mug, fullmug, coffeepot, key, mask, tooth_brush, paste :: Object
mug       = Obj "mug" "a coffee mug" "A coffee mug"
fullmug   = Obj "mug" "a full coffee mug" "A coffee mug containing freshly brewed coffee"
coffeepot = Obj "coffee" "a pot of coffee" "A pot containing freshly brewed coffee"
key       = Obj "key" "a key" "A small, silver key"
mask      = Obj "mask" "a mask" "A cloth face mask"
glasses   = Obj "item" "a pair of glasses" "A pair of glasses, with a very heavy prescription"
tooth_brush = Obj "tooth_brush" "a tooth_brush" "a blue tooth tooth_brush"
paste       = Obj "tooth_paste" "a tooth_paste" "colgate extra fresh tooth_paste"

bedroom, kitchen, hall, street :: Room

bedroom = Room "You are in your bedroom."
               [Exit "north" "To the north is a kitchen. " "kitchen",
                Exit "west" "To the west is a bathroom. " "bathroom"]
               [glasses, mug]

kitchen = Room "You are in the kitchen."
               [Exit "south" "To the south is your bedroom. " "bedroom",
                Exit "west" "To the west is a hallway. " "hall"]
               [coffeepot]

bathroom = Room "You are in the bathroom."
                [Exit "east" "To the east is your bedroom. " "bedroom"]
                [tooth_brush,paste]

hall = Room "You are in the hallway. The front door is closed. "
            [Exit "east" "To the east is a kitchen. " "kitchen",
             Exit "south" "To the south is a lounge. " "lounge"]
            []
            
lounge = Room "You are in the lounge."
            [Exit "north" "To the north is a hallway. " "hall"]
            [key]

-- New data about the hall for when we open the door

openedhall = "You are in the hallway. The front door is open. "
openedexits = [Exit "east" "To the east is a kitchen. " "kitchen",
               Exit "south" "To the south is a lounge. " "lounge",
               Exit "out" "You can go outside. " "porch"]
               
porch = Room "You are standing on the porch."
              [Exit "in" "You can go back inside.\nmust wear mask to .\ndoor Closed " "hall"]
              []

maskedporch = "You are standing on the porch and wearing your mask."
maskedexits = [Exit "in" "You can go back inside. " "hall",
               Exit "out" "You can go out into the street. " "street"]

street = Room "You have made it out of the house."
              [Exit "in" "You can go back inside if you like. " "porch"]
              []

gameworld = [("bedroom", bedroom),
             ("kitchen", kitchen),
             ("hall", hall),
             ("street", street),
             ("lounge", lounge),
             ("porch", porch),
             ("bathroom",bathroom)]

initState :: GameData
initState = GameData "bedroom" gameworld [mask] False False True False False False False False 

{- Return the room the player is currently in. -}

getRoomData :: GameData -> Room
getRoomData gd = maybe undefined id (lookup (location_id gd) (world gd))
