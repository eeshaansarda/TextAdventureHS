module World where
    
import Data.Aeson
import DataDecl

won :: GameData -> Bool
won gd = location_id gd == "street"


mug, fullmug, coffeepot, key, mask, toothbrush, toothpaste :: DataDecl.Object
mug         = Obj "mug" "a coffee mug" "A coffee mug"
fullmug     = Obj "mug" "a full coffee mug" "A coffee mug containing freshly brewed coffee"
coffeepot   = Obj "coffee" "a pot of coffee" "A pot containing freshly brewed coffee"
key         = Obj "key" "a key" "A small, silver key"
mask        = Obj "mask" "a mask" "A cloth face mask"
glasses     = Obj "glasses" "a pair of glasses" "A pair of glasses, with a very heavy prescription"
toothbrush  = Obj "toothbrush" "a toothbrush" "a blue tooth toothbrush"
toothpaste  = Obj "toothpaste" "toothpaste" "colgate extra fresh toothpaste"

bedroom, kitchen, hall, street :: Room
bedroom = Room "You are in your bedroom."
               [Exit North "To the north is a kitchen. " "kitchen",
                Exit West "To the west is a bathroom. " "bathroom"]
               [glasses, mug]

kitchen = Room "You are in the kitchen."
               [Exit South "To the south is your bedroom. " "bedroom",
                Exit West "To the west is a hallway. " "hall"]
               [coffeepot]

bathroom = Room "You are in the bathroom."
                [Exit East "To the east is your bedroom. " "bedroom"]
                [toothbrush,toothpaste]

hall = Room "You are in the hallway. The front door is closed. "
            [Exit East "To the east is a kitchen. " "kitchen",
             Exit South "To the south is a lounge. " "lounge"]
            []
            
lounge = Room "You are in the lounge."
            [Exit North "To the north is a hallway. " "hall"]
            [key]

-- New data about the hall for when we open the door

openedhall  = "You are in the hallway. The front door is open. "
openedexits = [Exit East "To the east is a kitchen. " "kitchen",
               Exit South "To the south is a lounge. " "lounge",
               Exit Outside "You can go outside. " "porch"]
               
porch = Room "You are standing on the porch. The door is closed."
              [Exit Inside "You can go back inside." "hall"]
              []

maskedporch = "You are standing on the porch and wearing your mask."
maskedexits = [Exit Inside "You can go back inside. " "hall",
               Exit Outside "You can go out into the street. " "street"]

street = Room "You have made it out of the house."
              [Exit Inside "You can go back inside if you like. " "porch"]
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
--                   locationID         inventory  caffeinated  finished  pasteApplied  unlocked
--                               world         poured      blind      masked      brushed

instance ToJSON GameData where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON GameData

instance ToJSON Room where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Room

instance ToJSON Exit where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Exit

instance ToJSON Direction' where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Direction'

instance ToJSON DataDecl.Object where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON DataDecl.Object