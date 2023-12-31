module Actions where

import qualified Data.ByteString.Lazy as B
import Data.Aeson
import World
import DataDecl

{- | Given a direction and a room to move from, return the room id in
   that direction, if it exists.
-}
move       :: Direction' -> Room -> Maybe String
move dir rm | length exit > 0 = Just (room (head exit))
                      -- exits exist
                      -- `head exit` will be the first (and only) exit in the direction.
                      -- `head` is safe here since to run this code, `length exit > 0`
            | otherwise       = Nothing
               where exit = filter (\candidate -> exit_dir candidate == dir) (exits rm)


{- | Return True if the object appears in the room. -}
objectHere     :: DataDecl.Object -> Room -> Bool
objectHere o rm = elem o (objects rm)


{- | Given an object id and a room description, return a new room description
     without that object -}
removeObject      :: DataDecl.Object -> Room -> Room
removeObject o rm = Room (room_desc rm)
                         (exits rm)
                         (filter (\candidate -> not(candidate == o)) (objects rm))
                                 -- Create list of objects excluding the one to remove

{- | Given an object and a room description, return a new room description
     with that object added -}
addObject     :: DataDecl.Object -> Room -> Room
addObject o rm = rm { objects = o : objects rm }

{-

findObj and objectData are not useful when using data structures instead of strings.

{- | Given an object id and a list of objects, return the object data. Note
     that you can assume the object is in the list (i.e. that you have
     checked with 'objectHere') -}
findObj     :: String -> [DataDecl.Object] -> DataDecl.Object
findObj o ds = head (filter (\x -> obj_name x == o) ds) -- used head as the list will never be empty
               where search id obj | obj_name obj == id  = True
                                   | otherwise           = False

{- | Use 'findObj' to find an object in a room description -}
objectData     :: String -> Room -> DataDecl.Object
objectData o rm = findObj o (objects rm)

-}

{- | Given a game state and a room id, replace the old room information with
     new data. If the room id does not already exist, add it. -}
updateRoom               :: GameData -> String -> Room -> GameData
updateRoom gd rmid rmdata = gd {
  world = (rmid, rmdata) : filter (\(x,_) -> not (x == rmid)) (world gd)
  }

{- | Given a game state and an object id, find the object in the current
     room and add it to the player's inventory -}
addInv       :: GameData -> DataDecl.Object -> GameData
addInv gd obj = gd {inventory = obj : inventory gd}

{- | Given a game state and an object id, remove the object from the
     inventory. -}
removeInv       :: GameData -> DataDecl.Object -> GameData
removeInv gd obj = gd { inventory = filter (\candidate -> not(candidate == obj)) (inventory gd) }

{- | Does the inventory in the game state contain the given object? -}
carrying       :: GameData -> DataDecl.Object -> Bool
carrying gd obj = elem obj [x | x <- inventory gd]

{- |
Define the "go" action. Given a direction and a game state, update the game
state with the new location. If there is no exit that way, report an error.
Remember Actions return a 2-tuple of GameData and String. The String is
a message reported to the player.
e.g.
*Main> go "north" initState
(kitchen,"OK")
-}
go          :: ActionDir
go dir state = check (move dir (getRoomData state))
                  where check Nothing  = (state, "Unknown location")
                        check (Just a) = (state { location_id = a }, "Moved")


{- |
   Remove an item from the current room, and put it in the player's inventory.
   This should only work if the object is in the current room. Use 'objectHere'
   and 'removeObject' to remove the object, and 'updateRoom' to replace the
   room in the game state with the new room which doesn't contain the object.
-}
get :: ActionObj
get obj state
  | objectHere obj this_room = (updateRoom (addInv state obj) curr_location (removeObject obj this_room), response)
  | otherwise                = (state, "No such object here")
  where curr_location = location_id state
        this_room     = getRoomData state
        response      = (obj_name obj) ++ " is put in inventory"

{- |
   Remove an item from the player's inventory, and put it in the current room.
   Similar to 'get' but in reverse - find the object in the inventory, create
   a new room with the object in, update the game world with the new room.
-}
put :: ActionObj
put obj state
  | carrying state obj = (updateRoom (removeInv state obj) curr_location (addObject obj this_room), response)
  | otherwise          = (state, "No such item")
  where curr_location = location_id state
        this_room     = getRoomData state
        response      = (obj_name obj) ++ " has been dropped"


{- |
   Don't update the state, just return a message giving the full description
   of the object. As long as it's either in the room or the player's 
   inventory! -}
examine :: ActionObj
examine obj state | carrying state obj        = (state, obj_desc obj)
                  | objectHere obj this_room  = (state, obj_desc obj)
                  | otherwise                 = (state, "This object is not here.")
                  where this_room = getRoomData state

{- |
   Pour the coffee. This should only work if the player is carrying
   both the pot and the mug. This should update the status of the "mug"
   object in the player's inventory to be a new object, a "full mug".
-}
pour :: ActionObj
pour obj state
  | obj == coffeepot && carrying state mug && carrying state coffeepot 
     = (state {
           inventory = (fullmug : filter (\x -> not (x == mug)) (inventory state)),
           poured = True
         },
        "Coffee Poured")
  | obj == coffeepot = (state, "Get mug and coffee")
  | otherwise        = (state, "Can't pour coffee")

{- |
   Drink the coffee. This only works if the player has a full coffee 
   mug! Doing this is required to be allowed to open the door. Once it is
   done, also update the 'caffeinated' flag in the game state.
   Also, put the empty coffee mug back in the inventory!
-}
drink :: ActionObj
drink obj state
  | isCoffee && isFull && hasBrushed && not wearingmask
      = (state {
            inventory = (mug : filter (\x -> not (x == fullmug)) (inventory state)),
            caffeinated = True,
            poured = False
         },
         "You feel energized")
  | hasBrushed == False = (state, "Brush your teeth before you drink coffee")
  | wearingmask         = (state, "Remove your mask to drink")
  | otherwise           = (state, "You need a full coffee mug for that")
    where
      isFull      = poured state
      wearingmask = masked state
      isCoffee    = obj == coffeepot
      hasBrushed  = brushed state

{- |
   Open the door. Only allowed if the player has had coffee! 
   This changes the description of the hall to say that the door is open,
   and adds an exit out to the street.
-}
open :: Action
open str state
  | not inHall && not inPorch                    = 
      -- No door in this room
      (state, "There's no door here")
  | inHall  && doorUnlocked && caffeinated state = 
      -- OK to unlock hall door
      (updateRoom state curr_location hallDesc, "Opened the door")
  | inPorch && maskWorn                          = 
      -- OK to unlock porch door
      (updateRoom state curr_location porchDesc, "Opened the porch door")
  | inPorch                                      =
      -- Missing mask in the porch
      (state, "Don't forget your mask" )
  | not doorUnlocked                             =
      -- Can't open a locked door.
      -- Don't need to differentiate between hall and porch door since to open porch
      -- door you must've opened the hall door.
      (state, "The door must be unlocked first")
  | not (caffeinated state)                      =
      -- Can't open door before coffee
      -- Don't need to differentiate between hall and porch door since to open porch
      -- door you must've opened the hall door.
      (state, "You need energy")
    where inHall        = curr_location == "hall"
          inPorch       = curr_location == "porch"
          curr_location = location_id state
          hallDesc      = Room openedhall openedexits []
          porchDesc     = Room maskedporch maskedexits []
          doorUnlocked  = unlocked state
          maskWorn      = masked state

{-| Unlock the door in the current room. Only works if the player has the key in their inventory. -}
unlock :: Action
unlock str state | not hasKey                        = (state,                   "Have you lost your key again?")
                 | inHall && hasKey && str == "door" = (state {unlocked = True}, "Unlocked the door")
                 | otherwise                         = (state,                   "You can't unlock that")
                   where inHall        = curr_location == "hall"
                         curr_location = location_id state
                         hasKey        = carrying state key

{- | Used to apply toothpaste to toothbrush, if both are in inventory. -}
apply :: ActionObj
apply obj state | obj == toothpaste && gotBrush && gotPaste = 
                    -- Got all items, good to apply
                    (state {pasteApplied = True}, "Toothpaste applied to brush")
                | gotBrush && gotPaste                      =
                    -- Can't apply anything other than toothpaste
                    (state, "Please apply \"toothpaste\" to the brush")
                | otherwise                                 =
                    -- Need to collect items
                    (state, "Please attain brush and toothpaste")
                  where gotBrush = carrying state toothbrush
                        gotPaste = carrying state toothpaste

{-| Action to brush teeth and provide an updated game state. -}
brush :: Action
brush str state | pasteApplied' && gotBrush && str == "teeth" = 
                    -- Got a brush with toothpaste and trying to brush teeth - OK!
                    (state {brushed = True, pasteApplied = False}, "Your teeth are shining")
                | pasteApplied' && gotBrush =
                    -- Trying to brush something that is not teeth
                    (state, "Is that really what you want to brush? I didn't think so.")
                | gotBrush && gotPaste =
                    -- Got brush and paste, but not applied paste yet
                    (state, "Please apply \"toothpaste\" to the brush")
                | otherwise = 
                    -- Missing an item
                    (state, "Please attain toothbrush and paste")
                  where gotBrush      = carrying state toothbrush
                        gotPaste      = carrying state toothpaste
                        pasteApplied' = pasteApplied state
                        
{-| Removes a wearable item from the inventory and updates the game state appropriately. -}
wear :: ActionObj
wear obj state | not (carrying state obj) = (state,                     "You're not carrying that at the moment")
               | obj == mask              = (newState {masked = True }, "Mask worn")
               | obj == glasses           = (newState {blind  = False}, "Wow!")
               | otherwise                = (state,                     "You can't wear that")
                 where newState = removeInv state obj -- take item out of inventory, since we're wearing it now
                    
{-| Removes a wearable item from the game state and puts it back in the inventory. -}
remove :: ActionObj
remove obj state | obj == mask && masked state         = (newState {masked = False}, "Mask removed and placed in your inventory")
                 | obj == glasses && not (blind state) = (newState {blind  = True }, "Glasses removed and placed in your inventory")
                 | otherwise                           = (state,                     "You're not wearing that at the moment")
                   where newState = addInv state obj -- place item into inventory

{-| Don't update the game state, just list what the player is carrying -}
inv      :: Command
inv state = (state, showInv (inventory state))
   where showInv [] = "You aren't carrying anything" ++ maskAndGlass
         showInv xs = "You are carrying:\n" ++ showInv' xs
         showInv' [x] = obj_longname x ++ maskAndGlass
         showInv' (x:xs) = obj_longname x ++ "\n" ++ showInv' xs
         maskAndGlass| masked state && not(blind state) = "\nYou are wearing mask and glasses" 
                     | masked state                     = "\nYou are wearing mask" 
                     | not (blind state)                = "\nYou are wearing glasses"
                     |otherwise                         = "\nYou are not wearing any accessories"

{-| End the game. -}
quit      :: Command
quit state = (state { finished = True }, "Bye bye")

{-| List all possible actions and commands in the game -}
help      :: Command
help state = (state, " Actions:\n\t go get drop pour examine drink open unlock wear remove apply brush \n\n Commands: \n\t ? inventory load save quit")
