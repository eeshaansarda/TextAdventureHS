module Actions where

import World

actions :: String -> Maybe Action
actions "go"      = Just go
actions "get"     = Just get
actions "drop"    = Just put
actions "pour"    = Just pour
actions "examine" = Just examine
actions "drink"   = Just drink
actions "open"    = Just open
actions _         = Nothing

commands :: String -> Maybe Command
commands "quit"      = Just quit
commands "inventory" = Just inv
commands _           = Nothing

{- Given a direction and a room to move from, return the room id in
   that direction, if it exists.

e.g. try these at the ghci prompt

*Main> move "north" bedroom
Just "kitchen"

*Main> move "north" kitchen
Nothing
-}

move :: String -> Room -> Maybe String
move dir rm = if length exit > 0 then -- exits exist
                      -- `head exit` will be the first (and only) exit in the direction.
                      -- `head` is safe here since to run this code, `length exit > 0`
                     Just (room (head exit))
                 else
                     Nothing
               where exit = filter (\candidate -> exit_dir candidate == dir) (exits rm)                     

{- Return True if the object appears in the room. -}

objectHere :: String -> Room -> Bool
-- Return True if object `o` is a member of the list of names of objects in room `rm`
objectHere o rm = elem o [y | x <- objects rm, y <- [obj_name x]]

{- Given an object id and a room description, return a new room description
   without that object -}

removeObject :: String -> Room -> Room
removeObject o rm = Room (room_desc rm)
                         (exits rm)
                         (filter (\candidate -> not(obj_name candidate == o)) (objects rm))
                                 -- Create list of objects excluding the one to remove

{- Given an object and a room description, return a new room description
   with that object added -}

addObject :: Object -> Room -> Room
addObject o rm = rm{ objects = o : objects rm }

{- Given an object id and a list of objects, return the object data. Note
   that you can assume the object is in the list (i.e. that you have
   checked with 'objectHere') -}

findObj :: String -> [Object] -> Object
findObj o ds = undefined

{- Use 'findObj' to find an object in a room description -}

objectData :: String -> Room -> Object
objectData o rm = undefined

{- Given a game state and a room id, replace the old room information with
   new data. If the room id does not already exist, add it. -}

-- Unsure
-- Can filter and then add
updateRoom :: GameData -> String -> Room -> GameData
updateRoom gd rmid rmdata = gd {
  world = (rmid, rmdata) : filter (\(x,_) -> not (x == rmid)) (world gd)
}

{- Given a game state and an object id, find the object in the current
   room and add it to the player's inventory -}

addInv :: GameData -> String -> GameData
addInv gd obj = undefined

{- Given a game state and an object id, remove the object from the
   inventory. Hint: use filter to check if something should still be in
   the inventory. -}

removeInv :: GameData -> String -> GameData
removeInv gd obj = gd { inventory = filter (\candidate -> not(obj_name candidate == obj)) (inventory gd) }

{- Does the inventory in the game state contain the given object? -}

carrying :: GameData -> String -> Bool
carrying gd obj = elem obj [y | x <- inventory gd, y <- [obj_name x]]
                            -- List of names of objects in inventory

{-
Define the "go" action. Given a direction and a game state, update the game
state with the new location. If there is no exit that way, report an error.
Remember Actions return a 2-tuple of GameData and String. The String is
a message reported to the player.

e.g.
*Main> go "north" initState
(kitchen,"OK")

-}

go :: Action
go dir state = undefined

{- Remove an item from the current room, and put it in the player's inventory.
   This should only work if the object is in the current room. Use 'objectHere'
   and 'removeObject' to remove the object, and 'updateRoom' to replace the
   room in the game state with the new room which doesn't contain the object.

   Hints: you will need to take care to update things in the right order here!
    * create a new state with the updated inventory (use 'objectData')
    * create a new room without the object (use 'removeObject')
    * update the game state with this new room in the current location
      (use 'location_id' to find where the player is)
-}

-- need to keep in mind how many times you use this_room because it changes
-- Unsure
get :: Action
get obj state
  | objectHere obj this_room = (updateRoom (addInv state obj) curr_location (removeObject obj this_room), response)
  | otherwise                = (state, "No such object here")
  where curr_location = location_id state
        this_room     = getRoomData state
        response      = obj ++ "is put in inventory"

{- Remove an item from the player's inventory, and put it in the current room.
   Similar to 'get' but in reverse - find the object in the inventory, create
   a new room with the object in, update the game world with the new room.
-}

-- need to keep in mind how many times you use this_room because it changes
-- Unsure
put :: Action
put obj state
  | carrying state obj = (updateRoom (removeInv state obj) curr_location (addObject item_obj this_room), response)
  | otherwise          = undefined
  where curr_location = location_id state
        this_room     = getRoomData state
        response      = obj ++ "is put outside the bag"
        item_obj      = findObj obj (inventory state)
        -- can't say in the room (street is not a room)

{- Don't update the state, just return a message giving the full description
   of the object. As long as it's either in the room or the player's 
   inventory! -}

-- TODO Test this after implementation of `findObj`, `objectData`
examine :: Action
examine obj state | carrying state obj        = (state, obj_desc (findObj    obj (inventory state)))
                  | objectHere obj this_room  = (state, obj_desc (objectData obj this_room))
                  | otherwise                 = (state, "")
                  where this_room = getRoomData state

{- Pour the coffee. Obviously, this should only work if the player is carrying
   both the pot and the mug. This should update the status of the "mug"
   object in the player's inventory to be a new object, a "full mug".
-}

pour :: Action
pour obj state = undefined

{- Drink the coffee. This should only work if the player has a full coffee 
   mug! Doing this is required to be allowed to open the door. Once it is
   done, also update the 'caffeinated' flag in the game state.

   Also, put the empty coffee mug back in the inventory!
-}

-- Unsure
drink :: Action
drink obj state
  | isCoffee && isFull = ( (addInv (removeInv state "fullmug") "mug")
                           { caffeinated = True },
                           "You drank coffee and are energized")
  | otherwise          = (state, "You need a full coffee mug for that")
        where
          isFull   = carrying state "fullmug"
          isCoffee = obj == "coffee"

{- Open the door. Only allowed if the player has had coffee! 
   This should change the description of the hall to say that the door is open,
   and add an exit out to the street.

   Use 'updateRoom' once you have made a new description. You can use 
   'openedhall' and 'openedexits' from World.hs for this.
-}

-- Unsure
-- What if the door is already open?
open :: Action
open obj state
  | caffeinated state && inHall
        = (updateRoom state curr_location hallDesc, "Opened the door")
  | caffeinated state = (state, "There's no door here")
  | otherwise = (state, "You need energy")  --don't open
        where inHall        = curr_location == "hall"
              curr_location = location_id state
              hallDesc      = Room openedhall openedexits []

{- Don't update the game state, just list what the player is carrying -}

inv :: Command
inv state = (state, showInv (inventory state))
   where showInv [] = "You aren't carrying anything"
         showInv xs = "You are carrying:\n" ++ showInv' xs
         showInv' [x] = obj_longname x
         showInv' (x:xs) = obj_longname x ++ "\n" ++ showInv' xs

quit :: Command
quit state = (state { finished = True }, "Bye bye")

