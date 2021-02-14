module WorldParser where

import World
import Actions
import Parsing
import DataDecl

---------------

--- Parser

userInput :: GameData -> String -> (GameData, String)
userInput state input
  | not (commandStr == "") = commands commandStr state
  | not (actionStr == "")  = actions  actionStr  state
  | otherwise        = (state, "Invalid input")
        where commandStr = fst (head (parse command input))
              actionStr  = fst (head (parse action input))

-- Command -
command :: Parser Command'
command = do com <- identifier
             let a = getCommand com
             case a of
               Just x -> return (x)
               Nothing -> failure

getCommand :: String -> Maybe Command'
getCommand "?"         = Just Help
getCommand "inventory" = Just Inventory
getCommand "quit"      = Just Quit
getCommand _           = Nothing
------------

-- Action -
-- Here need to parse direction / object / string first

action :: Parser Action'
action = do act <- identifier
            (do arg <- object
                let a = getActionObj act arg
                case a of
                  Just x -> return (x)
                  Nothing -> failure)
             ||| (do arg <- direction
                     let a = getActionDir act arg
                     case a of
                       Just x -> return (x)
                       Nothing -> failure)
             ||| (do arg <- identifier
                     let a = getActionStr act arg
                     case a of
                       Just x -> return (x)
                       Nothing -> failure)


getActionDir :: String -> Direction' -> Maybe Action'
getActionDir "go" dir = Just (Go dir)
getActionDir _    _   = Nothing

getActionObj :: String -> Object -> Maybe Action'
getActionObj "get"     obj = Just (Get obj)
getActionObj "drop"    obj = Just (Put obj)
getActionObj "examine" obj = Just (Examine obj)
getActionObj "open"    obj = Just (Open obj)
getActionObj "wear"    obj = Just (Wear obj)
getActionObj "unlock"  obj = Just (Unlock obj)
getActionObj "apply"   obj = Just (Apply obj)
getActionObj  _        _   = Nothing

getActionStr :: String -> String -> Maybe Action'
getActionStr "pour"    str = Just (Pour  str)
getActionStr "brush"   str = Just (Brush str)
getActionStr "drink"   str = Just (Drink str)
getActionStr  _        _   = Nothing
-----------

-- Direction -
direction :: Parser Direction'
direction = do dir <- identifier
               let a = getDirection dir
               case a of
                 Just x -> return (x)
                 Nothing -> failure

getDirection :: String -> Maybe Direction'
getDirection "north" = Just North
getDirection "south" = Just South
getDirection "east" =  Just East
getDirection "west" = Just West
getDirection "outside" = Just Outside
getDirection "inside" = Just Inside
getDirection _ = Nothing
---------------

-- Object -
object :: Parser Object
object = do obj <- identifier
            let a = getObject obj
            case a of
              Just x -> return (x)
              Nothing -> failure

getObject :: String -> Maybe Object
getObject "mug" = Just mug
getObject "coffee" = Just coffeepot
getObject "key" = Just key
getObject "mask" = Just mask
getObject "glasses" = Just glasses
getObject "toothbrush" = Just toothbrush
getObject "paste" = Just paste
getObject _ = Nothing
-----------



actions :: Action' -> GameData -> (GameData, String)
actions (Go str)      state = go str state
actions (Get str)     state = get str state
actions (Put str)     state = put str state
actions (Pour str)    state = pour str state
actions (Examine str) state = examine str state
actions (Drink str)   state = drink str state
actions (Open str)    state = open str state
actions (Wear str)    state = wear str state
actions (Unlock str)  state = unlock str state
actions (Apply str)   state = apply str state
actions (Brush str)   state = brush str state

commands :: Command' -> GameData -> (GameData, String)
commands Inventory state = inv state
commands Help      state = help state
commands Quit      state = quit state
