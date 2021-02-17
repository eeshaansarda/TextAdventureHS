module WorldParser where

import World
import Actions
import Parsing
import DataDecl

---------------

--- Parser
process :: GameData -> String -> (GameData, String)
process state input
  | not (length actionslist == 0) = doActions (fst (head actionslist)) state
  | not (length commandlist == 0) = commands (fst (head commandlist)) state
  | not (length actionlist == 0)  = actions  (fst (head actionlist))  state
  | otherwise                     = (state, "I don't understand")
        where commandlist = parse command input
              actionlist  = parse action input
              actionslist = parse twoActions input

              doActions :: (Action', Action') -> GameData -> (GameData, String)
              doActions (x, y) state = (state'', output ++ "\n" ++ output')
                        where (state', output) = actions x state
                              (state'', output') = actions y state'

-- Command -
command :: Parser Command'
command = do com <- identifier
             let a = getCommand com
             case a of
               Just x -> return (x)
               Nothing -> failure

-- ? does not get parsed by identifier
getCommand :: String -> Maybe Command'
getCommand "help"         = Just Help
getCommand "inventory" = Just Inventory
getCommand "quit"      = Just Quit
getCommand _           = Nothing
------------
-- Multiple Actions -

-- can do many with a bit of modification
twoActions :: Parser (Action', Action')
twoActions = do x <- action
                y <- identifier
                z <- action
                if y=="then" || y=="and" then return (x,z)
                  else failure

-----------
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
getActionObj "wear"    obj = Just (Wear obj)
getActionObj "remove"  obj = Just (Remove obj)
getActionObj "apply"   obj = Just (Apply obj)
getActionObj "drink"   obj = Just (Drink obj)
getActionObj "pour"    obj = Just (Pour  obj)
getActionObj  _        _   = Nothing

getActionStr :: String -> String -> Maybe Action'
getActionStr "brush"   str = Just (Brush str)
getActionStr "open"    str = Just (Open str)
getActionStr "unlock"  str = Just (Unlock str)
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
getObject "toothpaste" = Just toothpaste
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
actions (Remove str)  state = remove str state
actions (Unlock str)  state = unlock str state
actions (Apply str)   state = apply str state
actions (Brush str)   state = brush str state

commands :: Command' -> GameData -> (GameData, String)
commands Inventory state = inv state
commands Help      state = help state
commands Quit      state = quit state

getSave::Action' -> GameData -> (GameData, String)
actions (Save str)      state = save str state