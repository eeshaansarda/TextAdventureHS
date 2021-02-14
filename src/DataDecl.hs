module DataDecl where

import World
-- Data structs

data Command' = Quit | Inventory | Help
  deriving Eq
data Direction' = North | East | South | West | Inside | Outside
  deriving Eq
data Action' = Go Direction' | Get Object | Put Object | Pour String
             | Examine Object | Drink String | Open Object| Wear Object
             | Unlock Object | Apply Object | Brush String
  deriving Eq
