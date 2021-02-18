# CS2006 Haskell 1: Text Adventure - Group Report
Submitted by Group 1 (190020857, 190022658, 200012696) to Edwin Brady.

---

This project is an implementation of a simple text adventure, as laid out the [project specification](https://studres.cs.st-andrews.ac.uk/2020_2021/CS2006/Practicals/H1/H1.html). Namely:
* The code stubs in `Actions.hs` have been implemented.
  * Movement between rooms is possible.
  * `get`, `drop`, and other actions have been implemented.
  * Helper commands, including a general `?` (beyond the specification), have been implemented.
* The game world has been extended.
  * A living room, bathroom, and porch have been added (see `img/Map.png`).
  * New puzzles were added:
    * Glasses need to be collected and worn before the text prompts can be read properly.
    * You need to brush your teeth in the bathroom before you can have your morning coffee.
    * You need to put your mask on before you can leave the porch.
* Object, direction, and command strings have been replaced by dedicated data types.
* The input is accepted using Haskeline (including history and tab completion), and processed using `Parsing.hs`.
* The parser accepts two commands at the same time, separated by "and" or "then".

In terms of team organisation, we had regular meetings to pick out new tasks and targets, which were then added to Microsoft Planner to keep track of progress. Between meetings, people would assign tasks to themselves and complete them, and intermediate progress reports could be added to the task in Planner to further break down the work if needed, or avoid two people doing the same thing. The provided submission is a Git repository, so precise breakdown of lines of code written can be found by running `git log` or `git blame` (note though that commit identities have not been anonymised -- they contain names and email addresses instead of matriculation numbers).

Instructions for running the game can be found in `README.md`. Instructions to complete the game can be found in `CHEAT.md`.

## Design

**Haskeline** is used for input and output in the program. This means that control keys like backspace work correctly (unlike in standard Haskell `IO` in GHCi), and the up and down arrows can be used to browse the history. There is also support for tab completion of commands, based on code by [C. A. McCann](https://stackoverflow.com/a/6165913/10664143).

An extra puzzle was added to the start of the game, which involves the player picking up and wearing their **glasses**. When not wearing glasses (either because they've not been put on yet, or because the player has removed them), all program output (excluding the input prompt) is made fuzzy by replacing a selection of characters with `▒`. Initially, this was going to be done randomly, but since all Haskell functions are pure this randomness would require extra overhead which would make the code structure more complicated. This was deemed unnecessary, so instead the messages are always fuzzed in the same way -- half of alphanumeric characters are replaced with the fuzz symbol, with the rest and any punctuation remaining unchanged. The fuzzing is implemented using `putStrFuzzy` and `putStrLnFuzzy` which are abstractions on top of `outputStr`, taking both the string to output and the current game state. If the game state indicates that the player is blind, the string is fuzzed before being outputted.

Before the player can drink their coffee, they also need to go to the **bathroom** to **brush their teeth**.

Another additional puzzle involves the player needing a **key** to unlock and open the door. The key is in a new room, the **lounge**.

The final extra puzzle requires the player to wear a **mask** before they can leave another new room, the **porch**. The mask starts hidden in the player's inventory, instead of in a room. To add some realism to the game, the player can't drink coffee while their mask is on.

We switched to using `Parsing.hs` to parse user input. The system also accepts two commands at once, split by "and" or "then".

## Testing



## Evaluation & Known Bugs
* The `save` and `load` commands bypass the parser and are handled directly in the main game loop. This is a good thing as it means that the `process` function doesn't need to worry about IO, but adds a bit of complication since commands are handled in more than one place and the game loop has some more layers. With more time, this could be restructured.


## Conclusion

