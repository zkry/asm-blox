  ![logo](./doc/asm-blox-logo.png)

# asm-blox: a programming game inspired by WAT and YAML

  Solve a variety of puzzles involving moving data using WAT and YAML.
  The game is played on a 3x4 grid where each grid can hold 11 lines
  of text.  Nodes can send to adjacent notes number values.  Using
  YAML, you can define extra components such as stacks and heaps to
  help you solve your puzzles.

  ![screenshot](./doc/screenshot1.gif)

  ![screenshot](./doc/screenshot2.gif)

# Installation

  The game is still in progress but you can install it by adding the
  files in your load path and calling `(require 'asm-blox)`.

# Usage

  You can initialize a game with the command <kbd>M-x asm-blox</kbd>.
  This will open up a menu with a list of puzzles to choose from.  By
  pressing <kbd>RET</kbd> a new solution to that puzzle will be
  created.  Back on the puzzle selection screen, you can view and open
  up saved files by pressing <kbd>RET</kbd> on the `[1]`-like text.

  For the rest, you'll have to look at the manual...


# Configuration

  - `asm-blox-save-directory-name`: the directory in which all puzzles
    will be saved.
