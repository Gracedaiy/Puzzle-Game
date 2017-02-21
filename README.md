# Puzzle Games #

## Description ##
This project mean to apply higher-order function, monad and CSPs to solve Puzzle Games. First, there are some basic data type need to illustrate:
* `type Puzzle a b = [(b, a)]` - Puzzle is difined as a list of pairs which indicates the position in the puzzle.
* `type Constraint a = (a -> Bool)` - Unary constraint type.
* `type Constraint a b = (a -> b -> Bool)` - Binary constraint type.
* `type Csp a = [Constraint a]` - Csp is a list of  unary constraints.
* `type Csp2 a b = [Constriant a b]` - Csp2 is a list of binary constraints.
There are also some execution functions:
* `hasSolution` - Check whether there is a solution for Sudoku.
* `solu` - One solution for Sudoku.
* `listsu` - List all solutions for Sudoku.

## Examples ##
To play with Sudoku game, first you should download the Haskell platform and use "ghci" command to enter the platform. Then use `:l sudoku.hs` command to load the sudoku-solver. Once you are in the program, type "main 0" to print only one possible solution from given sudoku problems, otherwise, type "main 1" to print all possible solutions. Here is an example:
```
.51|9.3|...
32.|...|..5
.69|...|.3.
--- --- ---
..2|.9.|143
945|...|762
173|.6.|8..
--- --- ---
.3.|...|58.
8..|...|..6
...|7..|3..

Print one solution 

451|923|678
328|176|495
769|458|231
--- --- ---
682|597|143
945|831|762
173|264|859
--- --- ---
234|619|587
817|345|926
596|782|314
```
Same thing with animal game. 
- step 1: `ghci`
- step 2: `:l animal.hs`
- step 3: `main 0 animalPuzzle` - for just one possible solution
- step 4: `main 1 animalPuzzle` - for all possible solutions
Here is another example:
- My example is derived from this link http://www.enchantedlearning.com/math/logic/puzzles/favanimal.shtml
```
      BCHE
Sue  |....
Bill |....
DiDi |....
Ben  |....

B: Butterfly; C: Cat; H: Horse ; E: Elephant

Print one solution 

      BCHE
Sue  |1000
Bill |0001
DiDi |0100
Ben  |0010

B: Butterfly; C: Cat; H: Horse ; E: Elephant
```
