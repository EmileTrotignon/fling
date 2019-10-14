# fling
author : Emile Trotignon

## What is done

The base game is complete.

I did an initial situation generator (in valid.ml).
Currently it is always run for size 10, but the function is programmed for any size, making it variable is juste a matter of user interface.
This function has two "mode" (chosen by a boolean argument). In the first one, every game the selected is generated, and one is chosen at random.
In the other, random choices are made while generating the tree.

## Choices

The main unconventional choice here is the use of the functor Set.Make to define the game type.
Even if game contains full-fledged balls, only the position is used to define the comparison operator.
The advantages are many :
- it is guaranteed that you only have one ball at every position
- checking if there is a ball at a particular position is faster than with the list (O(log(n) instead of O(log(n)))
- very nice union and difference functions
- it is very easy to define a set of games (useful for valid.ml)

There is a main disadvantage which is that you cannot match a set to an empty and a non-empty one :
(=) is not set equality, you have to use Set.S.equal. I like pattern matching.

I also used dune to compile. The makefile still works, it's just that I know dune is the package builder for big ocaml project, and I wanted to try it out.


