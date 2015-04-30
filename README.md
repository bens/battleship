# battleship
Battleship game API

## Building
To build the project use `cabal-install` with `cabal repl`, that should load the `Battleship` module
and you can then run the `test` function to play a quick game.

This was developed quickly, just using ghc-7.10.1, it hasn't been tested on older versions.  Instead
of sensible version bounds in battleship.cabal `cabal freeze` was used, just for expediency.

To run the tests use `cabal test --show-details=always`.

## Design

The main two features are that the `Board` type is a comonad and the board preparation code exploits
that, and that the two players are represented as co-routines that are called in turn by a game
function.  They are provided with information about changes in the game but there's no way for them
to find out details about their opponent that they shouldn't know, short of passing in the opponent's
board in a closure.

Some care was taken to use a restricted `Prism` interface to `Position` and `Distance` to maintain
their invariants but some places duck under that and use the newtype directly.

Most of the code was written by trying it out all together and there were only bugs in the parts that
types didn't distinguish, for example passing the last couple of moves taken to each player.  The moves
aren't typed by which player made them so naturally they got muddled up until testing discovered the
bug.  One other bug was not realising that `StateT` under `Producer` resets the state between calls to
`lift`, which was solved by moving `Producer` to be under `StateT` instead.
