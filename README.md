# Hooridor

[![Build Status](https://travis-ci.com/imustafin/hooridor.svg?branch=master)](https://travis-ci.com/imustafin/hooridor)


Hooridor is Quoridor in Haskell language

Basic rules from [Quoridor Strats](https://quoridorstrats.wordpress.com/beginners-guide-rules-and-basics/)

* The game is played on a 9x9 board
* There are 2 players
* Each player starts with a pawn and ten walls
* Each turn is either moving the player's pawn OR placing a wall
* Pawns move up, down, left, or right
* Once placed, walls cannot be moved
* There should always be a path for the players to reach the goal

### Prerequisites

This project relies on the [Haskell Stack tool](https://docs.haskellstack.org/en/stable/README/).

It is recommended to get Stack with batteries included by
installing [Haskell Platform](https://www.haskell.org/platform/).

## Build

To build this project simply run

```sh
stack build
```

This will install all dependencies, including a proper version of GHC
(which should be there already if you have Haskell Platform 8.4.3).

## Run

This project has one executable that you can run with

```
stack exec hooridor-exe
```

## Documentation
Haddock documentation is [available](https://imustafin.github.io/hooridor/)!
