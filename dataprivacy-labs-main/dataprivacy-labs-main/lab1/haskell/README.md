# lab1/haskell

This directory contains the Haskell project template for Lab 1.

For a description of the lab assignment, see the Canvas page.

## Installing Haskell

The recommended way to install Haskell's toolchain (GHC and Cabal) is through [GHCup](https://www.haskell.org/ghcup/).
We recommend using GHC 9.10.3 and Cabal 3.16.0.0.
GHCup by default might install different versions, you can run `ghcup tui` to make sure these are the ones you're using.

## Installing dependencies

Cabal will automatically build all required dependencies the first time `cabal build` or `cabal test` is run.
