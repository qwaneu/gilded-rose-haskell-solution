# SPOILER ALERT

This repository contains a solution and QuickCheck properties.

# Haskell port of the Gilded-Rose Kata, with QuickCheck.

This is a solution for the [Haskell port of the *Gilded-Rose-Kata*](https://github.com/sheyll/gilded-rose-haskell). This version has been
modified, so we can use QuickCheck.

## Build and test. 

Install [Stack](https://github.com/commercialhaskell/stack) , then run
`stack test`. If that fails at first, follow instructions to get the
dependencies.

QuickCheck properties and generators are in `test/GildedRoseProp.hs`. A
solution can be found in `src/ImprovedGildedRose.hs`.

Refer to http://hspec.github.io/ for more information about writing tests using `Hspec`.

## Credits

The [original problem description and
exercise](https://github.com/NotMyself/GildedRose) in C# was created by
[Terry Hughes](https://github.com/NotMyself/).

The Haskell version was created by [Sven Heyll](https://github.com/sheyll),

[Emily Bache's interpretation](https://github.com/emilybache/GildedRose-Refactoring-Kata) with text-based approval testing was a source of inspiration. 
