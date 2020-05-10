---
toptitle: Projects
projects: true
---


# Projects

You can check out the coding projects I'm developing and maintaining by looking at my [github account](https://github.com/dwincort).  These projects are all in functional languages, with my recent work on Adaptive Fuzz in [OCaml](http://ocaml.org/) and the rest in [Haskell](http://www.haskell.org/haskellwiki/Haskell).

## Row-Types

Row-types is a library for extensible records and variants written in Haskell.  Check out the [website](https://target.github.io/row-types/) for examples, as well as some posts about cool things you can do with row-types.

## Adaptive Fuzz

Adaptive Fuzz is a language for performing differentially private computations in a safe, adaptive way.  Adaptive Fuzz uses the type system from [Fuzz](http://privacy.cis.upenn.edu/software.html) to ensure that every computational "piece" that uses sensitive data is properly differentially private.  This is achieved by using a novel system of partial evaluation to turn seemingly dynamic "sensitivity" types into concrete values so that the type checker can check these pieces at runtime but still before they are provided with any sensitive data.  Additionally, the language allows periods of adaptivity between these pieces and ties them all together using state of the art privacy filters.  Using Adaptive Fuzz currently requires building it from [source from github](https://github.com/dwincort/AdaptiveFuzz); instructions can be found in the accompanying readme.

## UISF

UISF is an arrowized FRP library for GUI creation that stems from work on Euterpea.  It was originally born during an overhaul of Euterpea's graphics package, which we decided to spin into its own package.  UISF is straightforward to use and has a bunch of useful built-in widgets, but it also makes it easy for people to create their own widgets.  You can download the [source from github](https://github.com/dwincort/UISF) or install from [Hackage](http://hackage.haskell.org/package/UISF) with "cabal install uisf".

There are a number of example GUIs in the examples folder, but my personal favorite is the [pinochle](http://en.wikipedia.org/wiki/Pinochle) example.  It provides a GUI to enter your pinochle hand, and then it tells you your possibilities and expected value of meld if you win the kitty.

The library is built on top of GLFW and provides an arrowized interface to layout and combine widgets: the arrow structure makes bidirectional widgets particularly easy.

## CFRP

CFRP (Communicating Functional Reactive Processes) is a language demonstrating ideas for resource-safe asynchronous AFRP that I am developing in conjunction with my graduate research.  It is more a collection of thoughts than a package.

## Euterpea

Euterpea is a domain-specific language for computer music that has been in development by Paul Hudak for years now.  As a senior member of Paul's group, I am one of the lead developers and maintainers of Euterpea.  You can read more about it on [euterpea.com/](http://www.euterpea.com/).
