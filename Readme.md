Description
===========

As usual, I didnt enter the contest but worked on an entry in my spare time during the week after. It seems like a good opportunity to play with new libraries and technologies.

This year the libraries where:
- 'bound' for variable scoping and substitution,
- 'aeson' for producing and parsing json,
- 'redis' for storage,
- 'llvm' for code generation.

The solution is incomplete in many ways. Off the top of my head:
- the program generator doesn't produce fold programs,
- there is no top level driver for solving batches of problems,
- some of the refinments, such as the llvm code generator or the tabling, are not efficient yet.

I stole many ideas from other entries, including:
- dynamic programming techniques for generation of the search tree,
- algebraic identities for pruning the search tree,
- abstract interpretation of closed expressions for further pruning.

Authors
=======

- Jose Iborra