# Merlin
Text based single player RPG/interactive storytelling game in which the player plays the role of Merlin the wizard from the King Arthur legends.

## Status
Currently the game is under development (in its initial stages) and not ready for playing.

## License
Copyright © 2016, Kasper van den Berg

[MIT License](LICENSE.md)

# Requirements
* Common Lisp
  ([SBCL](http://www.sbcl.org/) with the intention to be portable to other CL implementations)
* [CL-ECS](https://github.com/lispgames/cl-ecs): An implementation of the Entity-Component-System pattern mostly used in game development.
* An ASDF package/library manager, such as [Quicklisp](https://www.quicklisp.org/beta/)

# Installation
1. Install Quicklisp; see [Quicklisp § Installation](https://www.quicklisp.org/beta/#installation)
2. Clone [merlin.git](https://github.com/kaspervandenberg/merlin.git)
3. Link or copy `merlin.asd` to the `local-projects` directory of Quicklisp
4. Start the Lisp REPL and load Quicklisp: `CL-USER> (load "~/quicklisp/setup.lisp")`
5. Load merlin via quick lisp: `CL-USER> (ql:quickload :merlin)`

# Useage
The game is not ready for playing.  It does not yet export any functions/symbols/etc. you can access the defined functions via `merlin::`.

# Documentation

Currently the only documentation available are the function descriptions in the source code.
