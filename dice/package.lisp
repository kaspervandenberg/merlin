; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
; vim: set filetype=lisp:

(defpackage #:net.kaspervandenberg.merlin.dice
  (:nicknames #:merlin-dice #:dice)
  (:documentation
    "Random dice rolls and challenge levels")
  (:use #:cl)
  (:export
    dice-pool
    roll-dice
    describe-success))
