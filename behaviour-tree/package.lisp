; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
; vim: set filetype=lisp:

(defpackage #:net.kaspervandenberg.merlin.behaviour-tree
  (:nicknames #:merlin-behaviour-tree #:merlin-bht #:bht)
  (:documentation
    "Behaviour tree allows to define the AI for an NPC in a tree structure.
     Each node is a function that can return one of three results:
     * `t`, the action was successful;
     * `nil`, the action failed; and,
     * a function, the action is not yet complete, it should continue in 
       the next 'turn'.")
  (:use #:cl)
  (:export
    defbehaviour-task
    make-behaviour-sequence
    make-behaviour-selector
    end-turn))
