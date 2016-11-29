; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
; vim: set filetype=lisp:

(defpackage #:net.kaspervandenberg.merlin.ecs
  (:nicknames #:merlin-ecs #:ecs)
  (:documentation
    "An entity-component-system architecture for games in Common Lisp.
     Entities are symbols that group components together.  Components
     are CLOS classes that contain data.  An entity gains and loses 
     capabilities by adding and removing components to/from it.  
     Systems read and modify components and define the behaviour of 
     the game.
     See http://www.gamedev.net/page/resources/_/technical/game-programming/understanding-component-entity-systems-r3013")
  (:use #:cl)
  (:export
    create-entity
    with-existing-entity
    Component
    entity-add-fresh-component
    all-components-of
    apply-components
    defsystem))
