;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; vim: set filetype=lisp:

(defpackage #:merlin-asd
  (:use :cl :asdf))

(in-package #:merlin-asd)

(defsystem merlin
  :name "Merlin"
  :version 0.0.0
  :maintainer "Kasper van den Berg <kasper@kaspervandenberg.net>"
  :author "Kasper van den Berg <kasper@kaspervandenberg.net>"
  :description "Text based single player RPG/interactive storytelling game in 
                which the player plays the role of Merlin the wizard from the 
                King Arthur legends."
  :license "MIT license, see file LICENSE.md"
  :serial T
  :depends-on ("usocket")
  :components ((:file "package")
               (:module "entity-component-system"
                :components
                ((:file "package")
                 (:file "entity" :depends-on ("package"))
                 (:file "component" :depends-on ("package" "entity"))
                 (:file "system" :depends-on ("package" "component"))))
               (:module "behaviour-tree"
                :components
                ((:file "package")
                 (:file "behaviour-tree" :depends-on ("package"))))
               (:file "text-rendering")
               (:file "wod-dice")
               (:file "arthurian-character-names")
               (:file "skills-and-abilities")
               (:file "moves")
               (:file "player-moves")
               (:file "server")))
