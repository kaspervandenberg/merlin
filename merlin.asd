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
               (:file "arthurian-character-names" :depends-on ("entity-component-system"))
               (:module "character-attributes"
                :depends-on ("entity-component-system" "wod-dice")
                :components
                ((:file "package")
                 (:file "attribute-base" :depends-on ("package"))
                 (:file "abilities" :depends-on ("package" "attribute-base"))
                 (:file "skills" :depends-on ("package" "attribute-base" "abilities"))
                 (:file "accessors" :depends-on ("package" "attribute-base"))
                 (:file "random-generators" :depends-on ("package" "attribute-base" "abilities" "skills"))))
               (:file "moves" :depends-on ("entity-component-system"))
               (:file "player-moves" :depends-on ("entity-component-system" "behaviour-tree" "character-attributes"))
               (:file "server" :depends-on ("entity-component-system" "behaviour-tree" "text-rendering") )))
