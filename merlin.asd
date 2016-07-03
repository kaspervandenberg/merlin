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
  :description "Interactive story telling game about the Arthurian wizard Merlin Ambrosius."
  :serial T
  :components ((:file "package")
               (:file "wod-dice")
               (:file "arthurian-character-names")
               (:file "attributes-and-abilities.lisp"))
  :depends-on ("cl-ecs"))
