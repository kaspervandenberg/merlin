; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
; vim: set filetype=lisp:

(defpackage #:net.kaspervandenberg.merlin.character-attributes
  (:nicknames #:merlin-character-attributes #:attr)
  (:documentation
    "Components that represent attributes that a player character and NPCs may
     have.  Abtributes are both learned/trained skills and innate abilities.")
  (:use :cl)
  (:export
    attribute-dice-pool
    entity-add-random-attribute
    entity-add-all-random-abilities
    
    strength
    dexterity
    health
    charisma
    manipulation
    resolve
    intelligence
    wisdom
    magic
    mana
    
    |Folkloric Customs & Manners|
    harvesting
    ))
