(defpackage #:merlin
  (:documentation
    "Package for the interactive story telling game about the Arthurian Merlin Ambrosius.")
  (:use :cl)
  (:export
    start-server
    entity-add-random-character-identity
    entity-add-random-attribute
    entity-add-all-random-abilities
    Move-in-Progress
    make-behaviour-await-player-response
    make-behaviour-selector-resolution-dice-roll
    attribute-dice-pool
    character-identity
    name
    gender
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
    harvesting))

