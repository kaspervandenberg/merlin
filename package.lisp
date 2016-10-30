(defpackage #:merlin
  (:documentation
    "Package for the interactive story telling game about the Arthurian Merlin Ambrosius.")
  (:use :cl)
  (:export
    *entities*
    *components*
    defsystem
    defbehaviour-task
    Component
    add-entity
    component-entity
    entity-add-fresh-component
    entity-add-random-character-identity
    entity-add-random-attribute
    entity-add-all-random-abilities
    select-entities-having
    all-components-of
    end-turn
    make-behaviour-sequence
    make-behaviour-selector
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

