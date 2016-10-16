(defpackage #:merlin
  (:documentation
    "Package for the interactive story telling game about the Arthurian Merlin Ambrosius.")
  (:use :cl)
  (:export
    *entities*
    *components*
    Component
    add-entity
    component-entity
    entity-add-fresh-component
    entity-add-random-character-identity
    entity-add-random-attribute
    entity-add-all-random-abilities
    select-entities-having
    all-components-of
    attribute-dice-pool
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

