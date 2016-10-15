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
    entity-add-random-attribute
    select-entities-having
    all-components-of
    attribute-dice-pool
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

