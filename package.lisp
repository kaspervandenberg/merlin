(defpackage #:merlin
  (:documentation
    "Package for the interactive story telling game about the Arthurian Merlin Ambrosius.")
  (:use :cl :cl-ecs)
  (:export
    *entities*
    *components*
    Component
    add-entity
    component-entity
    entity-add-fresh-component
    select-entities-having
    all-components-of))

(cl-ecs:init-ecs)
