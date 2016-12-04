(defpackage #:merlin
  (:documentation
    "Package for the interactive story telling game about the Arthurian Merlin Ambrosius.")
  (:use :cl)
  (:export
    start-server
    entity-add-random-character-identity
    Move-in-Progress
    make-behaviour-await-player-response
    make-behaviour-selector-resolution-dice-roll
    character-identity
    name
    gender))

