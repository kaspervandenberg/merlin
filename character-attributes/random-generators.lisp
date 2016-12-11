; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
; vim: set filetype=lisp:

;;;; Add random stat components to an entity

(in-package :net.kaspervandenberg.merlin.character-attributes)

(defun entity-add-random-attribute (entity attribute-class power-level &rest initargs)
  "Create a fresh instance of `attribute-type` setting its `dots` to a random
   value appropriate for a character of the given `power-level`.  `power-level`
   can be: `:mundane` for normal commoners, `:heroic` for powerful knights and
   mages, and `:godlike` for entities way more powerful than normal people and 
   regular heroes."
  (let ((d (dice (case power-level
                   (:mundane 3)
                   (:heroic 5)
                   (:godlike 7)))))
    (apply #'ecs:entity-add-fresh-component (append (list entity attribute-class :dots d) initargs))))


(defun entity-add-all-random-abilities (entity power-level)
  "Set all abilities of `entity` –for which the entity does not have yet have
   a component– to a random value of dots appropriate for the given 
   `power-level`.  `power-level` can be: `:mundane` for normal commoners, 
   `:heroic` for powerful knights and mages, and `:godlike` for entities way 
   more powerful than normal people and regular heroes."
  (let* ((all-abilities '(strength dexterity health charisma manipulation resolve
                                  intelligence wisdom magic mana))
         (fresh-abilities (remove-if (lambda (x) (ecs:all-components-of entity x)) all-abilities)))
    (mapc (lambda (x) (entity-add-random-attribute entity x power-level)) fresh-abilities)))


