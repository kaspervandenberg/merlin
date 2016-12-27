; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
; vim: set filetype=lisp:

;;;; Functions to retrieve and make use of attributes.

(in-package :net.kaspervandenberg.merlin.character-attributes)

(defun print-dots (n)
  "Print a sequence of '⚫' that indicates a character's strength in
   an attribute or skill.  Each dot allows for rolling a single dice."
  (loop for i from 1 to n
        do (princ "⚫"))
  (princ #\Space))

(defgeneric attribute-level (attr)
  (:documentation
   "Return the `Level-Description` that corresponds with the `dots` of `attr`."))

(defmethod attribute-level ((attr Attribute))
  (let ((dots (attribute-dots attr)))
    (remove-if (lambda (l) (not (eql (slot-value l 'level) dots)))
               (attribute-levels attr))))


(defgeneric get-skill-ability (s)
  (:documentation "Return the `ability` component of the `skills`'s `entity`."))

(defmethod get-skill-ability ((s Skill))
  (car (ecs:all-components-of (entity s) (slot-value s 'Base-Ability))))



(defgeneric attribute-dice-pool (attr)
  (:documentation 
    "Create a dice pool for `attribute` (i.e. an `ability` or `skill` of 
     `entity`."))
            
(defmethod attribute-dice-pool ((ablty Ability))
  (dice:dice-pool (attribute-dots ablty)))

(defmethod attribute-dice-pool ((skl Skill))
  (dice:dice-pool (+ (attribute-dots (get-skill-ability skl))
                     (attribute-dots skl))))


