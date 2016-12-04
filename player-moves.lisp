;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; vim: set filetype=lisp:

;; The different moves (i.e. `behaviour-tasks`) that the player can perform
(in-package :merlin)

(bht:defbehaviour-task eval-player-input
  "Allow the player to input any s-expr and evaluate it."
  (format t "~%what do you do? (#eval) > ")
  (force-output)
  (let ((pl-input (read)))
    (eval pl-input)))


(defun gen-menu-selector (options)
  "Present the player a menu from which he/she can choose his//her next action.
   `options` is an alist of available options.  Each option is a list of three
   parts: a keyword (e.g. ':hack-n-slash') that the player can type to select 
   the option; a description of the option; and a behavior-task that defines the
   effect of the action"
  (labels ((prompt-user ()
             (format t "Choose among these options:~%~:{~a -->~16T~a~%~}What do you do? > " options)
             (force-output)
             (let ((inp (assoc (read) options)))
               (if (not inp)
                 (progn
                   (format t "Not a valid option; select an option from the list below")
                   (prompt-user))
                 inp))))
    (lambda (entity)
      (funcall (caddr (prompt-user)) entity))))


(defun gen-entity-attribute-dice-pool (attribute)
  (lambda (entity)
    (ecs:apply-components #'attr:attribute-dice-pool entity (list attribute))))


(defun gen-hack-and-slash (target counter-attack)
  "Attack target at close range with your melee weapon."
  (lambda (entity)
    (format t "You swing your weapon at ~a: ~a~%"
            target
            (describe-success (count-successes 7 (funcall (gen-entity-attribute-dice-pool 'attr:Strength) entity))))))
