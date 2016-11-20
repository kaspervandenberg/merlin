;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; vim: set filetype=lisp:

(in-package :merlin)

(defclass Move-in-Progress (Component)
  ((resolution-dice-roll
     :initform nil
     :documentation "One of the `*n-successes-interpretation*` values that 
                     describes the dice result of the player's response; i.e. 
                     `:botch`, `:failure`, `:marginal`, `:moderate`, 
                     `:complete`, `:exceptional`, and `:phenomenal`.  Note 
                     that the consequences of the `Move-in-Progress` are 
                     inverse to the `resolution-dice-roll`; e.g. when the player
                     scores an `:exceptional` the `Move-in-Progress` should fail
                     and when the player scores a `:failure` the `Move-in-Progress`
                     should have nasty consequences for the player.")
   (resolution-move :initform nil))
  (:documentation
    "In Dungeon world Monsters and the GM often make moves that demand action 
     from the player character.  The effect of the move depends on what the
     player character try to do in response and whether the character's action
     succeeds.  `Move-in_Progress` captures this move that awaits the player's
     response before it is resolved.
     `Move-in-Progress` is a `Component` normally it should be attached to the 
     entity #\(e.g. the monster or some adventure scence#\) that makes the 
     initial move #\(e.g. threat to the player character#\)."))


(defgeneric is-resolved (move)
  (:documentation 
    "Return `T` when the player #\(or someone else#\) has responsed to the move."))

(defmethod is-resolved ((move Move-in-Progress))
  (or (slot-value move 'resolution-dice-roll)
      (slot-value move 'resolution-move)))


(defgeneric make-behaviour-await-player-response (move)
  (:documentation
    "Return a behaviour-task that yields its turn until `move` has been resolved."))

(defmethod make-behaviour-await-player-response ((move Move-in-Progress))
  (labels ((await (_)
             (declare (ignore _))
             (if (not (is-resolved move))
               #'await
               t)))
    #'await))


(defgeneric make-behaviour-selector-resolution-dice-roll (move tasks-by-result)
  (:documentation
    "Generate a behaviour-task that selects and executes one of the tasks ∈ 
     `tasks-by-result` based on the `resolution-dice-roll` of `move`.
     `tasks-by-result` is a plist of `*n-successes-interpretation*`– 
     `behaviour-task`-pairs."))

(defmethod make-behaviour-selector-resolution-dice-roll ((move Move-in-Progress) tasks-by-result)
  (lambda (entity)
    (funcall (getf tasks-by-result 
                   (slot-value move 'resolution-dice-roll)
                   (lambda (_) (declare (ignore _)) nil))
             entity)))
