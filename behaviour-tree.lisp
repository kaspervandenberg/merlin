(in-package :merlin)

; Behaviour tree allows to define the AI for an NPC in a tree structure.
; Each node is a function that can return one of three results:

(defmacro defbehaviour (name &body body)
  "Behaviour tree allows to define the AI for an NPC in a tree structure.  
   Each node is a function that can return one of three results: failure
   (`nil`), success (`t`), and continue in the next game-loop iteration
   (a function to be called).  The function receives a single input 
   parameter: `entity`"
  `(defun ,name (entity)
     (declare (ignorable entity))
     ,@body))


(defun behaviour-sequence (&rest behaviours))


(defun make-behaviour-sequence (&rest behaviours)
  (labels ((f (entity behaviours)
             (if behaviours
               (let ((result (funcall (car behaviours) entity)))
                 (if (functionp result)
                   (g (cons result (cdr behaviours)))
                   (f entity (cdr behaviours))))
               t))
           (g (behaviours)
             (lambda (entity)
               (f entity behaviours))))
    (g behaviours)))


(defbehaviour end-turn
  (lambda (entity)
    (declare (ignore entity))
    t))
