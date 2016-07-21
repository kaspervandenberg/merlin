(in-package :merlin)

; Behavior tree allows to define the AI for an NPC in a tree structure.
; Each node is a function that can return one of three results:

(defmacro defbehavior (name &body body)
  "Behavior tree allows to define the AI for an NPC in a tree structure.  
   Each node is a function that can return one of three results: failure
   (`nil`), success (`t`), and continue in the next game-loop iteration
   (a function to be called).  The function receives a single input 
   parameter: `entity`"
  `(defun ,name (entity) ,@body))


(defun behavior-sequence (behaviors)
  (if behaviors 
    (lambda (entity)
      (let ((first-result (funcall (car behaviors) entity)))
        (if first-result
          (if (functionp first-result)
            (behavior-sequence (cons first-result (cdr behaviors)))
            (funcall (behavior-sequence (cdr behaviors)) entity))
          nil)))
    (lambda (_)
      (declare (ignore _))
      t)))


(defbehavior end-of-turn
  (declare (ignore entity))
  t)
