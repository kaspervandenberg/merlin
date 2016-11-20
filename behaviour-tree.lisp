;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; vim: set filetype=lisp:

(in-package :merlin)

; Behaviour tree allows to define the AI for an NPC in a tree structure.
; Each node is a function that can return one of three results:

(defmacro defbehaviour-task (name &body body)
  "Behaviour-task allows to define the AI for an NPC in a tree structure.  Each 
   node is a function that can return one of three results: failure 
   #\(`nil`#\), success #\(`t`#\), and continue in the next game-loop iteration 
   #\(a function to be called#\).  The function receives a single input 
   parameter: `entity`."
  `(defun ,name (entity)
     (declare (ignorable entity))
     ,@body))


(defun compose-behaviour (fComposer final-result &rest tasks)
  "Compose a behaviour task by applying `fComposer` to the results of calling 
   the `tasks`.  `fComposer` receives the following parameters:
   - a function that can be called to recurse into the remaining tasks
   - the result of the most recently executed behaviour task."
  (labels ((f (entity tasks)
             (if tasks
               (let ((result (funcall (car tasks) entity)))
                 (if (functionp result)
                   (g (cons result (cdr tasks)))
                   (funcall fComposer (lambda () (f entity (cdr tasks))) result)))
               final-result))
           (g (tasks)
             (lambda (entity)
               (f entity tasks))))
    (g tasks)))


(defun make-behaviour-sequence (&rest tasks)
  "Make a behaviour sequence, a behavour sequences executes its `tasks` in 
   sequence, until one fails."
  (compose-behaviour (lambda (f result)
                       (if result
                         (funcall f)
                         result))
                     t
                     tasks))


(defun make-behaviour-selector (&rest tasks)
  "Make a behaviour selector, a behaviour select tries to execute its `tasks`
   starting with the first, if a tasks fails the next tasks is tried as an 
   alternative.  When one of the `tasks` succeeds, the behaviour selector stops
   trying alternatives an reutrns success to its parent."
  (compose-behaviour (lambda (f result)
                       (if (not result)
                         (funcall f)
                         result))
                     nil
                     tasks))


(defbehaviour-task end-turn
  "Yield control back to the behaviour executor."
  (lambda (entity)
    (declare (ignore entity))
    t))


