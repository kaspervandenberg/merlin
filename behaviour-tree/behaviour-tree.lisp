;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; vim: set filetype=lisp:

(in-package :net.kaspervandenberg.merlin.behaviour-tree)

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

(define-condition Not-a-Behaviour-Task (Error)
  ((func
     :initarg :func
     :reader func))
  (:documentation
    "Condition indicates that `func` is not a Behaviour-Task #\(i.e. a function 
     accepting a single parameter name entity#\)."))


(defun make-behaviour-sequence (&rest tasks)
  "Make a behaviour sequence, a behavour sequences executes its `tasks` in 
   sequence, until one fails."
  (lambda (entity) 
    (if tasks 
      (if (functionp (car tasks))
        (let ((result (funcall (car tasks) entity)))
          (if result 
            (if (functionp result)
              (apply #'make-behaviour-sequence (cons result (cdr tasks)))
              (funcall (apply #'make-behaviour-sequence (cdr tasks)) entity))
            result))
        (error 'Not-a-Behaviour-Task :func (car tasks)))
      t)))


(defun make-behaviour-selector (&rest tasks)
  "Make a behaviour selector, a behaviour select tries to execute its `tasks`
   starting with the first, if a tasks fails the next tasks is tried as an 
   alternative.  When one of the `tasks` succeeds, the behaviour selector stops
   trying alternatives an reutrns success to its parent."
  (lambda (entity)
    (if tasks
      (if (functionp (car tasks))
        (let ((result (funcall (car tasks) entity)))
          (if result
            (if (functionp result)
              (apply #'make-behaviour-selector (cons result (cdr tasks)))
              result)
            (funcall (apply #'make-behaviour-selector (cdr tasks)) entity)))
        (error 'Not-a-Behaviour-Task :func (car tasks)))
      nil)))


(defbehaviour-task end-turn
  "Yield control back to the behaviour executor."
  (lambda (entity)
    (declare (ignore entity))
    t))
