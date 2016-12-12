;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; vim: set filetype=lisp:

;;;; Attach a behaviour-tree as a component to an entity

(in-package :merlin)


(defclass Behaviour (ecs:Component)
  ((tree
     :initarg :tree
     :accessor tree
     :documentation "The #\(sub#\)-tree that the entity will execute next."))
  (:documentation
    "Attach executable behaviour to an entity."))


(ecs:defsystem execute-behaviour ((bht 'Behaviour))
  (let ((res (funcall (tree bht) (ecs:entity bht))))
    (if (functionp res)
      (setf (tree bht) res)
      (ecs:delete-component bht))))
