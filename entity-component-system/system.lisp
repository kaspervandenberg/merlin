; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
; vim: set filetype=lisp:

;;;; Systems represent the active part of the game, systems receive an Entity 
;;;; and read and modify its components
(in-package :ecs)

(defun build-component-list (entity component-types)
  "Build an argument list by selecting the components of the given `component-types`
   among `(all-components-of entity)`.
   If a component-type appears multiple times in `component-types` each time a fresh
   component from `(all-components-of entity)` is selected.  If multiple componets
   of `(all-components-of entity)` match the component-type, the first non-used is
   passed to the resulting argument list."
  (let ((*components* (all-components-of entity)))
    (labels ((builder (types inverse-args all-boundp)
               (if types 
                 (let ((arg (set-difference (get-components-of-type (car types)) inverse-args)))
                   (builder (cdr types) (cons (car arg) inverse-args) (and arg all-boundp)))
                 (values (reverse inverse-args) all-boundp))))
      (builder component-types nil t))))


(defun apply-components (func entity component-types &optional call-with-incomplete-list)
  (multiple-value-bind (args all-boundp) (build-component-list entity component-types)
    (if (or all-boundp call-with-incomplete-list)
      (apply func args))))


(defmacro defsystem (name components &body body)
  "Define a system that receives the specified components.  The system is a 
   function that accepts a single argument the entity, binds the components as 
   specified in `components`, and then executes `body`.  `components` is an 
   alist of symbol component class pairs."
  (let* ((meta-components components)
        (meta-arg-decl (mapcar #'car meta-components))
        (meta-component-types (mapcar #'cadr meta-components)))
    `(defun ,name (entity)
       (apply-components
         (lambda ,meta-arg-decl ,@body)
         entity
         (list ,@meta-component-types)))))

