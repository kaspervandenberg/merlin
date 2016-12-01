; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
; vim: set filetype=lisp:

;;;; Entities represent 'things' that exist in the game world; for example the 
;;;; Player character, monsters and NPCs, active spells, scenes, and room in a
;;;; dungeon.
;;;; Entities are implemented as Lisp symbols that have a collection of 
;;;; Components.
(in-package :net.kaspervandenberg.merlin.ecs)


(defvar *entities* nil
  "List of symbols that represent entities in the Entity–Component–System-architecture.
   Entities represent the entitities that make up the game \(i.e. player 
   characters, NPCs, and scene objects\).
   Conceptually Entities have Components and nothing more; in this 
   implementation \(and also common among other implementations\) Entities are 
   symbols without any content and Components refer to the entity that contain 
   them.
   `*entities*` is a dynamic variable so it can have different values at 
   different points in the game via `(let *entities* …)`.")


(define-condition Unknown-Entity (error)
  ((entity
    :initarg :entity
    :reader entity))
  (:documentation
    "Condition indicates that the function received an `entity` parameter with
     a value ∉ `*entities*`."))


(defun create-entity (&optional (name (gensym)))
  "Add a fresh Entity to `*entities*`.
   `name` is an optional parameter it sets the symbol to use for the entity;
   if ommitted `(gensym)` is used to generate a fresh symbol for the entity.
   `components` is a list of components to assign to the created entity."
  (progn 
    (pushnew name *entities*)
    name))


(defun with-existing-entity (entity func)
  "Check that `entity` ∈ `*entities*`; if it exists execute `body` else 
   signal `Unknown-Entity`.
   the symbol `entity` is bound to the value of `entity`; `body` can refer to 
   `#:entity` and use it."
  (labels 
    ((f () 
       (restart-case 
         (if (member entity *entities*)
           (funcall func entity)
           (error 'Unknown-Entity :entity entity))
         (skip-unknown-entity 
           () 
           :report (lambda (str) (format str "Skip entity ~a; do not call ~a with ~a" entity func entity)) 
           :test (lambda (e) (typep e 'Unknown-Entity)) 
           nil) 
         (add-entity-and-retry 
           () 
           :report (lambda (str) (format str "Add entity ~a to `*entities*` and retry ~a." entity func)) 
           :test (lambda (e) (typep e 'Unknown-Entity)) 
           (progn 
             (create-entity :name entity) 
             (f)))))) 
    (f)))


(defmacro with-existing-entity* (entity &body body)
  `(with-existing-entity ,entity (lambda (entity) ,@body)))



