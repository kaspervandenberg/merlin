;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; vim: set filetype=lisp:

;; Entity Component System architecture for Merlin
;; Entity–Component–System is a pattern for building flexible games.
;; See http://www.gamedev.net/page/resources/_/technical/game-programming/understanding-component-entity-systems-r3013

(in-package :merlin)

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


(defvar *components* nil
  "List of `Components` that have been constructed.")


(defclass Component nil
  ((entity 
     :initarg :entity
     :reader entity))
  (:documentation
    "Components hold the data of an `Entity` \(e.g. an entity's position, 
     speed, strength attribute, intelligence attribute, and AI\).  Contracty to 
     the object–oriented-paradigm Components define only data and do not define 
     operations to perform on the data."))


(define-condition Unknown-Entity (error)
  ((entity
    :initarg :entity
    :reader entity))
  (:documentation
    "Condition indicates that the function received an `entity` parameter with
     a value ∉ `*entities*`."))


(defun add-entity (&optional (name (gensym)))
  "Add a fresh Entity to `*entities*`.
   `name` is an optional parameter it sets the symbol to use for the entity;
   if ommitted `(gensym)` is used to generate a fresh symbol for the entity.
   `components` is a list of components to assign to the created entity."
  (progn 
    (push name *entities*)
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
             (add-entity :name entity) 
             (f)))))) 
    (f)))


(defmacro with-existing-entity* (entity &body body)
  `(with-existing-entity ,entity (lambda (entity) ,@body)))


(defmethod initialize-instance :after ((instance Component) &rest initargs)
  (declare ( ignore initargs))
  (push instance *components*))


(defgeneric (setf component-entity) (entity component)
  (:documentation
    "Assign `component` to `entity`.  If the `component` belonged to an other 
     Entity `e2`, then `e2` looses ownership of the component."))


(defmethod (setf component-entity) (entity (component Component))
  (with-existing-entity* entity
    (setf (slot-value component 'entity) entity)))


(defun get-components-of-type (component-type)
  "Filter `*components*` to include only `Components` of type `component-type`."
  (flet ((is-component-type (x) (typep x component-type)))
    (remove-if-not #'is-component-type *components*)))


(defun entity-add-fresh-component (entity component-class &rest component-initargs)
  "Create a fresh-instance of `component-class` and add it to the components of 
   `entity`.
   `component-initargs` are supplied to `(make-instance component-class …)`."
  (with-existing-entity* entity
    (apply #'make-instance (append (list component-class :entity entity) component-initargs))))


(defun select-entities-having (component-type)
  "Return the entities ∈ `*entities*` that have at least one `Component` of
   `component-type`."
  (intersection *entities*
                    (mapcar #'entity 
                      (get-components-of-type component-type))))


(defun all-components-of (entity &optional (component-type 'Component))
  "Return all `Components` of `entity`.
   If `component-type` is specified, only the `Components` of `component-type`
   are returned otherwise all `Components` are returned."
  (with-existing-entity* entity
    (remove-if-not (lambda (x) (eql x entity)) 
                     (get-components-of-type component-type) 
                     :key #'entity)))
