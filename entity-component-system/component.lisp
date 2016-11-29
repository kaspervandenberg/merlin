; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
; vim: set filetype=lisp:

;;;; Components represent blocks of data that are part of an entity.
(in-package :ecs)

(defvar *components* nil
  "List of `Components` that have been constructed.")


(defclass Component nil
  ((entity 
     :initarg :entity
     :reader entity))
  (:documentation
    "Components hold the data of an `Entity` \(e.g. an entity's position, 
     speed, strength attribute, intelligence attribute, and AI\).  Contrary to 
     the object–oriented-paradigm Components define only data and do not define 
     operations to perform on the data."))



(defmethod initialize-instance :after ((instance Component) &rest initargs)
  (declare ( ignore initargs))
  (push instance *components*))


(defgeneric (setf component-entity) (entity component)
  (:documentation
    "Assign `component` to `entity`.  If the `component` belonged to an other 
     Entity `e2`, then `e2` looses ownership of the component."))


(defmethod (setf component-entity) (entity (component Component))
  (ecs::with-existing-entity* entity
    (setf (slot-value component 'entity) entity)))


(defun get-components-of-type (component-type)
  "Filter `*components*` to include only `Components` of type `component-type`."
  (flet ((is-component-type (x) (typep x component-type)))
    (remove-if-not #'is-component-type *components*)))


(defun entity-add-fresh-component (entity component-class &rest component-initargs)
  "Create a fresh-instance of `component-class` and add it to the components of 
   `entity`.
   `component-initargs` are supplied to `(make-instance component-class …)`."
  (ecs::with-existing-entity* entity
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
  (ecs::with-existing-entity* entity
    (remove-if-not (lambda (x) (eql x entity)) 
                     (get-components-of-type component-type) 
                     :key #'entity)))


