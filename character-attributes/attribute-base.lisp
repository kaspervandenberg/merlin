; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
; vim: set filetype=lisp:

;;;; Abstract base on which skills and abilities are built

(in-package :net.kaspervandenberg.merlin.character-atrributes)

(defclass Level-Description nil 
  ((level :initarg :level 
          :documentation "The number of dice the skill adds to a check.  The 
                          value ranges from 0 upto 5.")
   (keyword :initarg :keyword 
            :documentation "A short (one or two word) description of the level")
   (description :initarg :description
                :reader level-description
                :documentation "(Optional) longer description of the level of 
                                skill or ability to give an idea what one can 
                                accomplish at this level and how common or rare 
                                it is to find someone with this level"))
  (:documentation "Describe a attribute of skill level."))


(defclass Attribute (ecs:Component)
  ((description :reader attribute-description
                :documentation "Text describing what can be done with this 
                                Attribute")
   (levels :reader attribute-levels
           :documentation "Description for the levels of competence/innate 
                           prowess.")
   (dots :initarg :dots
         :reader attribute-dots
         :documentation "The number of dice the entity may throw when making a
                         ability or skill check with this ability or skill.  It
                         gives the level of competence."))
  (:documentation "Base base for Ability and Skill components."))


(defclass Ability (Attribute)
  ()
  (:documentation "An innate physical, mental, or social characteristic of an 
                   entity."))


(defclass Skill (Attribute)
  ((base-ability :reader base-ability
                 :documentation "The ability that is used in combination with 
                                 this skill for skill checks.")
   (station :reader attribute-station
            :documentation "The class/station in medieval society the skill is 
                            most often possessed by people of this station.")))


(defmacro defability (name description &body levels)
  "Define a component that represents an ability."
  `(flet ((deflevel (l)
           (destructuring-bind (lvl kw &optional descr) l
             (make-instance 'Level-Description :level lvl :keyword kw :description descr))))
    (let ((lvls (mapcar #'deflevel (quote ,levels))))
      (defclass ,name (Ability)
        ((description :allocation :class
                      :initform (quote ,description))
         (levels :allocation :class
                 :initform lvls))))))


(defmacro defskill (name ability-type station description &body levels)
  "Define a component that represents a skill."
  `(flet ((deflevel (l)
            (destructuring-bind (lvl kw &optional descr) l
              (make-instance 'Level-Description :level lvl :keyword kw :description descr))))
     (let ((lvls (mapcar #'deflevel (quote ,levels))))
       (defclass ,name (Skill)
         ((base-ability :allocation :class
                        :initform (quote ,ability-type))
          (station :allocation :class
                   :initform (quote ,station))
          (description :allocation :class
                       :initform (quote ,description))
          (levels :allocation :class
                  :initform lvls))))))

