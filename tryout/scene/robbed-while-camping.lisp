;;;; Experiment with systems that can play a single scene.
;;;; What kind of systems are needed?  Do my ideas work?

;;;; The experiment should be a scene from a turn-based rpg/interactive story telling game, where
;;;; the program procedurally generates an 'interesting' scene, followed by alternating reactions
;;;; from the player and the program.

;;;; The scene starts with Merlin, the player character, camping besides the road and waking to the
;;;; sound of two bandits.


(defun create-triples-link-generator
    (name subject-type property target-class phase cardinality distinct-values)
  `((,subject-type kb-link-generated-by ,name)
    (,name rdf-type kb-link-generator)
    (,name kb-link-property ,property)
    (,name kb-link-object ,target-class)
    (,name kb-generate-phase ,phase)
    (,name kb-cardinality ,cardinality)
    (,name kb-generate-distinct-values ,distinct-values)))


(defun create-triples-rdfs-class (class-name description)
  `((,class-name rdf-type rdfs-class)
    (,class-name rdfs-label ,description)))


(defparameter *kb-test*
  `((hello)
    ,@(gkere)
    ,@(lkdsf)))

(defparameter *kb-test2*
  `(,@(hello)
      ,@(gkere)
      ,@(lkdsf)))

(defparameter *kb-test3*
  `(,hello
    ,fsfssf
    (vsdsad)
    ,@(fdd)
    ))

(defparameter *kb-test4*
  `(,@hello
    fee
    gerjje))

(defun kb-test5 ()
  `(,@(hello)
      ))

(defun kb-test6 ()
  `(,(hello)
     fds
     gjj))

(defun kb-test7 ()
  `(,(hello)
     ,(fef)))

(defun kb-test8 ()
  `(,(hello)
     ,hht))
(defun kb-test9 ()
  `(,@(hello)
      ,rere))

(defun kb-test10 ()
  `(,@(geerer)
      


(defparameter *random-generation-keywords*
  `(,@(create-triples-rdfs-class 'kb-gen-class
				 "Class of randomly generated classes.

Instances of subclasses of kb-gen-class have one or more properties that are generated via kb-link-generator.")
      ,@(create-triples-rdfs-class 'kb-link-generator
				   "Instances of this class detail how links should be generated.")
      (kb-link-generator rdfs-label "Instances of this class detail how links should be generated.")
      (kb-link-generator rdf-type rdfs-class)
      (kb-link-generated-by rdfs-label "Points to the `kb-link-generator` that generates this link.")
      (kb-link-generated-by rdf-type rdf-property)
      (kb-link-generated-by rdfs-range kb-link-generator)
      (kb-link-property rdfs-label "Gives the property of the generated links.")
      (kb-link-property rdf-type rdf-property)
      (kb-link-property rdfs-range rdf-property)
      (kb-value-generator rdfs-label "Instances of subclasses of this class detail how triple object values should be generated.")
      (kb-value-generator rdf-type rdfs-class)
      (kb-link-object rdfs-label )))


(defparameter *static-triples*
  `((robbery-scene rdf-type kb-gen-class)
    (robbery-scene rdfs-subclass-of scene)
    ,@(create-triples-link-generator 'gen-robbery-scene-npc 'robbery-scene 'actor 'robber 'scene-start 2 t)
    (robbery-scene kb-link-generated-by gen-robbery-scene-npc)
    (gen-robbery-scene-npc rdf-type kb-link-generator)
    (gen-robbery-scene-npc kb-link-property actor)
    (gen-robbery-scene-npc kb-link-object robbery-scene-target-class-value)
    (robbery-scene-npc-target-class-value rdf-type kb-fixed-value)
    (robbery-scene-ncp-target-class-value rdf-type kb-class-value-generator)
    (robbery-scene-npc-target-class-value kb-value robber)
    (gen-robbery-scene-npc kb-generate-phase scene-start)
    (gen-robbery-scene-npc kb-cardinality robbery-scene-npc-cardinality-value)
    (robbery-scene-npc-cardinality-value rdf-type kb-n-tries-random)
    (robbery-scene-npc-cardinality-value rdf-type kb-cardinality-value-generator)
    (robbery-scene-npc-cardinality-value kb-n-tries 6)
    (robbery-scene-npc-cardinality-value kb-probability 0.4)
    (gen-robbery-scene-npc kb-generate-distinct-values t)
    (robbery-scene kb-link-generated-by gen-robbery-scene-environment)
    (robbery-scene kb-link-generated-by gen-robbery-scene-merlin)
    (robber rdf-type kb-gen-class)
    (robber rdfs-subclass-of npc)
    (robber kb-link-generated-by gen-robber-behaviour)
    (robber kb-link-generated-by gen-robber-appearance)
    )
  "The static data that describes the potential scene.

The program uses this data to dynamically create a scene which can be played.  Change this data to
alter the scenes in future runs of the program.  The data is stored as a list of tripples.")
 


;;; Query

(defun generate-triples-links (generator-subject subject target-triples generator-data)
  (let ((predicate (triple-object (car (select-triple generator-data
						      :subject generator-subject
						      :predicate 'kb-link-property))))
	(target-class (triple-object (car (select-triple generator-data
							 :subject generator-subject
							 :predicate kb-link-object)))))))
