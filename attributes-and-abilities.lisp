
(in-package :merlin)

(defclass level-description nil 
  ((level :initarg :level 
          :documentation "The number of dice the skill adds to a check.  The 
                          value ranges from 0 upto 5.")
   (keyword :initarg :keyword 
            :documentation "A short (one or two word) description of the level")
   (description :initarg :description 
                :documentation "(Optional) longer description of the level of 
                                skill or ability to give an idea what one can 
                                accomplish at this level and how common or rare 
                                it is to find someone with this level"))
  (:documentation "Describe a attribute of skill level."))


(defclass attribute nil
  ((name :initarg :name 
         :documentation "Identifier for the attribute")
   (ecs-compnent :initarg :ecs-component 
                 :documentation "The component to assign to `entities` that have 
                                 this attribute.")
   (description :initarg :description
                :documentation "Text describing what can be done with this 
                                attribute.")
   (levels :initarg :levels
           :documentation "Description for the levels of competence (i.e. the 
                           number of dots an entity has)."))
  (:documentation "A broad pysical characteristic a character has and that can 
                   be used"))


(defclass skill nil
  ((name :initarg :name
         :documentation "Identifier for the skill")
   (ecs-compnent :initarg :ecs-component
                 :documentation "The component to assign to `entities` that have
                                 this skill.")
   (attribute :initarg :attribute :type attribute
              :documentation "When making a check, which attribute should be 
                              combined with the skill to determine the dice 
                              pool?")
   (description :initarg :description
                :documentation "Text describing what can be done with this 
                                skill")
   (levels :initarg :levels
           :documentation "Description for the levels of competence (i.e. the 
                           number of dots an entity has)."))
  (:documentation "A specific skill attained throught study and/or practise."))



(if nil
  (progn 
    (defvar attribute-descriptions
      nil
      "alist of (attribute-name . description)-conses.  Description is a list of 
       symbols that translate to a human understandable description (in English) of
       the attribute.  Use `print-attribute-description` to display the attribute's
       description.") 
    (defvar attribute-level-descriptions 
      nil 
      "Nested structure of alist of (attribute-name . alist)-conses.  The inner
       alist is an alist of (level keyword)-lists.  The level indicates how
       many dice the character may roll `keyword` is a one or two word
       (English) description of the attribute level.  Optionally, the list can
       contain a more verbose description after the keyword part (not yet used).") 
    (defvar skill-attributes
      nil
            "When making a check, which attribute should be combined with the skill to
             determine the dice pool?  This alist contains (skill .attribute) conses 
             that indicate which attribute to use for a skill check.")

             (defvar skill-stations nil
               "Different persons have access to different skills based on to which medieval
                station, i.e. rank in society, they belong.  `skill-stations` is an alist of
                (skill-name . stations)-conses
                where `stations` is a list of social stations ")

                (defvar skill-descriptions nil
                  "alist of (skill-name . description)-conses.  Like `attribute-descriptions`
                   `description is a list of symbols giving a human understandable description
                   (in English) of the skill.")

                   (defvar skill-level-descriptions nil
                     "Nested structure of alist of (skill-name . alist)-conses; similar to 
                      `attribute-level-descriptions`.  The nested alist contains lists with the
                      structure (level keyword).  Level indicates how many dice a character has
                      in his dice pool for the given skill check.  `keyword is a list with a one 
                      or two word description (in English) for the skill level.  Optionally, the 
                      list can contain a more verbose description of the level of the skill.")))



 
(if nil
  (defmacro defattribute (name general-description &body level-descriptions)
    `(progn  (unless (member (quote ,name) (cl-ecs::all-components))
               (cl-ecs:defcomponent ,name (dots)))
             (pushnew '(,name . ,general-description)     attribute-descriptions          :key #'car)
             (pushnew '(,name . ,level-descriptions)      attribute-level-descriptions    :key #'car))))
(defmacro defattribute (name general-description &body level-descriptions)
  (let ((symbName (gensym)) (symbGenDescr (gensym)) (lvl-descrs (gensym)))
    `(let ((symbName (quote ,name))))
    )
  )



(defmacro defskill (name attribute station-list general-description &body level-descriptions)
  `(progn (unless (member (quote ,name) (cl-ecs::all-components))
            (cl-ecs:defcomponent ,name (dots)))
          (pushnew '(,name . ,attribute)                skill-attributes                :key #'car)
          (pushnew '(,name . ,general-description)      skill-descriptions              :key #'car) 
          (pushnew '(,name . ,level-descriptions)       skill-level-descriptions        :key #'car)
          (pushnew '(,name . ,station-list)             skill-stations                   :key #'car)))


(defattribute strength 
              (strength is a |character's| muscle |power,| it determines how much 
                        he can lift and how much physical damage he does in 
                        melee combat.) 
              (1 (Weak)) 
              (2 (Average))
              (3 (Strong))
              (4 (Exceptional))
              (5 (Outstanding)))

(defattribute dexterity 
              (skill and grace in physical movement.  skills like |dancing,| 
                     |riding,| |sleight of hand,| and archery require dexterity.  
                     in combat dexterity determines the chance to hit an 
                     opponent and the chance to dodge incoming attacks.) 
              (1 (Clumsy))
              (2 (Average))
              (3 (Dexterous))
              (4 (Graceful))
              (5 (Outstanding)))

(defattribute heatlh 
              (health defines how many hits you can |take,| how long you can 
                      keep at a strenuous activity like running or |climbing,| 
                      and how often your character is sick.)
              (1 (Frail/sickly))
              (2 (Average))
              (3 (Healthy))
              (4 (Vigorous))
              (5 (Outstanding)))

(defattribute charisma 
              (charisma is charm and force of personality.  characters with 
                        charisma are |likeable,| |persuasive,| |inspiring,| and 
                        they appear trustworthy.  it allows the character to win 
                        others for his views.)
              (1 (|Obnoxious/annoying|))
              (2 (Inconspicuous))
              (3 (Charming))
              (4 (Charismatic))
              (5 (Outstanding)))

(defattribute manipulation 
              (manipulation is flexibility in a social setting.  |it's| the 
                            quality of expressing oneself in such a way that 
                            others agree and comply.  compared to charisma 
                            manipulation is more cleverness with words.  people 
                            do what a character wants because his words seem 
                            true or the only option not because they trust the 
                            character or see him as a leader.)
              (1 (awkward))
              (2 (average))
              (3 (persuasive))
              (4 (exceptional))
              (5 (outstanding)))

(defattribute resolve 
              (how determined is a character to continue arguing his case.  
                   how |deterimined/focussed| is a character to continue to 
                   persue an intellectually challen investigation resolve 
                   determines both how many social punches a character can take 
                   before giving up and yielding to an opponent and how many 
                   intellectual setbacks/hits a character can take before 
                   giving up or losing his mind.  |(resolve| is on the social 
                   and mental scale as hitpoints is on the physical |scale.)|)
              (1 (hesitant))
              (2 (average))
              (3 (determined))
              (4 (tenacious))
              (5 (outstanding)))

(defattribute intelligence
              (a |character's| ability to |reason,| to study and learn academic 
                 |subjects,| to concentrate on an |(intellectual)| task at hand.  
                 the |character's| level of general knowledge and the ability 
                 to remember facts when they are helpful.  Effectively and 
                 flexibly using magic requires intelligence.  |(intelligence| 
                 is on the mental scale as is strength on the physical scale.  
                 and intelligence is on the magical scale as dexterity is on 
                 the physical |scale.)|) 
              (1 (dumb)) 
              (2 (average)) 
              (3 (intelligent)) 
              (4 (genius)) 
              (5 (outstanding))) 

(defattribute wisdom 
              (the ability to perceive |one's| surroundings.  the ability for 
                   quick thinking and flexibly use |one's| intelligence.)
              (1 (dull-witted))
              (2 (average)) 
              (3 (wise))
              (4 (sagious)) 
              (5 (outstanding)))

(defattribute magic
              (the power of a |mage's| spells.)
              (1 (poor))
              (2 (average))
              (3 (good))
              (4 (exceptional))
              (5 (outstanding)))

(defattribute mana 
              (the magical reserves a mage has.  some spells require spending 
                   some mana.  Mana is on the mental scale as health is on the 
                   physical scale.)
              (1 (little))
              (2 (average))
              (3 (potent))
              (4 (exceptional))
              (5 (outstanding)))

(setf attribute-descriptions (nreverse attribute-descriptions))
(setf attribute-level-descriptions (nreverse attribute-level-descriptions))

; Skills from Loseth's Dungeoneer game (or game concept, work in progress); a 
; rich and realistically set of medieval skills.
; See https://forum.rpg.net/showthread.php?510194-Dungeoneer-Rpg,
; http://s328.photobucket.com/user/loseth/media/ClassCards1.jpg.html?sort=3&o=11
; Is it suitable for Merlin?  It lacks magic skills that the player character
; and mage NPCs require, but that can be augmented.  It features lots of skills
; for the mundane people.  What is the effect of having a rich skill system
; compared to a few broader skills?
(defskill |Folkloric Customs & Manners|
  charisma 
  (underclass-village
    lowerclass-f-village lowerclass-m-village
    lower-middle-class-f-village lower-middle-class-m-village
    middle-class-f-village middle-class-m-village)
  (knowing how to behave when among the common people passing as a fellow 
           commoner.  knowing when which folk feast is celebrated at what time 
           and what it means.)
  (0 (none) (you lack any knowledge about |Folkloric Customs & Manners|))
  (1 (poor) (you have litte knowledge about |Folkloric Customs & Manners|.
                 people notice you are not from their regions or not from their
                 station.  You easily offend people by acting not according to 
                 their manners.))
  (2 (average) (you know what most people know about |Folkloric Customs & Manners|.
                    you easily fit in and can pass for one of the guys))
  (3 (good) (you know the |Folkloric Customs & Manners| better than most 
                 including commoners themselfs.  Gaining drinking buddies in the
                 local alehouse comes easy to you.  Locals ask your advise about
                 |Folkloric Customs & Manners|))
  (4 (exceptional) (you are an authority on the subject of |Folkloric Customs & Manners|.
                        People from a large region come to you for advise.
                        You know obscure details about folk feasts.  You can
                        settle disputes in accordance to the common peoples own
                        customs.))
  (5 (outstanding) (there are only a handful of people in your league when it
                          comes to |Folkloric Customs & Manners|.)))

(defskill harvesting
          strength
          (underclass-village 
            lowerclass-f-village lowerclass-m-village
            lower-middle-class-f-village lower-middle-class-m-village
            middle-class-f-village middle-class-m-village)
          (you can bring in the harvest when the time ripe.)
          (0 (none) (you lack any skills in harvesting.))
          (1 (poor) (you can harvest but it takes you more time than the average
                         villager.  you may destroy some produce in your attempt
                         to harvest.))
          (2 (average) (you are as good at harvesting as most villagers.  when 
                            seeing |crops,| you can tell whether they are ready 
                            to be harvested.))
          (3 (good) (you are better at harvesting than most people.  You are 
                         faster than most.  You can predict the best moment to
                         harvest the |crops,| taking into accout the weather and
                         the |crops'| growth.  You can predict the |harvest's|
                         yield.))
          (4 (exceptional) (when you |harvest,| the yield is more than a good 
                             harvester would have expected it to be.  You can
                             predict the best moment for harvesting and the 
                             yield without having to see the crops using only 
                             an average |harvester's| description of the fields
                             an the weather the fields experienced.  People from
                             a large region would love to have your help with 
                             their harvest.))
          (5 (outstanding) (there are only a few people in your league when it
                                  comes to harvesting.  Your speed borders on
                                  the supernatural.  Even crops that others 
                                  would consider ruined, give a reasonnable 
                                  field in your hands.  You are the subject of
                                  farmer stories in a large region.))
          
          )

                

(defun print-attribute-description (attribute)
  (game-print (cdr (assoc attribute attribute-descriptions))))


(defun print-dots (n)
  "Print a sequence of '⚫' that indicates a character's strength in
   an attribute or skill.  Each dot allows for rolling a single dice."
  (loop for i from 1 to n
        do (princ "⚫"))
  (princ #\Space))


(defun print-attribute-level-description (attribute level)
  "Print the attribute level (in dots '⚫') and its quallitative 
   description. `Level` ranges from 1 to 5."
  (format t "~a: ~a (~a)"
          (with-output-to-string (*standard-output*)
            (game-print (list attribute) t))
          (with-output-to-string (*standard-output*)
            (game-print (cadr (assoc level (cdr (assoc attribute attribute-level-descriptions)))) t))
          (with-output-to-string (*standard-output*) 
            (print-dots level))))


(defun attribute-dots (attribute)
  "Return the symbol that corresponds to the dots field of `attribute`."
  (find-symbol (format nil "~A/DOTS" attribute) 'merlin))


(defun get-attribute-dots (entity attribute)
  "Return the number of dots (i.e. level and available dice) `entity` 
   has in the given attribute."
  (apply (attribute-dots attribute) (list entity)))


(defun print-entity-attribute (entity attribute)
  "Print a human readable description of the level of `attribute` that
   `entity` has."
  (print-attribute-level-description 
    attribute
    (get-attribute-dots entity attribute)))


(defun  print-entity-all-attributes (entity)
  "Print a human readable description of the level of evry attribute
   that `entity` has."
  (mapcar #'(lambda (x)
              (if (member x (cl-ecs::entity-components entity))
                (progn 
                  (print-entity-attribute entity x)
                  (fresh-line))))
          (mapcar #'car attribute-descriptions)))


(defun attribute-dice-pool (entity attribute)
  "Create a dice pool for `attribute` of `entity`."
  (dice-pool (get-attribute-dots entity attribute)))


(defun add-random-attribute (entity attribute power-level)
  "Set `attribute` of `entity` to a random value appropriate for a
   character of the given `power-level`.  `power-level` can be `:mundane`,
   for normal commoners, `:heroic` for powerfull knights and mages, 
   and `:godlike` for entities way more powerfull than normal people 
   and normal heroes." 
  (cl-ecs:add-component entity attribute nil)
  (funcall (fdefinition `(setf ,(attribute-dots attribute)))
           (dice (case power-level
                   (:mundane 3)
                   (:heroic 5)
                   (:godlike 7)))
           entity))


(defun add-all-random-attributes (entity power-level)
  "Set all attributes of `entity`, that were not previously set, to
   random values appropriate for of characters the given `power-level`.
   `power-level` can be `:mundane`, for normal commoners, `:heroic` 
   for powerfull knights and mages, and `:godlike` for entities way 
   more powerfull than normal people and normal heroes.
   "
  (mapcar #'(lambda (x)
              (unless (member x (cl-ecs::entity-components entity))
                (add-random-attribute entity x power-level)))
          (mapcar #'car attribute-descriptions)))

