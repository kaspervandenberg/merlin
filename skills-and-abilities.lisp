
(in-package :merlin)

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


(defability strength
            (strength is a |character's| muscle |power,| it determines how much 
                      he can lift and how much physical damage he does in melee 
                      combat.)
            (1 (Weak) (You can barely lift a kettle.  Handling heavier weapons
                           is difficult for you.))
            (2 (Average))
            (3 (Strong) (When there is some heavy lifting to |do,| your neighbours
                              appreciate your help.  Perhaps you work as 
                              blacksmith.  You can do much damage with heavy 
                              weapons.))
            (4 (Exceptional) (You are known for your strength throughout your 
                                  region.  Some think you are as strong as a
                                  team of horses))
            (5 (Outstanding) (Your strength is legendary.)))

(defability dexterity 
            (skill and grace in physical movement.  skills like |dancing,| 
                   |riding,| |sleight of hand,| and archery require dexterity.  
                   in combat dexterity determines the chance to hit an 
                   opponent and the chance to dodge incoming attacks.) 
            (1 (Clumsy))
            (2 (Average))
            (3 (Dexterous))
            (4 (Graceful))
            (5 (Outstanding)))

(defability health 
            (health defines how many hits you can |take,| how long you can 
                    keep at a strenuous activity like running or |climbing,| 
                    and how often your character is sick.)
            (1 (Frail/sickly))
            (2 (Average))
            (3 (Healthy))
            (4 (Vigorous))
            (5 (Outstanding)))

(defability charisma 
            (charisma is charm and force of personality.  characters with 
                      charisma are |likeable,| |persuasive,| |inspiring,| and 
                      they appear trustworthy.  it allows the character to win 
                      others for his views.)
            (1 (|Obnoxious/annoying|))
            (2 (Inconspicuous))
            (3 (Charming))
            (4 (Charismatic))
            (5 (Outstanding)))

(defability manipulation 
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

(defability resolve 
            (how determined is a character to continue arguing his case.  
                 how |deterimined/focussed| is a character to continue to 
                 persue an intellectually challen investigation resolve 
                 determines both how many social punches a character can take 
                 before giving up and yielding to an opponent and how many 
                 intellectual setbacks/hits a character can take before 
                 giving up or losing his mind.  |#\(resolve| is on the social 
                 and mental scale as hitpoints is on the physical |scale.#\)|)
            (1 (hesitant))
            (2 (average))
            (3 (determined))
            (4 (tenacious))
            (5 (outstanding)))

(defability intelligence
            (a |character's| ability to |reason,| to study and learn academic 
               |subjects,| to concentrate on an |(intellectual)| task at hand.  
               the |character's| level of general knowledge and the ability 
               to remember facts when they are helpful.  Effectively and 
               flexibly using magic requires intelligence.  |#\(intelligence| 
               is on the mental scale as is strength on the physical scale.  
               and intelligence is on the magical scale as dexterity is on 
               the physical |scale.#\)|) 
            (1 (dumb)) 
            (2 (average)) 
            (3 (intelligent)) 
            (4 (genius)) 
            (5 (outstanding))) 

(defability wisdom 
            (the ability to perceive |one's| surroundings.  the ability for 
                 quick thinking and flexibly use |one's| intelligence.)
            (1 (dull-witted))
            (2 (average)) 
            (3 (wise))
            (4 (sagious)) 
            (5 (outstanding)))

(defability magic
            (the power of a |mage's| spells.)
            (1 (poor))
            (2 (average))
            (3 (good))
            (4 (exceptional))
            (5 (outstanding)))

(defability mana 
            (the magical reserves a mage has.  some spells require spending 
                 some mana.  Mana is on the mental scale as health is on the 
                 physical scale.)
            (1 (little))
            (2 (average))
            (3 (potent))
            (4 (exceptional))
            (5 (outstanding)))


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
          (you can bring in the harvest when the time is ripe.)
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
                                  would consider |ruined,| give a reasonnable 
                                  field in your hands.  You are the subject of
                                  farmer stories in a large region.)))


(defun print-dots (n)
  "Print a sequence of '⚫' that indicates a character's strength in
   an attribute or skill.  Each dot allows for rolling a single dice."
  (loop for i from 1 to n
        do (princ "⚫"))
  (princ #\Space))

(defgeneric attribute-level (attr)
  (:documentation
   "Return the `Level-Description` that corresponds with the `dots` of `attr`."))

(defmethod attribute-level ((attr Attribute))
  (let ((dots (attribute-dots attr)))
    (remove-if (lambda (l) (not (eql (slot-value l 'level) dots)))
               (attribute-levels attr))))


(defgeneric get-skill-ability (s)
  (:documentation "Return the `ability` component of the `skills`'s `entity`."))

(defmethod get-skill-ability ((s Skill))
  (car (ecs:all-components-of (entity s) (slot-value s 'Base-Ability))))



(defgeneric attribute-dice-pool (attr)
  (:documentation 
    "Create a dice pool for `attribute` (i.e. an `ability` or `skill` of 
     `entity`."))
            
(defmethod attribute-dice-pool ((ablty Ability))
  (dice-pool (attribute-dots ablty)))

(defmethod attribute-dice-pool ((skl Skill))
  (dice-pool (+ (attribute-dots (get-skill-ability skl))
                (attribute-dots skl))))


(defun entity-add-random-attribute (entity attribute-class power-level &rest initargs)
  "Create a fresh instance of `attribute-type` setting its `dots` to a random
   value appropriate for a character of the given `power-level`.  `power-level`
   can be: `:mundane` for normal commoners, `:heroic` for powerful knights and
   mages, and `:godlike` for entities way more powerful than normal people and 
   regular heroes."
  (let ((d (dice (case power-level
                   (:mundane 3)
                   (:heroic 5)
                   (:godlike 7)))))
    (apply #'ecs:entity-add-fresh-component (append (list entity attribute-class :dots d) initargs))))


(defun entity-add-all-random-abilities (entity power-level)
  "Set all abilities of `entity` –for which the entity does not have yet have
   a component– to a random value of dots appropriate for the given 
   `power-level`.  `power-level` can be: `:mundane` for normal commoners, 
   `:heroic` for powerful knights and mages, and `:godlike` for entities way 
   more powerful than normal people and regular heroes."
  (let* ((all-abilities '(strength dexterity health charisma manipulation resolve
                                  intelligence wisdom magic mana))
         (fresh-abilities (remove-if (lambda (x) (ecs:all-components-of entity x)) all-abilities)))
    (mapc (lambda (x) (entity-add-random-attribute entity x power-level)) fresh-abilities)))

