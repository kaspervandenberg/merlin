; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
; vim: set filetype=lisp:

;;;; Innate abilities –such as strength ans intelligence– that a character may 
;;;; possess.

(in-package :net.kaspervandenberg.merlin.character-attributes)


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
