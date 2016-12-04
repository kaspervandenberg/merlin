; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
; vim: set filetype=lisp:

;;;; Skills the character learned during his life, either by formal training or
;;;; via experience and practice.

(in-package :net.kaspervandenberg.merlin.character-atrributes)


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
