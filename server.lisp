;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; vim: set filetype=lisp:

;; Listen to a socket and run the Game when someone connects
(in-package :merlin)

(defun merlin-main (stream)
  (declare (type stream stream))
  (let ((*standard-output* stream)
        (*standard-input* stream))
    (print-title)
    (print-introduction)
    (force-output *standard-output*)
    (eval-player-input nil)))

(defun print-title ()
  (format t " =.=.=.=.=.=.=.= ~%...M.E.R.L.I.N...~% =.=.=.=.=.=.=.= ~%~%"))


(defun print-introduction ()
  (game-print '(|Welcome,| in this game you play the role of the legendary wizard
                           |"Myrrdin Emrys"| |(also| known as |"Merlin Ambrosius")|.
                           |It's| up to you to decide whether you try to live up 
                           to the legends or take a path to a different destiny.)))

(defun start-server ()
 (usocket:socket-server usocket:*wildcard-host* 7113 #'merlin-main nil :reuse-address t))
