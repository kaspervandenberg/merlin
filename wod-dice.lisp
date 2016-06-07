;;; The dice roll system of World of Darkness, i.e. dice pools of d10 dice.

(in-package :merlin)

(defun dice (n-sides)
  "A dice has a number of sides, `n-sides`, when evaluated `(dice n) returns a random integer in the range [1..n]."
  (+ (random n-sides) 1))

(defun dice-pool (n-dice &optional (n-sides 10))
  "A collection of `n-dice` independant dices each having `n-sides`.  WOD represents player capabilities as dice pools composed of a number of dice for the attribute and a number of dice for the skill."
  (loop for i from 1 upto n-dice collect `(dice ,n-sides)))

(defun roll-dice (dice-pool)
  "Evaluates all dice in `dice-pool` and returns a list with the dice roll for every dice in the pool."
  (loop for die in dice-pool collect (eval die)))

(defun count-successes (difficulty dice-pool &optional (output nil))
  "Roll the dice in `dice-pool`; count the number of successes, i.e. a dice roll greater than or equal to `difficulty`; count the number of failures, i.e. a dice roll of 1; and return successes minus failures. "
  (let* ((rolls (roll-dice dice-pool))
         (n-failures (count-if (lambda (x) (= x 1)) rolls))
         (n-successes (count-if (lambda (x) (>= x difficulty)) rolls)))
    (format output "rolls: ~a; #successes: ~a; #failures: ~a~%" rolls n-successes n-failures)
    (- n-successes n-failures)))

(defparameter *difficulties*
  '((:easy . 3)
    (:routine . 4)
    (:straightforward . 5)
    (:standard . 6)
    (:challenging . 7)
    (:difficult . 8)
    (:extremely-difficult . 9))
  "The qualitative description of the `difficulty` of `count-successes`.")

(defun difficulty (diff-symb)
  "Map a qualitative description of a difficulty (see `*difficulties*`) to a number required to roll for success."
  (cdr (assoc diff-symb *difficulties*)))

(defparameter *n-successes-interpretation*
  '((-1 . :botch)
    (0 . :failure)
    (1 . :marginal)
    (2 . :moderate)
    (3 . :complete)
    (4 . :exceptional)
    (5 . :phenomenal))
  "The qualitative description of a number of successes.")

(defun describe-success (n-successes)
  "Translate a number of successes, from `count-successes`, into a qualitative description."
  (let ((descr (cdr (assoc n-successes *n-successes-interpretation*))))
    (if (not (eq descr nil))
      descr
      (let ((lowest (first *n-successes-interpretation*))
            (highest (last *n-successes-interpretation*)))
        (if (<= n-successes (car lowest))
          (cdr lowest)
          (cdar highest))))))

