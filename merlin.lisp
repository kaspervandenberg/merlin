(defun random-elt (choices)
  (elt choices (random (length choices))))

(defun dice (n-Sides)
  (+ (random n-Sides) 1))

(defun dice-pool (n-Dice &optional (n-Sides 10))
  (loop for i from 1 upto n-Dice collect `(dice ,n-Sides)))

(defun roll-dice (dice-pool)
  (loop for die in dice-pool collect (eval die)))

(defun count-successes (difficulty dice-pool &optional (output nil))
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
    (:extremely-difficult . 9)))

(defun difficulty (diff-symb)
  (cdr (assoc diff-symb *difficulties*)))

(defparameter *n-successes-interpretation*
  '((-1 . :botch)
    (0 . :failure)
    (1 . :marginal)
    (2 . :moderate)
    (3 . :complete)
    (4 . :exceptional)
    (5 . :phenomenal)))

(defun describe-success (n-successes)
  (let ((descr (cdr (assoc n-successes *n-successes-interpretation*))))
    (if (not (eq descr nil))
      descr
      (let ((lowest (first *n-successes-interpretation*))
            (highest (last *n-successes-interpretation*)))
        (if (<= n-successes (car lowest))
          (cdr lowest)
          (cdar highest))))))

(defclass Non-Player-Character ()
  (gender
   name
   attitudes
   description-keywords))

(defparameter *genders* '(:male :female))

(defparameter *names* 
  '((:male . 
     (
      Afallach Afallech Accalon Accolon Addanc Aglouale Agravain Agrawain Alain Albion Alexander 
      Alis Amfortas Ancelot Andret Anguish Anir Antor Apollo Arawn Aroundight Artegal Arthgal 
      Arthgallo Arthur Arthwys Artur Artus Augusel Auguselus Avallach Avanc Awarnach
      Bagdemagus Balan Baldulf Balin Ban Beaumains Bedivere Bedver ))
    (:female .
     (
      Acheflour Ade Ana Angharad Angharat Angharawd Anglides Anna Argante Astolat Avalon
      Belakane Blanchefleur Blancheflor Brangaine Brengwain Bronwen
      Chelinda Chelinde Clarine Clarissant))))

(defun create-random-npc ()
  (let ((npc (make-instance 'nonPlayerCharacter)))
    (with-slots (gender name) npc
      (setf gender (random-elt *gender*))
      (setf name (random-elt (cdr (assoc gender *names*)))))
    npc))
