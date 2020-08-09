(defvar weight-table-colors1
  '(color
    ((red 1/6)
     (blue 1/6)
     (black 1/3))))

(defvar weight-table-colors2
  '(color
    ((blue 1/6)
     (yellow 1/6))))

(defvar weight-table-merge-colors
  '(color
    ((red 1/12)
     (blue 1/12)
     (black 1/6)
     (blue 1/12)
     (yellow 1/12)
     (yellow 1/36)
     (red 1/18)
     (black 1/9))))

(defvar conditional-weight-table-material
  '(material
    ((sand
      (((color red) 1/4)
       ((color blue) 1/3)))
     (metal
      (((color red) 1/6)
       ((color black) 1/2))))))

(defun get-random-variable (weight-table)
  (car weight-table))

(defun get-values (weight-table)
  (reverse
   (reduce
    #'(lambda (acc x) (adjoin x acc))
    (mapcar
     #'car
     (cadr weight-table))
    :initial-value nil)))

(defun probability-list (weight-table)
  (cadr weight-table))

(defun calc-probability (weight-table values)
  (reduce
   #'(lambda (acc x) (+ acc (cadr x)))
   (remove-if
    #'(lambda (x) (not (member (car x) values)))
    (probability-list weight-table))
   :initial-value 0))

(defun simplify (weight-table)
  (list
   (get-random-variable weight-table)
   (mapcar
    #'(lambda (x)
	(list
	 x
	 (calc-probability weight-table (list x))))
    (get-values weight-table))))

(defun sum-weights (weight-table)
  (reduce
   #'(lambda (acc x) (+ acc x))
   (mapcar
    #'cadr
    (cadr weight-table))))

(defun calc-probability-other (weight-table)
  (- 1 (sum-weights weight-table)))

(defun distinct-values (weight-table other)
  (let ((weight-table-values (get-values weight-table))
	(other-values (get-values other)))
    (remove-if
     (lambda (x) (member x other-values))
     weight-table-values)))

(defun distinct-value-probabilities (weight-table other)
  (let ((other-probability (calc-probability-other other))
	(values (distinct-values weight-table other)))
    (mapcar
     (lambda (x)
       (list x
	     (* (calc-probability weight-table (list x))
		other-probability)))
     values)))

(defun mul-probability-list (probabilities factor)
  (mapcar
   #'(lambda (x) (list (car x) (* factor (cadr x))))
   probabilities))

(defun mul-weight-table (weight-table factor)
  (list
   (get-random-variable weight-table)
   (mapcar
    #'(lambda (x)
	(list x (* (calc-probability weight-table (list x)) factor)))
    (get-values weight-table))))

(defun merge-weight-tables (weight-tables)
  (let ((factor (/ 1 (length weight-tables)))
	(variable (get-random-variable (car weight-tables))))
    (simplify
     (list
      variable
      (mapcan
       #'(lambda (weight-table)
	   (let ((other-tables (remove weight-table weight-tables)))
	     (append
	      (probability-list (mul-weight-table weight-table factor))
	      (mapcan
	       #'(lambda (other-table)
		   (mul-probability-list (distinct-value-probabilities other-table weight-table) factor))
	       other-tables))))
       weight-tables)))))
