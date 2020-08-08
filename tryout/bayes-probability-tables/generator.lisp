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

(defun calc-probability (weight-table values)
  (reduce
   #'+
   (remove-if
    #'(lambda (x) (not (member x values)))
    (cadr weight-table)
    :key #'cadr)
   :key #'cadr))

(defun simplify (weight-table)
  (list
   (get-random-variable weight-table)
   (mapcar
    #'(lambda (x)
	(list
	 (calc-probability weight-table (list x))
	 x))
    (get-values weight-table))))

(defun sum-weights (weight-table)
  (reduce
   #'(lambda (acc x) (+ acc x))
   (mapcar
    #'car
    (cadr weight-table))))

(defun calc-other (weight-table)
  (- 1 (sum-weights weight-table)))
