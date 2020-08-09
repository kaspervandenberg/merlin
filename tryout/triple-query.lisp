;;;; Triple-query

;;;; Query operators that work on lists of triples.

;;;; © Kasper van den Berg, 2018


(defun gen-triple-eq-predicate (&key
				  ((:subject subj) nil subj-supplied-p)
				  ((:predicate pred) nil pred-supplied-p)
				  ((:object obj) nil obj-supplied-p))
  "Generate a predicate function that compares the target triple to the given triple.

Only the specified arguments are used in the comparison when a part of the triple; i.e.:
`(gen-triple-eq-predicate :subject 'example-org-subject) finds triples that have
'example-org-subject as subject, they can have any predicate and any object."
  (lambda (s p o)
    (and (or (not subj-supplied-p) (eq subj s))
	 (or (not pred-supplied-p) (eq pred p))
	 (or (not obj-supplied-p) (eq obj o)))))


(defun select-triple-if (f triples)
  "Return the list of triples in `triples` for which the predicate `#'f` yields true."
  (remove-if-not (lambda (x) (apply f x)) triples))

(defun select-triple (triples
		      &rest triple-comparison
		      &key
			((:subject subj))
			((:predicate pred))
			((:object obj)))
  "Return the list of triples that match the given triple-comparison."
  (declare (ignore subj pred obj))
  (select-triple-if (apply #'gen-triple-eq-predicate triple-comparison) triples))


(defun member-triple-if (f triples)
  "Return true if there exist a triple in `triples` for which the predicate `#'f` yields true."
  (member-if (lambda (x) (apply f x)) triples))

(defun member-triple (triples
		      &rest triple-comparison
		      &key
			((:subject subj))
			((:predicate pred))
			((:object obj)))
  "Return true if there exist a triple in `triples` that matches the given triple-comparison."
  (declare (ignore subj pred obj))
  (member-triple-if (apply #'gen-triple-eq-predicate triple-comparison) triples))

(defun chain-triples (source-triples
		      target-triples
		      &key
			from
			to)
  "Follow links use the `:from` parts in `source-triples` to match as the `:to` part in `target-triples.

Both `from` and `to` can be one of `:subject`, `:predicate`, and `:object`.  Example:
`(chain-triples source-triples target-triples :from :object :to :subject)` yields the list of triples
from `target-triples` whose subject occurs as object in `source-triples`.
"
  (let ((f-chain (cond
		   ((eq to :subject) (lambda (s p o)
				       (declare (ignore p o)
						(sb-ext:muffle-conditions sb-ext:compiler-note))
				       (member-triple source-triples from s)))
		   ((eq to :predicate) (lambda (s p o)
					 (declare (ignore s o)
						  (sb-ext:muffle-conditions sb-ext:compiler-note))
					 (member-triple source-triples from p)))
		   ((eq to :object) (lambda (s p o)
				      (declare (ignore s p)
					       (sb-ext:muffle-conditions sb-ext:compiler-note))
				      (member-triple source-triples from o))))))
    (select-triple-if f-chain target-triples)))
