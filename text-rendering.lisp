(in-package :merlin)

(defun tweak-text (lst caps lit)
  "Correctly capitalise text in `lst`, a list of characters. `Caps` 
   changes the next single character to upper case.  `Lit` returns 
   all characters literally.  An end of sentence marker sets `caps`
   for the next character.  A double quote (#\") toggles the `lit`
   flag."
  (when lst
    (let  ((item (car lst)) (rest (cdr lst)))
      (cond 
        ((eq item #\space)                (cons item (tweak-text rest caps lit)))
        ((member item '(#\? #\! #\.))     (cons item (tweak-text rest T lit))) 
        ((eq item #\")                    (tweak-text rest caps (not lit))) 
        (lit                              (cons item (tweak-text rest nil lit))) 
        ((or caps lit)                    (cons (char-upcase item) (tweak-text rest nil lit))) 
        (T                                (cons (char-downcase item) (tweak-text rest nil nil)))))))


(defun game-print (lst &optional (single-line nil))
  "Convert a list of symbols to printable text."
  (princ (coerce (tweak-text (coerce (string-trim "() "
                                                  (princ-to-string (mapcar 'symbol-name lst)))
                                     'list)
                             T
                             nil)
                 'string))
  (if (not single-line)
    (fresh-line)))



