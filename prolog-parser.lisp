;; (C) 2013 IBM Corporation
;; Author: Alexandre Rademaker

(defun prolog-parse-line (line arity operator)
  (let ((re (format nil "^~a\\(([0-9]+),\'(.*)\'\\)\\.$" operator))) 
    (multiple-value-bind (a g)
	(cl-ppcre:scan-to-strings re line)
      (declare (ignore a))
      (if (not (equal (length g) arity))
	  (error line)
	  (princ (format nil "(~a ~{~s~^ ~})~%" operator (coerce g 'list)))))))

(defun prolog-parse-file (filename operator arity &optional (limit nil))
  (with-open-file (f filename)
    (do ((line (read-line f nil)
               (read-line f nil))
	 (counter 0 (+ 1 counter)))
        ((or (null line)
	     (and limit (> counter limit))))
      (prolog-parse-line line arity operator))))

;; testing

; (test-file "/Users/arademaker/Temp/wordnet/prolog/wn_g.pl" "g" 2 5)

