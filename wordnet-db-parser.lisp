
(in-package :wordnet2rdf)


(defun collect (data start size total)
  "Auxiliar function for collect subsequences of a sequence."
  (do ((pos start (+ pos size))
       (res nil)
       (count 1 (+ 1 count)))
      ((> count total)
       (reverse res))
    (push (subseq data pos (+ pos size)) res)))


(defun parse-index-line (line)
  "It reads a line from index.{noun,verb,adv,adj} WordNet database file."
  (let* ((data (cl-ppcre:split " " line)))
    data))

(defun parse-word (word)
  (let ((hexval (nth 1 word))) 
    (setf (nth 1 word) (parse-integer hexval :radix 16))
    word))

(defun parse-pointer (ptr)
  (let ((hexval (nth 3 ptr))) 
    (append (subseq ptr 0 3) 
	    (list (parse-integer (subseq hexval 0 2) :radix 16)
		  (parse-integer (subseq hexval 2 4) :radix 16)))))

(defun parse-frame (frm)
  (list (parse-integer (nth 1 frm))
	(parse-integer (nth 2 frm) :radix 16)))

(defun parse-data-line (line)
  "It reads a line from data.{noun,verb,adv,adj} wordnet database file
   and returns a synsets as a list."
  (let* ((gloss-sep (position #\| line))
	 (data (cl-ppcre:split " " (subseq line 0 gloss-sep)))
	 (gloss (subseq line (+ 1 gloss-sep)))
	 (w-cnt (parse-integer (nth 3 data) :radix 16))
	 (p-cnt-pos (+ 4 (* 2 w-cnt)))
	 (p-cnt (parse-integer (nth p-cnt-pos data)))
	 (fields (+ 5 (* 2 w-cnt) (* 4 p-cnt)))
	 (f-cnt (if (> (length data) fields)
		    (parse-integer (nth fields data)) 0)))
    (make-instance 'synset 
		   :id (nth 0 data)
		   :lex-filenum (nth 1 data)
		   :ss-type (nth 2 data)
		   :gloss (string-trim '(#\Space) gloss)
		   :words (mapcar #'parse-word (collect data 4 2 w-cnt))
		   :pointers (mapcar #'parse-pointer (collect data (+ p-cnt-pos 1) 4 p-cnt))
		   :frames (mapcar #'parse-frame (collect data (+ 1 fields) 3 f-cnt)))))


(defun parse-data-file (filename &optional (limit nil))
  "It reads a file data.{noun,verb,adj,adv} wordnet database."
  (with-open-file (f filename)
    (do* ((line (read-line f nil)
		(read-line f nil))
	  (parser? (string/= line "  " :end1 2)
		   (string/= line "  " :end1 2))
	  (counter 0 (if parser? 
			 (+ 1 counter) 
			 counter))
	  (res nil))
	 ((or (null line)
	      (and limit (> counter limit)))
	  (reverse res))
      (if parser? 
	  (let ((data (parse-data-line line)))
	    (if data (push data res)))))))


(defun parse-sents-file (path)
  (labels ((parse-line (line)
	     (multiple-value-bind (s a) 
		 (scan-to-strings "([0-9]+)[ ]+(.*)" line)
	       (declare (ignore s)) 
	       (list (parse-integer (aref a 0)) 
		     (aref a 1)))))
      (with-open-file (fin path :direction :input)
	(loop for line = (read-line fin nil)
	      while line
	      collect (parse-line line)))))


(defparameter *sents* (parse-sents-file #P"/Users/arademaker/Temp/wordnet/WordNet-3.0/dict/sents.vrb") 
  "parsing and loading the verb example sentences")

