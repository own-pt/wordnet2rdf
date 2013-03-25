
(in-package :wordnet2rdf)


(defun collect (data start size total)
  "Auxiliar function for collect subsequences of a sequence."
  (do ((pos start (+ pos size))
       (res nil)
       (count 1 (+ 1 count)))
      ((> count total)
       (reverse res))
    (push (subseq data pos (+ pos size)) res)))


(defun parse-word (word &optional (adj nil))
  (if (not (null adj))
      (multiple-value-bind (m g)
	  (scan-to-strings "(.*)\\((a|p|ip)\\)" (nth 0 word))
	(if (null m)
	    (list (nth 0 word) 
		  (parse-integer (nth 1 word) :radix 16) nil)
	    (list (aref g 0)
		  (parse-integer (nth 1 word) :radix 16) (aref g 1))))
      (list (nth 0 word) 
	    (parse-integer (nth 1 word) :radix 16) nil)))

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
   and returns a synset instance."
  (let* ((gloss-sep (position #\| line))
	 (data (cl-ppcre:split " " (subseq line 0 gloss-sep)))
	 (gloss (subseq line (+ 1 gloss-sep)))
	 (w-cnt (parse-integer (nth 3 data) :radix 16))
	 (p-cnt-pos (+ 4 (* 2 w-cnt)))
	 (p-cnt (parse-integer (nth p-cnt-pos data)))
	 (fields (+ 5 (* 2 w-cnt) (* 4 p-cnt)))
	 (f-cnt (if (> (length data) fields)
		    (parse-integer (nth fields data)) 0))
	 (words (if (search (nth 2 data) "as")  
		    (mapcar (lambda (w) (parse-word w t)) (collect data 4 2 w-cnt))
		    (mapcar #'parse-word (collect data 4 2 w-cnt)))))
    (make-instance 'synset 
		   :id (nth 0 data)
		   :lex-filenum (parse-integer (nth 1 data))
		   :ss-type (nth 2 data)
		   :gloss (string-trim '(#\Space) gloss)
		   :words words
		   :pointers (mapcar #'parse-pointer (collect data (+ p-cnt-pos 1) 4 p-cnt))
		   :frames (mapcar #'parse-frame (collect data (+ 1 fields) 3 f-cnt)))))


(defun parse-file (filename parser &optional (limit nil))
  "It reads a file {index,data}.{noun,verb,adj,adv} wordnet database."
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
	  (let ((data (funcall parser line)))
	    (if data (push data res)))))))


;; parser for each kind of file

(defun parser-senseidx (line)
  (let* ((data (cl-ppcre:split " " line))
	 (key (nth 0 data))
	 (keyparts (cl-ppcre:split "%" key))
	 (lemma (car keyparts))
	 (keyrest (cl-ppcre:split ":" (cadr keyparts))))
    (list :key key 
	  :lemma lemma 
	  :ss-type    (parse-integer (nth 0 keyrest)) 
	  :lexfilenum (parse-integer (nth 1 keyrest)) 
	  :lexid      (parse-integer (nth 2 keyrest))
	  :synset       (nth 1 data) 
	  :sense-number (nth 2 data) 
	  :tag-count    (nth 3 data))))


(defun parser-sents (line)
  (multiple-value-bind (s a) 
      (scan-to-strings "([0-9]+)[ ]+(.*)" line)
    (declare (ignore s)) 
    (list (parse-integer (aref a 0)) 
	  (aref a 1))))


(defun parser-sentidx (line)
  (let ((data (cl-ppcre:split " " line)))
    (list :key (nth 0 data)
	  :examples (mapcar #'parse-integer (cl-ppcre:split "," (nth 1 data))))))

(defun parser-lexnames (line)
  (multiple-value-bind (m g) 
      (scan-to-strings "([0-9]+)[ \\t]+([a-zA-Z\\.]*)[ \\t]+([0-9]*)" line)
    (declare (ignore m)) 
    (list (parse-integer (aref g 0)) 
	  (aref g 1)
	  (parse-integer (aref g 2)))))


(defparameter *sents* (parse-file #P"/Users/arademaker/Temp/wordnet/WordNet-3.0/dict/sents.vrb" #'parser-sents) 
  "parsing and loading the verb example sentences")

(defparameter *lexnames* (parse-file #P"/Users/arademaker/Temp/wordnet/WordNet-3.0/dict/lexnames" #'parser-lexnames)  
  "parsing and loading the lexname file")

