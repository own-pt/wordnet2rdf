;; (C) 2013 IBM Corporation
;;
;;  Author: Alexandre Rademaker
;; Project: Wordnet-BR

;; for parsing the data provided by Francis Bond.

(in-package :wordnet2rdf)

(defun parse-wikidictionary (filename)
  (with-open-file (fin filename)
    (do* ((line (read-line fin nil)
		(read-line fin nil))
	  (parser? (string/= line "#" :end1 1)
		   (string/= line "#" :end1 1))
	  (dict (make-hash-table :test #'equal)))
	 ((null line)
	  dict)
      (if parser? 
	  (let* ((data (cl-ppcre:split "\\t" line))
		 (ss (nth 0 data))
		 (word (nth 2 data)))
	    (if (gethash ss dict)
		(push word (gethash ss dict))
		(setf (gethash ss dict) (list word))))))))


(defun expand-synset (ss dict)
  (let* ((key (format nil "~a-~a" 
		      (slot-value ss 'id)
		      (slot-value ss 'ss-type)))
	 (new-words (gethash key dict)))
    (dolist (w new-words)
      (pushnew (list w nil nil) (slot-value ss 'words) :key 'car :test 'string=))
    ss))

(defun expand-report (ss1 ss2)
  (format *debug-io* "old: ~s new: ~s ~%" 
	  (length (slot-value ss1 'words))
	  (length (slot-value ss2 'words))))

(defun expand-test () 
  (let ((dict (parse-wikidictionary "/Users/arademaker/work/IBM/scolapp/wordnet/francis/wn-wikt-por.tab")))
    (dolist (file (directory *wordnet-br-dir*))
      (let ((my (make-instance 'sax-handler)))
	(cxml:parse file my)
	(mapcar (lambda (ss) 
		  (expand-report (synset-br2en ss) (expand-synset (synset-br2en ss) dict))) 
		(slot-value my 'synsets))))))
