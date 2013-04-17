;; (C) 2013 IBM Corporation
;;
;;  Author: Alexandre Rademaker
;; Project: Wordnet-BR
;;
;; Description: Given the general ideas of a mapping of WordNet to RDF
;; (http://www.w3.org/TR/wordnet-rdf/) this code creates an Allegro
;; Graph triplestore for the parsed database files from WordNet 3.0.

(in-package :wordnet2rdf)

(defparameter *data-files* (list "data.noun" "data.verb" "data.adj" "data.adv"))
(defparameter *wordnet-dict-dir* #P"/Users/arademaker/Temp/wordnet/WordNet-3.0/dict/")
(defparameter *wordnet-br-dir*   #P"/Users/arademaker/work/WordNet-BR/uwn-*.xml")
(defparameter *core-file* #P"/Users/arademaker/Temp/wordnet/core/wn30-core-synsets.tab")
(defparameter *sentiwordnet* #P"/Users/arademaker/work/IBM/scolapp/SentiWordNet_3.0.0/SentiWordNet_3.0.0_20130122.txt")

(register-freetext-predicate !wn30:lemma)
(register-freetext-predicate !wn30:lexicalForm)


(defun load-en ()
  (progn 
    (delete-triples )
    (dolist (f *data-files*)
      (mapcar #'add-synset (parse-file (merge-pathnames *wordnet-dict-dir* f) #'parse-data-line)))
    (mapcar #'add-senseidx (parse-file (merge-pathnames *wordnet-dict-dir* #P"index.sense") #'parser-senseidx))
    (mapcar #'add-sentidx (parse-file (merge-pathnames *wordnet-dict-dir* #P"sentidx.vrb") #'parser-sentidx))
    (mapcar #'add-core (parse-file *core-file* #'parser-core))))

(defun load-br () 
  (dolist (file (directory *wordnet-br-dir*))
    (let ((my (make-instance 'sax-handler)))
      (cxml:parse file my)
      (mapcar (lambda (ss) (add-synset (synset-br2en ss) :ns "wn30br")) 
	      (slot-value my 'synsets)))))

(defun load-sentiwordnet ()
  (mapcar #'add-sentiwordnet (parse-sentiwordnet *sentiwordnet*)))

(defun load-all ()
  (load-en)
  (load-br)
  (load-sentiwordnet))


;;; deduplication of entities:
;;
;; - SenseIndex 
;; - SenseIndex and WordSense
;; - Word

;; Checking the merging of SenseIndex entities:
;;   cut -d " " -f 1 sentidx.vrb > lixo.1
;;   cut -d " " -f 1 index.sense > lixo.2
;;   cat lixo.1 lixo.2 | sort | uniq | wc -l


(defun merge-nodes (old new)
  "Transfer all in and out edges from OLD to NEW, except owl:sameAs edges."
  (let ((new-triples nil))
    (progn 
      (mapcar #'(lambda (tr)
		  (if (not (get-triple :s new :p (predicate tr) :o (object tr)))
		      (push (list new (predicate tr) (object tr)) new-triples)))
	      (get-triples-list :s old :limit nil))
      (mapcar #'(lambda (tr)
		  (if (not (get-triple :s (subject tr) :p (predicate tr) :o new))
		      (push (list (subject tr) (predicate tr) new) new-triples)))
	      (get-triples-list :o old :limit nil))
      (dolist (a new-triples)
	(add-triple (nth 0 a) (nth 1 a) (nth 2 a)))
      (delete-triples :s old)
      (delete-triples :o old))))


(defun deduplicate-words ()
  (let ((wt (make-hash-table :test #'equal))
	(words (select0-distinct (?w ?l)
		 (q- ?w !wn30:lexicalForm ?l))))
    (dolist (w words)
      (let ((str (upi->value (cadr w))))
	(if (gethash str wt)
	    (push w (gethash str wt))
	    (setf (gethash str wt) (list w)))))
    (format *debug-io* "Finished hashtable~%")
    (maphash #'group-nodes wt)))

(defun deduplicate-senseindex ()
  (let ((wt (make-hash-table :test #'equal))
	(words (select0-distinct (?w ?l)
		 (q- ?w !wn30:senseKey ?l))))
    (dolist (w words)
      (let ((str (upi->value (cadr w))))
	(if (gethash str wt)
	    (push w (gethash str wt))
	    (setf (gethash str wt) (list w)))))
    (format *debug-io* "Finished hash-table ~a ~%" wt)
    (maphash #'group-nodes wt)))

(defparameter *index-count* 0)
(defparameter *counter* 0)

(defun my-index ()
  (if (> *index-count* 100)
      (progn 
	(format *debug-io* "Indexing all~%")
	(index-all-triples)
	(setf *index-count* 0))
      (progn 
	(format *debug-io* "Indexing new~%")
	(index-new-triples)
	(setf *index-count* (1+ *index-count*)))))


(defun group-nodes (key value)
  (declare (ignore key))
  (if (> (length value) 1)
      (progn 
	(format *debug-io* "Merging group ~a ~a~%" (incf *counter*) value)
	(let ((master (car value)))
	  (dolist (other (cdr value))
	    (progn 
	      ; (format *debug-io* "Transfer ~a to ~a~%" (car other) (car master))
	      (merge-nodes (car other) (car master))))))))


(defun group-nodes-1 (key value)
  (declare (ignore key))
  (if (> (length value) 1)
      (progn 
	(format *debug-io* "Merging group ~a~%" value)
	(let ((master (car value)))
	  (dolist (other (cdr value))
	    (progn 
	      (format *debug-io* "Transfer ~a to ~a~%" (car other) (car master))
	      (merge-nodes (car other) (car master))))
	  (my-index)))))


(defun print-hash-entry (key value)
  (format t "key ~S is ~S~%" key value))
