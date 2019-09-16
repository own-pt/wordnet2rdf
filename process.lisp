;; (C) 2013 IBM Corporation
;;
;;  Author: Alexandre Rademaker
;; Project: Wordnet-BR
;;
;; Description: Given the general ideas of a mapping of WordNet to RDF
;; (http://www.w3.org/TR/wordnet-rdf/) this code creates an Allegro
;; Graph triplestore for the parsed database files from WordNet 3.0.

(in-package :wordnet2rdf)


(defparameter *sents*
  (parse-file  (merge-pathnames #P"sents.vrb" *src*) 
	       #'parser-sents)
  "parsing and loading the verb example sentences")


(defparameter *lexnames*
  (parse-file (merge-pathnames #P"lexnames" *src*)
	      #'parser-lexnames)
  "parsing and loading the lexname file")


(defun load-en (dict-dir core-file)
  (progn
    (setf w:*db* (make-instance 'w:indexed-db :size 100000))
    (dolist (f '("data.noun" "data.verb" "data.adj" "data.adv"))
      (mapcar #'add-synset (parse-file (merge-pathnames dict-dir f) #'parse-data-line)))
    (mapcar #'add-senseidx (parse-file (merge-pathnames dict-dir #P"index.sense") #'parser-senseidx))
    (mapcar #'add-sentidx (parse-file (merge-pathnames dict-dir #P"sentidx.vrb") #'parser-sentidx))
    (mapcar #'add-core (parse-file core-file #'parser-core))))

(defun to-rdf (out-fp &key (format :ntriples))
  (load-en *src* "~/test")
  (with-open-file (s out-fp :direction :output
			    :if-does-not-exist :create
			    :if-exists :supersede)
    (w::db-dump w:*db* s w:*db* format)))


;; (defun load-br ()
;;   (let ((dict (parse-wikidictionary *wikidictionary*)))
;;     (dolist (file (directory *wordnet-br-dir*))
;;       (let ((my (make-instance 'sax-handler)))
;; 	(cxml:parse file my)
;; 	(mapcar (lambda (ss) (add-synset (expand-synset (synset-br2en ss) dict) :ns "wn30br")) 
;; 		(slot-value my 'synsets))))))


;; (defun load-sentiwordnet (sentiwordnet-file)
;;   (mapcar #'add-sentiwordnet (parse-sentiwordnet sentiwordnet-file)))
