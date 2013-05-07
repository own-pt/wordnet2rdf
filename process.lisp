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
(defparameter *src*              #P"/Users/arademaker/work/IBM/scolapp/")
(defparameter *wordnet-br-dir*   #P"/Users/arademaker/work/WordNet-BR/uwn-*.xml")
(defparameter *wordnet-dict-dir* (merge-pathnames #P"wordnet/WordNet-3.0/dict/" *src*))
(defparameter *core-file*        (merge-pathnames #P"wordnet/core/wn30-core-synsets.tab" *src*))
(defparameter *sentiwordnet*     (merge-pathnames #P"SentiWordNet/SentiWordNet_3.0.0_20130122.txt" *src*))


(defparameter *sents* (parse-file  (merge-pathnames #P"wordnet/WordNet-3.0/dict/sents.vrb" *src*) 
				   #'parser-sents) 
  "parsing and loading the verb example sentences")

(defparameter *lexnames* (parse-file (merge-pathnames #P"wordnet/WordNet-3.0/dict/lexnames" *src*) 
				     #'parser-lexnames)  
  "parsing and loading the lexname file")


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

