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

(defun load-all ()
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
