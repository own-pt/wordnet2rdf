;; (C) 2013 IBM Corporation
;;  Author: Alexandre Rademaker
;; Project: Wordnet-BR
;;
;; Description: Given a mapping of WordNet to RDF
;; (http://www.w3.org/TR/wordnet-rdf/) this code creates an Allegro
;; Graph triplestore for the parsed database files from WordNet 3.0.

(in-package :wordnet2rdf)

(defun make-synset-uri (ss-id ss-type)
  (resource (format nil "synset-~a-~a" ss-id ss-type) "wn30"))


(defun synset-uri (ss)
  (make-synset-uri (synset-id ss) (synset-type ss)))


(defun synset-class (ss)
  (caddr (assoc (synset-type ss) *type-table* :test 'equal)))


(defun wordsense-uri (ss-id ss-type ws-num)
  (resource (format nil "wordsense-~a-~a-~a" ss-id ss-type ws-num) "wn30"))


(defun add-wordsense (ss ss-res ws-num ws)
  (let ((ws-uri (wordsense-uri (synset-id ss) (synset-type ss) ws-num))
	(word (literal (nth 0 ws))))
    (add-triple ws-uri !rdf:type !wn20:WordSense)
    (add-triple ws-uri !rdfs:label word)
    ; what is a sense number?
    (add-triple ws-uri !wn20:senseNumber (literal (write-to-string ws-num)))
    (add-triple ss-res !wn20:containsWordSense ws-uri)
    ; add RDF list too
    (with-blank-nodes (w)
      (add-triple w !rdf:type !wn20:Word)
      (add-triple w !wn20:lexicalForm word)
      (add-triple ws-uri !wn20:word w))))


(defun add-wordsenses (ss ss-res wordsenses)
  (dotimes (i (length wordsenses))
    (add-wordsense ss ss-res i (nth i wordsenses))))


(defun add-pointer (ss ss-res p-num p)
  t)


(defun add-pointers (ss ss-res pointers)
  (dotimes (i (length pointers))
    (add-pointer ss ss-res p-num p)))


(defun add-synset (synset)
  (let ((ss-uri (synset-uri synset)))
    (add-triple ss-uri !rdf:type (synset-class synset))
    (add-triple ss-uri !wn20:synsetId (literal (format nil "~a" (synset-id synset))))
    (add-triple ss-uri !wn20:gloss (literal (synset-gloss synset)))
    ; (add-triple ss-uri !wn20:tagCount (literal (format nil "~a" tag-count)))
    (add-wordsenses synset ss-uri (synset-words synset))
    (add-pointers (synset-id synset) ss-uri (synset-pointers synset))))

