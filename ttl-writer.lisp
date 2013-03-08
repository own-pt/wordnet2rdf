;; (C) 2013 IBM Corporation
;; Author: Alexandre Rademaker
;; Project: Wordnet-BR 

(defparameter *BASE-URI-INSTANCE* "http://research.ibm.com/brl/2013/wordnet/instance")
(defparameter *BASE-URI-SCHEMA* "http://research.ibm.com/brl/2013/wordnet/schema")


(defclass part ()
  ())

(defclass resource (part)
  ((base :initarg :base) 
   (uri  :initarg :uri)))

(defclass literal (part)
  ((value :initarg :value)))

(defun literal (value)
  (make-instance 'literal :value value))

(defun resource (base uri)
  (make-instance 'resource :base base :uri uri))

(defgeneric part->string (part)
  (:documentation "convert part to string"))

(defmethod part->string ((part resource))
  (format nil "~a:~a" 
	  (slot-value part 'base) 
	  (slot-value part 'uri)))

(defmethod part->string ((part literal))
  (format nil "~s" (slot-value part 'value)))


(defun synset-uri (id)
  (resource "wn30" (format nil "synset-~a" id)))


(defun ttl-line (s p o)
  (format *STANDARD-OUTPUT* "~a ~a ~a . ~&" (part->string s) (part->string p) (part->string o)))


(defun synset-class (type) 
  (let ((trans `(("n" "wn20:NounSynset")
		 ("v" "wn20:VerbSynset")
		 ("a" "wn20:AdjectiveSynset")
		 ("s" "wn20:AdjectiveSatelliteSynset")
		 ("r" "wn20:AdverbSynset"))))
    (resource "wn20" (cadr (assoc type trans :test 'equal)))))


(defun synset-to-rdf (synset)
  "It generates an ttl lines for the synset."
  (let ((uri (synset-uri (slot-value synset 'id))))
    (ttl-line uri (resource "rdf" "type") 
	      (synset-class (slot-value synset 'ss-type)))
    (ttl-line uri (resource "wn20" "gloss") (literal (slot-value synset 'gloss)))))

