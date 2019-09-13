;; (C) 2013 IBM Corporation
;;
;;  Author: Alexandre Rademaker
;; Project: Wordnet-BR
;;
;; Description: Given a mapping of WordNet to RDF
;; (http://www.w3.org/TR/wordnet-rdf/) this code creates an Allegro
;; Graph triplestore for the parsed database files from WordNet 3.0.

(in-package :wordnet2rdf)

(w:add-namespace "wn30en" "https://w3id.org/own-pt/wn30-en/instances/")
(w:add-namespace "wn30"  "https://w3id.org/own-pt/wn30/schema/")

(defun make-synset-uri (ss-id ss-type &key (ns "wn30en"))
  (if (equal ss-type "s")
      (resource (format nil "synset-~a-a" ss-id) ns)
      (resource (format nil "synset-~a-~a" ss-id ss-type) ns)))

(defun synset-uri (ss &key (ns "wn30en"))
  (make-synset-uri (synset-id ss) (synset-type ss) :ns ns))

(defun synset-class (ss)
  (third (assoc (synset-type ss) *type-table* :test 'equal)))

;; (declaim (inline resource add-triple))

(defun resource (path ns)
  (make-instance 'w:node :uri (format nil "~a:~a" ns path) :name-resolved-p nil))

(defun add-triple (subj pred obj)
  (w:add-triple (w:triple subj pred obj)))

(defun wordsense-uri (ss-id ss-type ws-num &key (ns "wn30en"))
  (resource (format nil "wordsense-~a-~a-~a" ss-id ss-type ws-num) ns))

(defun make-blank-node ()
  (w:node nil))

(defun add-wordsense (ss ss-res ws-num ws &key (ns "wn30en"))
  (let ((ws-uri (wordsense-uri (synset-id ss) (synset-type ss) ws-num :ns ns))
	(word (w:literal (nth 0 ws))))
    (add-triple ws-uri !rdf:type !wn30:WordSense)
    (add-triple ws-uri !rdfs:label word)
    (if (nth 1 ws)
	(add-triple ws-uri !wn30:lexicalId (w:literal (write-to-string (nth 1 ws)))))
					; word number is different from sense number
    (add-triple ws-uri !wn30:wordNumber (w:literal (write-to-string ws-num)))
    (if (nth 2 ws)
	(add-triple ws-uri !wn30:syntacticMarker (w:literal (nth 2 ws))))
    (add-triple ss-res !wn30:containsWordSense ws-uri)
					; add RDF list too
    (let ((w (make-blank-node)))
      (add-triple w !rdf:type !wn30:Word)
      (add-triple w !wn30:lemma (w:literal (string-downcase (nth 0 ws))))
      (add-triple w !wn30:lexicalForm word)
      (add-triple ws-uri !wn30:word w))))


(defun get-property (ss-type p)
  (let ((pvalue (cadr (assoc (nth 0 p) *ptrs-table* :test #'string=)))
	(pair-type (concatenate 'string ss-type (nth 2 p))))
    (if (listp pvalue)
	(cadar (remove-if-not (lambda (x) (member pair-type (car x) :test #'string=)) pvalue))
	pvalue)))


(defun add-pointer (ss ss-res p &key (ns "wn30en"))
  (let ((snum (nth 3 p))
	(tnum (nth 4 p))
	(property (get-property (synset-type ss) p)))
    (if property 
	(if (= 0 (+ snum tnum))
	    (add-triple ss-res property (make-synset-uri (nth 1 p) (nth 2 p) :ns ns))
	    (let ((source (wordsense-uri (synset-id ss) (synset-type ss) snum :ns ns))
		  (target (wordsense-uri (nth 1 p) (nth 2 p) tnum :ns ns)))
	      (add-triple source property target)))
	(error "Oops! I don't know this pointer/property."))))


(defun add-frame (ss ss-res frame &key (ns "wn30en"))
  (let ((sentence (w:literal (nth (1- (nth 0 frame)) *frames*)))) 
    (if (= 0 (nth 1 frame))
	(add-triple ss-res !wn30:frame sentence)
	(let ((source (wordsense-uri (synset-id ss) (synset-type ss) (nth 1 frame) :ns ns)))
	  (add-triple source !wn30:frame sentence)))))


(defun add-synset (synset &key (ns "wn30en"))
  (let ((ss-uri (synset-uri synset :ns ns))
	(lexname (if (synset-lnum synset)
		     (cadr (assoc (synset-lnum synset) *lexnames*)))))
    (add-triple ss-uri !rdf:type (synset-class synset))
    (add-triple ss-uri !wn30:synsetId (w:literal (synset-id synset)))
    (if (synset-gloss synset)
	(add-triple ss-uri !wn30:gloss (w:literal (synset-gloss synset))))
    (if (synset-base synset)
	(add-triple ss-uri !rdf:type !wn30:BaseConcept))
    (if lexname 
	(add-triple ss-uri !wn30:lexicographerFile (w:literal lexname)))
    (dolist (f (synset-frames synset))
      (add-frame synset ss-uri f :ns ns))
    (dolist (p (synset-pointers synset))
      (add-pointer synset ss-uri p :ns ns))
    (let ((wordsenses (synset-words synset)))
      (dotimes (i (length wordsenses))
	(add-wordsense synset ss-uri (1+ i) (nth i wordsenses) :ns ns)))))

(defun add-senseidx (senseidx)
  (let ((ss-uri (make-synset-uri (getf senseidx :synset) 
				 (cadr (assoc (getf senseidx :ss-type) *type-table-inv*))))
	(w (make-blank-node))) 	;blank node
    (add-triple w !rdf:type !wn30:SenseIndex)
    (add-triple w !wn30:senseKey    (w:literal (getf senseidx :key)))
    (add-triple w !wn30:lemma       (w:literal (getf senseidx :lemma)))
    (add-triple w !wn30:senseNumber (w:literal (getf senseidx :sense-number)))
    (add-triple w !wn30:lexfile     (w:literal (cadr (assoc (getf senseidx :lexfilenum) *lexnames*))))
    (add-triple w !wn30:lexId       (w:literal (write-to-string (getf senseidx :lexid))))
    (add-triple w !wn30:tagCount    (w:literal (getf senseidx :tag-count)))
    (add-triple ss-uri !wn30:containsSenseIndex w)))


(defun add-sentidx (sentidx)
  (let ((w (make-blank-node))) 	; blank node
    (add-triple w !rdf:type !wn30:SenseIndex)
    (add-triple w !wn30:senseKey (w:literal (getf sentidx :key)))
    (dolist (e (getf sentidx :examples))
      (add-triple w !wn30:example (w:literal (cadr (assoc e *sents*)))))))


(defun add-core (core)
  (let ((ss-uri (make-synset-uri (getf core :offset) 
				 (getf core :type))))
    (add-triple ss-uri !rdf:type !wn30:CoreConcept)))
