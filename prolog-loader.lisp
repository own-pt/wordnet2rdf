

;; synsets

(defvar n "n")
(defvar v "v")
(defvar a "a")
(defvar s "s")
(defvar r "r")

(defun s (synset-id w-num word ss-type sense-num tag-count)
  (let ((ss-uri (synset-uri synset-id))
	(ws-uri (wordsense-uri synset-id w-num)))
    (add-triple ss-uri !rdf:type (get-synset-class ss-type))
    (add-triple ss-uri !wn20:synsetId (literal (format nil "~a" synset-id)))
    (add-triple ss-uri !wn20:tagCount (literal (format nil "~a" tag-count)))
    (add-triple ss-uri !wn20:containsWordSense ws-uri)
    (add-triple ws-uri !rdf:type !wn20:WordSense)
    (add-triple ws-uri !rdfs:label (literal word))
    (add-triple ws-uri !wn20:senseNumber (literal (format nil "~a" sense-num)))
    (with-blank-nodes (w)
      (add-triple w !rdf:type !wn20:Word)
      (add-triple w !wn20:lexicalForm (literal word))
      (add-triple ws-uri !wn20:word w))))


;; relations 

(defun ant (a b c d)
  (add-triple (wordsense-uri a b) !wn20:antonymOf (wordsense-uri c d)))


(defun at (a b)
  (let ((cls (object (get-triple :s (synset-uri a) :p !rdf:type))))
    (if (part= cls !wn20:NounSynset)
	(add-triple (synset-uri a) !wn20:attribute (synset-uri b))
	(add-triple (synset-uri b) !wn20:attribute (synset-uri a)))))

