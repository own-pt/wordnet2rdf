
;; The code of this file is made to be runned in AG version 4.10 (at the time it was written, the AG 4.10 
;; used is the one running on Amazon AWS server.

;; Deduplication of entities:
;; - SenseIndex 
;; - SenseIndex and WordSense
;; - Word
;;
;; Checking the merging of SenseIndex entities. 
;;   cut -d " " -f 1 sentidx.vrb > lixo.1
;;   cut -d " " -f 1 index.sense > lixo.2
;;   cat lixo.1 lixo.2 | sort | uniq | wc -l

(in-package :db.agraph.user)

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
      (dolist (a (remove-if (lambda (a) (part= (nth 1 a) !owl:sameAs)) new-triples))
	(add-triple (nth 0 a) (nth 1 a) (nth 2 a)))
      (delete-triples :s old)
      (delete-triples :o old))))


(defun group-nodes (key value &optional (counter 0))
  (declare (ignore key))
  (if (> (length value) 1)
      (progn 
	(format *debug-io* "Merging group ~a ~a~%" counter value)
	(let ((master (car value)))
	  (dolist (other (cdr value))
	    (progn 
	      (merge-nodes (car other) (car master))))))))


(defun deduplicate-words ()
  (let ((wt (make-hash-table :test #'equal))
	(words (select0-distinct (?w ?l)
		 (q- ?w !wn30:lexicalForm ?l)))
	(counter 0))
    (dolist (w words)
      (let ((str (upi->value (cadr w))))
	(if (gethash str wt)
	    (push w (gethash str wt))
	    (setf (gethash str wt) (list w)))))
    (format *debug-io* "Finished hashtable~%")
    (maphash (lambda (k v) (group-nodes k v (incf counter)))  wt)))


(defun deduplicate-senseindex ()
  (let ((wt (make-hash-table :test #'equal))
	(words (select0-distinct (?w ?l)
		 (q- ?w !wn30:senseKey ?l)))
	(counter 0))
    (dolist (w words)
      (let ((str (upi->value (cadr w))))
	(if (gethash str wt)
	    (push w (gethash str wt))
	    (setf (gethash str wt) (list w)))))
    (format *debug-io* "Finished hash-table ~a ~%" wt)
    (maphash (lambda (k v) (group-nodes k v (incf counter))) wt)))


; WordNet-3.0 has 29 SenseIndex nodes with the same lemma+lexid of
; other 2 WordSenses. For 28 cases, it doesn't matter because both
; WordSense are indistinguishabe. In one case (Utopia0) we have to
; mannualy remove one sameAs triple before deduplicate the nodes. I
; choose the one related with wn30i:wordsense-03020193-a-2 which is a
; sense related to the synset 07283198.

(defun identify-senseindex/wordsense ()
  (select0/callback (?si ?ws) 
      (lambda (p) 
	(add-triple (nth 0 p) !owl:sameAs (nth 1 p)))
    (q- ?ss !wn30:containsSenseIndex ?si)
    (q- ?ss !wn30:containsWordSense ?ws)
    (q- ?ws !wn30:word ?w)
    (q- ?ws !wn30:lexicalId ?i1)
    (q- ?w  !wn30:lemma ?l1)
    (q- ?si !wn30:lexId ?i2)
    (q- ?si !wn30:lemma ?l2)
    (lispp (equal (concatenate 'string (part->value ?l1) (part->value ?i1))
		  (concatenate 'string (part->value ?l2) (part->value ?i2))))))


(defun deduplicate-sameAs (c)
  (do* ((tripla (get-triple :p !owl:sameAs)
		(get-triple :p !owl:sameAs))
	(counter 0 
		 (1+ counter)))
       ((or (null tripla)
	    (> counter c)))
    (progn 
      (merge-nodes (subject tripla) (object tripla))
      (format *debug-io* "~a ~a ~%" (subject tripla) (object tripla)))))


(defun clean-senseindex ()
  (delete-triples :o !wn30:SenseIndex)
  (delete-triples :p !wn30:lexId)
  (delete-triples :p !wn30:containsSenseIndex))

;; After the previous function, the following query was executed in
;; the web interface:
;;
;; delete {
;;   ?ws wn30:lemma ?val .
;;   }	where {
;;   ?ws wn30:lemma ?val .
;;   ?ws a wn30:WordSense .
;; }


;; Finally, we must fix Wordnet-BR mapping the adjective synsets
;; that in WordNet 3.0 are actually adjectives satellite. I did it in
;; two steps. First run the query below:
;;
;; construct { ?a owl:sameAs ?a } 
;; where {
;;   ?a a wn30:AdjectiveSatelliteSynset .
;; }
;; 
;; Some manual edition are necessary in the generated ttl file. This
;; file will have owl:sameAs triples mapping all satellite
;; synsets to the coresponding adjective synset.
;;
;; Old code:
;;
;; (defun correct-synsets-br ()
;;   (select0/callback (?ss1 ?id) 
;;       (lambda (p)
;; 	(let ((addr (format nil "synset-~a-s" (part->value (second p)))))
;; 	  (add-triple (first p) !owl:sameAs (resource addr "wn30br"))))
;;     (q- ?ss1 !wn30:synsetId ?id)
;;     (q- ?ss1 !rdf:type !wn30:AdjectiveSynset)
;;     (q- ?ss2 !wn30:synsetId ?id)
;;     (q- ?ss2 !rdf:type !wn30:AdjectiveSatelliteSynset)))

