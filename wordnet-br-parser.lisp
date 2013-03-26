;; Referencias:
;; - http://code.google.com/p/cl-en/source/browse/trunk/basics.lisp#148
;; - http://www.ibm.com/developerworks/xml/tutorials/x-usax/
;; - http://common-lisp.net/project/cxml/
;; - http://common-lisp.net/project/cxml/saxoverview/index.html

(in-package :wordnet2rdf)

(defparameter *xml-fields* '(("BC" . bc) 
			     ("WN-3.0-Synset" . id)
			     ("PT-Words-Man"  . words-man)
			     ("PT-Word-Cand"  . words-sug)
			     ("PT-Gloss"      . gloss-man)
			     ("PT-Gloss-Sug"  . gloss-sug)
			     ("EN-Gloss"      . gloss-en)
			     ("EN-Words"      . words-en)
			     ("SPA-Words-Sug" . words-sp)
			     ("Comments"      . comments)))

(defclass synset-br ()
  ((id :initform nil)
   (bc :initform nil)
   (words-man :initform nil)
   (words-sug :initform nil)
   (gloss-man :initform nil)
   (gloss-sug :initform nil)
   (gloss-en  :initform nil)
   (words-en  :initform nil)
   (words-sp  :initform nil)
   (comments  :initform nil)))


(defun synset-br2en (ss-br)
  (let* ((id (slot-value ss-br 'id))
	 (id-pos (subseq id 0 1))
	 (id-offset (format nil "~8,'0d" (parse-integer (subseq id 1))))
	 (words-slot (if (not (slot-value ss-br 'words-man))
			 'words-sug 
			 'words-man))
	 (words nil))
    (dolist (w (cl-ppcre:split "\\s*(,|;)\\s*" (string-trim '(#\Space #\Tab) (slot-value ss-br words-slot))))
      (push (list w nil nil) words))
    (make-instance 'synset 
		   :id id-offset
		   :ss-type id-pos
		   :words words
		   :gloss (if (not (slot-value ss-br 'gloss-man)) 
			      (slot-value ss-br 'gloss-sug) 
			      (slot-value ss-br 'gloss-man))
		   :notes (if (slot-value ss-br 'comments)
			      (slot-value ss-br 'comments)))))


(defclass sax-handler (sax:default-handler)
  ((current-ss     :initform nil :reader current-wn)
   (current-field  :initform nil :reader current-field)
   (synsets        :initform nil :reader synsets) 
   (stack          :initform nil :reader collected-text)))


(defmethod sax:start-element ((h sax-handler) (namespace t) (local-name t) (qname t) (attributes t))
  (with-slots (current-ss current-field stack) h
    (cond 
      ((equal local-name "row") 
       (setf current-ss (make-instance 'synset-br)))
      ((assoc local-name *xml-fields* :test 'equal) 
       (setf current-field (cdr (assoc local-name *xml-fields* :test 'equal)))
       (setf stack nil)))))


(defmethod sax:end-element ((h sax-handler) (namespace t) (local-name t) (qname t))
  (with-slots (current-ss current-field stack synsets) h
    (cond 
      ((equal local-name "row") 
       (push current-ss synsets))
      ((assoc local-name *xml-fields* :test 'equal) 
       (if (> (length stack) 0)
	   (setf (slot-value current-ss current-field) (format nil "~{~A~}" (reverse stack))))
       (setf stack nil)))))


(defmethod sax:characters ((h sax-handler) (data t))
  (with-slots (stack) h
    (push data stack)))




