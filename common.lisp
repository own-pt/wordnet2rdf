
(in-package :wordnet2rdf)

(defclass synset ()
  ((id          :initarg :id 
		:initform nil
		:accessor synset-id)
   (lex-filenum :initarg :lex-filenum 
		:initform nil
		:accessor synset-lnum)
   (ss-type     :initarg :ss-type 
		:accessor synset-type)  
   (words       :initarg :words 
		:initform nil
		:accessor synset-words) 
   (pointers    :initarg :pointers 
		:initform nil
		:accessor synset-pointers)
   (gloss       :initarg :gloss
		:accessor synset-gloss)
   (frames      :initarg :frames 
		:initform nil
		:accessor synset-frames)
   (notes      :initarg :notes 
		:initform nil
		:accessor synset-notes)))


