
(in-package :wordnet2rdf)

(defclass synset ()
  ((id          :initarg :id 
		:reader synset-id)
   (lex-filenum :initarg :lex-filenum 
		:reader synset-lnum)
   (ss-type     :initarg :ss-type 
		:reader synset-type)  
   (words       :initarg :words 
		:initform nil
		:reader synset-words) 
   (pointers    :initarg :pointers 
		:initform nil
		:reader synset-pointers)
   (gloss       :initarg :gloss
		:reader synset-gloss)
   (frames      :initarg :frames 
		:initform nil
		:reader synset-frames)))


(defclass senseidx ()
  ((key         :initarg :key
		:reader senseidx-key)
   (lemma       :initarg :lemma
		:reader senseidx-lema)
   (ss-type     :initarg :ss-type
		:reader senseidx-ss-type)
   (lexfile     :initarg :lexfile
		:reader senseidx-lexfile)
   (synset      :initarg :synset
		:reader senseidx-synset)
   (number      :initarg :number
		:reader senseidx-number)
   (tagcnt      :initarg :tagcnt
		:reader senseidx-tagcnt)))
