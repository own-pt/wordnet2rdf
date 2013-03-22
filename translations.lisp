
(in-package :wordnet2rdf)

(defparameter *type-table* `(("n" 1 !wn30:NounSynset)
			     ("v" 2 !wn30:VerbSynset)
			     ("a" 3 !wn30:AdjectiveSynset)
			     ("s" 5 !wn30:AdjectiveSatelliteSynset)
			     ("r" 4 !wn30:AdverbSynset)))

(defparameter *type-table-inv* (mapcar (lambda (x) (list (nth 1 x) (nth 0 x) (nth 2 x))) 
				       *type-table*))


(defparameter *ptrs-table* `(("@"  !wn30:hypernymOf) 
			     ("~"  !wn30:hyponymOf)
			     ("@i" !wn30:instanceOf) 
			     ("~i" !wn30:hasInstance)
			     ("%m" !wn30:memberMeronymOf) 
			     ("#m" !wn30:memberHolonymOf)
			     ("&"  !wn30:similarTo)
			     ("*"  !wn30:entails)
			     ("%s" !wn30:substanceMeronymOf) 
			     ("#s" !wn30:substanceHolonymOf)
			     ("%p" !wn30:partMeronymOf) 
			     ("#p" !wn30:partHolonymOf)
			     (";c" !wn30:classifiedByTopic)
			     (";r" !wn30:classifiedByRegion)
			     (";u" !wn30:classifiedByUsage)
			     ("-c" !wn30:classifiesByTopic)
			     ("-r" !wn30:classifiesByRegion)
			     ("-u" !wn30:classifiesByUsage)
			     ("+"  !wn30:derivationallyRelated)
			     (">"  !wn30:causes)
			     ("$"  !wn30:sameVerbGroupAs)
			     ("="  ((("na" "ns" "an" "sn") !wn30:attribute)))
			     ("!"  !wn30:antonymOf)
			     ("^"  !wn30:seeAlso)
			     ("<"  !wn30:participleOf)
			     ("\\" ((("an" "aa" "as" "sn" "sa" "ss") !wn30:adjectivePertainsTo) 
				    (("ra" "rs") !wn30:adverbPertainsTo)))))



;; remember that frames are ordered from 1-35

(defparameter *frames* '("Something ----s"
			 "Somebody ----s"
			 "It is ----ing"
			 "Something is ----ing PP"
			 "Something ----s something Adjective/Noun"
			 "Something ----s Adjective/Noun"
			 "Somebody ----s Adjective"
			 "Somebody ----s something"
			 "Somebody ----s somebody"
			 "Something ----s somebody"
			 "Something ----s something"
			 "Something ----s to somebody"
			 "Somebody ----s on something"
			 "Somebody ----s somebody something"
			 "Somebody ----s something to somebody"
			 "Somebody ----s something from somebody"
			 "Somebody ----s somebody with something"
			 "Somebody ----s somebody of something"
			 "Somebody ----s something on somebody"
			 "Somebody ----s somebody PP"
			 "Somebody ----s something PP"
			 "Somebody ----s PP"
			 "Somebody's (body part) ----s"
			 "Somebody ----s somebody to INFINITIVE"
			 "Somebody ----s somebody INFINITIVE"
			 "Somebody ----s that CLAUSE"
			 "Somebody ----s to somebody"
			 "Somebody ----s to INFINITIVE"
			 "Somebody ----s whether INFINITIVE"
			 "Somebody ----s somebody into V-ing something"
			 "Somebody ----s something with something"
			 "Somebody ----s INFINITIVE"
			 "Somebody ----s VERB-ing"
			 "It ----s that CLAUSE"
			 "Something ----s INFINITIVE"))

