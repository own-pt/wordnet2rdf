
(in-package :wordnet2rdf)

(defun parse-sentiwordnet (filename)
  (with-open-file (fin filename)
    (do* ((line (read-line fin nil)
		(read-line fin nil))
	  (parser? (string/= line "#" :end1 1)
		   (string/= line "#" :end1 1))
	  (res nil))
	 ((null line)
	  (reverse res))
      (if parser? 
	  (let ((data (subseq (cl-ppcre:split "\\t" line) 0 4)))
	    (push data res))))))


(defun add-sentiwordnet (senti)
  (let ((ss-uri (make-synset-uri (nth 1 senti) (nth 0 senti))))
    (add-triple ss-uri !swn30:posScore (literal (nth 2 senti)))
    (add-triple ss-uri !swn30:negScore (literal (nth 3 senti)))))

