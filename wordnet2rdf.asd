;; (C) 2013 IBM Corporation
;;
;;  Author: Alexandre Rademaker
;; Project: Wordnet-BR

;; For info why the dependencies file is necessary, read
;; http://weitz.de/packages.html

(asdf:defsystem #:wordnet2rdf
  :serial t
  :depends-on (#:agclient #:cl-ppcre #:cxml)
  :components ((:file "package")
	       (:file "ag-init" :depends-on ("package"))
	       (:file "common"  :depends-on ("package"))
	       (:file "wordnet-db-parser" :depends-on ("common"))
	       (:file "wordnet-br-parser" :depends-on ("common"))
	       (:file "ag-loader"         :depends-on ("common"))
	       (:file "sentiwordnet"      :depends-on ("ag-loader"))
	       (:file "wikidictionary"    :depends-on ("ag-loader"))
	       (:file "process"           :depends-on ("wikidictionary"))
	       (:file "deduplication"     :depends-on ("ag-loader"))))

