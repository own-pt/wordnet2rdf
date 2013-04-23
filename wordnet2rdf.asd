;;;; lattes-to-bibtex.asd

;; For info why the dependencies file is necessary, read
;; http://weitz.de/packages.html

(asdf:defsystem #:wordnet2rdf
  :serial t
  :depends-on (#:cl-ppcre #:cxml)
  :components ((:file "dependencies") 
	       (:file "package" :depends-on ("dependencies"))
	       (:file "ag-init" :depends-on ("package"))
	       (:file "common"  :depends-on ("package"))
	       (:file "translations"      :depends-on ("common"))
	       (:file "wordnet-db-parser" :depends-on ("common"))
	       (:file "wordnet-br-parser" :depends-on ("common"))
	       (:file "ag-loader"         :depends-on ("common"))
	       (:file "sentiwordnet"      :depends-on ("ag-loader"))
	       (:file "process"           :depends-on ("ag-loader"))
	       (:file "deduplication"     :depends-on ("ag-loader"))))

;; (:file "convert")
;; (:file "prolog-loader")
;; (:file "prolog-parser")
;; (:file "ttl-writer")
