;; (C) 2013 IBM Corporation
;; Author: Alexandre Rademaker

(in-package :wordnet2rdf)

(enable-!-reader)
(enable-print-decoded t)

(register-namespace "wn30" "http://research.ibm.com/brl/wn30br/instances/" :errorp nil)
(register-namespace "wn20" "http://www.w3.org/2006/03/wn/wn20/schema/" :errorp nil)

(create-triple-store "wn30")


