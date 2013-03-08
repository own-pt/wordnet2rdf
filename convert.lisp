
; wn30bri http://emap.fgv.br/2012/wn/wn30-br/instances/
; wn30brs http://emap.fgv.br/2012/wn/wn30-br/schema/

; (require :agraph "/Applications/agraph-4.9-client-acl8.2/agraph4.fasl")
(require :agraph "/mnt/agraph-client-90/agraph4.fasl")

(in-package :db.agraph.user)

(enable-!-reader)
(enable-print-decoded t)

(register-namespace "wn30" "http://research.ibm.com/brl/wn30br/instances/" :errorp nil)
(register-namespace "wn20" "http://www.w3.org/2006/03/wn/wn20/schema/" :errorp nil)

(open-triple-store "wn30" :user "super" :password "super22" 
		   ; :triple-store-class 'remote-triple-store 
		   ; :server "logics.emap.fgv.br"
		   )


(load (compile-file "functions.lisp"))

;; start the conversion of the files

(load "src/wn_s.lisp")
(load "src/wn_ant.lisp")
(load "src/wn_at.lisp")

(commit-triple-store)


;; (load (compile-file "functions.lisp"))
;; ;; start the conversion of the files
;; (load "src/wn_s.lisp")
;; (load "src/wn_ant.lisp")
;; (load "src/wn_at.lisp")
;; (commit-triple-store)
