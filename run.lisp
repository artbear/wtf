(ql:quickload "cl-ppcre")
(ql:quickload "cl-log")
(ql:quickload "rt")

(load "wtf.asd")
(asdf:oos 'asdf:compile-op :wtf-asdf)
(asdf:oos 'asdf:load-op :wtf-asdf)



(defparameter *filename* "data/CommonModule.qMeta.Module.txt")
(defparameter *tst* (lexer.wtf:file->list *filename*))
(defparameter *where* (config.wtf:build-dir config.wtf:*report-dir* (lexer-rule.wtf::split-by-dot (pathname-name *filename*))))

(lexer-rule.wtf:run-rule 'lexer-rule.wtf::check-keyword *tst* *where*)
