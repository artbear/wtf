(ql:quickload "cl-ppcre")
(ql:quickload "cl-log")
(ql:quickload "rt")

(load "wtf.asd")
(asdf:oos 'asdf:load-op :wtf-asdf)




(defparameter *filename* "data/CommonModule.qMeta.Module.txt")
(defparameter *tst* (lexer.wtf:file->list *filename*))
(defparameter *where* (build-dir *report-dir* (split-by-dot (pathname-name *filename*))))

(lexer-rule.wtf:run-rule 'check-keyword *tst* *where*)
