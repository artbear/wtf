(ql:quickload "cl-ppcre")
(ql:quickload "cl-log")
(ql:quickload "yacc")
(ql:quickload "rt")

(load "wtf.asd")
(asdf:oos 'asdf:compile-op :wtf-asdf)
(asdf:oos 'asdf:load-op :wtf-asdf)


;(load "wtf-test.asd")
;(asdf:oos 'asdf:compile-op :wtf-test-asdf)
;(asdf:oos 'asdf:load-op :wtf-test-asdf)

(in-package :wtf)
(run-all)
