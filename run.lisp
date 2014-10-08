(ql:quickload "cl-ppcre")
(ql:quickload "cl-log")
(ql:quickload "yacc")
(ql:quickload "rt")
(ql:quickload "dso-lex")
(ql:quickload "dso-util")
;(load "lib/dso-parse/dso-parse.asd")
;(asdf:oos 'asdf:compile-op :dso-parse)
;(asdf:oos 'asdf:load-op :dso-parse)

(load "wtf.asd")

(asdf:oos 'asdf:compile-op :wtf-asdf)
(asdf:oos 'asdf:load-op :wtf-asdf)


(load "wtf-test.asd")
(asdf:oos 'asdf:compile-op :wtf-test-asdf)
(asdf:oos 'asdf:load-op :wtf-test-asdf)

(in-package :wtf)
(run-all)
;;(defparameter *file* (make-instance 'file-entry :filename
;"/home/wwalll/src/wtf/data/test.txt"))

