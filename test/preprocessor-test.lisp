(in-package :wtf-test)


(defparameter *tst-paramter* (wtf::file->list  "test/test-file-preprocessor.txt"))

(defparameter *tt* (wtf::parse-preprocessor *tst-paramter*))


(defun tokens->list (in-list)
  (loop for x in in-list
     collect (list (wtf::token-type x) (wtf::token-text x))))

(deftest preprocessor/client
    (tokens->list (wtf::compile-preprocessor *tt* :client))
  ((:VAR "var") (:ID "x") (:EXPORT "export") (:SEMI ";")
   (:FUNCTION "function") (:ID "xx") (:OPEN "(") (:CLOSE ")")
   (:ID "start") (:OPEN "(") (:CLOSE ")") (:SEMI ";") (:ID "end")
   (:OPEN "(") (:CLOSE ")") (:SEMI ";")
   (:ENDFUNCTION "endfunction")))


(deftest preprocessor/server
    (tokens->list (wtf::compile-preprocessor *tt* :server))
  ((:VAR "var") (:ID "x") (:EXPORT "export") (:SEMI ";")
   (:FUNCTION "function") (:ID "xx") (:OPEN "(") (:CLOSE ")")
   (:ID "start") (:OPEN "(") (:CLOSE ")") (:SEMI ";")
   (:ID "дляСервера") (:OPEN "(") (:CLOSE ")") (:SEMI ";")
   (:ID "end") (:OPEN "(") (:CLOSE ")") (:SEMI ";")
   (:ENDFUNCTION "endfunction")))
  


(deftest preprocessor/externalconection
    (tokens->list (wtf::compile-preprocessor *tt* :externalconection))
  ((:VAR "var") (:ID "x") (:EXPORT "export") (:SEMI ";")
   (:FUNCTION "function") (:ID "xx") (:OPEN "(") (:CLOSE ")")
   (:ID "start") (:OPEN "(") (:CLOSE ")") (:SEMI ";") (:ID "end")
   (:OPEN "(") (:CLOSE ")") (:SEMI ";")
   (:ENDFUNCTION "endfunction")))

