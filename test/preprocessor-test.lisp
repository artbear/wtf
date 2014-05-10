(in-package :wtf-test)


(defparameter *tst-paramter* (wtf::file->list (make-instance 'wtf::file-entry :filename  "test/test-file-preprocessor.txt")))

(defparameter *tt* (wtf::parse-preprocessor *tst-paramter*))


(defun tokens->list (in-list)
  (loop for x in in-list
     collect (list (wtf::token-type x) (wtf::token-text x))))

(deftest preprocessor/client
    (tokens->list (wtf::compile-preprocessor *tt* :client))
  ((:IDENTIFIER "start") (:OPEN "(") (:CLOSE ")") (:SEMICOLON ";")
               (:IDENTIFIER "end") (:OPEN "(") (:CLOSE ")") (:SEMICOLON ";"))
    )

(deftest preprocessor/server
    (tokens->list (wtf::compile-preprocessor *tt* :server))
  ((:IDENTIFIER "start") (:OPEN "(") (:CLOSE ")") (:SEMICOLON ";")
               (:IDENTIFIER "дляСервера") (:OPEN "(") (:CLOSE ")")
               (:SEMICOLON ";") (:IDENTIFIER "end") (:OPEN "(") (:CLOSE ")")
   (:SEMICOLON ";")))

(deftest preprocessor/externalconection
    (tokens->list (wtf::compile-preprocessor *tt* :externalconection))
  ((:IDENTIFIER "start") (:OPEN "(") (:CLOSE ")") (:SEMICOLON ";")
               (:IDENTIFIER "end") (:OPEN "(") (:CLOSE ")") (:SEMICOLON ";"))
    )
