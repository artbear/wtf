(defpackage :wtf-test
  (:use :cl :rt :wtf))

(in-package :wtf-test)

(defun read-all-tokens (in-string)
  (loop for x in  (wtf::string->list in-string)
     collect (list (wtf::token-type x)
                   (wtf::replace-all
                    (wtf::replace-all  (wtf::token-text x) (string #\NewLine) "#n" )
                    (string #\Return) "#r"))))



(deftest lexer/1 
  (read-all-tokens
   "// строка комментария
    function (f)
      var m;
      doStuff(\"nonsense\", 45.0, f(m));
    endfunction
    ")
  ((:COMMENT-LINE "// строка комментария#n    fun") (:ID "ction")
               (:WS " ") (:OPEN "(") (:ID "f") (:CLOSE ")") (:WS "#n      ")
               (:VAR "var") (:WS " ") (:ID "m") (:SEMI ";") (:WS "#n      ")
   (:ID "doStuff") (:OPEN "(") (:STRING "\"nonsense\"")))


