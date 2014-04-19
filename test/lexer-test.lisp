(defpackage :lexer-test
  (:use :cl :rt :lexer.wtf))

(in-package :lexer-test)

(defun read-all-tokens (in-string)
  (loop for x in (lexer.wtf:string->list in-string)
     collect (list (lexer.wtf:token-type x)  (lexer.wtf::format-object 'log (lexer.wtf:token-text x)))))


(deftest lexer/1 
  (read-all-tokens
   "// строка комментария
    function (f)
      var m;
      doStuff(\"nonsense\", 45.0, f(m));
    endfunction
    ")
  ((:COMMENT-LITERAL "// строка комментария#n") (:WS "    ")
   (:FUNCTION "function") (:WS " ") (:OPEN "(") (:IDENTIFIER "f")
   (:CLOSE ")") (:WS "#n      ") (:VAR "var") (:WS " ")
   (:IDENTIFIER "m") (:SEMICOLON ";") (:WS "#n      ")
   (:IDENTIFIER "doStuff") (:OPEN "(")
   (:STRING-LITERAL "\"nonsense\"") (:COMMA ",") (:WS " ")
   (:NUMBER "45.0") (:COMMA ",") (:WS " ") (:IDENTIFIER "f")
   (:OPEN "(") (:IDENTIFIER "m") (:CLOSE ")") (:CLOSE ")")
   (:SEMICOLON ";") (:WS "#n    ") (:ENDFUNCTION "endfunction")
   (:WS "#n    ")))



