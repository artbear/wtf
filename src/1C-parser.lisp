(in-package :wtf)

(defun lexer-code (token-list)
  ;; get tokens from string using lexer
    (lambda ()
      (let ((rtoken (pop token-list)))
        (if (null rtoken)
            (values nil nil)
            (values (token-type rtoken) rtoken)))))


(defun element-end (x) ;;заглушка временно
  x)
(defun element-start (x) ;; заглушка
  x)


(defparameter *token-list* (expand-hashtable-to-values *tokens-to-symbols* ))

(yacc:define-parser 1C
;  (:print-derives-epsilon t)
;  (:print-first-terminals t)
;  (:print-states t)
 ; (:print-goto-graph t)
;  (:print-lookaheads t)
  (:muffle-conflicts nil)
  (:start-symbol programm)
  (:terminals  (:UNDEFINED :FALSE :TRUE  :WHILE :VAR :VAL :TRY  :TO :THEN :RETURN :THROW  :PROCEDURE :OR  :NOT :NEW :IN :IF :GOTO :FUNCTION :FOR :EXPORT :EXCEPT :ENDTRY :ENDPROCEDURE :ENDIF :ENDFUNCTION :ENDDO :ELSE :ELSIF :EACH :DO :CONTINUE  :BREAK  :AND :CLOSE :OPEN :RIGHT-BRACKET :LEFT-BRACKET :DOT :MINUS :PLUS :PERCENT :SLASH :ASTERISK :LABEL-DECLARATION :GREATER-THAN :LESS-THAN :GREATER-THAN-EQUALS :LESS-THAN-EQUALS :NOT-EQUALS :EQUALS :COLON :HOOK :COMMA :SEMICOLON :identifier))
;;  (:precedence nil)

  (programm start)
  
  (start
   (module-var-list meths statement-list))

  (module-var-list
   module-vars
   (module-var-list module-vars)
   nil)
  
  (module-vars
   (:var var-line :semicolon))

  (var-line
   one-var
   (var-line :comma one-var))


  (one-var
   :identifier
   (:identifier :export))
   

  (meths
   function-decl
   (meths function-decl)
   nil)



  (literal
   :number
   :string-literal
   :true
   :false
   :null
   :undefined)
  
  (function-decl
   (:function :identifier :left-paren formal-parameter-list :right-paren module-var-line statement-list :endfunction)
   (:procedure :identifier :left-paren formal-parameter-list :right-paren export-decl module-var-line statement-list :endprocedure))
  
  (formal-parameter-list
   (formal-parameter-list :comma parameter)
   parameter)

  (parameter
   :identifier
   (:val :identifier)
   (:identifier :equals literal)
   (:val :identifier :equals literal))


  (statement-list
   statement
   (statement-list :semicolon statement))
  

  

  (statement
   call-expression
   assignment-expression
   if-statement
   iteration-statement
   continue-statement
   break-statement
   return-statement
   raise-statement
   labelled-statement
   try-statement
   IfStatement)



  (try-statement
   (:try statement-list :except statement-list :endtry))
  (raise-statement
   (:throw  expression ) 
   :throw )
  
  (IfStatement
   (:if expression :then statement-list :EndIf ) 
   (:if expression :then statement-list CaseBlock :EndIf ) 
   (:if expression :then statement-list DefaultClause :endif ))
  
  (CaseBlock
   CaseClauses
   (CaseClauses DefaultClause))
 
  (CaseClauses
   CaseClause
   (CaseClauses CaseClause))
 
  (CaseClause
   (:elsif expression :then statement-list))

  (DefaultClause
      (:else statement-list))


  (iteration-statement
   (:while expression :do statement-list :enddo) 
   (:for :each :identifier :in expression  :do statement-list :enddo)
   (:for :identifier :equals expression :to expression :do statement-list :enddo))
    
  (continue-statement :continue )
  (break-statement :break )

  (return-statement
   (:return expression)
   :return)

  (call-expression
   :identifier
   (call-expression arguments)
   (call-expression :dot :identifier)
   (call-expression :left-bracket expression :right-bracket))

  (new-expression
   (:new arguments)
   (:new  :identifier arguments))


  (arguments
   (:left-paren argument-list :right-paren)
   (:left-paren :right-paren))

  (argument-list 
   expression
   (argument-list :comma expression))

  (unary-expression
   call-expression
   new-expression
   (:left-paren expression  :right-paren)
   (:hook :left-paren logical-OR-expression :comma expression :comma expression :right-paren)
   literal
   (:plus unary-expression)
   (:minus unary-expression)
   (:not unary-expression))
  
  (multiplicative-expression
   (multiplicative-expression :asterisk unary-expression)
   (multiplicative-expression :slash unary-expression)
   (multiplicative-expression :percent unary-expression)
   unary-expression)
  
  (additive-expression
      multiplicative-expression
   (additive-expression :plus multiplicative-expression)
   (additive-expression :minus multiplicative-expression))

  (relational-expression
   additive-expression
  (relational-expression :equals additive-expression) 
  (relational-expression :not-equals additive-expression) 
  (relational-expression :less-than additive-expression) 
  (relational-expression :less-than-equals additive-expression) 
  (relational-expression :greater-than additive-expression) 
  (relational-expression :greater-than-equals additive-expression))

  


  (logical-AND-expression
   (logical-AND-expression :and  relational-expression) 
   equality-expression)

  (expression
   logical-AND-expression
   (expression :or logical-AND-expression)
   )

  (assignment-expression
   (call-expression :equals expression)))



(defun parse-code (data )
  (yacc:parse-with-lexer
   (lexer-code (remove-unused data)) 1C))

;    (compile-preprocessor (remove-unused data) context)) 1C))



(defparameter *tst-paramter* (wtf::file->list (make-instance 'wtf::file-entry :filename  "test/test-file-preprocessor.txt")))

(defparameter *tt* (wtf::parse-preprocessor *tst-paramter*))

