(in-package :wtf)


(defparameter *current-model* :client)


(defparameter *preprocessor-token* '(:preprocessor-if :preprocessor-endif :preprocessor-else  :preprocessor-elsif :then :client :server :externalconection :or :and :not :open :close) )

(defparameter *inside-preprocessor* nil)

(defun remove-unused (token-list)
  (remove-if #'(lambda (token) (equal :ws (token-type token))) token-list))


(defun lexer-fn (token-list)
  ;; get tokens from string using lexer
    (lambda ()
      (let ((rtoken (pop token-list)))
        (if (null rtoken)
            (values nil nil)
            (let ((ttype (token-type rtoken)))
              (if (member ttype *preprocessor-token*)
                  (if (and (eql :then ttype) (not *inside-preprocessor*))
                      (values :any rtoken)
                      (progn
                        (setf *inside-preprocessor* (not (eql :then ttype)))
                        (values ttype rtoken)))
                  (values :any rtoken)))))))


(defun lexer->list (token-list)
  (loop for rtoken in token-list
     collect
        (if (null rtoken)
            (cons nil nil)
            (let ((ttype (token-type rtoken)))
              (if (member ttype *preprocessor-token*)
                  (if (and (eql :then ttype) (not *inside-preprocessor*))
                      (cons :any rtoken)
                      (progn
                        (setf *inside-preprocessor* (not (eql :then ttype)))
                        (cons ttype rtoken)))
                  (cons :any rtoken))))))





(defparser preprocessor
  
  ((programm dataList) $1) ;;; вот тут все

  ((dataList data) (list $1))
  ((dataList dataList data) (append (if (listp $1) $1 (list $1)) (list $2)))

  ((data :any) (list $1))
  ((data preprocessor-command) $1)

  ((preprocessor-command :preprocessor-if expression :then dataList :preprocessor-endif) (if $2 $4 nil))
  ((preprocessor-command :preprocessor-if expression :then dataList CaseBlock :preprocessor-endif) (if $2 $4 $5))
  ((preprocessor-command :preprocessor-if expression :then dataLiet DefaultClause :preprocessor-endif) (if $2 $4 $5))

  ((CaseBlock CaseClauses) $1)
  ((CaseBlock CaseClauses DefaultClause) (if $1 $1 $2))
 
  ((CaseClauses  CaseClause) $1)
  ((CaseClauses CaseClauses  CaseClause)  (if $1 $1 $2))
   
  ((CaseClause :preprocessor-elsif expression :then programm) (if $2 $4 nil))

  ((DefaultClause :preprocessor-else programm) $2)

  ((expression :not expression) (not $2))
  ((expression and-expression) $1)

  ((and-expression and-expression :and or-expression)  (and $1 $3))
  ((and-expression or-expression) $1)

  ((or-expression or-expression :or const-expression) (or $1 $3))
  ((or-expression const-expression) $1)

  ((const-expression :open expression :close) $2)
  ((const-expression :client)  (equal (token-type x) *current-model*))
  ((const-expression :server)  (equal (token-type x) *current-model*))
  ((const-expression :externalconection)  (equal (token-type x) *current-model*)))


(defun parse-preprocessor (data)
  (yacc:parse-with-lexer (lexer-fn (remove-unused 
                          (if (typep data 'string)
                              (string->list data)
                              data)))
                          preprocessor))
  


;;(parse-preprocessor "d1 d2 d3 #if ( client and server )then d7 #elsif client then dd1 #else d8 #endif d4")



