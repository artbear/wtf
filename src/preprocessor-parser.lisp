(in-package :wtf)

(defparameter *tst-paramter* (file->list "/home/wwalll/src/wtf/test/test-file-preprocessor.txt"))


(defparameter *preprocessor-token* '(:preprocessor-if :preprocessor-endif :preprocessor-else  :preprocessor-elsid :then :client :server :externalconection))

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
                        (setf *inside-preprocessor* (not (eql :preprocessor-endif ttype)))
                        (values ttype rtoken)))
                  (values :any rtoken)))))))




(defparser preprocessor
    ((programm startState) $1)

  ((startState datalist) $1)
  ((datalist datalist data) (append $1 $2))
  ((datalist data) $1)
  
  ((data :any) (list $1))
  ((data preprocessor-command) (list $1))
  
  ((preprocessor-command :preprocessor-if expression :then startState :preprocessor-endif)
   (list $1))

  ((preprocessor-command :preprocessor-if expression :then startState CaseBlock :preprocessor-endif)
   (list $1))

  ((preprocessor-command :preprocessor-if expression :then startState DefaultClause :preprocessor-endif)
   (list $1))


  ((CaseBlock CaseClauses) (list $1))
  ((CaseBlock CaseClauses DefaultClause) (append $1 (list $2)))
 
  ((CaseClauses CaseClause) (list $1))
  ((CaseClause  CaseClauses CaseClause) (append $1 (list $2)))
 
  ((CaseClause :preprocessor-elsif expression :then programm) (list $1))
  
  ((DefaultClause :preprocessor-else programm)  (list $1))

  ((expression :not expression) $1)
  ((expression and-expression) $1)

  ((and-expression and-expression :and or-expression) $1)
  ((and-expression or-expression) $1)

  ((or-expression or-expression :and const-expression) $1)
  ((or-expression const-expression) $1)

  ((const-expression :open expression :close) $1)
  ((const-expression :client) $1)
  ((const-expression :sever) $1)
  ((const-expression :externalconection) $1)
  
)


(yacc:parse-with-lexer (lexer-fn  (remove-unused *tst-paramter*)) preprocessor)
