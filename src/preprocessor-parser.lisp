(in-package :wtf)

(defparameter *tst-paramter* (file->list (make-instance 'file-entry :filename  "/home/wwalll/src/wtf/test/test-file-preprocessor.txt")))


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
                        (setf *inside-preprocessor* (not (eql :then ttype)))
                        (values ttype rtoken)))
                  (values :any rtoken)))))))



(defstruct const
  (data nil :read-only t))

(defstruct binary-op
  (op nil)
  (op1 nil)
  (op2 nil))

(defstruct unary-op
  (op nil)
  (op1 nil))

(defstruct if-op
  (condition nil)
  (true-part nil)
  (default-part nil)
  (switch-part nil))

(defstruct if-part
  (condition nil)
  (true-part nil))

(yacc:define-parser preprocessor
  (:muffle-conflicts t)
  (:start-symbol programm)
  (:terminals (:any :or :and :not :client :server :preprocessor-if :preprocessor-endif :preprocessor-elsif :preprocessor-else :then))

  (:precedence nil)
  
  (programm startState)
  
  (startState datalist)
  
  (datalist
   (datalist data
             #'(lambda (x1 y1) (append (if (listp x1) x1 (list x1)) y1)))
    data )
  
  (data
   (:any #'list)
   (preprocessor-command #'list))
  
  (preprocessor-command
   (:preprocessor-if expression :then startState :preprocessor-endif
                     #'(lambda($1 $2 $3 $4 $5)
                         (declare (ignore $1 $3 $5))
                         (make-if-op :condition $2 :true-part $4)))
   (:preprocessor-if expression :then startState CaseBlock :preprocessor-endif
                     #'(lambda($1 $2 $3 $4 $5 $6)
                         (declare (ignore $1 $3 $6))
                         (multiple-value-bind (sw de) $5
                           (make-if-op :condition $2 :true-part $4 :switch-part sw :default-part de))))
   (:preprocessor-if expression :then startState DefaultClause :preprocessor-endif
                     #'(lambda($1 $2 $3 $4 $5 $6)
                         (declare (ignore $1 $3 $6))
                         (make-if-op :condition $2 :true-part $4 :default-part $5))))

  
  (CaseBlock
   CaseClauses 
   (CaseClauses DefaultClause #'(lambda ($1 $2) (values $1 $2))))
 
  (CaseClauses
   CaseClause
   (CaseClauses  CaseClause #'(lambda(x y) (append (if (listp x) x (list x)) y))))
 
  (CaseClause
   (:preprocessor-elsif expression :then programm
                        #'(lambda ($1 $2 $3 $4)
                            (declare (ignore $1 $3))
                            (make-if-part :condition $2 :true-part $4))))
  
  (DefaultClause
      (:preprocessor-else programm #'(lambda ($1 $2) (declare (ignore $1)) $2)))

  (expression
   (:not expression #'(lambda(x y) (make-unary-op :op x :op1 y)))
   and-expression)

   (and-expression
    (and-expression :and or-expression #'(lambda(x y z) (make-binary-op :op y :op1 x :op2 z)))
    or-expression)

   (or-expression
    (or-expression :or const-expression #'(lambda(x y z) (make-binary-op :op y :op1 x :op2 z)))
    const-expression)

   (const-expression
    (:open expression :close #'(lambda (x y z) (declare (ignore x z)) y))
    (:client #'(lambda (x)   (make-const :data (token-type x))))
    (:server #'(lambda (x)   (make-const :data (token-type x))))
    (:externalconection #'(lambda (x)   (make-const :data (token-type x)))))
  
)






(defun local-eval (data)

  (error "stop"
  ))


(defun compile-one-preprocessor (data context)
  (let ((expr (if-op-condition data)))
    (if (local-eval expr)
        (if-op-true-part data)
        (if (null (if-op-switch-part data))
            (if-op-default-part data)
            (loop for x in (if-op-switch-part data)
               do (if (local-eval (if-part-condition x))
                      (return (if-part-true-part x))))))))


(defun compile-preprocessor (data context)
  (loop for x in data
     collect (if (token-p x)
                 x
                 (compile-one-preprocessor x context))))


(defun group-items (fn list)
  (labels ((rec (list acc group)
             (cond ((null list) (reverse (cons group acc)))
                   ((funcall fn (car list))
                    (rec (cdr list) acc (cons (car list) group)))
                   (t (rec (cdr list)
                           (cons (car list)
                                 (if (consp group)
                                     (cons group acc)
                                     acc))
                           '())))))
    (rec list '() '())))

(defun group-list (list)
  (group-items #'(lambda (x) (token-p  x)) list))

(defparameter *tt* (group-list (yacc:parse-with-lexer (lexer-fn  (remove-unused *tst-paramter*)) preprocessor)))
