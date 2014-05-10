(in-package :wtf)



(defparameter *preprocessor-token* '(:preprocessor-if :preprocessor-endif :preprocessor-else  :preprocessor-elsif :then :client :server :externalconection :or :and :not))

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
  (:terminals (:any :or :and :not :client :server :EXTERNALCONECTION :preprocessor-if :preprocessor-endif :preprocessor-elsif :preprocessor-else :then))

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
   (:not expression #'(lambda(x y) (make-unary-op :op 'not :op1 y)))
   and-expression)

   (and-expression
    (and-expression :and or-expression #'(lambda(x y z) (make-binary-op :op 'and :op1 x :op2 z)))
    or-expression)

   (or-expression
    (or-expression :or const-expression #'(lambda(x y z) (make-binary-op :op 'or :op1 x :op2 z)))
    const-expression)

   (const-expression
    (:open expression :close #'(lambda (x y z) (declare (ignore x z)) y))
    (:client #'(lambda (x)   (make-const :data (token-type x))))
    (:server #'(lambda (x)   (make-const :data (token-type x))))
    (:EXTERNALCONECTION #'(lambda (x)   (make-const :data (token-type x)))))
  
)






(defun local-eval (data context)
  (cond
    ((const-p data) (equal (const-data data) context))
    ((unary-op-p data) (if (null (unary-op-op data))
                           (local-eval (unary-op-op1 data) context)
                           (not (local-eval (unary-op-op1 data) context))))
    ((binary-op-p data) (let ((x (local-eval (binary-op-op1 data) context))
                              (y (local-eval (binary-op-op2 data) context))
                              (op  (binary-op-op data)))
                          (cond
                            ((equal 'and op) (and x y))
                            ((equal 'or op) (or x y))
                            (t (error (format nil "Unknown operator ~a" op))))))
    (t  (error (format nil "Unknown token ~a" data)))))


(defun compile-if (data context)
  (let ((expr (if-op-condition data)))
    (if (local-eval expr context)
        (if-op-true-part data)
        (if (null (if-op-switch-part data))
            (if-op-default-part data)
            (let ((switchdata (if (listp (if-op-switch-part data)) (if-op-switch-part data) (list (if-op-switch-part data)))))
            (loop for x in switchdata
               do (if (local-eval (if-part-condition x) context)
                      (return (if-part-true-part x)))))))))


(defun compile-preprocessor (data context)
  (flatten
   (loop for x in data
     collect (if (token-p x)
                 x
                 (compile-if x context)))))







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

(defun parse-preprocessor (data)
  (yacc:parse-with-lexer (lexer-fn  (remove-unused data)) preprocessor))
  
