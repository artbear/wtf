(in-package :wtf)

(defparameter *rule-table* (make-hash-table :test 'equal)
  "Список загруженных пользовательских правил")

(defparameter *current-file* nil
  "Обрабатываемый файл")


(defmacro defrule (&rest body)
  (let ((name (getf body :name))
        (token (getf body :token))
        (des (getf body :description))
        (fn (getf body :check)))
    `(add-rule ,name  ,token ,des ,fn)))


(defun add-rule (name token-id description  fn)
  (let ((istructure (make-rule :name name :token-id token-id :description-rule description :fn fn)))
;;    (setf (gethash name *rule-name-table*) istructure)
    (if (listp token-id)
        (dolist (id token-id)
          (setf (gethash id *rule-table*) (append (list istructure) (gethash id *rule-table*))))
        (setf (gethash token-id *rule-table*) (append (list istructure) (gethash token-id *rule-table*))))))



(defun init-hash-tables ()
  (setf *rule-table* (make-hash-table :test 'equal)))

(defmethod add-result (rule token)
  (let ((results (gethash rule (slot-value *current-file* 'rule-lexer-result))))
    (setf (gethash rule (slot-value *current-file* 'rule-lexer-result))
          (append
           (list token)
           (if (listp results)
               results
               (list results))))))
  

(defun check-rule (token)
  (loop for rule in (gethash (token-type token) *rule-table*)
     when (not (apply (rule-fn rule) (list token)))
     do (add-result rule token)))


(defun apply-all-rules (file)
  (setf *current-file* file)
  (let ((token-list (slot-value file 'token-list)))
    (dolist (token token-list) 
      (if  (gethash (token-type token) *rule-table*)
           (check-rule token)))))



(defun load-rule (rule-directory)
  (init-hash-tables)
  (cl-fad:walk-directory rule-directory
                         #'(lambda (pathname) (load pathname))))


