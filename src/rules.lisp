(in-package :wtf)

(defparameter *rule-table* (make-hash-table :test 'equal)
  "Список загруженных пользовательских правил")



(defgeneric apply-rules (rule-type  object)
  (:documentation "Метод для применени правил"))

(defgeneric add-rule-result (rule value object)
  (:documentation "Запись результата проверки в объект"))


(defmacro defrule (&rest body)
  (let ((name (getf body :name))
        (token (getf body :token))
        (des (getf body :description))
        (scope (getf body :where))
        (fn (getf body :check)))
    `(add-rule ,scope ,name  ,token ,des ,fn)))


(defun add-rule (scope name token-id description  fn)
  (let ((istructure (make-rule :scope scope :name name :token-id token-id :description-rule description :fn fn))
        (hash (gethash scope *rule-table*))
        )
    (unless hash
      (setf (gethash scope *rule-table*) (make-hash-table :test 'equal))
      (setf hash (gethash scope *rule-table*)))
;;    (setf (gethash name *rule-name-table*) istructure)
    (if (listp token-id)
        (dolist (id token-id)
          (setf (gethash id hash) (append (list istructure) (gethash id hash))))
        (setf (gethash token-id hash) (append (list istructure) (gethash token-id hash))))))



(defun init-rule-tables ()
  (setf *rule-table* (make-hash-table :test 'equal)))


(defmethod add-rule-result (rule value  (object file-entry))
  (let ((resulthash (gethash (rule-scope rule) (rule-result object) 'nil)))
    (if (null resulthash) 
        (progn
          (setf (gethash (rule-scope rule) (rule-result object)) (make-hash-table :test 'equal))
          (setf resulthash (gethash (rule-scope rule) (rule-result object)))))
    (let ((resultlist (gethash rule resulthash)))
      (setf  (gethash rule resulthash)
             (append (list value) resultlist)))))




(defmethod apply-rules ((rule-type (eql 'token))  object)
  "Применим вызов правила ко всему списку"
  (loop for value in (slot-value object 'token-list)
     do 
       (let ((rules-hash (gethash rule-type *rule-table*)))
         (let ((used-rules (gethash (token-type value) rules-hash)))
           (loop for rule in used-rules
              do (if (apply (rule-fn rule) (list (token-text value) (token-type value) (token-start-point value) (token-end-point value) object))
                     (add-rule-result rule value object)))))))




(defmacro do-vector (var-seq &body body)
  (destructuring-bind (var user-index seq) var-seq
    (let ((vector (gensym "VECTOR"))
	  (index (gensym "INDEX")))
      `(let ((,vector ,seq))
	(dotimes (,index (length ,vector))
	  (let ((,var (aref ,vector ,index))
                (,user-index  ,index))
	    ,@body))))))

(defmethod apply-rules ((rule-type (eql 'lexer))  object)
  "Применим вызов правила лексера"
  (let ((vector (apply #'vector (slot-value object 'token-list)))
        (rules-hash (gethash rule-type *rule-table*)))
    (do-vector (value index vector)
      (let ((used-rules (gethash (token-type value) rules-hash)))
        (loop for rule in used-rules
           do (if (apply (rule-fn rule) (list (token-text value) (token-type value) (token-start-point value) (token-end-point value) object index vector))
                  (add-rule-result rule value object)))))))


(defun find-in-vector (index data skip  inc-value)
  (if (or (= 0 index) (= index (length data)))
      nil
      (let ((value (aref data index)))
        (if (equal (token-type value) skip)
            (find-in-vector (+ inc-value index) data skip inc-value)
            value))))

  
