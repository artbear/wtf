(in-package :wtf)

(defparameter *rule-table* (make-hash-table :test 'equal)
  "Список загруженных пользовательских правил")



(defgeneric apply-rules (rule-type value object)
  (:documentation "Метод для применени правил"))

(defgeneric check-rule (rule-type rule value object)
  (:documentation "Проверяет переданное правило на значиении value, и записывает результат"))

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


(defmethod check-rule ((rule-type (eql 'codelexer)) rule (value token) object)
  (unless (apply (rule-fn rule) (list (token-text value) (token-type value) (token-start-point value) (token-end-point value) object))
    (add-rule-result rule value object)))


(defmethod apply-rules ((rule-type (eql 'codelexer)) (value token) object)
  "Метод просто анализирует токен"
  (let ((rules-hash (gethash rule-type *rule-table*)))
    (let ((used-rules (gethash (token-type value) rules-hash)))
      (loop for rule in used-rules
           do (check-rule rule-type rule value object)))))


(defmethod apply-rules ((rule-type (eql 'codelexer)) (value list) object)
  "Применим вызов правила ко всему списку"
  (loop for item in value
     do (apply-rules rule-type item object)))
