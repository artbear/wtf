(in-package :wtf)


(defparameter *rule-table* (make-hash-table :test 'equal))
(defparameter *rule-name-table* (make-hash-table :test 'equal))
(defparameter *rule-result* (make-hash-table :test 'equal))


(defun init-hash-tables ()
  (setf *rule-table* (make-hash-table :test 'equal))
  (setf *rule-name-table* (make-hash-table :test 'equal))
  (setf *rule-result* (make-hash-table :test 'equal)))


(defstruct rule 
  (id (id) :type number :read-only t)
  (name nil :type symbol :read-only t)
  (token-id nil  :read-only t)
  (description-rule "" :type string :read-only t)
  (fn nil :read-only t)
  (result nil))

(defmethod add-result (rule token)
  (setf (rule-result rule) (append (list token) (rule-result rule))))
  


(defmacro defrule (&rest body)
  (let ((name (getf body :name))
        (token (getf body :token))
        (des (getf body :description))
        (fn (getf body :check)))
    `(add-rule ,name  ,token ,des ,fn)))



(defun add-rule (name token-id description  fn)
  (let ((istructure (make-rule :name name :token-id token-id :description-rule description :fn fn)))
    (setf (gethash name *rule-name-table*) istructure)
    (if (listp token-id)
        (dolist (id token-id)
          (setf (gethash id *rule-table*) (append (list istructure) (gethash id *rule-table*))))
        (setf (gethash token-id *rule-table*) (append (list istructure) (gethash token-id *rule-table*))))))




(defun check-rule (token)
  (loop for rule in (gethash (token-type token) *rule-table*)
     when (not (apply (rule-fn rule) (list token)))
     do (add-result (gethash (rule-name rule) *rule-name-table*) token)
     collect (list rule token)))

  

(defun flatten (structure)
  (cond ((null structure) nil)
        ((atom structure) (list structure))
        (t (mapcan #'flatten structure))))

(defun local-run-rule (input-list)
  (loop for token in input-list
     when (and (gethash (token-type token) *rule-table*) (not (check-rule token)))
     collect token))

       
(defstruct rule-result
  (filename "" :type string :read-only t)
  (data nil :read-only t))

(defun run-rule (filename )
  (local-run-rule (file->list filename))
  (loop for key being the hash-keys of *rule-name-table* 
                           using (hash-value value)
                           collect  value))

(defun run-rules (pathname file-entry)
  (setf (wtf::rule-lexer-result file-entry) (run-rule pathname)))



(defmethod format-object ((what (eql 'xml)) (value rule-result))
  (with-open-file (stream (make-pathname :directory (pathname-directory config.wtf:*report-dir*)
                                         :name (pathname-name (rule-result-filename value))
                                         :type "xml")
                          :direction :output :if-exists :overwrite :if-does-not-exist :create)
   (format stream "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<ruleresult>
    <filename>~a</filename>
    <rules>~{~%~a~%~}</rules>
</ruleresult>"
          (rule-result-filename value)
          (mapcar #'(lambda (rule) (format-object what rule)) (rule-result-data value)))
   stream))



(defmethod local-token  (value)
  (format nil "<index>~a</index><line>~a</line><char>~a</char>" (codepoint-index value) (codepoint-line-number value) (codepoint-char value)))

(defmethod local-format  (value )
    (format nil "<token id=\"~a\" type =\"~a\" value=\"~a\" >
<start>~a</start>
<end>~a</end>
</token>"
            (token-id value) (token-type value)
             (token-text value)
            (local-token (token-start-point value))
            (local-token (token-end-point value))))


(defmethod format-object ((what (eql 'xml)) (value rule))
  (format nil "<rule>
<name>~a</name>
<description>~a </description>
<result>~{~a~%~}</result>
</rule>"
          (rule-name value)
          (rule-description-rule value)
          (mapcar #'(lambda(token) (local-format token)) (rule-result value))))

(defun split-by-dot (string)
    (loop for i = 0 then (1+ j)
          as j = (position #\. string :start i)
          collect (subseq string i j)
          while j))


(defun load-one-rule (pathname)
  (cl-log:log-message :wtf  (format nil "Загрузка правил из файла ~a" pathname))
  (load pathname))




(defun load-rule (rule-directory)
  (init-hash-tables)
  (cl-fad:walk-directory rule-directory
                         #'load-one-rule
                         ))


