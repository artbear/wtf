(in-package :lexer-rule.wtf)


(defparameter *rule-table* (make-hash-table :test 'equal))


(defstruct rule-structure 
  (id (util.wtf:id) :type number :read-only t)
  (name nil :type symbol :read-only t)
  (token-id nil  :read-only t)
  (description-rule "" :type string :read-only t)
  (fn nil :read-only t))
 


(defun add-rule (name token-id description  fn)
  (let ((istructure (make-rule-structure :name name :token-id token-id :description-rule description :fn fn))
        )
    (setf (gethash name *rule-table*) istructure)))



(defmacro defrule (&rest body)
  (let ((name (getf body :name))
        (token (getf body :token))
        (des (getf body :description))
        (fn (getf body :check)))
    `(add-rule ,name  ,token ,des ,fn)))


(defun check-rule (token rule)
   (cl-log:log-message :lexer-rule.debug (format t "Проверка правила ~a " (rule-structure-name rule)))
  
  (apply (rule-structure-fn rule) (list token)))

(defun flatten (structure)
  (cond ((null structure) nil)
        ((atom structure) (list structure))
        (t (mapcan #'flatten structure))))


;;;; 
(defun run-rule (rule-name input-list dir)
  (let* ((rule (gethash rule-name *rule-table*))
         (tokenslist (flatten  (rule-structure-token-id rule)))
         (result-list (loop for token in input-list 
                         when  (and (member (lexer.wtf::token-type token) tokenslist) (not (check-rule token rule)))
                         collect token)))
    (with-open-file (output-stream (make-pathname :directory dir :name (string (rule-structure-name rule)) :type "xml")
                                                  :direction :output :if-exists :overwrite :if-does-not-exist :create)
      (write-line "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" output-stream)
      (write-line "<Rule>" output-stream)
      (write-line (format nil "<Name>~a</Name>" (rule-structure-name rule)) output-stream)
      (write-line (format nil "<Description>~a</Description>" (rule-structure-description-rule rule)) output-stream)
      (write-line "<Tokens>" output-stream)
      (loop for token in result-list
         do (write-line
             (format nil "~%<token id=\"~a\" type=\"~a\"><text>~a</text>~%<position>~%<start line='~a' column='~a'/>~%<end line='~a' column='~a'/>~%</position>~%</token>"
                     (lexer.wtf::token-id token)
                     (lexer.wtf::token-type token)
                     (lexer.wtf::token-text token)
                     (lexer.wtf::codepoint-line-number (lexer.wtf::token-start-point token))
                     (lexer.wtf::codepoint-char (lexer.wtf::token-start-point token))
                     (lexer.wtf::codepoint-line-number (lexer.wtf::token-end-point token))
                     (lexer.wtf::codepoint-char (lexer.wtf::token-end-point token)))
             output-stream))
      (write-line "</Tokens>" output-stream)
      (write-line "</Rule>" output-stream)
      )))


(defun split-by-dot (string)
    (loop for i = 0 then (1+ j)
          as j = (position #\. string :start i)
          collect (subseq string i j)
          while j))


  ;; (:export add-rule run-rule run-all))

;;; временно 

(defrule 
  :name 'check-keyword
  :token lexer.wtf::*keyword-symbols*
  :description "Ключевое слово не соответствует каночническому написанию"
  :check #'(lambda (token) (let ((token-text (lexer.wtf::token-text token))
                            (correct-word (gethash (lexer.wtf::token-type token) lexer.wtf::*correct-keywords*)))
                        (or
                         (equal token-text (car correct-word))
                         (equal token-text (cadr correct-word))))))


