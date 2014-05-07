(in-package :wtf)


(defparameter *current-file* nil
  "Обрабатываемый файл")



;(defmethod add-result (rule token)
;  (let ((results (gethash rule (slot-value *current-file* 'rule-lexer-result))))
;    (setf (gethash rule (slot-value *current-file* 'rule-lexer-result))
;          (append
;           (list token)
;           (if (listp results)
;               results
;               (list results))))))
  

;
;(defun check-rule (token)
;  (loop for rule in (gethash (token-type token) *rule-table*)
;     when (not (apply (rule-fn rule) (list token)))
;     do (add-result rule token)))


;(defun apply-all-rules (file)
;  (setf *current-file* file)
;  (let ((token-list (slot-value file 'token-list)))
;    (dolist (token token-list) 
;      (if  (gethash (token-type token) *rule-table*)
;           (check-rule token)))))



(defun load-rule (rule-directory)
  (init-rule-tables)
  (cl-fad:walk-directory rule-directory
                         #'(lambda (pathname) (load pathname))))


