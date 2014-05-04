(in-package :wtf)

(defparameter *files-table* (make-hash-table :test 'equal))

(defclass file-entry ()
  ((filename :initform "" :initarg :filename :accessor :filename)
   (lexer-list :initform (make-hash-table :test 'equal))
   (rule-lexer-result :initform (make-hash-table :test 'equal) :accessor rule-lexer-result)))


(defun walk-files ()
  (cl-fad:walk-directory config.wtf:*data-dir*
                         (lambda (x) (setf (gethash x *files-table*) (make-instance 'file-entry :filename x)))))


(defun lexer-rules ()
  (load-rule config.wtf:*rule-dir*)
  (loop for key being the hash-keys of *files-table*
     using (hash-value value)
       do (run-rules key value)))





    
