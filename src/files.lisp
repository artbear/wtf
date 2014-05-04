(in-package :wtf)

(defparameter *files-table* (make-hash-table :test 'equal))

(defclass 1C-file-entry ()
  ((filename :initform "" :initarg :filename :accessor :filename)
   (token-list :initform nil :accessor :token-list)
   (lexer-list :initform (make-hash-table :test 'equal))
   (rule-lexer-result :initform (make-hash-table :test 'equal) :accessor rule-lexer-result)))



(defmethod initialize-instance :after ((object 1C-FILE-ENTRY) &key)
  (let ((filename (slot-value object 'filename)))
    (setf (slot-value object 'token-list) (file->list filename))))

(defun walk-files ()
  (cl-fad:walk-directory config.wtf:*data-dir*
                         (lambda (x) (setf (gethash x *files-table*) (make-instance '1C-file-entry :filename x)))))




(defun lexer-rules ()
  (load-rule config.wtf:*rule-dir*)
  (loop for key being the hash-keys of *files-table*
     using (hash-value value)
       do (apply-all-rules value)))


(defun output-lexer-result ()
  (loop for key being the hash-keys of *files-table*
     using (hash-value value)
     do (let ((outputname (make-pathname :directory (pathname-directory config.wtf:*report-dir*)
                                         :name (pathname-name key):type "xml")))
           (with-open-file (stream outputname :direction :output :if-exists :overwrite :if-does-not-exist :create)
             (format-object 'xml value stream)))))


(defun run-all ()
  (walk-files)
  (lexer-rules)
  (output-lexer-result ))

