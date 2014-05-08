(in-package :wtf)

(defparameter *files-table* (make-hash-table :test 'equal))


(defun walk-files ()
  (cl-fad:walk-directory config.wtf:*data-dir*
                         (lambda (x) (setf (gethash x *files-table*) (make-instance 'file-entry :filename x)))))



(defun lexer-rules-onefile (object)
  (format t "~% Проерям файл ~a" (slot-value object 'filename))
  (setf *current-file* object)
  (setf (slot-value object 'token-list) (file->list object))
  (apply-rules 'token object)
  (apply-rules 'lexer object)
  )

  
(defun lexer-rules ()
  (load-rule config.wtf:*rule-dir*)
  (loop for key being the hash-keys of *files-table*
     using (hash-value value)
     do (lexer-rules-onefile value)))


(defun output-lexer-result ()
  (loop for key being the hash-keys of *files-table*
     using (hash-value value)
     do (let ((outputname (make-pathname :directory (pathname-directory config.wtf:*report-dir*)
                                         :name (pathname-name key):type "xml")))
           (with-open-file (stream outputname :direction :output :if-exists :supersede :if-does-not-exist :create)
             (format-object 'xml value stream)))))

(defun run-all ()
  (walk-files)
  (lexer-rules)
  (output-lexer-result))

