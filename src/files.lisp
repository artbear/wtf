(in-package :wtf)

(defparameter *files-table* (make-hash-table :test 'equal))

(defun walk-files ()
  (setf *files-table* (make-hash-table :test 'equal))
  (cl-fad:walk-directory config.wtf:*data-dir*
                         (lambda (x) (setf (gethash x *files-table*) (make-file-entry :filename x)))))

(defun lexer-rules-onefile (object)
  (format t "~% Проерям файл ~a" (file-entry-filename object))
  (setf (file-entry-token-list object ) (file->list (file-entry-filename object)))
  (apply-rules 'lexer object))

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

