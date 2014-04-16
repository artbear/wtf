
(in-package :logger.wtf)


(defun build-log-dir (dir-list)
  (let ((filename (format nil "~a" *log-dir*)))
    (dolist (dir dir-list)
      (setf filename (config.wtf:create-and-clear-direcory filename dir)))
    (pathname-directory (cl-fad:pathname-as-directory filename))))

(defun logger-name (filepathname)
  (let ((dir (build-log-dir (cdr (pathname-directory filepathname)))))
    (make-pathname
     :directory dir
     :type "log"
     :name (pathname-name filepathname))))
  


(defmacro start-logger ()
  (when config.wtf:*debug-log*
    (let* ((filename (or *compile-file-truename* *load-truename*))
           (category-symbol (format nil ":~a" (pathname-name filename))))
      `(progn
       (format t "Начали логирование f ~a in ~a. logger name ~a" ,filename (logger-name ,filename) ,category-symbol)
       (cl-log:defcategory (intern ,category-symbol) (append (list 'or) (map 'list (lambda (x) (intern (format nil "~a.~a" ,category-symbol x))) (list 'info 'debug 'warn 'error))))
       (start-messenger 'text-file-messenger
                        :name (intern ,category-symbol)
                        :filename (logger-name ,filename))))))


