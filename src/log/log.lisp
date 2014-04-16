
(in-package :logger.wtf)


(defun logger-name (filepathname)
  (let ((dir (config.wtf:build-dir *log-dir* (cdr (pathname-directory filepathname)))))
    (make-pathname
     :directory dir
     :type "log"
     :name (pathname-name filepathname))))
  


(defmacro start-logger ()
  (when config.wtf:*debug-log*
    (let* ((filename (or *compile-file-truename* *load-truename*))
           (category-symbol (format nil "~a~a" '#:s- (pathname-name filename))))
      `(progn
       (format t "Начали логирование модуля ~a в файл ~a. Имя логгера ~a" ,filename (logger-name ,filename) ,category-symbol)
       (cl-log:defcategory (intern ,category-symbol) (append (list 'or) (map 'list (lambda (x) (intern (format nil "~a~a.~a" '#:s- ,category-symbol x))) (list 'info 'debug 'warn 'error))))
       (start-messenger 'text-file-messenger
                        :filter (append (list 'or) (map 'list (lambda (x) (intern (format nil "~a.~a" ,category-symbol x))) (list 'info 'debug 'warn 'error)))
                        :name (intern ,category-symbol)
                        :filename (logger-name ,filename))))))


