(defpackage :config.wtf
  (:use :cl :cl-fad)
  (:export *base-dir*
           *log-dir*
           *rule-dir*
           *report-dir*
           *debug-log*
           build-dir
           create-and-clear-direcory
           create-direcory )
)

(in-package :config.wtf)

(defparameter *debug-log* t)



(defun build-dir (base dir-list)
  (let ((filename (format nil "~a" base)))
    (dolist (dir dir-list)
      (setf filename (config.wtf:create-direcory filename dir)))
    (pathname-directory (cl-fad:pathname-as-directory filename))))


(defun create-and-clear-direcory (base dirname)
  (let ((filename (cl-fad:pathname-as-directory  (format nil "~a~a/" base dirname))))
    (cl-fad:delete-directory-and-files filename :if-does-not-exist :ignore)
    (ensure-directories-exist filename)
    filename))

(defun create-direcory (base dirname)
  (let ((filename (cl-fad:pathname-as-directory  (format nil "~a~a/" base dirname))))
    (ensure-directories-exist filename)
    filename))



(defparameter *base-dir* (cl-fad:pathname-directory-pathname (or *compile-file-truename* *load-truename*)))
(defparameter *log-dir*  (create-and-clear-direcory *base-dir* "log"))
(defparameter *rule-dir* (create-direcory *base-dir* "rule"))
(defparameter *report-dir* (create-and-clear-direcory *base-dir* "report"))


(in-package :cl-log)

(setf (cl-log:log-manager)
      (make-instance 'cl-log:log-manager :message-class 'formatted-message))




(defmethod format-message ((self formatted-message))
  (format nil "~a ~a ~?~&"
	  (local-time:format-timestring nil 
					(local-time:universal-to-timestamp 
					 (timestamp-universal-time (message-timestamp self))))
	  (message-category self)
	  (message-description self)
	  (message-arguments self)))
