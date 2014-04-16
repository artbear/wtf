(defpackage :config.wtf
  (:use :cl :cl-fad)
  (:export *base-dir*
           *log-dir*
           *rule-dir*
           *debug-log*
           create-and-clear-direcory)
)

(in-package :config.wtf)

(defparameter *debug-log* t)



(defun create-and-clear-direcory (base dirname)
  (let ((filename (cl-fad:pathname-as-directory  (format nil "~a~a/" base dirname))))
    (cl-fad:delete-directory-and-files filename :if-does-not-exist :ignore)
    (ensure-directories-exist filename)
    filename))



(defparameter *base-dir* (cl-fad:pathname-directory-pathname (car (cl-fad:list-directory "."))))
(defparameter *log-dir* (create-and-clear-direcory *base-dir* "log"))
(defparameter *rule-dir* (create-and-clear-direcory *base-dir* "rule"))


