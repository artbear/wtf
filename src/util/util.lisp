(in-package :util.wtf)

(defparameter *id* 0)
(defun id () (incf *id*))
(defun reset-id () (setf *id* 0))

