(in-package :wtf)

(defun find-pred (index data &key skip )
  (find-in-vector (1- index) data skip  -1))
