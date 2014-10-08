;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

#|
Copyright (C) 2007, 2008  David Owen <dsowen@fugue88.ws>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser Public License for more details.

You should have received a copy of the GNU Lesser Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
|#

(in-package #:dso-parse)



(defun substring (string &key (start 0) end length)
  (when (and end length) (error "Specify only one of END or LENGTH."))
  (when end (setf length (- end start)))
  (unless length (setf length (- (length string) start)))
  (make-array length
	      :element-type 'character
	      :displaced-to string
	      :displaced-index-offset start))

(defmacro while (cond &body body)
  `(do ()
    ((not ,cond))
    ,@body))

(defmacro symcase (symvar &body clauses)
  (flet ((is-default (clause)
	   (string= (symbol-name (first clause)) #.(symbol-name 't))))
    (let ((symname (gensym))
	  (default-clause (find-if #'is-default clauses)))
      `(let ((,symname (symbol-name ,symvar)))
	(cond
	  ,@(mapcar (lambda (clause)
		      (destructuring-bind (sym form) clause
			`((string= ,symname ,(symbol-name sym)) ,form)))
		    (remove-if #'is-default clauses))
	  ,@(when default-clause
		  (destructuring-bind (sym form) default-clause
		    `((,sym ,form)))))))))
