;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

#|
Copyright (C) 2007, 2008, 2009  David Owen <dsowen@fugue88.ws>

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

(require '#:dso-lex)
(require '#:dso-parse)
(use-package '#:dso-lex)
(use-package '#:dso-parse)
(import 'dso-parse::substring)



(defparameter *input* "7,'now is ''the'' time'
13,\"for all good \"\"men\"\"\"
")

(flet ((trim (s) (substring s :start 1 :length (- (length s) 2))))
  (defun un-squote (s)
    (cl-ppcre:regex-replace-all "''" (trim s) "'"))
  (defun un-dquote (s)
  0  (cl-ppcre:regex-replace-all "\"\"" (trim s) "\"")))

(deflexer lex-csv (:priority-only t)
  ("," comma)
  ("\\r\\n?|\\n" newline)
  ("'(?:[^']|'')*'" value un-squote)
  ("\"(?:[^\"]|\"\")*\"" value un-dquote)
  ("[^,'\"\\n\\r]+" value)
  ("." illegal))

(defun lex-all-csv (input)
  (labels ((lex-all (start tokens)
	     (multiple-value-bind (class image next-offset)
		 (lex-csv input start)
	       (cond
		 ((and class (eq class 'illegal))
		  (error "Illegal input"))
		 (class
		  (lex-all next-offset (cons (cons class image) tokens)))
		 (t
		  (nreverse tokens))))))
    (lex-all 0 nil)))

(defmacro defmatcher (class)
  (let ((fn-sym (intern (concatenate 'string "T-" (symbol-name class)))))
    `(defun ,fn-sym (token-list)
      (when token-list
	(destructuring-bind (class . image) (first token-list)
	  (when (eq class ',class)
	    (values t (rest token-list) (list image))))))))

(defmatcher comma)
(defmatcher newline)
(defmatcher value)



(defgrammar ()
  (file (+ row))
  (row (t-value (* row-rest) (= t-newline))
       :filter (lambda (row) (cons (caar row) (mapcar #'second (second row)))))
  (row-rest ((= t-comma) t-value) :filter 'car))



;; Tokenize the input first...
(let ((tokens (lex-all-csv *input*)))
  ;; Then parse.
  (format t "~S~%" (nth-value 2 (file tokens))))



;; Or, do the tokenizing in the parser:
(defgrammar ()
  (file2 (+ row2))
  (row2 (value (* row-rest2) (= newline))
       :filter (lambda (row) (cons (car row) (mapcar #'second (second row)))))
  (row-rest2 ((= comma) value) :filter 'identity)
  (squoted-value ((= #\') (^ "([^']|'')+") (= #\'))
                 :cclass t
                 :filter
                 (lambda (s) (cl-ppcre:regex-replace-all "''" (car s) "'")))
  (dquoted-value ((= #\") (^ "([^\"]|\"\")+") (= #\"))
                 :cclass t
                 :filter
                 (lambda (s) (cl-ppcre:regex-replace-all "\"\"" (car s) "\"")))
  (unquoted-value (^ "[^,'\"\\n\\r]+") :cclass t)
  (value (/ squoted-value dquoted-value unquoted-value) :cclass t)
  (newline (^ "\\r\\n?|\\n"))
  (comma #\,))

;; Then parse directly.
(format t "~S~%" (nth-value 2 (file2 *input*)))
