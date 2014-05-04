(in-package :wtf)

(defparameter *id* 0)
(defun id () (incf *id*))
(defun reset-id () (setf *id* 0))


;;; забрано отсюда http://filonenko-mikhail.blogspot.ru/2011/05/common-lisp-export-struct.html
(defmacro defstruct-and-export (structure &rest members)
  "Define a structure STRUCT with members MEMBERS and export the
   standard functions created. SPECIALS is a list of extra parameters eg
   ((:print-function pf)). Note double parentheses."
  (append
   `(progn
        ,(if (not (null members))
            (if (stringp (car members))
                `(defstruct ,structure ,(car members) ,@(cdr members))
                `(defstruct ,structure ,@members))
             `(defstruct ,structure))
        ,`(export ,`(quote ,(intern (concatenate 'string "MAKE-" (symbol-name (car structure))))))
        ,`(export ,`(quote ,(intern (concatenate 'string "COPY-" (symbol-name (car structure)))))))
      (if (not (null members))
          (if (stringp (car members))
              (mapcar  #'(lambda (member)
                             `(export ,`(quote ,(intern (concatenate 'string (symbol-name (car structure)) "-" (symbol-name (car member))))))) (cdr members))
              (mapcar  #'(lambda (member)
                             `(export ,`(quote ,(intern (concatenate 'string (symbol-name (car structure)) "-" (symbol-name (car member))))))) members)))
      (if (find :named structure)
          `((export ,`(quote ,(intern (concatenate 'string (symbol-name (car structure)) "-P" ))))
               (deftype ,(intern (symbol-name (car structure))) () '(satisfies ,(intern (concatenate 'string (symbol-name (car structure)) "-P" ))))))))







(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos))) 



(defgeneric format-object (what  value stream)
  (:documentation "Форматированый вывод"))



(defun flatten (data)
  (cond ((null data) nil)
        ((atom data) (list data))
        (t (mapcan #'flatten data))))
