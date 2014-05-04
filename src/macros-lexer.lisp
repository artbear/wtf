(in-package :wtf)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; макросы для сканера
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun re-cond-clause (string start block-name clause)
  (destructuring-bind (re-form &body body) clause
    (if (eq re-form t)
        `(return-from ,block-name
          (progn ,@body))
        `(multiple-value-bind (%s %e )
          (scan ,re-form ,string :start ,start)
          (unless (null %s)
            (return-from ,block-name
              (progn ,@body)))))))

(defmacro re-cond ((string &key (start 0)) &rest clauses)
  "(re-cond (STRING :start START) (REGEXP FORMS*)*  
If REGEXP matches STRING, then %S, %E, %SUB-S, and %SUB-E will be bound during execution of FORMS"
  (let ((gblock (gensym))
        (gstart (gensym))
        (gstring (gensym)))
    `(block ,gblock 
      (let ((,gstart ,start)
            (,gstring ,string))
        ,@(loop for clause
                in clauses
                collect (re-cond-clause gstring gstart gblock clause))))))
  