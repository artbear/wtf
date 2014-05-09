(in-package :wtf)

(defun prefix-p (string prefix)
  "return:  whether prefix is a prefix of the string."
  (and (<= (length prefix) (length string))
       (string= string prefix :end1 (length prefix))))


(defun maptree (fn tree)
  "MAPTREE maps a function over a tree of cons cells.
   If TREE is NIL, returns NIL.
   If TREE is a cons cell, recursively calls MAPTREE on the CAR and CDR and returns a new cons cell
   whose CAR and CDR are the results.
   Otherwise, returns the result of applying FN to TREE."
  (cond
    ((consp tree)
     (cons (maptree fn (car tree))
           (maptree fn (cdr tree))))
    ((null tree)
     tree)
    (t
     (funcall fn tree))))




(defparameter *muffle-conflicts* t)
(defun expand-hashtable-to-values (hashtable)
  "Returns a list of all the values stored in a hashtable."
  (let ((valuelist '()))
    (maphash #'(lambda (k v) 
                 (declare (ignore k))
                 (setf valuelist (cons v valuelist)))
             hashtable)
    valuelist))

; need to collect productions 

(defmacro defparser (parser-name starting-production &body productions)
  "This macro emulates the Lispworks parsergenerator's defparser macro, but instead creates output
   for CL-YACC"
  (let* ((starting-point (first starting-production))
         (starting-symbol (first starting-point))
         (header `(yacc:define-parser ,parser-name
                   (:muffle-conflicts t)
;		   (:print-derives-epsilon t)
;		   (:print-first-terminals t)
;		   (:print-states t)
;		   (:print-goto-graph t)
;		   (:print-lookaheads )
                   (:start-symbol ,starting-symbol)
                   (:terminals (append (list :any ) *keyword-symbols* ))
                   (:precedence nil)
                   ,starting-point)))
    (append header (generate-productions productions))))

; here we turn
;  ((primary-expression object-literal) $1)
;   into 
;  (primary-expression
;      (object-literal #'(lambda (&rest expr) (nth 0 expr))))
;
; and
;
; ((literal :number) (make-numeric-literal :value $1))
;  into
; (literal
;   (:number #'(lambda (&rest expr) (make-numeric-literal :value (nth 0 expr)))))



(defun generate-productions (productions)
  "Used by defparser macro. Take the lispworks list of productions and convert them into
   CL-YACC versions"
  (let* ((production-map (make-hash-table)))
    (dolist (production productions)
            (let* ((rule (nth 0 production))
                   (action (maptree 'replace-special-variables (nth 1 production)))
                   (rule-name (first rule))
                   (rule-terminals (rest rule)))
              (setf (gethash rule-name production-map) 
                    (cons (append rule-terminals `(#'(lambda (&rest expr) ,action)))
                          (gethash rule-name production-map)))))
    (let* ((output '()))
      (maphash #'(lambda (k v) 
                   (setf output (cons (append (list k) (reverse v)) output)))
               production-map)
      (reverse output))))

(defun replace-special-variables (leaf)
  "Replace $$n with (token-value (nth n-1 expr)) and $n with (nth n-1 expr)"
  (if (symbolp leaf)
    (let ((symname (symbol-name leaf)))
      (cond
        ((string= symname "$S")
         `(element-start expr))
        ((string= symname "$E")
         `(element-end expr))
        ((prefix-p symname "$$")
         `(token-value (nth ,(1- (parse-integer (subseq symname 2))) expr)))
        ((prefix-p symname "$")
         `(nth ,(1- (parse-integer (subseq symname 1))) expr))
        (t
         leaf)))
    leaf))


