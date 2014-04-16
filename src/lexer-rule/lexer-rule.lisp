(in-package :lexer-rule.wtf)

(defparameter *rule-table* (make-hash-table :test 'equal))


(defstruct rule-structure
  (id)
  )


(defun add-rule (name token-id description file fn)

  (let ((data (gethash token-id *rule-table* '()))))

  )



;; (:export add-rule run-rule run-all))
