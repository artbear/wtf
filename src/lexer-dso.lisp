(in-package :wtf)

(deflexer 1C-scan (:priority-only t)
  ("//(?:[^\n])*\n" :comment-line)
  ("\\." :dot)
  (";" :semi)
  ("\\?" :hook)
  (":" :colon)
  ("=" :equal)
  ("<>" :not-equal)
  (">=" :gt-eql)
  ("<=" :le-eql)
  (">" :gt)
  ("<" :le)
  ("~" :label-start)
  ("\\*" :mul)
  ("/" :div)
  ("%" :mod)
  ("\\*" :mul)
  ("\\+" :plus)
  ("\\-" :minus)
  ("\\(" :open)
  ("\\)" :close)
  ("\\[" :array-open)
  ("\\]" :array-close)
  ("\"(?:[^\"]|\"\")*\"" :string)
  ("\'[0-9]+\'" :date)
  ("[0-9]+" :number)
  ("[0-9]+\.[0-9]+" :number)
  ("\\#\\w+" :preprocessor-word)
  ("\\&\\w+" :scope-word)
  ("\\w+" :word)
  ("\\s+" :ws))

(defun 1C-lex (input)
  (let ((position->location (make-hash-table ))
        (count-line-counter 0))
    (labels ((init-position-location ()
               (let ((index 0) (line 1) (column 0))
                 (loop for c across input
                    :do (progn
                          (if (eq c #\Newline)
                              (setf line (+ 1 line) column 0)
                              (incf column))
                          (setf (gethash (incf index) position->location) (list line column))))
                 (setf count-line-counter (hash-table-count position->location))))
             
             (get-location (position)
               (let ((flist (gethash (min (+ 1 position) count-line-counter) position->location 'empty)))
                 (make-codepoint :index position
                                 :line-number (first flist)
                                 :char (second flist))))
             (recognize (type text)
               (case type
                 ((:preprocessor-word :scope-word :word)
                  (let ((readed (gethash (string-downcase text) *tokens-to-symbols* 'nil)))
                    (if readed (values readed type)
                        (values :id type))))
                 (t (values type type))))
             
             
             (create-token (type text start end)
               (multiple-value-bind  (type kind) (recognize type text)
                 (make-token :type type :kind kind :text text
                             :start-point (get-location start) :end-point (get-location end))))
             (lex-all (start tokens)
               (multiple-value-bind (class image next-offset)
                   (1C-scan input start)
                 (cond
                   ((and class (eq class 'illegal))
                    (error "Illegal input"))
                   (class
                    (lex-all next-offset (cons (create-token class image start (1- next-offset)) tokens)))
                   (t
                    (nreverse tokens))))))
      (progn
        (init-position-location)
        (lex-all 0 nil)))))



(defun read-file-into-string (pathname)
  (with-open-file (file-stream pathname)
    (let ((*print-pretty* nil))
      (read-char file-stream)
      (with-output-to-string (datum)
        (let ((buffer (make-array 4096 :element-type 'character)))
	  (loop
	     :for bytes-read = (read-sequence buffer file-stream)
	     :do (write-sequence buffer datum :start 0 :end bytes-read)
	     :while (= bytes-read 4096)))))))


(defun file->list (file)
  (1C-lex (read-file-into-string file)))

(defun string->list (input)
  (1C-lex input))
