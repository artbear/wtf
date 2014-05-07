(in-package :wtf)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Описание регэкспов для разбора
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter string-re '(:SEQUENCE #\"
                                   (:GREEDY-REPETITION 0 NIL
                                                       (:REGISTER (:ALTERNATION :EVERYTHING (:REGISTER "\"\""))))
                                   #\")
  "Regular expression for recognizing string literals")  



;  (create-scanner 
;                          '(:sequence
;                            :start-anchor
;                            (:alternation
;                             (:sequence  #\" (:greedy-repetition 0 nil (:alternation (:sequence #\\ :everything) (:inverted-char-class #\"))) #\")
;                             (:sequence #\' (:greedy-repetition 0 nil (:alternation (:sequence #\\ :everything) (:inverted-char-class #\'))) #\'))))



(defparameter comment-re (create-scanner 
                          '(:sequence
                            :start-anchor 
                              (:sequence 
                                "//"
                                (:greedy-repetition 0 nil
                                    (:inverted-char-class #\Newline))
                                #\Newline)))
  "Regular expression for recognizing comment")

(define-parse-tree-synonym non-terminator
    (:inverted-char-class #\Newline #\Return))


(defparameter operator-re (create-scanner 
                           (list :sequence 
                                 :start-anchor 
                                 (cons :alternation
                                       (reverse *operator-tokens*))))
  "Regular expression for recognizing operators")

(defparameter whitespace-re (create-scanner
                                          '(:sequence
                                            :start-anchor
                                            (:greedy-repetition 1 nil
                                               :whitespace-char-class)))
  "Regular expression for consuming (and thereby skipping) whitespace ")

(defparameter integer-re (create-scanner
                          '(:sequence
                            :start-anchor
                             (:greedy-repetition 1 nil :digit-class)))
  "Regular expression for recognizing integer literals")

(defparameter floating-re (create-scanner
                            '(:sequence
                              :start-anchor
                              (:alternation
                                (:sequence
                                  (:greedy-repetition 1 nil :digit-class)
                                 #\.
                                 (:greedy-repetition 1 nil :digit-class)
                                 )
                                (:sequence
                                  #\.
                                  (:greedy-repetition 1 nil :digit-class)))))
  "Regular expression for recognizing floating-point literals")





(defparameter *white-space* '(#\Tab #\Space #\Newline #\Return ))
(defparameter *line-counter* (make-hash-table))
(defparameter *count-line-counter* 0)

(defun init-line-counter (in-string)
  "Инициализирует соответствие позиции списку (номер строки, номер колонки)"
  (let ((index 0) (line 1) (column 0))
    (loop for c across in-string
       :do (progn
             (if (eq c #\Newline)
                 (setf line (+ 1 line) column 0)
                 (incf column))
             (setf (gethash (incf index) *line-counter*) (list line column))))
    (setf *count-line-counter* (hash-table-count *line-counter*))))



(defun point (x)
  (let ((flist (gethash (min (+ 1 x) *count-line-counter*) *line-counter* 'empty)))
    (make-codepoint :index x
                :line-number (first flist)
                :char (second flist))))
    


(defun create-token (type text start end)
  (make-token :type type
              :text text
              :start-point (point start)
              :end-point (point end)))


(defparameter *dot-readed* nil)
(defparameter *text* "")
(defparameter *cursor* 0)
(defparameter *text-length* 0)

(defun set-cursor (new-pos)
  (setf *cursor* new-pos))

(defun tokenize ()
  (when (< *cursor* *text-length*)
    (re-cond (*text* :start *cursor*)
             ("^$"
              (create-token :eof "" %s %e ))
             

             (whitespace-re
              (set-cursor  %e)
              (setf *dot-readed* nil)
              (create-token :ws (subseq *text*  %s  %e) %s %e))


             (comment-re
              (set-cursor  %e)
              (setf *dot-readed* nil)
              (create-token :comment-literal (subseq *text*  %s  %e) %s %e))
             (operator-re
              (set-cursor  %e)
              (let* ((text (subseq *text* %s %e))
                     (terminal (gethash text *tokens-to-symbols*))
                     (result   (create-token terminal text  %s %e)))
                    (setf *dot-readed* (equal terminal :dot))
                    result))
       


             (floating-re
              (set-cursor %e)
              (setf *dot-readed* nil)
              (create-token :number
                            (subseq *text*  %s  %e)
                            %s %e))
             (integer-re
              (set-cursor %e)
              (setf *dot-readed* nil)
              (create-token :number
                            (subseq *text*  %s  %e)
                            %s %e))

             ("^(\\&|(\\#)?\\w)+"
              (set-cursor %e)
              (let* ((text (subseq *text* %s %e))
                     (scope (equal "&" (subseq text 0 1)))
                     (readed (gethash (string-downcase text) *tokens-to-symbols*))
                     (terminal (or (if (and *dot-readed* readed) :identifier readed)
                                   :identifier))
                     (result (create-token  (if scope :scope terminal)  text %s %e )))
                (setf *dot-readed* nil)
                result))

             (string-re
              (set-cursor %e)
              (setf *dot-readed* nil)
              (create-token :string-literal (subseq *text*  %s  %e)  %s %e))
             
             ("^\\S+"
              (error "unrecognized token: '~A' at ~a " (subseq *text*   %s %e) (point %s) ))
             (t
              (error "coding error - we should never get here")))))

(defun next-token ()
  "Возвращает токен. Изменяет позицию в потоке"
  (tokenize))


(defun create-lexer (string-to-analize)
  (setf *text* string-to-analize)
  (setf *cursor* 0)
  (setf *text-length* (length *text*)))


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



(defun lexer->list (input-string)
  (init-line-counter input-string)
  (create-lexer input-string)
  (prog1
      (loop as _token = (next-token)
         while  _token
         collect  _token)
    (setf *line-counter* (make-hash-table))))

(defun file->list (file)
  (setf *current-file* file)
  (lexer->list (read-file-into-string (slot-value file 'filename ))))


(defun string->list (input)
  (lexer->list input))

