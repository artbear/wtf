(in-package :lexer.wtf)
(logger.wtf:start-logger)


(defstruct codepoint
  "Описание позиции в файле"
  (id (util.wtf:id) :type number :read-only t)
  (index 0 :type number :read-only t)
  (char 0 :type number :read-only t) 
  (line-number 0 :type number :read-only t)
)


(defstruct token
  "Структура описывает прочитанный токен"
  (id (util.wtf:id) :type number :read-only t)  
  (type nil :type symbol :read-only t)
  (text "" :type string :read-only t)
  (start-point nil :type (or null codepoint) :read-only t)
  (end-point nil :type (or null codepoint) :read-only t))

(defparameter *white-space* '(#\Tab #\Space #\Newline #\Return ))

(defstruct lexer
  "Структура описывает настройки и состояние лексера"
  (file-id 0 :type number :read-only t) ;; идентификатор анализируемого файла
  (skip-WS nil :type symbol :read-only t) ;; признак анализа пробельных символов
  (after-dot nil :type symbol ) ;; признак прочитаной точки
  (peeked  nil :type (or null token)) ;; прочитанный токен
  (readed  nil :type (or null token)) ;; окончательно прочитанный токен
  (current-index 0 :type number )
  (current-char 0 :type number) 
  (current-line-number 0 :type number )
  (stream nil :read-only t))


(defmethod peek-token ((lexer lexer))
  "Возвращает токен без изменения позиции в потоке"
  (or (lexer-peeked lexer)
      (setf (lexer-peeked lexer) (tokenize lexer))))



(defmethod next-token ((object lexer))
  "Возвращает токен. Изменяет позицию в потоке"
  (let ((value (if (lexer-peeked object)
      (let ((r (lexer-peeked object)))
        (setf (lexer-peeked object) nil)
        r)
      (tokenize object))))
    (setf (lexer-readed object) value)
    value))

(defmethod next-token :after ((object lexer))
 "Выводит информаци о причтанном токена в поток"
  (let* ((token (lexer-readed object))
         (start (token-start-point token))
         (end (token-end-point token))
         )
    (if (eql :eof (token-type token))
        (cl-log:log-message :lexer.debug "Конец потока")
        (cl-log:log-message :lexer.debug 
                            (format nil "Прочитан токен : Тип ~a. Текст '~a'. Начало токена (~a ~a ~a) Конец токена (~a ~a ~a) " (token-type token)
                                    (token-text token)
                                    (codepoint-index start) (codepoint-line-number start) (codepoint-char start)
                                    (codepoint-index end) (codepoint-line-number end) (codepoint-char end))))))


(defun tokenize (lexer)
  "Преобразовывет поток символов в поток лексем"
  (let 
      ((stream (lexer-stream lexer ))
       (start-index (lexer-current-index lexer ))
       (start-line   (lexer-current-line-number lexer ))
       (start-char  (lexer-current-char lexer ))       
       (index 0) (line 0) (char 0)
       (in-dot (lexer-after-dot lexer )))
    
    (defun start-token ()
      ;(setf line start-line char  start-char index start-index )
      )

    (defun ctoken (type text)
      (let ((res 
             (make-token :type type
                         :text text
                         :start-point (make-codepoint :index start-index :char start-char :line-number start-line)
                         :end-point (make-codepoint
                                     :index (+ index start-index)
                                     :char (+ char start-char)
                                     :line-number (+ line start-line)))))
        (setf (lexer-current-index lexer) (+ index start-index)
              (lexer-current-char lexer) start-char
              (lexer-current-line-number lexer) (+ line start-line))

        res))


    (defun peek-c ()
      (peek-char nil stream nil nil))

    (defun next-c ()
      (let ((ch (read-char stream)))
        (when ch
          (incf index)
          (if (equal #\Newline ch)
              (setf line (1+ line) char 0 )
              (incf char)))
        ch))

    (defun read-while (pred )
      (with-output-to-string (*standard-output*)
        (loop :for ch := (peek-c )
           :while (and ch (or (null ch) (funcall pred ch)))
           :do (princ (next-c )))))
q
    (defun read-until (pred )
      (with-output-to-string (*standard-output*)
        (loop :for ch := (peek-c )
           :until (and ch (or (null ch) (funcall pred ch)))
           :do (princ (next-c )))))
    
         


    
    (defun read-newline ()
      (ctoken :nl (read-while (lambda (x) (find x  '(#\Newline #\Return ))))))

    
    (defun read-whitespace ()
      (ctoken :ws (read-while (lambda (x) (find x  '(#\Tab #\Space ))))))

      
    (defun skip-whitespace ()
      (progn
        (read-while (lambda (x) (find x *white-space*)))
        (setf (lexer-current-index lexer) (+ index start-index)
              (lexer-current-char lexer) start-char
              (lexer-current-line-number lexer) (+ line start-line))
        

      ))

    (defun read-datastring ()
      (ctoken :datastring
              (concatenate 'string
                           (string (next-c ))
                           (read-until (lambda (ch) (char= ch #\')))
                           (string (next-c )))))

    (defun read-string ()
      "TODO. Вот тут косяк будет если поток заканчяивается кавычкой. Переписать"
      (ctoken :string (concatenate 'string
                                   (string (next-c ))
                                   (with-output-to-string (*standard-output*)
                                     (loop
                                        (let ((ch (next-c )))
                                          (if (char= ch #\")
                                              (if (char= #\" (peek-c ))
                                                  (progn
                                                    (princ ch)
                                                    (princ (next-c )))
                                                  (progn
                                                    (princ ch)
                                                    (return)))
                                              (princ ch))))))))
    

    (defun to-string (in-char)
      (if in-char
          (string in-char)
          ""))

    (defun read-comment ()
      (let ((token 
             (ctoken :comment
                     (concatenate 'string "/" (read-until (lambda (x) (char= #\Newline x)))))))
        (next-c)
        token))
  
    (defun read-div-or-comment ()
      (next-c )
      (if (char= #\/ (peek-c ))
          (read-comment ) ;; будем помнить что один символ уже забран
          (ctoken :operator-div "/")))
    
  
    (defun read-operator ()
      (let ((ch (next-c )))
        (ctoken   (gethash (string ch) *tokens-to-symbols*)
                  (string ch))))
    
    (defun read-number ()
      (let* ((text (read-while #'digit-char-p ))
             (stext (if (eql #\. (peek-c ))
                        (progn
                          (next-c )
                          (concatenate 'string "." (read-while #'digit-char-p )))
                        "")))
        (ctoken :digit
                (concatenate 'string text stext))))
  
    (defun identifier-char-p (ch)
      (or (alphanumericp ch)  (char= ch #\_)))
  
    (defun read-word-loc ()
      (read-while #'identifier-char-p ))
  
    (defun read-preprocessor ()
      (let* ((name (concatenate 'string (string (next-c)) (read-word-loc )))
             (token (ctoken  (gethash (string-downcase name) *tokens-to-symbols*)
                             name)))
        token))
    (defun read-place ()
      (let* ((name (concatenate 'string (string (next-c )) (read-word-loc )))
             (token (ctoken  (gethash (string-downcase name) *tokens-to-place*)
                             name)))
        token))
  
    (defun read-id-word ()
      (let* ((name  (read-word-loc ))
             (keyword  (gethash (string-downcase name) *tokens-to-symbols*)))
        (prog1
            (ctoken  (if in-dot :id (or keyword :id))
                     name)
          (setf in-dot nil))))
  
    (defun get-token ()
      (start-token)
        (case (peek-c)
          ('nil (make-token :type :eof ))
          ((#\Newline #\Return)
           (if (not (lexer-skip-ws lexer))
               (read-newLine)
               (progn
                 (skip-whitespace)
                 (tokenize lexer))))
          
          ((#\Tab #\Space )
           (if (not (lexer-skip-ws lexer))
               (read-whitespace)
               (progn
                 (skip-whitespace)
                 (tokenize lexer))))

          (#\' (read-datastring ))
          (#\" (read-string ))
          (#\/ (read-div-or-comment ))
          (#\. (progn (setf in-dot t) (read-operator )))
          ((\~ #\+ #\- #\* #\> #\< #\= #\% #\: #\? #\( #\) #\[ #\] #\, #\;) (read-operator))
          (#\# (read-preprocessor))
          (#\& (read-place))
          ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) (read-number))
          (otherwise (read-id-word ))))

    (get-token)))


(defun create-lexer (string-to-analize)
  (make-lexer :skip-ws nil :stream (make-string-input-stream string-to-analize)  ))


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
  (let ((lexer (create-lexer input-string)))
    (loop as _token = (next-token lexer)
       while (not (eql :eof (token-type _token)))
       collect  _token)))

(defun file->list (filename)
  (cl-log:log-message :lexer.debug (format nil "Начали разбор файла ~a " filename))
  (lexer->list (read-file-into-string filename)))


(defun string->list (input)
  (cl-log:log-message :lexer.debug (format nil "Начали разбор строки '~a' " input))  
  (lexer->list input))

