(in-package :lexer.wtf)

(defstruct-and-export (codepoint)
  "Описание позиции в файле"
  (id (util.wtf:id) :type number :read-only t)
  (index 0 :type number :read-only t)
  (char 0 :type number :read-only t) 
  (line-number 0 :type number :read-only t))


(defstruct-and-export (token)
  "Структура описывает прочитанный токен"
  (id (util.wtf:id) :type number :read-only t)  
  (type nil :type symbol :read-only t)
  (text "" :type string :read-only t)
  (start-point nil :type  codepoint :read-only t)
  (end-point nil :type codepoint :read-only t))

