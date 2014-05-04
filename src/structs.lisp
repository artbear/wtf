(in-package :wtf)

(defstruct codepoint
  "Описание позиции в файле"
  (id (id) :type number :read-only t)
  (index 0 :type number :read-only t)
  (char 0 :type number :read-only t) 
  (line-number 0 :type number :read-only t))


(defstruct token
  "Структура описывает прочитанный токен"
  (id (id) :type number :read-only t)  
  (type nil :type symbol :read-only t)
  (text "" :type string :read-only t)
  (start-point nil :type  codepoint :read-only t)
  (end-point nil :type codepoint :read-only t))


(defstruct rule
  "Структура правила"
  (id (id) :type number :read-only t)
  (name nil :type symbol :read-only t)
  (token-id nil  :read-only t)
  (description-rule "" :type string :read-only t)
  (fn nil :read-only t))

