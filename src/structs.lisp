(in-package :wtf)

(defstruct counted
  (id (id) :type number :read-only t)
  )

(defstruct owner
  (owner 0 :type number :read-only t))

(defstruct ( counted-owner (:include counted))
  (owner 0 :type number :read-only t))

(defstruct (codepoint (:include counted))
  "Описание позиции в файле"
;  (id (id) :type number :read-only t)
  (index 0 :type number :read-only t)
  (char 0 :type number :read-only t) 
  (line-number 0 :type number :read-only t))


(defstruct (token (:include))
  "Структура описывает прочитанный токен"
  (type nil :type symbol :read-only t)
  (kind nil :type symbol :read-only t)
  (text "" :type string :read-only t)
  (start-point nil :type  codepoint :read-only t)
  (end-point nil :type codepoint :read-only t))


(defstruct (rule (:include counted))
  "Структура правила"
  (name nil :type symbol :read-only t)
  (token-id nil  :read-only t)
  (description-rule "" :type string :read-only t)
  (scope nil :type symbol :read-only t)
  (fn nil :read-only t))



(defstruct (token-list (:include counted-owner))
  (tokens nil :read-only t))
