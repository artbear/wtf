(in-package :lexer.wtf)

(logger.wtf:start-logger)

;;;; Token definitions
(defmacro deftoken (token-type symbol key &optional en-key)
  "Описываем токен. В зависимости от token-type заполняем хэши"
  (cond
    ((eq token-type :operator-token)
     `(progn
       (setf (gethash ,key *tokens-to-symbols*) ,symbol)
       (setf (gethash ,symbol *symbols-to-tokens*) ,key)
       (push ,key *operator-tokens*)))
    ((eq token-type :operation)
     `(setf (gethash ,symbol *symbols-to-tokens*) ,key))
    ((eq token-type :single-comment)
     `(progn
       (setf (gethash ,key *comment-to-symbols*) ,symbol)
       (push ,symbol *comment-symbols*)))
    ((eq token-type :multi-comment)
     `(progn
       (setf (gethash ,key *comment-multiple-to-symbols*) ,symbol)
       (push ,symbol *comment-symbols*)))
    ((eq token-type :keyword)
     `(progn
       (setf (gethash (string-downcase ,key) *tokens-to-symbols*) ,symbol)
       (setf (gethash (string-downcase ,en-key) *tokens-to-symbols*) ,symbol)
       (setf (gethash  ,symbol *correct-keywords*) (list ,key ,en-key))
       
       (push ,symbol *keyword-symbols*)))
    ((eq token-type :place)
     `(progn
       (setf (gethash ,key *tokens-to-place*) ,symbol)
       (push ,symbol *keyword-symbols*)))
    (key
     `(setf (gethash ,key *tokens-to-symbols*) ,symbol))
    (t
     `(setf (gethash ,symbol *tokens-to-symbols*) ,symbol))))

;;;; Token lookups

(defparameter *tokens-to-place* (make-hash-table :test 'equal)
  "Хэш описывает список строк для &НаКлиенте и т.д.")

(defparameter *tokens-to-symbols* (make-hash-table :test 'equal)
  "Ключевые слова ")

(defparameter *correct-keywords* (make-hash-table :test 'equal)
  "Ключевые слова ")


(defparameter *comment-to-symbols* (make-hash-table :test 'equal)
  "rus: Преобразует тэг xml в символ. ")

(defparameter *comment-multiple-to-symbols* (make-hash-table :test 'equal)
   "rus: Преобразует тэг xml в символ. ")


(defparameter *symbols-to-tokens* (make-hash-table :test 'eq)
   "rus: Соответствие токенов для символов. ")

(defparameter *operator-tokens* nil
  "Список операторов")

(defparameter *keyword-symbols* nil
   "rus: Список ключевых слов")

(defparameter *comment-symbols* nil
   "rus: Список xml тэгов для комментария")


(defparameter *restricted-tokens* (make-hash-table :test 'eq)
  "wtf???")

;; Operators and punctuators
(deftoken :operator-token :semicolon    ";" )
(deftoken :operator-token :comma        "," )
(deftoken :operator-token :hook         "?" )
(deftoken :operator-token :colon        ":" )
(deftoken :operator-token :equals       "=" )
(deftoken :operator-token :not-equals   "<>" )
(deftoken :operator-token :less-than-equals "<=" )
(deftoken :operator-token :greater-than-equals ">=" )
(deftoken :operator-token :less-than    "<" )
(deftoken :operator-token :greater-than ">" )
(deftoken :operator-token :label-declaration       "~" )
(deftoken :operator-token :asterisk     "*" )
(deftoken :operator-token :slash        "/" )
(deftoken :operator-token :percent      "%" )
(deftoken :operator-token :plus         "+" )
(deftoken :operator-token :minus        "-" )
(deftoken :operator-token :dot          "." )
(deftoken :operator-token :left-bracket "[" )
(deftoken :operator-token :right-bracket "]" )
(deftoken :operator-token :open   "(" )
(deftoken :operator-token :close  ")" )

(deftoken :operation :assign       "=" )
(deftoken :operation :plus          "+")
(deftoken :operation :minus     "-" )
(deftoken :operation :mul     "*" )
(deftoken :operation :div       "/" )
(deftoken :operation :mod       "%" )


;; Keywords

(deftoken :keyword :and  "и" "and")
(deftoken :keyword :break "Прервать" "Break")
(deftoken :keyword :continue "Продолжить" "continue" )
(deftoken :keyword :do "Цикл" "do" )
(deftoken :keyword :each "каждого" "each" )
(deftoken :keyword :elsif "ИначеЕсли" "elsif" )
(deftoken :keyword :else "Иначе" "else" ) 
(deftoken :keyword :enddo "КонецЦикла" "enddo" ) 
(deftoken :keyword :endfunction "КонецФункции" "endfunction" ) 
(deftoken :keyword :endif "КонецЕсли" "endif" ) 
(deftoken :keyword :endprocedure "КонецПроцедуры" "endprocedure" ) 
(deftoken :keyword :endtry "КонецПопытки" "endtry" ) 
(deftoken :keyword :except "Исключение" "except" ) 
(deftoken :keyword :export "Экспорт" "export" ) 
(deftoken :keyword :for "Для" "for" ) 
(deftoken :keyword :function "Функция" "function" ) 
(deftoken :keyword :goto "Перейти" "goto" )  
(deftoken :keyword :if "Если" "if" ) 
(deftoken :keyword :in "Из" "in" ) 
(deftoken :keyword :new "Новый" "new" ) 
(deftoken :keyword :not "Не" "not" ) 
(deftoken :keyword :or "Или" "or" ) 
(deftoken :keyword :procedure "Процедура" "procedure" ) 
(deftoken :keyword :throw "Вызватьисключение" "raise" ) 
(deftoken :keyword :return "Возврат" "return" ) 
(deftoken :keyword :then "Тогда" "then" )  
(deftoken :keyword :to "по" "to" )  
(deftoken :keyword :try "Попытка" "try" ) 
(deftoken :keyword :val "Знач" "val" ) 
(deftoken :keyword :var "Перем" "var" ) 
(deftoken :keyword :while "Пока" "while" )
(deftoken :keyword :true "Истина" "true" ) 
(deftoken :keyword :false "Ложь" "false" ) 
(deftoken :keyword :undefined "Неопределено" "undefined" ) 

(deftoken :keyword :client "Клиент" "client" ) 
(deftoken :keyword :server "Сервер" "server" ) 
(deftoken :keyword :externalconection "внешнеесоединение" "externalconection" ) 

(deftoken :keyword :preprocessor-if "#если" "#if" )
(deftoken :keyword :preprocessor-endif "#конецесли" "#endif" )
(deftoken :keyword :preprocessor-elsif "#иначеесли" "#elsif" )
(deftoken :keyword :preprocessor-else "#иначе" "#else" )

(deftoken :place :client-place "&наклиенте" "&client" )
(deftoken :place :server-place "&насервере" "&server" )
