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
       (setf (gethash ,key *tokens-to-symbols*) ,symbol)
       (setf (gethash ,en-key *tokens-to-symbols*) ,symbol)
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
(deftoken :keyword :break "прервать" "break")
(deftoken :keyword :continue "продолжить" "continue" )
(deftoken :keyword :do "цикл" "do" )
(deftoken :keyword :each "каждого" "each" )
(deftoken :keyword :elsif "иначеесли" "elsif" )
(deftoken :keyword :else "иначе" "else" ) 
(deftoken :keyword :enddo "конеццикла" "enddo" ) 
(deftoken :keyword :endfunction "конецфункции" "endfunction" ) 
(deftoken :keyword :endif "конецесли" "endif" ) 
(deftoken :keyword :endprocedure "конецпроцедуры" "endprocedure" ) 
(deftoken :keyword :endtry "конецпопытки" "endtry" ) 
(deftoken :keyword :except "исключение" "except" ) 
(deftoken :keyword :export "экспорт" "export" ) 
(deftoken :keyword :for "для" "for" ) 
(deftoken :keyword :function "функция" "function" ) 
(deftoken :keyword :goto "перейти" "goto" )  
(deftoken :keyword :if "если" "if" ) 
(deftoken :keyword :in "из" "in" ) 
(deftoken :keyword :new "новый" "new" ) 
(deftoken :keyword :not "не" "not" ) 
(deftoken :keyword :or "или" "or" ) 
(deftoken :keyword :procedure "процедура" "procedure" ) 
(deftoken :keyword :throw "вызватьисключение" "raise" ) 
(deftoken :keyword :return "возврат" "return" ) 
(deftoken :keyword :then "тогда" "then" )  
(deftoken :keyword :to "по" "to" )  
(deftoken :keyword :try "попытка" "try" ) 
(deftoken :keyword :val "знач" "val" ) 
(deftoken :keyword :var "перем" "var" ) 
(deftoken :keyword :while "пока" "while" )
(deftoken :keyword :true "истина" "true" ) 
(deftoken :keyword :false "ложь" "false" ) 
(deftoken :keyword :undefined "неопределено" "undefined" ) 

(deftoken :keyword :client "клиент" "client" ) 
(deftoken :keyword :server "сервер" "server" ) 
(deftoken :keyword :externalconection "внешнеесоединение" "externalconection" ) 

(deftoken :keyword :preprocessor-if "#если" "#if" )
(deftoken :keyword :preprocessor-endif "#конецесли" "#endif" )
(deftoken :keyword :preprocessor-elsif "#иначеесли" "#elsif" )
(deftoken :keyword :preprocessor-else "#иначе" "#else" )

(deftoken :place :client-place "&наклиенте" "&client" )
(deftoken :place :server-place "&насервере" "&server" )
