wtf
===

Формализованная проверка кода на языке 1С. 
ПОзволяет 
1. Анализровать токен
2. Анализировать список токенов

В планах
1. Построение AST
2. Анализ AST
3. Генератор документации по токенам, AST и примененным правилам



Пользуясь случаем - все участникам конференции lisp@conference.jabber.ru - парни, огромное спасибо за терпение и ответы на мои вопросы



Запуск - 

sbcl --load run.lisp --quit

В файле congig.lisp выставить *base-dir* в директорию куда склонировали репозиторий


в файле doc/doc.org - попытка описания