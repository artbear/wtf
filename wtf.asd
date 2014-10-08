(asdf:defsystem #:wtf-asdf
  :depends-on (#:cl-ppcre #:cl-log #:local-time #:yacc)
  :serial t
  :components
  ((:file "config")
   (:module :src
            :depends-on ("config")
            :serial t
            :components
            ((:file "package")
             (:file "util")
             (:file "log")
             (:file "tokens")
             (:file "structs")
             (:file "lexer-dso")
             (:file "macros-parser")
             (:file "function-lexer")
             (:file "rules")
             (:file "lexer-rule")
             (:file "files")
             (:file "format-object")
             (:file "parse-yacc")
             (:file "preprocessor-parser")
             (:file "main")))))
