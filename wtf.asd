(asdf:defsystem #:wtf-asdf
  :depends-on (#:cl-ppcre #:cl-log #:local-time )
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
             (:file "macros-lexer")
             (:file "lexer")
             (:file "lexer-rule")
             (:file "files")
             (:file "format-object")
             (:file "main")
))))
