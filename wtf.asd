(asdf:defsystem #:wtf-asdf
  :depends-on (#:cl-ppcre #:cl-log #:local-time )
  :serial t
  :components
  ((:file "config")
   (:module :src
            :depends-on ("config")
            :serial t
            :components
            ((:module :log :components
                      ((:file "package")
                       (:file "log")))
             (:module :util
                      :components ((:file "package")
                                   (:file "util")))
             (:module :lexer
                      :depends-on (:log :util)
                      
                      :serial t
                      :components ((:file "package" )
                                   (:file "tokens")
                                   (:file "lexer")))
             (:module :lexer-rule
                      :components ((:file "package")
                                   (:file "lexer-rule")))
             (:file "package")
             (:file "main")))))
  
