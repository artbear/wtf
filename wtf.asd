(asdf:defsystem #:config.wtf-asdf
  :depends-on (#:cl-ppcre #:cl-log #:local-time )
  :components ((:file "config")))

(asdf:defsystem #:log.wtf-asdf
  :depends-on (#:cl-ppcre #:cl-log #:local-time #:config.wtf-asdf)
  :components 
  ((:module :src
           :components
           ((:module :log
                     :components ((:file "package")
                                  (:file "log")))))))


(asdf:defsystem #:util.wtf-asdf
  :depends-on (#:log.wtf-asdf)
  :components
  ((:module :src
           :components
           ((:module :util
                    :components ((:file "package")
                                 (:file "util")))))))

(asdf:defsystem #:lexer.wtf-asdf
  :depends-on (#:cl-ppcre #:util.wtf-asdf)
  :components
  ((:module :src
           :components
           ((:module :lexer
                    :components ((:file "package" )
                                 (:file "tokens")
                                 (:file "lexer")))))))

(asdf:defsystem #:lexer-rule.wtf-asdf
  :depends-on (#:cl-ppcre #:cl-log #:local-time #:config.wtf-asdf #:log.wtf-asdf #:util.wtf-asdf #:lexer.wtf-asdf)
  :components 
  ((:module :src
           :components
           ((:module :lexer-rule
                    :components ((:file "package")
                                 (:file "lexer-rule")))))))

(asdf:defsystem #:wtf-asdf
  :depends-on (#:cl-ppcre #:cl-log #:local-time #:config.wtf-asdf #:log.wtf-asdf #:util.wtf-asdf #:lexer.wtf-asdf
                          #:lexer-rule.wtf-asdf)
  :components 
  ((:module :src
           :components
           ((:file "package")
            (:file "main")))))
  
