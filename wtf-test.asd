(asdf:defsystem #:wtf-test-asdf
  :depends-on (#:cl-ppcre #:cl-log #:local-time #:wtf-asdf)
  :serial t
  :components
  ((:file "config")
   (:module :test
            :depends-on ("config")
            :serial t
            :components
             ((:file "lexer-test")
	    (:file "preprocessor-test")
             (:file "main")))))
  
