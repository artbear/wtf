(defparser l-1C-script
  ((program startState) $1) 
  ((startState Переменные meths statement-list)
   (make-unit :vars $1 :meth $2 :stmt $3 :start $s :end $e))

  ((Переменные Переменные module-vars)  (append $1 $2))
  ((Переменные module-vars)   (list $1))
  ((Переменные ) nil)
  
  ((module-vars :var var-line :semicolon)   $2)

  ((var-line one-var)   (list $1))
  ((var-line var-line :comma one-var)   (append $1 (list $3)))

  ((export-decl  :export ) t) 
  ((export-decl  ) nil)
  
  ((one-var :identifier export-decl)
   (make-module-var-decl :name $1 :is-export $2 :start $s :end $e))

  ((meths function-decl) (list $1))
  ((meths meths function-decl) (append $1 (list $2)))
  ((meths ) nil)



  ((literal :number)
   (make-numeric-literal  :value $$1 :start $s :end $e))

  ((literal :string-literal)
   (make-string-literal  :value $$1 :start $s :end $e))

  ((literal :true)
   (make-special-value  :symbol 'true :start $s :end $e))
  ((literal :false)
   (make-special-value  :symbol 'false :start $s :end $e))
  ((literal :undefined)
   (make-special-value  :symbol 'undefined :start $s :end $e))
    ((literal :null)
   (make-special-value  :symbol 'null :start $s :end $e))
  
  
  ((function-decl
    :function :identifier :left-paren formal-parameter-list :right-paren export-decl ЛокальныеПеременные statement-list :endfunction)
   (make-function-decl :name $2 :parameters $4 :vars $7 :body $8 :start $s :end $e :is-export $6 :is-procedure nil))
  ((function-decl
    :procedure :identifier :left-paren formal-parameter-list :right-paren export-decl ЛокальныеПеременные statement-list :endprocedure)
   (make-function-decl :name $2 :parameters $4 :vars $7 :body $8 :start $s :end $e :is-export $6  :is-procedure t))


  ((formal-parameter-list parameter) (list $1))
  ((formal-parameter-list formal-parameter-list :comma parameter) (append $1 (list $3)))
  ((formal-parameter-list ) nil)

  ((parameter :identifier)
   (make-parameter :name $1 :start $s :end $e))
  ((parameter :val :identifier)
   (make-parameter :name $1 :byval t :start $s :end $e))
  ((parameter :identifier :equals literal)
   (make-parameter :name $1 :value $3 :start $s :end $e))
  ((parameter :val :identifier :equals literal)
   (make-parameter :name $1 :value $4 :byval t :start $s :end $e))


  ((ЛокальныеПеременные ЛокальныеПеременные local-vars) (append $1 $2))
  ((ЛокальныеПеременные local-vars) $1)
  ((ЛокальныеПеременные ) nil)
  
  ((local-vars :var local-var-line :semicolon) $2)

  ((local-var-line local-one-var) (list $1))
  ((local-var-line local-var-line :comma local-one-var)
   (append $1 (list $3)))

  ((local-one-var :identifier )
   (make-var-decl :name $1 :start $s :end $e ))

  ((statement-list ) (list ))
  ((statement-list statement) (list $1))
  ((statement-list statement-list :semicolon statement) (append $1 (list $3)))



  ((statement call-expression) $1)
  
  ((statement assignment-expression) $1)
  ((statement if-statement) $1)
  ((statement iteration-statement) $1)
  ((statement continue-statement) $1)
  ((statement break-statement) $1)
  ((statement return-statement) $1)
  ((statement raise-statement) $1)
  ((statement labelled-statement) $1)
  ((statement try-statement) $1)
  ((statement IfStatement) $1)
  ((statement ) nil)


  ((try-statement :try statement-list :except statement-list :endtry)
   (make-try :body  $2 :catch-clause $4 :start $s :end $e))

  ((raise-statement :throw  expression ) 
   (make-raise-statement :value $2 :start $s :end $e))
  
  ((raise-statement :throw ) 
   (make-raise-statement :start $s :end $e))

  ((IfStatement :if expression :then statement-list :EndIf ) 
   (make-if-statement  :condition $2 :then-statement (make-statement-block :statements $4) ))
  ((IfStatement :if expression :then statement-list CaseBlock :EndIf ) 
   (make-if-statement  :condition $2
                       :then-statement (make-statement-block :statements $4)
                       :else-statement (make-statement-bock :statements $5) ))

  ((IfStatement :if expression :then statement-list DefaultClause :endif )
   (make-if-statement  :condition $2
                       :then-statement (make-statement-block :statements $4)
                       :else-statement (make-statement-block :statements $5) ))

  
  ((CaseBlock CaseClauses) (list $1))
  ((CaseBlock CaseClauses DefaultClause) (append $1 (list $2)))
 
  ((CaseClauses CaseClause) (list $1))
  ((CaseClause  CaseClauses CaseClause) (append $1 (list $2)))
 
  ((CaseClause :elsif expression :then statement-list)
   (make-if-statement  :condition $2 :then-statement $4 ))

  ((DefaultClause :else statement-list)
   (make-statement-block :statements $2))
  
  
  ((iteration-statement :while expression :do statement-list :enddo) 
   (make-while :condition $2 :body $4 :start $s :end $e))
  ((iteration-statement :for :each :identifier :in expression  :do statement-list :enddo)
   (make-for-in :binding  $3 :collection $5   :body $7 :start $s :end $e))
  ((iteration-statement :for :identifier :equals expression :to expression :do statement-list :enddo)
   (make-for :variable  $2 :initializer $4  :condition $6  :body $8 :start $s :end $e))
  
  ((continue-statement :continue ) 
   (make-continue-statement :start $s :end $e))
  
  ((break-statement :break ) 
   (make-break-statement :start $s :end $e))
  
  ((return-statement :return expression) 
   (make-return-statement :arg $2 :start $s :end $e))
  ((return-statement :return) 
   (make-return-statement :arg nil :start $s :end $e))
  
  
  ((call-expression :identifier)
   (make-property-access :target 'find-scope :field $1  :start $s :end $e))
  ((call-expression call-expression arguments)
   (make-fn-call :fn $1 :args $2 :start $s :end $e))
  ((call-expression call-expression :dot call-expression) 
   (make-property-access :target $1 :field $3  :start $s :end $e))
  ((call-expression call-expression :left-bracket expression :right-bracket)
   (make-array-access :array-base $1 :array-offset $3 :start $s :end $e))




  ((new-expression :new arguments) (make-new-expr :constructor 'in-string :args $2 :start $s :end $e))
  ((new-expression :new  :identifier arguments) (make-new-expr :constructor $2 :args $2 :start $s :end $e))


  ((arguments :left-paren argument-list :right-paren) $2)

  ((argument-list ) (list ))
  ((argument-list arg) (list $1))
  ((argument-list argument-list :comma arg) (append $1 (list $3)))
  
  ((arg expression) $1)
  ((arg ) (list ))

  
  ((unary-expression call-expression) $1)
  ((unary-expression new-expression) $1)
  ((unary-expression :left-paren expression  :right-paren)
   (make-unary-op :op-symbol 'paren :arg $2))
  ((unary-expression :hook :left-paren logical-OR-expression :comma expression :comma expression :right-paren)
   (make-conditional :condition $1 :true-arg $3 :false-arg $5 ))
  ((unary-expression literal) $1)
  ((unary-expression :plus unary-expression)  (make-unary-operator :op-symbol :unary-plus :arg $2))
  ((unary-expression :minus unary-expression) (make-unary-operator :op-symbol :unary-minus :arg $2))
  ((unary-expression :not unary-expression)   (make-unary-operator :op-symbol :logical-not :arg $2))
  
  ((multiplicative-expression unary-expression) $1)
  ((multiplicative-expression multiplicative-expression :asterisk unary-expression) 
   (make-binary-operator :op-symbol :multiply :left-arg $1 :right-arg $3 ))
  ((multiplicative-expression multiplicative-expression :slash unary-expression) 
   (make-binary-operator :op-symbol :divide :left-arg $1 :right-arg $3 ))
  ((multiplicative-expression multiplicative-expression :percent unary-expression) 
   (make-binary-operator :op-symbol :modulo :left-arg $1 :right-arg $3 ))

  ((additive-expression multiplicative-expression) $1)
  ((additive-expression additive-expression :plus multiplicative-expression) 
   (make-binary-operator :op-symbol :add :left-arg $1 :right-arg $3 ))
  ((additive-expression additive-expression :minus multiplicative-expression) 
   (make-binary-operator :op-symbol :subtract :left-arg $1 :right-arg $3))

  ((relational-expression additive-expression) $1)
  ((relational-expression relational-expression :equals additive-expression) 
   (make-binary-operator :op-symbol :equals :left-arg $1 :right-arg $3 ))
  ((relational-expression relational-expression :less-than additive-expression) 
   (make-binary-operator :op-symbol :less-than :left-arg $1 :right-arg $3 ))
  ((relational-expression relational-expression :less-than-equals additive-expression) 
   (make-binary-operator :op-symbol :less-than-equals :left-arg $1 :right-arg $3 ))
  ((relational-expression relational-expression :greater-than additive-expression) 
   (make-binary-operator :op-symbol :greater-than :left-arg $1 :right-arg $3 ))
  ((relational-expression relational-expression :greater-than-equals additive-expression) 
   (make-binary-operator :op-symbol :greater-than-equals :left-arg $1 :right-arg $3 ))


  ((equality-expression relational-expression) $1)
  ((equality-expression equality-expression :equals relational-expression) 
   (make-binary-operator :op-symbol :equals :left-arg $1 :right-arg $3 ))
  ((equality-expression equality-expression :not-equals relational-expression) 
   (make-binary-operator :op-symbol :not-equals :left-arg $1 :right-arg $3 ))


  ((logical-AND-expression  equality-expression) $1)
  ((logical-AND-expression logical-AND-expression :and  equality-expression) 
   (make-binary-operator :op-symbol :logical-AND :left-arg $1 :right-arg $3 ))


  ((logical-OR-expression logical-AND-expression) $1)
  ((logical-OR-expression logical-OR-expression :or logical-AND-expression) 
   (make-binary-operator :op-symbol :logical-OR :left-arg $1 :right-arg $3 ))

  ((conditional-expression logical-OR-expression) $1)

  ((assignment-expression call-expression :equals expression )
   (make-assign  :lhs $1 :rhs $3 ))
  
  ((expression conditional-expression) $1)
)
