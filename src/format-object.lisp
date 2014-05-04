(in-package :wtf)

(defmethod format-object (what value stream)
  (format stream "GENERIC what = ~a value = ~a" what value ))
  

(defmethod format-object (what  (value list) stream)
  (format stream "<list>")
  (dolist (item value)
    (format-object what item stream))
  (format stream "</list>"))

(defmethod format-object ((what (eql 'xml)) (value 1C-FILE-ENTRY) stream)
  (format stream "<?xml version=\"1.0\" encoding=\"UTF-8\"?> ")
  (format stream "<file>")
  (format stream "<name>~a</name>" (slot-value value 'filename))
  (format stream "<rules>")
  (loop for key being the hash-keys of (slot-value value 'rule-lexer-result)
     using (hash-value token-list)
     do (progn
          (format-object what key stream)
          (format-object what token-list stream)))
  (format stream "</rules>")
  (format stream "</file>"))

(defmethod format-object ((what (eql 'xml)) (value rule) stream)
  (format stream "<rule>")
  (format stream "<name>~a</name>" (rule-name value))
  (format stream "<description>~a</description>" (rule-description-rule value))
  (format stream "</rule>"))


(defmethod format-object ((what (eql 'xml)) (value token) stream)
  (format stream "<token>")
  (format stream "<type>~a</type>" (token-type value))
  (format stream "<text>~a</text>" (token-text value))
  (format stream "<start>")
  (format-object what (token-start-point value) stream)
  (format stream "</start>")
  (format stream "<end>")
  (format-object what (token-end-point value) stream)
  (format stream "</end>")
  (format stream "</token>"))


(defmethod format-object ((what (eql 'xml)) (value codepoint) stream)
  (format stream "<point>")
  (format stream "<index>~a</index>" (codepoint-index value))
  (format stream "<line>~a</line>" (codepoint-line-number value))
  (format stream "<char>~a</char>" (codepoint-char value))
  (format stream "</point>"))

  
  
