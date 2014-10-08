(in-package :wtf)

(defun write-escaped (string )
  (replace-all
   (replace-all
    (replace-all
     (replace-all
      (replace-all string "<" "&lt;")
      ">" "&gt;")
     "&" "&amp;")
    "'" "&apos;")
   "\"" "&quot;"))


(defmethod format-object (what value stream)
  (format stream "GENERIC what = ~a value = ~a" what value ))
  
(defmethod format-object ((what (eql 'xml)) (value symbol) stream)
  (format stream "<Symbol>~a</Symbol>" value))
  
(defmethod format-object ((what (eql 'xml)) (value hash-table) stream)
  (format stream "<HashTable>")
  (loop for key being the hash-keys of value
     using (hash-value hash-value)
     do (progn
          (format stream "<Item>")
          (format stream "<Key>")
          (format-object what key stream)
          (format stream "</Key>")
          (format stream "<Value>")
          (format-object what hash-value stream)
          (format stream "</Value>")
          (format stream "</Item>")))
  (format stream "</HashTable>"))

(defmethod format-object (what  (value list) stream)
  (format stream "<List>")
  (dolist (item value)
    (format-object what item stream))
  (format stream "</List>"))

(defmethod format-object ((what (eql 'xml)) (value file-entry) stream)
  (format stream "<?xml version=\"1.0\" encoding=\"UTF-8\"?> ")
  (format stream "<file>")
  (format stream "<name>~a</name>" (file-entry-filename value))
  (format stream "<lexeme>")
  (format-object what  (file-entry-token-list value ) stream)
  (format stream "</lexeme>")
  (format stream "<rules>")
  (format-object what  (file-entry-rule-result value) stream)
  (format stream "</rules>")
  (format stream "</file>"))

(defmethod format-object ((what (eql 'xml)) (value rule) stream)
  (format stream "<rule>")
  (format stream "<name>~a</name>" (rule-name value))
  (format stream "<description>~a</description>" (rule-description-rule value))
  (format stream "</rule>"))


(defmethod format-object ((what (eql 'xml)) (value token) stream)
  (format stream "<token id=\"~a\">" (token-id value))
  (format stream "<type>~a</type>" (token-type value))
  (format stream "<text>~a</text>" (write-escaped (token-text value)))
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

  
  
