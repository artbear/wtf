(in-package :wtf)

(defmethod format-object (what value)
  (format nil "GENERIC what = ~a value = ~a" what value ))
  
(defmethod format-object ((what (eql 'log)) (value string))
  (replace-all
   (replace-all value (string #\NewLine) "#n" ) (string #\Return) "#r"))
  
(defmethod format-object ((what (eql 'log)) (value codepoint))
  (format nil "(~a,~a,~a)" (codepoint-index value) (codepoint-line-number value) (codepoint-char value)))

(defmethod format-object ((what (eql 'log)) (value token))
  (when config.wtf:*debug-log*
    (format nil "Token id = '~a' type = '~a' value = '~a' start = '~a' end = '~a'"
            (token-id value) (token-type value)
            (format-object 'log (token-text value))
            (format-object 'log (token-start-point value))
            (format-object 'log (token-end-point value)))))



(defmethod format-object ((what (eql 'xml)) (value token))
  (when config.wtf:*debug-log*
    (format nil "<token id = '~a' type = '~a' value = '~a' >
<start> ~a </start>
<end>~a</end>"
            (token-id value) (token-type value)
             (token-text value)
            (format-object 'xml (token-start-point value))
            (format-object 'xml (token-end-point value)))))

(defmethod format-object ((what (eql 'xml)) (value codepoint))
  (format nil "<index>~a</index><line>~a</linse><char>~a</char>" (codepoint-index value) (codepoint-line-number value) (codepoint-char value)))

