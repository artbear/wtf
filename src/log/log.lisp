(in-package :logger.wtf)

(cl-log:defcategory :lexer (or :lexer.debug :lexer.info :lexer.warn))
(cl-log:defcategory :lexer-rule (or :lexer-rule.debug :lexer-rule.info :lexer-rule.warn))
(cl-log:defcategory :wtf)

(defun launch-logger (&optional (log-dir ""))
  "Start logging facility.  Create log-dir if necessary."
  (when config.wtf:*debug-log*
    (flet ((start-log-messenger (name-keyword)
             (cl-log:start-messenger
              'cl-log:text-file-messenger
              :name name-keyword
              :filename (make-pathname
                         :directory (pathname-directory
                                     (ensure-directories-exist
                                      (pathname log-dir)))
                         :name (string-downcase name-keyword)
                         :type "log")
              :category name-keyword)))

      (setf (cl-log:log-manager)
            (make-instance 'cl-log:log-manager))
      (values
       (start-log-messenger :lexer)
       (start-log-messenger :lexer-rule)))))



(launch-logger config.wtf:*log-dir*)
