(in-package :wtf)

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
       (start-log-messenger :wtf)))))



(launch-logger config.wtf:*log-dir*)

