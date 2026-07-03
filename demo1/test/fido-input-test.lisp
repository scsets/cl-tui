(require :asdf)

(defun try-get (label &rest keys)
  (format t "~A: " label)
  (finish-output)
  (handler-case
      (let ((text (apply #'uiop:run-program
                         '("/usr/sbin/vmadm" "get" "3a8fcbe5-1248-4abf-9afc-ea55aa8202c5")
                         keys)))
        (format t "ok len=~D~%" (length text))
        t)
    (error (e)
      (format t "FAIL ~A~%" e)
      nil)))

(try-get "get-no-input" :output :string :error-output nil)
(try-get "get-input-nil" :input nil :output :string :error-output nil)
(try-get "get-input-string" :input '(:string "") :output :string :error-output nil)

(format t "~%list with input nil: ~D~%"
        (length (uiop:run-program '("/usr/sbin/vmadm" "list")
                                  :input nil
                                  :output :string
                                  :error-output nil)))
(quit)
