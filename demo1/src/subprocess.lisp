(in-package :smartos-test)

(defun run-capture (command)
  "Run COMMAND, returning trimmed stdout or NIL on failure.

Child stdin is closed. SmartOS tools such as vmadm read stdin and block
when it is inherited from an interactive terminal session."
  (handler-case
      (string-trim '(#\newline #\return #\space #\tab)
                   (uiop:run-program command
                                     :input '(:string "")
                                     :output :string
                                     :error-output nil))
    (error (condition)
      (declare (ignore condition))
      nil)))
