(in-package :smartos-test)

(defun run-capture (command)
  "Run COMMAND, returning trimmed stdout or NIL on failure."
  (handler-case
      (string-trim '(#\newline #\return #\space #\tab)
                   (uiop:run-program command
                                     :output :string
                                     :error-output :string))
    (error (condition)
      (declare (ignore condition))
      nil)))
