(in-package :smartos-test)

(defun run-capture (command)
  "Run COMMAND, returning trimmed stdout or NIL on failure.

Child stdin is redirected to the null device. SmartOS tools such as vmadm
read stdin and block when it is inherited from an interactive terminal.
Use :input nil (UIOP null device on Unix), not (:string \"\"), which
breaks on LispWorks 7.1."
  (handler-case
      (let ((text (uiop:run-program command
                                     :input nil
                                     :output :string
                                     :error-output nil)))
        (when (and text (plusp (length text)))
          (string-trim '(#\newline #\return #\space #\tab) text)))
    (error (condition)
      (declare (ignore condition))
      nil)))
