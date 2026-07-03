(in-package :smartos-test)

(defun read-line-trimmed (&optional (prompt nil))
  (when prompt
    (format t "~A" prompt)
    (finish-output))
  (string-trim '(#\space #\tab)
               (or (read-line *standard-input* nil "") "")))

(defun read-menu-choice (&key valid)
  "Read one menu token. VALID is a list of strings such as (\"0\" \"1\" \"2\")."
  (loop
     (let ((choice (read-line-trimmed "Enter choice: ")))
       (when (member choice valid :test #'string=)
         (return choice))
       (terminal-bell)
       (format t "Invalid choice. Use one of: ~{~A~^, ~}.~%"
               valid))))

(defun wait-for-enter (&optional (prompt "Press Enter to continue..."))
  (format t "~%~A" prompt)
  (finish-output)
  (read-line *standard-input* nil)
  (values))
