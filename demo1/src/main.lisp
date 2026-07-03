(in-package :smartos-test)

(defun main (&optional argv)
  (declare (ignore argv))
  (format t "~A ~A~%" *app-title* *version*)
  (command-loop)
  (format t "~%Goodbye.~%")
  (finish-output))
