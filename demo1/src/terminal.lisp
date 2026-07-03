(in-package :smartos-test)

(defconstant +min-cols+ 80)
(defconstant +min-rows+ 24)

(defun terminal-size ()
  "Return (COLS ROWS), floored at 80x24."
  (multiple-value-bind (cols rows)
      (ignore-errors
        (let* ((line (string-trim '(#\space #\tab #\newline #\return)
                                  (uiop:run-program '("stty" "size")
                                                    :output :string
                                                    :error-output nil)))
               (parts (uiop:split-string line)))
          (when (= (length parts) 2)
            (values (parse-integer (first parts) :junk-allowed t)
                    (parse-integer (second parts) :junk-allowed t)))))
    (values (max (or cols +min-cols+) +min-cols+)
            (max (or rows +min-rows+) +min-rows+))))

(defun terminal-clear ()
  (format t "~C[2J~C[H" #\Escape #\Escape)
  (finish-output))

(defun terminal-bell ()
  (format t "~C" #\Bell)
  (finish-output))

(defun terminal-move (row col)
  (format t "~C[~D;~DH" #\Escape row col)
  (finish-output))

(defun draw-horizontal-rule (cols)
  (format t "~A~%" (make-string (max 0 (- cols 1)) :initial-element #\-)))
