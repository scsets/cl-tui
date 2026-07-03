(in-package :smartos-test)

(defconstant +min-cols+ 80)
(defconstant +min-rows+ 24)

(defun default-terminal-size ()
  (values +min-cols+ +min-rows+))

(defun normalize-terminal-size (cols rows)
  (values (max cols +min-cols+) (max rows +min-rows+)))

(defun terminal-size-from-env ()
  "Use LINES and COLUMNS when stty is unavailable."
  (let ((lines (uiop:getenv "LINES"))
        (cols (uiop:getenv "COLUMNS")))
    (when (and lines cols)
      (handler-case
          (let ((rows (parse-integer lines :junk-allowed nil))
                (cols-val (parse-integer cols :junk-allowed nil)))
            (when (and (plusp rows) (plusp cols-val))
              (normalize-terminal-size cols-val rows)))
        (error (condition)
          (declare (ignore condition))
          nil)))))

(defun terminal-size-from-stty ()
  "Query terminal size via stty. Output is rows columns."
  (handler-case
      (let* ((line (string-trim '(#\space #\tab #\newline #\return)
                                (uiop:run-program '("/usr/bin/stty" "size")
                                                  :input '(:string "")
                                                  :output :string
                                                  :error-output :string)))
             (parts (uiop:split-string line)))
        (when (= (length parts) 2)
          (let ((rows (parse-integer (first parts) :junk-allowed nil))
                (cols (parse-integer (second parts) :junk-allowed nil)))
            (when (and rows cols (plusp rows) (plusp cols))
              (normalize-terminal-size cols rows)))))
    (error (condition)
      (declare (ignore condition))
      nil)))

(defun terminal-size ()
  "Return (COLS ROWS), floored at 80x24."
  (multiple-value-bind (cols rows)
      (multiple-value-call #'values (terminal-size-from-env))
    (if (and cols rows)
        (values cols rows)
        (multiple-value-bind (cols rows)
            (multiple-value-call #'values (terminal-size-from-stty))
          (if (and cols rows)
              (values cols rows)
              (default-terminal-size))))))

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
