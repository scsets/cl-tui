(in-package :smartos-test)

(defun draw-header (cols)
  (format t "~A ~A -- demo operator console~%" *app-title* *version*)
  (draw-horizontal-rule cols)
  (terpri))

(defun draw-status-line (cols rows message)
  (terminal-move rows 1)
  (format t "~A~%" (subseq (concatenate 'string message (make-string cols :initial-element #\space))
                           0 (max 0 (1- cols))))
  (finish-output))

(defun run-menu-screen (title items &key footer)
  "ITEMS is a list of (NUMBER . LABEL) conses. Returns the chosen number string."
  (loop
     (multiple-value-bind (cols rows)
         (terminal-size)
       (terminal-clear)
       (draw-header cols)
       (when title
         (format t "~A~%~%" title))
       (loop for (num . label) in items
             do (format t "  ~A. ~A~%" num label))
       (terpri)
       (format t "  0. Back~%")
       (when footer
         (terpri)
         (format t "~A~%" footer))
       (terpri)
       (draw-status-line cols rows "0=Back  Enter=choose"))
     (let ((choice (read-menu-choice
                    :valid (cons "0" (mapcar #'car items)))))
       (unless (string= choice "0")
         (return choice)))))

(defun show-command-then-run (command &key description)
  "SMIT-style transparency: show the command, run it, return output text."
  (let* ((cmdline (format nil "~{~A~^ ~}" command))
         (banner (or description (format nil "Running: ~A" cmdline))))
    (format t "~A~%" banner)
    (format t "~A~%~%" cmdline)
    (finish-output)
    (handler-case
        (uiop:run-program command
                          :output :string
                          :error-output :string)
      (error (c)
        (format nil "Command failed: ~A" c)))))

(defun paginate-text (text cols rows)
  "Show TEXT in pages sized for the terminal body."
  (let* ((body-rows (max 8 (- rows 6)))
         (lines (uiop:split-string text :separator '(#\newline)))
         (page-size body-rows))
    (loop for start from 0 below (length lines) by page-size
          for page from 1
          do (progn
               (terminal-clear)
               (draw-header cols)
               (format t "Output (page ~D)~%~%" page)
               (loop for line in (subseq lines start (min (+ start page-size) (length lines)))
                     do (format t "~A~%" line))
               (when (< (+ start page-size) (length lines))
                 (wait-for-enter "Press Enter for next page...")))
          finally (wait-for-enter))))
