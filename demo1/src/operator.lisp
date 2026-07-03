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
                          :input nil
                          :output :string
                          :error-output nil)
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

(defun numeric-choice-p (text)
  (and (> (length text) 0)
       (every (lambda (ch) (char<= #\0 ch #\9)) text)))

(defun read-picker-choice (&optional (prompt "Enter choice: "))
  (string-downcase (read-line-trimmed prompt)))

(defun picker-body-rows (rows)
  "Rows available for numbered items after header, title, and footer."
  (max 5 (- rows 10)))

(defun picker-page-count (item-count page-size)
  (max 1 (ceiling item-count (max 1 page-size))))

(defun picker-last-page-start (item-count page-size)
  (* (1- (picker-page-count item-count page-size)) page-size))

(defun pick-numbered-item (items &key title (label #'identity) (prompt "Enter choice: "))
  "Show ITEMS with global numbers, paginated by terminal height.

Accepts:
  0       cancel
  1..N    select that item (from any page)
  N/next  next page
  P/prev  previous page"
  (when items
    (let* ((count (length items))
           (page-start 0))
      (loop
         (multiple-value-bind (cols rows)
             (terminal-size)
           (let* ((page-size (picker-body-rows rows))
                  (page-end (min count (+ page-start page-size)))
                  (page-index (1+ (floor page-start page-size)))
                  (page-count (picker-page-count count page-size)))
             (terminal-clear)
             (draw-header cols)
             (format t "~A~%~%" (or title "Select an item"))
             (format t "~D items. Page ~D of ~D.~%~%"
                     count page-index page-count)
             (loop for item in (subseq items page-start page-end)
                   for n from (1+ page-start)
                   do (format t "  ~D. ~A~%" n (funcall label item)))
             (terpri)
             (format t "  0. Cancel~%")
             (when (< page-end count)
               (format t "  N. Next page~%"))
             (when (plusp page-start)
               (format t "  P. Previous page~%"))
             (terpri)
             (draw-status-line cols rows
                               "0=Cancel  number=select  N/P=page")))
         (let ((choice (read-picker-choice prompt)))
           (multiple-value-bind (cols rows)
               (terminal-size)
             (let ((page-size (picker-body-rows rows)))
               (cond
                 ((string= choice "0")
                  (return nil))
                 ((member choice '("n" "next") :test #'string=)
                  (if (< (+ page-start page-size) count)
                      (setf page-start (min (+ page-start page-size)
                                            (picker-last-page-start count page-size)))
                      (progn (terminal-bell)
                             (format t "Already on the last page.~%"))))
                 ((member choice '("p" "prev") :test #'string=)
                  (if (plusp page-start)
                      (setf page-start (max 0 (- page-start page-size)))
                      (progn (terminal-bell)
                             (format t "Already on the first page.~%"))))
                 ((numeric-choice-p choice)
                  (let ((n (parse-integer choice)))
                    (if (and (>= n 1) (<= n count))
                        (return (nth (1- n) items))
                        (progn (terminal-bell)
                               (format t "Enter a number from 1 to ~D.~%" count)))))
                 (t (terminal-bell)
                    (format t "Invalid choice.~%"))))))))))
