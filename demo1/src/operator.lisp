(in-package :smartos-test)

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
