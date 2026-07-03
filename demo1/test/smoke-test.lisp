;;; Smoke tests for picker pagination and imgadm parsing.
;;; Run: sbcl --load demo1/test/smoke-test.lisp

(pushnew (uiop:pathname-directory-pathname
          (merge-pathnames "../" *load-pathname*))
         asdf:*central-registry*
         :test #'equal)

(asdf:load-system :smartos-test)

(defun assert-true (form message)
  (unless form
    (error "Assertion failed: ~A" message)))

(defun test-picker-pages ()
  (assert-true (= (smartos-test::picker-page-count 763 20) 39)
               "763 items at 20 per page => 39 pages")
  (assert-true (= (smartos-test::picker-last-page-start 763 20) 760)
               "last page starts at item 761")
  (assert-true (= (smartos-test::picker-page-count 20 20) 1)
               "exactly one page when count fits")
  (format t "picker page math: ok~%"))

(defun test-imgadm-parse ()
  (let* ((line "007feacd-1e4b-45ee-b61f-ffd6cca23dfd  canary  1.0.0  other  other  2006-01-02")
         (record (smartos-test::parse-imgadm-avail-line line)))
    (assert-true record "imgadm line parses")
    (assert-true (string= (smartos-test::image-record-name record) "canary")
                 "image name")
    (assert-true (string= (smartos-test::image-record-version record) "1.0.0")
                 "image version")
    (format t "imgadm parse: ok~%")))

(test-picker-pages)
(test-imgadm-parse)
(format t "All smoke tests passed.~%")
