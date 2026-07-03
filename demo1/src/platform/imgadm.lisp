(in-package :smartos-test)

(defparameter +imgadm-avail-cmd+ '("/usr/sbin/imgadm" "avail"))

(defun split-fields (line)
  (remove-if (lambda (s) (zerop (length s)))
             (uiop:split-string line :separator '(#\space #\tab))))

(defun parse-imgadm-avail-line (line)
  "Parse one imgadm avail row. Returns an IMAGE-RECORD or NIL.

imgadm uses fixed columns; image names may contain spaces, so the tail
fields (version, os, type, published date) are taken from the end."
  (let ((fields (split-fields line)))
    (when (and (>= (length fields) 6)
               (>= (length (first fields)) 8)
               (not (char= (char (first fields) 0) #\-)))
      (let* ((n (length fields))
             (uuid (first fields))
             (published (nth (1- n) fields))
             (type (nth (- n 2) fields))
             (os (nth (- n 3) fields))
             (version (nth (- n 4) fields))
             (name (format nil "~{~A~^ ~}" (subseq fields 1 (- n 4)))))
        (make-image-record
         :uuid uuid
         :name name
         :version version
         :os os
         :type type
         :published published)))))

(defun parse-imgadm-avail (text)
  (loop for line in (uiop:split-string text :separator '(#\newline))
        for record = (parse-imgadm-avail-line line)
        when record
          collect record))

(defun list-available-images ()
  (let ((text (run-capture +imgadm-avail-cmd+)))
    (if text
        (parse-imgadm-avail text)
        nil)))

(defun image-picker-label (image)
  (format nil "~A  ~A  ~A  ~A"
          (image-record-name image)
          (image-record-version image)
          (image-record-os image)
          (image-record-type image)))

(defun imgadm-avail-count-text (images)
  (format nil "~D images available from imgadm.~%"
          (length images)))
