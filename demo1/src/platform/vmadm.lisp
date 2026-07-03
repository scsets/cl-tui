(in-package :smartos-test)

(defparameter +vmadm-list-cmd+ '("/usr/sbin/vmadm" "list"))

(defun platform-run-capture (command)
  (ignore-errors
    (uiop:run-program command
                      :output :string
                      :error-output nil)))

(defun split-fields (line)
  (remove-if (lambda (s) (zerop (length s)))
             (uiop:split-string line :separator '(#\space #\tab))))

(defun parse-vmadm-list-line (line)
  "Parse one vmadm list row. Returns a VM-RECORD or NIL."
  (let ((fields (split-fields line)))
    (when (and (>= (length fields) 4)
               (>= (length (first fields)) 8)
               (not (char= (char (first fields) 0) #\-)))
      (destructuring-bind (uuid name state brand . rest) fields
        (make-vm-record
         :uuid uuid
         :name name
         :state state
         :brand brand
         :ram (or (first rest) "")
         :disk (or (second rest) ""))))))

(defun parse-vmadm-list (text)
  "Return a list of VM-RECORD structs from vmadm list output."
  (loop for line in (uiop:split-string text :separator '(#\newline))
        for record = (parse-vmadm-list-line line)
        when record
          collect record))

(defun list-vms ()
  "Run vmadm list and return VM records."
  (let ((text (platform-run-capture +vmadm-list-cmd+)))
    (if text
        (parse-vmadm-list text)
        nil)))

(defun pad-field (text width)
  (let ((s (subseq (concatenate 'string text (make-string width :initial-element #\space))
                   0 width)))
    s))

(defun vm-list-table-text (records)
  (if (null records)
      (format nil "No virtual machines found, or ~A is unavailable on this host.~%~%"
              (format nil "~{~A~^ ~}" +vmadm-list-cmd+))
      (with-output-to-string (out)
        (flet ((print-row (uuid name state brand ram disk)
                 (format out "~A ~A ~A ~A ~A ~A~%"
                         (pad-field uuid 36)
                         (pad-field name 16)
                         (pad-field state 10)
                         (pad-field brand 10)
                         (pad-field ram 8)
                         (pad-field disk 8))))
          (print-row "UUID" "NAME" "STATE" "BRAND" "RAM" "DISK")
          (loop for record in records
                do (print-row (vm-record-uuid record)
                              (vm-record-name record)
                              (vm-record-state record)
                              (vm-record-brand record)
                              (vm-record-ram record)
                              (vm-record-disk record)))))))
