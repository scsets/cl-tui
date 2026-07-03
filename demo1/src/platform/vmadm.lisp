(in-package :smartos-test)

(defparameter +vmadm-list-cmd+ '("/usr/sbin/vmadm" "list"))

(defun split-fields (line)
  (remove-if (lambda (s) (zerop (length s)))
             (uiop:split-string line :separator '(#\space #\tab))))

(defun parse-vmadm-list-line (line)
  "Parse one vmadm list row. Returns a VM-RECORD or NIL.

Format: UUID TYPE RAM STATE ALIAS"
  (let ((fields (split-fields line)))
    (when (and (>= (length fields) 5)
               (>= (length (first fields)) 8)
               (not (char= (char (first fields) 0) #\-)))
      (destructuring-bind (uuid type ram state alias) fields
        (make-vm-record
         :uuid uuid
         :name alias
         :state state
         :type type
         :brand type
         :ram ram)))))

(defun parse-vmadm-list (text)
  "Return a list of VM-RECORD structs from vmadm list output."
  (loop for line in (uiop:split-string text :separator '(#\newline))
        for record = (parse-vmadm-list-line line)
        when record
          collect record))

(defun list-vms ()
  "Run vmadm list and return VM records."
  (let ((text (run-capture +vmadm-list-cmd+)))
    (if text
        (parse-vmadm-list text)
        nil)))

(defun vmadm-get-cmd (uuid)
  (list "/usr/sbin/vmadm" "get" uuid))

(defun get-vm-json (uuid)
  (run-capture (vmadm-get-cmd uuid)))

(defun json-skip-ws (json pos)
  (loop while (and json (< pos (length json)))
        for ch = (char json pos)
        when (member ch '(#\space #\tab #\newline #\return))
        do (incf pos)
        finally (return pos)))

(defun json-read-scalar (json start)
  "Read one JSON scalar starting at START. Returns (VALUE END-POS)."
  (let ((pos (json-skip-ws json start)))
    (cond
      ((>= pos (length json))
       (values nil pos))
      ((char= (char json pos) #\")
       (let ((end (position #\" json :start (1+ pos))))
         (if end
             (values (subseq json (1+ pos) end) (1+ end))
             (values nil (length json)))))
      (t
       (let ((end (or (position #\, json :start pos)
                      (position #\} json :start pos)
                      (position #\] json :start pos)
                      (length json))))
         (values (string-trim '(#\space #\tab #\newline #\return)
                              (subseq json pos end))
                 end))))))

(defun json-field-needle (key)
  (format nil "~A~A~A~A" (code-char 34) key (code-char 34) (code-char 58)))

(defun json-top-level-field (json key)
  "Return the string form of a top-level JSON field value, or NIL."
  (when json
    (let ((needle (json-field-needle key)))
      (let ((pos (search needle json)))
        (when pos
          (json-read-scalar json (+ pos (length needle))))))))

(defun vm-ram-from-json (json record)
  (or (json-top-level-field json "ram")
      (json-top-level-field json "max_physical_memory")
      (vm-record-ram record)
      "Unknown"))

(defun build-vm-detail (record json)
  (make-vm-detail
   :uuid (vm-record-uuid record)
   :name (or (json-top-level-field json "alias") (vm-record-name record))
   :hostname (or (json-top-level-field json "hostname") "Unknown")
   :state (vm-record-state record)
   :brand (or (json-top-level-field json "brand") (vm-record-type record))
   :type (vm-record-type record)
   :ram-mb (vm-ram-from-json json record)
   :vcpus (or (json-top-level-field json "vcpus") "")
   :image-uuid (or (json-top-level-field json "image_uuid") "Unknown")
   :created (or (json-top-level-field json "create_timestamp") "Unknown")))

(defun lookup-vm-detail (record)
  "Fetch full settings for RECORD via vmadm get."
  (let ((json (get-vm-json (vm-record-uuid record))))
    (if json
        (build-vm-detail record json)
        (make-vm-detail
         :uuid (vm-record-uuid record)
         :name (vm-record-name record)
         :state (vm-record-state record)
         :brand (vm-record-type record)
         :type (vm-record-type record)
         :ram-mb (vm-record-ram record)
         :hostname "Unknown"
         :vcpus ""
         :image-uuid "Unknown"
         :created "Unknown"))))

(defun pad-field (text width)
  (subseq (concatenate 'string text (make-string width :initial-element #\space))
          0 width))

(defun vm-list-table-text (records)
  (if (null records)
      (format nil "No virtual machines found, or ~A is unavailable on this host.~%~%"
              (format nil "~{~A~^ ~}" +vmadm-list-cmd+))
      (with-output-to-string (out)
        (flet ((print-row (uuid type ram state alias)
                 (format out "~A ~A ~A ~A ~A~%"
                         (pad-field uuid 36)
                         (pad-field type 5)
                         (pad-field ram 8)
                         (pad-field state 10)
                         (pad-field alias 16))))
          (print-row "UUID" "TYPE" "RAM" "STATE" "ALIAS")
          (loop for record in records
                do (print-row (vm-record-uuid record)
                              (vm-record-type record)
                              (vm-record-ram record)
                              (vm-record-state record)
                              (vm-record-name record)))))))

(defun vm-picker-label (record)
  (format nil "~A  ~A  ~A MB  ~A"
          (vm-record-name record)
          (vm-record-type record)
          (vm-record-ram record)
          (vm-record-state record)))
