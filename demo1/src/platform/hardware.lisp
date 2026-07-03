(in-package :smartos-test)

(defparameter +hostname-cmd+ '("/usr/bin/hostname"))
(defparameter +uname-cmd+ '("/usr/bin/uname" "-a"))
(defparameter +psrinfo-cmd+ '("/usr/sbin/psrinfo" "-pv"))
(defparameter +prtconf-memory-cmd+
  '("/bin/sh" "-c" "prtconf 2>/dev/null | grep -i memory | head -5"))
(defparameter +smbios-system-cmd+ '("/usr/sbin/smbios" "-t" "1"))

(defun run-capture (command)
  "Run COMMAND, returning trimmed stdout or NIL on failure."
  (ignore-errors
    (string-trim '(#\newline #\return #\space #\tab)
                 (uiop:run-program command
                                   :output :string
                                   :error-output nil))))

(defun first-line (text)
  (when text
    (first (uiop:split-string text :separator '(#\newline)))))

(defun blank-line-p (line)
  (zerop (length (string-trim '(#\space #\tab) line))))

(defun summarize-psrinfo (text)
  (if (or (null text) (string= text ""))
      "Unknown"
      (let ((lines (remove-if #'blank-line-p
                              (uiop:split-string text :separator '(#\newline)))))
        (if (<= (length lines) 3)
            text
            (format nil "~D processors (~A ...)"
                    (length (remove-if-not
                             (lambda (line)
                               (search "The " line))
                             lines))
                    (first-line text))))))

(defun parse-uname (uname)
  "Return (ARCHITECTURE PLATFORM) from uname -a output."
  (when uname
    (let ((parts (uiop:split-string uname :separator '(#\space))))
      (cond
        ((and (string= (first parts) "SunOS")
               (>= (length parts) 6))
         (values (nth 5 parts)
                 (format nil "SunOS ~A ~A" (nth 2 parts) (nth 3 parts))))
        ((>= (length parts) 3)
         (values (nth 2 parts) (first parts)))
        (t (values "Unknown" "Unknown"))))))

(defun gather-hardware-report ()
  "Collect host hardware parameters via standard SmartOS/Solaris tools."
  (let* ((uname (run-capture +uname-cmd+))
         (arch+platform (multiple-value-list (parse-uname uname))))
    (make-hardware-report
     :hostname (or (run-capture +hostname-cmd+) "Unknown")
     :kernel (or uname "Unknown")
     :architecture (or (first arch+platform) "Unknown")
     :platform (or (second arch+platform) "Unknown")
     :cpu-summary (summarize-psrinfo (run-capture +psrinfo-cmd+))
     :memory-summary (or (run-capture +prtconf-memory-cmd+) "Unknown")
     :smbios-system (or (first-line (run-capture +smbios-system-cmd+))
                        "Unknown"))))

(defun hardware-report-text (report)
  (format nil "~{~A~%~}"
          (append (hardware-report-lines report)
                  '(""
                    "Source commands:"
                    "  /usr/bin/hostname"
                    "  /usr/bin/uname -a"
                    "  /usr/sbin/psrinfo -pv"
                    "  prtconf | grep -i memory"
                    "  /usr/sbin/smbios -t 1"))))
