(in-package :smartos-test)

(defstruct hardware-report
  (hostname "Unknown" :type string)
  (kernel "Unknown" :type string)
  (architecture "Unknown" :type string)
  (platform "Unknown" :type string)
  (cpu-summary "Unknown" :type string)
  (memory-summary "Unknown" :type string)
  (smbios-system "Unknown" :type string))

(defstruct vm-record
  (uuid "" :type string)
  (name "" :type string)
  (state "" :type string)
  (brand "" :type string)
  (ram "" :type string)
  (disk "" :type string))

(defun hardware-report-lines (report)
  (with-slots (hostname kernel architecture platform
               cpu-summary memory-summary smbios-system)
      report
    (list (format nil "Hostname     : ~A" hostname)
          (format nil "Kernel       : ~A" kernel)
          (format nil "Architecture : ~A" architecture)
          (format nil "Platform     : ~A" platform)
          (format nil "Processors   : ~A" cpu-summary)
          (format nil "Memory       : ~A" memory-summary)
          (format nil "SMBIOS       : ~A" smbios-system))))
