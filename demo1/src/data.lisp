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
  (type "" :type string)
  (brand "" :type string)
  (ram "" :type string)
  (disk "" :type string))

(defstruct vm-detail
  (uuid "" :type string)
  (name "" :type string)
  (hostname "" :type string)
  (state "" :type string)
  (brand "" :type string)
  (type "" :type string)
  (ram-mb "" :type string)
  (vcpus "" :type string)
  (image-uuid "" :type string)
  (created "" :type string))

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

(defun vm-detail-lines (detail)
  (with-slots (uuid name hostname state brand type ram-mb vcpus
               image-uuid created)
      detail
    (list (format nil "Name         : ~A" name)
          (format nil "UUID         : ~A" uuid)
          (format nil "Hostname     : ~A" hostname)
          (format nil "State        : ~A" state)
          (format nil "Brand        : ~A" brand)
          (format nil "Type         : ~A" type)
          (format nil "RAM (MB)     : ~A" ram-mb)
          (format nil "vCPUs        : ~A" (if (string= vcpus "") "n/a" vcpus))
          (format nil "Image UUID   : ~A" image-uuid)
          (format nil "Created      : ~A" created))))

(defun vm-detail-text (detail &key command)
  (format nil "~{~A~%~}~%Source command:~%  ~A"
          (append (vm-detail-lines detail) '(""))
          command))
