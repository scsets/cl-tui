(in-package :smartos-test)

(defstruct image-record
  (uuid "" :type string)
  (name "" :type string)
  (version "" :type string)
  (os "" :type string)
  (type "" :type string)
  (published "" :type string))

(defun image-detail-lines (image)
  (with-slots (uuid name version os type published)
      image
    (list (format nil "Name      : ~A" name)
          (format nil "UUID      : ~A" uuid)
          (format nil "Version   : ~A" version)
          (format nil "OS        : ~A" os)
          (format nil "Type      : ~A" type)
          (format nil "Published : ~A" published))))

(defun image-detail-text (image &key command)
  (format nil "~{~A~%~}~%Source command:~%  ~A"
          (append (image-detail-lines image) '(""))
          command))
