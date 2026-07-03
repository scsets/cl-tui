(require :asdf)
(pushnew #P"/opt/tmp/cl-tui/demo1/" asdf:*central-registry* :test #'equal)
(asdf:load-system :smartos-test)
(format t "run-capture vmadm get...~%")
(finish-output)
(let ((out (smartos-test::run-capture
            '("/usr/sbin/vmadm" "get" "3a8fcbe5-1248-4abf-9afc-ea55aa8202c5"))))
  (format t "len=~D~%" (length (or out ""))))
(quit)
