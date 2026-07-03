;;; Load and start the SmartOS Test demo from a LispWorks Listener.
;;;
;;;   (load "/path/to/cl-tui/demo1/run.lisp")
;;;
;;; Or register the system once, then:
;;;
;;;   (asdf:load-system :smartos-test)
;;;   (smartos-test:main)

(require :asdf)

(pushnew (uiop:pathname-directory-pathname *load-pathname*)
         asdf:*central-registry*
         :test #'equal)

(asdf:load-system :smartos-test)
(smartos-test:main)
