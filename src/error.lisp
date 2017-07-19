(in-package :cl-user)
(defpackage dbi-cp.error
  (:use cl))
(in-package :dbi-cp.error)

(cl-syntax:use-syntax :annot)

@export
(define-condition <dbi-cp-no-connection> (simple-error) ()
  (:documentation "Exception raised when no connection found on connection pool")
  (:report
   (lambda (condition stream)
     (format stream
             "no database connection found"))))
