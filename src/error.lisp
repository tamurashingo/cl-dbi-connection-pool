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

@export
(define-condition <dbi-cp-packet-size-exceeded> (simple-error)
  ((query-size :initarg :query-size
               :reader query-size)
   (max-allowed :initarg :max-allowed
                :reader max-allowed)
   (query :initarg :query
          :reader query
          :initform nil))
  (:documentation "Exception raised when query size exceeds max_allowed_packet")
  (:report
   (lambda (condition stream)
     (format stream
             "Query size (~A bytes) exceeds max_allowed_packet (~A bytes)~@[~%Query: ~A~]"
             (query-size condition)
             (max-allowed condition)
             (query condition)))))
