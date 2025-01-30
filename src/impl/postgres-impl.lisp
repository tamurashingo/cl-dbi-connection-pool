(in-package :cl-user)
(defpackage dbi-cp.impl.postgres
  (:use :cl)
  (:import-from #:dbi-cp.proxy
                #:set-autocommit-impl))
(in-package dbi-cp.impl.postgres)

(defmethod set-autocommit-impl ((conn dbd.postgres:<dbd-postgres>) mode not-supported-error-p)
  (declare (ignore conn))
  (if not-supported-error-p
      (error "not support set-autocommit in postgres")
      nil))

(defmethod get-autocommit-impl ((conn dbd.postgres:<dbd-postgres>) not-supported-error-p)
  (declare (ignore conn))
  (if not-supported-error-p
      (error "not support get-autocommit in postgres")
      nil))

