(in-package :cl-user)
(defpackage dbi-cp.impl.sqlite3
  (:use :cl)
  (:import-from #:dbi-cp.proxy
                #:set-autocommit-impl))
(in-package dbi-cp.impl.sqlite3)

(defmethod set-autocommit-impl ((conn dbd.sqlite3:<dbd-sqlite3>) mode not-supported-error-p)
  (declare (ignore conn))
  (if not-supported-error-p
      (error "not support set-autocommit in sqlite3")
      nil))

(defmethod get-autocommit-impl ((conn dbd.sqlite3:<dbd-sqlite3>) not-supported-error-p)
  (declare (ignore conn))
  (if not-supported-error-p
      (error "not support get-autocommit in sqlite3")
      nil))

