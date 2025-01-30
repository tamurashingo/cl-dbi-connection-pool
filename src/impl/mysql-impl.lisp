(in-package :cl-user)
(defpackage dbi-cp.impl.mysql
  (:use :cl)
  (:import-from #:dbi-cp.proxy
                #:set-autocommit-impl))
(in-package dbi-cp.impl.mysql)

(defmethod set-autocommit-impl ((conn dbd.mysql:<dbd-mysql>) mode not-supported-error-p)
  (declare (ignore not-supported-error-p))
  (dbi:do-sql conn "set autocommit = ?" (when mode 1 0)))

(defmethod get-autocommit-impl ((conn dbd.mysql:<dbd-mysql>) not-supported-error-p)
  (declare (ignore not-supported-error-p))
  (let* ((query (dbi:prepare conn "show variables like 'autocommit'"))
         (query (dbi:execute query '()))
         (result (dbi:fetch result)))
    (string= (getf result :|Value|) "ON")))

