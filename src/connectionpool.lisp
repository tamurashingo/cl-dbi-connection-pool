(in-package :cl-user)
(defpackage dbi-cp.connectionpool
  (:use :cl
	:cl-annot))
(in-package :dbi-cp.connectionpool)

(cl-syntax:use-syntax :annot)


(defvar *CONNECTIONS* NIL
  "array for connection and in use flag")

(defvar *CONNECTION-HASH* NIL
  "dbi-connection to connection array index")

@export
(defun make-connection-pool (driver-name &rest params &key database-name username password (initial-size 10) (max-size 10))
  "make connection pool

Example
  (make-connection-pool :mysql :database-name \"cldbi\" :username \"root\" :password \"password\")"
  (disconnect-all)
  (setf *CONNECTIONS* (make-array initial-size :adjustable T))
  (setf *CONNECTION-HASH* (make-hash-table :test #'eq))
  (loop for index from 0 below initial-size
        ; do (format T "setting ~A~%" index)
        do (let ((conn (apply #'dbi:connect driver-name params)))
             (setf (aref *CONNECTIONS* index)
                   (list :conn conn
                         :use NIL)))))
@export
(defun disconnect-all ()
  "disconnect all connections"
  (when *CONNECTIONS*
    (loop for conn across *CONNECTIONS*
          for index = 0 then (1+ index)
          if (not (null (getf conn :conn)))
	     do ; (format T "disconnect ~A~%" index)
                (remhash (getf conn :conn) *CONNECTION-HASH*)
                (dbi:disconnect (getf conn :conn))
                (setf (getf conn :conn) NIL)
	        (setf (getf conn :use) NIL))))


@export
(defun get-connection ()
  "get connection from connection pool"
  (loop for conn across *CONNECTIONS*
        for index = 0 then (1+ index)
        ; do (format T "checking ~A ...~%" index)
        if (not (eq (getf conn :use) :USE))
           do ; (format T "use: ~A~%" index)
              (setf (getf conn :use) :USE)
              (setf (gethash (getf conn :conn) *CONNECTION-HASH*) index)
              (return (getf conn :conn))))

@export
(defun disconnect (dbi-conn)
  "returns connection to connection pool"
  (multiple-value-bind (index flag)
      (gethash dbi-conn *CONNECTION-HASH*)
    (when flag
      (let ((conn (aref *CONNECTIONS* index)))
        (setf (getf conn :use) NIL)
        (remhash dbi-conn *CONNECTION-HASH*)))))



