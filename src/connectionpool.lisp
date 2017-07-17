(in-package :cl-user)
(defpackage dbi-cp.connectionpool
  (:use :cl)
  (:import-from :dbi-cp.semaphore
                :make-dbi-semaphore
                :borrow
                :back
                :reset-counter
                :synchronized
                :<synchronized>
                :synchronized))
(in-package :dbi-cp.connectionpool)

(cl-syntax:use-syntax :annot)


(defvar *CONNECTIONS* NIL
  "array for connection and in use flag")

(defvar *CONNECTION-HASH* NIL
  "dbi-connection to connection array index")


@export
(defclass <dbi-connection-pool> ()
  ((pool :type array
         :initarg :pool
         :documentation "array of <pooled-connection>")
   (connect-fn :type function
               :initarg :connect-fn
               :documentation "function what connecto to database ")))

(defclass <pooled-connnection> ()
  ((connect-p :type boolean
              :accessor connect-p
              :initform NIL)
   (semaphore :accessor semaphore
              :initform (bt-sem:make-semaphore :count 1))
   (dbi-connection-proxy :type <dbi-connection-proxy>
                         :initarg :dbi-connection-proxy
                         :accessor dbi-connection-proxy
                         :initform NIL)))

@export
(defun make-connection-pool (driver-name &rest params &key database-name username passwod (initial-size 10) (max-size 10))
  "make connection pool

Example
  (make-connection-pool :mysql :database-name \"cldbi\" :username \"root\" :password \"password\")"
  (let ((pool (make-array max-size :initial-element NIL))
        (dbi-connection-pool
         (make-instance '<dbi-connection-pool>
                        :pool pool
                        :connect-fn (lambda ()
                                      (apply #'dbi:connect driver-name params)))))
    ;; create <pooled-connection> ]instance
    (loop for idx from 0 below max-size
          do (setf (aref pool idx)
                   (make-instance '<pooled-connection>)))
    ;; connect initial-size connection
    (let ((connect-fn (connect-fn dbi-connection-pool)))
      (loop for idx from 0 below initial-size
            do (let ((pooled-conn (aref pool idx)))
                 (setf (connect-p pooled-conn) T)
                 (setf (dbi-connection-proxy pooled-conn) (funcall connect-fn)))))

    dbi-connection-pool))


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



