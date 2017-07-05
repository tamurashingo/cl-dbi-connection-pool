(in-package :cl-user)
(defpackage dbi-cp.semaphore
  (:use :cl))
(in-package :dbi-cp.semaphore)

(cl-syntax:use-syntax :annot)

@export
(defclass <dbi-semaphore> ()
  ((semaphore :initarg :semaphore
              :accessor semaphore
              :initform (bt-sem:make-semaphore :count 1))
   (connection :initarg :semaphore-connection
               :accessor semaphore-connection
               :initform NIL)))

@export
(defun make-dbi-semaphore (&key (connection NIL))
  (make-instance '<dbi-semaphore>
                 :semaphore-connection connection))

@export
(defmethod borrow-p ((sem <dbi-semaphore>))
  (> (bt-sem:semaphore-count (semaphore sem))))

@export
(defmethod borrow ((sem <dbi-semaphore>))
  (if (bt-sem:try-semaphore (semaphore sem))
      (semaphore-connection sem)
      NIL))

@export
(defmethod back ((sem <dbi-semaphore>))
  (bt-sem:signal-semaphore (semaphore sem)))

@export
(defmethod reset-counter ((sem <dbi-semaphore>) &optional (n 0))
  "reset semaphore counter"
  (with-slots ((lck bt-sem:lock)
               (count bt-sem:count)) (semaphore sem)
    (bt:with-lock-held (lck)
      (setf count n))))
