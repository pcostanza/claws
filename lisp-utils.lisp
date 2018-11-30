(in-package :claws)

(declaim (inline get-function))

(defun get-function (f)
  (etypecase f
    (function f)
    (symbol (symbol-function f))
    (cons (fdefinition f))))

(declaim (inline list-length-less))

(defun list-length-less (list length)
  (declare (list list) (fixnum length) #.*optimization*)
  (if (< length 0) nil
    (loop for count of-type fixnum from 0
          for nil in list
          until (>= count length)
          finally (return (< count length)))))
