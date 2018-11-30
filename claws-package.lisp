(in-package :cl-user)

(defpackage #:claws
  (:use #:cl)
  (:export #:fork #:spawn #:sync #:parlet #:seqlet #:reset-workers #:psort #:stable-psort))
