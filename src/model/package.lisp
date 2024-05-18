;;;; package.lisp
;;;;

(defpackage :model
  (:use #:cl)
  (:export #:user
           #:review
           #:pull-request
           #:make-user
           #:make-review
           #:make-pull-request))
