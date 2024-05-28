;;;; package.lisp
;;;;

(defpackage :net
  (:use #:cl
        #:drakma
        #:cl-json)
  (:import-from #:util)
  (:export #:with-json-decoding
           #:http-request-with-json-decoding))
