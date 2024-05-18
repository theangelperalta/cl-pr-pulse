;;;; cl-pr-pulse.asd
;;;;

(eval-when (:compile-toplevel :load-toplevel :execute))

(asdf:defsystem #:cl-pr-pulse
  :description "Describe cl-pr-pulse here"
  :author "Angel Perlata <acort3255@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :pathname "src"
  :serial t
  :depends-on (#:util
               #:net
               #:model
               #:decode)
  :components (
               (:file "main")))


(defpackage :cl-pr-pulse
  (:use #:cl)
  (:export #:main))
