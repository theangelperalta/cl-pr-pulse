;;;; util.asd
;;;;

(asdf:defsystem #:util
  :description "Utilities functions"
  :author "Angel Cortez <angel.cortez@nbcuni.com>"
  :license "MIT"
  :depends-on (#:cl-json)
  :components ((:file "package")
               (:file "util")))
