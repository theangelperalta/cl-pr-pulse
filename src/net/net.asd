;;;; net.asd
;;;;

(asdf:defsystem #:net
  :description "Networking"
  :author "Angel Cortez <angel.cortez@nbcuni.com>"
  :license "MIT"
  :depends-on (#:drakma
               #:cl-json)
  :components ((:file "package")
               (:file "net")))
