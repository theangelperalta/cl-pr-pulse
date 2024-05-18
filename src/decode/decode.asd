;;;; decode.asd
;;;;

(asdf:defsystem #:decode
  :description "JSON decoding system"
  :author "Angel Peralta <acort3255@gmail.com>"
  :license "MIT"
  :depends-on (#:model #:local-time)
  :serial t
  :components ((:file "package")
               (:file "decode")))
