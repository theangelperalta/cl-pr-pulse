;;;; model.asd
;;;;

(asdf:defsystem #:interactor
  :description "Interactor"
  :author "Angel Peralta <acort3255@gmail.com>"
  :license "MIT"
  :depends-on (#:util
               #:net
               #:model
               #:decode)
  :components ((:file "package")
               (:file "interactor")))
