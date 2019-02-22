;;;; dc-ann-examples.asd

(asdf:defsystem :dc-ann-examples
  :description "Examples of how to use the dc-ann neural network."
  :author "Donnie Cameron <macnod@gmail.com>"
  :license "MIT License"
  :depends-on (:dc-utilities :dc-ann :cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "dc-ann-examples")))


