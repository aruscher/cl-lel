;;;; cl-lel.asd

(asdf:defsystem #:cl-lel
  :description "Describe cl-lel here"
  :author "Andreas Ruscheinski <andreas.ruscheinski@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "cl-lel")))
