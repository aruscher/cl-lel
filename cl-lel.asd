;;;; cl-lel.asd

(asdf:defsystem #:cl-lel
  :description "Describe cl-lel here"
  :author "Andreas Ruscheinski <andreas.ruscheinski@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :pathname "src/"
  :components ((:file "cl-lel"))
  :in-order-to ((test-op (test-op "cl-lel/tests"))))

(asdf:defsystem #:cl-lel/tests
  :description "Describe cl-lel here"
  :author "Andreas Ruscheinski <andreas.ruscheinski@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on ("cl-lel" "fiveam")
  :pathname "t/"
  :components ((:file "tests"))
  :perform (test-op (o s)
		    (uiop:symbol-call :fiveam '#:run!
				      (uiop:find-symbol* '#:all-tests :cl-lel-tests))))
