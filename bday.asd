(defsystem "bday"
  :version "0.1.0"
  :author "Jason S. Robinson"
  :license "BSD-3"
  :depends-on ("cl-csv"
               "postmaster")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "Birthday greetings kata used for Property-Based Testing."
  :in-order-to ((test-op (test-op "bday/tests"))))

(defsystem "bday/tests"
  :author "Jason S. Robinson"
  :license "BSD-3"
  :depends-on ("bday"
	       "mockingbird"
	       "postmaster-mock"
               "cl-quickcheck")
  :components ((:module "tests"
                :components
                ((:file "csv-test")
		 (:file "suite"))))
  :description "Test system for bday"
  :perform (test-op (op c) (cl-quickcheck:quickcheck
			     (symbol-call :bday/tests.suite :run)))
