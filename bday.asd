(defsystem "bday"
  :version "0.2.1"
  :author "Jason S. Robinson"
  :license "BSD-3"
  :depends-on ("fare-csv")
	         ;; "postmaster"
  :components ((:module "src"
                :components
                ((:file "bday-csv")
		 (:file "main"))))
  :description "Birthday greetings kata used for Property-Based Testing."
  :in-order-to ((test-op (test-op "bday/tests"))))

(defsystem "bday/tests"
  :author "Jason S. Robinson"
  :license "BSD-3"
  :depends-on ("bday"
               "cl-quickcheck") 	       ;; "mockingbird" ;; "postmaster-mock"
  :components ((:module "tests"
                :components
                ((:file "generators")
		 (:file "csv-test")
		 (:file "filter-test")
		 (:file "suite"))))
  :description "Test system for bday"
  :perform (test-op (op c) (cl-quickcheck:quickcheck
			     (symbol-call :bday/tests.suite :run))))
