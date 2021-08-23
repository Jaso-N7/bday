(defpackage bday/tests.suite
  (:documentation "Ties all the tests together.")
  (:use :cl
        :bday
        :cl-quickcheck)
  (:import-from :bday/tests.csv
		#:sample-test)
  (:export #:run))

(in-package :bday/tests.suite)

(defparameter *skipped*
  (vector NIL)
  "Do not run these tests.")

(defparameter *suite*
  (vector #'sample-test)
  "Test Suite. Holds the names of all the tests to be run.")

(defun run ()
  "Run ALL tests."
  (dotimes (index (length *suite*))
    (let ((test-name (svref *suite* index)))
      (unless (find test-name *skipped*)
	(funcall test-name)))))

