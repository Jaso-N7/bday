(defpackage bday/tests.suite
  (:documentation "Ties all the tests together.")
  (:use :cl
        :bday
        :cl-quickcheck)
  (:export #:run))

(in-package :bday/tests.suite)

(defparameter *skipped*
  (vector nil)
  "Do not run these tests.")

(defparameter *suite*
  (vector nil)
  "Test Suite. Holds the names of all the tests to be run.")

(defun run ()
  "Run ALL tests."
  (dotimes (index (length *suite*))
    (let ((test-name (svref *suite* index)))
      (unless (find test-name *skipped*)
	(funcall test-name)))))

