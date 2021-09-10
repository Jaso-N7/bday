(defpackage bday/tests.filter
  (:documentation "All tests related to filtering employee records.")
  (:use :cl :bday))

(in-package :bday/tests.filter)

;;; DATA DEFINITIONS


;;; UNIT TESTS

(defun filter-examples ()
  "Unit Tests for Filtering Records."

  (named "property-style filtering test"
    (let ((years (generated-years-data 2018 2038))
	  (people (generate-people-for-years 3)))
      (mapcar #'(lambda (yeardata)
		  (let ((birthdays (find-birthdays-for-year people yeardata)))
		    (progn
		      (every-birthday-once people birthdays)
		      (on-right-date people birthdays))))
	      years))))

;;; PROPERTY TESTS


;;; HELPERS

(defun leap-year-p (year)
  "Accepts a YEAR and returns T if it is a leap year; Otherwise NIL."
  (if (zerop (mod year 4))
      (plusp (mod year 100))
      (zerop (mod year 400))))

;;; GENERATORS
