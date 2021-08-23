(defpackage bday/tests.csv
  (:documentation "Property-Based and Unit Tests for the CSV Parsing package.")
  (:use :cl :cl-quickcheck)
  (:export #:sample-test))

(in-package :bday/tests.csv)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defmacro one-of (&rest exprs)
    "Courtesy of Paul Graham's ANSI CL - Macros pg 170."
    `(case (random, (length exprs))
       ,@(let ((key -1))
	   (mapcar #'(lambda (expr)
		       `(,(incf key) ,expr))
		   exprs)))))

;;; PROPERTIES
(defun sample-test ()
  (named "Dummy Test"
	 (is= 1 1)))


;;; HELPERS

(defparameter textdata
  (concatenate 'string 
	       "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
	       ":;<=>?@ !#$%&'()*+-./\\^_`{}"))

(defun field ()
   (one-of (unquoted-text) (quotable-text)))

(defun unquoted-text ()
  "Used to generate data that will require no known escape sequence once in it once converted."
  (error "Not yet implemented"))

(defun quotable-text ()
  "Used to generate sequences that may possibly require escaping (the four escapable characters
 are only present in this one)."
  (error "Not yet implemented."))


;;; GENERATORS
