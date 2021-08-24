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
			   exprs))))
	   (defmacro csv-vector (field-size field-type)
	     `(let* ((new-field field-type)
		  (lim (length new-field)))
		(cond ((= lim field-size)
		    new-field)
		   ((> lim size)
		    (subseq new-field 0 field-size))
		   (T (let ((size-diff (- field-size lim)))
			(format nil "~A~A"
				new-field
				(make-string size-diff :initial-element #\+))))))))


;;; PROPERTIES
(defun sample-test ()
  (named "Dummy Test"
	 (is= 1 1)))


;;; HELPERS



;;; GENERATORS

(defparameter *textdata*
  (concatenate 'string 
	       "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
	       ":;<=>?@ !#$%&'()*+-./[\\]^_`{}")
  "Contains all the valid characters allowed by CSV specification.")




(define (field)
  "Randomly generate valid CSV field data."
  (one-of (unquoted-text) (quotable-text)))

(defun unquoted-text ()
  "Used to generate data that will require no known escape sequence once in it once converted."
  (generate-bounded-string *textdata*))

(defun quotable-text ()
  "Used to generate sequences that may possibly require escaping (the four escapable characters
 are only present in this one)."
  (generate-bounded-string (concatenate 'string "\r\n,\"" *textdata*)))

(defun generate-bounded-string (sample-text)
  (let* ((limit (length sample-text))
      (to-string (make-string (random limit)
			      :initial-element #\SPACE))
      (string-lim (length to-string)))
    (dotimes (i string-lim to-string)
      (setf (char to-string i)
	 (char sample-text (random limit))))))

(define (name) field)

(define (header size)
  "Generates a type of string with characters from `*TEXTDATA*' for the CSV header, with a known
fixed length SIZE as an argument."
  (csv-vector size name))

(define (records size)
    "Generates a type of string with characters from `*TEXTDATA*' for the CSV row, with a known
fixed length SIZE as an argument."
  (csv-vector size field))
