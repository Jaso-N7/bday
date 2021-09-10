(defpackage bday/tests.generators
  (:documentation "Provides custom generators for the properties")
  (:use :cl :cl-quickcheck)
  (:export #:csv-source))

(in-package :bday/tests.generators)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defmacro one-of (&rest exprs)
    "Courtesy of Paul Graham's ANSI CL - Macros pg 170."
    `(case (random ,(length exprs))
       ,@(let ((key -1))
	   (mapcar #'(lambda (expr)
		       `(,(incf key) ,expr))
		   exprs))))
  (defmacro csv-list (columns field-type)
    (let ((cols (gensym))
	  (ft (gensym)))
      `(let ((,cols ,columns)
	     (,ft ,field-type))
	 (let ((row (make-list ,cols :initial-element "")))
	   (dotimes (r ,cols row)
	     (setf (nth r row) (generate ,ft))))))))

;;; DATA DEFINITIONS

(defparameter *textdata*
  (concatenate 'string 
	       "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
	       ":;<=>?@ !#$%&'()*+-./[\\]^_`{|}~")
  "Contains all the valid characters allowed by CSV specification.")

;;; GENERATORS

;; *-- CSV: CSV Parsing

(define (unquoted-text)
  "Used to generate data that will require no known escape sequence once in it 
once converted."
  (generate (a-csv-string *textdata*)))

(define (quotable-text)
  "Used to generate sequences that may possibly require escaping (the four escapable
 characters are only present in this one)."
  (generate (a-csv-string (concatenate 'string
				       #(#\NEWLINE #\" #\,)
				       *textdata*))))

(define (field)
  "Randomly generate valid CSV field data."
  (one-of (generate unquoted-text) (generate quotable-text)))

(defun a-csv-string (sample-text)
  (lambda ()
    (let* ((limit (length sample-text))
	   (to-string (make-string (random limit)
				   :initial-element #\SPACE))
	   (string-lim (length to-string)))
      (dotimes (i string-lim to-string)
	(setf (char to-string i)
	      (char sample-text (random limit)))))))

(define (name)
  (generate field))

(defun header (size)
  "Generates a type of string with characters from `*TEXTDATA*' for the CSV header,
 with a known fixed length SIZE as an argument."
  (csv-list size name))

(defun record (size)
  "Generates a type of string with characters from `*TEXTDATA*' for the CSV row,
 with a known fixed length SIZE as an argument." 
  (csv-list size field))

#|Imitates the PICK functionality

(defmacro pick (&body body)
  "Generates random instances of BODY and returns a list of results."
  `(let (sample)
     (quickcheck
       (let ((*num-trials* (1+ (random 10))))
	 (for-all ((src ,@body))
	   (is= T T)
	   (push src sample))))
     sample))

|#

;; List
(define (csv-source)
  "Picks up a SIZE value that represents how many entries will be in each row.
Returns a hashtable containing all the data of the needed records."
  (let ((size (generate an-index)))
    (let ((keys (header (1+ size))))
      (entry (1+ size) keys))))

;; Number -> Vector -> HashTable
(defun entry (size keys)
  "Generates one set of headers (the keys of every map), and then uses them to create
a list of entries."
  (let ((vals (records size)))
    (append (list keys) vals)))

(defun records (size)
  "Generate SIZE rows of SIZE records each. Returns a list of lists. The list is of
length SIZE, with each list containing SIZE elements."
  (let (rows)
    (dotimes (s size (nreverse rows))
      (push (record size) rows))))
