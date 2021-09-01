(defpackage bday/tests.csv
  (:documentation "Property-Based and Unit Tests for the CSV Parsing package.")
  (:use :cl :cl-quickcheck)
  (:import-from :bday/csv
		#:csv-encode
		#:csv-decode)
  (:export #:csv-pbts))

(in-package :bday/tests.csv)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defmacro one-of (&rest exprs)
    "Courtesy of Paul Graham's ANSI CL - Macros pg 170."
    `(case (random ,(length exprs))
       ,@(let ((key -1))
	   (mapcar #'(lambda (expr)
		       `(,(incf key) ,expr))
		   exprs))))
  (defmacro csv-list (&body lists)
	    `(list ,@lists))
  (defmacro csv-vector (field-size field-type)
    (let ((fs (gensym))
	  (ft (gensym)))
      `(let ((,fs ,field-size)
	     (,ft ,field-type))
	 (let ((vec (make-array ,fs :element-type 'string)))
	   (dotimes (ind ,fs vec)
	     (setf (svref vec ind) (generate ,ft))))))))


;;; DATA DEFINITIONS

(defparameter *textdata*
  (concatenate 'string 
	       "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
	       ":;<=>?@ !#$%&'()*+-./[\\]^_`{|}~")
  "Contains all the valid characters allowed by CSV specification.")


;;; PROPERTIES

(defun csv-pbts ()
  (named "roundtrip encoding/decoding"
    (for-all ((maps csv-source))
      (is= maps (csv-decode (csv-encode maps))))))


;;; HELPERS



;;; GENERATORS
;; :field :name :header :record :csv-source :entry

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
  (csv-vector size name))

(defun record (size)
  "Generates a type of string with characters from `*TEXTDATA*' for the CSV row,
 with a known fixed length SIZE as an argument." 
  (csv-vector size field))

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
    (let ((keys (header size)))
      (entry size keys))))

;; Number -> Vector -> HashTable
(defun entry (size keys)
  "Generates one set of headers (the keys of every map), and then uses them to create
a list of entries."
  (let ((vals (records size))
	(entries (make-hash-table :size size :test #'equal)))
    (dotimes (ind size entries)
      (setf (gethash (svref keys ind) entries)
	    (aref vals ind)))))

(defun records (size)
  (let ((rows (make-array size)))
    (dotimes (s size rows)
      (setf (aref rows s) (record size)))))
