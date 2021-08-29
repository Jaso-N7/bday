(defpackage bday/tests.csv
  (:documentation "Property-Based and Unit Tests for the CSV Parsing package.")
  (:use :cl :cl-quickcheck)
  (:export #:csv-pbts))

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
	     (let ((fs (gensym))
		   (ft (gensym))
		   (lim (gensym)))
	       `(let* ((,fs ,field-size)
		       (,ft (funcall ,field-type))
		       (,lim (length ,ft)))
		  (cond ((= ,lim ,fs)
			 ,ft)
			((> ,lim ,fs)
			 (subseq ,ft 0 ,fs))
			(T (let ((size-diff (- ,fs ,lim)))
			     (format nil "~A~A"
				     ,ft
				     (make-string size-diff :initial-element #\+)))))))))

;;; DATA DEFINITIONS

(defparameter *textdata*
  (concatenate 'string 
	       "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
	       ":;<=>?@ !#$%&'()*+-./[\\]^_`{}")
  "Contains all the valid characters allowed by CSV specification.")


;;; PROPERTIES

(defun csv-pbts ()
  (named "roundtrip encoding/decoding"
    (for-all ((maps csv-source))
      (is= maps (csv-decode (csv-encode maps))))))


;;; HELPERS



;;; GENERATORS
;; :field :name :header :record :csv-source :entry

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

(define (name)
  (generate field))

(defun header (size)
  "Generates a type of string with characters from `*TEXTDATA*' for the CSV header, with a known
fixed length SIZE as an argument."
  (csv-vector size name))

(defun record (size)
  "Generates a type of string with characters from `*TEXTDATA*' for the CSV row, with a known
fixed length SIZE as an argument."
  (csv-vector size field))

;; List
(define (csv-source)
  "Picks up a SIZE value that represents how many entries will be in each row."
  (let ((size (generate an-index)))
    (let ((keys (header size)))
      (list (entry size keys)))))

;; HashTable
(defun entry (size keys)
  "Generates one set of headers (the keys of every map), and then uses them to create
a list of entries."
  (let ((vals (record size))
	(entries (make-hash-table :size size)))
    (mapc #'(lambda (k v)
	      (setf (gethash k entries) v))
	  keys vals)))
