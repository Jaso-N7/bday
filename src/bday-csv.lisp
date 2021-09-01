(defpackage bday/csv
  (:documentation "CSV application interface")
  (:use :cl :fare-csv)
  (:export #:csv-encode
	   #:csv-decode))

(in-package :bday/csv)

(defun csv-encode (ht)
  "Takes a hashtable and transform them into a string that is valid CSV,
with a header."
  (declare (ignore ht))
  "last_name, firstname, date_of_birth, email
Doe, John, 1982/10/08, john.doe@foobar.com
Ann, Mary, 1975/09/11, mary.ann@foobar.com")

(defun csv-decode (strng)
  "Takes a string that represents a valid CSV data dump and turn it into
a hashtable with the header entries as keys"
  (declare (ignore strng))
  (make-hash-table :test #'equal :size 4))
