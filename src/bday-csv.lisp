(defpackage bday/csv
  (:documentation "CSV application interface")
  (:use :cl :fare-csv)
  (:export #:csv-encode
	   #:csv-decode))

(in-package :bday/csv)

(defun csv-encode (lists)
  "Takes a list of lists containing strings and transform them into a string
 that is valid CSV, with a header. It is assumed that the first list is the
header."
  (with-output-to-string (var)
    (fare-csv:write-csv-lines lists var)))

(defun csv-decode (strng)
  "Takes a string that represents a valid CSV data dump and turn it into
a list of lists with the header entries as the first list."
  (with-input-from-string (s strng)
    (fare-csv:read-csv-stream s)))
