(defpackage bday/csv
  (:documentation "CSV application interface")
  (:use :cl :fare-csv)
  (:export #:csv-encode
	   #:csv-decode))

(in-package :bday/csv)

(defun pre-process-list (lsts)
  "For any record or field (a list within the list) that is NIL, convert it to the
expected list of empty string.
EXAMPLE: (pre-process-list '((\"field\") NIL) => ((\"field\") (\"\")"
  (dotimes (i (length lsts) lsts)
    (unless (consp (nth i lsts))
      (setf (nth i lsts) (list "")))))

(defun csv-encode (lists)
  "Takes a list of lists containing strings and transform them into a string
 that is valid RFC 4180 CSV, with a header. It is assumed that the first list is
 the header."
  (fare-csv:with-rfc4180-csv-syntax ()
    (let ((fare-csv:*separator* #\,))
      (with-output-to-string (var)
	(fare-csv:write-csv-lines (pre-process-list lists) var)))))

(defun csv-decode (strng)
  "Takes a string that represents a valid RFC 4180 CSV data dump and turn it into
a list of lists with the header entries as the first list."
  (pre-process-list
   (fare-csv:with-rfc4180-csv-syntax ()
     (let ((fare-csv:*separator* #\,))
       (with-input-from-string (s strng)
	 (fare-csv:read-csv-stream s))))))
