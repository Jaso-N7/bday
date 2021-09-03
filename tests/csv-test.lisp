(defpackage bday/tests.csv
  (:documentation "Property-Based and Unit Tests for the CSV Parsing package.")
  (:use :cl :cl-quickcheck)
  (:import-from :bday/tests.generators
		#:csv-source)
  (:import-from :bday/csv
		#:csv-encode
		#:csv-decode)
  (:export #:csv-pbts
	   #:csv-examples))

(in-package :bday/tests.csv)

;;; DATA DEFINITIONS



;;; UNIT TESTS

(defun csv-examples ()

  (named "UNIT: duplicate keys unsupported"
    (let* ((csv-sample
	     "field_name,field_name,field_name
aaa,bbb,ccc
zzz,yyy,xxx
")
	   (dcode (csv-decode csv-sample)))
      (is string= "field_name" (caar dcode))))
  
  (named "UNIT: roundtrip encoding/decoding of empty field(s)"
    ;; This was revealed in the property testing that
    ;; given '(("") ("") (NIL NIL)) leads to '(NIL NIL ("" ""))
    ;; I modified csv-*code to check for NILs and return ("")
    (let* ((sample '(("(7c~6JwERr9E8faA5m6-}Ai;+aNlMTu)N%|cy%;y") (""))))
      (is= sample (csv-decode (csv-encode sample)))))

  (named "one column CSV files are inherently ambiguous"
    (is= (concatenate 'string
		      (string #\NEWLINE)
		      (string #\NEWLINE))
	 (csv-encode '(("") (""))))))

;;; PROPERTIES

(defun csv-pbts ()
  (named "roundtrip encoding/decoding"
    (for-all ((maps csv-source))
      (is= maps (csv-decode (csv-encode maps))))))


;;; HELPERS



;;; GENERATORS
;; Custom generators from package BDAY/TESTS.GENERATORS
