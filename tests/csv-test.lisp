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

  (named "UNIT: one column CSV files are inherently ambiguous"
	 (is= (concatenate 'string
			   (string #\NEWLINE)
			   (string #\NEWLINE))
	      (csv-encode '(("") ("")))))

  (named "UNIT: one record per line"
	 (is= '(("aaa" "bbb" "ccc")
		("zzz" "yyy" "xxx"))
	      (csv-decode "aaa,bbb,ccc
zzz,yyy,xxx
")))

  (named "UNIT: optional trailing CRLF"
	 (is= '(("aaa" "bbb" "ccc")
		("zzz" "yyy" "xxx"))
	      (csv-decode "aaa,bbb,ccc
zzz,yyy,xxx")))

  (named "UNIT: double quotes"
	 (is= '(("aaa" "bbb" "ccc")
		("zzz" "yyy" "xxx"))
	      (csv-decode "\"aaa\",\"bbb\",\"ccc\"
zzz,yyy,xxx")))

  (named "UNIT: escape CRLF"
	 (is= '(("aaa" "b
bb" "ccc")
		("zzz" "yyy" "xxx"))
	      (csv-decode "\"aaa\",\"b
bb\",\"ccc\"
zzz,yyy,xxx")))

  (named "UNIT: double quote escaping"
	 ;; Since it's decided headeres are mandatory, this test adds a line
	 ;; with empty values 'CRLF,,' to the example from the RFC
	 (is= '(("aaa" "b\"bb" "ccc")
		("" "" ""))
	      (csv-decode "\"aaa\",\"b\"\"bb\",\"ccc\"
,,"))))

;;; PROPERTIES

(defun csv-pbts ()
  (named "roundtrip encoding/decoding"
    (for-all ((maps csv-source))
      (is= maps (csv-decode (csv-encode maps))))))


;;; HELPERS



;;; GENERATORS
;; Custom generators from package BDAY/TESTS.GENERATORS
