;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :templeton)
(in-suite test)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PP-TABLE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def (function e) pp-table (lines &key (stream t) (fs " |") (rs #\Newline)
                             (underline #\-) (before #\Newline) (after ""))
  "example:
                  (pp-table '((peace love understanding)
                              (34.1 33.4 324.2)
                              (334 222222 33)) :stream t)
   output:

                  PEACE |   LOVE | UNDERSTANDING
                  ----- | ------ | -------------
                   34.1 |   33.4 |         324.2
                    334 | 222222 |            33"

  (let* ((header (first lines))
          (data   (rest  lines))
          (cols   (swank::transpose-lists lines))
          (widths (mapcar #'(lambda (col)
                              (reduce #'max col :key #'(lambda (item)
                                                         (length (princ-to-string item)))))
                    cols))
          (sep "")
          (fmt   (with-output-to-string (s)
                   (dolist (width widths)
                     (format s "~a~~~a<~a~~>" sep (1+ width) "~a")
                     (setf sep fs))
                   (format s "~a" rs))))
    (format stream "~a" before)
    (apply #'format `(,stream  ,fmt ,@header))
    (setf sep "")
    (dolist (w widths)
      (format stream "~a ~a" sep (make-string w :initial-element underline))
      ;;(nchars w underline))
      (setf sep fs))
    (format stream "~a" rs)
    (dolist (line data)
      (apply #'format `(,stream  ,fmt ,@line)))
    (format stream "~a" after)))


(def test pp-table-output.0 ()
  (let ((tmp (with-output-to-string (s)
               (pp-table '((peace love understanding)
                        (34.1 33.4 324.2)
                        (334 222222 33)) :stream s))))

    (is (string-equal tmp 
"
 PEACE |   LOVE | UNDERSTANDING
 ----- | ------ | -------------
  34.1 |   33.4 |         324.2
   334 | 222222 |            33
"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PP-GRID
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def (function e) pp-grid (table &optional (stream t))
  "printed grid output format for list-of-lists data
   example: (pp-grid '((one two three)(1 2 3)(11 22 33)))

   .-------------.
   |ONE|TWO|THREE|
   |-------------|
   |1  |2  |3    |
   |-------------|
   |11 |22 |33   |
   '-------------'"
  
  (let* ((table-as-string
	  (mapcar (lambda (x) (mapcar #'prin1-to-string x))
		  table))
	 (column-widths
	  (apply #'mapcar
		 (lambda (&rest column)
		   (apply #'max (mapcar #'length column)))
		 table-as-string))
	 (first-row (format nil ".~a.~%" (make-string (+ (apply #'+ column-widths)
							 (length column-widths) -1)
						      :initial-element #\-)))
	 (intermediate-row (substitute #\| #\. first-row))
	 (last-row (substitute #\' #\. first-row))
	 (printed-first-row nil))
    (format stream first-row)
    (dolist (row table-as-string)
      (if printed-first-row (format stream intermediate-row) (setq printed-first-row t))
      (mapcar (lambda (item column-width)
		(format stream
			(format nil "|~a~a~a" "~a~" (- column-width (length item)) ",1@t") item))
	      row column-widths)
      (format stream "|~%"))
    (format stream last-row))
  (values))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PP-TABLE*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def (function e) pp-table* (stream headings &rest columns)
  "Alternative table-printing utility. An example:

 (pp-table* *standard-output*
            '(left-column middle right)
            '(1 2 3 123)
            '(12 23 34 1234)
            '(123 234 345 12345))

 LEFT-COLUMN | MIDDLE | RIGHT |
       1     |    12  |   123 |
       2     |    23  |   234 |
       3     |    34  |   345 |
     123     |  1234  | 12345 |"
  
  (setf headings (mapcar #'princ-to-string headings))
  (when (some #'null columns)
    (error "The ~dth column has no data" (position nil columns)))
  (setf columns (mapcar #'(lambda (l)
			    (mapcar #'princ-to-string l))
			columns))
  (let* ((elt-widths  (mapcar #'(lambda (l) (reduce #'max l :key #'length))
			      columns))
	 (col-widths  (mapcar #'(lambda (elt head)
				  (+ 2 (max elt (length head))))
			      elt-widths
			      headings)))
    (fresh-line stream)
    (mapc #'(lambda (cw head)
	      ;; center heading in a column of width col-width
	      (format stream "~v:@<~a~>|" cw head))
	  col-widths
	  headings)
    (terpri stream)
    (apply #'mapc
	   #'(lambda (&rest row)
	       (mapc #'(lambda (cw ew elt)
			 ;; right-justify elt in elt-width, centered in column
                         ;; The extra format string is necessary in Lispworks,
                         ;; because they don't implement ~v correctly.
			 (format stream (format nil "~~~d:@<~~~d@a~~>|" cw ew) elt))
		     col-widths
		     elt-widths
		     row)
	       (terpri stream))
	   columns)))



(def test pp-table*-output.0 ()
  (is (equalp
        (with-output-to-string (s)
          (pp-table* s
            '(left-column middle right)
            '(1 2 3 123)
            '(12 23 34 1234)
            '(123 234 345 12345)))
        
" LEFT-COLUMN | MIDDLE | RIGHT |
       1     |    12  |   123 |
       2     |    23  |   234 |
       3     |    34  |   345 |
     123     |  1234  | 12345 |
")))
