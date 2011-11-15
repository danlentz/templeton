;;;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; base: 10; -*-
;;;;;

(in-package :wilbur.turtle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEST-SUITE: GS.EBU.WILBUR.TURTLE 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-readtable extended)

(def suite (gs.ebu.wilbur :in root-suite :documentation ""))

(def suite*  (gs.ebu.wilbur.turtle :in gs.ebu.wilbur :documentation ""))

(def (function e) run-all-tests ()
  (gs.ebu.wilbur.turtle))


#|
TURTLE> (run-all-tests)
................................................................................
............................
#<test-run: 7 tests, 108 assertions, 0 failures in 6.778 sec>
TURTLE>
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unit Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defvar *ttl-source-data* (list "vann.ttl" "rss.ttl" "skos.ttl" "http.ttl"))

(defun test-file (name)
  (merge-pathnames name
    (merge-pathnames "tests/"
      (directory-namestring (asdf:system-definition-pathname :gs.ebu.wilbur.turtle)))))


(def test fixtures/ttl-source-data ()
  (dolist (file *ttl-source-data*)
    (is (probe-file (test-file file)))))

(def test parse/vann.0 ()
  (let ((ttlsrc (read-file-to-string (test-file "vann.ttl"))))
    (parse-turtle-string ttlsrc)))

(def test parse/rss.0 ()
  (let ((ttlsrc (read-file-to-string (test-file "rss.ttl"))))
    (is 16 (length (parse-turtle-string ttlsrc)))))

(def test parse/rss.1 ()
  (let* ((ttlsrc (read-file-to-string (test-file "rss.ttl")))
          (ttlast  (parse-turtle-string ttlsrc)))
    (is (pop ttlast) :TURTLE)
    (let ((prefixes (remove-if-not #'(lambda (sexp)
                                       (and (consp sexp) (eq (first sexp) :@PREFIX)))
                      ttlast)))
      (is (eql 5 (length prefixes)))
      (dolist (pfx '("rdf:" "owl:" "dc:" "xsd:" "rdfs:"))
        (is (find pfx prefixes :key #'second :test #'string-equal))))))
    
(def (test :auto-call nil) parse/prefixes (ttl-file &rest prefix-list)
  (let* ((ttlsrc (read-file-to-string (test-file ttl-file)))
          (ttlast  (parse-turtle-string ttlsrc)))
    (is (pop ttlast) :TURTLE)
    (let ((prefixes (remove-if-not #'(lambda (sexp)
                                       (and (consp sexp) (eq (first sexp) :@PREFIX)))
                      ttlast)))
      (is (eql (length prefix-list) (length prefixes)))
      (dolist (pfx prefix-list)
        (is (find pfx prefixes :key #'second :test #'string-equal)))))
  t)

(def test parse/prefixes.0 ()
  (dotimes (i 3)
    (is (parse/prefixes "rss.ttl"   "rdf:" "owl:" "dc:"  "xsd:"  "rdfs:"))
    (is (parse/prefixes "skos.ttl"  "rdf:" "dct:" "owl:" "rdfs:" "skos:"))
    (is (parse/prefixes "vann.ttl"  "rdf:" "dct:" "dc:"  "rdfs:" "foaf:" "vann:" "msg0:"))
    (is (parse/prefixes "http.ttl"  "rdf:" "owl:" "rdfs:"))))




;;(parse/prefixes.0)

;;;;;
;; Local Variables:
;; indent-tabs: nil
;; outline-regexp: ";;[;]+"
;; End:
;;;;;
