;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :templeton)
(in-suite test)

(def (constant) +uri-pattern+ "^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?"
  "The regular-expression from rfc3986. NB. the escaped '?'.")

(def (special-variable) /uri-scanner/ (ppcre:create-scanner +uri-pattern+))

(def (constant) +urn-pattern+ "^urn:(\\w{4}):([0-9a-fA-F]{8}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{12})$")

(def (special-variable) /urn-scanner/ (ppcre:create-scanner +urn-pattern+))

(defun urn-namestring-p (string)
  (ppcre:scan /urn-scanner/ string))

(defun make-unique-urn-namestring ()
  (unicly:uuid-as-urn-string nil (unicly:make-v4-uuid)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scanner Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def test test-urn-namestring-generator.0 (&optional (population-size 100) &aux population)
  (let ((population (loop :repeat population-size :collect (make-unique-urn-namestring))))
    (map nil
      #'(lambda (test-subject)
          (multiple-value-bind (begin end nid nss) (ppcre:scan  /urn-scanner/ test-subject)
            (is (eql    begin       0))
            (is (eql    end        45))
            (is (equalp nid   #(4  9)))
            (is (equalp nss   #(8 45)))))
      population)))



