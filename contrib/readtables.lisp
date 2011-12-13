;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;
;;;;; named readtables
;;;;;


(in-package :templeton)

(defreadtable :|standard|
  (:merge :standard))

(defreadtable :|node|
  (:macro-char #\! #'wilbur::inline-node-reader t))

(defreadtable :|literal|
  (:macro-char #\# :dispatch)
  (:dispatch-macro-char #\# #\" #'wilbur::inline-literal-reader))

(defreadtable :|wilbur|
  (:merge :|standard|)
  (:merge :|node|)
  (:merge :|literal|))

(defreadtable :|templeton|
  (:merge :|wilbur|))


