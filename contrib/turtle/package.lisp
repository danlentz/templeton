;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;
;;; Portions Copyright © 2009 The Open University

(in-package :cl-user)


(defpackage :wilbur.turtle
  (:nicknames :ttl)
  (:use :common-lisp
    :named-readtables
    :hu.dwim.def
    :hu.dwim.defclass-star
    :hu.dwim.stefil)
  (:export
    :turtle-db
    :bnodes-of
    :bnodes
    :intern-bnode
    :turtle-parser
    :parse-turtle-string
    :parse-turtle-stream
    :parse
    :run-all-tests))
