;;;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; base: 10; -*-
;;;;;
;;;;; Adapted from dave lambert's "porky"
;;;;;
;;;;; Portions Copyright © 2009 The Open University
;;;;;

(in-package :cl-user)

(asdf:defsystem :gs.ebu.wilbur.turtle
  :serial t
  :depends-on (:de.setf.wilbur)
  :components ((:static-file "gs.ebu.wilbur.turtle.asd")
                (:file "package")
                (:file "utilities")
                (:file "lexer")
                (:file "ast")
                (:file "target")
                (:file "grammar")
                (:file "turtle")
                (:file "test")))
