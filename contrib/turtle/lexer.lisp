;;;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; base: 10; -*-
;;;;;
;;;;; turtle-lexer
;;;;;
;;;;; Maintainer:  <danlentz@gmail.com>
;;;;;
;;;;; Portions Copyright © 2009 The Open University

(in-package :wilbur.turtle)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lexical Tokenization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deflexer turtle-lexer

    (ignore :whitespace "[\\s\\r\\n]+" :multi-line-mode t)
    (ignore :comment "#.*\\r?\\n" :multi-line-mode t)

    (token :uriref "<[^ \\n>]*>")

    (token :qname "([a-zA-Z][a-zA-Z0-9_-]*)?:[a-zA-Z0-9_-]+")
    (token :prefix "[a-zA-Z][a-zA-Z0-9_-]*:")
    (token :nodeid "_[a-zA-Z0-9_-]*:[a-zA-Z0-9_-]+")

    (token :double "[+-]?[0-9]*\\.[0-9]+[eE][+-]?[0-9]+")
    (token :decimal "[+-]?[0-9]*\\.[0-9]+")
    (token :integer "[+-]?[0-9]+")

    (token :long-string "\"\"\"(\\\\\"|[^\"])*[^\\\\]?\"\"\""
           :multi-line-mode t)
    (token :string "\"(\\\\\"|[^\"\\n])*[^\\\\]?\"")

    (token :boolean "true|false")

    (token :period "\\.")
    (token :semicolon ";")
    (token :prefix ":")
    (token :comma ",")

    (token :open-bracket "\\[")
    (token :close-bracket "\\]")
    (token :open-paren "\\(")
    (token :close-paren "\\)")

    (token :a-for-type "a(?=\\s)")

    (token :^^ "\\^\\^")

    (token :@base "@base")
    (token :@prefix "@prefix")
    (token :language "@[a-z]+(-[a-z0-9]+)*"))

