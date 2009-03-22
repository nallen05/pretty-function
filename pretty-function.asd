;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; -*-

(in-package :cl-user)

(defpackage :pretty-function.system
  (:use :cl :asdf))

(in-package :pretty-function.system)

(asdf:defsystem :pretty-function
    :version "0.1"
    :components ((:file "pretty-function")))
