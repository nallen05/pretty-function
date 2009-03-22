;; -*- Mode: LISP;  Syntax: COMMON-LISP; Package: CL-USER; -*- 
;; Sun Oct 21 17:22:01 2007 by Nick Allen <nallen05@gmail.com>
;; pretty-function-test.lisp


;; note: these tests require ptester, see http://www.cliki.net/ptester

;; note: I had trouble forcing GC during this file on some lisps, so to check
;;       if the pretty functions are really being garbage collected you have
;;       to do it yourself

;;          1. run the tests in this file
;;          2. force a full garbage collection
;;          3. run: (print-pretty-function-table)

;;             if you don't see anything like  #<TEST-1234567> or
;;             #<test-pretty-function-printer> then the functions from these
;;             tests were garbage collected


(defpackage :pretty-function.test
  (:use :cl :pretty-function))

(in-package :pretty-function.test)

(setf ptester:*break-on-test-failures* t)



;suport

(defparameter *supported* nil)

(ptester:test #+(or allegro clisp cmu lispworks mcl sbcl) t
	      #-(or allegro clisp cmu lispworks mcl sbcl) nil
	      (setf *supported*
		    (not (not (find "pretty-function" *modules* :test #'string-equal)))))

(ptester:test *supported* *pretty-function-printing-supported-p*)



;enabling

;should this work on a temporary table?

(if *supported*
    (ptester:test-no-warning (enable-pretty-function-printing))
    (ptester:test-warning (enable-pretty-function-printing)))



;fn-printer

(defparameter *printer-string*  "#<test-pretty-function-printer>")

(defparameter *printer*
  (lambda (s) (write-string *printer-string* s)))

(let ((fn (lambda () (print 5))))

  (ptester:test nil (get-function-printer fn))

  (ptester:test-no-error (setf (get-function-printer fn) *printer*))

  (ptester:test *printer* (get-function-printer fn))

  (ptester:test t
 		  (progn
		    #+(or allegro clisp lispworks mcl)
		    (block finding
		      (maphash (lambda (fn printer)
				 (declare (ignore fn))
				 (if (eq *printer* printer)
				     (return-from finding t)))
			       pretty-function::*weak-fn-ht*)
		      nil)

		    #+(or cmu sbcl)
		    (not (not (find *printer*
				    pretty-function::*weak-fn-alist*
				    :key #'rest))))))



;make sure function printer prints correctly

(if *supported*
    (ptester:test *printer-string*
		  (with-output-to-string (out)
		    (write (with-function-printer *printer*
			     (lambda () (print 5)))
			   :stream out
			   :pretty t))
		  :test #'string=))



;WITH-FUNCTION-PRINTER

(ptester:test (if *supported*
		  *printer*)
	      (get-function-printer (with-function-printer *printer*
				      (lambda () (print 5)))))



;named-lambda*

(ptester:test *supported*
	      (not (not (search "TEST-1234567"
				(with-output-to-string (out)
				  (write (named-lambda* (concatenate 'string
								     "TEST-"
								     "123"
								     "4567")
					     () (print 5))
					 :stream out
					 :pretty t))))))



;named-lambda

(ptester:test *supported*
	      (not (not (search "TEST-1234567"
				(with-output-to-string (out)
				  (write (named-lambda "TEST-1234567" () (print 5))
					 :stream out
					 :pretty t))))))