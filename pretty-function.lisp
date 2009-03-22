;; -*- Mode: LISP;  Syntax: COMMON-LISP; Package: CL-USER; -*- 
;; Sun Oct 21 11:55:20 2007 by Nick Allen <nallen05@gmail.com>
;; pretty-function.lisp

;;  Copyright (c) 2007, Streamtech (http://streamtech.nl)
;;  All rights reserved.

;;  Redistribution and use in source and binary forms, with or without
;;  modification, are permitted provided that the following conditions are met:
;;      * Redistributions of source code must retain the above copyright
;;        notice, this list of conditions and the following disclaimer.
;;      * Redistributions in binary form must reproduce the above copyright
;;        notice, this list of conditions and the following disclaimer in the
;;        documentation and/or other materials provided with the distribution.
;;      * Neither the name of the <organization> nor the
;;        names of its contributors may be used to endorse or promote products
;;        derived from this software without specific prior written permission.

;;  THIS SOFTWARE IS PROVIDED BY STREAMTECH ``AS IS'' AND ANY
;;  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;  DISCLAIMED. IN NO EVENT SHALL <copyright holder> BE LIABLE FOR ANY
;;  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;;  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;;  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; function: ENABLE-PRETTY-FUNCTION-PRINTING (&optional (priority 0) (table *print-pprint-dispatch*))

;;       modifies the pprint dispatch table `TABLE' to pprint functions using their pretty
;;       function printer (see GET-FUNCTION-PRINTER)

;;       this means that you can make all the pretty functions you want, but until you run

;;          (enable-pretty-function-printing)

;;      they wont print differently in the REPL or in stack traces!

;; macro: NAMED-LAMBDA (name lambda-list &body body) 

;;     like LAMBDA except the resultant function is written as #<named-lambda NAME>
;;     when pprinted to a stream when pretty printing functions is enabled (see
;;     ENABLE-PRETTY-FUNCTION-PRINTING) and *PRINT-PRETTY* is T

;;     `NAME' is not evaluated.

;;     caveat: unlike lambda, NAMED-LAMBDA cannot be used as the first element of a list

;;        ((lambda (a b) (+ a b)) 5 6) => 11

;;        ((named-lambda mistake (a b) (+ a b)) 5 6) ==> THROWS AN ERROR

;; macro: NAMED-LAMBDA* (name-form lambda-list &body body)

;;     like `NAMED-LAMBDA' except NAME-FORM is evaluated

;; macro: WITH-FUNCTION-PRINTER (printer fn-form)

;;     returns the result of evaluating `FN-FORM', which should evaluate to a function.

;;     this function will now be pprinted with `PRINTER' when written to a stream and pretty
;;     printing functions is enabled (see ENABLE-PRETTY-FUNCTION-PRINTING) and *PRINT-PRETTY*
;;     is T.

;;     `PRINTER' should be a lambda expression or name of a function that takes
;;     `STREAM' as its only argument and prints a pretty representation of `FUNCTION' to
;;     `STREAM'

;;         CL-USER> (enable-pretty-function-printing)

;;         CL-USER> (let ((n 0))
;;  	              (setf x (with-function-printer (lambda (s) (format s "#<counter ~A>" n))
;; 		                (lambda () (incf n)))))

;;         #<counter 0>

;;         CL-USER> (funcall x)
;;         1

;;         CL-USER> x
;;         #<counter 1>

;; variable: *PRETTY-FUNCTION-PRINTING-SUPPORTED-P*

;;       is T on implimentations that support pretty function printing, NIL on the rest

;; function: PRINT-PRETTY-FUNCTION-TABLE (&optional (stream *standard-output*))

;;       prints all known pretty functions

;; function: CLEAR-PRETTY-FUNCTION-TABLE (&optional (stream *standard-output*))

;;       turns all known pretty functions into normal, non-pretty functions.

;;       individual pretty functions can be turned back into normal functions by SETF-ing their
;;       GET-FUNCTION-PRINTER to NIL

;; function: GET-FUNCTION-PRINTER (function)

;;     returns the printer that is responsible for printing function printing `FUNCTION'. returns NIL if
;;     `FUNCTION' is not a pretty function.

;;     you can turn a non-pretty function into a pretty function by SETF-ing GET-FUNCTION-PRINTER to a
;;     an acceptible printer (see WITH-FUNCTION-PRINTER). you can also turn a pretty function back into
;;     a normal function by SETF-ing its GET-FUNCTION-PRINTER to NIL


(defpackage :pretty-function
  (:use :cl)
  (:export ;; enabling pretty function printing
	   #:enable-pretty-function-printing

	   ;; using
	   #:named-lambda
	   #:named-lambda*
	   #:with-function-printer

	   ;; debugging
           #:*pretty-function-printing-supported-p*
	   #:print-pretty-function-table
	   #:clear-pretty-function-table
	   #:get-function-printer))


(in-package :pretty-function)


;supported implimentations

(defparameter *pretty-function-printing-supported-p*
 
  #+(or allegro clisp cmu lispworks mcl sbcl) t

  #-(or allegro clisp cmu lispworks mcl sbcl) nil
)


;enabling pretty function printing

(defvar *pretty-function-printing-enabled-p* nil)

(defun enable-pretty-function-printing (&optional (priority 0) (table *print-pprint-dispatch*))

  #+(or allegro clisp cmu lispworks mcl sbcl)
  (progn
    (set-pprint-dispatch 'function '.print-pretty-function priority table)
    (setf *print-pretty* t)
    (let ((% (not *pretty-function-printing-enabled-p*)))
      (setf *pretty-function-printing-enabled-p* t)
      %))

    #-(or allegro clisp cmu mcl lispworks sbcl)
    (warn "pretty function printing is not supported on ~A ~A"
	  (lisp-implementation-type)
	  (lisp-implementation-version)))

(defun .print-pretty-function (s fn)
  (let ((printer (get-function-printer fn)))
    (if printer
	(funcall (coerce printer 'function) s)
	(let ((*print-pretty* nil))
	  (write fn :stream s)))))


;the machinary

#+allegro
 (defvar *weak-fn-ht* (make-hash-table :test #'eq
				       :weak-keys t))

#+(or clisp mcl)
(defvar *weak-fn-ht* (make-hash-table :test #'eq
				      :weak :key))

#+lispworks
(defvar *weak-fn-ht* (make-hash-table :test #'eq
				      :weak-kind :key))

#+(or cmu sbcl)
(progn

  (defvar *weak-fn-alist* nil)

  (defvar *weak-fn-alist-outdated-p* nil)

  (defun .outdate-weak-fn-alist ()
    (setf *weak-fn-alist-outdated-p* t)))

#+cmu
(progn

  (defun .update-weak-fn-alist ()
    (if *weak-fn-alist-outdated-p*
	(setf *weak-fn-alist* (remove-if-not (lambda (a)
					       (and (rest a)
						    (ext:weak-pointer-value (first a))))
					     *weak-fn-alist*)
	      *weak-fn-alist-outdated-p* nil)))

  (pushnew '.update-weak-fn-alist ext:*after-gc-hooks*))

#+sbcl
(progn

  (defun .update-weak-fn-alist ()
    (setf *weak-fn-alist* (remove-if-not (lambda (a)
					   (and (rest a)
						(sb-ext:weak-pointer-value (first a))))
					 *weak-fn-alist*)
	  *weak-fn-alist-outdated-p* nil))

  (pushnew '.update-weak-fn-alist sb-ext:*after-gc-hooks*))

;WITH-FUNCTION-PRINTER macro

(defmacro with-function-printer (printer fn)

  #+(or allegro lispworks mcl clisp)
  `(let ((p ,printer)
	 (f ,fn))
    (setf (gethash f *weak-fn-ht*) p)
    f)

  #+cmu
  `(let ((p ,printer)
	 (f ,fn))
    (let ((w (extensions:make-weak-pointer f)))
      (push (cons w p) *weak-fn-alist*)
      (extensions:finalize f #'.outdate-weak-fn-alist)
      f))

  #+sbcl
  `(let ((p ,printer)
	(f ,fn))
    (let ((w (sb-ext:make-weak-pointer f)))
      (push (cons w p) *weak-fn-alist*)
      (sb-ext:finalize f #'.outdate-weak-fn-alist)
      f)))



;NAMED-LAMBDA and NAMED-LAMBDA* macros

(defmacro named-lambda (name lambda-list &body body)

  #+(or allegro clisp cmu lispworks mcl sbcl)
  `(named-lambda* ',name ,lambda-list ,@body)

  #-(or allegro clisp cmu lispworks mcl sbcl)
  `(lambda ,lambda-list ,@body))

(defmacro named-lambda* (name-form lambda-list &body body)

  #+(or allegro clisp cmu lispworks mcl sbcl)
  `(with-function-printer (lambda (s) (format s "#<named-lambda ~A>" ,name-form))
    (lambda ,lambda-list ,@body))

  #-(or allegro clisp cmu lispworks mcl sbcl)
  `(progn ,name-form
         (lambda ,lambda-list ,@body)))

;FUNCTION-PRINTER fn

(defun get-function-printer (fn)

  #+(or allegro lispworks mcl clisp) (values (gethash fn *weak-fn-ht*))

  #+sbcl (rest (assoc fn *weak-fn-alist* :key #'sb-ext:weak-pointer-value))

  #+cmu (rest (assoc fn *weak-fn-alist* :key #'ext:weak-pointer-value))

  #-(or allegro clisp cmu lispworks mcl sbcl) nil
)

(defsetf get-function-printer (fn) (printer)

  #+(or allegro clisp lispworks mcl)
  `(let ((p ,printer)
	 (f ,fn))
    (if p
	(setf (gethash f *weak-fn-ht*) p)
	(remhash f *weak-fn-ht*))
    p)

  #+cmu
  `(let ((f ,fn)
	 (p ,printer))
    (let ((a (assoc f
		     *weak-fn-alist*
		     :key #'ext:weak-pointer-value)))
      (cond (a (setf (rest a) p)
	       (if (null p)
		   (.outdate-weak-fn-alist)))
	    (t (push (cons (ext:make-weak-pointer f) p)
		     *weak-fn-alist*)	  
	       (ext:finalize f #'.outdate-weak-fn-alist)))
      p))

  #+sbcl
  `(let ((f ,fn)
	 (p ,printer))
    (let ((a (assoc f
		    *weak-fn-alist*
		    :key #'sb-ext:weak-pointer-value)))
      (cond (a (setf (rest a) p)
	       (if (null p)
		   (.outdate-weak-fn-alist)))
	    (t (push (cons (sb-ext:make-weak-pointer f) p)
		     *weak-fn-alist*)
	       (sb-ext:finalize f #'.outdate-weak-fn-alist)))
      p))

  #-(or allegro clisp cmu lispworks mcl sbcl)
  `(progn ,fn ,printer)
)

;PRINT-PRETTY-FUNCTION-TABLE

(defun print-pretty-function-table (&optional (stream *standard-output*))

  #+(or allegro clisp lispworks mcl)
  (let ((n (hash-table-count *weak-fn-ht*)))
    (format stream "~%there are ~A pretty function~p in the pretty function table~%" n n)
    (maphash (lambda (fn printer)
	       (declare (ignore fn))
	       (funcall printer stream)
	       (terpri stream))
	     *weak-fn-ht*))

  #+(or cmu sbcl)
  (let ((n (length *weak-fn-alist*)))
    (format stream "~%there are ~A pretty function~p in the pretty function table~%" n n)
    (mapc (lambda (%)
	    (funcall (rest %) stream)
	    (terpri stream))
	  *weak-fn-alist*))

  #-(or allegro clisp cmu lispworks mcl sbcl)
  (warn "The implimentation you are using does not support pretty function printing")

  (values))

;CLEAR-PRETTY-FUNCTION-TABLE
	
(defun clear-pretty-function-table (&optional (stream *standard-output*))

  #+(or allegro clisp lispworks mcl)
  (let ((n (hash-table-count *weak-fn-ht*)))
    (if (zerop n)
	(format stream "The pretty function table is empty!~%")
	(format stream "~A pretty function~p deleted from the pretty function table~%" n n))
    (clrhash *weak-fn-ht*))


  #+(or cmu sbcl)
  (let ((n (length *weak-fn-alist*)))
    (if (zerop n)
	(format stream "The pretty function table is empty!~%")
	(format t "~A pretty function~p deleted from the pretty function table~%" n n))
    (setf *weak-fn-alist* nil))

  #-(or allegro clisp cmu lispworks mcl sbcl)
  (warn "The implimentation you are using does not support pretty function printing")

  (values))

#+(or allegro clisp cmu mcl lispworks sbcl)
(provide :pretty-function)