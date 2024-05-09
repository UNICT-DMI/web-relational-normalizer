;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;; Copyright (c) 20015-2016, Renzo Orsini.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


;;; This file contains the main algorithms for functional dependencies
;;; and normalization

(in-package #:fundep)

(declaim (optimize (speed 3) (safety 0)))
;(declaim (optimize (debug 3) (safety 3) (speed 0)))

;;(declaim (ftype (function (schema attribute-set &optional stream) attribute-set) int-attr-set-closure))
(defun sch-attr-set-closure (S X &optional st1)
  "return the closure of the set of attributes X in the schema S"
  (declare (type schema S) (type attribute-set X) (type mystream st1))
  (attr-set-closure X (sch-f-dep-s S) st1))

(defun attr-set-closure (X F &optional st1) ; -> set-attribute
  "Compute the closure of a set of attributes X with respect to the functional dependencies F 
   with the “inefficient” method. In all tests done, this method is more efficient than the efficient one!"
  (declare (type attribute-set X) (type functional-dependency-set F) (type mystream st1))
  (let* ((X+ X)
	 (non-modified nil))
    (expl st1 "Starting computing the closure of (~/fundep:att-s/) with respect to:~%~%~/fundep:f-dep-s/:~%~%" X F)
    (expl st1 "The initial closure is: ~%~%(~/fundep:att-s/)~%~%" X+)
    (do () (non-modified
	    (progn
	      (expl st1 "No other dependencies can be used, so the final closure is: (~/fundep:att-s/)" (sort-symbols X+))
	      X+))
      (setq non-modified t)
      (dolist (dip F)
	(let ((lh (determinant dip))
	      (rh (determinate dip)))
	  (when (and (subsetp lh X+) (not (subsetp rh X+)))
	    (expl st1 "Using the dependency: ~/fundep:f-dep/~%~%" dip)
	    (setq X+ (att-s-union X+ rh)
		  non-modified nil)
	    (expl st1 "   now the closure is: (~/fundep:att-s/)~%~%" X+)))))))

