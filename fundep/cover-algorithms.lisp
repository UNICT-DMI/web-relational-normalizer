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

(defun reduce-right (d) ; -> set-fun-dep
  "given X → A1A2...An returns {X→A1  X→A2  ... X→An}"
  (declare (type functional-dependency d))
  (let ((rh (determinate d))
	(lh (determinant d)))
    (mk-f-dep-s (if (= (length rh) 1)
		    (list d)
		    (mapcar (lambda(x) (mk-f-dep lh (list x))) rh)))))

(defun reduce-left (d F &optional st1) ; -> fun-dep
  "eliminate estraneous attributes from d (with respect to F)"
  (declare (type functional-dependency d) (type functional-dependency-set F) (type mystream st1))
  (let* ((rh (determinate d))
	 (lh (determinant d))
	 (lh1 nil)
	 (res lh))
    (if (= (length lh) 0)   ;; test for dependencies ∅ → A
	d
	(dolist (l lh (mk-f-dep res rh))
	  (setq lh1 (remove l res))
	  (when (fun-dep-in-closure (mk-f-dep lh1 rh) F)
	    (expl st1 "In ~/fundep:f-dep/, ~a is estraneous since {~/fundep:att-s/}+ = ~a. " d l lh1 (attr-set-closure lh1 F))
	    (setq res lh1))))))

(defun sch-canonical-set (S &optional st1)
  "compute the canonical set of the dependencies of the schema S"
  (declare (type schema S) (type mystream st1))
  (canonical-set (sch-f-dep-s S) st1))

(defun canonical-set (F &optional st1)        ; -> set-fun-dep
  "compute the canonical set of the dependencies F"
  (declare (type functional-dependency-set F) (type mystream st1))
  (expl st1 "Computing the canonical set of the dependencies:~%~%~/fundep:f-dep-s/~%~%" F)
  (let* ((deps (loop for d in F
		  append (reduce-right d))) ; deps is a list of dep. X -> A
	 (res (progn (expl st1 "These are the dependencies with a single attribute on the rigth:~%~%~/fundep:f-dep-s/~%~%"
			   (mk-f-dep-s deps))
		     deps))
	 (d1 nil)
	 (deps1 (dolist (d deps (reverse res))
		  (setq d1 (reduce-left d (mk-f-dep-s res) st1))
		  (when (< (length (determinant d1)) (length (determinant d)))
		    (expl st1 "Replacing ~/fundep:f-dep/ with ~/fundep:f-dep/~%~%" d d1))
		  (setq res (remove d res))
		  (setq res (cons d1 res))))
	 (res1 deps1)
	 (F2 (progn (expl st1 "After the elimination of extraneous attributes, the dependendencies are:~%~%~/fundep:f-dep-s/~%~%" res1)
		    nil)))
    (dolist (fm deps1 
	     (let ((res (mk-f-dep-s res1)))
	       (expl st1 "No other dependency can be removed.~%~%The final set of dependencies is:~%~%~/fundep:f-dep-s/~%~%" res)
	       res))
      (setq F2 (remove fm res1))
      (when (fun-dep-in-closure fm (mk-f-dep-s F2))
	(expl st1 "Removing the redundant dependency: ~/fundep:f-dep/~%~%" fm)
	(setq res1 F2)))))
