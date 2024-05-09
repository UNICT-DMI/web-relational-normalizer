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

(defun is-trivial (d) ;  -> bool
  "check if the functional dependency d is trivial"
  (declare (type functional-dependency d))
  (subsetp (determinate d) (determinant d)))

(defun fun-dep-in-closure (d F &optional st1) ; -> bool
  "check if the dependency d belongs to F+"
  (declare (type functional-dependency d) (type functional-dependency-set F) (type mystream st1))
  (let* ((dant (determinant d))
	 (clos (attr-set-closure dant F))
	 (det (determinate d))
	 (res (subsetp det clos)))
    (expl st1 "~/fundep:att-s/ is~:[~; not~] in the closure of ~/fundep:att-s/, which is equal to ~/fundep:att-s/~%~%" det (not res) dant clos)
    res))

(defun sch-a-implies-b (s d &optional st1)
  "check if the dependencies of the schema s implies the set of dependencies d"
  (declare (type schema s) (type cons d) (type mystream st1))
  (a-implies-b (sch-f-dep-s s) (car d) st1))

(defun a-implies-b (a b &optional st1) ; -> bool
"check if the set of depencencies a implies the set of dependencies b,
 returns two values: t or nil, and the dependencies not implied from a"
(declare (type functional-dependency-set a b) (type mystream st1))
  (let ((bad-deps nil))
    (dolist (dep b)
      (when (not (fun-dep-in-closure dep a))
	(push dep bad-deps)))
    (when bad-deps
      (if (= (length b) (length bad-deps))
    	  (if (cdr bad-deps)
    	      (expl st1 "no dependency in ~%~/fundep:f-dep-s/~% can be derived from:~%~%~/fundep:f-dep-s/~%~%"
    		    (mk-f-dep-s bad-deps) a)
    	      (expl st1 "the dependency cannot be derived from:~%~%~/fundep:f-dep-s/~%~%"
    		    a))
    	  (expl st1 "the dependencies:~%~%~/fundep:f-dep-s/~%~%cannot be derived from:~%~%~/fundep:f-dep-s/~%~%"
    		(mk-f-dep-s bad-deps) a)))
    (values (not bad-deps) bad-deps)))

(defun sch-a-equivalent-b (S d &optional st1)  ; -> bool
  "check if the set of dependencies of the schema S are equivalent to the set of dependencies d"
  (declare (type schema S) (type cons d) (type mystream st1))
  (a-equivalent-b (sch-f-dep-s s) (car d) st1))
   
(defun a-equivalent-b (a b &optional st1)  ; -> bool
  "check if a and b are equivalent set of dependencies"
  (declare (type functional-dependency-set a b) (type mystream st1))  
  (and (a-implies-b a b st1) (a-implies-b b a st1)))

(defun sch-project-set-fun-dep (S t1 &optional st1) ; -> set-fun-dep
  "project the functional dependencies of schema S over the attributes t1"
  (declare (type schema S) (type attribute-set t1) (type mystream st1))
  (project-set-fun-dep t1 (sch-f-dep-s S) st1))

(defun project-set-fun-dep (z f &optional st1)
  "Gottlob’s algorithm to return a closure of a projection of a set of dependency f
   over a set of attributes z"
  (declare (type attribute-set z) (type functional-dependency-set f) (type mystream st1))
  (expl st1 "Algorithm used: Gottlob, G. “Computing Covers for Embedded Functional Dependencies,” PODS '87 Proceedings, pp. 58–69, ACM Press, 1987. doi:10.1145/28659.28665.~%~%")
  (let ((g f)
	(x (set-difference (f-dep-s-att-s f) z))
	(a nil)
	(res nil)
	(newg nil))
    (expl st1 "Starting the projection of functional dependencies. Eliminating the attributes: ~/fundep:att-s/.~%~%" x)
    (loop while x
       do (setf g (canonical-set g)
		a (car x)
		x (cdr x)
		res nil)
       do (expl st1 "Current canonical cover: ~%~/fundep:f-dep-s/;~% now eliminating attribute ~a~%~%" g a)
       do (loop for (y (a1)) in g
	     when (eq a1 a)
	     do (loop for (w (b)) in g
		   when (member a w)
		   do (let ((h (mk-f-dep (union y (remove a w)) (list b))))
			(when (not (is-trivial h))
			  (push h res)))))
       do (setf newg g)
       do (loop for f in g
	     when (member a (f-dep-att-s f))
	     do (setf newg (remove f newg :test 'equal)))
       do (setf g (union newg res)))
    (expl st1 "Final canonical cover:~%~/fundep:f-dep-s/;" g)
    g))
