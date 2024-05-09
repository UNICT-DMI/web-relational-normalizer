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

;(declaim (optimize (speed 3) (safety 0)))
(declaim (optimize (debug 3) (safety 3) (speed 0)))

(defun violates-boyce-codd (s &optional st1) ; -> bool
  "check if the schema s in in BCNF"
  (declare (type schema s) (type mystream st1))
  (let ((bad-deps nil)
	(f1 (sch-f-dep-s s)))
    (dolist (dep f1)
      (when (not (is-trivial dep))
	(when (not (is-superkey (determinant dep) s))
	  (push dep bad-deps))))
    (when bad-deps (expl st1 "~/fundep:f-dep-s/ in ~A" (mk-f-dep-s bad-deps) (sch-name s)))
    bad-deps))

(defun bcnf (s &optional st1)  ; -> set-set-attribute
  "Decompose the rel-schema s in Boyce Codd Normal Form"
  (declare (type schema s) (type mystream st1))
  (setf s (mk-sch (sch-name s) (sch-att-s s) (canonical-set (sch-f-dep-s s))))
  (let* ((rho (list s))
	 (ns (sch-name s))
	 (bads rho)
	 (goods nil))
    (resetCountName)
    (loop 
       while bads
       for sch = (pop bads)
       for atts = (sch-att-s sch)
       for deps = (sch-f-dep-s sch)
       for dep = (find-if (lambda(dep) (not (is-superkey (determinant dep) sch))) deps)
       if dep 
       do (progn (expl st1 "In: ~%~%~/fundep:sch/~% the determinant of ~/fundep:f-dep/ is not a superkey " sch dep)
		 (let* ((x (determinant dep))
			(xP (attr-set-closure x deps))
			(newschema1 (mk-sch (genCountName ns) 
					    xP 
					    (project-set-fun-dep xP deps)))
			(atts2 (mk-att-s (set-difference atts
							 (set-difference xP x))))
			(newschema2 (mk-sch  (genCountName ns) 
					     atts2
					     (project-set-fun-dep atts2 deps))))
		   (push newschema1 bads)
		   (push newschema2 bads)
		   (expl st1 "and so ~A is replaced by: ~%~%~/fundep:sch/~% and:~%~%~/fundep:sch/~%~%"
			 (sch-name sch) newschema1 newschema2)))
       else do (push sch goods))
    (setq goods (mk-sch-s goods))
    (expl st1 "The final result is:~%~%~/fundep:sch-s/~%~%" goods)
    goods))

(defun bcnftdp (S &optional st1)  ;  -> set-set-attribute
  "Decompose the rel-schema s in Boyce-Codd Normal Form"
  (declare (type schema S) (type mystream st1))
  (let* ((rho (bcnf s st1))
	 (alldeps (mk-f-dep-s (loop for s1 in rho appending (sch-f-dep-s s1)))))
    (multiple-value-bind (impl bads) (a-implies-b alldeps (sch-f-dep-s s) nil)
      (unless impl
	(expl st1 "The dependencies: ~%~%~/fundep:f-dep-s/~%~% are not preserved in the decomposition." bads))
      (list rho impl))))

(defun interpolate-numbers (l n)
  "starting from n, produces a list with the element of l interpolated with (n+1)"
  (declare (type list l) (type number n))
  (when l
      (cons n (cons (first l) (interpolate-numbers (rest l) (1+ n))))))

(defun bc-sbs (S hist &optional st1)  ; -> set-set-attribute
  "produces then next step to find the BCNF from schema S using the steps provided by the hist specified.
This is used to produce a BCNF step-by-step"
  (declare (type schema S) (type list hist) (type mystream st1))
  (setf s (mk-sch (sch-name s) (sch-att-s s) (canonical-set (sch-f-dep-s s))))
  (let* ((rho (list s))
	 (ns (sch-name s))
	 (bads rho)
	 (goods nil)
	 (dep nil)
	 (res nil))
    (resetCountName)
    (loop 
       while bads
       for sch = (pop bads)
       for atts = (sch-att-s sch)
       for deps = (sch-f-dep-s sch)
       for bad-deps = (remove-if (lambda(dep) (is-superkey (determinant dep) sch)) deps)
       if (null bad-deps)
	 do (push sch goods)
	 else if (null (second bad-deps))
                 do (setq dep (first bad-deps))
              else if (null hist)
                     do (return-from bc-sbs (format nil "Please choose a number corresponding to a dependency for splitting: ~%~{~A. ~/fundep:f-dep/~%~}" (interpolate-numbers bad-deps 1)))
		   else do (let ((ch (1- (pop hist))))
			     (when (or (< ch 0) (>= ch (length bad-deps)))
			       (setq ch 0))
			     (setq dep (nth ch bad-deps)))
	           end
	      end
              and do (progn (expl st1 "In: ~%~%~/fundep:sch/~%~% the determinant of ~/fundep:f-dep/ is not a superkey " sch dep)
			    (let* ((x (determinant dep))
				   (xP (attr-set-closure x deps))
				   (newschema1 (mk-sch (genCountName ns) 
							  xP 
							  (project-set-fun-dep xP deps)))
				   (atts2 (mk-att-s (set-difference atts
								      (set-difference xP x))))
				   (newschema2 (mk-sch (genCountName ns) 
							  atts2
							  (project-set-fun-dep atts2 deps))))
			      (push newschema1 bads)
			      (push newschema2 bads)
			      (expl st1 "and for this reason ~A is replaced by the two schemas: ~%~%~/fundep:sch/~% and:~%~%~/fundep:sch/~%"
				    (sch-name sch) newschema1 newschema2))))
    (setq goods (mk-sch-s goods))
    (setq res (format nil "*~/fundep:sch-s/~%~%" goods))
    (expl st1 "The final result is:~%~%~/fundep:sch-s/~%~%" goods)
    (multiple-value-bind (impl bads)
	(a-implies-b (mk-f-dep-s (loop for s1 in goods appending (sch-f-dep-s s1))) (sch-f-dep-s s) nil)
      (unless impl
	(expl st1 "The dependenc~@p:~%~%~/fundep:f-dep-s/~%~% ~[are~;is~;are~] not preserved in the decomposition."
	      (length bads) bads (length bads)))
      (values res impl))))

(defun bcnftsou (s &optional st1)
  "algoritmo di Tsou & Fischer"
  (declare (ignore st1))
  (let ((x (sch-att-s s))
	(ns (sch-name s))
	(f (canonical-set (sch-f-dep-s s)))
	(y nil)
	(z nil)
	(res nil))
    (loop while (not (set-equal x y))
       do (tagbody
	    (setf y x
		  z nil)
	   repeat
	     (when (> (length y) 2)
	       (loop for a in y
		  do (loop for b in (remove a y)
			when (member a (attr-set-closure (remove b (remove a y)) f))
			do (setf y (remove b y)
				 z a)
			and do (go repeat))))
	     (setf x (remove z x))
	     (push (mk-sch (genCountName ns) y nil) res)))
    res))

