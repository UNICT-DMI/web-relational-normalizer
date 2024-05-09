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

(defun is-superkey (k1 s) ;  -> bool
  "check if k1 is a superkey of the schema s"
  (declare (type attribute-set k1) (type schema s))
  (subsetp (sch-att-s s) (attr-set-closure k1 (sch-f-dep-s s))))

(defun find-a-key (S &optional st1) ; -> set attribute
  "Find a key of rel-schema s"
  (declare (type schema S) (type mystream st1))
  (let* ((t1 (sch-att-s s))
	 (t2 t1)
	 (good t2))
    (expl st1 "Starting from the full set of attributes ~/fundep:att-s/ and trying to reduce it.~%~%" good)
    (dolist (att t2 (progn (expl st1 "~/fundep:att-s/ is a key." good) (mk-att-s good)))
      (when (is-superkey (mk-att-s  (remove att good)) s)
	(setq good (remove att good))
	(expl st1 "~/fundep:att-s/ is a superkey.~%~%" good)))))

(defun find-all-keys (s1 &optional st1) ; -> set-set-attribute
  "return all the keys of the relation schema s1"
  (declare (type schema s1) (type mystream st1))
  (let* ((atts1 (sch-att-s s1))
	 (fds1 (sch-f-dep-s s1))
	 (left-atts (let (res)
		      (dolist (f fds1)
			(setf res (att-s-union res (determinant f))))
		      res))
	 (right-atts (let (res)
		       (dolist (f fds1)
			 (setf res (att-s-union res (determinate f))))
		       res))
	 (no-right (att-s-set-difference atts1 right-atts))
	 (no-left (att-s-set-difference atts1 left-atts))
	 (left-right (att-s-intersection right-atts left-atts))
	 (possible-other-primes (att-s-set-difference (att-s-set-difference atts1 no-right) no-left))
	 (candidates (list (cons no-right left-right)))
	 (keys nil))
    (when no-right
      (expl st1 "The attributes {~/fundep:att-s/} do not appear in any determinate, so they must be present in <b>all</b> the keys.~%~%" no-right))
    (when no-left
      (expl st1 "The attributes {~/fundep:att-s/} do not appear in any determinant, so they <b>cannot</b> be present in a key.~%~%" no-left))
    (when possible-other-primes
      (expl st1 "Considering for addition to {~/fundep:att-s/} the attributes: {~/fundep:att-s/}.~%~%" no-right possible-other-primes))
    (loop while candidates
       for (x . y) = (first candidates)
       for lenx = (length x)
       do (setf candidates (rest candidates))
       when (notany (lambda(k) (and (> lenx (length k)) (att-s-subsetp k x))) keys)
       do (let ((xplus (attr-set-closure x fds1)))
	    (expl st1 "Computing {~/fundep:att-s/}+ = {~/fundep:att-s/}. " x xplus)
	    (if (att-s-subsetp atts1 xplus)
		(progn (push x keys) (expl st1 " Found key {~/fundep:att-s/}." x))
		(loop for (ai . ai+1ton) on (att-s-set-difference y xplus)
		   do (setf candidates (append candidates
					       (list (cons (att-s-cons ai x) ai+1ton))))))
	    (expl st1 "~%~%")))
    (let ((all-keys (mk-att-s-s keys)))
      (expl st1 "These are all the keys found: ~/fundep:att-s-s/~%~%" all-keys)
      all-keys)))
		  
(defun find-primes(s1 &optional st1) ; -> set-attribute
  "return the prime attributes of the relation schema s1"
  (declare (type schema s1) (type mystream st1))
  (let ((primes (att-s-s-union (find-all-keys s1 st1))))
    (expl st1 "The primes attributes are: ~/fundep:att-s/." primes)
    primes))

;; (defun find-all-keys (s1 &optional st1) ; -> set-set-attribute
;;   "return all the keys of the relation schema s1"
;;   (declare (ignore st1))
;;   (let ((k1 (time (old-find-all-keys s1)))
;; 	(k2 (time (new-find-all-keys s1))))
;;     (if (not (equal k1 k2))
;; 	(progn (print k1) (print k2) (error "****************"))
;; 	(progn (print k1) (print k2) k2))))

;; (defun old-find-all-keys (s1 &optional st1) ; -> set-set-attribute
;;   "return all the keys of the relation schema s1"
;;   (declare (ignore st1))
;;   (let* ((t1 (sch-att-s s1))
;; 	 (attrs t1)
;; 	 (res nil))
;;     (dolist (pk (powerset attrs) (mk-att-s-s res))
;;       (when (notany (lambda(k) (subsetp k pk)) res)
;; 	(when (is-superkey (mk-att-s pk) s1)
;; 	  (push pk res))))
;;     (mk-att-s-s res)))

