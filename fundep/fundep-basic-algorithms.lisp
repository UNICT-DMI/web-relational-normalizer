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

;; almost all the following functions take as optional argument a stream
;; when present, it is sometimes used to output explanations about the operation being performed



;(defun kill-on-timeout (start-time thread request acceptor)
;  (declare (ignore start-time thread request acceptor)))


  
;; (defun XG+Closure (X rho F) ;  -> set-attribute
;;   "calculate the closure of X with respect to F in all the schemas rhos"
;;   (let* ((XGPIU X)
;; 	 (schemas rho)
;; 	 (non-modified nil))
;;     (do () (non-modified (mk-att-s XGPIU))
;;       (setq non-modified t)
;;       (dolist (s schemas)
;; 	(let* ((newAttributes (attr-set-closure (mk-att-s (intersection XGPIU s)) F))
;; 	       (newXGPIU (att-s-union (intersection newAttributes s) XGPIU)))
;; 	  (when (> (length newXGPIU) (length XGPIU))
;; 	    (setq non-modified nil
;; 		  XGPIU newXGPIU)))))))

;; (defun check-dep-preservation (rho F) ;  -> bool
;;   "check if the decomposition rho preserves all the dependencies of F"
;;   (dolist (d F)
;;     (when (not (subsetp (determinate d) (XG+Closure (determinant d) rho F)))
;;       (return-from check-dep-preservation nil)))
;;   t)

;; (defun classify-attributes ((s rel-schema)) ; -> set-attribute x 5
;;   "Classify the attributes of rel-schema s"
;;   (let* ((t1 (attrs s))
;; 	 (f1 (fdeps s))
;; 	 (deps f1)
;; 	 (attrs t1)
;; 	 (lha (reduce #'union deps :initial-value nil :key (lambda(d) (represent (determinant d)))))
;; 	 (rha  (reduce #'union deps :initial-value nil :key (lambda(d) (represent (determinate d))))))
;;     (values-list (mapcar #'f-mk-att-s
;; 			(setize (list lha rha (set-difference lha rha)
;; 			       (set-difference rha lha)
;; 			       (set-difference attrs (union lha rha))))))))

;; (defgeneric only-left-side-attributes(s)
;;   (:documentation "attributes only in left side of dependencies of rel-schema s"))

;; (defmethod only-left-side-attributes((s rel-schema)) ; -> set-attribute
;;   (multiple-value-bind (r1 r2 r3 r4 r5) (classify-attributes s)
;;     (declare (ignore r1 r2 r4 r5))
;;     r3))


