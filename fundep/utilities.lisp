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

;;; This file contains utility functions

(in-package #:fundep)

(declaim (optimize (speed 1) (safety 3)))

;;; utility type

(deftype mystream()
  "type union of null and stream, as parameter for functions
   that can write explanations about the algorithm on a stream"
  `(or null stream))

;;; name generators and decorators

(defun mkstr (&rest args)
  "make a string with the printed representation of all the arguments"
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defmacro expl (stream fmt-string &rest args)
  "when stream is not nil output args to stream with the format specified by fmt-string"
  `(when ,stream (format ,stream ,fmt-string ,@args)))

(defun symb (&rest args)
  "make a new symbol whose name is (mkstring arg1 ... argn)"
  (values (intern (apply #'mkstr args))))

(let ((count 0))
  (defun genCountName(s)
    "generate a name with a unique numeric suffix and the prefix specified"
    (symb s "<sub>" (incf count) "</sub>"))
  (defun resetCountName()
    "reset a the counter used for generating unique names"
    (setf count 0)))

;;; general utilities

(defun my-merge (x y)
  "merge two lists of symbols *already sorted and without duplicates*
   (and return the resulting list sorted and without duplicates)"
  (let* ((first (cons nil nil))
         (last first))
    (loop while (and x y)
       for cx = (car x)
       for cy = (car y)
       if (string= cx cy)
       do (setf x (cdr x))
       else if (string< cx cy)
       do (rplacd last (cons cx nil))
       and do (setf last (cdr last) 
                    x (cdr x))
       else do (rplacd last (cons cy nil))
       and do (setf last (cdr last) 
                    y (cdr y)))
    (rplacd last (or x y))
    (cdr first)))

;;; for debugging

(let ((d 0))
  (defun myb (lev str)
    (when (>= d lev) (break str)))
  (defun dob (lev)
    (setf d lev)))

;;(eval-when (:LOAD-TOP-LEVEL)
  ;;(defun fundep::|#?-reader| (stream fundep::subchar fundep::arg)
    ;;(declare (ignore fundep::subchar fundep::arg))
    ;;(let* ((fundep::a (read stream t nil t))
      ;;     (fundep::lvl (- (char-code (elt fundep::a 0)) (char-code #\0)))
        ;;   (fundep::msg (subseq fundep::a 1)))
      ;;(list 'fundep::myb fundep::lvl fundep::msg)))
  ;;(set-dispatch-macro-character #\# #\? #'fundep::|#?-reader|))
;;;

