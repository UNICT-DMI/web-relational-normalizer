;;;; package.lisp

(defpackage #:newnormalizer
  (:nicknames "NN")
  (:use #:cl #:fundep #:hunchentoot #:ht-simple-ajax #:cl-who )
  (:export :start-web-server :stop-web-server))


