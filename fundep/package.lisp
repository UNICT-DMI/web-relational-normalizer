;;;; package.lisp

(defpackage #:fundep
  (:use #:cl #:alexandria)
  (:export :mk-att :attp :mk-att-s :att-sp :mk-att-s-s :att-s-union
	   :mk-f-dep :f-depp :determinant :determinate :f-dep-att-s
	   :mk-mv-dep :mk-mv-dep-s :mv-depp :mv-dep-sp :my-merge
	   :mk-f-dep-s :f-dep-sp :f-dep-s-att-s
	   :mv-dep-att-s :mv-dep-s-att-s
	   :mk-sch :mk-sch-s :schp :sch-s :sch-name :sch-att-s :sch-f-dep-s
	   :my-sort :sch-attr-set-closure :sch-project-set-fun-dep
	   :sch-canonical-set :sch-a-implies-b :sch-a-equivalent-b
	   :a-implies-b :a-equivalent-b :attr-set-closure
	   :fun-dep-in-closure :check-decomposition
	   :canonical-set :check-dep-preservation :is-superkey
	   :is-trivial :violates-boyce-codd :find-a-key
	   :3nf :project-set-fun-dep :find-all-keys :find-primes :bcnf
	   :is-3nf :schema-with-name :3nfpd :3nftbc :bcnftdp :bc-sbs
	   :full-normalization :4nf :is-4nf :genCountName :resetCountName
	   :full-decomposition))


