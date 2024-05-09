;;;; normalizer.lisp

(in-package #:newnormalizer)

(declaim (optimize (speed 3) (safety 0)))
;(declaim (optimize (debug 3) (safety 3) (speed 0)))

(declaim (inline is-blank not-valid ignore-blank ignore-non-valid))

(defparameter safety-max-attributes 50
  "hard limit to the number of attributes for exponential algorithms")
(defparameter safety-max-size 20000
  "maximum size for each field")

(defvar cur-acceptor "global variable for the acceptor of the current web application")

(defparameter *ajax-processor*
  (make-instance 'ajax-processor :server-uri "/ajax"))

(defun prepare-for-html (str)
"replace newlines with '<br/>' in string str"
  (let ((res
	 (replace-all str "
" "<br/>")))
    res))

(defmacro model-op (op
		    op1
		    (t1 &optional t2)
			  (&optional exp)
			     (fmt-str &rest fmt-args)
					(expl-fmt-str &rest expl-fmt-args))
  (declare (ignore t1))
  (let ((arglist (if t2 '(arg1 arg2 arg3 mexp) '(arg1 arg2 mexp)))
	(ignorelist (append (unless expl-fmt-args (list `(ignore trans)))
			    (unless t2 (list `(ignore arg2)))))
	(letlist `((size1 (and (checklen arg1)
			       (checklen arg2)
			       ,(if t2 `(checklen arg3) t)))
		   (arg1 (when size1 (checkschema arg1 arg2)))
		   (arg2 (when size1 ,(when t2 (cond ((eq t2 'a) `(checkattrs arg3))
						     ((eq t2 'd) `(checkfundeps arg3))
						     (t `(checkdecomposition arg3))))))
		   (mx (string= mexp "true"))
		   (dont-do (cond ((null size1) 'too-long)
				  ((null arg1) 'no-valid-schema)
				  ((and ,exp (> (length arg1) safety-max-attributes)) 'too-much)))
		   (st1 (when mx (make-string-output-stream)))
		   (res (if dont-do nil ,(if t2 `(,op1 arg1 arg2 st1) `(,op1 arg1 st1))))
		   (trans (when mx (get-output-stream-string st1)))
		   (fmtstring (if mx (concatenate 'string ,fmt-str ,expl-fmt-str) ,fmt-str)))))
    `(defun-ajax ,op ,arglist (*ajax-processor*)
       (let* ,letlist
	 ,(when ignorelist `(declare ,@ignorelist))
	 (prepare-for-html 
	  (cond ((eq dont-do 'too-much)
		 (format nil "The operation has been canceled because the number of attributes is greater than the limit for exponential algorithms (~a)." safety-max-attributes))
		((eq dont-do 'no-valid-schema)
		 "Error: The functional dependencies contain attributes not present in the relation")
		((eq dont-do 'too-long)
		 "Error: Limit of input size exceeded.")
		(t ,(if 'mx
			`(format nil fmtstring ,@fmt-args mx ,@expl-fmt-args)
			`(format nil fmtstring ,@fmt-args)))))))))

(defun-ajax mysyntax (strattr strdeps) (*ajax-processor*)
  (if (and (checklen strattr) (checklen strdeps))  
      (let* ((satts (checkattrs strattr))
	     (sdeps (checkfundeps strdeps))
	     (attsfromdeps (att-s-union (f-dep-s-att-s (first sdeps)) (mv-dep-s-att-s (second sdeps)))))
	(if (> (length satts) safety-max-attributes)
	    "len"
	    (if (subsetp attsfromdeps satts)
		"yes"
		"no")))
      "size"))

(model-op norm full-normalization (s) ()
	  ("~a" res)
	  (""))

(model-op checkdec full-decomposition (s c) ()
	  ("~a" res)
	  (""))

(model-op closure sch-attr-set-closure (s a) ()
	  ("The closure of ~A with respect to:~%~% ~/fundep:f-dep-s/~% ~%is:~%~%~A.~%~%" arg2 (sch-f-dep-s arg1) res)
	  ("~:[~;Here is the transcript of the algorithm:~%~%~A~]" trans))

(model-op project sch-project-set-fun-dep (s a) (t)
	  ("One canonical form of the projection over ~A of the dependenc~@p:~%~%~/fundep:f-dep-s/~%~%is:~%~%~/fundep:f-dep-s/~%~%"
	   arg2 (length (sch-f-dep-s arg1)) (sch-f-dep-s arg1) res)
	  ("~:[~;Here is the transcript of the algorithm:~%~%~A~]" trans))

(model-op canonicalize sch-canonical-set (s) ()
	  ("One canonical form of: ~%~%~/fundep:f-dep-s/ ~%~%is:~%~% ~/fundep:f-dep-s/.~%~%" (sch-f-dep-s arg1) res)
	  ("~:[~;Here is the transcript of the algorithm:~%~%~A~]" trans))

(model-op findonekey find-a-key (s) ()
	  ("A key of: ~%~%~/fundep:sch/ ~% is: ~%~%~/fundep:att-s/.~%~%" arg1 res)
	  ("~:[~;Here is the transcript of the algorithm:~%~%~A~]" trans))

(model-op findallkeys find-all-keys (s) (t)
	  ("The key~P of: ~%~%~/fundep:sch/ ~%~[~;is~:;are~]: ~%~%~/fundep:att-s-s/.~%~%" (length res) arg1 (length res) res)
	  ("~:[~;Here is the transcript of the algorithm:~%~%~A~]" trans))

(model-op findprimes find-primes (s) (t)
	  ("The prime attribute~P of: ~%~%~/fundep:sch/~%~[~;is~:;are~]: ~%~%~/fundep:att-s/.~%~%"
	   (length res) arg1 (length res) res)
	  ("~:[~;Here is the transcript of the algorithm:~%~%~A~]" trans))

(model-op isbcnf violates-boyce-codd (s) (t)
	  ("~/fundep:sch/ ~%is ~:[~;not~] in Boyce-Codd Normal Form." arg1 res)
	  ("~:[~;~:[~; for the following dependenc~@p in which the determinant is not a superkey:~%~%~a~%~]~]"
	   res (length trans) trans))

(model-op is3nf is-3nf (s) (t)
	  ("~/fundep:sch/ ~%is ~:[not~;~] in Third Normal Form" arg1 res)
	  ("~:[~;~:[~; because ~A~%~]~]" (not res) trans))

(model-op tnf 3nf (s) ()
	  ("The Third Normal Form of ~%~%~/fundep:sch/~%is:~%~%~/fundep:sch-s/~%~%" arg1 res)
	  ("~:[~;Here is the transcript of the algorithm:~%~%~A~]" trans))

(model-op tnfpd 3nfpd (s) ()
	  ("The Third Normal Form of ~%~%~/fundep:sch/~%is:~%~%~/fundep:sch-s/~%~%" arg1 res)
	  ("~:[~;Here is the transcript of the algorithm:~%~%~A~]" trans))

(model-op tnftbc 3nftbc  (s) ()
	  ("The Third Normal Form of ~%~%~/fundep:sch/~%is:~%~%~/fundep:sch-s/~% ~:[There are scheme(s) not in Boyce-Codd Normal Form.~;Every schema is in Boyce-Codd Normal Form.~]~%~%" arg1 (first res) (second res))
	  ("~:[~;Here is the transcript of the algorithm:~%~%~A~]" trans))

(model-op bctdp bcnftdp  (s) ()
	  ("The Boyce-Codd Normal form of ~%~%~/fundep:sch/~%is:~%~%~/fundep:sch-s/~% The dependencies ~:[are not~;are~] preserved.~%~%"
	   arg1 (first res) (second res))
	  ("~:[~;Here is the transcript of the algorithm:~%~%~A~]" trans))

(model-op aimplb sch-a-implies-b (s d) ()
	  ("The dependenc~@p:~%~%~/fundep:f-dep-s/~%~% ~[are~;is~:;are~] ~:[not~;~] implied by:~%~%~/fundep:f-dep-s/~% "
	   (length (car arg2)) (car arg2) (length (car arg2)) res (sch-f-dep-s arg1))
	  ("~:[~;~:[~;~%because ~A~%~]~]" (not res) trans))

(model-op aequivb sch-a-equivalent-b (s d) ()
	  ("The dependenc~@p:~%~%~/fundep:f-dep-s/~%~% ~[~;is~:;are~] ~:[not~;~] equivalent to:~%~%~/fundep:f-dep-s/~%"
	   (length (car arg2)) (car arg2) (length (car arg2)) res (sch-f-dep-s arg1))
	  ("~:[~;~:[~;~%because ~A~%~]~]" (not res) trans))

(model-op is4nf is-4nf (s) (t)
	  ("~/fundep:sch/ ~%is ~:[not~;~] in Fourth Normal Form" arg1 res)
	  ("~:[~;~:[~; because ~A~%~]~]" (not res) trans))

(model-op fnf 4nf (s) ()
	  ("The Fourth Normal Form of ~%~%~/fundep:sch/~%is:~%~%~/fundep:sch-s/~%~%" arg1 res)
	  ("~:[~;Here is the transcript of the algorithm:~%~%~A~]" trans))

;; (model-op checkdec check-decomposition (s c) ()
;; 	  ("The decomposition of ~%~%~/fundep:sch/~%in:~%~%~/fundep:sch-s/ is ~:[not~;~] correct~%~%" arg1 arg2 res)
;; 	  ("~:[~;Here is the transcript of the algorithm:~%~%~A~]" trans))

(defun split-string (string char)
    "Returns a list of substrings of string
divided by ONE occurrence of char each.
Note: Two consecutive char will be seen as
if there were an empty string between them."
    (unless (string= string "")
      (loop for i = 0 then (1+ j)
	 as j = (position char string :start i)
	 collect (subseq string i j)
	 while j)))

; boyce-codd interactive
(defun-ajax bcsbs (atts deps hist mexp) (*ajax-processor*)
  (let* ((arg1 (checkschema atts deps))
	 (dont-do (null arg1))
	 (mx (string= mexp "true"))
	 (hist (mapcar #'parse-integer (split-string hist #\*)))
	 (st (when mx (make-string-output-stream))))
    (if dont-do
	"Error: The functional dependencies contain attributes not present in the relation"
	(multiple-value-bind (res pres) (bc-sbs arg1 hist st)
	  (if (char= (elt res 0) #\*)
	      (prepare-for-html
	       (let ((output (format nil "*The Boyce-Codd Normal Form of: ~%~%~/fundep:sch/~%is:~%~%~aThe dependencies ~:[are not~;are~] preserved.~%~%" arg1 (subseq res 1) pres)))
		 (when mx (setq output (concatenate 'string output (format nil "Here is the transcript of the algorithm:~%~%~A" (get-output-stream-string st)))))
		 output))
	      res)))))

(defparameter *default-port* "8080")

(defun start-web-server(&optional interactive)
  "start the web server"
  (let* ((home-user (uiop:getenv "HOME"))
	 (port (parse-integer *default-port*))
	 (mname (machine-instance))
	 (pathname (concatenate
		    'string
		    home-user
		    (cond ((string= mname "dblab")
			   "/.local/share/common-lisp/source/newnormalizer/")
			  ((and (>= (length mname) 8) (string= "pborsini" (subseq mname 0 8)))
			   "/Projects/lisp/newnormalizer/")
			  (t
			   "/.quicklisp/local-projects/newnormalizer/")))))
    (format t "** Starting hunchentoot @ 0.0.0.0:~A~% from directory ~A" port pathname)
    (if interactive
	(setf *catch-errors-p* nil
	  *show-lisp-errors-p* t
	  *show-lisp-backtraces-p* t
	  *log-lisp-backtraces-p* t
	  *log-lisp-warnings-p* t
	  *lisp-warnings-log-level* :warning) ; for more debug: :info
	(setf *catch-errors-p* t
	  *show-lisp-errors-p* nil
	  *show-lisp-backtraces-p* nil
	  *log-lisp-backtraces-p* t
	  *log-lisp-warnings-p* t
	  *lisp-warnings-log-level* :warning)) ; for more debug: :info
    (setf *DEFAULT-PATHNAME-DEFAULTS* (pathname pathname)
	  cur-acceptor (make-instance 'easy-acceptor :port port)
	  (acceptor-document-root cur-acceptor) (merge-pathnames "www/")
	  (acceptor-error-template-directory cur-acceptor) (merge-pathnames "www/errors/")
	  *dispatch-table* (list 'dispatch-easy-handlers 
				 (create-ajax-dispatcher *ajax-processor*)))
    (start cur-acceptor)))

(defun stop-web-server()
  "stop the currently running web server"
  (hunchentoot:stop cur-acceptor))
