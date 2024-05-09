(in-package #:newnormalizer)

(declaim (optimize (speed 3) (safety 0)))
;(declaim (optimize (debug 3) (safety 3) (speed 0)))

(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part in string
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos)))

(defun is-blank (c)
  "c is blank or equivalent (including newline)"
  (or (char= c #\Space) (char= c #\Newline) (char= c #\Tab)))

(defun not-valid (c)
  "c is not alfanumeric or blank or arrow"
  (and (not (alphanumericp c))
       (not (is-blank c))
       (not (char= c #\→))
       (not (char= c #\↠))
       (not (char= c #\↔))))

(defun ignore-if (test str frompos topos)
  "returns the index of the first character of str[frompos,topos]
   which does not pass test, or the last index"
  (loop for idx from frompos below topos
     while (funcall test (char str idx))
     finally (return idx)))

(defun ignore-non-valid (str frompos topos)
  "returns the index of the first character of str[frompos,topos]
   which is not invalid, or the last index"
  (ignore-if #'not-valid str frompos topos))

(defun ignore-blank (str frompos topos)
  "returns the index of the first character of str[frompos,topos]
   which is not blank, or the last index"
  (ignore-if #'is-blank str frompos topos))

(defun must-identifier (str frompos topos)
  "returns the first identified in str[frompos, topos] and the first
   character following it, or nil and frompos if none is present"
  (let* ((tp (ignore-non-valid str frompos topos))
	 (end-id (if (>= tp topos)
		     nil
		     (and (alpha-char-p (char str tp))
			  (loop for idx from (1+ tp) below topos
			     while (alphanumericp (char str idx))
			     finally (return idx))))))
    (if end-id
	(values (subseq str tp end-id) end-id)
	(values nil frompos))))

(defun checkattrs (str &optional (botm 0) (top (length str)))
  "analyze str[botm,top] to return all the attributes (contiguous letters) in it"
  (setq str (substitute-if #\Space #'not-valid str))
  (let ((res nil))
    (loop do (setq botm (ignore-if #'is-blank str botm top))
	 do (multiple-value-bind (newatt newbot) (must-identifier str botm top)
	      (setq botm newbot)
	      (if newatt
		  (push newatt res)
		  (return (my-sort (mapcar #'intern res))))))))

(defun read-non-null-line (s)
  "return the first non null line of s after cleaning it from non-valid or blank 
    characters"
  (let ((l (read-line s nil nil))
	(m nil))
    (when l
      (setq m (substitute-if #\Space #'not-valid l))
      (setq l (string-trim '(#\Space) m))
      (if (string= l "")
	  (read-non-null-line s)
	  l))))



(defun checkfundeps (str)
  "returns the list of fun-deps present in str after checking their syntax"
  (setq str (replace-all str "<->" " ↔ "))
  (setq str (replace-all str "->>" " ↠ "))
  (setq str (replace-all str "->->" " ↠ "))
  (setq str (substitute-if #\Space #'not-valid (replace-all str "->" " → ")))
  (let ((res nil) (mvds nil))
    (with-input-from-string (s str)
      (loop as line = (read-non-null-line s) ;(read-line s nil nil)
	 while line
	 as pos1 = (position #\→ line)
	 as double = (position #\↔ line)
	 as mv = (position #\↠ line)
	 as pos = (or pos1 double mv)
	 while (and pos (>= pos 0) (<= pos (length line)))
	 as dett = (checkattrs line 0 pos)
	 as dete = (checkattrs line (1+ pos) (length line))
	 ;;while (and (>= (length dett) 0)
	 ;;	    (>= (length dete) 0))
	 do (cond (mv (push (mk-mv-dep dett dete) mvds))
		  (pos1 (push (mk-f-dep dett dete) res))
		  (double (push (mk-f-dep dett dete) res)
			  (push (mk-f-dep dete dett) res))
		  (t (error "syntax error")))
	 finally (return (list (mk-f-dep-s (reverse res))
			       (mk-mv-dep-s (reverse mvds))))))))


(defun checkschema (strattr strdeps)
  "returns the schema obtained by extracting from the  strings passed to it
   attributes, functional and multivalued dependencies, nil if error" 
  (let* ((satts (checkattrs strattr))
	 (sdeps (checkfundeps strdeps))
	 (attsfromdeps (att-s-union (f-dep-s-att-s (first sdeps)) (mv-dep-s-att-s (second sdeps)))))
    (when (subsetp attsfromdeps  satts)
      (mk-sch 'R satts (first sdeps) (second sdeps)))))	; note uniques is null

(defun checkdecomposition (str)
  "returns a set of schemas obtained by extracting from the string passed to it
   the attributes of each schema; schema are separated by newlines and/or parentheses"
  (let* ((open (loop with start = 0
		  for p = (position #\( str :start start)
		  while p collect p
		  do (setf start (1+ p))))
	 (closed (loop with start = 0
		    for p = (position #\) str :start start)
		    while p collect p
		    do (setf start (1+ p))))
	 (matches (and (= (length open) (length closed))
		       (loop for o in open
			  for c in closed
			  always (< o c))
		       (loop for o in (cdr open)
			  for c in closed
			  always (> o c)))))
    (resetCountName)
    (if matches
	(mk-sch-s (loop for p in open
		     for c in closed
		     collect (mk-sch (genCountName 'R) (checkattrs str p c) '())))
	(error "syntax error"))))

(defun checklen(str)
  (and (<= (array-dimension str 0) safety-max-size)
       str))
