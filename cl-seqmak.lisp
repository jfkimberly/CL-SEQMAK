;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MIT License
;; Copyright (c) 2019 Junghoon Kim
;; jfkimberly@skku.edu

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to
;; deal in the Software without restriction, including without limitation the
;; rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
;; sell copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;; IN THE SOFTWARE.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(ql:quickload :cl-strings)
(use-package :cl-strings)
(ql:quickload :alexandria)



(defparameter *arms* (make-array 0 :fill-pointer T))
(defparameter *strand-list* (make-hash-table))
(defparameter *linker-list* '())
(defparameter *bases* "AGCT")


(defun help ()
  "Help function which shows all possible commands."
  
  (format t "Possible commands are:~%")
  (terpri)
  (format t "(na) newarms~%")
  (format t "(s)  show~%")
  (format t "(l)  link~%")
  (format t "(c)  crunch~%")
  (format t "(sg) strandgen~%")
  (format t "(rc) repeat-check~%")
  (format t "(dc) dyad-check~%")
  (format t "(sv) save~%")
  (format t "(ld) load~%")
  (format t "quit~%")
  (terpri)
  (format t "E.g., type 'na' or 'newarms' and hit 'Enter'.~%")
  (format t "Use '--help' for more information about a command, e.g. 'na --help'~%"))


(defun arm-len ()
  "Asks the user for the length of the subarms ('arm-length'); used in
'newarms' function."
  (format t "What is the length of the subarms of these arms?~%")
  (let ((arm-length (read)))
    (cond ((equal arm-length 'quit) 'nil)
	  ((and (integerp arm-length)
		(> arm-length 0))
	   arm-length)
	  (T (progn
	       (princ "Type in a positive integer.")
	       (fresh-line)
	       (arm-len))))))

 
(defun newarms ()
  "Asks the user for the number of arms which are stored in *arms* array. The
  number of arms is the number of dotted lists in the array and the 'arm-len'
  function used in this function determines the length of the sequence (and its
  complement) in each list."
  (let ((arm-numbers)
	(arm-length))

    (format t "How many arms do you want?~%")
    (setf arm-numbers (read))

    (cond ((equal arm-numbers 'quit) 'nil)
	  ((and (integerp arm-numbers)
		(> arm-numbers 0))
	   arm-numbers)
	  (T (progn
	       (format t "Type in a positive integer.~%")
	       (newarms))))

    ;; calls 'arm-len' function to set the the length of the subarms in
    ;; 'arm-length'
    (setf arm-length (arm-len))

    ;; (setf x-string (make-string arm-length :initial-element #\x))

    ;; need to make separate strings (sequences) for each element in the array
    ;; such as done below; if one string is made such as 'x-string' above, and
    ;; copied 'k' times into the array, each element has the same pointer and
    ;; changing one element changes all elements in the array
    ;; https://stackoverflow.com/questions/39702669/cannot-modify-specific-element-in-array-of-objects
    ;; fill *arms* array with initial strings of x's of length 'arm-length'
    ;; *arms* has data structure of #(('xxx' . 'xxx') ('xxx' . 'xxx') ...))
    ;; the index of each arm in *arms* starts at 0 for the first dotted list in
    ;; *arms* until the end of the array
    (dotimes (k arm-numbers) (vector-push-extend 
			      (cons
			       (make-string arm-length :initial-element #\x)
			       (make-string arm-length :initial-element #\x))
			      *arms*))))


(defun show ()
  "Prints out each arm in *arms* into 5 nt segments."
  (if (= (length *arms*) 0)
      'nil
      (dotimes (i (length *arms*))
	(terpri)
	(format t "arm~a:~%" (1+ i))
	(format t "~{~a ~}~%" (chop (car (aref *arms* i)) 5))
	(format t "~{~a ~}~%" (chop (cdr (aref *arms* i)) 5)))))


(defun linker3 ()
  "Takes user input of the ordered pairs of arm segments in the 5' -> 3'
direction and returns them as a list. Used in the 'linker' function."
  (format t "Enter the arm and it's 3'-linked arm:~%")
  (let ((formatted-link3 (mapcar 'parse-integer
				 (mapcar 'clean (split (read-line) ",")))))
    (loop :for (a b) :on formatted-link3 :by #'cddr :while b
	  :collect (list a b))))


(defun linker5 ()
  "Takes user input of the ordered pairs of arm segments in the 3' -> 5'
direction and returns them as a list. Used in the 'linker' function."
  (terpri)
  (format t "Enter the arm and it's 5'-linked arm (press 'Enter' if there are none):~%")
  (let ((formatted-link5 (mapcar 'parse-integer
				 (mapcar 'clean (split (read-line) ",")))))
    (loop :for (a b) :on formatted-link5 :by #'cddr
	  :collect (list a b))))


(defun linker ()
  "'linker' function used in the 'strandgen' function."
  (let ((link3 (linker3))
	(link5 (linker5))
	forearms
	postarms)
    ;; !!try to redefine nested loop into tail-optimized recursion in future
    ;; outer loop searches through 'link5' list until all elements of 'link5'
    ;; have been deleted.
    (loop :for z :in link5 :when link5

       ;; takes each element in link5 and sequentially looks for a match
       ;; between the first (last) element of all the elements 
       :do (loop :for x :in link5
	      :do (setf forearms 'nil
			postarms 'nil)
		(loop :for y :in link3
		   :do (when (= (first x) (first (last y)))
			 (setf postarms y)
			 (setf link3 (delete y link3 :test #'equal)))
		     (when (= (first (last x)) (first y))
		       (setf forearms y)
		       (setf link3 (delete y link3 :test #'equal))))
		(if (and forearms postarms)
		    (setf link3
			  (nconc (list (append postarms forearms)) link3)))
		(setf link5 (remove x link5 :test #'equal))))
    (setf *linker-list* (reverse link3))))


(defun seqgen (criton)
  "Create a random string consisting of the bases A, G, C T of length
'criton'."
  (let ((segment (make-array criton
			     :element-type 'character
			     :fill-pointer 0)))
    (dotimes (k criton segment) (vector-push (alexandria:random-elt *bases*) segment))))


(defun compgen (segment)
  "Create a complementary string of 'segment'."
  (map 'string (lambda (c)
		 (case (char-upcase c)
		   (#\A #\T)
		   (#\T #\A)
		   (#\G #\C)
		   (#\C #\G)			  
		   (t c)))
       segment))


(defun user-decision ()
  "Asks user for a yes, no, or abort command."
  (let ((user-input (read)))
    (cond
      ((equal user-input 'y) 'accept)
      ((equal user-input 'n) 'reject)
      ((equal user-input 'a) 'abort)
      (t
       (format t "Please answer (y)es, (n)o, or (a)bort:~%")
       (user-decision)))))


(defun accept-decision ()
  "Asks user for an accept, reject, or set command."
  (let ((user-input (read)))
    (cond
      ((equal user-input 'a) 'accept)
      ((equal user-input 'r) 'reject)
      ((equal user-input 's) 'set)
      (t
       (format t "Type 'a', 'r', or 's'.~%")
       (accept-decision)))))


(defun armgen (segment crunch-dat)
  "Takes a 'segment' string, creates its complement, and replaces a part or the
  whole arm in *arms* determined by 'armnum' (first element of list
  'crunch-dat') starting at base position 'start' (second element of list
  'crunch-dat')."
  (let ((armnum (1- (first crunch-dat)))
	(start (1- (second crunch-dat)))
;	(end (1- (third crunch-dat)))
	(comp-segment (compgen segment)))

    (setf (car (aref *arms* armnum))
	  (replace (car (aref *arms* armnum)) (string-upcase segment)
		   :start1 start))

    (setf (cdr (aref *arms* armnum))
	  (replace (cdr (aref *arms* armnum)) (string-upcase comp-segment)
		   :start1 start))))


(defun seggen (segsize)
  "Creates a random 'segment' of size 'segsize' and stores it in
  *segment-list*. Used in 'crunch function. If 'segment' already exists in
  *segment-list*, asks the user for use confirmation. If the 'segment' is
  accepted for use, stores 'segment' and its complement in *segment-list*. If
  'set' option is chosen, 'segment' sequence is determined by the user."
  (let ((segment (seqgen segsize))
	(decision))
    
    (format t "~a~%" segment)

    ;; check how many repeats of 'segment' there are in all strands
    (format t "There are ~d repeats of this segment.~%" (repeats segment))
    (format t "(a)ccept or (r)eject or (s)et~%")
    (setf decision (accept-decision))

    (case decision
      ((accept)
       (return-from seggen segment))
      ((reject)
       (seggen segsize))
      ((set)
       (format t "Enter your DNA sequence of length ~a.~%" segsize)
       (setf segment (write-to-string (read)))
       (format t "Is \"~a\" the sequence you would like to use? (y/n)~%" segment)
       (format t "There are ~d repeats of this segment.~%" (repeats segment))
       (case (user-decision)
	 ((accept) segment)
	 ((reject) (seggen segsize))
	 ('abort (return-from seggen)))))))


(defun strandgen ()
  "Creates strands by combining the arms in *arms* according to *linker-list*.
*linker-list* is a list of lists, e.g. ((4 8) (1 2 3 4) ...), which contains
the arm numbers to be combined into strands. E.g. for 'link-elem' (4 8) [we
need to subtract each number by 1, since the arm numbering starts at 0 (but is
shown to the user to start at 1)], the arms in even-numbered
positions (starting at 0) [e.g. 4 from (4 8)], are concatenated as is, whereas
the arms in odd-numbered positions, e.g. 8, are reversed and concatenated"
  (let (strand
	(link-elem-num 0)
	;; start strand number at 1
	(strandnum 1))
    (terpri)
    ;; if *linker-list* exists, create strands by combining arms from the
    ;; *arms* array
    (if *linker-list*
	(dolist (link *linker-list*)
	  (setf strand 'nil)
	  (dolist (link-elem link)
	    (if (evenp link-elem-num)
		(setf strand
		      (concatenate 'string
				   (car (elt *arms* (1- link-elem))) strand))
		(setf strand
		      (concatenate 'string
				   (reverse (cdr (elt *arms* (1- link-elem))))
				   strand)))
	    (incf link-elem-num))

	  (setf (gethash strandnum *strand-list*) strand)
	  (format t "strand ~d (~d bases)~%" strandnum (length strand))
	  (incf strandnum)
	  (format t "~{~a ~}~%" (chop strand 5))
	  (terpri))
	;; else if imported from user strand file just print out strand number
	;; and strand sequences
	(dolist (strand-dat (reverse
			     (alexandria:hash-table-alist *strand-list*)))
	  (format t "strand ~a (~a bases)~%" (car strand-dat) (length (cdr strand-dat)))
	  (format t "~{~a ~}~%" (chop (cdr strand-dat) 5))
	  (terpri)
	  ))))


(defun count-substrings (substring string)
  "Count the number of times 'substring' appears in 'string'."
  (loop
    :with sub-length = (length substring)
    :for i :from 0 :to (- (length string) sub-length)
    :when (string= string substring
		   :start1 i :end1 (+ i sub-length))
      count it))


(defun repeats (segment)
  "Returns the number of times 'segment' is repeated in all the strands."
  (let ((repeatseg 0))
    (dolist (strand (alexandria:hash-table-values *strand-list*) repeatseg)
      (incf repeatseg (count-substrings segment strand)))))


(defun crunch ()
  "Randomly generates or user defines a sequence using the 'seggen' function
and generates arms from it using the 'armgen' function."
  (format t "Please enter the following:~%")
  (format t "arm #, starting base, end base~%")

  (let ((crunch-dat (mapcar #'parse-integer
 			    (mapcar #'clean (split (read-line) ","))))
	(arm)
	(start)
	(end)
	(segsize)
	(segment))

    ;; check if repeats should be allowed
    ;; (default is no input meaning no repeats allowed)

    ;; need to change this code to conditions
    ;; DO NOT USE THIS CODE
    ;; (unless (or (= (length crunch-dat) 4)
    ;; 		(= (length crunch-dat) 5))
    ;;   (format t "Please enter only 4 or 5 numbers separated by commas!~%")
    ;;   (crunch))

    (setf arm (1- (first crunch-dat))
	  start (1- (second crunch-dat))
	  end (1- (third crunch-dat))
	  segsize (1+ (- end start)))

    ;; set 'segment' to generated sequence of length 'segsize' and into
    ;; designated arm which in turn saves it and its complement to *arms*
    (setf segment (seggen segsize))
    (armgen segment crunch-dat)
    (strandgen)))


(defun substring-positions (substring string)
  ;; find all occurrences of 'substring' in 'string' and return their positions
  ;; in a list
  (loop
     :with sub-length = (length substring)
     :for i :from 0 :to (- (length string) sub-length)
     :when (string= string substring
		    :start1 i :end1 (+ i sub-length))
     :append (list i)))


(defun repeat-check ()
  ;; checks the number of repeats and returns a list of the positions of the
  ;; repeating segments

  (format t "Enter min. CRITON size, max. CRITON size, min. # of repeats, max. # of repeats~%")
  (format t "(ex) 4,8,3,6~%")

  (let* ((repeat-dat (mapcar #'parse-integer
 			     (mapcar #'clean (split (read-line) ","))))
	 (mincrit (first repeat-dat))
	 (maxcrit (second repeat-dat))
	 (minrep (third repeat-dat))
	 (maxrep (fourth repeat-dat))
	 (test-segment)
	 (segment-list ())
	 (repeat-pos)
	 (repeat-position-list ())
	 (repeat-count))

    ;; check for correct input of 4 numbers

    ;; need to change this code to conditions
    ;; DO NOT USE THIS CODE
    ;; (unless (= (length repeat-dat) 4)
    ;;   (format t "Please enter 4 integers!~%")
    ;;   (repeat-check))

    ;; create a list of criton lengths given by min/max crition size
    ;; input and go through each one
    (dolist (criton (alexandria:iota (1+ (- maxcrit mincrit)) :start mincrit))

      ;; pick each strand in *strand-list*
      (setf segment-list ())
      (dolist (strand (alexandria:hash-table-values *strand-list*))

	;; dissect each strand from the *strand-list* into criton size
	;; segments, 'test-segment', starting from the 5' terminus and ending
	;; at the 3' terminus (minus the criton size)
	(dolist (base-num (alexandria:iota (1+ (- (length strand) criton))))
	  (setf test-segment (subseq strand base-num (+ base-num criton)))

	  ;; check whether the dissected segment, 'test-segment', is in the
	  ;; 'segment-list' which means that it's already been checked for
	  ;; repeats
	  (unless (member test-segment segment-list :test #'equal)

	    ;; take alist (strandnum . sequence) of each strand in
	    ;; *strand-list* and use 'substring-positions' function to check
	    ;; for repeats and return a list of their positions
	    (setf segment-list (cons test-segment segment-list))
	    (setf repeat-position-list ())
	    (dolist (strand-dat (alexandria:hash-table-alist *strand-list*))
	      (setf repeat-pos (substring-positions test-segment (cdr strand-dat)))
	      ;; if repeats are found, store in 'repeat-position-list' in
	      ;; (strandnum (starting base positions)) format
	      (if repeat-pos
		  (setf repeat-position-list
			(cons (list (car strand-dat) repeat-pos)
			      repeat-position-list))))

	    ;; count repeating segments in 'repeat-count'
	    (setf repeat-count 0)
	    (dolist (k repeat-position-list repeat-count)
	      (incf repeat-count (length (cadr k))))
	    
	    ;; if the 'repeat-count' is between min/max number of repeats given
	    ;; as input print out the results
	    (when (and (>= repeat-count minrep)
		       (<= repeat-count maxrep))
	      (terpri)
	      (format t "'~a' has ~d repeats~%" test-segment repeat-count)
	      (format t "strand # => starting base positions~%")
	      (dolist (repeat-elem repeat-position-list)
		(format t "strand ~a => ~d~%"
			(car repeat-elem) (mapcar #'1+ (cadr repeat-elem)))))))))))


(defun dyad-check ()
  "Checks each strand (individually) for segments of dyad symmetry."
  (format t "Enter the segment size (in nt's) of dyad symmetry check:~%")
  (let ((segsize (read))
	(test-segment)
	(dyad)
	(dyad-pos)
	(dyad-position-list ())
	(dyad-count)
	(total-dyad-count)
	(dyad-list ()))
    
    ;; need to change this code to conditions
    ;; DO NOT USE THIS CODE
    ;; (unless (integerp segsize)
    ;;   (dyad-check))

    ;; iterate through each strand in *strand-list* where 'strand-dat' is a
    ;; list of lists, e.g. ((1 . "xxxx") ... ), with the car of each list
    ;; indicating the strandnum and the cdr indicating the strand sequence
    (dolist (strand-dat (reverse (alexandria:hash-table-alist *strand-list*)))
      (setf dyad-list ())
      (setf total-dyad-count 0)
      (terpri)
      (format t "Strand ~a dyads:~%" (car strand-dat))
      ;; 'base-num' is the base number of 'strand' starting from the 5'-end and
      ;; ending at the 3'-end (minus the length of the input 'segsize') to be
      ;; used as the starting position of the 'test-segment'
      (dolist (base-num (alexandria:iota (1+ (- (length (cdr strand-dat)) segsize))))
	
	;; 'test-segment' is the segment of the strand to be converted into a
	;; 'dyad' segment and tested for dyad symmetry
	(setf test-segment (subseq (cdr strand-dat) base-num (+ base-num segsize)))
	(setf dyad (reverse (compgen test-segment)))

	;; 'dyad-count' counts the dyad symmetric segments for given strand and
	;; 'dyad-position-list' is a dotted list with car of strand number and
	;; cdr of list of their starting positions
	(setf dyad-count 0)
	(setf dyad-position-list ())

	;; check if 'test-segment' is stored in 'dyad-list', which means its
	;; dyad has already been tested and printed out
	(unless (member (write-to-string test-segment) dyad-list :test #'equal)

	  ;; check for dyad symmetric segments, and set 'dyad-pos' to a list of
	  ;; their positions
	  (setf dyad-pos (substring-positions dyad (cdr strand-dat)))

	  ;; if 'dyad-pos' is non-nil (non-empty list), then set
	  ;; 'dyad-position-list' as dotted list with car of strand number and
	  ;; cdr of list of their starting positions, e.g. (1 . (6 20))
	  (when dyad-pos
	    (setf dyad-position-list
		  (cons (list (car strand-dat) dyad-pos)
			dyad-position-list))
	    ;; if a dyad symmetric segment is found, store it in 'dyad-list' so
	    ;; it doesn't search for it again
	    (setf dyad-list (cons (write-to-string dyad) dyad-list)))

	  ;; count the number of dyad symmetric segments by counting the number
	  ;; of items in 'dyad-position-list'
	  (dolist (k dyad-position-list dyad-count)
	    (incf dyad-count (length (cadr k))))

	  (if (>= dyad-count 1)
	      (format t "(~d)'~a'-...-~a'~a' (~d repeats)~%"
		      base-num test-segment dyad-pos dyad dyad-count))
	  ;; increase 'total-dyad-count' for given strand
	  (incf total-dyad-count dyad-count)))

      ;; if total-dyad-count is 0, then no dyad symmetric repeats for given
      ;; strand
      (if (= total-dyad-count 0)
	  (format t "Strand ~d has no dyad symmetric repeats of length ~d~%"
		  (car strand-dat) segsize)))))


(defun save ()
  "Saves strand numbers/sequences and arm numbers/sequences to user designated
'file-name'."
  (format t "Type the name of file:~%")
  (let ((file-name (read-line))
	(decision))
    ;; check if file already exists
    (if (probe-file file-name)
	(progn 
	  (format t "File with name '~a' exists. Overwrite? (y/n)~%" file-name)
	  (setf decision (user-decision)))
	(progn
	  (format t "Save as file? (y/n)~%")
	  (setf decision (user-decision))))

    (when (equal 'accept decision)
      (with-open-file (stream file-name :direction :output :if-exists :supersede)
	(format stream "All sequences of the strands and arms are in the 5' -> 3' direction.~%~%")
	;; write strand # and sequence to file
	(dolist (strand-dat (reverse (alexandria:hash-table-alist *strand-list*)))
	  (format stream "strand ~d:~%" (car strand-dat))
	  (format stream "~a~%~%" (cdr strand-dat)))

	;; write arm # and sequence to file
	(dotimes (i (length *arms*))
	  (format stream "~%arm~a:~%" (1+ i))
	  (format stream "~{~a ~}~%" (chop (car (aref *arms* i)) 5))
	  (format stream "~{~a ~}~%" (chop (cdr (aref *arms* i)) 5)))))))


(defun load-file ()
  "Loads a text file with one strand sequence per line, which may or may not be
separated by newlines. Each strand sequence is stored in the *strand-list* hash
table."
  (format t "Enter the name of the file (e.g., strands.txt):~%")
  (let ((file-name (read-line))
	(strandnum 1))
    ;; check if file already exists
    (unless (probe-file file-name)
      (format t "File '~a' does not exist!~%" file-name)
      (return-from load-file))

    ;; reallocate *strand-list* hash table
    (setf *strand-list* (make-hash-table))
    (with-open-file (stream file-name :direction :input)
      (loop :for line = (read-line stream nil 'eof)
    	 :until (eq line 'eof)
	 :unless (string= "" line)
	 :do 
	   (setf (gethash strandnum *strand-list*) (remove #\space line))
	   (incf strandnum)))))


(defun run-seqmak ()
  "Main function to run SEQMAK."
  (let ((command))
    (loop
       :do 
	 (format t "Type in a command or 'help' for available commands:~%")
	 (setf command (read))
	 (case command
	   ((help)
	    (funcall #'help))
	   ((newarms na)
	    (funcall #'newarms)
	    (format t "New arms created.~%"))
	   ((show s)
	    (funcall #'show))
	   ((link l)
	    (funcall #'linker))
	   ((crunch c)
	    (funcall #'crunch))
	   ((strandgen sg)
	    (funcall #'strandgen))
	   ((repeat-check rc)
	    (funcall #'repeat-check))
	   ((dyad-check dc)
	    (funcall #'dyad-check))
	   ((save sv)
	    (funcall #'save))
	   ((load-file lf)
	    (funcall #'load-file))
	   ((quit)
	    (format t "Bye!~%")
	    (return-from run-seqmak))
	   (otherwise
	    (format t "Undefined command. Retype command.~%"))))))


