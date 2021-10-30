;;;; cl-lel.lisp

(defpackage #:cl-lel
  (:use #:cl)
  (:export #:delay
	   #:force
	   #:lcons
	   #:lcons-car
	   #:lcons-cdr
	   #:lcons-nil
	   #:lcons-null
	   #:lazy-take
	   #:lazy-take-while
	   #:lazy-drop
	   #:lazy-drop-while
	   #:lazy-nth
	   #:lazy-filter
	   #:lazy-mapcar
	   #:list->lazy-seq
	   #:numbers-from
	   #:cycle))

(in-package #:cl-lel)


(defmacro delay (expression)
  "Delays the execution of an EXPRESSION."
  `(lambda () ,expression ))

(defun force (delayed-expression)
  "Evaluates a DELAYED-EXPRESSION. Returns nil when DELAYED-EXPRESSION is nil."
  (when delayed-expression
    (funcall delayed-expression)))

(defmacro lcons (head tail)
  "Creates a lazy cons-cell where the evaluation of the HEAD and TAIL is delayed.

A lazy sequence can be created by passing another lazy con-cell as the TAIL."
  `(cons (delay ,head) (delay ,tail)))

(defun lcons-nil ()
  "Creates the nil equalivalent for lazy cons-cells."
  (cons nil nil))

(defun lcons-null (lcons)
  "Return t when LCONS is the lcons-nil; otherwise, return nil"
  (equal lcons (lcons-nil)))

(defun lcons-car (lcons)
  "Evaluates the car of a lazy cons-cell LCONS."
  (force (car lcons)))

(defun lcons-cdr (lcons)
  "Evaluation the cdr of a lazy cons-cell LCONS."
  (force (cdr lcons)))

(defun lazy-take (n lseq)
  "Takes the first N elements from a lazy sequence LSEQ."
  (loop :repeat n
	:while (not (lcons-null lseq))
	:for elem = (lcons-car lseq)
	:collect elem
	:do (setf lseq (lcons-cdr lseq))))

(defun lazy-take-while (predicate lseq)
  "Takes elements satisfying the PREDICATE from a lazy sequence LSEQ."
  (loop :for elem = (lcons-car lseq)
	:while (and (not (lcons-null lseq))
		    (funcall predicate elem))
	:collect elem
	:do (setf lseq (lcons-cdr lseq))))

(defun lazy-drop (n lseq)
  "Drops the first N elements from a lazy sequence LSEQ."
  (if (zerop n)
      lseq
      (lazy-drop (1- n) (lcons-cdr lseq))))

(defun lazy-drop-while (predicate lseq)
  "Drops the first elements of a lazy sequence LSEQ satisfying the PREDICATE."
  (if (funcall predicate (lcons-car lseq))
      (lazy-drop-while predicate (lcons-cdr lseq))
      lseq))

(defun lazy-nth (n lseq)
  "Returns the Nth element from a lazy sequence LSEQ."
  (lcons-car (lazy-drop n lseq)))

(defun lazy-filter (predicate lseq)
  "Uses a lazy sequence LSEQ to create a lazy sequence which elements are satisfying the PREDICATE."
  (if (not (lcons-null lseq))
    (let ((elem (lcons-car lseq)))
      (if (funcall predicate elem)
	  (lcons elem (lazy-filter predicate (lcons-cdr lseq)))
	  (lazy-filter predicate (lcons-cdr lseq))))
    (lcons-nil)))

(defun lazy-mapcar (func lseq)
  "Uses a lazy sequence LSEQ to create a lazy sequence"
  (if (not (lcons-null lseq))
    (lcons (funcall func (lcons-car lseq))
	   (lazy-mapcar func (lcons-cdr lseq)))
    (lcons-nil)))

(defun list->lazy-seq (list)
  "Creates a lazy sequence from LIST."
  (if list
      (lcons (car list) (list->lazy-seq (cdr list)))
      (lcons-nil)))

(defun numbers-from (start &optional (step-size 1))
  "Creates a lazy sequence with numbers starting from START which are subsequentally incremented by STEP-SIZE. "
  (lcons start (numbers-from (+ start step-size))))

(defun cycle (sequence)
  "Creates a lazy sequence which elements are cycling through SEQUENCE."
  (let ((seq-length (length sequence)))
    (labels ((inner-cycle (sequence index)
	       (lcons (nth index sequence) (inner-cycle sequence (mod (1+ index) seq-length)))))
      (inner-cycle sequence 0))))


