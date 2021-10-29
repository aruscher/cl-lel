;;;; cl-lel.lisp

(defpackage #:cl-lel
  (:use #:cl)
  (:export #:delay
	   #:force
	   #:lcons
	   #:lcons-car
	   #:lcons-cdr
	   #:lazy-take
	   #:lazy-take-while
	   #:lazy-drop
	   #:lazy-drop-while
	   #:lazy-nth
	   #:lazy-filter
	   #:list->lazy-seq
	   #:numbers-from
	   #:cycle))

(in-package #:cl-lel)


(defmacro delay (expression)
  `(lambda () ,expression ))

(defun force (delayed-expression)
  (when delayed-expression
    (funcall delayed-expression)))

(defmacro lcons (head tail)
  `(cons (delay ,head) (delay ,tail)))

(defun lcons-car (lcons)
  (force (car lcons)))

(defun lcons-cdr (lcons)
  (force (cdr lcons)))

(defun lazy-take (n lcons)
  (loop :repeat n
	:for elem = (lcons-car lcons)
	:while elem
	:collect elem
	:do (setf lcons (lcons-cdr lcons))))

(defun lazy-take-while (predicate lcons)
  (loop :for elem = (lcons-car lcons)
	:while (and elem (funcall predicate elem))
	:collect (lcons-car lcons)
	:do (setf lcons (lcons-cdr lcons))))

(defun lazy-drop (n lcons)
  (if (zerop n)
      lcons
      (lazy-drop (1- n) (lcons-cdr lcons))))

(defun lazy-drop-while (predicate lcons)
  (if (funcall predicate (lcons-car lcons))
      (lazy-drop-while predicate (lcons-cdr lcons))
      lcons))

(defun lazy-nth (n lcons)
  (lcons-car (lazy-drop n lcons)))

(defun lazy-filter (predicate lcons)
  (when lcons
    (let ((elem (lcons-car lcons)))
      (if (funcall predicate elem)
	  (lcons elem (lazy-filter predicate (lcons-cdr lcons)))
	  (lazy-filter predicate (lcons-cdr lcons))))))

(defun list->lazy-seq (list)
  (when list
    (lcons (car list) (list->lazy-seq (cdr list)))))

(defun numbers-from (start &optional (step-size 1))
  (lcons start (numbers-from (+ start step-size))))

(defun cycle (sequence)
  (let ((seq-length (length sequence)))
    (labels ((inner-cycle (sequence index)
	       (lcons (nth index sequence) (inner-cycle sequence (mod (1+ index) seq-length)))))
      (inner-cycle sequence 0))))


