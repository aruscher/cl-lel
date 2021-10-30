(defpackage #:cl-lel-tests
  (:use #:cl #:cl-lel #:fiveam)
  (:export #:all-tests))

(in-package #:cl-lel-tests)

(def-suite all-tests)

(in-suite all-tests)

(test delay-force
  (is (functionp (delay 123)))
  (is (= 0 (force (delay 0)))))

(test lcons
  (let ((lcons (lcons 0 1)))
    (is (= 0 (lcons-car lcons)))
    (is (= 1 (lcons-cdr lcons)))))

(test lazy-take
  (let ((lazy-seq (numbers-from 1)))
    (is (equal '(1 2 3) (lazy-take 3 lazy-seq)))))

(test lazy-take-while
  (let ((lazy-seq (numbers-from 1))
	(predicate (lambda (x) (<= x 3))))
    (is (equal '(1 2 3)
	       (lazy-take-while predicate lazy-seq)))))

(test lazy-drop
  (let ((lazy-seq (numbers-from 1)))
    (is (equal '(4 5 6)
	       (lazy-take 3 (lazy-drop 3 lazy-seq))))))

(test lazy-drop-while
  (let ((lazy-seq (numbers-from 1))
	(predicate (lambda (x) (<= x 3))))
    (is (equal '(4 5 6)
	       (lazy-take 3 (lazy-drop-while predicate lazy-seq))))))

(test lazy-nth
  (let ((lazy-seq (numbers-from 1)))
    (is (= 4 (lazy-nth 3 lazy-seq)))))

(test lazy-filter
  (is (equal '(2 4 6) (lazy-take 3
				 (lazy-filter #'evenp
					      (numbers-from 1))))))

(test list->lazy-seq
  (let ((l '(1 2 3)))
    (is (equal l (lazy-take 3
			    (list->lazy-seq l))))
    (is (lcons-null (lazy-drop 3
			       (list->lazy-seq l))))
    (is (null (lazy-nth 10
			(list->lazy-seq l))))
    (is (equal '(2) (lazy-take 10
			       (lazy-filter #'evenp
					    (list->lazy-seq l)))))
    (is (equal '(2 3 4) (lazy-take 3
				   (lazy-mapcar #'1+
						(list->lazy-seq l)))))))



