# Common Lisp Lazy Evaluation Library (cl-lel)
 [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
 
This library provides an implementation of lazy sequences and its commonly used operators, e.g., filter, drop, and take.

I believe that this library does not provide something unique in contrast to the linked [alternatives](#alternatives) or the [references](#acknowledgements).

## Features

- Implementation of lazy sequences
- Default methods working with lazy sequences
- Some generators


## Installation

Unfortunately, `cl-lel` is currently not available for download via [quicklisp](https://www.quicklisp.org/beta/).
Thus, you have to install this library manually.

1. Clone this repository to your `ql:*local-project-directories*`
``` console
$ cd /home/user/quicklisp/local-projects/
$ git clone git@github.com:aruscher/cl-lel.git
```

2. Now, your systems can depend on this library
``` lisp
(asdf:defsystem #:my-system
  ...
  :depends-on ("cl-lel")
  ...)
```

You can run the tests via:
``` lisp
(asdf:test-system "cl-lel" :force t :verbose t)
```

## Usage/Examples

``` lisp
(defun fibonacci ()
  (labels ((inner-fibonacci (a b)
	     (lcons a (inner-fibonacci b (+ a b)))))
    (inner-fibonacci 0 1)))

(lazy-take 10 (fibonacci)) ; => (0 1 1 2 3 5 8 13 21 34)
```
  
## Acknowledgements

 - [Scheme lazy-seq](http://wiki.call-cc.org/eggref/5/lazy-seq)
 - [Clojure lazy-seq](https://clojure.org/reference/lazy)
 - [Barski - Land of Lisp](http://landoflisp.com/)

## Alternatives

 - [CLAZY](https://common-lisp.net/project/clazy/)
 - [lazy](https://github.com/massung/lazy)
 - [slow-jam](https://github.com/thezerobit/slow-jam)

## Authors

- [@aruscher](https://github.com/aruscher)
