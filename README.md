# Common Lisp Lazy Evaluation Library
 [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
 
This library provides an implementation of lazy sequences and its commonly used operators, e.g., filter, drop, and take.

I believe that this library does not provide something special in contrast to the linked [alternatives](#alternatives) or the [references](#acknowledgements].

## Features

- Implementation of lazy sequences
- Default methods working with lazy sequences
- Some generators


## Installation

Unfortunatly, cl-lel is currently not available via [quicklisp](https://www.quicklisp.org/beta/).
Thus, you have to install this library manually:

Install cl-lel with ???

```lisp
(install-my-cl-lel)
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
 - [Barski - Land of Lisp](http://landoflisp.com/]

## Alternatives

 - [CLAZY](https://common-lisp.net/project/clazy/)
 - [lazy](https://github.com/massung/lazy)
 - [slow-jam](https://github.com/thezerobit/slow-jam)

## Authors

- [@aruscher](https://github.com/aruscher)
