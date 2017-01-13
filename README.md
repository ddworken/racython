# racython

Inspired by my Fundamentals of Computer Science 1 course, this is a Racket interpreter written in Python. To be more precise, it is an ISL (Intermediate Student Language) interpreter with the addition of ```begin``` and ```display```. 

This code is still extremely experiment but is functional. As an example of what works, the below code runs properly in racython with the same result as in Racket: 

``` racket
(define-struct leaf [val])
(define-struct node [left val right])
;;; A [WeightedTreeOf X] is one of:
;;;   - (make-leaf X)
;;;   - (make-node [WeightedTreeOf X] X [WeightedTreeOf X])
;;; Interpretation: Everything to the left the center val is less
;;;                 than it and everything to the right is greater.

;;; [WeightedTreeOf X] X -> Boolean
(define wtree-contains? (lambda (wtree x)
  (cond [(leaf? wtree) (equal? x (leaf-val wtree))]
        [(equal? x (node-val wtree)) #true]
        [(< x (node-val wtree))
         (wtree-contains? (node-left wtree) x)]
        [else (wtree-contains? (node-right wtree) x)])))
        
(define t2.L (make-node (make-leaf 2) 3 (make-leaf 4)))
(define t2.R (make-node (make-leaf 6) 7 (make-leaf 8)))
(define t3 (make-node t2.L 5 t2.R))

(check-expect (wtree-contains? (make-leaf 5) 5) #true)
(check-expect (wtree-contains? (make-leaf 5) 6) #false)
(check-expect (wtree-contains? t3 5) #true)
(check-expect (wtree-contains? t3 2) #true)
(check-expect (wtree-contains? t3 3) #true)
(check-expect (wtree-contains? t3 4) #true)
(check-expect (wtree-contains? t3 6) #true)
(check-expect (wtree-contains? t3 7) #true)
(check-expect (wtree-contains? t3 8) #true)
(check-expect (wtree-contains? t3 9) #false)
```

For more examples, see [testing.py](https://github.com/ddworken/racython/blob/master/testing.py)
