#lang racket
(provide (all-defined-out))
(module+ test
  (require rackunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple list functions

;; Follow this template for functions on lists of numbers where appropriate.
;; [Listof Number] ... -> ...
#;
(define (lon-template ls ...)
  (match ls
    ['() ...]
    [(cons n ls) (... n (lon-template ls ...) ...)]))

;; [Listof Number] -> Natural
;; Compute the length of given list of numbers
(define (length-lon ls)
  (match ls ['() 0] [ls (+ 1 (length-lon (list-tail ls 1)))]
  )
  )

(module+ test
  (check-equal? (length-lon '()) 0)
  (check-equal? (length-lon '(1)) 1)
  (check-equal? (length-lon '(2)) 1)
  (check-equal? (length-lon '(1 2)) 2))

;; [Listof Number] -> Number
;; Compute the sum of given list of numbers
(define (sum ls)
  (match ls
      ['() 0]
  [ls (+ (list-ref ls 0) (sum (list-tail ls 1)))]
  )
  )

(module+ test
  (check-equal? (sum '()) 0)
  (check-equal? (sum '(1)) 1)
  (check-equal? (sum '(2)) 2)
  (check-equal? (sum '(1 2)) 3))

;; [Listof Number] [Listof Number] -> [Listof Number]
;; Compute the pairwise sum of given list of numbers
;; ASSUME: lists have equal length
(define (zip-add ls1 ls2)
  (match ls1
      ['() '()]
    [ls1 (cons (+ (list-ref ls1 0) (list-ref ls2 0)) (zip-add (list-tail ls1 1) (list-tail ls2 1)))]
  )
  )

(module+ test
  (check-equal? (zip-add '() '()) '())
  (check-equal? (zip-add '(1) '(2)) '(3))
  (check-equal? (zip-add '(1 3) '(2 4)) '(3 7)))

;; [Listof Number] [Listof Number] -> [Listof [List Number Number]]
;; Compute the pairwise list of given list of numbers
;; ASSUME: lists have equal length
(define (zip-lon ls1 ls2)
  (match ls1
      ['() '()]
    [ls1 (cons (cons (list-ref ls1 0) (cons (list-ref ls2 0) '())) (zip-lon (list-tail ls1 1) (list-tail ls2 1)))]
  )
)

(module+ test
  (check-equal? (zip-lon '() '()) '())
  (check-equal? (zip-lon '(1) '(2)) '((1 2)))
  (check-equal? (zip-lon '(1 3) '(2 4)) '((1 2) (3 4))))

;; [Pairof Real [Listof Real]] -> Real
;; Compute a minimum element of non-empty list of numbers
(define (min-lon xs)
  (match xs
   [(list a) a]
    [xs (let ([len (min-lon (list-tail xs 1))]) (if (< (list-ref xs 0) len ) (list-ref xs 0) len) )]
  )
  )

(module+ test
  (check-equal? (min-lon '(1)) 1)
  (check-equal? (min-lon '(1 2)) 1)
  (check-equal? (min-lon '(2 1)) 1)
  (check-equal? (min-lon '(2 3 1)) 1))

;; [Listof Real] -> [Listof Real]
;; Sort list into descending order
;; HINT: do insertion sort by writing and using the helper below
(define (sort-desc xs)
  (match xs
    ['() '()]
    [xs (insert-desc (first xs) (sort-desc (list-tail xs 1)))]
  )
)

(module+ test
  (check-equal? (sort-desc '()) '())
  (check-equal? (sort-desc '(1)) '(1))
  (check-equal? (sort-desc '(1 2)) '(2 1))
  (check-equal? (sort-desc '(2 1)) '(2 1))
  (check-equal? (sort-desc '(2 3 1)) '(3 2 1)))

;; Real [Listof Real] -> [Listof Real]
;; Insert number into sorted list
;; ASSUME: given list is sorted in descending order
(define (insert-desc n xs)
  (match xs
    ['() (cons n '())]
  [xs (if (> n (first xs)) (cons n xs) (cons (first xs) (insert-desc n (list-tail xs 1))))]
    )
  )

(module+ test
  (check-equal? (insert-desc 5 '()) '(5))
  (check-equal? (insert-desc 5 '(7)) '(7 5))
  (check-equal? (insert-desc 5 '(3)) '(5 3)))
