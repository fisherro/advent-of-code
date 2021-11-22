#lang racket

(require anaphoric)

; A quick unit test function:
(define (expect name a b)
  (printf "[~a] \"~a\" returned ~a expected ~a\n"
          (if (equal? a b)
              "PASS"
              "FAIL")
          name
          a
          b))
  
; Given a list,
; e.g. (0 1 2 3 4),
; return a list of adjacent pairs,
; e.g. ((0 1) (1 2) (2 3) (3 4))
; Note that it is a list of 2-element lists, not a list of pairs.
(define (pair-off xs)
  (map list (drop-right xs 1)
       (rest xs)))

(expect "pair-off" (pair-off (range 5)) '((0 1) (1 2) (2 3) (3 4)))

(define (read-file file)
  (with-input-from-file file
    (thunk
     (sequence->list (sequence-map string->number (in-lines))))))

(define (eq1 x)
  (= x 1))

(define (eq3 x)
  (= x 3))

(define (part1 file)
  (define (rev-minus xs)
    (- (second xs)
       (first xs)))
  (aand (read-file file)
        (cons 0 it)
        (sort it <)
        (pair-off it)
        (map rev-minus it)
        (* (count eq1 it)
           (add1 (count eq3 it)))))

(expect "part1 test1" (part1 "test1.txt") (* 7 5))
(expect "part1 test2" (part1 "test2.txt") (* 22 10))
(expect "part1 input" (part1 "input.txt") 2590)