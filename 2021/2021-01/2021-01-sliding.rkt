#lang racket

; A more concise version using the "sliding" algorithm

(require "../qtest.rkt")
(require threading)
(require algorithms)
(require srfi/26)

(define (part1 file)
  (with-input-from-file file
    (thunk
     (~> (in-lines)
         (sequence->list _)
         (map string->number _)
         (sliding _ 2 1)
         (map (cute apply < <>) _)
         (count (cute equal? #t <>) _)))))

(define (part2 file)
  (with-input-from-file file
    (thunk
     (~> (in-lines)
         (sequence->list _)
         (map string->number _)
         (sliding _ 3 1)
         (map (cute apply + <>) _)
         (sliding _ 2 1)
         (map (cute apply < <>) _)
         (count (cute equal? #t <>) _)))))

(qtest (part1 "test.txt") 7)
(qtest (part1 "input.txt"))
(qtest (part2 "test.txt") 5)
(qtest (part2 "input.txt"))