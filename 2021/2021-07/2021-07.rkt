#lang racket

(require "../qtest.rkt")
(require srfi/26)
(require anaphoric)

(define (do-stuff file calc-cost)
  (with-input-from-file file
    (thunk
     (define positions
       (aand (read-line)
             (string-split it ",")
             (map string->number it)))
     (define minpos (apply min positions))
     (define maxpos (apply max positions))
     (define costs
       (for/list ((target (in-inclusive-range minpos maxpos)))
         (apply + (map (cute calc-cost target <>)
                       positions))))
     (apply min costs))))

(define (part1 file)
  (do-stuff file (compose abs -)))

(qtest (part1 "test.txt") 37)
(qtest (part1 "input.txt"))

(define (part2 file)
  (do-stuff file (λ (target position)
                   (define distance (abs (- target position)))
                   (for/sum ((n (in-inclusive-range 1 distance)))
                     n))))
                   
(qtest (part2 "test.txt") 168)
(qtest (part2 "input.txt"))