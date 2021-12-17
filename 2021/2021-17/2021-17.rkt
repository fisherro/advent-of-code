#lang racket

; +x is forward; +y is upward

#|
Find the initial velocity that causes the probe to
reach the highest y position and still eventually
be within the target area after any step. What is
the highest y position it reaches on this
trajectory?
|#

#|
The probe's x position increases by its x velocity.

The probe's y position increases by its y velocity.

Due to drag, the probe's x velocity changes by 1
toward the value 0; that is, it decreases by 1 if
it is greater than 0, increases by 1 if it is less
than 0, or does not change if it is already 0.

Due to gravity, the probe's y velocity decreases by 1.
|#

(require "../qtest.rkt")

#|
In the above example, using an initial velocity of
6,9 is the best you can do, causing the probe to
reach a maximum y position of 45.
|#
(define test-input "target area: x=20..30, y=-10..-5")
(define test-target '(20 30 -10 -5))
(define real-input "target area: x=240..292, y=-90..-57")
(define real-target '(240 292 -90 -57))

(define (try-y initial-dy min-y max-y)
  (let loop ((y 0)
             (dy initial-dy)
             (highest 0))
    (cond ((and (y . <= . max-y)
                (y . >= . min-y))
           (printf "~a; ~a; ~a\n" initial-dy y highest)
           highest)
          ((y . < . min-y)
           (printf "~a missed\n" initial-dy)
           #f)
          (else
           (loop (+ y dy)
                 (sub1 dy)
                 (max y highest))))))

(define (find-highest-y min-y max-y)
  (let loop ((dy 0)
             (prev 0))
    (define result (try-y dy min-y max-y))
    (if result
        (loop (add1 dy)
              result)
        prev)))

(define (find-highest-y-2 min-y max-y)
  (let loop ((dy 0)
             (prev 0))
    (define result (try-y dy min-y max-y))
    (if (dy . < . 100)
        (loop (add1 dy)
              result)
        prev)))

; PART 1 solutions:
#;(qtest (find-highest-y -10 -5) 45)
#;(qtest (find-highest-y-2 -90 -57))
; 946 was wrong; 4005 was right, with an initial dy of 89

; sed 's/^.*,//' test-dy0.txt | sort -un
; dy: -10 to 9
; dx: 6 to 30

#|
(displayln "trying dy values:")
(for ((dy0 (in-inclusive-range -100 100)))
  (try-y dy0 -10 -5))
|#

(define (try-x dx0 min-x max-x)
  (let loop ((x 0)
             (dx dx0))
    (cond ((and (x . <= . max-x)
                (x . >= . min-x))
           (printf "dx0: ~a; final x: ~a\n" dx0 x))
          ((x . > . max-x)
           (printf "dx0: ~a missed\n" dx0))
          ((= dx 0)
           (printf "dx0: ~a stopped at ~a\n" dx0 x))
          (else
           (loop (+ x dx)
                 (if (dx . > . 0)
                     (sub1 dx)
                     0))))))

#|
(displayln "trying dx values:")
(for ((dx0 (in-inclusive-range 1 100)))
  (try-x dx0 20 30))
|#

; Smallest dx to try: 1
; Largest  dx to try: max-x
; Smallest dy to try: min-y
; Largest  dy to try: 9 or 4005

(define (part2-try-x dx0 min-x max-x)
  (let loop ((x 0)
             (dx dx0))
    (cond ((and (x . <= . max-x)
                (x . >= . min-x))
           #t)
          ((x . > . max-x)
           #f)
          ((= dx 0)
           #f)
          (else
           (loop (+ x dx)
                 (if (dx . > . 0)
                     (sub1 dx)
                     0))))))

(define (part2-try-y dy0 min-y max-y)
  (let loop ((y 0)
             (dy dy0))
    (cond ((and (y . <= . max-y)
                (y . >= . min-y))
           #t)
          ((y . < . min-y)
           #f)
          (else
           (loop (+ y dy)
                 (sub1 dy))))))

(define (get-all-dx0 min-x max-x)
  (for/fold ((good '()))
            ((dx0 (in-inclusive-range 1 max-x)))
    (if (part2-try-x dx0 min-x max-x)
        (cons dx0 good)
        good)))

(define (get-all-dy0 min-y max-y)
  (for/fold ((good '()))
            ((dy0 (in-inclusive-range min-y 5000)))
    (if (part2-try-y dy0 min-y max-y)
        (cons dy0 good)
        good)))

#|
30
$ sed 's/^.*,//' test-dy0.txt | sort -un | wc -l
      20
$ sed 's/,.*$//' test-dy0.txt | sort -un | wc -l
      21
|#

#|
(length (get-all-dx0 20 30))
(length (get-all-dy0 -10 -5))
(* (length (get-all-dx0 20 30))
   (length (get-all-dy0 -10 -5)))
(for ((dx0 (get-all-dx0 20 30)))
  (for ((dy0 (get-all-dy0 -10 -5)))
    (printf "~a,~a\n" dx0 dy0)))
|#

(define (try-both dx0 dy0 min-x max-x min-y max-y)
  (let loop ((x 0)
             (y 0)
             (dx dx0)
             (dy dy0))
    (cond ((and (x . <= . max-x)
                (x . >= . min-x)
                (y . <= . max-y)
                (y . >= . min-y))
           #t)
          ((or #;(= dx 0)
               (x . > . max-x)
               (y . < . min-y))
           #;(printf "dx: ~a; x: ~a; y: ~a\n" dx x y)
           #f)
          (else
           (loop (+ x dx)
                 (+ y dy)
                 (if (dx . > . 0)
                     (sub1 dx)
                     0)
                 (sub1 dy))))))

; Is it wasted work to get the good dx0s & dy0s first?
(define (part2 min-x max-x min-y max-y)
  (for*/fold ((count 0))
             ((dx0 (get-all-dx0 min-x max-x))
              (dy0 (get-all-dy0 min-y max-y)))
    (if (try-both dx0 dy0 min-x max-x min-y max-y)
        (add1 count)
        count)))

;(qtest (part2 20 30 -10 -5) 112)
(qtest (part2 240 292 -90 -57))

#|
(define known-good
  (with-input-from-file "test-good-sexp.txt"
    (thunk (read))))
(for/fold ((failed '()))
          ((start known-good))
  (match-define (list dx0 dy0) start)
  (define ok (try-both dx0 dy0 20 30 -10 -5))
  (if ok failed (cons start failed)))
|#

#|
known good that failed:
((6 9)
  (6 6)
  (7 9)
  (7 5)
  (6 5)
  (7 4)
  (7 6)
  (7 8)
  (7 3)
  (6 2)
  (7 7)
  (6 4)
  (6 7)
  (6 3)
  (6 8))
Bug found & fixed!
|#