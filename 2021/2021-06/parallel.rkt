#lang racket

(require future-visualizer)

; Experimenting with parallization & the table for part 2

(define (calc-children start days)
  (quotient (+ days (- 6 start)) 7))

; This still takes a while, but it uses a tractable about of memory.
(define (calc-descendants start days)
  (define adjusted-days (+ days (- 6 start)))
  (let loop ((current-days (- adjusted-days 7))
             (count 1))
    (if (current-days . < . 0)
        count
        (loop (- current-days 7)
              (+ count (calc-descendants 8 current-days))))))

;(define start-values '(1 2 3 4 5))
(define start-values '(0 1 2 3 4 5 6))

(define (serial-calc-table days)
  (for/list ((i start-values))
    (list i (calc-descendants i days))))

(define (parallel-calc-table days)
  (define futures
    (for/list ((i start-values))
      (future (thunk (list i (calc-descendants i days))))))
  (map touch futures))

;(define day-count 256)
(define day-count 192)

(time (serial-calc-table day-count))
;(visualize-futures
 (time (parallel-calc-table day-count))
; )