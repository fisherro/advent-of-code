#lang racket

(require "../qtest.rkt")
(require threading)

(define ≤ <=)
(define ≥ >=)

; Run simulation
; If target area hit, return highest y reached.
; Otherwise, return false.
(define (simulate Δx₀ Δy₀ min-x max-x min-y max-y)
  (let loop ((x 0)
             (y 0)
             (Δx Δx₀)
             (Δy Δy₀)
             (highest 0))
    (cond ((and (x . ≥ . min-x)
                (x . ≤ . max-x)
                (y . ≥ . min-y)
                (y . ≤ . max-y))
           highest)
          ((or (x . > . max-x)
               (y . < . min-y))
           #f)
          (else
           (loop (+ x Δx)
                 (+ y Δy)
                 (if (Δx . > . 0)
                     (sub1 Δx)
                     Δx)
                 (sub1 Δy)
                 (max y highest))))))

; Since the target area min-x & max-x are both positive,
; the minimum Δx₀ is 1...
; (Actually, it's greater than that, but 1 will do.)
; ...and the maximum Δx₀ is max-x.
; Since the target area min-y & max-y are both negative,
; the minimum Δy₀ is min-y,
; and the maximum Δy₀ is - min-y.
; Simulate all combinations within those ranges.
; Filter out the failures.
; Then find the highest y reached.
(define (part1 target)
  (match-define (list min-x max-x min-y max-y)
    target)
  (~> (for*/list ((Δx₀ (in-inclusive-range 1 max-x))
                  (Δy₀ (in-inclusive-range min-y (- min-y))))
        (simulate Δx₀ Δy₀ min-x max-x min-y max-y))
      (filter values _)
      (apply max _)))

; See above for the logic of the Δx₀ Δy₀ ranges to test.
; After simulating all Δx₀/Δy₀ combinations,
; filter out the failures,
; and count what remains.
(define (part2 target)
  (match-define (list min-x max-x min-y max-y)
    target)
  (~> (for*/list ((Δx₀ (in-inclusive-range 1 max-x))
                  (Δy₀ (in-inclusive-range min-y (- min-y))))
        (simulate Δx₀ Δy₀ min-x max-x min-y max-y))
      (filter values _)
      (length _)))

; target area: x=20..30, y=-10..-5"
(define test-target '(20 30 -10 -5))
; target area: x=240..292, y=-90..-57"
(define real-target '(240 292 -90 -57))

(qtest (part1 test-target) 45)
(qtest (part1 real-target) 4005)

(qtest (part2 test-target) 112)
(qtest (part2 real-target) 2953)