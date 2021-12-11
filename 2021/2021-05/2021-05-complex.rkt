#lang racket

; Instead of actually storing results in a grid,
; use a hash table of complex numbers.

(require threading)
(require algorithms)
(require srfi/26)
(require "../qtest.rkt")

(define (read-data file)
  (with-input-from-file file
    (thunk
     (~>> (in-lines)
          (sequence-map
           (λ~>> (regexp-match #px"(\\d+),(\\d+) *-> *(\\d+),(\\d+)")
                 rest
                 (map string->number)
                 (chunks-of _ 2)
                 (map (cute apply make-rectangular <>))))
          sequence->list))))

(define (render-line line)
  (match-define (list a b) line)
  (define step (make-rectangular (sgn (- (real-part b)
                                         (real-part a)))
                                 (sgn (- (imag-part b)
                                         (imag-part a)))))
  (let loop ((points (list a)))
    (define current (first points))
    (if (= current b)
        points
        (loop (cons (+ step current)
                    points)))))

(define (add-points-to-hash hash-in points)
  (for/fold ((hsh hash-in))
            ((point points))
    (hash-update hsh point add1 0)))

(define (render-lines lines)
  (for/fold ((hsh (hash)))
            ((line lines))
    (add-points-to-hash hsh (render-line line))))

(define (decompose cn)
  (values (real-part cn)
          (imag-part cn)))

(define (orthogonal? line)
  (match-define (list a b) line)
  (define-values (ra ia) (decompose a))
  (define-values (rb ib) (decompose b))
  (or (= ra rb)
      (= ia ib)))

(define (go file (part 'part2))
  (define filter-proc (if (symbol=? part 'part1)
                          orthogonal?
                          (const #t)))
  (~>> (read-data file)
       (filter filter-proc)
       render-lines
       hash-values
       (count (cute > <> 1))))

(qtest (go "test.txt" 'part1) 5)
(qtest (go "test.txt") 12)
(qtest (go "input.txt" 'part1) 4826)
(qtest (go "input.txt") 16793)