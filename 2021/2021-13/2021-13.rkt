#lang racket

(require "../qtest.rkt")
(require anaphoric)

(define (read-data file)
  (with-input-from-file file
    (thunk
     (define-values (points folds)
       (for/fold ((points (set))
                  (folds '()))
                 ((line (in-lines)))
         (acond ((not (non-empty-string? line))
                 (values points folds))
                ((regexp-match #px"^fold along y=(\\d+)$" line)
                 (define y (string->number (second it)))
                 (define fold (make-rectangular 0 y))
                 (values points (cons fold folds)))
                ((regexp-match #px"^fold along x=(\\d+)$" line)
                 (define x (string->number (second it)))
                 (define fold (make-rectangular x 0))
                 (values points (cons fold folds)))
                ((regexp-match #px"^(\\d+),(\\d+)$" line)
                 (match-define (list x y)
                   (map string->number (rest it)))
                 (define point (make-rectangular x y))
                 (values (set-add points point)
                         folds)))))
     (values points (reverse folds)))))

; Y-axis fold
; 0,14 Y-axis fold at 7 = 0,0
; 1,10                  = 1,4
; 7-(14-7)
; 7-(10-7)

;(- 0+7i (- 0+14i 0+7i))
;(- 0+7i (- 0+10i 0+7i))

(define (fold-points points fld)
  (cond ((= 0 (real-part fld))
         (define fold-y (imag-part fld))
         (for/set ((point points))
           (define-values (x y)
             (values (real-part point)
                     (imag-part point)))
           (cond ((y . < . fold-y)
                  point)
                 (else
                  (make-rectangular x (- fold-y (- y fold-y)))))))
        (else
         (define fold-x (real-part fld))
         (for/set ((point points))
           (define-values (x y)
             (values (real-part point)
                     (imag-part point)))
           (cond ((x . < . fold-x)
                  point)
                 (else
                  (make-rectangular (- fold-x (- x fold-x))
                                    y)))))))

(define (part1 file)
  (define-values (points folds)
    (read-data file))
  (define fld (first folds))
  (define folded-points (fold-points points fld))
  (set-count folded-points))

(qtest (part1 "test.txt") 17)
(qtest (part1 "input.txt"))

(define-values (dot space)
  (if #f
      (values "#" ".")
      (values "█" " ")))

(define (render-points points)
  (define x-max (apply max (set-map points real-part)))
  (define y-max (apply max (set-map points imag-part)))
  (for* ((y (in-inclusive-range 0 y-max))
         (x (in-inclusive-range 0 x-max)))
    (when (and (= x 0)
               (not (= y 0)))
      (newline))
    (display (if (set-member? points (make-rectangular x y)) dot space))))

(define (part2 file)
  (define-values (start-points folds)
    (read-data file))
  (define folded-points
    (for/fold ((points start-points))
              ((fold folds))
      (fold-points points fold)))
  (newline)
  (displayln file)
  (render-points folded-points)
  (newline))

(part2 "test.txt")
(part2 "input.txt")
; CEJKLUGJ

; None of the complex arithmetic worked out as I had hoped.
; Which may be more due to my ignorance of complex arithmetic.
; Two-element vectors would've probably have been better.
; Then it would be easier to write generic code to handle the X & Y cases.

; Harder input from reddit
(part2 "harder.txt")