#lang racket

(require anaphoric)
(require srfi/26)

; Points are 2-element vectors: #(x y)
; Folds are 2-element vectors: #(index coord)
; So, "fold along y=7" would be #(1 7)
; And, "fold along x=4" would be #(0 4)
(define (read-data file)
  (with-input-from-file file
    (thunk
     (define-values (points folds)
       (for/fold ((points (set))
                  (folds '()))
                 ((line (in-lines)))
         (acond ((not (non-empty-string? line))
                 (values points folds))
                ((regexp-match #px"^(\\d+),(\\d+)$" line)
                 (match-define (list x y)
                   (map string->number (rest it)))
                 (values (set-add points (vector x y))
                         folds))
                ((regexp-match #px"^fold along ([xy])=(\\d+)$" line)
                 (match-define (list _ coord n-string)
                   it)
                 (define n (string->number n-string))
                 (define i (case coord
                             (("x") 0)
                             (("y") 1)))
                 (values points (cons (vector i n)
                                      folds))))))
     (values points (reverse folds)))))

; Given a 2-element vector, v
; Return a new vector with the value at index i relpaced with c.
; Surely there's an easier way.
(define (point-update v i c)
  (if (= i 0)
      (vector c (vector-ref v 1))
      (vector (vector-ref v 0) c)))

(define (fold-points points f)
  (match-define (vector i n) f)
  (for/set ((p points))
    (define c (vector-ref p i))
    (cond ((c . < . n)
           p)
          (else
           (point-update p i (- n (- c n)))))))

(define (render-points points)
  (define x-max (apply max (set-map points (cute vector-ref <> 0))))
  (define y-max (apply max (set-map points (cute vector-ref <> 1))))
  (for* ((y (in-inclusive-range 0 y-max))
         (x (in-inclusive-range 0 x-max)))
    (when (and (= x 0)
               (not (= y 0)))
      (newline))
    (display (if (set-member? points (vector x y))
                 "█"
                 " "))))

(define (part2 file)
  (define-values (start-points folds)
    (read-data file))
  (define folded-points
    (for/fold ((points start-points))
              ((f folds))
      (fold-points points f)))
  (newline)
  (displayln file)
  (render-points folded-points)
  (newline))

(part2 "input.txt")