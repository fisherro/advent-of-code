#! "/Applications/Racket v8.3/bin/racket"
#lang racket

(require anaphoric)

(define (read-data)
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
  (values points (reverse folds)))

(define (split-complex c)
  (values (real-part c)
          (imag-part c)))

(define (fold-points points fold-spec)
  (define (fold-coord f p)
    (if (or (= f 0)
            (p . < . f))
        p
        (- (* 2 f)
           p)))
  (define-values (fx fy)
    (split-complex fold-spec))
  (for/set ((point points))
    (define-values (px py)
      (split-complex point))
    (make-rectangular (fold-coord fx px)
                      (fold-coord fy py))))

(define (render-points points)
  (define dot "█")
  (define space " ")
  (define x-max (apply max (set-map points real-part)))
  (define y-max (apply max (set-map points imag-part)))
  (for* ((y (in-inclusive-range 0 y-max))
         (x (in-inclusive-range 0 x-max)))
    (when (and (= x 0)
               (not (= y 0)))
      (newline))
    (display (if (set-member? points (make-rectangular x y))
                 dot
                 space))))

(define (decode)
  (define-values (starting-points folds)
    (read-data))
  (define folded-points
    (for/fold ((points starting-points))
              ((fold-spec folds))
      (fold-points points fold-spec)))
  (render-points folded-points)
  (newline))

(define test-data
  (string-append
   "27,0\n27,1\n26,2\n28,2\n26,3\n26,4\n26,5\n29,5\n28,6\n18,2\n20,2\n16,3\n17,4\n"
   "19,4\n20,5\n16,6\n18,6\n20,6\n10,2\n12,2\n9,3\n13,3\n9,4\n11,4\n13,4\n9,5\n10,"
   "6\n12,6\n1,0\n3,0\n5,0\n2,1\n5,1\n3,2\n3,3\n3,4\n3,5\n2,6\n4,6\n26,15\n25,14\n"
   "27,14\n29,14\n27,13\n27,12\n27,11\n27,10\n17,14\n19,14\n21,14\n17,13\n18,12\n2"
   "0,12\n21,11\n17,10\n19,10\n9,14\n11,14\n8,13\n12,13\n8,12\n10,12\n12,12\n8,11"
   "\n9,10\n11,10\n0,16\n2,16\n4,16\n0,15\n3,15\n2,14\n2,13\n2,12\n2,11\n1,10\n3,10"
   "\n\nfold along y=8\n"))

(define (decode-test)
  (with-input-from-string test-data
    (thunk (decode))))

; I'm not sure this is the best way, but...
; If TERM isn't defined, assume we're running in DrRacket.
(if (getenv "TERM")
    (decode)
    (decode-test))