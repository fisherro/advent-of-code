#lang racket

(require "../qtest.rkt")
(require srfi/26)
(require anaphoric)

(define (read-data file)
  (with-input-from-file file
    (thunk
     (aand (regexp-match* #px"(\\d+),(\\d+) *-> *(\\d+),(\\d+)\\D+"
                          (current-input-port)
                          #:match-select rest)
           (map (cute map (compose1 string->number bytes->string/utf-8) <>)
                it)))))

(define (is-linear? line)
  (or (= (first line)
         (third line))
      (= (second line)
         (fourth line))))

(define (is-xline? line)
  (= (second line)
     (fourth line)))

(define (is-yline? line)
  (= (first line)
     (third line)))

(define (unzip lst)
  (for/fold ((xs '())
             (ys '()))
            ((i (in-range 0 (length lst)))
             (n lst))
    (if (even? i)
        (values (cons n xs) ys)
        (values xs (cons n ys)))))

(define (get-maxes lines)
  (define-values
    (xs ys)
    (unzip (flatten lines)))
  (values (apply max xs)
          (apply max ys)))

; No negatives in input file! W00t!

(struct space (xmax ymax data) #:transparent)

; I think I got my x/y messed up, but I'm going to fix it with a hammer.
(define (make-space xmax ymax)
  (let* ((new-xmax (add1 (max xmax ymax)))
         (new-ymax new-xmax))
    (space new-xmax new-ymax (make-vector (* new-xmax new-ymax) ))))

#;(define (make-space xmax ymax)
    (let ((xmax (add1 xmax))
          (ymax (add1 ymax)))
      (space xmax ymax (make-vector (* xmax ymax) 0))))

#;(define my-space (make-space xmax ymax))

(define (space-get the-space x y)
  ;(printf "space-get x=~a y=~a\n" x y)
  (define index (+ x (* y (space-ymax the-space))))
  (vector-ref (space-data the-space) index))

(define (show-space the-space)
  (for ((y (in-range 0 (space-ymax the-space))))
    (for ((x (in-range 0 (space-xmax the-space))))
      (printf "~a " (space-get the-space x y)))
    (newline))
  (newline))

(define (space-add1! the-space x y)
  (define index (+ x (* y (space-ymax the-space))))
  ;(printf "space-get x=~a y=~a index=~a\n" x y index)
  ;(printf "ymax: ~a\n" (space-ymax the-space))
  ;(printf "space size: ~a\n" (vector-length (space-data the-space)))
  (vector-set! (space-data the-space)
               index
               (add1 (vector-ref (space-data the-space)
                                 index))))

#;(space-add1! my-space 2 4)

(define (space-add-x! the-space x1 x2 y)
  (if (x2 . < . x1)
      (space-add-x! the-space x2 x1 y)
      (for ((x (in-inclusive-range x1 x2)))
        (space-add1! the-space x y))))

(define (space-add-y! the-space x y1 y2)
  (if (y2 . < . y1)
      (space-add-y! the-space x y2 y1)
      (for ((y (in-inclusive-range y1 y2)))
        (space-add1! the-space x y))))

(define (space-add-xy! the-space x1 y1 x2 y2)
  (for ((x (in-inclusive-range x1 x2 (if (x1 . < . x2) 1 -1)))
        (y (in-inclusive-range y1 y2 (if (y1 . < . y2) 1 -1))))
    (space-add1! the-space x y)))

#;(space-add-x! my-space 1 8 4)
#;(space-add-y! my-space 4 1 8)

(define (draw-lines! the-space data)
  (for ((line data))
    (match-define (list x1 y1 x2 y2) line)
    (cond ((is-xline? line)
           (space-add-x! the-space x1 x2 y1))
          ((is-yline? line)
           (space-add-y! the-space x1 y1 y2)))))

(define (draw-lines2! the-space data)
  (for ((line data))
    (match-define (list x1 y1 x2 y2) line)
    (cond ((is-xline? line)
           (space-add-x! the-space x1 x2 y1))
          ((is-yline? line)
           (space-add-y! the-space x1 y1 y2))
          (else
           (space-add-xy! the-space x1 y1 x2 y2)))))

#;(show-space my-space)

(define (part1 file)
  (define lines (read-data file))
  (define-values (xmax ymax) (get-maxes lines))
  ;(printf "xmax: ~a; ymax: ~a\n" xmax ymax)
  (define my-space (make-space xmax ymax))
  (draw-lines! my-space lines)
  ;(show-space my-space)
  (vector-count (cute > <> 1)
                (space-data my-space)))

(define (part2 file)
  (define lines (read-data file))
  (define-values (xmax ymax) (get-maxes lines))
  (define my-space (make-space xmax ymax))
  (draw-lines2! my-space lines)
  (vector-count (cute > <> 1)
                (space-data my-space)))

(qtest (part1 "test.txt") 5)
(qtest (part1 "input.txt"))

;space-get x=643 y=989 index=980742
;ymax: 991
;space size: 980099

(qtest (part2 "test.txt") 12)
(qtest (part2 "input.txt"))

; Maybe refactor with Racket's math arrays?