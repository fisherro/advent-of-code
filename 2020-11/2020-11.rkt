#lang racket

(require anaphoric)
(require srfi/26)

; A quick unit test function:
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))
(define (expect a b)
  (define result (eval a ns))
  (printf "[~a] \"~a\" returned ~a expected ~a\n"
          (if (equal? result b)
              "PASS"
              "FAIL")
          a
          result
          b))

; Syntactic sugar:
(define ≥ >=)

(define (wl x)
  (writeln x)
  x)

(define (write-seats seats)
  (define last (sub1 (vector-length seats)))
  (printf "#(~a\n" (vector-ref seats 0))
  (for ((i (in-range 1 last)))
    (printf "  ~a\n" (vector-ref seats i)))
  (printf "  ~a)\n" (vector-ref seats last)))

(define (sequence->vector seq)
  (vector->immutable-vector
   (for/vector ((element seq))
     element)))

(define (read-file file)
  (with-input-from-file file
    (thunk
     (sequence->vector
      (in-lines (current-input-port))))))

; Returns: col-length row-length
(define (get-lengths seats)
  (values (string-length (vector-ref seats 0))
          (vector-length seats)))

; Returns #\! if col or row is out-of-range
(define (get-seat seats col row)
  (define-values (col-length row-length) (get-lengths seats))
  (cond [(or (col . < . 0)
             (col . ≥ . col-length)
             (row . < . 0)
             (row . ≥ . row-length))
         #\!]
        [else
         (string-ref (vector-ref seats row)
                     col)]))

(define (get-visible-seat seats col row direction)
  (define delta-col (first direction))
  (define delta-row (second direction))
  (let loop ((c (+ col delta-col))
             (r (+ row delta-row)))
    (let ((position (get-seat seats c r)))
      (case position
        ((#\.)
         (loop (+ c delta-col)
               (+ r delta-row)))
        (else
         position)))))

(define (count-adjacent-occupants seats col row)
  (aand (list (get-seat seats (sub1 col) row)
              (get-seat seats (add1 col) row)
              (get-seat seats col (sub1 row))
              (get-seat seats col (add1 row))
              (get-seat seats (sub1 col) (sub1 row))
              (get-seat seats (sub1 col) (add1 row))
              (get-seat seats (add1 col) (sub1 row))
              (get-seat seats (add1 col) (add1 row)))
        #;(begin (printf "(~a,~a): ~a\n" col row it)
                 it)
        (count (cute char=? #\# <>)
               it)))

(define (count-visible-occupants seats col row)
  (define directions
    '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1)))
  (aand (map (cute get-visible-seat seats col row <>) directions)
        (count (cute char=? #\# <>)
               it)))              

#|
If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
Otherwise, the seat's state does not change.
|#

(define (do-generation-check current-seats)
  (define-values (col-length row-length) (get-lengths current-seats))
  (for/vector ((row row-length))
    (list->string
     (for/list ((col col-length))
       (define this (get-seat current-seats col row))
       (define neighbors
         (count-adjacent-occupants current-seats col row))
       (integer->char (+ neighbors (char->integer #\0)))))))

(define (update-seat-part1 this-seat neighbors)
  (cond [(and (char=? this-seat #\L)
              (= 0 neighbors))
         #\#]
        [(and (char=? this-seat #\#)
              (neighbors . ≥ . 4))
         #\L]
        [else
         this-seat]))

(define (update-seat-part2 this-seat neighbors)
  (cond [(and (char=? this-seat #\L)
              (= 0 neighbors))
         #\#]
        [(and (char=? this-seat #\#)
              (neighbors . ≥ . 5))
         #\L]
        [else
         this-seat]))

(define (do-generation count-neighbors update-seat current-seats)
  (define-values (col-length row-length) (get-lengths current-seats))
  (for/vector ((row row-length))
    (list->string
     (for/list ((col col-length))
       (define this (get-seat current-seats col row))
       (define neighbors (count-neighbors current-seats col row))
       (update-seat this neighbors)))))

(define (do-generation-part1 current-seats)
  (do-generation count-adjacent-occupants
                 update-seat-part1
                 current-seats))

(define (do-generation-part2 current-seats)
  (do-generation count-visible-occupants
                 update-seat-part2
                 current-seats))

(define (count-occupied-seats seats)
  (define (count-row row)
    (for/sum ((c row))
      (if (char=? #\# c) 1 0)))
  (for/sum ((row seats))
    (count-row row)))

(define (count-final-occupants generation-f file)
  (let loop ((current-seats (read-file file)))
    (let ((next-seats (generation-f current-seats)))
      (if (equal? current-seats next-seats)
          (count-occupied-seats next-seats)
          (loop next-seats)))))

(define (part1 file)
  (count-final-occupants do-generation-part1 file))

(expect '(part1 "test.txt") 37)
(expect '(part1 "input.txt") 2126)

(define (part2 file)
  (count-final-occupants do-generation-part2 file))

(expect '(part2 "test.txt") 26)
(expect '(part2 "input.txt") 1914)