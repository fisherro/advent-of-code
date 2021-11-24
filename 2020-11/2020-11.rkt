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

(define (get-seat seats col row)
  (define-values (col-length row-length) (get-lengths seats))
  (cond [(or (col . < . 0)
             (col . ≥ . col-length)
             (row . < . 0)
             (row . ≥ . row-length))
         #\.]
        [else
         (string-ref (vector-ref seats row)
                     col)]))

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

(define (do-generation current-seats)
  (define-values (col-length row-length) (get-lengths current-seats))
  (for/vector ((row row-length))
    (list->string
     (for/list ((col col-length))
       (define this (get-seat current-seats col row))
       (define neighbors
         (count-adjacent-occupants current-seats col row))
       (cond [(and (char=? this #\L)
                   (= 0 neighbors))
              #\#]
             [(and (char=? this #\#)
                   (neighbors . ≥ . 4))
              #\L]
             [else
              this])))))

(define (count-occupied-seats seats)
  (define (count-row row)
    (for/sum ((c row))
      (if (char=? #\# c) 1 0)))
  (for/sum ((row seats))
    (count-row row)))
  
(define (part1 file)
  (let loop ((current-seats (read-file file)))
    (let ((next-seats (do-generation current-seats)))
      (if (equal? current-seats next-seats)
          (count-occupied-seats next-seats)
          (loop next-seats)))))

(expect '(part1 "test.txt") 37)
(expect '(part1 "input.txt") 2126)