#lang racket

(require "../qtest.rkt")
(require threading)
(require math/matrix)
(require math/array)
(require srfi/26)
(require graph)
(require algorithms)

(define (wl x)
  (writeln x)
  x)

(define (read-data file)
  (with-input-from-file file
    (thunk
     (~> (for/list ((line (in-lines)))
           (~> (string->list line)
               (map string _)
               (map string->number _)))
         (list*->matrix _)))))

(define (safe-add x y)
  (if (or (not x)
          (not y))
      #f
      (+ x y)))

(define (foo risk-map row col)
  (define results (xfoo risk-map row col))
  #;(printf "[~a,~a]: ~s\n" row col results)
  results)

(define (xfoo risk-map row col)
  (define next-row (add1 row))
  (define next-col (add1 col))
  (define num-rows (matrix-num-rows risk-map))
  (define num-cols (matrix-num-cols risk-map))
  (cond ((and (= next-row num-rows)
              (= next-col num-cols))
         (list (matrix-ref risk-map row col)))
        ((or (= row num-rows)
             (= col num-cols))
         (list #f))
        (else
         (define add
           (cute safe-add <> (matrix-ref risk-map row col)))
         (~> (append (map add (foo risk-map next-row col))
                     (map add (foo risk-map row next-col)))
             (filter values _)))))

(define (mr m r c)
  (if (or (= r (matrix-num-rows m))
          (r . < . 0)
          (= c (matrix-num-cols m))
          (c . < . 0))
      10
      (matrix-ref m r c)))

(define (bar risk-map row col)
  (printf "(~a,~a): ~a\n" row col (mr risk-map row col))
  (cond ((and (= row 0)
              (= col 0))
         0)
        (else
         (define this-risk (matrix-ref risk-map row col))
         (define row-risk (mr risk-map (sub1 row) col))
         (define col-risk (mr risk-map row (sub1 col)))
         (when (= row-risk col-risk)
           (printf "Equal risk! (~a,~a)\n" row col))
         (cond ((row-risk . < . col-risk)
                (+ this-risk (bar risk-map (sub1 row) col)))
               (else
                (+ this-risk (bar risk-map row (sub1 col))))))))

(define (part1-take1 file)
  (define risk-map (read-data file))
  (~> (foo risk-map 0 0)
      (apply min _)
      (- _ (matrix-ref risk-map 0 0))))

(define (part1-take2 file)
  (define risk-map (read-data file))
  (bar risk-map 9 9))

(define (valid-index? m i)
  (match-define (vector r c) i)
  (define num-rows (matrix-num-rows m))
  (define num-cols (matrix-num-cols m))
  (and (r . >= . 0)
       (c . >= . 0)
       (r . < . num-rows)
       (c . < . num-cols)))

(define (make-edge m source destination)
  (cond ((valid-index? m destination)
         (list (array-ref m destination)
               source
               destination))
        (else #f)))

(define (index-update index which updater)
  (if (symbol=? 'row which)
      (vector (updater (vector-ref index 0))
              (vector-ref index 1))
      (vector (vector-ref index 0)
              (updater (vector-ref index 1)))))

; Edge (weight vertex1 vertex2)
; e.g. (1 (0 0) (0 1))
; Needed the N & W edges too.
(define (matrix->graph m)
  (define edges
    (for/fold ((edges '()))
              ((index (in-array-indexes (array-shape m))))
      (~> (list (make-edge m index (index-update index 'row add1))
                (make-edge m index (index-update index 'col add1))
                (make-edge m index (index-update index 'row sub1))
                (make-edge m index (index-update index 'col sub1)))
          (filter values _)
          (append _ edges))))
  (weighted-graph/directed edges))

(define (part1-take3 file)
  (define risk-map (read-data file))
  (define-values (hash1 hash2)
    (~> (matrix->graph risk-map)
        (dijkstra _ #(0 0))
        #;(bellman-ford _ #(0 0))))
  (define end (vector (sub1 (matrix-num-rows risk-map))
                      (sub1 (matrix-num-cols risk-map))))
  (hash-ref hash1 end))

#;(qtest (part1-take3 "test.txt") 40)
#;(qtest (part1-take3 "input.txt"))
#;(qtest (part1-take3 "test-full.txt") 315)

(define (increment-matrix m n)
  (define (zero->nine n)
    (if (= 0 n)
        9
        n))
  (matrix-map (λ~> (+ n)
                   (modulo 9)
                   zero->nine)
              m))

(define (extend-data-right m)
  (for/list ((i (in-range 0 5)))
    (increment-matrix m i)))

(define (extend-data-down lst)
  (for/list ((i (in-range 0 5)))
    (map (λ (m)
           (increment-matrix m i))
         lst)))

(define (combine-matrices ms)
  (list*->matrix
   (apply append
          (map (λ (row-of-matrices)
                 (~> (map matrix->list* row-of-matrices)
                     (cons append _)
                     (apply map _)))
               ms))))

(define (print-lol lol)
  (for-each (λ (l)
              (for-each display l)
              (newline))
            lol))

(define (print-lolol lolol)
  (for-each (λ (lol)
              (displayln "----")
              (print-lol lol))
            lolol))

(define (print-matrix m)
  (for ((index (in-array-indexes (array-shape m))))
    (when (= 0 (vector-ref index 1))
      (newline))
    (display (array-ref m index))))

(define test-data (read-data "test.txt"))
#;(~> test-data
      extend-data-right
      extend-data-down
      combine-matrices
      print-matrix)

#;(print-matrix
   (combine-matrices
    (extend-data-down
     (extend-data-right test-data))))

(define temp
  '(((1 2 3) (4 5 6) (7 8 9))
    ((10 11 12) (13 14 15) (16 17 18))
    ((19 20 21) (22 23 24) (25 26 27))))

#;(apply map (cons append temp))

(define (part2 file)
  (define risk-map
    (~> (read-data file)
        extend-data-right
        extend-data-down
        combine-matrices))
  (define-values (hash1 hash2)
    (~> (matrix->graph risk-map)
        (dijkstra _ #(0 0))))
  (define end (vector (sub1 (matrix-num-rows risk-map))
                      (sub1 (matrix-num-cols risk-map))))
  (hash-ref hash1 end))

(qtest (part2 "test.txt") 315)
(qtest (part2 "input.txt"))