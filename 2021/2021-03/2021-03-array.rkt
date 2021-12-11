#lang racket

; Solving 2021-03 using Racket's math/array library

(require math/array)
(require threading)
(require srfi/26)
(require "../qtest.rkt")

(define (string->bitlist s)
  (~>> s
       string->list
       (map string)
       (map string->number)))

(define (read-data file)
  (with-input-from-file file
    (thunk
     (define bitlistlist
       (~> (in-lines)
           sequence->list
           (map string->bitlist _)))
     (define rows (length bitlistlist))
     (define cols (length (first bitlistlist)))
     (list->array (vector rows cols)
                  (flatten bitlistlist)))))

(define (bool->integer b)
  (if b 1 0))

(define (bit-inverse n)
  (if (n . > . 0) 0 1))

(define (bitarray->number bits)
  (~>> bits
       array->list
       (map number->string)
       (apply string-append)
       (string->number _ 2)))

(define (part1 file)
  (define data (read-data file))
  (define nrows (vector-ref (array-shape data)
                            0))
  (define col-sums (array-axis-sum data 0))
  (define γ-bits (array-map (compose1 bool->integer (cute > <> (/ nrows 2)))
                            col-sums))
  (define ε-bits (array-map bit-inverse γ-bits))
  (* (bitarray->number γ-bits)
     (bitarray->number ε-bits)))

(qtest (part1 "test.txt") 198)
(qtest (part1 "input.txt"))

; TODO: Part 2