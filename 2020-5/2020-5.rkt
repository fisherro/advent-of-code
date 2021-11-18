#lang racket

;; Half open range: Max is one past the end
(define (find-half min max)
  (+ min
     (quotient (- max min)
               2)))

;(find-half 0 128)
;(find-half 0 64)
;(find-half 0 32)
;(find-half 0 16)
;(find-half 0 8)
;(find-half 0 4)
;(find-half 0 2)
;(find-half 0 1)

;; Could add upper & lower characters.
;; Then stop parsing when unrecognized char is seen.
;; I should be able to treat the string as a sequence instead, right?
(define (spec->number spec min max)
  (if (equal? min (sub1 max))
      (values min spec)
      (let ((rst (substring spec 1))
            (half (find-half min max))
            (current (string-ref spec 0)))
        (if (or (equal? current #\F)
                (equal? current #\L))
            (spec->number rst min half)
            (spec->number rst half max)))))

(define (spec->row-col spec)
  (let-values (((row rst)
                (spec->number spec 0 128)))
    (let-values (((col rst)
                  (spec->number rst 0 8)))
      (values row col))))

(define (row-col->id row col)
  (+ col (* row 8)))

;; I'm sure there's an easier way I'm forgetting.
(define (spec->id spec)
  (let-values (((row col)
                (spec->row-col spec)))
    (row-col->id row col)))

(define (test spec)
  (let-values (((row col) (spec->row-col spec))
               ((id) (spec->id spec)))
    (printf "~a: row ~a, column ~a, seat ID ~a.~n" spec row col id)))
  
;FBFBBFFRLR: row 44, column 5, seat ID 357.
;BFFFBBFRRR: row 70, column 7, seat ID 567.
;FFFBBBFRRR: row 14, column 7, seat ID 119.
;BBFFBBFRLL: row 102, column 4, seat ID 820.

(test "FBFBBFFRLR")
(test "BFFFBBFRRR")
(test "FFFBBBFRRR")
(test "BBFFBBFRLL")

(display "Highest seat ID in input: ")
(with-input-from-file "input.txt"
  (λ ()
    (sequence-fold
     max
     0
     (sequence-map spec->id (in-lines)))))

(display "Highest seat ID possible: ")
(row-col->id 127 8)

(define seats (make-vector 1025 #f))
(with-input-from-file "input.txt"
  (λ ()
    (sequence-for-each
     (λ (id)
       (vector-set! seats id #t))
     (sequence-map spec->id (in-lines)))))
(define first-seat (vector-member #t seats))
(define good-seats (vector-drop seats first-seat))
(define my-seat (+ first-seat (vector-member #f good-seats)))
(vector-ref seats (sub1 my-seat))
(vector-ref seats my-seat)
(vector-ref seats (add1 my-seat))
(printf "My seat: ~a" my-seat)

