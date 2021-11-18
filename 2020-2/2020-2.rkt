#lang racket

(require srfi/26)

;;; Examples of the lines we'll have to parse:
;1-3 a: abcde
;1-3 b: cdefg
;2-9 c: ccccccccc

;; The regexp for parsing the lines of the input
(define rx #rx"^([0-9]+)-([0-9]+) ([a-z]): (.+)$")

;;; How best to do the transforms?

;; Compose list-update
;; This smells inefficient.
(define transform1
  (compose1 (cute list-update <> 2 (cute string-ref <> 0))
            (cute list-update <> 1 string->number)
            (cute list-update <> 0 string->number)))

;; Quasiquotation
;; It seems a shame to walk the list separately for each element.
(define (transform2 lst)
  `(,(string->number (first lst))
    ,(string->number (second lst))
    ,(string-ref (third lst) 0)
    ,(fourth lst)))

;; for/list
;; This seems like it would be the more efficient option.
(define (transform3 lst)
  (for/list (((value i) (in-indexed lst)))
    (case i
      ((0 1) (string->number value))
      ((2) (string-ref value 0))
      (else value))))

;; Use callback to validate each line of the input.
;; Return the count of valid lines.
(define (process callback)
  (with-input-from-file "input.txt"
    (thunk
     (sequence-count
      (cute apply callback <>)
      (sequence-map
       (compose1
        ;transform1
        ;transform2
        transform3
        rest
        (cute regexp-match rx <>))
       (in-lines))))))

;; Part 1: Expects 424
(process
 (λ (min max char pw)
   (let ((count (sequence-count (cute equal? char <>)
                                pw)))
     (and (>= count min)
          (<= count max)))))

;; Part 2: Expects 747
(process
 (λ (pos1 pos2 char pw)
   (xor (equal? char (string-ref pw (sub1 pos1)))
        (equal? char (string-ref pw (sub1 pos2))))))