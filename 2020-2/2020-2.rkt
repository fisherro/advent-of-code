#lang racket

(require srfi/26)

;1-3 a: abcde
;1-3 b: cdefg
;2-9 c: ccccccccc

(define rx #rx"^([0-9]+)-([0-9]+) ([a-z]): (.+)$")

(define (process callback)
  (with-input-from-file "input.txt"
    (thunk (for/fold ((valid-count 0))
                     ((line (in-lines)))
             (let* ((matches (regexp-match rx line))
                    (min (string->number (second matches)))
                    (max (string->number (third matches)))
                    (char (string-ref (fourth matches) 0))
                    (pw (fifth matches)))
               (if (callback min max char pw)
                   (add1 valid-count)
                   valid-count))))))

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