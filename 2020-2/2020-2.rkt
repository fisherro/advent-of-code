#lang racket

;;; TODO: Use cut/cute or something similar for predicate?

;1-3 a: abcde
;1-3 b: cdefg
;2-9 c: ccccccccc

(define rx #rx"^([0-9]+)-([0-9]+) ([a-z]): (.+)$")

(define line "1-3 a: abcde")

(define (process callback)
  (with-input-from-file "input.txt"
    (λ ()
      (let loop ((line (read-line))
                 (valid-count 0))
        (if (eof-object? line)
            (displayln valid-count)
            (let* ((matches (regexp-match rx line))
                   (min (string->number (second matches)))
                   (max (string->number (third matches)))
                   (char (string-ref (fourth matches) 0))
                   (pw (fifth matches))
                   (is-valid (callback min max char pw)))
              (if is-valid
                  (loop (read-line)
                        (add1 valid-count))
                  (loop (read-line)
                        valid-count))))))))

;; part 1
(process
 (λ (min max char pw)
   (let ((count (sequence-count (λ (x)
                                  (equal? x char))
                                pw)))
     (and (>= count min)
          (<= count max)))))

;; part 2
(process
 (λ (pos1 pos2 char pw)
   (xor (equal? char (string-ref pw (sub1 pos1)))
        (equal? char (string-ref pw (sub1 pos2))))))