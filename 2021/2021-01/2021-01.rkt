#lang racket

(require anaphoric)
(require "../qtest.rkt")
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))
;(require test-engine/racket-tests)

(qtest ns '(+ 2 2) 4)

(define (add-em a b)
  (+ a b))

(qtest ns '(add-em 2 2) 4)

(qtest ns '(add-em 5 5))

#|
; Racket's tests only print details on failure,
; but I want details on success too.
(check-expect (+ 2 2) 4)
(check-expect (add-em 2 2) 7)
(test)
|#

(define (read-data file)
  (with-input-from-file file
    (thunk
     (aand (sequence->list (in-lines))
           (map string->number it)))))

(define (part1 file)
  (define data (read-data file))
  (let loop ((count 0)
             (prev (first data))
             (remaining (rest data)))
    (cond ((null? remaining)
           count)
          (else
           (define next (first remaining))
           (define new-count (if (next . > . prev)
                                 (add1 count)
                                 count))
           (loop new-count next (rest remaining))))))

(qtest ns '(part1 "test.txt") 7)
(qtest ns '(part1 "input.txt"))

(define (part2 file)
  (define data (read-data file))
  (let loop ((count 0)
             (window (take data 3))
             (remaining (drop data 3)))
    #;(printf "count: ~s; window: ~s; remaining ~s\n" count window remaining)
    (cond ((null? remaining)
           count)
          (else
           (define prev-sum (apply + window))
           (define new-window (append (rest window)
                                      (list (first remaining))))
           (define next-sum (apply + new-window))
           (define next-count (if (next-sum . > . prev-sum)
                                  (add1 count)
                                  count))
           (loop next-count new-window (rest remaining))))))

(qtest ns '(part2 "test.txt") 5)
(qtest ns '(part2 "input.txt"))