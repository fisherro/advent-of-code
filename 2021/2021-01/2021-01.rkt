#lang racket

(require anaphoric)
(require "../qtest.rkt")

; Testing qtest itself...
(qtest (+ 2 2) 4)

(define (add-em a b)
  (+ a b))

(qtest (add-em 2 2) 4)

(qtest (add-em 5 5))

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

(qtest (part1 "test.txt") 7)
(qtest (part1 "input.txt"))

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

(qtest (part2 "test.txt") 5)
(qtest (part2 "input.txt"))

(define (part2-vector file)
  (define (sum-window data index)
    ((compose + vector->values) data index (+ index 3)))
  ; Is compose always preferable to call-with-values?
  ; It may be more performant.
  #;(define (sum-window data index)
    (call-with-values (thunk (vector->values data index (+ index 3)))
                      +))
  (define data (list->vector (read-data file)))
  #;(printf "data: ~s\n" data)
  (for/fold ((count 0))
            ((index (in-range 3 (vector-length data))))
    (define w1-index (- index 3))
    (define w2-index (- index 2))
    (define w1-sum (sum-window data w1-index))
    (define w2-sum (sum-window data w2-index))
    #;(printf "index: ~s; w1-index: ~s; w2-index: ~s\n" index w1-index w2-index)
    (if (w2-sum . > . w1-sum)
        (add1 count)
        count)))

(qtest (part2-vector "test.txt") 5)
(qtest (part2-vector "input.txt"))