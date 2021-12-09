#lang typed/racket

; A so-far unsuccessful attempt to port the solution to Typed Racket

(require anaphoric)
(require "../qtest.rkt")

; Testing qtest itself...
(qtest (+ 2 2) 4)

(define (add-em a b)
  (+ a b))

(qtest (add-em 2 2) 4)

(qtest (add-em 5 5))

; TODO: Use Positive-Integer?
(: read-data (-> Path-String (Listof Integer)))
(define (read-data file)
  (with-input-from-file file
    (thunk
     ; The fact that aand can return false complicates things.
     ; Let's not use it for now.
     (define results (map string->number (sequence->list (in-lines))))
     (if (andmap integer? results)
         results
         0 #;(raise results))
     #;(aand (sequence->list (in-lines))
             (map string->number it)))))

(: part1 (-> Path-String Integer))
(define (part1 file)
  (define data (read-data file))
  (let loop ((count : Integer 0)
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

(: part2 (-> String Integer))
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

#|
(: part2-vector (-> String Integer))
(define (part2-vector file)
  (: sum-window (-> (Vectorof Integer) Integer Integer))
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
|#