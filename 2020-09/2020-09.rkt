#lang racket

; I'm just going to use a list for the queue.
; Inefficient, but it works.

(define (enqueue q element)
  #;(writeln q)
  (append q (list element)))

(define (dequeue q)
  #;(writeln q)
  (values (first q)
          (rest q)))

(define (dequeue-q q)
  (rest q))

; Quick test of the queue:
(let loop ((q '())
           (n 0))
  #;(printf "Iteration ~a~n" n)
  (cond ((< n 4)
         (loop (enqueue q n)
               (add1 n)))
        ((= n 4)
         (define-values (_ q2) (dequeue q))
         (loop q2 (add1 n)))
        ((< n 10)
         (loop (enqueue q n)
               (add1 n)))
        (else
         (sequence-for-each displayln q))))

; TODO: Make the above a real test.

; Just keep the buffered numbers in a list
; Start with first number & scan the second+ for a match
; Then go to the second number & scan the third+ for a match
; etc.

(define (find-combo sum q)
  (cond ((empty? q)
         #f)
        (else
         (define n (first q))
         (define tail (rest q))
         (define result (for/or ((m tail))
                          (if (= sum (+ n m))
                              (list n m)
                              #f)))
         (if result
             result
             (find-combo sum tail)))))

(define q (shuffle (build-list 25 add1)))
(find-combo 26 q) ; valid
(find-combo 49 q) ; valid
(find-combo 100 q) ; invalid
(find-combo 50 q) ; invalid
(define q2 (enqueue (remove 20 q) 45))
(find-combo 26 q2) ; valid
(find-combo 65 q2) ; invalid
(find-combo 64 q2) ; valid
(find-combo 66 q2) ; valid

; TODO: Make the above into real tests.

(define (check-file preamble file)
  (with-input-from-file file
    (thunk
     (define q (for/fold ((q '()))
                         ((i (in-range preamble))
                          (line (in-lines)))
                 (enqueue q (string->number line))))
     (for/fold ((q q))
               ((line (in-lines)))
       (define n (string->number line))
       (define matches (find-combo n q))
       (when (not matches) (displayln n))
       #:break (not matches)
       (enqueue (dequeue-q q) n)))))

(displayln "----")
(check-file 5 "test.txt") ; 127
(displayln "----");
(check-file 25 "input.txt")