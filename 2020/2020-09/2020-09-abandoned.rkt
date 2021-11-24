#lang racket

#|
Do I need to create my own queue for this exercise?
No, a simple list would probably be good enough.
But I'm doing it anyway.

I first learned about this from...
Chris Okasaki's Purely Functional Data Structures,
but he cites other sources.

There is a "front" list & a "back" list.
Enqueuing cons' onto the front of the back list.
Dequeuing pull from the front of the front list.
When front is empty, we replace it with the reverse back.
Use prop:sequence to make it usable as a sequence.
|#
(struct queue (front back)
  #:transparent
  #:property prop:sequence
  (λ (q)
    ; The order is screwy, but good enough for my purposes.
    (in-sequences (queue-front q)
                  (queue-back q))))

(define (enqueue q element)
  #;(writeln q)
  (queue (queue-front q)
         (cons element (queue-back q))))

(define (dequeue q)
  #;(writeln q)
  (define q-front (queue-front q))
  (define q-back (queue-back q))
  (if (null? q-front)
      (if (null? q-back)
          #f
          (let ((new-front (reverse q-back)))
            (values (queue (rest new-front)
                           '())
                    (first new-front))))
      (values (queue (rest q-front)
                     q-back)
              (first q-front))))

; Quick test of the queue:
(let loop ((q (queue '() '()))
           (n 0))
  #;(printf "Iteration ~a~n" n)
  (cond ((< n 4)
         (loop (enqueue q n)
               (add1 n)))
        ((= n 4)
         (define-values (q2 _) (dequeue q))
         (loop q2 (add1 n)))
        ((< n 10)
         (loop (enqueue q n)
               (add1 n)))
        (else
         (sequence-for-each displayln q))))

; Just keep the buffered numbers in a list
; Start with first number & scan the second+ for a match
; Then go to the second number & scan the third+ for a match
; etc.

; Hmm. This looks like it ends up being very inefficient.
; It'd probably be better to either...
; ...use functions specific to the queue rather than sequence functions
; ...or just use a list & deal with the enqueue/dequeue overhead
(define (find-combo sum q)
  (cond ((= 0 (sequence-length q))
         #f)
        (else
         (define n (sequence-ref q 0))
         (define tail (sequence-tail q 1))
         (define m (for/or ((m tail))
                     (if (= sum (+ n m))
                         (list n m)
                         #f)))
         (if m
             m
             (find-combo sum tail)))))

(define q (queue '() '(1 2 3 4 5)))
(find-combo 2 q)
(find-combo 8 q)