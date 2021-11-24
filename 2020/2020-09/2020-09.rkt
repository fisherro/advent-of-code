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

(define (check-list preamble lst)
  (define q (take lst preamble))
  (for/fold ((q q))
            ((n (drop lst preamble))
             #:break (not (list? q)))
    (define matches (find-combo n q))
    (cond (matches
           (enqueue (dequeue-q q) n))
          (else
           n))))

(define (check-file preamble file)
  (with-input-from-file file
    (thunk
     (check-list preamble (sequence->list
                           (sequence-map string->number (in-lines)))))))
    
(displayln "----")
(check-file 5 "test.txt") ; 127
(displayln "----");
(check-file 25 "input.txt") ; 32321523
(displayln "----");

(define (find-run sum lst)
  (cond ((> (length lst) 1)
         (define result
           (for/or ((len (in-range 2 (length lst))))
             (define run (take lst len))
             (define run-sum (foldl + 0 run))
             (cond ((= run-sum sum)
                    run)
                   ((> run-sum sum)
                    #t)
                   (else
                    #f))))
         (cond ((list? result)
                result)
               (else
                (find-run sum (rest lst)))))
        (else
         #f)))

(define (find-weakness-in-file preamble file)
  (with-input-from-file file
    (thunk
     (define data (sequence->list
                   (sequence-map string->number (in-lines))))
     (define invalid (check-list preamble data))
     (define run (find-run invalid data))
     (define weakness (+ (argmin values run)
                         (argmax values run)))
     #;(writeln data)
     #;(writeln invalid)
     #;(writeln weakness)
     (printf
      "The encryption weakness of \"~a\" using a preamble length of ~a is ~a.\n"
      file
      preamble
      weakness))))

(find-weakness-in-file 5 "test.txt")
(find-weakness-in-file 25 "input.txt")
