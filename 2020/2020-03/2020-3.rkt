#lang racket

(require locale/format)

(define (count-trees right down)
  (with-input-from-file "input.txt"
    (thunk
     (for/fold ((pos 0)
                (down-pos 0)
                (tree-count 0)
                #:result tree-count)
               ((line (in-lines)))
       (let ((new-down (remainder (add1 down-pos) down)))
         (cond ((= pos 0)
                (values (+ pos right)
                        new-down
                        tree-count))
               ((not (= down-pos 0))
                (values pos
                        new-down
                        tree-count))
               (else
                (let ((char (string-ref
                             line
                             (remainder
                              pos
                              (string-length line)))))
                  (values (+ pos right)
                          new-down
                          (if (equal? #\# char)
                              (add1 tree-count)
                              tree-count))))))))))

(printf "1,1: ~a (expect 84)~n" (count-trees 1 1))
(printf "3,1: ~a (expect 289)~n" (count-trees 3 1))
(printf "5,1: ~a (expect 89)~n" (count-trees 5 1))
(printf "7,1: ~a (expect 71)~n" (count-trees 7 1))
(printf "1,2: ~a (expect 36)~n" (count-trees 1 2))

(define answer (* (count-trees 1 1)
                  (count-trees 3 1)
                  (count-trees 5 1)
                  (count-trees 7 1)
                  (count-trees 1 2)))

(printf "answer: ~a (expect 5,522,401,584)~n"
        (format-number answer))
(displayln answer)