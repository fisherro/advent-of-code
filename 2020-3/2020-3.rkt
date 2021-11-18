#lang racket

(require locale/format)

(define (line-stream)
  (let ((line (read-line)))
    (if (eof-object? line)
        empty-stream
        (stream-cons line (line-stream)))))

(define (count-trees right down)
  (with-input-from-file "input.txt"
    (λ ()
      (third
       (stream-fold
        (λ (state line)
          ;(displayln state)
          ;(displayln line)
          (let ((pos (first state))
                (down-pos (second state))
                (tree-count (third state)))
            (cond ((= pos 0)
                   (list (+ pos right)
                         (remainder (add1 down-pos) down)
                         tree-count))
                  ((not (= down-pos 0))
                   (list pos
                         (remainder (add1 down-pos) down)
                         tree-count))
                  (else
                   (list (+ pos right)
                         (remainder (add1 down-pos) down)
                         (if (equal?
                              #\#
                              (string-ref
                               line
                               (remainder pos (string-length line))))
                             (add1 tree-count)
                             tree-count))))))
        (list 0 0 0)
        (line-stream))))))

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