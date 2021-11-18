#lang racket

;;; It's time we use/write a file line iterator function

;(define line-reader
;  (generator ()
;             (let loop ()
;               (yield (read-line))
;               (loop))))

(require racket/stream)

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

(displayln (count-trees 1 1))
(displayln (count-trees 3 1))
(displayln (count-trees 5 1))
(displayln (count-trees 7 1))
(displayln (count-trees 1 2))

(displayln (* (count-trees 1 1)
              (count-trees 3 1)
              (count-trees 5 1)
              (count-trees 7 1)
              (count-trees 1 2)))