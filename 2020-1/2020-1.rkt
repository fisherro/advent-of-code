#lang racket

;;; Use equal? for numbers in Racket
;;; eqv? is "lightly" not recommended for numbers
;;; eqv? or eq? might be faster for numbers (but not as portable)
;;; Reports are that immutable hash sets are about twice
;;; the speed of mutable hash sets.
;;; Vector might be faster...and gen:set means we could make a flat-set

;; Read a line
;; If it is EOF, return #f
;; Else convert it to a number
(define (read-number)
  (let ((line (read-line)))
    (if (eof-object? line)
        #f
        (string->number line))))

(define seen (mutable-set))
(with-input-from-file
    "2020-1-input.txt"
  (λ ()
    (let loop ()
      (let* ((n (read-number))
             (m (if n
                    (- 2020 n)
                    #f)))
        (cond ((set-member? seen m)
               (displayln (* n m)))
              ((number? n)
               (set-add! seen n)
               (loop))
              (else
               (displayln "No match found")))))))

;;; TODO: Change to use Typed Racket