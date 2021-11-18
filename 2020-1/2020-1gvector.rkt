#lang racket

;;; Use equal? for numbers in Racket
;;; eqv? is "lightly" not recommended for numbers
;;; eqv? or eq? might be faster for numbers (but not as portable)
;;; Reports are that immutable hash sets are about twice
;;; the speed of mutable hash sets.
;;; Vector might be faster...and gen:set means we could make a flat-set

(require data/gvector)

;; Read a line
;; If it is EOF, return #f
;; Else convert it to a number
(define (read-number)
  (let ((line (read-line)))
    (if (eof-object? line)
        #f
        (string->number line))))

(define (member? gv v)
  (let loop ((first 0)
             (last (gvector-count gv)))
    (if (= first last)
        #f
        (begin
          (let* ((half (quotient (- last first) 2))
                 (x (gvector-ref gv half)))
            (cond ((= x v)
                   half)
                  ((> x v)
                   (loop first half))
                  (else
                   (loop half last))))))))                  

(define seen (make-gvector #:capacity 100))
(with-input-from-file
    "2020-1-input.txt"
  (λ ()
    (let loop ()
      (let* ((n (read-number))
             (m (if n
                    (- 2020 n)
                    #f))
             (pos (if n
                      (member? seen m)
                      #f)))
        (cond ((member? seen m)
               (displayln (* n m)))
              ((number? n)
               (set-add! seen n)
               (loop))
              (else
               (displayln "No match found")))))))
