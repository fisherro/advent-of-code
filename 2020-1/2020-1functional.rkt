#lang racket

;;; The previous version of this code used a named let loop.
;;; This one uses for/fold instead, & I'm unsure if it is better.
;;;
;;; Another change is using sequence-map & in-lines, & that is better.

(with-input-from-file "2020-1-input.txt"
  (thunk
   (for/fold ((result #f)
              (seen (set))
              #:result result)
             ((n (sequence-map string->number (in-lines))))
     #:break result
     (let ((m (- 2020 n)))
       (values (if (set-member? seen m)
                   (* n m)
                   #f)
               (set-add seen n))))))

;;; TODO: Change to use Typed Racket