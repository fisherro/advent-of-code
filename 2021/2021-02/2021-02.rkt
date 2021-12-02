#lang racket

(require "../qtest.rkt")
(require anaphoric)
(require threading)

(define (parse line)
  (define matches (regexp-match #rx"^([a-z]+) ([0-9]+)$" line))
  (values (second matches)
          (string->number (third matches))))

(define (part1 file)
  (with-input-from-file file
    (thunk
     (define-values
       (final-h final-d)
       (for/fold ((h 0)
                  (d 0))
                 ((line (in-lines)))
         (define-values
           (direction distance)
           (parse line))
         (case direction
           (("forward")
            (values (+ h distance) d))
           (("down")
            (values h (+ d distance)))
           (("up")
            (values h (- d distance))))))
     (* final-h final-d))))
     

(qtest (part1 "test.txt") 150)
(qtest (part1 "input.txt"))

(define (part2 file)
  (with-input-from-file file
    (thunk
     (define-values
       (final-h final-d final-aim)
       (for/fold ((h 0)
                  (d 0)
                  (aim 0))
                 ((line (in-lines)))
         (define-values
           (direction distance)
           (parse line))
         (case direction
           (("forward")
            (values (+ h distance) (+ d (* aim distance)) aim))
           (("down")
            (values h d (+ aim distance)))
           (("up")
            (values h d (- aim distance))))))
     (* final-h final-d))))

(qtest (part2 "test.txt") 900)
(qtest (part2 "input.txt"))