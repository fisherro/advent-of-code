#lang racket

; TODO: The docs say that array is 25 to 50 times faster in Typed Racket
;       So, I should test that.

(require "../qtest.rkt")
(require threading)
(require math/array)
(require srfi/26)

(define (string->digit-list s)
  (map (compose1 string->number string)
       (string->list s)))

(define (read-octopodes file)
  (with-input-from-file file
    (thunk
     (define list-of-lists
       (~>> (in-lines)
            sequence->list
            (map string->digit-list)))
     (define nrows (length list-of-lists))
     (define ncols (length (first list-of-lists)))
     (list->array (vector ncols nrows)
                  (flatten list-of-lists)))))

(define (print-octopodes octopodes)
  (define lol (array->list* octopodes))
  (for ((lst lol))
    (~>> lst
         (map number->string)
         (apply string-append)
         (printf "~a\n")))
  (newline))

(define (pin-zero n)
  (if (n . < . 0)
      0
      n))

(define (pin max n)
  (if (n . > . max)
      max
      n))

(define (count-nearby-flashers octopodes i)
  (match-define (vector max-col max-row)
    (array-shape octopodes))
  (match-define (vector col row) i)
  (define pin-col (cute pin max-col <>))
  (define pin-row (cute pin max-row <>))
  (define col-start (pin-zero (sub1 col)))
  (define row-start (pin-zero (sub1 row)))
  (define col-end (pin-col (+ col 2)))
  (define row-end (pin-row (+ row 2)))
  (define nearby (array-slice-ref octopodes (list (:: col-start col-end 1)
                                                  (:: row-start row-end 1))))
  (array-count (cute > <> 9)
               nearby))

;First, the energy level of each octopus increases by 1.
(define (substep1 octopodes)
  (array-map add1 octopodes))

;Then, any octopus with an energy level greater than 9 flashes.
;This increases the energy level of all adjacent octopuses by 1,
;including octopuses that are diagonally adjacent.
;If this causes an octopus to have an energy level greater than 9,
;it also flashes. This process continues as long as new octopuses
;keep having their energy level increased beyond 9.
;(An octopus can only flash at most once per step.)
(define (flash octopodes)
  (define shape (array-shape octopodes))
  (for/array: #:shape shape ([i (in-array-indexes shape)])
    (define this (array-ref octopodes i))
    (cond ((this . = . 0)
           0)
          ((this . > . 9)
           0)
          (else
           (+ this (count-nearby-flashers octopodes i))))))

(define (substep2 start-state)
  (let loop ((current-state start-state)
             (total-flashers 0))
    (define flashers (array-count (cute > <> 9)
                                  current-state))
    (if (flashers . > . 0)
        (loop (flash current-state)
              (+ total-flashers flashers))
        (values current-state total-flashers))))

;Finally, any octopus that flashed during this step has its energy
;level set to 0, as it used all of its energy to flash.
; This ended up not being needed...
#;(define (substep3 octopodes)
    (array-map (λ (n)
                 (if (n . > . 9)
                     0
                     n))
               octopodes))

(define (do-step octopodes)
  (~> octopodes
      substep1
      substep2))

#|
(define octopodes0
  (list->array
   #(5 5)
   (flatten
    '((1 1 1 1 1)
      (1 9 9 9 1)
      (1 9 1 9 1)
      (1 9 9 9 1)
      (1 1 1 1 1)))))
(print-octopodes octopodes0)

(define octopodes1 (do-step octopodes0))
(print-octopodes octopodes1)
|#

(define (part1 file steps)
  (define start (read-octopodes file))
  #;(print-octopodes start)
  (for/fold ((current start)
             (total-flashers 0)
             #:result total-flashers)
            ((step (in-inclusive-range 1 steps)))
    (define-values (next flashers)
      (do-step current))
    (printf "Step ~a\n" step)
    #;(print-octopodes next)
    (values next (+ total-flashers flashers))))

; How many flashes hawe there been after 100 steps?
#;(qtest (part1 "test.txt" 100) 1656)
#;(qtest (part1 "input.txt" 100))

(define (part2 file)
  (define start (read-octopodes file))
  (define size (array-size start))
  #;(print-octopodes start)
  (let loop ((step 1)
             (current start))
    (define-values (next flashers)
      (do-step current))
    (printf "Step ~a\n" step)
    (if (= flashers size)
        step
        (loop (add1 step)
              next))))

#;(qtest (part2 "test.txt") 195)
(qtest (part2 "input.txt"))