#lang racket

(require anaphoric)
(require srfi/26)

; A quick unit test function:
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))
(define (expect a b)
  (define result (eval a ns))
  (printf "[~a] \"~a\" returned ~a expected ~a\n"
          (if (equal? result b)
              "PASS"
              "FAIL")
          a
          result
          b))

(struct move (instruction value) #:transparent)

(define (parse-move line)
  (move (string-ref line 0)
        (string->number (substring line 1))))

(define (read-file file)
  (with-input-from-file file
    (thunk
     (map parse-move
          (sequence->list (in-lines))))))

(struct ship (east south bearing) #:transparent)

#|
Action N means to move north by the given value.
Action S means to move south by the given value.
Action E means to move east by the given value.
Action W means to move west by the given value.
Action L means to turn left the given number of degrees.
Action R means to turn right the given number of degrees.
Action F means to move forward by the given value in the
         direction the ship is currently facing.
|#

(define (add-bearing start value)
  (define new (+ start value))
  (cond [(new . < . 0)
         (+ new 360)]
        [(new . >= . 360)
         (- new 360)]
        [else new]))

(expect '(add-bearing 90 -180) 270)
(expect '(add-bearing 270 180) 90)

(define (do-move the-ship the-move)
  (define (my-sin bearing)
    (exact-round (sin (degrees->radians bearing))))
  (define (my-cos bearing)
    (- (exact-round (cos (degrees->radians bearing)))))
  (case (move-instruction the-move)
    [(#\N)
     (struct-copy ship the-ship (south (- (ship-south the-ship)
                                          (move-value the-move))))]
    [(#\S)
     (struct-copy ship the-ship (south (+ (ship-south the-ship)
                                          (move-value the-move))))]
    [(#\E)
     (struct-copy ship the-ship (east (+ (ship-east the-ship)
                                         (move-value the-move))))]
    [(#\W)
     (struct-copy ship the-ship (east (- (ship-east the-ship)
                                         (move-value the-move))))]
    [(#\L)
     (struct-copy ship
                  the-ship
                  (bearing (add-bearing (ship-bearing the-ship)
                                        (- (move-value the-move)))))]
    [(#\R)
     (struct-copy ship
                  the-ship
                  (bearing (add-bearing (ship-bearing the-ship)
                                        (move-value the-move))))]
    [(#\F)
     (struct-copy ship
                  the-ship
                  (east (+ (ship-east the-ship)
                           (* (move-value the-move)
                              (my-sin (ship-bearing the-ship)))))
                  (south (+ (ship-south the-ship)
                            (* (move-value the-move)
                               (my-cos (ship-bearing the-ship))))))]))

(expect '(do-move (ship 0 0 90)
                  (move #\N 10))
        (ship 0 -10 90))
(expect '(do-move (ship 0 0 90)
                  (move #\S 10))
        (ship 0 10 90))
(expect '(do-move (ship 0 0 90)
                  (move #\E 10))
        (ship 10 0 90))
(expect '(do-move (ship 0 0 90)
                  (move #\W 10))
        (ship -10 0 90))
(expect '(do-move (ship 0 0 90)
                  (move #\L 90))
        (ship 0 0 0))
(expect '(do-move (ship 0 0 90)
                  (move #\R 90))
        (ship 0 0 180))
(expect '(do-move (ship 0 0 90)
                  (move #\F 10))
        (ship 10 0 90))
(expect '(do-move (ship 0 0 180)
                  (move #\F 10))
        (ship 0 10 180))

(define (manhattan-distance x y)
  (+ (abs x)
     (abs y)))

(define (part1 file)
  (define end-ship
    (for/fold ((the-ship (ship 0 0 90)))
              ((the-move (read-file file)))
      (do-move the-ship the-move)))
  (manhattan-distance (ship-east end-ship)
                      (ship-south end-ship)))

(expect '(part1 "test.txt") 25)
(expect '(part1 "input.txt") 1631)

#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Almost all of the actions indicate how to move a waypoint which is relative to
the ship's position:

Action N means to move the waypoint north by the given value.
Action S means to move the waypoint south by the given value.
Action E means to move the waypoint east by the given value.
Action W means to move the waypoint west by the given value.
Action L means to rotate the waypoint around the ship left (counter-clockwise)
                  the given number of degrees.
Action R means to rotate the waypoint around the ship right (clockwise) the
                  given number of degrees.
Action F means to move forward to the waypoint a number of times equal to the
                  given value.

The waypoint starts 10 units east and 1 unit north relative to the ship.
The waypoint is relative to the ship; that is, if the ship moves, the waypoint
moves with it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
|#

(struct waypoint (east south) #:transparent)
(struct ship2 (east south waypoint) #:transparent)

; TODO: Define a point structure that waypoint & ship & ship2 extend?