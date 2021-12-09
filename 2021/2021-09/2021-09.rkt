#lang racket

(require "../qtest.rkt")
(require anaphoric)
(require srfi/26)

(define (wl x)
  (writeln x)
  x)

; This might be a good place to use the threading macros?
(define (read-data file)
  (with-input-from-file file
    (thunk
     (aand (sequence->list (in-lines))
           (map (λ (line)
                  (aand (string->list line)
                        (map string it)
                        (map string->number it)))
                it)))))
#|
; I think the use of cute & compose here obscures things more than the original above.
(define (read-data file)
  (with-input-from-file file
    (thunk
     (aand (sequence->list (in-lines))
           (map (compose (cute map (compose string->number string) <>)
                         string->list)
                it)))))
|#

(define (hmap-get hmap x y)
  (cond ((y . < . 0)
         #f)
        ((y . >= . (length hmap))
         #f)
        (else
         (define row (list-ref hmap y))
         (cond ((x . < . 0)
                #f)
               ((x . >= . (length row))
                #f)
               (else
                (list-ref row x))))))

(define (get-adjacent hmap x y)
  (filter number?
          (list (hmap-get hmap (sub1 x) y)
                (hmap-get hmap (add1 x) y)
                (hmap-get hmap x (sub1 y))
                (hmap-get hmap x (add1 y)))))

(define (lowpoint? hmap x y)
  (define h (hmap-get hmap x y))
  (define adjacents (get-adjacent hmap x y))
  (if (andmap (λ (adjacent)
                (h . < . adjacent))
              adjacents)
      h
      #f))

; Pretty inefficient.
; Using vectors or arrays could help.
; Or walking the lists instead of indexing into them.
; But...the inefficiency doesn't really matter.
(define (get-lowpoints hmap)
  (aand (for*/list ((x (in-range 0 (length (first hmap))))
                    (y (in-range 0 (length hmap))))
          (lowpoint? hmap x y))
        (filter number? it)))

(define (part1 file)
  (aand (read-data file)
        #;(wl it)
        (get-lowpoints it)
        #;(wl it)
        (map add1 it)
        #;(wl it)
        (apply + it)
        #;(wl it)))

(qtest (part1 "test.txt") 15)
(qtest (part1 "input.txt"))

(struct loc (x y h) #:transparent)

(define (get-lowpoint-locs hmap)
  (aand (for*/list ((x (in-range 0 (length (first hmap))))
                    (y (in-range 0 (length hmap))))
          (if (lowpoint? hmap x y)
              (loc x y (hmap-get hmap x y))
              #f))
        (filter loc? it)))

(define (add-adjacent-locs basin-set hmap x y)
  (define (check-loc bset xx yy)
    (define hh (hmap-get hmap xx yy))
    (define the-loc (loc xx yy hh))
    (if (and hh (hh . < . 9) (not (set-member? bset the-loc)))
        (add-adjacent-locs (set-add bset the-loc)
                           hmap
                           xx
                           yy)
        bset))
  (aand (check-loc basin-set (sub1 x) y)
        (check-loc it (add1 x) y)
        (check-loc it x (sub1 y))
        (check-loc it x (add1 y))))

; Given a lowpoint, find its basin
(define (get-basin-size hmap the-loc)
  (define the-basin (add-adjacent-locs (set) hmap (loc-x the-loc) (loc-y the-loc)))
  (set-count the-basin))

(define (part2 file)
  (define hmap (read-data file))
  (define lowpoints (get-lowpoint-locs hmap))
  (define basin-sizes (map (cute get-basin-size hmap <>)
                           lowpoints))
  (aand (sort basin-sizes >)
        (take it 3)
        (apply * it)))

(qtest (part2 "test.txt") 1134)
(qtest (part2 "input.txt"))