#lang racket

(require "../qtest.rkt")
(require threading)
(require math/matrix)
(require srfi/26)
(require math/array)

(define (wl x)
  (writeln x)
  x)

#;(define (pix->boolean pix)
    (case pix
      ((#\.) #f)
      ((#\#) #t)))

(define (pix->number pix)
  (case pix
    ((#\.) 0)
    ((#\#) 1)))

(define (number->pix n)
  (if (n . > . 0) #\# #\.))

(define (read-data file)
  (with-input-from-file file
    (thunk
     (define iea
       (~> (read-line)
           (string->list _)
           (list->vector _)
           (vector-map pix->number _)))
     (read-line)
     (define input-image
       (~> (sequence->list (in-lines))
           (map string->list _)
           (map (λ (lst)
                  (map pix->number lst))
                _)
           (list*->matrix _)))
     (values iea input-image))))

(define (embiggen-image img bg)
  (define blank-row
    (list (make-list (+ 4 (matrix-num-cols img)) bg)))
  (define affix (make-list 2 bg))
  (~> (matrix->list* img)
      (map (cute append affix <> affix) _)
      (append blank-row blank-row _ blank-row blank-row)
      (list*->matrix _)))

(define (bitlist->number bl)
  (~> (map number->string bl)
      (apply string-append _)
      (string->number _ 2)))

; TODO: Make iea a string or vector?
(define (enhance iea input-image bg)
  (define new-bg (case bg
                   ((0) (vector-ref iea 0))
                   ((1) (vector-ref iea 511))))
  (define in-img (embiggen-image input-image bg))
  #;(writeln in-img)
  (define max-row (- (matrix-num-rows in-img) 2))
  (define max-col (- (matrix-num-cols in-img) 2))
  #;(printf "num (~a ~a)\n" (matrix-num-rows in-img)
            (matrix-num-cols in-img))
  #;(printf "max (~a ~a)\n" max-row max-col)
  (values new-bg
          (for*/matrix max-row max-col ((row (in-inclusive-range 1 max-row))
                                        (col (in-inclusive-range 1 max-col)))
            #;(printf "(~a ~a)\n" row col)
            (~> (submatrix in-img
                           (:: (sub1 row) (+ 2 row))
                           (:: (sub1 col) (+ 2 col)))
                (matrix->list _)
                (bitlist->number _)
                (vector-ref iea _)))))

#|
(~> (list*->matrix '((1 2 3 4 5)
                     (6 7 8 9 10)
                     (11 12 13 14 15)
                     (16 17 18 19 20)))
    (enhance #f _))
|#

(define (print-image image)
  (~> (matrix->list* image)
      (map (cute map number->pix <>) _)
      (map list->string _)
      (for-each displayln _))
  (newline)
  image)

#;(define (part1 file)
    (define-values (iea input-image)
      (read-data file))
    #;(print-image input-image)
    (define enhanced1 (enhance iea input-image #t))
    #;(print-image enhanced1)
    (define enhanced2 (enhance iea enhanced1 #f))
    #;(print-image enhanced2)
    (~> (matrix->list enhanced2)
        (filter (cute > <> 0) _)
        (length _)))

(define (part2 file iterations)
  (define bg 0)
  (define-values (iea input-image)
    (read-data file))
  (define-values (output-bg output-image)
    (for/fold ((bg 0)
               (image input-image))
              ((i (in-range 0 iterations)))
      (enhance iea image bg)))
  (~> (matrix->list output-image)
      (filter (cute > <> 0) _)
      (length _)))

#;(qtest (part1 "test.txt") 35)
#;(qtest (part1 "input.txt") 5573)
; 5479 is too low
; I wasn't taking into account if first bit of iea was 1.
; Correct answer: 5573

(qtest (part2 "test.txt" 2) 35)
(qtest (part2 "input.txt" 2) 5573)
(qtest (part2 "test.txt" 50) 3351)
(qtest (part2 "input.txt" 50))

#;(string->number "111111111" 2)