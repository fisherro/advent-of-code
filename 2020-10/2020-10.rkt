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

(define (wl x)
  (writeln x)
  x)

; Given a list,
; e.g. (0 1 2 3 4),
; return a list of adjacent pairs,
; e.g. ((0 1) (1 2) (2 3) (3 4))
; Note that it is a list of 2-element lists, not a list of pairs.
(define (pair-off xs)
  (map list (drop-right xs 1)
       (rest xs)))

(expect '(pair-off (range 5)) '((0 1) (1 2) (2 3) (3 4)))

(define (read-file file)
  (with-input-from-file file
    (thunk
     (sequence->list (sequence-map string->number (in-lines))))))

(define (eq1 x)
  (= x 1))

(define (eq3 x)
  (= x 3))

(define (rev-minus xs)
  (- (second xs)
     (first xs)))

(define (part1 file)
  (aand (read-file file)
        (cons 0 it)
        (sort it <)
        #;(wl it)
        (pair-off it)
        (map rev-minus it)
        #;(wl it)
        (* (count eq1 it)
           (add1 (count eq3 it)))))

(expect '(part1 "test1.txt") (* 7 5))
(expect '(part1 "test2.txt") (* 22 10))
(expect '(part1 "input.txt") 2590)

; Part 2:
; Drop each element from the initial list.
; If the element with the dropped value validates, increment count.
; Then recurse.

; Given a list of joltages, make sure no pair is > 3.
(define (validate xs)
  #;(writeln xs)
  (andmap (cute < <> 4)
          (map rev-minus (pair-off xs))))

(expect '(validate '(0 3 7 10)) #f)
(expect '(validate '(0 3 6 9)) #t)

; TODO: Collect list of combos so we can remove-duplicates
;       Or do we have a mistake that is causing duplicates?
;#t (0 1 4 7 10 11 12 15 16 19 22)
;#t (0 1 4 6 7 10 12 15 16 19 22)
;#t (0 1 4 7 10 12 15 16 19 22)
;#t (0 1 4 7 10 12 15 16 19 22)
(define (count-combos joltages)
  (cond ((validate joltages)
         (printf "#t ~a\n" joltages)
         (add1
          (for/sum ((i (in-range 1 (sub1 (length joltages)))))
            (count-combos (append (take joltages i)
                                  (drop joltages (add1 i)))))))
        (else
         (printf "#f ~a\n" joltages)
         0)))

(define (part2 file)
  (aand (read-file file)
        (sort it <)
        (append it (list (+ 3 (last it))))
        (cons 0 it)
        (count-combos it)))

(expect '(part2 "test1.txt") 8)
