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

; This counted lots of duplicates.
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

; This worked but took too long even on test2.
(define (collect-combos joltages)
  (cond ((validate joltages)
         (for/fold ((combos (list joltages)))
                   ((i (in-range 1 (sub1 (length joltages)))))
           (remove-duplicates
            (append combos
                    (collect-combos
                     (append (take joltages i)
                             (drop joltages (add1 i))))))))
        (else
         '())))

; TODO We could re-write part2 in terms of this...
(define (part2-direct joltages)
  (aand (sort joltages <)
        (append it (list (+ 3 (last it))))
        (cons 0 it)
        (collect-combos it)
        (remove-duplicates it)
        #;(wl it)
        (length it)))

(define (part2 file)
  (aand (read-file file)
        (sort it <)
        (append it (list (+ 3 (last it))))
        (cons 0 it)
        (collect-combos it)
        (remove-duplicates it)
        (length it)))

(expect '(part2 "test1.txt") 8)
#;(expect '(part2 "test2.txt") 19208)

(define (calc-increments joltages)
  (aand (sort joltages <)
        (pair-off it)
        (map rev-minus it)))

(define (calc-increments-with-ends joltages)
  (aand (sort joltages <)
        (append it (list (+ 3 (last it))))
        (cons 0 it)
        (pair-off it)
        (map rev-minus it)))

; Does the data contain any skips of 2?
(define (any-twos? joltages)
  (aand (calc-increments-with-ends joltages)
        (member 2 it)
        (if it #t #f)))

(define (any-twos-file? file)
  (aand (read-file file)
        (any-twos? it)))

(define has-twos '(2 4 6))
(expect '(any-twos? has-twos) #t)

; They made it easier by not have any gaps of 2.
(expect '(any-twos-file? "test1.txt") #f)
(expect '(any-twos-file? "test2.txt") #f)
(expect '(any-twos-file? "input.txt") #f)

; Exactly what set of increments do we find?
(define (increments joltages)
  (remove-duplicates
   (flatten
    (calc-increments-with-ends joltages))))

(define (increments-for-file file)
  (increments (read-file file)))

(expect '(increments-for-file "test1.txt") '(1 3))
(expect '(increments-for-file "test2.txt") '(1 3))
(expect '(increments-for-file "input.txt") '(1 3))

; Any runs of more than three 1s?
(define (longest-run-of-ones data)
  (let loop ((max 0)
             (data1 data))
    (let*-values (((data2) (dropf data1 (negate (cute = 1 <>))))
                  ((run rest) (splitf-at data2 (cute = 1 <>)))
                  ((len) (length run)))
      (if (null? data2)
          max
          (loop (if (> len max)
                    len
                    max)
                rest)))))

(define (longest-run-of-ones-for-file file)
  (aand (read-file file)
        (calc-increments-with-ends it)
        (longest-run-of-ones it)))

; Longest run of ones we need to handle is 4!
(expect '(longest-run-of-ones-for-file "test1.txt") 3)
(expect '(longest-run-of-ones-for-file "test2.txt") 4)
(expect '(longest-run-of-ones-for-file "input.txt") 4)

(define two-ones '(3 4 5))
(define three-ones '(3 4 5 6))
(define four-ones '(3 4 5 6 7))
(expect '(part2-direct two-ones) 2)
(expect '(part2-direct three-ones) 4)
(expect '(part2-direct four-ones) 7)

; Based on tests with runs of 1s, I infer...
; - A run of 2 ones gives a factor of 2
; - A run of 3 ones gives a factor of 4
; - A run of 4 ones gives a factor of 7

(define (runs-of-ones-to-factor data)
  #;(wl data)
  (let loop ((factor 1)
             (data1 data))
    (let*-values (((data2) (dropf data1 (negate (cute = <> 1))))
                  ((run rest) (splitf-at data2 (cut = <> 1)))
                  ((len) (length run)))
      #;(wl len)
      (if (null? data2)
          factor
          (loop (* factor (case len
                            ((2) 2)
                            ((3) 4)
                            ((4) 7)
                            (else 1)))
                rest)))))

(define (part2-new file)
  (aand (read-file file)
        (calc-increments-with-ends it)
        #;(wl it)
        (runs-of-ones-to-factor it)))

(expect '(part2-new "test1.txt") 8)
(expect '(part2-new "test2.txt") 19208)
(expect '(part2-new "input.txt") 226775649501184)

; Arguably this one took me too long because I didn't make enough
; itermediary functions. At least, not until later.