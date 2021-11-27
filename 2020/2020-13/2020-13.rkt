#lang racket

(require anaphoric)
(require srfi/26)

(define (wl x)
  (writeln x)
  x)

; A quick unit test function:
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))
(define (expect a b)
  (define result (eval a ns))
  (printf "[~a] \"~s\" returned ~a expected ~a\n"
          (if (equal? result b)
              "PASS"
              "FAIL")
          a
          result
          b))

(define (part1 earliest-departure buses)
  (apply *
         (argmin second
                 (for/list ((bus buses))
                   (define wait-time
                     (- bus (remainder earliest-departure bus)))
                   (list bus wait-time)))))

(expect '(part1 939 (list 7 13 59 31 19)) 295)

(define (read-file file)
  (with-input-from-file file
    (thunk
     (define earliest-departure (string->number (read-line)))
     (define buses
       (aand (string-split (read-line)
                           ",")
             (filter-not (cute string=? "x" <>)
                         it)
             (map string->number it)))
     (values earliest-departure buses))))

(define part1-file (compose part1 read-file))

(expect '(part1-file "test.txt") 295)
(expect '(part1-file "input.txt") 174)

#|
7,13,x,x,59,x,31,19
0, 1,2,3, 4,5, 6, 7

0 = (t % 7)
0 = ((t + 1) % 13)
0 = ((t + 4) % 59)
0 = ((t + 6) % 31)
0 = ((t + 7) % 19)

The "reverse" of modulo has many results.
|#

(define (parse-buses str)
  (aand (string-split str ",")
        (map list it (range (length it)))
        (filter-not (λ (lst)
                      (string=? "x" (first lst)))
                    it)
        (map (λ (lst)
               (list (string->number (first lst))
                     (second lst)))
             it)))
  
(define (read-file-part2 file)
  (with-input-from-file file
    (thunk
     (define earliest-departure (string->number (read-line))) ; unused
     (define buses (parse-buses (read-line)))
     buses)))

(define (make-validator buses)
  (λ (t)
    (define (validate-one bus index)
      (= 0 (remainder (+ t index)
                      bus)))
    (andmap (cute apply validate-one <>)
            buses)))

(define (find-it-old buses)
  (define validator (make-validator buses))
  (let loop ((t 1))
    (if (validator t)
        t
        (loop (add1 t)))))

; Can I jump in increments of the max bus number?
; I'll have to compensate for the offsets...
; (remainder (t + index) bus)

(define (find-it buses start-hint)
  (define validator (make-validator buses))
  (define big-bus-info (argmax first buses))
  (define big-bus (first big-bus-info))
  (define big-index (second big-bus-info))
  (define start (if start-hint
                    (* big-bus (quotient start-hint big-bus))
                    big-bus))
  (let loop ((x start))
    (let ((t (- x big-index)))
      (if (validator t)
          t
          (loop (+ x big-bus))))))

(define (part2-file file (start-hint #f))
  (define buses (read-file-part2 file))
  #;(apply * (map first buses)) ; product of bus IDs
  (find-it buses start-hint))

(define (part2 str (start-hint #f))
  (define buses (parse-buses str))
  (find-it buses start-hint))

(expect '(part2-file "test.txt") 1068781)
(expect '(part2 "17,x,13,19") 3417)
(expect '(part2 "67,7,59,61") 754018)
(expect '(part2 "67,x,7,59,61") 779210)
(expect '(part2 "67,7,x,59,61") 1261476)
(expect '(part2 "1789,37,47,1889" 1000000000) 1202161486)

#;(expect '(part2-file "input.txt" 100000000000000) #f)

; The puzzle says the answer for "input.txt" will "surely" be greater than:
; 100000000000000
; Product of all the bus IDs in input.txt:
; 1473355587699697

#;(- 1473355587699697 100000000000000)

; Wolfram Alpha syntax:
; 0 = ((t - index) mod bus) ...

(define (wolfram-alpha file (start-hint #f))
  (define buses (read-file-part2 file))
  (printf "0")
  (for ((bus buses))
    (printf " = ((t + ~a) mod ~a)"
            (second bus)
            (first bus)))
  (printf "\n"))
(wolfram-alpha "input.txt")

; Wolfram Alpha answer:
; 1,473,355,587,699,697 n + 780,601,154,795,940
; 780601154795940