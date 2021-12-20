#lang racket

(require "../qtest.rkt")
(require anaphoric)
(require threading)
(require srfi/26)

(define (explode-left left before)
  (define left-matches
    (regexp-match
     #px"^(.*?)(\\d+)(\\D*)$"
     before))
  (cond (left-matches
         (match-define (list _ lbefore lnum lafter)
           left-matches)
         (define new-lnum
           (number->string (+ (string->number lnum)
                              (string->number left))))
         (string-append lbefore new-lnum lafter))
        (else before)))

(define (explode-right right after)
  (define right-matches
    (regexp-match
     #px"^(\\D*)(\\d+)(.*)$"
     after))
  (cond (right-matches
         (match-define (list _ rbefore rnum rafter)
           right-matches)
         (define new-rnum
           (number->string (+ (string->number rnum)
                              (string->number right))))
         (string-append rbefore new-rnum rafter))
        (else after)))

#;(define explode-regexp
    #px"^(\\[.*?\\[.*?\\[.*?\\[.*?)\\[(\\d+),(\\d+)\\](.*)$")

; TODO: regexp won't work alone?
;       Need to match [ and ]?

(define (explosive? sn)
  (define-values (_ matched)
    (for/fold ((open-count 0)
               (matched #f))
              ((i (in-range (string-length sn))))
      #:break matched
      #;(when matched (printf "**** MATCHED ****\n"))
      (define c (string-ref sn i))
      #;(printf "open-count: ~a; matched: ~s; c: ~a\n"
                open-count matched c)
      (acond ((and (open-count . >= . 4)
                   (regexp-match
                    #px"^\\[(\\d+),(\\d+)\\](.*)$"
                    sn
                    i))
              (match-define (list _ left right after) it)
              (define before (substring sn 0 i))
              (values open-count (list before
                                       left
                                       right
                                       after)))
             ((char=? c #\[)
              (values (add1 open-count)
                      matched))
             ((char=? c #\])
              (values (sub1 open-count)
                      matched))
             (else (values open-count matched)))))
  matched)

#;(define (explosive? sn)
    (regexp-match? explode-regexp sn))

#;(qtest (explosive? "[[[[0,7],4],[15,[0,13]]],[1,1]]"))
(qtest (explosive? "[[[[0,7],4],[15,[0,13]]],[1,1]]") #f)
(qtest (explosive? "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"))

(define (explode before left right after)
  (define new-left (explode-left left before))
  (define new-right (explode-right right after))
  (string-append new-left "0" new-right))

(define (explode-test sn)
  (apply explode (explosive? sn)))

(qtest (explode-test "[[[[[9,8],1],2],3],4]")
       "[[[[0,9],2],3],4]")
(qtest (explode-test "[7,[6,[5,[4,[3,2]]]]]")
       "[7,[6,[5,[7,0]]]]")
(qtest (explode-test "[[6,[5,[4,[3,2]]]],1]")
       "[[6,[5,[7,0]]],3]")
(qtest (explode-test "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]")
       "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
(qtest (explode-test "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
       "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")

(define split-regexp
  #px"\\d{2,}")

(define (splittable? sn)
  (regexp-match? split-regexp sn))

(define (split sn)
  (define pos (first (regexp-match-positions split-regexp sn)))
  (define before (substring sn 0 (car pos)))
  (define num-string (substring sn (car pos) (cdr pos)))
  (define after (substring sn (cdr pos)))
  (define half (/ (string->number num-string) 2))
  (define left (number->string (floor half)))
  (define right (number->string (ceiling half)))
  (string-append before "[" left "," right "]" after))

(qtest (split "10") "[5,5]")
(qtest (split "11") "[5,6]")
(qtest (split "12") "[6,6]")

(define (reduce-sn sn)
  #;(displayln sn)
  (acond ((explosive? sn)
          (reduce-sn (apply explode it)))
         ((splittable? sn)
          (reduce-sn (split sn)))
         (else sn)))

(qtest (reduce-sn "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")
       "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")

(define (sn+ a b)
  (~> (string-append "[" a "," b "]")
      (reduce-sn _)))

(qtest (sn+ "[[[[4,3],4],4],[7,[[8,4],9]]]" "[1,1]")
       "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")

(define (sn-sum sns)
  (for/fold ((sum (first sns)))
            ((next (rest sns)))
    (define total (sn+ sum next))
    #;(printf "  ~a\n+ ~a\n= ~a\n" sum next total)
    total))

(qtest (sn-sum '("[1,1]" "[2,2]" "[3,3]" "[4,4]"))
       "[[[[1,1],[2,2]],[3,3]],[4,4]]")
(qtest (sn-sum '("[1,1]" "[2,2]" "[3,3]" "[4,4]" "[5,5]"))
       "[[[[3,0],[5,3]],[4,4]],[5,5]]")
(qtest (sn-sum '("[1,1]" "[2,2]" "[3,3]" "[4,4]" "[5,5]" "[6,6]"))
       "[[[[5,0],[7,4]],[5,5]],[6,6]]")

(define (sum-file file)
  (with-input-from-file file
    (thunk
     (sn-sum (sequence->list (in-lines))))))

(qtest (sum-file "test1.txt")
       "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")

(qtest
 (sn+
  "[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]"
  "[2,9]")
 "[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]")

(define (sn-magnitude sn)
  (define parsed
    (with-input-from-string (string-replace sn "," " ")
      (thunk (read))))
  (define (mag n)
    (cond ((number? n)
           n)
          (else
           (+ (* 3 (mag (first n)))
              (* 2 (mag (second n)))))))
  (mag parsed))

(qtest (sn-magnitude "[9,1]") 29)
(qtest (sn-magnitude "[1,9]") 21)
(qtest (sn-magnitude "[[9,1],[1,9]]") 129)
(qtest (sn-magnitude "[[1,2],[[3,4],5]]") 143)
(qtest (sn-magnitude "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]") 1384)
(qtest (sn-magnitude "[[[[1,1],[2,2]],[3,3]],[4,4]]") 445)
(qtest (sn-magnitude "[[[[3,0],[5,3]],[4,4]],[5,5]]") 791)
(qtest (sn-magnitude "[[[[5,0],[7,4]],[5,5]],[6,6]]") 1137)
(qtest (sn-magnitude "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]") 3488)

(qtest (sum-file "test2.txt")
       "[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]")

(define (sum-mag-file file)
  (sn-magnitude (sum-file file)))

(qtest (sum-mag-file "test2.txt") 4140)
(qtest (sum-mag-file "input.txt"))

(define (largest-magsum file)
  (define sns
    (with-input-from-file file
      (thunk
       (sequence->list (in-lines)))))
  (~> (cartesian-product sns sns)
      (filter-not (cute apply string=? <>) _)
      (map (cute apply sn+ <>) _)
      (map sn-magnitude _)
      (apply max _)))

(qtest (largest-magsum "test2.txt") 3993)
(qtest (largest-magsum "input.txt"))