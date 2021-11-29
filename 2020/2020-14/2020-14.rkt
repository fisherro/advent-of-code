#lang racket

; TODO: There's a package with a bit-vector datatype...

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

#|
mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0
|#

;#b0010
;#b1101

; #b0010 xor #b1111 -> #b1101
; #b0010 xor #b0000 -> #b0010

#|
(map (compose1 (cute format "#b~b" <>)
               (cute <> #b0010 #b1111))
     (list bitwise-ior bitwise-and bitwise-xor))

(map (compose1 (cute format "#b~b" <>)
               (cute <> #b0010 #b0000))
     (list bitwise-ior bitwise-and bitwise-xor))

(map (compose1 (cute format "#b~b" <>)
               (cute <> #b1101 #b1111))
     (list bitwise-ior bitwise-and bitwise-xor))

(map (compose1 (cute format "#b~b" <>)
               (cute <> #b1101 #b0000))
     (list bitwise-ior bitwise-and bitwise-xor))

(make-vector 36 0)
|#

(define (parse-mask s)
  (for/vector ((c s))
    (case c
      ((#\1) 1)
      ((#\0) 0)
      (else #f))))
#;(parse-mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X")

(define (parse-number s)
  (aand (string->number s)
        (number->string it 2)
        (string-append (make-string (- 36 (string-length it))
                                    #\0)
                       it)
        (parse-mask it)))
(expect '(parse-number "11")
        '#(0 0 0 0 0 0 0 0
             0 0 0 0 0 0 0 0
             0 0 0 0 0 0 0 0
             0 0 0 0 0 0 0 0
             1 0 1 1))

(define (bitvector->number bv)
  (for/fold ((n 0))
            ((index (in-range 0 36)))
    #;(printf "n: ~s; index: ~s\n" n index)
    (+ n (* (vector-ref bv (- 35 index))
            (expt 2 index)))))
(expect '(bitvector->number (parse-number "11")) 11)

(define (mask-it value mask)
  (define (helper v m)
    (case m
      ((1) 1)
      ((0) 0)
      (else v)))
  (vector-map helper value mask))
#;(mask-it (parse-number "11")
           (parse-mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"))

(define (mask-it2 value mask)
  (define (update-results results index bit)
    #;(printf "update-results ~s ~s ~s\n" results index bit)
    (cond ((number? bit)
           (map (cute cons bit <>)
                results))
          (else
           (append
            (map (cute cons 1 <>)
                 results)
            (map (cute cons 0 <>)
                 results)))))
  (define value-bitvector (parse-number value))
  (aand (for/fold ((results (list '())))
                  ((index (in-range 0 36)))
          (define value-bit (vector-ref value-bitvector index))
          (define mask-bit (vector-ref mask index))
          (case mask-bit
            ((0)
             (update-results results index value-bit))
            ((1)
             (update-results results index 1))
            (else
             (update-results results index #f))))
        (map reverse it)
        (map list->vector it)
        (map bitvector->number it)))

(define mask-it2-test
  (sort
   (mask-it2
    "42"
    (parse-mask "000000000000000000000000000000X1001X"))
   <))

(expect 'mask-it2-test '(26 27 58 59))

(define (part1 file)
  (with-input-from-file file
    (thunk
     (define mem (make-hash))
     (define mask (make-vector 36 #f))
     (for ((line (in-lines)))
       #;(printf "Line: ~s\n" line)
       (acond ((string-prefix? line "mask = ")
               (set! mask (parse-mask (substring line 7)))
               #;(printf "Mask: ~s\n" mask))
              ((regexp-match #rx"mem\\[([0-9]+)\\] = ([0-9]+)" line)
               (define addr (string->number (second it)))
               (define arg (mask-it (parse-number (third it))
                                    mask))
               (hash-set! mem addr arg))
              (else
               (printf "Error!\n"))))
     #;(writeln mem)
     (sequence-fold + 0 (sequence-map
                         bitvector->number
                         (in-hash-values mem))))))

(define (part2 file)
  (with-input-from-file file
    (thunk
     (define mem (make-hash))
     (define mask (make-vector 36 #f))
     (for ((line (in-lines)))
       #;(printf "Line: ~s\n" line)
       (acond ((string-prefix? line "mask = ")
               (set! mask (parse-mask (substring line 7)))
               #;(printf "Mask: ~s\n" mask))
              ((regexp-match #rx"mem\\[([0-9]+)\\] = ([0-9]+)" line)
               (define arg (parse-number (third it)))
               (define addrs (mask-it2 (second it)
                                       mask))
               (for-each (cute hash-set! mem <> arg)
                         addrs))
              (else
               (printf "Error!\n"))))
     #;(writeln mem)
     (sequence-fold + 0 (sequence-map
                         bitvector->number
                         (in-hash-values mem))))))

(expect '(part1 "test.txt") 165)

(expect '(part1 "input.txt") 7477696999511)

(expect '(part2 "test2.txt") 208)

(expect '(part2 "input.txt") 3687727854171)