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

(expect '(part1 "test.txt") 165)

(expect '(part1 "input.txt") 7477696999511)
