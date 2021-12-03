#lang racket

(require "../qtest.rkt")
(require srfi/26)

(define (wl x)
  (writeln x)
  x)

(define (char->bit c)
  (if (char=? c #\0)
      0
      1))

(define (bit->char b)
  (if (= b 0)
      #\0
      #\1))

(define (read-file file)
  (with-input-from-file file
    (thunk
     (sequence->list (in-lines)))))

(define (most-common-bit data bit-index)
  (define bits (map (cute string-ref <> bit-index)
                    data))
  #;(writeln bits)
  (define ones-count (count (cute char=? <> #\1)
                            bits))
  #;(writeln ones-count)
  (define half-data-count (/ (length data) 2))
  #;(writeln data-count)
  (cond ((= ones-count half-data-count)
         1)
        ((ones-count . > . half-data-count)
         1)
        (else
         0)))

(qtest (most-common-bit (read-file "test.txt") 0) 1)

(define (bitlist->number bitlist)
  (string->number
   (list->string (map bit->char bitlist))
   2))

(define (invert bitlist)
  (map (λ (bit)
         (if (= 1 bit)
             0
             1))
       bitlist))

(define (part1 file)
  (define data (read-file file))
  (define nbits (string-length (first data)))
  (define bitlist (for/list ((index (in-range 0 nbits)))
                    (most-common-bit data index)))
  (define γ (bitlist->number bitlist))
  (define ε (bitlist->number (invert bitlist)))
  (* γ ε))

(qtest (part1 "test.txt") 198)
(qtest (part1 "input.txt"))

(define (find-O data (index 0))
  (cond ((= 1 (length data))
         (string->number (first data)
                         2))
        (else
         (define mcb (most-common-bit data index))
         (find-O (filter (λ (datum)
                           (= mcb (char->bit (string-ref datum index))))
                         data)
                 (add1 index)))))

; Clearly find-O & find-CO2 could be refactored into one.
(define (find-CO2 data (index 0))
  (cond ((= 1 (length data))
         (string->number (first data)
                         2))
        (else
         (define lcb (cond ((= 1 (most-common-bit data index)) 0)
                           (else 1)))
         (find-CO2 (filter (λ (datum)
                             (= lcb (char->bit (string-ref datum index))))
                           data)
                   (add1 index)))))

(qtest (find-O (read-file "test.txt")) 23)
(qtest (find-CO2 (read-file "test.txt")) 10)

(define (part2 file)
  (define data (read-file file))
  (* (find-O data)
     (find-CO2 data)))

(qtest (part2 "test.txt") 230)
(qtest (part2 "input.txt"))