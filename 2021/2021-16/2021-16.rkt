#lang racket

; 1st 3 bits: version
; 2nd 3 bits: packet type ID

; packet type 4: literal
; pad with leading zeros to multiple of 4 bits
; broken into 4 bit groups
; each group except the last is prefixed by 1; last, 0

; any other packet type is an operator
; length type ID:
;  0: Next 15 bits are total length in bits of the subpackets
;  1: Next 11 bits are number of subpackets contained
; then subpackets

(require "../qtest.rkt")
(require threading)
(require (only-in srfi/13
                  string-pad
                  string-take
                  string-drop))

(define (hex-digit->binary c)
  (~> (string c)
      (string->number _ 16)
      (number->string _ 2)
      (string-pad _ 4 #\0)))

(define (hex->binary s)
  (~> (string->list s)
      (map hex-digit->binary _)
      (apply string-append _)))

(qtest (hex->binary "D2FE28") "110100101111111000101000")

(define (binary->number s)
  (string->number s 2))

#;(for ((c "0123456789abcdef"))
    (printf "~a -> ~a -> ~a\n"
            c
            (hex-digit->binary c)
            (binary->number (hex-digit->binary c))))

(struct packet (version type) #:transparent)
(struct literal-packet packet (value) #:transparent)
(struct operator-packet packet (subpackets) #:transparent)

(define (parse-literal pkt bitstream)
  (let loop ((bs bitstream)
             (digits ""))
    (match-define (list _ prefix new-digits remaining)
      (regexp-match #px"^(.)(....)(.*)$" bs))
    (define all-digits (string-append digits new-digits))
    (cond ((string=? prefix "1")
           (loop remaining all-digits))
          (else
           (values (literal-packet (packet-version pkt)
                                   (packet-type pkt)
                                   (binary->number all-digits))
                   remaining)))))

(define (parse-op-total pkt bs)
  (define length (binary->number (substring bs 0 15)))
  (define end-of-subpkts (+ 15 length))
  (define subpkts (substring bs 15 end-of-subpkts))
  (define remaining (substring bs end-of-subpkts))
  (define subpkt-list (parse-packets subpkts))
  (values (operator-packet (packet-version pkt)
                           (packet-type pkt)
                           subpkt-list)
          remaining))

(define (parse-op-sub pkt bs)
  (define subpkt-count (binary->number (substring bs 0 11)))
  (define-values (subpkts remaining)
    (for/fold ((subpkts '())
               (remaining (substring bs 11)))
              ((i (in-range 0 subpkt-count)))
      (define-values (pkt more)
        (parse-packet remaining))
      (values (append subpkts (list pkt))
              more)))
  (values (operator-packet (packet-version pkt)
                           (packet-type pkt)
                           subpkts)
          remaining))

(define (parse-operator pkt bitstream)
  (define length-type (string-take bitstream 1))
  (define bs (string-drop bitstream 1))
  (cond ((string=? length-type "0")
         (parse-op-total pkt bs))
        (else
         (parse-op-sub pkt bs))))

(define (parse-packet bs)
  (define pkt
    (packet (binary->number (substring bs 0 3))
            (binary->number (substring bs 3 6))))
  (define remaining (substring bs 6))
  (case (packet-type pkt)
    ((4)
     (parse-literal pkt remaining))
    (else
     (parse-operator pkt remaining))))

(define (parse-one-packet bs)
  (define-values (pkt remaining)
    (parse-packet bs))
  pkt)

(define (parse-packets bitstream)
  (let loop ((pkts '())
             (bs bitstream))
    (cond (((string-length bs) . < . 8)
           pkts)
          (else
           (define-values (pkt remaining)
             (parse-packet bs))
           (loop (append pkts (list pkt))
                 remaining)))))

(qtest (parse-one-packet (hex->binary "D2FE28"))
       (literal-packet 6 4 2021))

(qtest (parse-one-packet (hex->binary "38006F45291200"))
       (operator-packet 1 6 (list (literal-packet 6 4 10)
                                  (literal-packet 2 4 20))))

(qtest (parse-one-packet (hex->binary "EE00D40C823060"))
       (operator-packet 7 3 (list (literal-packet 2 4 1)
                                  (literal-packet 4 4 2)
                                  (literal-packet 1 4 3))))

(define (sum-packet-versions pkt)
  (cond ((operator-packet? pkt)
         (~> (map sum-packet-versions
                  (operator-packet-subpackets pkt))
             (apply + _)
             (+ _ (packet-version pkt))))
        (else
         (packet-version pkt))))

(define (boolean->number b)
  (if b 1 0))

#|
0: sum
1: product
2: min
3: max
5: >
6: <
7: =
|#
(define (eval-packet pkt)
  (case (packet-type pkt)
    ((0)
     (apply + (map eval-packet (operator-packet-subpackets pkt))))
    ((1)
     (apply * (map eval-packet (operator-packet-subpackets pkt))))
    ((2)
     (apply min (map eval-packet (operator-packet-subpackets pkt))))
    ((3)
     (apply max (map eval-packet (operator-packet-subpackets pkt))))
    ((4)
     (literal-packet-value pkt))
    ((5)
     (boolean->number
      (apply > (map eval-packet (operator-packet-subpackets pkt)))))
    ((6)
     (boolean->number
      (apply < (map eval-packet (operator-packet-subpackets pkt)))))
    ((7)
     (boolean->number
      (apply = (map eval-packet (operator-packet-subpackets pkt)))))))

; Parse the packets & add up the version numbers
(define (part1 data)
  (~> (hex->binary data)
      (parse-packets _)
      (map sum-packet-versions _)
      (apply + _)))

(define (part1-file file)
  (with-input-from-file file
    (thunk
     (part1 (read-line)))))

(define (part2 data)
  (~> (hex->binary data)
      (parse-packets _)
      (first _)
      (eval-packet _)))

(define (part2-file file)
  (with-input-from-file file
    (thunk
     (part2 (read-line)))))

(qtest (part1 "D2FE28") 6)
(qtest (part1 "8A004A801A8002F478") 16)
(qtest (part1 "620080001611562C8802118E34") 12)
(qtest (part1 "C0015000016115A2E0802F182340") 23)
(qtest (part1 "A0016C880162017C3686B18A3D4780") 31)
(qtest (part1-file "input.txt")) ; 1038

(with-input-from-file "input.txt"
  (thunk (length (parse-packets (hex->binary (read-line))))))

(qtest (part2 "C200B40A82") 3)
(qtest (part2 "04005AC33890") 54)
(qtest (part2 "880086C3E88112") 7)
(qtest (part2 "CE00C43D881120") 9)
(qtest (part2 "D8005AC2A8F0") 1)
(qtest (part2 "F600BC2D8F") 0)
(qtest (part2 "9C005AC2F8F0") 0)
(qtest (part2 "9C0141080250320F1802104A08") 1)
(qtest (part2-file "input.txt"))