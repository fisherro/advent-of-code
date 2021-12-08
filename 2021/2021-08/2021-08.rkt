#lang racket

(require "../qtest.rkt")
(require srfi/26)
(require anaphoric)

(define (wl x)
  (writeln x)
  x)

#|
  0:      1:      2:      3:      4:
 aaaa    ....    aaaa    aaaa    ....
b    c  .    c  .    c  .    c  b    c
b    c  .    c  .    c  .    c  b    c
 ....    ....    dddd    dddd    dddd
e    f  .    f  e    .  .    f  .    f
e    f  .    f  e    .  .    f  .    f
 gggg    ....    gggg    gggg    ....

  5:      6:      7:      8:      9:
 aaaa    aaaa    aaaa    aaaa    aaaa
b    .  b    .  .    c  b    c  b    c
b    .  b    .  .    c  b    c  b    c
 dddd    dddd    ....    dddd    dddd
.    f  e    f  .    f  e    f  .    f
.    f  e    f  .    f  e    f  .    f
 gggg    gggg    ....    gggg    gggg
|#

; be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe

; In the output values, how many times do digits 1, 4, 7, or 8 appear?

; 1 = 2 segments
; 4 = 4 segments
; 7 = 3 segments
; 8 = 7 segments

(define (easy? segments)
  (case (string-length segments)
    ((2 4 3 7) #t)
    (else #f)))

(define (part1 file)
  (with-input-from-file file
    (thunk
     (for/sum ((line (in-lines)))
       (aand (string-split line " | ")
             (string-split (second it)
                           " ")
             #;(wl it)
             (count easy? it)
             #;(wl it))))))

(qtest (part1 "test.txt") 26)
(qtest (part1 "input.txt"))

; For part2, it feels like I should break-out some Prolog...
; ...but I didn't.
; Racket has a Prolog-like subsystem: Racklog

; Which segments are in each digit...
; 0: abc efg 6 (length 6 set: 0 6 9)
; 1:   c  f  2
; 2: a cde g 5 (length 5 set: 2 3 5)
; 3: a cd fg 5
; 4:  bcd f  4
; 5: ab d fg 5
; 6: ab defg 6
; 7: a c  f  3
; 8: abcdefg 7
; 9: abcd fg 6

; The signals in 1 are c & f.
; The signal in 7 that is missing from 1 is a.        <--
; The signals in 4 that are missing from 1 are b & d.
; The signals missing from 1, 4, & 7 are e & g.

; 2, 3, & 5 all contain a, d, g.
; 0, 6, & 9 all contain a, b, f, g.
; 0, 6, & 9 are distinguished by c, d, e.

; Between c & f, f is the one that appears in all of the length 6 patterns. <--
; Between b & d, d appears in all length 5 patterns & b appears in all length 6 patterns. <--

; IDs for a, b, c, d, f need e & g
; g appears in all length 5 & 6 patterns <--

; What's the normal mapping from signals to digit?
(define normal-mapping
  (hash "abcefg"  0
        "cf"      1
        "acdeg"   2
        "acdfg"   3
        "bcdf"    4
        "abdfg"   5
        "abdefg"  6
        "acf"     7
        "abcdefg" 8
        "abcdfg"  9))

; Sort the segment signals in a pattern.
(define (normalize segments)
  (aand (string->list segments)
        (sort it char<?)
        (list->string it)))

(qtest (normalize "gfedcba") "abcdefg")

; Read a file & return a list of entries.
; Each entry is: (list patterns output)
; The patterns & output are each a list of strings.
; Each string is a set of signals.
(define (read-data file)
  (with-input-from-file file
    (thunk
     (for/list ((line (in-lines)))
       (aand (string-split line " | ")
             (map (cute string-split <> " ")
                  it)
             (map (λ (part)
                    (map normalize part))
                  it))))))

; Get the character at index i from a string.
; Return a single-character string.
(define (string-char s i)
  (substring s i (add1 i)))

; Remove a character from a string.
; The char, c, is actually a single-character string.
(define (string-remove s c)
  (string-replace s c ""))

; Remove the characters in cs from s.
(define (string-remove-chars s cs)
  (for/fold ((result s))
            ((c cs))
    (string-remove result (string c))))

(qtest (string-remove-chars "abc" "ac") "b")

; Check if the length of s is n.
(define (string-length=? s n)
  (= n (string-length s)))

; Given two characters (cs),
; Find which one is in all patterns.
; Return it as the first result; the other as the second.
(define (which-in-all patterns cs)
  (if (andmap (λ (pattern)
                (string-contains? pattern (string-char cs 1)))
              patterns)
      (values (string-char cs 1)
              (string-char cs 0))
      (values (string-char cs 0)
              (string-char cs 1))))

; Given the patterns from an entry...
; ...create a mapping from the crossed signals to the proper signals.
(define (find-signal-map patterns)
  (define one (findf (cute string-length=? <> 2)
                     patterns))
  (define seven (findf (cute string-length=? <> 3)
                       patterns))
  (define four (findf (cute string-length=? <> 4)
                      patterns))
  (define a (string-remove-chars seven one))
  (define length-six-patterns (filter (cute string-length=? <> 6)
                                      patterns))
  (define-values (f c)
    (which-in-all length-six-patterns one))
  (define b-or-d (string-remove-chars four one))
  (define-values (b d)
    (which-in-all length-six-patterns b-or-d))
  (define all-but-e-and-g (string-append a b c d f))
  (define e-or-g (string-remove-chars "abcdefg" all-but-e-and-g))
  (define-values (g e)
    (which-in-all length-six-patterns e-or-g))
  (hash a "a" b "b" c "c" d "d" e "e" f "f" g "g"))

(define (translate mapping crossed-signals)
  (aand (string->list crossed-signals)
        (map string it)
        (map (cute hash-ref mapping <>)
             it)
        (apply string-append it)
        (normalize it)))

(define (fix-output patterns output)
  (define mapping (find-signal-map patterns))
  (define uncrossed-output (map (cute translate mapping <>)
                                output))
  (apply string-append (map number->string (map (cute hash-ref normal-mapping <>)
                                                uncrossed-output))))

(define (fix-file file)
  (define records (read-data file))
  (map (λ (record)
         (match-define (list patterns output)
           record)
         (fix-output patterns output))
       records))

(qtest (fix-file "test1.txt") '("5353"))

(define (part2 file)
  (aand (fix-file file)
        #;(wl it)
        (map string->number it)
        #;(wl it)
        (apply + it)
        #;(wl it)))

(qtest (part2 "test.txt") 61229)
(qtest (part2 "input.txt"))