#lang racket

(require threading)
(require algorithms)
(require srfi/26)
(require test-engine/racket-tests)

; A start on a 5x5 font...but then I found the 8x8.
#|
(define A (list
           ".###."
           "#...#"
           "#####"
           "#...#"
           "#...#"))

(define B (list
           "####."
           "#...#"
           "####."
           "#...#"
           "####."))

(define C (list
           ".###."
           "#   #"
           "#    "
           "#   #"
           " ### "))

(define D (list
           "#### "
           "#   #"
           "#   #"
           "#   #"
           "#### "))
|#

; Parse a 8-bit hex number from the 8x8 font file:
; (Note that the bits of the font are in reverse order.)
; Returns a string of 8 zeros & ones, LSB to MSB.
; e.g. 0x12 → "01001000"
(define (parse-char c)
  (~> (substring c 2 4)
      (string->number _ 16)
      (~r _ #:base 2 #:min-width 8 #:pad-string "0")
      (string->list _)
      (reverse _)
      (list->string _)))
(check-expect (parse-char "0x12") "01001000")

; Parse a line from the 8x8 font file:
; Returns a list of 8 strings from parse-char.
; Returns #f if the line lacks font data.
(define (parse-line line)
  (and~> (regexp-match #px"\\{ (.+)\\}" line)
         (second _)
         (regexp-match* #px"0x[0-9A-Fa-f]{2}" _)
         (map parse-char _)))
(check-expect
 (parse-line
  "{ 0x3F, 0x66, 0x66, 0x3E, 0x36, 0x66, 0x67, 0x00},   // U+0052 (R)")
 '("11111100"
   "01100110"
   "01100110"
   "01111100"
   "01101100"
   "01100110"
   "11100110"
   "00000000"))

; Parse the 8x8 font file:
; Returns a list of lists produced by parse-line.
; Use ASCII values as an index into the list.
; TODO: Make a vector instead of a list?
(define the-font
  (with-input-from-file "font8x8_basic.h"
    (thunk
     (~> (sequence->list (in-lines))
         (map parse-line _)
         (filter list? _)))))

; Takes the list of strings of 0s & 1s.
; Returns a list of points.
; Points are a list of two numbers.
(define (fontchar->points fontchar)
  (for/fold ((points '()))
            (((row y) (in-indexed fontchar)))
    (for/fold ((points points))
              (((col x) (in-indexed row)))
      (if (char=? #\1 col)
          (append points (list (list x y)))
          points))))

; Takes a list of points.
; Unfolds half of them along y.
; Returns the points & a fold spec string.
(define (unfold-points-y h points)
  (define (unfold-point pt)
    (list-update pt 1 (cute - (* h 2) <>)))
  (values (map unfold-point points)
          (format "fold along y=~a" h)))
(check-expect
 (let-values (((pts f) (unfold-points-y
                        8
                        '((0 0) (1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7)))))
   pts)
 '((0 16) (1 15) (2 14) (3 13) (4 12) (5 11) (6 10) (7 9)))

(struct charinfo (points width height) #:transparent)

; Looks up a char (c) in font.
; Converts it to points.
; Finds the character width & height.
; Returns the info in a charinfo struct.
(define (get-charinfo font c)
  (define fontchar (list-ref font (char->integer c)))
  (charinfo (fontchar->points fontchar)
            (string-length (first fontchar))
            (length fontchar)))
(check-expect
 (get-charinfo the-font #\R)
 (charinfo
  '((0 0)
    (1 0)
    (2 0)
    (3 0)
    (4 0)
    (5 0)
    (1 1)
    (2 1)
    (5 1)
    (6 1)
    (1 2)
    (2 2)
    (5 2)
    (6 2)
    (1 3)
    (2 3)
    (3 3)
    (4 3)
    (5 3)
    (1 4)
    (2 4)
    (4 4)
    (5 4)
    (1 5)
    (2 5)
    (5 5)
    (6 5)
    (0 6)
    (1 6)
    (2 6)
    (5 6)
    (6 6))
  8
  8))

; Shifts a set of points along the x-axis.
; This is used for placing characters.
(define (shift-points points x)
  (map (λ (pt)
         (list-update pt 0 (cute + x <>)))
       points))
(check-expect
 (shift-points
  '((0 0) (0 2) (1 1) (2 0) (2 2))
  10)
 '((10 0) (10 2) (11 1) (12 0) (12 2)))
 

; Given a font and some text...
; Create a list of points representing that text.
; Also returns the width & height.
(define (render-string font text)
  (for/fold ((points '())
             (current-width 0)
             (max-height 0))
            ((c text))
    (define ci (get-charinfo font c))
    (values (append (shift-points (charinfo-points ci)
                                  current-width)
                    points)
            (+ current-width (charinfo-width ci))
            (max max-height (charinfo-height ci)))))
(check-expect
 (let-values (((pts w h) (render-string the-font "Ro")))
   (list w h))
 '(16 8))

; Given a list of points...
; Print them one to a line.
(define (print-points points)
  (for-each (λ (pt)
              (match-define (list x y)
                pt)
              (printf "~a,~a\n" x y))
            points))

(define (render-and-unfold font text)
  (define-values (points w h)
    (render-string font text))
  (define chunks (chunks-of points 2))
  (define to-not-unfold (map first chunks))
  (define to-unfold
    (map second (if (odd? (length points))
                    (init chunks)
                    chunks)))
  (define-values (unfolded fold)
    (unfold-points-y h to-unfold))
  (define appended
    (append to-not-unfold unfolded))
  (print-points appended)
  (newline)
  (displayln fold))

; TODO: Handle doing multiple unfolds
(define args (current-command-line-arguments))
(define the-text
  (if ((vector-length args) . < . 1)
      "Test"
      (vector-ref args 0)))

(render-and-unfold the-font the-text)
#|
(current-command-line-arguments)
(define env (current-environment-variables))
(define env-keys (sort (environment-variables-names env)
                       bytes<?))
(for-each (λ (k)
            (define v (environment-variables-ref env k))
            (printf "~s=~s\n" k v))
          env-keys)

(if (getenv "TERM")
    (displayln "TERM is set!")
    (displayln "TERM is NOT set."))
|#

#;(test)
