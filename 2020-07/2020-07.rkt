#lang racket

; list(bag)
; bag = list(subbags)
; subbag = count bag-spec

; light red bags contain 1 bright white bag, 2 muted yellow bags.
; split on contains
; then split on comma

(define (parse-bag s)
  (let* ((it (string-split s)))
    (list (string->number (first it))
          (string-join (rest it)))))

;; (ab)using let* this way always feels like bad style
(define (parse-line line)
  (let* ((it (string-trim line #rx"\\."))
         (it (string-replace it #rx"bags?" ""))
         (it (string-split it "contain"))
         (key (string-trim (first it)))
         (it (second it))
         (it (string-split it ","))
         (it (map string-trim it))
         (it (if (and (= 1 (length it))
                      (string=? "no other" (first it)))
                 '()
                 it))
         (it (map parse-bag it)))
    (list key it)))

(define (parse-input)
  (sequence->list
   (sequence-map
    parse-line
    (in-lines))))

; for each bag type
;  can it contain shiny gold?
;  can any of its containable types contain shiny gold?
; memoization opportunity!

(define (can-contain rules holder held)
  (let* ((rule (assoc holder rules))
         (contents (second rule)))
    (ormap (λ (subbag-data)
             (let ((subbag (second subbag-data)))
               (or (string=? held subbag)
                   (can-contain rules subbag held))))
           contents)))

(define (which-can-contain rules held)
  (for/fold ((results '()))
            ((rule rules))
    (let ((bag (first rule)))
      (if (can-contain rules bag held)
          (cons bag results)
          results))))

(define rules
  (with-input-from-file "input.txt"
    parse-input))
;(writeln rules)

;(can-contain rules "bright white" "shiny gold")
;(can-contain rules "light red" "shiny gold")
;(can-contain rules "faded blue" "shiny gold")
;(can-contain rules "dark olive" "shiny gold")

(define containers (which-can-contain rules "shiny gold"))
(writeln containers)
(writeln (length containers)) ; 208