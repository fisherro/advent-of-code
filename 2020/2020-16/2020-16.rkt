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

(define (info expr)
  (define result (eval expr ns))
  (printf "[INFO] \"~s\" returned ~a\n" expr result))

(define (string-empty? s)
  (= 0 (string-length s)))

(struct field-info
  (name valid?)
  #:transparent)

(define (make-range-validator a b)
  (λ (n)
    ((n . >= . a) . and . (n . <= . b))))

(define (make-or-validator a b)
  (λ (n)
    (or (a n)
        (b n))))

(define (parse-field s)
  (define (s->n s)
    (define n (string->number s))
    (if n n s))
  (define matches
    (regexp-match #rx"^([^:]+): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)$"
                  s))
  (match (map s->n matches)
    ((list _ name a b x y)
     (field-info name
                 (make-or-validator (make-range-validator a b)
                                    (make-range-validator x y))))))

(info '(parse-field "class: 1-3 or 5-7"))
(expect '((field-info-valid? (parse-field "class: 1-3 or 5-7")) 4) #f)
(expect '((field-info-valid? (parse-field "class: 1-3 or 5-7")) 2) #t)

(define (parse-file)
  (define (parse-ticket s)
    (map string->number (string-split s ",")))
  (define-values (mode fields my-ticket nearby-tickets)
    (for/fold ((mode 'parsing-fields)
               (fields '())
               (my-ticket '())
               (nearby-tickets '()))
              ((line (in-lines)))
      (cond ((eq? mode 'parsing-fields)
             (if (string-empty? line)
                 (values 'parsing-my-ticket fields my-ticket nearby-tickets)
                 (values mode
                         (cons (parse-field line)
                               fields)
                         my-ticket
                         nearby-tickets)))
            ((eq? mode 'parsing-my-ticket)
             (cond ((string=? line "your ticket:")
                    (values mode fields my-ticket nearby-tickets))
                   ((string-empty? line)
                    (values 'parsing-nearby-tickets
                            fields
                            my-ticket
                            nearby-tickets))
                   (else
                    (values mode
                            fields
                            (parse-ticket line)
                            nearby-tickets))))
            (else
             (cond ((string=? line "nearby tickets:")
                    (values mode fields my-ticket nearby-tickets))
                   ((string-empty? line)
                    (values mode fields my-ticket nearby-tickets))
                   (else
                    (values mode
                            fields
                            my-ticket
                            (cons (parse-ticket line)
                                  nearby-tickets))))))))
  (values fields my-ticket nearby-tickets))

; This is so ugly... :(
(define (part1 file)
  (define-values
    (fields my-ticket nearby-tickets)
    (with-input-from-file file parse-file))
  (define (find-invalid n)
    (map (λ (f)
           (if ((field-info-valid? f) n)
               #t
               n))
         fields))
  (aand (map (λ (ticket)
               #;(printf "ticket: ~s\n" ticket)
               (aand (map find-invalid ticket)
                     (filter (λ (lst)
                               (andmap number? lst))
                             it)
                     (filter-not empty? it)
                     (flatten it)
                     (remove-duplicates it)
                     #;(wl it)))
             nearby-tickets)
        (flatten it)
        (apply + it)))

(expect '(part1 "test.txt") 71)
(expect '(part1 "input.txt") 19060)