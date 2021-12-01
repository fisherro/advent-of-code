#lang racket

(provide qtest)

#|
(define-namespace-anchor a)
(define local-ns (namespace-anchor->namespace a))
|#
#;(define ns (make-parameter local-ns))

; A quick test procudure
(define qtest
  (case-lambda
    ((ns form-to-test)
     (define result (eval form-to-test ns))
     (printf "[INFO] \"~s\" returned \"~s\"\n"
             form-to-test
             result))
    ((ns form-to-test expected-result)
     (define result (eval form-to-test ns))
     (define label (if (equal? result expected-result)
                       "PASS"
                       "FAIL"))
     (printf "[~a] \"~s\" returned \"~s\" expected \"~s\"\n"
             label
             form-to-test
             result
             expected-result))))

#;(define-syntax (qtest stx)
  (syntax-case stx (a)
    ((qtest ftt er) #'(define-namespace-anchor a)
                    #'(qtest-proc (namespace-anchor->namespace a) ftt er))))
