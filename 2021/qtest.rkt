#lang racket

#|
; Racket's tests only print details on failure,
; but I want details on success too.
; So, I made qtest.
; Example of Racket's normal tests:
(check-expect (+ 2 2) 4)
(test)
|#

(provide qtest)

(require (for-syntax syntax/to-string))

; TODO: Figure out how to lift common elements of the two cases.
(define-syntax (qtest stx)
  (syntax-case stx ()
    ((_ form-to-test expected-result)
     (with-syntax ((s (datum->syntax stx (syntax->string #'form-to-test))))
       #'(let ((result form-to-test))
           (printf "[~a] (~a) returned ~s expected ~s\n"
                   (if (equal? result expected-result)
                       "PASS"
                       "FAIL")
                   s
                   result
                   expected-result))))
    ((_ form-to-test)
     (with-syntax ((s (datum->syntax stx (syntax->string #'form-to-test))))
       #'(let ((result form-to-test))
           (printf "[INFO] (~a) returned ~s\n"
                   s
                   result))))))
