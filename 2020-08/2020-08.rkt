#lang racket

;;; read & parse program
;;; convert the program into a vector for random access?
;;; execute...
;;;  check to see if this instruction has been executed before
;;;  if not execute it & add it to the history

(struct instruction (addr opcode arg) #:transparent)

(define (parse-instruction s addr)
  (define lst (string-split s " "))
  (instruction addr
               (first lst)
               (string->number (second lst))))

(define (parse-program)
  (vector->immutable-vector
   (for/vector ((line (in-lines))
                (addr (in-naturals)))
     (parse-instruction line addr))))

(define (execute-instruction program pc acc)
  (define inst (vector-ref program pc))
  (case (instruction-opcode inst)
    (("acc") (values (add1 pc) (+ acc (instruction-arg inst))))
    (("jmp") (values (+ pc (instruction-arg inst)) acc))
    (("nop") (values (add1 pc) acc))))

(define (execute-program program)
  (let loop ((pc 0)
             (acc 0)
             (history (set)))
    (cond ((set-member? history pc)
           (printf "Repeating addr ~a. Acc = ~a~n" pc acc)
           #f)
          ((>= pc (vector-length program))
           (printf "Terminating cleanly! Acc = ~a~n" acc)
           #t)
          (else
           (let-values (((new-pc new-acc)
                         (execute-instruction program pc acc)))
             (loop new-pc new-acc (set-add history pc)))))))

(define (run file)
  (printf "Running \"~a\": " file)
  (with-input-from-file file
    (thunk
     (define program (parse-program))
     ;(writeln program)
     (execute-program program))))

(define (patch program addr)
  (define (patch-it inst)
    (cond ((= addr (instruction-addr inst))
           (case (instruction-opcode inst)
             (("jmp") (instruction addr "nop" (instruction-arg inst)))
             (("nop") (instruction addr "jmp" (instruction-arg inst)))
             (else 'no-chage)))
          (else inst)))
  (cond ((< addr 0) program)
        ((>= addr (vector-length program))
         'past-end)
        ((string=? "acc" (instruction-opcode (vector-ref program addr)))
         'no-change)
        (else
         (vector-map patch-it program))))

(define (find-fix file)
  (with-input-from-file file
    (thunk
     (define program (parse-program))
     (let loop ((addr-to-patch -1))
       (let ((p (patch program addr-to-patch)))
         (cond ((equal? p 'past-end)
                (println "No successful patch found"))
               ((equal? p 'no-change)
                (loop (add1 addr-to-patch)))
               ((execute-program p)
                (printf "Patched ~a~n" addr-to-patch))
               (else
                (loop (add1 addr-to-patch)))))))))

(run "test.txt")
(run "test-fixed.txt")
(run "input.txt")
(find-fix "test.txt")
(find-fix "input.txt")