(define next-statement car)
(define rest-statements cdr)

(define evaluate
  (lambda (program state)
    (cond
      ((null? program) state)
      (else (evaluate (rest-statements program) (M_state next-statement state))))))
      