; stub function that just returns the current state and has no effect, to ensure the code compiles
(define M_state
  (lambda (statement state)
    state))

(define M_value
  (lambda (l)
    (cond
      ((null? l) 0)
      ((number? l) l)
      ((eq? (operator l) '+) (+ (M_value (operand1 l)) (M_value (operand2 l))))
      ((eq? (operator l) '-) (- (M_value (operand1 l)) (M_value (operand2 l))))
      ((eq? (operator l) '*) (* (M_value (operand1 l)) (M_value (operand2 l))))
      ((eq? (operator l) '/) (quotient (M_value (operand1 l)) (M_value (operand2 l))))
      ((eq? (operator l) '%) (remainder (M_value (operand1 l)) (M_value (operand2 l))))
      (else (error 'unknown "Unknown expression")))))

; infix notation
; (define operator cadr)
; (define operand1 car)
; (define operand2 caddr)

; prefix notation
(define operator car)
(define operand1 cadr)
(define operand2 caddr)

; postfix notation
; (define operator caddr)
; (define operand1 cadr)
; (define operand2 car)