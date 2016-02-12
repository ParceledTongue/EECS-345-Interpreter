; stub function that just returns the current state and has no effect, to ensure the code compiles
(define M_state
  (lambda (statement state)
    state))

(define M_value
  (lambda (l state)
    (cond
      ((null? l) 0)
      ((or (number? l) (boolean? l)) l)
      ((or (number? (operator l)) (boolean? (operator l))) (operator l))
      ; else it is a pair or an expression
      ((eq? (operator l) '+) (mv-operate l state +))
      ((eq? (operator l) '-) (mv-operate l state -))
      ((eq? (operator l) '*) (mv-operate l state *))
      ((eq? (operator l) '/) (mv-operate l state quotient))
      ((eq? (operator l) '%) (mv-operate l state remainder))

      ; control flow statements
      ((eq? (operator l) 'if) (if (M_value (condition l) state)    ; if condition = true
                                      (M_value (st-then l) state)  ; get the value of the then part
                                      (if (pair? (cdddr state))    ; else if it has an else
                                          (M_value (st-else l) state)   ; get the value of the else part
                                          '())))                        ; else return null
      ((eq? (operator l) 'while) (if (M_value (condition l) state) ; if condition = true
                                     (M_value l (M_state l state)) ; calculate the new state and then get the value
                                     (M_value (st-then l) state))) ; else just return the value
      ; types of comparison
      ((eq? (operator l) '==) (mv-operate l state =))
      ((eq? (operator l) '>)  (mv-operate l state >))
      ((eq? (operator l) '>=) (mv-operate l state >=))
      ((eq? (operator l) '<)  (mv-operate l state <))
      ((eq? (operator l) '<=) (mv-operate l state <=))
      ; logical operations
      ((eq? (operator l) '&&) (mv-operate l state andf))
      ((eq? (operator l) '||) (mv-operate l state orf))
      ((eq? (operator l) '!) (not (M_value (operand1 l) state)))

      ; now we're looking at control flow statements
      ((eq? (operator l) 'return) (M_value (operand1 l) state))
      ((eq? (operator l) 'var) (if (has-value? l) (M_value (dec-value l) state) '()))
      ((eq? (operator l) '=) (M_value (dec-value l) state))
      ; else it is a variable
      (else (state-get (operator l) state)))))

; prefix notation
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define condition cadr)
(define st-then caddr)
(define st-else cadddr)

; shorthand for all those binary operator functions
(define mv-operate
  (lambda (l state func)
    (func (M_value (operand1 l) state) (M_value (operand2 l) state))))

; creating functions because apparently "and" and "or" aren't functions
(define andf
  (lambda (a1 a2)
    (and a1 a2)))

(define orf
  (lambda (a1 a2)
    (or a1 a2)))

; check if a declaration also contains an assignment
(define has-value? (lambda (l) (pair? (cddr l))))
(define dec-value caddr)
