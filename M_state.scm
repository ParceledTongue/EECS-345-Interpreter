(load "state.scm")

; stub function that just returns the current state and has no effect, to ensure the code compiles
(define M_state
  (lambda (statement state)
    state))

(define M_value
  (lambda (l state)
    (cond
      ((null? l) (error "Tried to get the value of null."))
      ((number? l) l) ; input is a number
      ((symbol? l) (state-get l state)) ; input is a variable
      ; else it is an expression or an assignment
      ((eq? (operator l) '+) (mv-operate l state +))
      ((eq? (operator l) '-) (mv-operate l state -))
      ((eq? (operator l) '*) (mv-operate l state *))
      ((eq? (operator l) '/) (mv-operate l state quotient))
      ((eq? (operator l) '%) (mv-operate l state remainder))
      ((eq? (operator l) '=) (M_value (dec-value l) state)) ; the value of an assignment is the value being assigned
      ((and (eq? (operator l) 'var) (has-value? l)) (M_value (dec-value l) state)) ; declaration must include assignment
      (else (error "M_value is only defined for numbers, variables, expressions, and assignments.")))))

; prefix notation
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define condition cadr)
(define st-then caddr)
(define st-else cadddr)

; shorthand for all those binary operator functions (and unary -)
(define mv-operate
  (lambda (l state func)
    (cond
      ((= (length l) 3) (mv-operate-binary l state func)) ; list has two operands
      ((= (length l) 2) (mv-operate-unary l state func)) ; list has one operand
      (else (error "Only binary and unary operations are supported")))))

(define mv-operate-binary
  (lambda (l state func)
    (func (M_value (operand1 l) state) (M_value (operand2 l) state))))

(define mv-operate-unary
  (lambda (l state func)
    (func (M_value (operand1 l) state))))
  
; creating functions because apparently "and" and "or" aren't functions
(define andf
  (lambda (a1 a2)
    (and a1 a2)))

(define orf
  (lambda (a1 a2)
    (or a1 a2)))

; check if a declaration also contains an assignment
(define has-value? (lambda (l) (pair? (cddr l))))
(define dec-value caddr) ; the value being assigned in an assignment or declaration