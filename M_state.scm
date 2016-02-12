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
      ((not (pair? l)) (error "M_value is only defined for numbers, variables, expressions, and assignments."))
      ; else it is an expression or an assignment
      ((eq? (operator l) '+) (mv-operate l state +))
      ((eq? (operator l) '-) (mv-operate l state -))
      ((eq? (operator l) '*) (mv-operate l state *))
      ((eq? (operator l) '/) (mv-operate l state quotient))
      ((eq? (operator l) '%) (mv-operate l state remainder))
      ((eq? (operator l) '=) (M_value (dec-value l) state)) ; the value of an assignment is the value being assigned
      ((and (eq? (operator l) 'var) (has-value? l)) (M_value (dec-value l) state)) ; declaration must include assignment
      (else (error "M_value is only defined for numbers, variables, expressions, and assignments.")))))

(define M_boolean
  (lambda (l state)
    (cond
      ((null? l) (error "Tried to get the boolean value of null"))
      ((eq? l 'true) 'true) ; true is true
      ((eq? l 'false) 'false) ; false is false
      ((not (pair? l)) (error "M_boolean is only defined for 'true, 'false, comparisons, and logical operators."))
      ; types of comparison
      ((eq? (operator l) '==) (mb-compare l state =))
      ((eq? (operator l) '>)  (mb-compare l state >))
      ((eq? (operator l) '>=) (mb-compare l state >=))
      ((eq? (operator l) '<)  (mb-compare l state <))
      ((eq? (operator l) '<=) (mb-compare l state <=))
      ; logical operations
      ((eq? (operator l) '&&) (mb-and l state))
      ((eq? (operator l) '||) (mb-or l state))
      ((eq? (operator l) '!) (mb-not l state))
      (else (error "M_boolean is only defined for 'true, 'false, comparisons, and logical operators."))))) 

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

; shorthand for boolean comparisons
; (we cannot use mv-operate, because we want 'true and 'false rather than #t and #f)
(define mb-compare
  (lambda (l state func)
    (if (func (M_value (operand1 l) state) (M_value (operand2 l) state))
        'true
        'false)))

; functions for boolean "and", "or", and "not"
(define mb-and
  (lambda (l state)
    (if (and (eq? (M_boolean(operand1 l) state) 'true) (eq? (M_boolean(operand2 l) state) 'true))
        'true
        'false)))

(define mb-or
  (lambda (l state)
    (if (or (eq? (M_boolean(operand1 l) state) 'true) (eq? (M_boolean(operand2 l) state) 'true))
        'true
        'false)))

(define mb-not
  (lambda (l state)
    (if (eq? (M_boolean(operand1 l) state) 'true)
        'false
        'true)))

; check if a declaration also contains an assignment
(define has-value? (lambda (l) (pair? (cddr l))))
(define dec-value caddr) ; the value being assigned in an assignment or declaration