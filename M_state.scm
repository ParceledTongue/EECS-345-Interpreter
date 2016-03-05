; EECS 345 Project #2
; Jonah Raider-Roth (jer135)
; Zachary Palumbo (ztp3)

; Language: Pretty Big
; To run a program, run (interpret <filename>)

(load "layered-state.scm")

; M_state
(define M_state
  (lambda (statement state)
    (cond
      ((null? statement) state) ; no statement
      ((eq? (statement-type statement) 'var) ; declaration
       (if (eq? (length statement) 2)
           (state-declare (dec-var statement) state) ; declaration without assignment
           (state-set (dec-var statement) (M_value (dec-value statement) state) (state-declare (dec-var statement) state)))) ; declaration with assignment
      ((eq? (statement-type statement) '=) ; assignment
       (state-set (dec-var statement) (M_value (dec-value statement) state) state))
      ((eq? (statement-type statement) 'if) ; "if" statement
       (M_state-if statement state))
      ((eq? (statement-type statement) 'begin) ; code block that hasn't been examined yet
        (other-layers (evaluate-state-call/cc (arguments statement) (add-layer state))))
      ((eq? (statement-type statement) 'while) ; "while" statement
       (M_state-while statement state))
      ((eq? (statement-type statement) 'break) ; "break" instruction
       (state-add-bottom-break state))
      ((eq? (statement-type statement) 'continue) ; "continue" instruction
       (state-add-bottom-continue state))
      ((eq? (statement-type statement) 'begin) ; code block that hasn't been examined yet
        (other-layers (evaluate-state-call/cc (arguments statement) (add-layer state))))
      ((eq? (statement-type statement) 'while) ; "while" statement
       (M_state-while statement state))
      ((eq? (statement-type statement) 'try)
       (M_state-try (try-part statement) (catch-part statement) (finally-part statement) state))
;      ((eq? (statement-type statement) 'throw)
;       (error (operand1 statement) "Thrown error outside try/catch"))
      ((eq? (statement-type statement) 'return) ; "return" statement
       (state-add-bottom-return (M_value (return-val statement) state) state)))))

; M_state for while loops
(define M_state-while
  (lambda (statement state)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (state)
                        (cond
                          ((state-has-return? state) (break state))
                          ((state-has-break? state) (break (state-remove-break state)))
                          ((state-has-continue? state) (loop (state-remove-continue state)))
                          (else
                           (if (eq? (M_value (condition statement) state) 'true)
                               (loop (evaluate-state-call/cc (list (while-body statement)) state))
                               (break state)))))))
         (loop state))))))

; M_state for if statements
(define M_state-if
  (lambda (statement state)
    (if (eq? (length statement) 3)
           (if (eq? (M_value (condition statement) state) 'true) ; statement without "else" clause
               (M_state (st-then statement) state) ; condition was true
               state) ; condition was false (then we just return the state since there is no "else" clause)
           (if (eq? (M_value (condition statement) state) 'true) ; statement with "else" clause
               (M_state (st-then statement) state) ; condition was true
               (M_state (st-else statement) state))))) ; condition was false

; M_state for finally blocks
(define M_state-finally
  (lambda (finally state)
    (cond
      ((null? finally) state)
      (else (M_state-finally (rest-statements finally) (M_state (next-statement finally) state))))))

; M_state for catch blocks
(define M_state-catch
  (lambda (cbody finally cstate)
      (cond
        ((and (null? cbody) (null? finally)) cstate)
        ((null? cbody) (M_state-finally finally cstate))
        (else (M_state-catch (rest-statements cbody) finally (M_state (next-statement cbody) cstate))))))

; M_state for try blocks
(define M_state-try
  (lambda (body catch finally state)
;    (other-layers
     (call/cc
      (lambda (break)
        (letrec ((loop (lambda (body catch finally state)
                         (cond
                           ((state-has-return? state) (break state))
                           ((state-has-thrown? state) (break (M_state-catch catch finally (state-add-bottom-thrown (operand1 (next-statement body)))))) 
                           ((null? body) (break (M_state-finally (cadr finally) state)))
                           ((eq? (operator (next-statement body)) 'throw) (break
                                                                           (let* ((cbody (catch-contents catch)) (cvar (catch-var catch)) (cstate (state-set cvar (operand1 (next-statement body)) (state-declare cvar state))))
                                                                              (M_state-catch cbody finally state)))); (state-add-bottom-thrown (operand1 (next-statement body)) state)))))
                           (else (loop (rest-statements body) catch finally (M_state (next-statement body) state)))))))
          (loop body catch finally (add-layer state)))))))

; M_value
(define M_value
  (lambda (l state)
    (cond
      ((number? l) l) ; input is a number
      ((eq? l 'true) 'true) ; input is the boolean value true
      ((eq? l 'false) 'false) ; input is the boolean value false
      ((symbol? l) (state-get l state)) ; input is a variable
      ; expressions
      ((eq? (operator l) '+) (mv-operate l state +))
      ((eq? (operator l) '-) (mv-operate l state -))
      ((eq? (operator l) '*) (mv-operate l state *))
      ((eq? (operator l) '/) (mv-operate l state quotient))
      ((eq? (operator l) '%) (mv-operate l state remainder))
      ; assignment or declaration with assignment
      ((eq? (operator l) '=) (M_value (dec-value l) state)) ; the value of an assignment is the value being assigned
      ((and (eq? (operator l) 'var) (has-value? l)) (M_value (dec-value l) state)) ; declaration must include assignment
      ; types of comparison
      ((eq? (operator l) '==) (mb-compare l state =))
      ((eq? (operator l) '>)  (mb-compare l state >))
      ((eq? (operator l) '>=) (mb-compare l state >=))
      ((eq? (operator l) '<)  (mb-compare l state <))
      ((eq? (operator l) '<=) (mb-compare l state <=))
      ((eq? (operator l) '!=) (mb-compare l state !=))
      ; logical operations
      ((eq? (operator l) '&&) (mb-and l state))
      ((eq? (operator l) '||) (mb-or l state))
      ((eq? (operator l) '!) (mb-not l state)))))

; macros for statements
(define statement-type car) ; the type of statement (e.g. "if", "=", "return", ...)
(define dec-var cadr) ; the variable being assigned to in an assignment or declaration
(define dec-value caddr) ; the value being assigned in an assignment or declaration
(define return-val cadr) ; the value being returned in a "return" statement
(define condition cadr) ; the boolean condition being evaluated in an "if" or "else" statement
(define st-then caddr) ; the "then" statement in an "if"
(define st-else cadddr) ; the "else" statement in an "if"
(define while-body caddr) ; the body of a "while" statement

; macros for value (prefix notation)
(define operator car)
(define arguments cdr)
(define argument1 cadr)
(define rest-arguments cddr)    
(define operand1 cadr)
(define operand2 caddr)
(define trystmt2 caaddr)
(define next-statement car)
(define rest-statements cdr)
(define catch-contents cddr)
(define catch-var caadr)

(define try-part cadr)
(define catch-part caddr)
(define finally-part cadddr)

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

(define !=
  (lambda (a1 a2)
    (not (= a1 a2))))

; functions for boolean "and", "or", and "not"
(define mb-and
  (lambda (l state)
    (if (and (eq? (M_value(operand1 l) state) 'true) (eq? (M_value(operand2 l) state) 'true))
        'true
        'false)))

(define mb-or
  (lambda (l state)
    (if (or (eq? (M_value(operand1 l) state) 'true) (eq? (M_value(operand2 l) state) 'true))
        'true
        'false)))

(define mb-not
  (lambda (l state)
    (if (eq? (M_value(operand1 l) state) 'true)
        'false
        'true)))

; check if a declaration also contains an assignment
(define has-value? (lambda (l) (pair? (cddr l))))
