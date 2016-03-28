; EECS 345 Project #2
; Jonah Raider-Roth (jer135)
; Zachary Palumbo (ztp3)

; Language: Pretty Big
; To run a program, run (interpret <filename>)

(load "functionParser.scm")
(load "M_state.scm")

(define empty-state '((()())))

(define interpret
  (lambda (filename)
    (evaluate-call/cc (parser filename) empty-state)))

#| OLD EVALUATE FUNCTIONS (not used in this version)
(define evaluate
  (lambda (program state)
    (cond
      ((state-has-return? state) (state-get 'return state))
      ((null? program) 'null) ; an empty program (the ultimate evaluation of a program without a return statement) returns 'null
      (else (evaluate (rest-statements program) (M_state (next-statement program) state))))))

(define evaluate-state ; gets the final state after a program runs; only used inside of code blocks
  (lambda (program state)
    (cond
      ((state-has-return? state) state)
      ((null? program) state)
      (else (evaluate-state (rest-statements program) (M_state (next-statement program) state))))))
|#
(define evaluate-call/cc
  (lambda (program state)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (program state)
                        (cond
                          ((state-has-return? state) (break (state-get 'return state)))
                          ((null? program) 'null)
                          (else (evaluate-call/cc (rest-statements program) (M_state (next-statement program) state)))))))
         (loop program state))))))

(define evaluate-state-call/cc
  (lambda (program state)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (program state)
                            (cond
                              ((state-has-return? state) (break state))
                              ((null? program) state)
                              (else (loop (rest-statements program) (M_state (next-statement program) state)))))))
                (loop program state))))))

; macros for program evaluation
(define next-statement car)
(define rest-statements cdr)