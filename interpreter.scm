; EECS 345 Project #1
; Jonah Raider-Roth (jer135)
; Zachary Palumbo (ztp3)

; Language: Pretty Big
; To run a program, run (interpret <filename>)

(load "simpleParser.scm")
(load "M_state.scm")

(define empty-state '(()()))

(define interpret
  (lambda (filename)
    (evaluate (parser filename) empty-state)))

(define evaluate
  (lambda (program state)
    (cond
      ((state-has-return? state) (state-get 'return state))
      ((null? program) 'null) ; an empty program (the ultimate evaluation of a program without a return statement) returns 'null
      (else (evaluate (rest-statements program) (M_state (next-statement program) state))))))

; macros for program evaluation
(define next-statement car)
(define rest-statements cdr)