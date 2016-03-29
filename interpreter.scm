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

; get the return value of the program
(define evaluate-call/cc
  (lambda (program state)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (program state)
                        (cond
                          ((state-has-break? state) (error "You cannot break outside of a loop."))
                          ((state-has-continue? state) (error "You cannot continue outside of a loop."))
                          ((state-has-return? state) (break (state-get 'return state)))
                          ; ((state-has-thrown? state) (error (state-get 'thrown state) "Thrown error"))
                          ((null? program) 'null)
                          (else (loop (rest-statements program) (M_state (next-statement program) state)))))))
         (loop program state))))))

; return the state resulting from executing the list of statements in program.
(define evaluate-state-call/cc
  (lambda (program state)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (program state)
                            (cond
                              ((or (or (state-has-return? state) (state-has-break? state)) (state-has-continue? state)) (break state))
                              ((state-has-return? state) (break state))
                              ((state-has-thrown? state) (error "Thrown error"))
                              ((null? program) state)
                              (else (loop (rest-statements program) (M_state (next-statement program) state)))))))
                (loop program state))))))

; macros for program evaluation
(define next-statement car)
(define rest-statements cdr)