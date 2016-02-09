(load "simpleParser.scm")
(load "state.scm")
(load "M_state.scm")
(load "evaluate.scm")

(define program (parser "tests/test1"))

; the empty state has one value - "return" - which is set to null
(define empty-state
  (lambda ()
    '((return) (()))))

; output the return value of the program
(state-get 'return (evaluate program (empty-state)))