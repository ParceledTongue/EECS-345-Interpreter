(load "simpleParser.scm")
(load "state.scm")
(load "evaluate.scm")

(define program (parser "tests/test1"))
(define empty-state
  (lambda ()
    '(() ())))

(printf (state-get 'return (evaluate program (empty-state))))