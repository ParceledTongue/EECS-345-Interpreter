; EECS 345 Project #2
; Jonah Raider-Roth (jer135)
; Zachary Palumbo (ztp3)

; Language: Pretty Big
; To run a program, run (interpret <filename>)

(load "layer.scm")

(define new-state '( ( () () ) ))
(define new-layer '(()()))

(define add-layer (lambda (state) (cons new-layer state)))
(define top-layer car)
(define other-layers cdr)

(define state-get
  (lambda (name state)
    (cond
      ((null? state) (error 'unknown "Unknown expression"))
      ((