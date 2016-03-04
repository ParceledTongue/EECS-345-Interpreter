; EECS 345 Project #2
; Jonah Raider-Roth (jer135)
; Zachary Palumbo (ztp3)

; Language: Pretty Big
; To run a program, run (interpret <filename>)

(load "layer.scm")

(define new-state '( ( () () ) ))
(define new-layer '(()()))
(define sample-state '(((x z)(2 4))((y a)(3 5))))

(define add-layer (lambda (state) (cons new-layer state)))
(define top-layer car)
(define other-layers cdr)

(define state-get
  (lambda (name state)
    (cond
      ((null? state) (error name "Unknown expression"))
      ((layer-get name (top-layer state)) (layer-get name (top-layer state))) ; if the top layer contains the variable, just return it
      (else (state-get name (other-layers state))))))

(define state-set
  (lambda (name value state)
    (cond
      ((null? state) (error name "Unknown expression"))
      ((layer-get name (top-layer state)) (cons (layer-set name value (top-layer state)) (other-layers state)))
      (else (cons (top-layer state) (state-set name value (other-layers state)))))))

(define state-declare
 (lambda (name state)
   (cons (layer-declare name (top-layer state)) (other-layers state))))

(define state-add-return ; not used in this version
  (lambda (value state)
    (cons (layer-add-return value (top-layer state)) (other-layers state))))

(define state-add-bottom-return
  (lambda (value state)
    (cond
      ((null? (other-layers state)) (list (layer-add-return value (top-layer state))))
      (else (cons (top-layer state) (state-add-bottom-return value (other-layers state)))))))

(define state-has-return?
  (lambda (state)
    (cond
      ((null? state) #f)
      ((layer-has-return? (top-layer state)) #t)
      (else (state-has-return? (other-layers state))))))