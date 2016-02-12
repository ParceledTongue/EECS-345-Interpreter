
; ======== STATE IMPLEMENTATION ========

; the state list has two sublists containing names and valuess ("vals"), respectively
; for example, the statements (x = 1; y = 2; z = 3;) would yield the state:
; '((x y z) (1 2 3))

(define names  car)
(define vals   cadr)

; returns the first name-value pair in state
(define state-car
  (lambda (state)
    (list (car (names state)) (car (vals state)))))

; returns all the name-value pairs in state except the first
(define state-cdr
  (lambda (state)
    (list (cdr (names state)) (cdr (vals state)))))

; inserts the given name-value pair into a state which does not already contain that pair
(define state-cons
  (lambda (pair state)
    (list
     (cons (names pair) (names state))
     (cons (vals pair) (vals state)))))

; replaces the first item in a list with a different item
(define replace-first
  (lambda (l value)
    (cons value (cdr l))))

; returns the value associated with the given name in a given state
(define state-get
  (lambda (name state)
    (cond
      ((null? state) (error 'unknown "Unknown expression"))
      ((null? (names state)) (error name "Variable is undeclared"))
      ((eq? name (car (names state))) (car (vals state)))
      (else (state-get name (state-cdr state))))))

; edits the value associated with the given name in a given state, and returns the new state
(define state-set
  (lambda (name value state)
    (cond
      ((null? (names state)) (error name "Variable is undeclared"))
      ((eq? name (car (names state)))
       ; replace the first entry in vals with the new value and return the entire state
       (list (names state) (replace-first (vals state) value)))
      (else (state-cons
             (state-car state)
             (state-set name value (state-cdr state)))))))

; creates a new name-value pair in a given state with the given name and the value '(), and returns the new state
(define state-declare
  (lambda (name state)
    (cond
      ((null? (names state)) (list (list name) (list null)))
      ((eq? name (car (names state))) (error 'declared "Variable is already declared"))
      (else (state-cons (state-car state) (state-declare name (state-cdr state)))))))
