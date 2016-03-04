; EECS 345 Project #1
; Jonah Raider-Roth (jer135)
; Zachary Palumbo (ztp3)

; Language: Pretty Big
; To run a program, run (interpret <filename>)

; ======== layer IMPLEMENTATION ========

; the layer list has two sublists containing names and valuess ("vals"), respectively
; for example, the layerments (x = 1; y = 2; z = 3;) would yield the layer:
; '((x y z) (1 2 3))

#|

TODO - implement new layer-get, layer-set

|#

(define names  car)
(define vals   cadr)


; returns the first name-value pair in layer
(define layer-car
  (lambda (layer)
    (list (car (names layer)) (car (vals layer)))))

; returns all the name-value pairs in layer except the first
(define layer-cdr
  (lambda (layer)
    (list (cdr (names layer)) (cdr (vals layer)))))

; inserts the given name-value pair into a layer which does not already contain that pair
(define layer-cons
  (lambda (pair layer)
    (list
     (cons (names pair) (names layer))
     (cons (vals pair) (vals layer)))))

; replaces the first item in a list with a different item
(define replace-first
  (lambda (l value)
    (cons value (cdr l))))

; returns the value associated with the given name in a given layer
(define layer-get
  (lambda (name layer)
    (cond
      ((null? layer) (error 'unknown "Unknown expression"))
      ((null? (names layer)) (error name "Variable is undeclared"))
      ((eq? name (car (names layer))) (car (vals layer)))
      (else (layer-get name (layer-cdr layer))))))

; edits the value associated with the given name in a given layer, and returns the new layer
(define layer-set
  (lambda (name value layer)
    (cond
      ((null? (names layer)) (error name "Variable is undeclared"))
      ((eq? name (car (names layer)))
       ; replace the first entry in vals with the new value and return the entire layer
       (list (names layer) (replace-first (vals layer) value)))
      (else (layer-cons
             (layer-car layer)
             (layer-set name value (layer-cdr layer)))))))

; creates a new name-value pair in a given layer with the given name and the value '(), and returns the new layer
(define layer-declare
  (lambda (name layer)
    (cond
      ((null? (names layer)) (list (list name) (list null)))
      ((eq? name 'return) (error "'return cannot be used as a variable name"))
      ((eq? name (car (names layer))) (error 'declared "Variable is already declared"))
      (else (layer-cons (layer-car layer) (layer-declare name (layer-cdr layer)))))))

; creates a name-value pair in a given layer that represents the return value of the layer
(define layer-add-return
  (lambda (value layer)
    (list
     (cons 'return (names layer))
     (cons value (vals layer)))))

; whether a given layer has a return value
(define layer-has-return?
  (lambda (layer)
    (cond
      ((null? (names layer)) #f)
      ((eq? 'return (car (names layer))) #t)
      (else (layer-has-return? (layer-cdr layer))))))