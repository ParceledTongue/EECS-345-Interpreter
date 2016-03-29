; EECS 345 Project #1
; Jonah Raider-Roth (jer135)
; Zachary Palumbo (ztp3)

; Language: Pretty Big
; To run a program, run (interpret <filename>)

; ======== STATE LAYER IMPLEMENTATION ========

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
(define layer-get-box
  (lambda (name layer)
    (cond
      ((null? layer) #f)
      ((null? (names layer)) #f)
      ((eq? name (car (names layer))) (car (vals layer)))
      (else (layer-get-box name (layer-cdr layer))))))

(define layer-get
  (lambda (name layer)
    (cond
      ((null? layer) #f)
      ((null? (names layer)) #f)
      ((eq? name (car (names layer))) (unbox (car (vals layer))))
      (else (layer-get name (layer-cdr layer))))))

; edits the value associated with the given name in a given layer, and returns the new layer
(define layer-set
  (lambda (name value layer)
      (cond
        ((null? (names layer)) #f)
        ((eq? name (car (names layer)))
         ; replace the first entry in vals with the new value and return the entire layer
         (begin (set-box! (car (vals layer)) value) layer))
        (else (layer-cons
               (layer-car layer)
               (layer-set name value (layer-cdr layer)))))))
    

; creates a new name-value pair in a given layer with the given name and the value '(), and returns the new layer
(define layer-declare
  (lambda (name layer)
    (cond
      ((null? (names layer)) (list (list name) (list (box null))))
      ((eq? name 'return) (error "'return cannot be used as a variable name"))
      ((eq? name (car (names layer))) (error 'declared "Variable is already declared"))
      (else (layer-cons (layer-car layer) (layer-declare name (layer-cdr layer)))))))

; creates a name-value pair in a given layer that represents the return value of the layer
(define layer-add-return
  (lambda (value layer)
    (list
     (cons 'return (names layer))
     (cons value (vals layer)))))

(define layer-add-thrown
  (lambda (value layer)
    (list
     (cons 'thrown (names layer))
     (cons value (vals layer)))))

; creates a name-value pair in a given layer that represents a break
(define layer-add-break
  (lambda (layer)
    (list
     (cons 'break (names layer))
     (cons '() (vals layer)))))

; creates a name-value pair in a given layer that represents a break
(define layer-add-continue
  (lambda (layer)
    (list
     (cons 'continue (names layer))
     (cons '() (vals layer)))))

; whether a given layer has a return value
(define layer-has-return?
  (lambda (layer)
    (cond
      ((null? (names layer)) #f)
      ((eq? 'return (car (names layer))) #t)
      (else (layer-has-return? (layer-cdr layer))))))

; whether a given layer has a break indicator
(define layer-has-break?
  (lambda (layer)
    (cond
      ((null? (names layer)) #f)
      ((eq? 'break (car (names layer))) #t)
      (else (layer-has-break? (layer-cdr layer))))))

; whether a given layer has a continue indicator
(define layer-has-continue?
  (lambda (layer)
    (cond
      ((null? (names layer)) #f)
      ((eq? 'continue (car (names layer))) #t)
      (else (layer-has-continue? (layer-cdr layer))))))

; remove the break indicator from the current layer
(define layer-remove-break
  (lambda (layer)
    (cond
      ((null? (car (names layer))) (error "The layer has no break indicator."))
      ((eq? (car (names layer)) 'break) (layer-cdr layer))
      (else (layer-cons (layer-car layer) (layer-remove-break layer))))))

; remove the continue indicator from the current layer
(define layer-remove-continue
  (lambda (layer)
    (cond
      ((null? (car (names layer))) (error "The layer has no continue indicator."))
      ((eq? (car (names layer)) 'continue) (layer-cdr layer))
      (else (layer-cons (layer-car layer) (layer-remove-continue layer))))))

(define layer-has-thrown?
  (lambda (layer)
    (cond
      ((null? (names layer)) #f)
      ((eq? 'thrown (car (names layer))) #t)
      (else (layer-has-thrown? (layer-cdr layer))))))
