; EECS 345 Project #3
; Jonah Raider-Roth (jer135)
; Zachary Palumbo (ztp3)

; Language: Pretty Big
; To run a program, run (interpret <filename>)

(load "classParser.scm")

; ; ; ; ; ; ; ;
; INTERPRETER ;
; ; ; ; ; ; ; ;
; This section contains the high-level program interpretation and evaluation functions

(define empty-state '((()()) (()())) )
(define main-call '(funcall main))

(define interpret
  (lambda (filename classname)
    ((lambda (program cname state)
       (return-main (state-get-class cname state) (load-classes program state)))
     (parser filename) classname empty-state)))

; create the outer layer of the program state
(define make-outer-layer
  (lambda (program state)
    (if (null? program)
        state
        (make-outer-layer (rest-statements program) (M_state (next-statement program) state)))))

; return the value returned by the main method
(define return-main
  (lambda (class state)
    (M_value main-call (state-get 'main (methods class)))))

; return the return value resulting from executing a list of statements and a given global state
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

; return the global state resulting from executing a list of statements and a given global state
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

; returns a state containing all class definitions from a program
(define load-classes
  (lambda (program state)
    (cond
    ((null? program) state)
    (else (load-classes (rest-statements program) (state-add-class (cadr (next-statement program)) (add-class-def (next-statement program) state) state))))))

; macros for program evaluation
(define next-statement car)
(define rest-statements cdr)

; ; ; ; ; ; ; ; ; ; ;
; MSTATE AND MVALUE ;
; ; ; ; ; ; ; ; ; ; ;
; This section contains the M_state and M_value functions and all their helpers

; M_state
(define M_state
  (lambda (statement state classname instance)
    (cond
      ((null? statement) state) ; no statement
      ((eq? (statement-type statement) 'var) ; declaration
       (if (eq? (length statement) 2)
           (state-declare (dec-var statement) state) ; declaration without assignment
           (state-set (dec-var statement) (M_value (dec-value statement) state classname instance) (state-declare (dec-var statement) state)))) ; declaration with assignment
      ((eq? (statement-type statement) '=) ; assignment
       (state-set (dec-var statement) (M_value (dec-value statement) state classname instance) state))
      ((eq? (statement-type statement) 'if) ; "if" statement
       (M_state-if statement state classname instance))
      ((eq? (statement-type statement) 'begin) ; code block that hasn't been examined yet
        (other-layers (evaluate-state-call/cc (arguments statement) (add-layer state))))
      ((eq? (statement-type statement) 'while) ; "while" statement
       (M_state-while statement state classname instance))
      ((eq? (statement-type statement) 'break) ; "break" instruction
       (state-add-bottom-break state))
      ((eq? (statement-type statement) 'continue) ; "continue" instruction
       (state-add-bottom-continue state))
      ((eq? (statement-type statement) 'begin) ; code block that hasn't been examined yet
        (other-layers (evaluate-state-call/cc (arguments statement) (add-layer state))))
      ((eq? (statement-type statement) 'while) ; "while" statement
       (M_state-while statement state classname instance))
      ((eq? (statement-type statement) 'try)
       (M_state-try (try-part statement) (catch-part statement) (finally-part statement) state classname instance))
      ((eq? (statement-type statement) 'throw)
       (state-add-bottom-thrown (M_value (operand1 statement) state classname instance) state))
      ((eq? (statement-type statement) 'return) ; "return" statement
       (state-add-bottom-return (M_value (return-val statement) state classname instance) state))
      ((eq? (statement-type statement) 'funcall)
       (ms-function (funcall-name statement) (funcall-args statement) state))
      ((eq? (statement-type statement) 'function) ; declaring a function is similar to declaring a variable, we just bind the closure to the name
       (state-declare-and-set (funcdec-name statement) (make-closure statement (lambda (v) (state-get-bottom-n-layers (num-layers state) v)) (lambda (state) (state-get-class classname state))) state)))))

; check if a declaration also contains an assignment
(define has-value? (lambda (l) (pair? (cddr l))))

; M_state for while loops
(define M_state-while
  (lambda (statement state classname instance)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (state)
                        (cond
                          ((state-has-return? state) (break state))
                          ((state-has-break? state) (break (state-remove-break state)))
                          ((state-has-continue? state) (loop (state-remove-continue state)))
                          (else
                           (if (eq? (M_value (condition statement) state classname instance) 'true)
                               (loop (evaluate-state-call/cc (list (while-body statement)) state))
                               (break state)))))))
         (loop state))))))

; M_state for if statements
(define M_state-if
  (lambda (statement state classname instance)
    (if (eq? (length statement) 3)
           (if (eq? (M_value (condition statement) state classname instance) 'true) ; statement without "else" clause
               (M_state (st-then statement) state classname instance) ; condition was true
               state) ; condition was false (then we just return the state since there is no "else" clause)
           (if (eq? (M_value (condition statement) state classname instance) 'true) ; statement with "else" clause
               (M_state (st-then statement) state classname instance) ; condition was true
               (M_state (st-else statement) state classname instance))))) ; condition was false

; M_state for finally blocks
(define M_state-finally
  (lambda (finally state classname instance)
    (cond
      ((null? finally) state)
      (else (M_state-finally (rest-statements finally) (M_state (next-statement finally) state) classname instance)))))

; M_state for catch blocks
(define M_state-catch
  (lambda (cbody finally cstate classname instance)
      (cond
        ((and (null? cbody) (null? finally)) cstate)
        ((null? cbody) (M_state-finally (cadr finally) cstate classname instance))
        (else (M_state-catch (rest-statements cbody) finally (M_state (next-statement cbody) cstate classname instance) classname instance)))))

; M_state for try blocks
(define M_state-try
  (lambda (body catch finally state classname instance)
    (other-layers
     (call/cc
      (lambda (break)
        (letrec ((loop (lambda (body catch finally state classname instance)
                         (cond
                           ((state-has-return? state) (break state))
                           ((state-has-thrown? state) ;(break (M_state-catch catch finally (state-add-bottom-thrown (operand1 (next-statement body)) state))))
                            (break
                               (let* ((cbody (catch-contents catch)) (cvar (catch-var catch)) (cstate (state-set cvar (state-get 'thrown state) (state-declare cvar state))))
                                 (M_state-catch (car cbody) finally cstate classname instance))));
                           ((null? body) (break (M_state-finally (cadr finally) state classname instance)))
                           ((eq? (operator (next-statement body)) 'throw) (break
                                                                           (let* ((cbody (catch-contents catch)) (cvar (catch-var catch)) (cstate (state-set cvar (operand1 (next-statement body)) (state-declare cvar state))))
                                                                              (M_state-catch (car cbody) finally cstate)))); (state-add-bottom-thrown (operand1 (next-statement body)) state)))))
                           (else (loop (rest-statements body) catch finally (M_state (next-statement body) state classname instance) classname instance))))))
          (loop body catch finally (add-layer state) classname instance)))))))

; M_value
(define M_value
  (lambda (l state classname instance)
    (cond
      ((number? l) l) ; input is a number
      ((eq? l 'true) 'true) ; input is the boolean value true
      ((eq? l 'false) 'false) ; input is the boolean value false
      ((symbol? l) (state-get l state)) ; input is a variable
      ; expressions
      ((eq? (operator l) '+) (mv-operate l state + classname instance))
      ((eq? (operator l) '-) (mv-operate l state - classname instance))
      ((eq? (operator l) '*) (mv-operate l state * classname instance))
      ((eq? (operator l) '/) (mv-operate l state quotient classname instance))
      ((eq? (operator l) '%) (mv-operate l state remainder classname instance))
      ; assignment or declaration with assignment
      ((eq? (operator l) '=) (M_value (dec-value l) state classname instance)) ; the value of an assignment is the value being assigned
      ((and (eq? (operator l) 'var) (has-value? l)) (M_value (dec-value l) state) classname instance) ; declaration must include assignment
      ; types of comparison
      ((eq? (operator l) '==) (mb-compare l state = classname instance))
      ((eq? (operator l) '>)  (mb-compare l state > classname instance))
      ((eq? (operator l) '>=) (mb-compare l state >= classname instance))
      ((eq? (operator l) '<)  (mb-compare l state < classname instance))
      ((eq? (operator l) '<=) (mb-compare l state <= classname instance))
      ((eq? (operator l) '!=) (mb-compare l state != classname instance))
      ; logical operations
      ((eq? (operator l) '&&) (mb-and l state classname instance))
      ((eq? (operator l) '||) (mb-or l state classname instance))
      ((eq? (operator l) '!) (mb-not l state))
      ; function calls
      ((eq? (operator l) 'funcall) (mv-function (funcall-name l) (funcall-args l) state classname instance))
      ((eq? (operator l) 'this) instance)
      ((eq? (operator l) 'super) (state-get-class (superclass (state-get-class (classname)))))
      ((eq? (operator l) 'new) )
      )))

; macros for statements
(define statement-type car) ; the type of statement (e.g. "if", "=", "return", ...)
(define dec-var cadr) ; the variable being assigned to in an assignment or declaration
(define dec-value caddr) ; the value being assigned in an assignment or declaration
(define return-val cadr) ; the value being returned in a "return" statement
(define condition cadr) ; the boolean condition being evaluated in an "if" or "else" statement
(define st-then caddr) ; the "then" statement in an "if"
(define st-else cadddr) ; the "else" statement in an "if"
(define while-body caddr) ; the body of a "while" statement
(define funcall-name cadr) ; for function calls
(define funcall-args cddr)
(define funcdec-name cadr) ; for function declarations
(define funcdec-formals caddr)
(define funcdec-text cadddr) ; the actual code run inside of a function

; macros for function closures
(define closure-formals car)
(define closure-text cadr)
(define closure-environment-function caddr)

; macros for value (prefix notation)
(define operator car)
(define arguments cdr)
(define argument1 cadr)
(define rest-arguments cddr)
(define operand1 cadr)
(define operand2 caddr)
(define trystmt2 caaddr)
(define next-statement car)
(define rest-statements cdr)
(define catch-contents cddr)
(define catch-var caadr)

(define try-part cadr)
(define catch-part caddr)
(define finally-part cadddr)

; shorthand for all those binary operator functions (and unary -)
(define mv-operate
  (lambda (l state func classname instance)
    (cond
      ((= (length l) 3) (mv-operate-binary l state func classname instance)) ; list has two operands
      ((= (length l) 2) (mv-operate-unary l state func classname instance)) ; list has one operand
      (else (error "Only binary and unary operations are supported")))))

(define mv-operate-binary
  (lambda (l state func classname instance)
    (func (M_value (operand1 l) state classname instance) (M_value (operand2 l) state classname instance))))

(define mv-operate-unary
  (lambda (l state func classname instance)
    (func (M_value (operand1 l) state classname instance))))

; shorthand for boolean comparisons
; (we cannot use mv-operate, because we want 'true and 'false rather than #t and #f)
(define mb-compare
  (lambda (l state func classname instance)
    (if (func (M_value (operand1 l) state classname instance) (M_value (operand2 l) state classname instance))
        'true
        'false)))

(define !=
  (lambda (a1 a2)
    (not (= a1 a2))))

; functions for boolean "and", "or", and "not"
(define mb-and
  (lambda (l state classname instance)
    (if (and (eq? (M_value(operand1 l) state classname instance) 'true) (eq? (M_value(operand2 l) state classname instance) 'true))
        'true
        'false)))

(define mb-or
  (lambda (l state classname instance)
    (if (or (eq? (M_value(operand1 l) state classname instance) 'true) (eq? (M_value(operand2 l) state classname instance) 'true))
        'true
        'false)))

(define mb-not
  (lambda (l state classname instance)
    (if (eq? (M_value(operand1 l) state classname instance) 'true)
        'false
        'true)))

;;;;;;;; function calls ;;;;;;;;;

; return the value of a function given the function name, actual params, and state
; (this is the umbrella function for this section)
; TODO make it so it only gets the part of the state that's in scope
(define mv-function
  (lambda (name args state classname instance)
    ((lambda (closure)
      (evaluate-call/cc (closure-text closure) (function-make-env closure args state)))
     (state-get name state))))

(define ms-function
  (lambda (name args state classname instance)
    (other-layers
     ((lambda (closure)
        (evaluate-state-call/cc (closure-text closure) (function-make-env closure args state)))
      (state-get name state)))))

; create the function closure from the function declaration and the given environment production function (which takes a state)
(define make-closure
  (lambda (declaration environment-function class-function)
    (append (cddr declaration) (list environment-function) (list class-function))))

; get the closure for a given function
(define lookup-closure
  (lambda (oexpr function-id environment)
    (if (list? oexpr)
        (lookup-closure-in-class-def function-id (state-get (true-type oexpr) environment)) ; oexpr is an object definition
        (lookup-closure-in-class-def function-id (state-get oexpr environment))))) ; oexpr is the name of a class

; get a function closure from a class definition
(define lookup-closure-in-class-def
  (lambda (function-id class-def)
    (state-get function-id (methods class-def))))

; bind actual params to formal params and include these bindings in a state containing all variables in scope (adding a new layer)
(define function-make-env
  (lambda (closure args state)
    (function-build-env (add-layer ((closure-environment-function closure) state)) args (closure-formals closure) state)))

; recursively bind a list of function arguments to their respective formal parameters
(define function-build-env
  (lambda (environment args formals state)
    (cond
      ((and (null? args) (null? formals)) environment)
      ((or  (null? args) (null? formals)) (error 'arguments "Wrong number of arguments provided"))
      (else (state-declare-and-set (car formals) (M_value (car args) state) (function-build-env environment (cdr args) (cdr formals) state))))))

; return a list of the formal parameters of a given function in a given state
(define function-formals
  (lambda (name state)
    (funcdec-formals (state-get name state))))

; ; ; ; ;
; CLASS ;
; ; ; ; ;
; This section contains functions which create, access, and modify class definitions

; a class definition is a list of the following in the given order:
; 1. the superclass name
; 2. a list of instance fields (in reverse-declared order)
; 3. a list (environment) of methods (names bound to closures)

(define empty-class-def (list '() '() empty-state))

; macros for accessing parts of a class declaration statement
(define class-dec-name cadr)
(define class-dec-super (lambda (x)
                          (if (pair? (caddr x))
                              (car (cdaddr x))
                              '())))
(define class-dec-body cadddr)

(define add-class-def
  (lambda (statement state)
    (state-add-class (class-dec-name statement)
                     (class-def-builder (class-dec-body statement)
                                        (class-dec-name statement)
                                        (list (class-dec-super statement) '() empty-state)
                                        (state-declare (class-dec-name statement) state))
                     state)))

(define class-def-builder
  (lambda (body class-name class-def state)
    (cond
      ((null? body) class-def)
      ((eq? (statement-type (next-statement body)) 'var) (class-def-builder
          (rest-statements body)
          class-name
          (add-field (next-statement body) class-def state)
          (state-set class-name (add-field (next-statement body)
                                              class-def state) state)))
      ((or (eq? (statement-type (next-statement body)) 'function) (eq? (statement-type (next-statement body)) 'static-function)) (class-def-builder
         (rest-statements body)
         class-name
         (add-function (next-statement body) class-name class-def state)
         (state-set class-name (add-function (next-statement body) class-name class-def state) state))) ; function definitions
      (else (error (statement-type (next-statement body)) "This construct is not supported in class definitions")))))

(define add-function
  (lambda (statement class-name class-def state)
    (if (or (eq? (statement-type statement) 'function) (eq? (statement-type statement) 'static-function))
        (list (superclass class-def) (instance-fields class-def)
              (state-declare-and-set (funcdec-name statement) (make-closure statement
                  (lambda (v) (state-get-bottom-n-layers (num-layers state) v))
                  (lambda (state) (state-get-class class-name state))) (methods class-def)))
        (error (statement-type statement) "Wrong type of statement"))))

(define add-field
  (lambda (statement class-def state)
    (if (eq? (statement-type statement) 'var)
        (list (superclass class-def) (cons (dec-var statement) (instance-fields class-def))
              (methods class-def))
        (error (statement-type statement) "Wrong type of statement"))))

; macros for accessing class definitions
(define superclass car)
(define instance-fields cadr)
(define methods caddr)

; ; ; ; ; ;
; OBJECT  ;
; ; ; ; ; ;
; Create, access, and modify object definitions

; an object definition is a list of the following in the given order:
; 1. the true type name
; 2. a list of instance field values (in reverse-declared order)

; add an object definition to a state
(define add-object-def
  (lambda (name true-type state)
    (state-declare-and-set name (construct-object true-type state) state)))

; construct and return an object definition from its declaration5
; (this is our generic constructor)
(define construct-object
  (lambda (true-type state)
    (object-def-builder (list true-type '()) (length (instance-fields (state-get-class true-type state))))))

(define object-def-builder
  (lambda (object-def num-field-values)
    (if (eq? num-field-values 0)
        object-def
        (object-def-builder (list (true-type object-def) (cons (box null) (instance-values object-def))) (- num-field-values 1)))))

; set an object's instance field by name
(define set-field
  (lambda (name val object state)
    (state-set object (state-set name val (make-field-environment object state)) state)))

(define make-field-environment
  (lambda (object state)
    (list (cons (instance-fields (state-get-class (true-type (state-get object state)) state))
          (list (instance-values (state-get object state)))))))

; macros for accessing parts of objects
(define true-type car)
(define instance-values cadr)

; THE DOT OPERATOR

(define function? ; is a function closure
  (lambda (x state)
    (and
      (not (pair? x))
      (list? (state-get x state))
      (eq? (length (state-get x state)) 3))))

(define class-def?
  (lambda (x state)
    (and
      (not (pair? x))
      (list? (state-get x state))
      (eq? (length (state-get x state)) 4))))

(define is-object?
  (lambda (x state)
    (and
      (not (pair? x))
      (list? (state-get x state))
      (eq? (length (state-get x state)) 2))))

; takes the left hand side of a dot operator and returns the current type
(define ldot-class-type
  (lambda (statement state classname instance)
    (cond
      ((function? (statement state)) (true-type (M_value statement state classname instance)))
      ((is-object? (statement state)) (true-type (M_value statement state classname instance)))
      ((class-def? (statement state)) #f) ; TODO
      ((eq? (operator statement) 'new) (operand1 statement))
      ((eq? (operator statement) 'this) classname)
      ((eq? (operator statement) 'super) (superclass instance))
      ((state-get (statement state)) (true-type (state-get (statement state))))
      (else (error 'unknown "Can't use the dot operator on this type")))))

(define ldot-contents
  (lambda (statement state classname instance)
  (cond
    ((function? (statement state)) (M_value statement state classname instance))
    ((is-object? (statement state)) (M_value statement state classname instance))
    ((class-def? (statement state)) (statement state)) ; TODO
    ((eq? (operator statement) 'new) (M_value statement state classname instance))
    ((eq? (operator statement) 'this) (M_value statement state classname instance))
    ((eq? (operator statement) 'super) (M_value statement state classname instance))
    ((state-get (statement state)) (state-get (statement state)))
    (else (error 'unknown "Can't use the dot operator on this type")))))

;An object. Can come from...
;A variable / field
;A method call
;new
;this
;A class name
;Super


; ; ; ; ;
; STATE ;
; ; ; ; ;
; This section contains functions which modify and examine states

(define new-layer '(()()))

(define sample-state (list (list (list 'x 'z)(list (box 2) (box 4)))(list (list 'y 'a)(list (box 3) (box 5)))))

(define add-layer (lambda (state) (cons new-layer state)))
(define top-layer car)
(define other-layers cdr)
(define num-layers length)

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

(define state-declare-and-set
  (lambda (name value state)
    (state-set name value (state-declare name state))))

; used for static scoping
(define state-get-bottom-n-layers
  (lambda (n state)
    (cond
      ((< (num-layers state) n) (error n "Not enough layers in the given state"))
      ((= (num-layers state) n) state)
      (else (state-get-bottom-n-layers n (other-layers state))))))

(define state-add-bottom-return
  (lambda (value state)
    (cond
      ((null? (other-layers state)) (list (layer-add-return value (top-layer state))))
      (else (cons (top-layer state) (state-add-bottom-return value (other-layers state)))))))

(define state-add-bottom-break
  (lambda (state)
    (cond
      ((null? (other-layers state)) (list (layer-add-break (top-layer state))))
      (else (cons (top-layer state) (state-add-bottom-break (other-layers state)))))))

(define state-add-bottom-continue
  (lambda (state)
    (cond
      ((null? (other-layers state)) (list (layer-add-continue (top-layer state))))
      (else (cons (top-layer state) (state-add-bottom-continue (other-layers state)))))))

(define state-add-bottom-thrown
  (lambda (value state)
    (cond
      ((null? (other-layers state)) (list (layer-add-thrown value (top-layer state))))
      (else (cons (top-layer state) (state-add-bottom-thrown value (other-layers state)))))))

(define state-has-return?
  (lambda (state)
    (cond
      ((null? state) #f)
      ((layer-has-return? (top-layer state)) #t)
      (else (state-has-return? (other-layers state))))))

(define state-has-break?
  (lambda (state)
    (cond
      ((null? state) #f)
      ((layer-has-break? (top-layer state)) #t)
      (else (state-has-break? (other-layers state))))))

(define state-has-continue?
  (lambda (state)
    (cond
      ((null? state) #f)
      ((layer-has-continue? (top-layer state)) #t)
      (else (state-has-continue? (other-layers state))))))

(define state-has-thrown?
  (lambda (state)
    (cond
      ((null? state) #f)
      ((layer-has-thrown? (top-layer state)) #t)
      (else (state-has-thrown? (other-layers state))))))

(define state-remove-break
  (lambda (state)
    (cond
      ((null? state) (error "The state contains no break indicator."))
      ((layer-has-break? (top-layer state)) (list (layer-remove-break (top-layer state))))
      (else (cons (top-layer state) (state-remove-break (other-layers state)))))))

(define state-remove-continue
  (lambda (state)
    (cond
      ((null? state) (error "The state contains no continue indicator."))
      ((layer-has-continue? (top-layer state)) (list (layer-remove-continue (top-layer state))))
      (else (cons (top-layer state) (state-remove-continue (other-layers state)))))))

; getter and setter for class definitions stored on the bottom layer of an environment
(define state-add-class
  (lambda (cname class state)
    (cond
      ((null? state) (error "State has no global layer"))
      ((null? (other-layers state)) (list (layer-set cname class (layer-declare cname (top-layer state)))))
      (else (cons (top-layer state) (state-add-class cname class (other-layers state)))))))

(define state-get-class
  (lambda (cname state)
    (cond
      ((null? state) (error "State has no global layer"))
      ((null? (other-layers state)) (layer-get cname (top-layer state)))
      (else (state-get-class cname (other-layers state))))))

; ; ; ; ;
; LAYER ;
; ; ; ; ;
; This section contains functions which modify and examine layers

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
      ((eq? name (car (names layer))) (error (car (names layer)) "Variable is already declared"))
      (else (layer-cons (layer-car layer) (layer-declare name (layer-cdr layer)))))))

; creates a name-value pair in a given layer that represents the return value of the layer
(define layer-add-return
  (lambda (value layer)
    (list
     (cons 'return (names layer))
     (cons (box value) (vals layer)))))

(define layer-add-thrown
  (lambda (value layer)
    (list
     (cons 'thrown (names layer))
     (cons (box value) (vals layer)))))

; creates a name-value pair in a given layer that represents a break
(define layer-add-break
  (lambda (layer)
    (list
     (cons 'break (names layer))
     (cons (box '()) (vals layer)))))

; creates a name-value pair in a given layer that represents a break
(define layer-add-continue
  (lambda (layer)
    (list
     (cons 'continue (names layer))
     (cons (box '()) (vals layer)))))

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
