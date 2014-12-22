; Functional and Logical Programming - Exercise 7
; ID : 321464935
; ID : 300800257

(require (lib "trace.rkt"))

; Global interpreter constants
(define CONTEXT_TYPE 'static) ; can be either 'static or 'dynamic
(define PRINT_MACRO_EXPANSION #f)

; Bonus constant - change to #t if you implement the bonus. Keep on #f otherwise.
(define SWITCH_IP_ENABLED #f)

; ********************************************************************************************
; * Do not change anything in the code below, until where marked 'Your code should be here'. *
; * You may only change value of user-definitions if you do part 7.                          *
; ********************************************************************************************

; Special keywords - special forms that are implemented in the interpreter
(define special-keywords '(() #t #f lambda nlambda macro if eval apply))

; Primitive functions - functions that are used as primitives from the Dr. Racket interpreter
(define primitive-functions '(+ - * / < > <= >= <> = eq? equal? null? pair? cons car cdr))

; System context - contains default system functions (non-primitive) and constants - Can add more here
(define system-definitions '((pi 3.14159265358979)
                             (list (lambda x x))
                             (quote (nlambda (x) x))
                             (caar (macro (p) (list 'car (list 'car p))))
                             (cadr (macro (p) (list 'car (list 'cdr p))))
                             (cadar (macro (p) (list 'car (list 'cdr (list 'car p)))))
                             (cond-make-conds (lambda (conds)
                                                (if (null? conds)
                                                    ()
                                                    (if (eq? 'else (caar conds))
                                                        (cadar conds)
                                                        (list 'if (caar conds) (cadar conds)
                                                              (cond-make-conds (cdr conds)))))))
                             (cond (macro conds (cond-make-conds conds)))
                             (map (lambda (pred lst)
                                    (if (null? lst) ()
                                        (cons (pred (car lst)) (map pred (cdr lst))))))
                             (append (lambda (lst1 lst2)
                                       (if (null? lst1) lst2
                                           (cons (car lst1) (append (cdr lst1) lst2)))))
                             (let (macro (defs body) 
                                         (append (list (list 'lambda (map car defs) body))
                                                 (map cadr defs))))
                             ))

; User context - contains user functions (non-primitive) and constants - Can add more here
(define user-definitions '((first (macro (lst) (list 'car lst)))
                           (second (macro (lst) (list 'car (list 'cdr lst))))
                           (third (macro (lst) (list 'car (list 'cdr (list 'cdr lst)))))
                           (fourth (macro (lst) (list 'car (list 'cdr (list 'cdr (list 'cdr lst))))))
                           ; ***********************
                           ; * Add bonus code here *
                           ; ***********************
                           ))

; Makes a context out of a given list of definitions
(define (make-context dict)
  (if (null? dict) ()
      (dict-put (caar dict) (evaluate (cadar dict) ()) (make-context (cdr dict)))))

; Runs user code with an empty initial context
(define (run-code expr)
  (evaluate expr ()))

; Shows a prompt to the user to enter his code to run
(define (show-prompt-loop)
  (display "Enter an expression (type 'exit' to stop):")
  (newline)
  (let ((exp (read)))
    (if (not (eq? exp 'exit))
        (let ((result (run-code exp)))
          (if (not (eq? result (void)))
              (begin
                (display result)
                (newline)))
          (show-prompt-loop)))))

; Dictionary management (from class)
(define (dict-put key value ctx)
  (cons (list key value) ctx))

(define (dict-put-many entries ctx)
  (append entries ctx))

(define (dict-get key ctx)
  (let ((res (assoc key ctx)))
    (if res (cadr res) '_value_not_in_dict)))

; ***************************************************************************************
; ********************************* Add your code here! *********************************
; ***************************************************************************************

;Part:1
(define (eval-args args ctx)
  (map (lambda (arguments) (evaluate arguments ctx)) args))

;Part2:
(define (bind params args)
  (if (null? params) ()      
  (if (pair? params)
      ;If params is a list
      (cons (list (car params)(car args)) (bind (cdr params)(cdr args)))
      ;If params is a symbol (i.e it can be the cdr ( .x) or 'x)
      (list (list params args)) 
  )))

;Part 3
(define (eval-symbol sym ctx)
  ;If symbol represent a prymitive function
  (if (member sym primitive-functions)
      (list (quote _primitive) (eval sym))
  ;If symbol represent one of the special keywords
  (if (member sym special-keywords)
      sym
  ;if sym represent a value in the context hierarhy
  (if  (NOT (equal? (dict-get sym  ctx) (quote _value_not_in_dict))) 
       (dict-get sym  ctx)       
       (if (NOT (equal? (dict-get sym  user-context) (quote _value_not_in_dict)))
           (dict-get sym  user-context)
           (if (NOT (equal? (dict-get sym  system-context) (quote _value_not_in_dict)) ) 
               (dict-get sym  system-context)
                (error “reference to undefined identifier”)))       
  ))))

;Part 4
(define (eval-if condition if-true if-false ctx)
  (if (evaluate condition ctx) 
      (evaluate if-true ctx)
      (evaluate if-false ctx)))

;Part 5
;A
(define (exec-func func args ctx)
  (if (equal? (first func) '_primitive)
      (apply (second func)(eval-args args ctx))
      (exec-user-func func args ctx)
  ))
;B
(define (exec-apply func args-list ctx)
    (evaluate (cons func (evaluate args-list ctx)) ctx))
;C
(define (exec-user-func func args ctx)
  (let* ((context (if (eq? CONTEXT_TYPE 'static) (fourth func)
                     ctx))
        ;if given type of func is Lambda eval arguments before binding ,
        ;  if it's not bind arguments without eval args.
         (givenFunc (if (eq? (first func) '_user_lambda)
                        (let* ((arguments (eval-args args ctx)))
                          (bind (second func) arguments))
                        (bind (second func) args)))
      (newContext (dict-put-many givenFunc context)))
    (cond ((eq? (first func) '_user_lambda) (evaluate (third func) newContext))
          ((eq? (first func) '_user_nlambda)(evaluate (third func) newContext))
          ((eq? (first func) '_user_macro)
           (let (( macroResultAfterEvals (evaluate (third func) newContext)))
            ;Produce makro answer
             (if PRINT_MACRO_EXPANSION
                (begin 
                  (display "Macro expansion from :")
                  (display (third func))
                  (display "To")
                  (display macroResultAfterEvals)
                  (display "Evaluation result :")
                  (display (evaluate macroResultAfterEvals ctx))))                                  
             (evaluate macroResultAfterEvals ctx))))
    ))
;Part 6 Evaluate func
(define (evaluate exp ctx)
  (cond ((symbol? exp) (eval-symbol exp ctx))               
        ((pair? exp)
         ;evaluating the first element of the list
         (let ((firstElement (evaluate (first exp) ctx)))
           ;check if the first element of the list is: Macro/Lambda/If expression/apply/eval/nlambda/ 
           (cond ((eq? firstElement 'lambda) (list '_user_lambda (second exp) (third exp)ctx))
                 ((eq? firstElement 'nlambda)(list '_user_nlambda (second exp) (third exp) ctx))
                 ((eq? firstElement 'macro)  (list '_user_macro (second exp) (third exp) ctx))
                 ((eq? firstElement 'if )(eval-if (second exp) (third exp) (fourth exp) ctx))
                 ((eq? firstElement 'eval) (evaluate (evaluate (second exp) ctx) ctx))
                 ((eq? firstElement 'apply)(exec-apply (second exp) (third exp) ctx))                 
                 ;When evaluation of first element does not return a symbol 
                 (else (exec-func firstElement (cdr exp) ctx)))))                         
        ;if this primitive value return it as is 
        (else exp)))

; ***************************************************************************************
; *           The following lines should appear at the end, BELOW your code!            *
; *                            Do NOT change the code below                             *
; ***************************************************************************************

; Initially create system context
(define system-context (make-context system-definitions))

; Initially create user context
(define user-context (make-context user-definitions))

; A macro that is used to test the behavior of the interpreter
(defmacro assert (expr result)
  `(if (not (equal? ,expr ,result))
       (error "Assertion failed: " (quote ,expr))))

; Basic tests
(assert (run-code 7) 7)
; Primitive function execution and recursive evaluation
(assert (run-code '(+ 1 2)) 3)
(assert (run-code '(+ (- 2 1) 2)) 3)
(assert (run-code '(+ 1 (* 1 2))) 3);
; Lambda execution test
(assert (run-code '((lambda (x y z) (+ x y z)) 1 2 3)) 6)
; Bind test
(assert (run-code '((lambda (x . z) (cons x z)) 1 2 3)) '(1 2 3))
(assert (run-code '((lambda x x) 1 2 3)) '(1 2 3))
; nLambda + eval test
(assert (run-code '(let ((a 1)) ((nlambda (x) (+ (eval x) 1)) a))) 2)
; macro test
(assert (run-code '(let ((a 1)) ((macro (x) (list (quote +) x 1)) 1))) 2)
; IF test
(assert (run-code '(let ((x 2)) (if (< x 3) 'small 'large))) 'small)
(assert (run-code '(let ((x 4)) (if (< x 3) 'small 'large))) 'large)
; Bonus test
(if SWITCH_IP_ENABLED
    (assert (run-code 
             '(let ((my-ip '(194 90 181 27)))
                      (switch-ip my-ip
                                 ((129 117) 'bezeq-international)
                                 ((194 90) 'netvision)
                                 ((85 44 2) 'zahav-012)
                                 ((37 142 198) 'hot-net)
                                 (default (display "Unknown IP address"))))) 'netvision))