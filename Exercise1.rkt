;Functional and Logical Programming 2014 - Exercise 1 
;ID : 321464935
;Exercise 1 :
; A) I   infix notation   ((300 + 11) + (8 * 9)) 
;    II  infix notation   (((1 + 2) * 8) + (33 / 11))
;    III infix notation   (((((((7 * 6) * 8) * 5) * 4) * 3) * 2) * 1) 

; B) I   prefix notation  (+ 5(- (/ 8 2)(* 8 9)))
;    II  prefix notation  (* (+ (+ 8 4) 5)(/ (+ 9 7)(* (* 8 7) 6))) 
;    III prefix notation  (- 2(/ (+ (* 8 9) 4) 4))

;Exercise 2: A 
(define positiveOdd (lambda (x)
  (if (and (= (modulo x 2) 0)(>= x 0))
      'yes
      (quote no))))

;Exercise 2: B
(define circle-area (lambda (r)
      (* pi(* r r))))

;Exercise 3: A
(define someSequence (lambda (n)
         (if (> n 1)
            (+ (someSequence (- n 1)) (expt (* n 2) n))
             1)))

;Exercise 4:
;Internal function iFibo that compute fibbonacci result
(define iFibo (lambda (n)
                 (if(<= n 2)
                    1                  
                  (+ (iFibo (- n 1)) (iFibo (- n 2)))                                 
                  )))
(define fibo (lambda (n)
                (cond                   
                  ((>= n 1)
                   (begin
                     (fibo (- n 1))
                     (display (iFibo n ))
                     (newline)
                     )))))                     