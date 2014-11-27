;Functional and Logical Programming 2014 - Exercise #4 
;ID : 321464935

;Part 1 A:
;The macro should expand to code that evaluates to area of a circle with the radius r. (If
;you do not remember, the area of a circle with radius r is π*r*r)
;Use the predefined value pi for π value
(defmacro circle-area (r)
  `(let ((radius ,r))
    (if(null? radius)
       (display '(radius is undefined))
          (* pi (sqr radius)))))

;B The macro should expand to code that evaluates the minimal value among x and y.
(defmacro min (x y)
  `(let ((firstValue ,x)
         (secondValue ,y))
     (if(> firstValue  secondValue)
         secondValue
         firstValue)))

;Part 2 A:  The function (test-ip ip prefix) receives an IP address ip and a prefix
;prefix (using the defined lists representation), and returns #t if the IP address
;belongs to the network represented by the prefix or #f otherwise.
(define (test-ip ip prefix)
  (define (checkForValidIp ip_Adress )
    (if (null? ip_Adress)
         ()
         (if (OR (> (car ip_Adress) 255) ( < (car ip_Adress) 0))
             #f
             (checkForValidIp (cdr ip_Adress)))))
  
  (define (checkIfPrefixBelongsToNetwork ip_Adress i_Prefix)
    (if (null? i_Prefix)
        #t
        (if(NOT (equal? (car ip_Adress)(car i_Prefix)))
           #f
           (checkIfPrefixBelongsToNetwork (cdr ip_Adress) (cdr i_Prefix)))))
         
  (let ((ipAdress ip )
        (iPrefix prefix ))
  (if (NOT (equal? (length ip) 4))
      (display `(entered ,ip is not valid))
      (if(equal? (checkForValidIp ipAdress) #f)
         (display `(entered ip adress: ,ip is not in valid form please reenter))
         (if (null? iPrefix)
             (display '(u have not entered a prefix adress fix it))
         (if (equal? (checkIfPrefixBelongsToNetwork ipAdress iPrefix) #t)
             #t
             #f))))))

;Part 2 B : The function factory (make-ip-filter prefix) creates a function for filtering
;of IP addresses lists. A filter function receives a list of IP addresses and returns a list
;with only the IP addresses that match the prefix with which the filter was created.
(define (make-ip-filter prefix)
  (define (helper list)
    (if(null? list)
       ()
       (if (equal? (test-ip (car list) prefix) #t)
        (cons (car list)(helper (cdr list)))
        (helper(cdr list)))))
  
  (lambda (list)
    (if (null? list)
        ()
        (if (equal? (test-ip (car list) prefix) #t)
            (cons (car list) (helper (cdr list)))
        ()))))
;two function for check if everything working 
(define local-network-filter (make-ip-filter '(10 0 0)))
(define local-network-filter2 (make-ip-filter '(192 168)))

;Part 2 C : 
 (defmacro switch-ip (ip . cases)
   (define (expandCase case)
     (if (equal? (car case) 'default)
         `(else ,(cadr case))
         `((equal? (test-ip value '(,@(car case))) #t)
           ,(cadr case)))
     )
  `(let ((value ,ip))
     (cond ,@(map expandCase cases)))
  )
;You can use this example for check the the exercise 2c
(let ((my-ip '(194 90 181 27)))
(switch-ip my-ip
           ((129 117) 'bezeq-international)
           ((194 90) 'netvision)
           ((85 44 2) 'zahav-012)
           ((37 142 198) 'hot-net)
           (default (display "Unknown IP address"))))
;Part3 - Boolean Logic Define XOR: 
(defmacro xor (value . list)
  (define (helper counter args)      
    (if (null? args)       
         (if (odd? counter )
              #t
              #f)
        `(let ((val ,(car args)))
         (if  val
              ,(helper (+ counter 1) (cdr args))
              ,(helper counter (cdr args)))        
  )))
(helper 0 (cons value list)))

;Examples for check 3A
(not (let ((x 10)) (xor (< x 20) (> x 10) #f)))
(let ((x 10)) (xor (< x 10) #f))
(xor #t (display "This message should be printed"))
(let ((x 10)) (xor (> x 15) (< x 30) (display "This message should be printed")))

;3B DEFINE NAND
(defmacro nand (value . list)
  (define (helper args)
    (if (null? args)
          #f
        `(let ((val ,(car args)))
         (if  val 
              ,(helper (cdr args))
              ,#t)        
  )))
(helper (cons value list)))
;Examples for 3B
(not (let ((x 10)) (nand (< x 20) (> x 0) #t)))
(let ((x 10)) (nand (< x 20) (> x 0) #f))
(nand #f (display "This message should never be printed"))
(let ((x 10)) (nand (> x 0) (< x 10) (display "This message should never be printed"))) 
