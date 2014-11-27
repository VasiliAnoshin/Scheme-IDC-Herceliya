;Functional and Logical Programming 2014 - Exercise #2 
;ID : 321464935

; Part 1 :  A (The function returns a list in which only the first element, 
;              from each one of the lists in lists appears)
(define (cars lists)
  (if (null? lists)      
      ()       
      (cons (caar lists)
            (cars (cdr lists)))))
         
;Part 1 : B (The function returns a list in which only the remaining of the lists,
;            without the first elements from each one of the lists in lists appears, according to the order in lists.)
(define (cdrs lists)
   (if(null? lists)
      ()
      (cons (cdar lists)
            (cdrs (cdr lists))) 
  ))

;Part 2 : A 
(define (quick-sort lst)
  ; Take the first element of the list
  (define (pivot lst)
    (if (or (null? lst) (= 1 (length lst)))
        'done
        (car lst)
  ))
  ; Function Split recieve a list and a pivot 
  (define (split lst pivot)
    (define (helper lst l1 l2)                
          (cond
            ( (null? lst)  
                (cons l1 l2))
            ( (>= pivot (car lst))
                (helper (cdr lst) (append (list (car lst)) l1) l2))                                            
            ( else 
                (helper (cdr lst) l1 (append (list (car lst)) l2)))))
  (helper lst () ()))
  ; QuickSort itself         
    (if (equal? (pivot lst) 'done)
        lst       
       (append (quick-sort( car (split lst (pivot lst) ))) (quick-sort (cdr (split lst (pivot lst)))))                 
   ))

;Part 2 : B 
(define (quick-sort-with-pivot lst)
  ; Take the median element of the list
  (define (pivot lst)
    (if (or (null? lst) (= 1 (length lst)))
        'done
        (if (even? (length lst))
            (list-ref lst ( / (length lst) 2) )
            (list-ref lst ( /(- (length lst) 1) 2) )
  )))
  
  ; Function Split recieve a list and a pivot 
  (define (split lst pivot)
    (define (helper lst l1 l2)                
          (cond
            ( (null? lst)  
                (cons l1 l2))
            ( (>= pivot (car lst))
                (helper (cdr lst) (append (list (car lst)) l1) l2))                                            
            ( else 
                (helper (cdr lst) l1 (append (list (car lst)) l2)))))
  (helper lst () ()))
  ; QuickSort itself         
    (if (equal? (pivot lst) 'done)
        lst       
       (append (quick-sort( car (split lst (pivot lst) ))) (quick-sort (cdr (split lst (pivot lst)))))                 
   ))

;Part 3 A:
;The function receives a number and returns the number of bits that would have been required 
;to be “on” in order to represent the input number in binary base.
(define (numOfBitsOn number)
  (define (helper iNumber sum)
     (let ((iRem (remainder iNumber 2)))
      (if (= iNumber 0)
         (+ sum iRem)
         (helper (quotient iNumber 2)(+ sum iRem))
         )))
  (helper number 0))
;Part 3 B:
;The function returns the estimated square root of n. Delta defines how ‘good’ does the estimate have to be
;Use binary search to find the square root, using the following algorithm:
(define (findSqrt n delta)
  (define (helper guessNumber min max)
    (if (< (abs (- (expt guessNumber 2) n)) delta)
        guessNumber
        (if (< (- (expt guessNumber 2) n) 0)
            (helper (/ (+ guessNumber max) 2) guessNumber max)
            (helper (/ (+ guessNumber min) 2) min guessNumber)))
    )
  (helper (/ n 4) 0 (/ n 2)))

;Part 3 : C
;Implement cars-tail as tail recursion version of cars from part 1.
(define (cars-tail listsOfLists)
  ;Define Help Function that recieve List of ListNumbers && RequiredList (null in the first iteration)
  (define (tailRecursion listsOfLists requiredList)
  (if (null? listsOfLists)
      ;If List == null we reach the end of the tail recursion , return requiredList of Car
       requiredList       
       (tailRecursion (cdr listsOfLists) (append  requiredList (list (car (car listsOfLists)))))))
  (tailRecursion listsOfLists ()))

;Part 3 : D
(define (cdrs-tail listsOfLists)
  ;Define Help Function that recieve List of ListNumbers && RequiredList (null in the first iteration)
  (define (tailRecursion listsOfLists requiredList)
  (if (null? listsOfLists)
       ;If List == null we reach the end of the tail recursion , return requiredList of Cuders
       requiredList       
       (tailRecursion (cdr listsOfLists) (append  requiredList (list (cdr (car listsOfLists)))))))
  (tailRecursion listsOfLists ()))            