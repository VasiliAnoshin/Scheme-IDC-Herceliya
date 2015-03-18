;Exercise 8 321464935

;Part1
(define (get-weight items)
  (if(null? items)
     0
  (+ (caar items) (get-weight (cdr items)))))

(define (get-values items)
  (if (null? items)
      0
      (+ (cadar items) (get-values (cdr items)))))

; Part 2
(define (knapsack1 items capacity)
  ;Sort
  (define (SortValuesInDecreasingOrder listOfValuesToSort)
   (sort listOfValuesToSort (lambda (x y) (> (/ (cadr x) (car x)) (/ (cadr y) (car y))))))
  
  ;Fill the sack with appropriae values
  (define (comuteAnswer items maxWeight)
    (if (null? items)
        ()
    (if (>= maxWeight (caar items))
        (cons (car items) (comuteAnswer (cdr items) (- maxWeight (caar items))))
        (if (> maxWeight 0)
            (comuteAnswer (cdr items) maxWeight)
            ()
        ))))  
  
  (let* ((sortedValues (SortValuesInDecreasingOrder items)))
    (if (null? items) ; if items is empty list 
        ()
        (comuteAnswer sortedValues capacity)
  )))

;Time complexity : Sort the input : O(nlogn)


;Part 3 
(define (knapsack2 items capacity optimization-type)
    
  (define (guessSolution listOfItems currentSack capacityOfTheSack)
    (if (or (null? listOfItems) (= capacityOfTheSack 0))
        currentSack
        (let ((currentListItem (car listOfItems)))
          ; check if we have free space in the sack
          (if (>= capacityOfTheSack (car currentListItem))
            ;Divide the problem in two parts : First part include the solution that check what will be list of appropriate items 
              ; with given element , and the second check what the result without given element. Thats implementing backtracing.    
              (let ((guessIncludeResult (guessSolution (cdr listOfItems) (cons currentListItem currentSack) (- capacityOfTheSack (car currentListItem))))
                  (guessNotIncludeResult (guessSolution (cdr listOfItems) currentSack capacityOfTheSack)))           
              ;Check which optimization-type is it . 
              (if (equal? optimization-type 'weight)   
                  ;Check  which solution is more optimal             
                  (if (> (length guessIncludeResult) (length guessNotIncludeResult))
                      guessIncludeResult
                      guessNotIncludeResult)
           
                  (if (> (get-values guessIncludeResult) (get-values guessNotIncludeResult))
                      guessIncludeResult
                      guessNotIncludeResult)))
           
              (guessSolution (cdr listOfItems) currentSack capacityOfTheSack)))))
;Do not really need reverse here , only for getting the same result from example of the question  
  (reverse (guessSolution items () capacity))
)