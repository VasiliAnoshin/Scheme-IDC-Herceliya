;Exercise 6 
;321464935


;Part 1 – Multi-Dict

;A:
;The internal representation of Multi-Dict can be in this form : ((a 1 2)(b 4)(c 7)) 
;that is list of lists and each of them represented as key . value
 
;B:
(define (multidict-get key dict)
  (if (null? dict)
      #f
  (if (equal? (caar dict) key)
      (cdar dict)
      (multidict-get key (cdr dict)))
  ))
;C:(define (multidict-remove key dict)
(define (multidict-remove key dict)  
  (define (helper dict listWithoutGivenKey)
   ;if dictionary is null return list with the values without given value
    (if (null? dict)
        listWithoutGivenKey
        (if (equal? (caar dict) key)
            (helper (cdr dict) listWithoutGivenKey) 
            (helper (cdr dict) (append listWithoutGivenKey (list(car dict)))))))
  (helper dict ()))
;D
(define (multidict-put key value dict)
 (let ((existedValues (multidict-get key dict)))
   (if existedValues      
       (append (list (append (list key) existedValues value)) (multidict-remove key dict))
       (append  dict (list(append(list key) value)))                 
       )))

;Part 2 Simple Streams : 
;A:
(define (generate-even-stream) 
  (define (helper init)
    (stream-cons init (helper (+ init 2))))
  (helper 2))

;B:
(define (generate-fibo)
  (define (helper init numb)
    (stream-cons init (helper numb (+ init numb))))
  (helper 1 1))

;Part 3:
;A
(define (list-to-stream lst)
    (if (= (length lst) 0)
        (stream-cons lst ())
        (stream-cons (car lst) (list-to-stream (cdr lst))))
    )
;B:
(define (list-to-infinite-stream lst)
  (define (helper streamList constValue)
     (if (= (length streamList) 0)
         (helper constValue constValue)
         (stream-cons (car streamList) (helper (cdr streamList) constValue)) 
         ))
  (helper lst lst))

;4
(define (stream-comp action baseStream . conditions)  
  (define (helper action stream conds)  
    
    (define (CheckIfNumberisValid value condition)    
      (if (null? condition)
        #t
        (if ((car condition) value)
          (CheckIfNumberisValid value (cdr condition))           
           #f)
        ))
  ;If conditions is null  
  (if (null? conditions)  
      (stream-cons (action (stream-car stream)) (helper action (stream-cdr stream) conds))  
  ;else If conditions is not null
  (if (CheckIfNumberisValid (stream-car stream) conds)                  
        (stream-cons (action (stream-car stream)) (helper action (stream-cdr stream) conds))        
        (helper action (stream-cdr stream) conds))
  ))
  (helper action baseStream conditions))
