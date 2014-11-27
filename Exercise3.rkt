;Functional and Logical Programming 2014 - Exercise #3 
;ID : 321464935

;Part 1 QuickSort 
;lst is the list of numbers to be sorted
;pred is the predicate by which the list is ordered, the signature of this predicate is:

(define (quick-sort pred lst)
  ;Select pivot to be a median of the list
  (define (pivot lst)
    (if (or (null? lst) (= 1 (length lst)))
        'done
        (if (even? (length lst))
            (list-ref lst ( / (length lst) 2) )
            (list-ref lst ( /(- (length lst) 1) 2) )
  )))
    (if (null? lst)
        ()       
        (if (= (length lst) 1)  
            lst
        (let* ((iPivot (pivot lst))             
              ;filter will take to right  && left side all elements from the given list that satisfy condition of predicate 
              (rightSide (filter (lambda (x) (pred iPivot x)) lst))
              (leftSide  (filter (lambda (x) (pred x iPivot)) lst)))
        ;after than we recieve two lists(left from the pivot and right) and pivot itself ,apply to each side quick-sort and append in the end. 
        (append (quick-sort pred leftSide)(cons iPivot (quick-sort pred rightSide)))))))

;Part 2 Factories : A
(define (do2add lst)
  (if(null? lst)
     ()
     (cons (+ (car lst)(cadr lst)) (do2add (cddr lst)))))

;B The function will apply F on each two consecutive elements
;  F signature is (lambda (x y) ..)

(define (do2F F lst)
  (if (null? lst)
      '()
      (cons (F (car lst) (cadr lst)) (do2F F (cddr lst)))))

;C  function (makeDo2F F) that gets F as a parameter and returns a function 
;   with the parameter lst that applies F on each two consecutive elements of lst
(define (makeDo2F F)
  (lambda(lst) (do2F F lst)))

;D 
;I: do2addFactory 
(define do2addFactory (makeDo2F + ))
;II do2mult 
(define do2Mult (makeDo2F *))
;III do2eq? 
(define do2eq (makeDo2F equal?))
;IV do2eq1st 
(define do2eqlst (makeDo2F (lambda (x y) (equal? (car x) (car y)))))

;Part3 
(define (makeDo2FM F)
  ;the help function recieve list and predicate and apply condition of predicate on the rest of the list. 
  (define (helper F z)
    (if (null? z)
      '()
       (cons (F (car z) (cadr z)) (helper F (cddr z)))))
  (lambda (x y . z) (cons(F x y)(helper F z))))
  

(define do2MultM (makeDo2FM *))
