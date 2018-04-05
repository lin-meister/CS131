; Return #t if obj is an empty listdiff, #f otherwise.
(define (null-ld? obj)
  (eq? (car obj) (cdr obj))
)

; Helper function for ld
(define (ld-helper tail lst)
  (cond [(eq? tail lst)
          #t
        ]
        [(not (pair? lst))
          #f
        ]
        [else 
          (ld-helper tail (cdr lst))
        ]
  )
)

; Return #t if obj is a listdiff, #f otherwise.
(define (ld? obj)
  (and (pair? obj) (ld-helper (cdr obj) (car obj)))
)

; Return a listdiff whose first element is obj and whose remaining elements are listdiff. 
; (Unlike cons, the last argument cannot be an arbitrary object; it must be a listdiff.)
(define (cons-ld obj listdiff)
  (cons (cons obj (car listdiff)) (cdr listdiff))
)

; Return the first element of listdiff. It is an error if listdiff has no elements.
(define (car-ld listdiff)
  (cond [(not (ld? listdiff))
          (display "; car-ld: invalid listdiff\n")
        ]
        [(null-ld? listdiff) 
          (display "; car-ld: listdiff contains no elements\n")
        ]
        [else 
          (caar listdiff)
        ]
  )
)

; Return a listdiff containing all but the first element of listdiff. 
(define (cdr-ld listdiff)
  (cond [(not (ld? listdiff))
          (display "; cdr-ld: invalid listdiff\n")
        ]
        [(null-ld? listdiff) 
          (display "; cdr-ld: listdiff contains no elements\n")
        ]
        [else 
          (cons (cdar listdiff) (cdr listdiff))
        ]
  )
)

; Return a newly allocated listdiff of its arguments.
(define ld (lambda obj (cond [(empty? obj) '(())] 
                             [else (cons obj empty)]
                       )
           )
)

(define (get-diff-helper tail lst)
  (if (or (empty? lst) (eq? tail lst))
    empty
    (cons (car lst) (get-diff-helper tail (cdr lst)))
  )
)

; Gets the difference representd by a listdiff
(define (get-diff listdiff)
  (get-diff-helper (cdr listdiff) (car listdiff))
)

; Return the length of listdiff.
(define (length-ld listdiff)
  (if (not (ld? listdiff)) 
        (display "; length-ld: invalid listdiff\n")
        (length (get-diff listdiff))
  )
)

; Merges multiple listdiffs together into one listdiff
(define (merge-listdiffs lds)
  (cond [(empty? (cdr lds)) 
          empty
        ]
        [(null-ld? (car lds)) 
          (merge-listdiffs (cdr lds))
        ]
        [else 
          (cons (car-ld (car lds)) (merge-listdiffs (cons (cdr-ld (car lds)) (cdr lds))))
        ]
  )
)

; Return a listdiff consisting of the elements of the first listdiff followed by the elements of the other listdiffs. 
; The resulting listdiff is always newly allocated, except that it shares structure with the last argument. 
; (Unlike append, the last argument cannot be an arbitrary object; it must be a listdiff.)
(define append-ld (lambda lds (cond [(empty? lds) '(())] 
                                    [else (let ([lastdiff (car (reverse lds))])
                                            (cons (append (merge-listdiffs lds) (car lastdiff)) (cdr lastdiff))
                                          )
                                    ]
                              )
                  )
)

; Return listdiff, except with the first k elements omitted. If k is zero, return listdiff. 
; It is an error if k exceeds the length of listdiff.
(define (ld-tail listdiff k)
  (cond [(= k 0) 
          listdiff
        ]
        [(< k 0) 
          (display "; ld-tail: k is negative\n")
        ]
        [(> k (length-ld listdiff)) 
          (display "; ld-tail: k is greater than the size of listdiff\n")
        ]
        [else 
          (ld-tail (cdr-ld listdiff) (- k 1))
        ]
  )
)

; Return a listdiff that represents the same elements as list.
(define (list->ld list)
  (cons list empty)
)

; Return a list that represents the same elements as listdiff.
(define (ld->list listdiff)
  (get-diff listdiff)
)

; Checks if the lengths of all listdiffs in lds are equal to n
(define (all-of-length-n? lds n)
  (if (empty? lds)
    #t
    (and (= (length-ld (car lds)) n) (all-of-length-n? (cdr lds) n))
  )
)

; Checks if the listdiffs in lds are all of the same length
(define (all-same-length? lds)
  (if (empty? lds)
    #t
    (all-of-length-n? (cdr lds) (length-ld (car lds)))
  )
)

; Get the nth element in a listdiff
(define (get-nth-in-ld ld n)
  (cond [(<= (length-ld ld) n)
          (display "; get-nth-in-ld: n out of bounds\n")
        ]
        [(= n 0)
          (car-ld ld)
        ]
        [(not (ld? ld))
          (display "; get-nth-in-ld: invalid listdiff\n")
        ]
        [else 
          (get-nth-in-ld (cdr-ld ld) (- n 1))      
        ]
  )
)

; Apply the function proc to a single listdiff
(define (map-to-ld proc ld)
  (cond [(not (ld? ld))
          (display "; map-to-ld: invalid listdiff\n")
        ]
        [(null-ld? ld)
          empty
        ]
        [else
          (cons (proc (car-ld ld)) (map-to-ld proc (cdr-ld ld)))
        ]
    
  )
)

; Aggregates results of applying proc to the nth element of each listdiff in lds
(define (map-to-nth n proc lds)
  (cond [(empty? (cdr lds))
          (map-to-ld proc (car lds))
        ] 
        [else
          (map-to-nth n (curry proc (get-nth-in-ld (car lds) n)) (cdr lds))
        ]
  )
)

; Aggregates results of applying proc to every element of each listdiff in lds
(define (map-ld-helper n proc lds)
  (cond [(= n (length-ld (car lds))) 
          empty
        ]
        [else
          (cons (list-ref (map-to-nth n proc lds) n) (map-ld-helper (+ n 1) proc lds)) 
        ]
  )
)

; The listdiffs should all have the same listdiff length. Proc should accept as many arguments as there are listdiffs 
; and return a single value. Proc should not mutate any of the listdiffs. The map-ld procedure applies proc element-wise 
; to the elements of the listdiffs and returns a listdiff of the results, in order. This acts like the standard map function, 
; except that it uses listdiffs instead of lists, and it avoids the overhead of converting its listdiff arguments to lists.
(define (map-ld proc . lds)
  (cond [(empty? lds)
          (cons empty empty)
        ]
        [(not (all-same-length? lds))
          (display "; map-ld: Not all listdiffs have same length\n")
        ]
        [else 
          (cons (map-ld-helper 0 proc lds) empty)
        ]
  )
)

; Return a Scheme expression that is like expr except that it uses listdiff procedures 
; instead of the corresponding list procedures.
(define (expr2ld expr)
  (if (null? expr)
    empty
    (letrec ([e (car expr)] [rest (cdr expr)]) 
      (cond [(pair? e)
              (cons (expr2ld e) (expr2ld rest))
            ]
            [(equal? e 'null?)
              (cons 'null-ld? (expr2ld rest))
            ]
            [(equal? e 'empty?)
              (cons 'null-ld? (expr2ld rest))
            ]
            [(equal? e 'list?)
              (cons 'ld? (expr2ld rest))
            ]
            [(equal? e 'cons)
              (cons 'cons-ld (expr2ld rest))
            ]
            [(equal? e 'car)
              (cons 'car-ld (expr2ld rest))
            ]
            [(equal? e 'cdr)
              (cons 'cdr-ld (expr2ld rest))
            ]
            [(equal? e 'list)
              (cons 'ld (expr2ld rest))
            ]
            [(equal? e 'length)
              (cons 'length-ld (expr2ld rest))
            ]
            [(equal? e 'append)
              (cons 'append-ld (expr2ld rest))
            ]
            [(equal? e 'list-tail)
              (cons 'ld-tail (expr2ld rest))
            ]
            [(equal? e 'map)
              (cons 'map-ld (expr2ld rest))
            ]
            [else 
              (cons e (expr2ld rest))
            ]
      )
    )
  )
)

; Test cases

; (define ils (append '(a e i o u) 'y))
; (define d1 (cons ils (cdr (cdr ils))))
; (define d2 (cons ils ils))
; (define d3 (cons ils (append '(a e i o u) 'y)))
; (define d4 (cons '() ils))
; (define d5 0)
; (define d6 (ld ils d1 37))
; (define d7 (append-ld d1 d2 d6))
; (define e1 (expr2ld '(map (lambda (x) (+ x 1))
;                             (list (length (list d1)) 2 4 8)
;                             (append (list) (list-tail (list 1 16 32) 1)))))

; (define kv1 (cons d1 'a))
; (define kv2 (cons d2 'b))
; (define kv3 (cons d3 'c))
; (define kv4 (cons d1 'd))
; (define d8 (ld kv1 kv2 kv3 kv4))
; (define d9 (ld kv3 kv4))

; (display (ld? d1)) (display "\n")                  
; (display (ld? d2)) (display "\n")                      
; (display (not (ld? d3))) (display "\n")                              
; (display (not (ld? d4))) (display "\n")                           
; (display (not (ld? d5))) (display "\n")                            
; (display (ld? d6)) (display "\n")                              
; (display (ld? d7)) (display "\n")                             

; (display (not (null-ld? d1))) (display "\n")                        
; (display (null-ld? d2)) (display "\n")                   
; (display (not (null-ld? d3))) (display "\n")                 
; (display (not (null-ld? d6))) (display "\n")
                          
; (display (equal? (car-ld d1) 'a)) (display "\n")     
; (display (car-ld d2)) (display "\n")             
; (display (car-ld d3)) (display "\n")             
; (display (equal? (car-ld d6) '(a e i o u . y))) (display "\n")             

; (display (= (length-ld d1) 2)) (display "\n")                   
; (display (= (length-ld d2) 0)) (display "\n")          
; (display (length-ld d3)) (display "\n")                  
; (display (= (length-ld d6) 3)) (display "\n")         
; (display (= (length-ld d7) 5)) (display "\n")          

; (display (eq? d8 (ld-tail d8 0))) (display "\n")             
; (display (equal? (ld->list (ld-tail d8 2)) (ld->list d9))) (display "\n")              
; (display (null-ld? (ld-tail d8 4))) (display "\n")   
  
; (display (eq? (car-ld d6) ils)) (display "\n")                   
; (display (eq? (car-ld (cdr-ld d6)) d1)) (display "\n")          
; (display (eqv? (car-ld (cdr-ld (cdr-ld d6))) 37)) (display "\n") 
; (display (equal? (ld->list d6) (list ils d1 37))) (display "\n")               
; (display (eq? (list-tail (car d6) 3) (cdr d6))) (display "\n") 
    
; (equal? e1 '(map-ld (lambda (x) (+ x 1))
; (ld (length-ld (ld d1)) 2 4 8)
; (append-ld (ld) (ld-tail (ld 1 16 32) 1))))

; (define ld1 (ld (length-ld (ld d1)) 2 4 8))
; (define ld2 (append-ld (ld) (ld-tail (ld 1 16 32) 1)))
; (define lds (list ld1 ld2))

; (display (map-ld (lambda (x) (+ x 1)) ld1)) (display "\n")
; (display (map-ld (lambda (x y) (+ x y)) (ld 1 2 3) (ld 4 5 6)))
