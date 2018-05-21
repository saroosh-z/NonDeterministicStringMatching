;;next-state returns all possible states available for the given character
(define (next-state character nfa)
  (cond ((null? nfa) nfa)
        ((equal? character (car (car nfa))) (cdr (car nfa)))
        (else (next-state character (cdr nfa)))))

;;all possible states
; member?. This function checks if an atom is in a list.
(define (member? a li)
  (cond
    ((null? li) #f)
    (else (or (equal? a (car li))
              (member? a (cdr li))))
    )
  )

;;following function divides nfa into small nfas for each state
(define (sub-nfa state nfa)
  (cond ((null? nfa) nfa)
        ((member? state (car nfa)) (cdar nfa))
        (else (sub-nfa state (cdr nfa)))))

;following returns all the available states nfa can transition to
(define (go-to-next-state state character nfa)
  (cond ((null? nfa) nfa)
        (else (next-state character (sub-nfa state (cdr nfa))))))
;;backtrack
(define (backtrack pattern start_state next_state nfa)
  (cond ((null? next_state ) '())
        (else
          (display 'calling-nfa-execute)  (newline)
         (cond ((null? (nfa-call (cdr pattern) (car next_state) nfa))
                (backtrack pattern start_state (cdr next_state) nfa))
               (else
                (cond ((null? (cdr pattern)) (display 'accepted))
                       (else (nfa-call (cdr pattern) (car next_state) nfa))))))))

   
;;this is the acceptence state of our nfa which is first element of nfa
(define (acceptence-state nfa)
  (car nfa))
;;search using nfa

(define (nfa-call pattern start_state nfa)
    (display 'new-start-state) (display start_state) (newline)

  (cond ((null? pattern)
         (cond ((not (member? start_state (acceptence-state nfa))) '())
               (else (list start_state))))  
       (else (backtrack pattern start_state (go-to-next-state start_state (car pattern) nfa) nfa))))

(define nfa1 '((4) (1 (a 1 2 3) (b 1) (c 1)) (2 (b 3)) (3 (c 4))))
;(nfa-execute '(a  ) 1 m1) ;returns '(1 2 3 4)
(nfa-call '(a b c) 1 nfa1)
(define n1 '((5) (1 (n 1 2 ) (a 1) (0 1)) (2 (a 3)) (3 (n 1 4)) (4 (o 5))))

(nfa-call '(n a n a n o ) 1 n1)