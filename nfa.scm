;;next-state
(define (next-state character nfa)
  (cond ((null? nfa) nfa)
        ((equal? character (car (car nfa))) (cdr (car nfa)))
        (else (next-state character (cdr nfa)))))
;;all possible states
(define (all-possible-states start_state character nfa)
  (cond ((null? nfa) nfa)
        (else (next-state cha

;;find states to transition to
(define (transition-to-states start_state character nfa)
  (cond ((null? nfa) nfa)
        (else (next-state character (all-possible-states start_state (cdr nfa))))))


;;search using nfa

(define (search-nfa pattern start_state nfa)
  (cond ((null? pattern) pattern)
        (else (brute-force pattern start_state (transition-to-states start_state (car pattern) nfa) nfa))))