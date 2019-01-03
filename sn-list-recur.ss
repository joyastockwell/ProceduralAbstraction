; Many operations on sn-lists, which are lists of the form
; <sn-list> ::= ( {<sn-expression>}* )
; <sn-expression> ::= <number> | <symbol> | <sn-list>

;; returns procedures that recur on sn-lists

(define snlist-recur
	(lambda (base-val lstProc snProc)
		(letrec ([helper
			(lambda (lst)
						; return the base value if the list is null
				(cond	[(null? lst) base-val]
						; if the car of the list is a symbol or number, return snlist-recur of the car and cdr
						[(or (number? (car lst)) (symbol? (car lst)))
							(snProc (car lst) (helper (cdr lst)))]
						; if the car is a list, use lstProc on the snlist-recur retVal for both the car and cdr 
						[(list? (car lst))
							(lstProc (helper (car lst)) (helper (cdr lst)))]
						; otherwise, an error has occured
						[else (errorf 'sn-list-recur "ERROR")]))])
				; return the procedure made
				helper)))
					
					
					
;; sums numbers; ignores symbols

(define sn-list-sum 
	(lambda (snlist)
		((snlist-recur 
			0 
			(lambda (carVal cdrVal) (+ carVal cdrVal)) 
			(lambda (carVal cdrVal) (+ carVal cdrVal)))
			snlist)))
			
;; applies proc to each element of arg and returns an sn-list that has the same “shape” as arg

(define sn-list-map
	(lambda (mapProc snlist)
			((snlist-recur
				'()
				(lambda (carVal cdrVal) (cons carVal cdrVal))
				(lambda (carVal cdrVal) (cons (mapProc carVal) cdrVal)))
				snlist)))
				
;; counts the number of parentheses required to produce the printed representation of snlst

(define sn-list-paren-count
	(lambda (snlist)
		((snlist-recur
			2
			; subtract 2 from cdrVal because of invisible empty list at end
			(lambda (carVal cdrVal) (+ (+ 2 carVal) (- cdrVal 2)))
			(lambda (carVal cdrVal) cdrVal))
			snlist)))
			
;; reverses snlst and all of its sublists

(define sn-list-reverse
	(lambda (snlist)
		((snlist-recur
			'()
			(lambda (carVal cdrVal) (append cdrVal (list carVal)))
			(lambda (carVal cdrVal) (append cdrVal (list carVal))))
			snlist)))			
				
;; counts how many times the symbol s occurs in the sn-list snlst)

(define sn-list-occur
	(lambda (s snlist)
		((snlist-recur
			0
			(lambda (carVal cdrVal) (+ cdrVal carVal))
			(lambda (carVal cdrVal) (if (equal? carVal s)
										(+ 1 cdrVal)
										cdrVal)))
			snlist)))

;;  finds the maximum nesting-level of parentheses in the printed representation of snlist

(define sn-list-depth
	(lambda (snlist)
		((snlist-recur
			1
			(lambda (carVal cdrVal) (max (+ 1 carVal) cdrVal))
			(lambda (carVal cdrVal) cdrVal))
			snlist)))