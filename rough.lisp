(define eqlist?
	(lambda ( l1 l2)
		(cond
			((and ( null? l1 ) ( null? l2)) #t)
			((and (null? l1 ) ( atom? (car l2)))
				#f
			)
			((null? l1) #f)
			((and ( atom? ( car l1)) (null? l2))
				#f
			)
			(
				(and ( atom? ( car l1 ))
					( atom? ( car l2))
				)
				(and ( eqan ? ( car l1 ) (car l2))
					( eqlist? ( cdr l1 ) ( cdr l2))
				)
			)
			(( atom? ( car l1)) #f)
			((null? l2) #f)
			(( atom? ( car l2)) #f)
			(else
			(and ( eqlist? ( car l1 ) ( car l2))
			( eqlist? ( cdr l1 ) ( cdr l2))))
		)
	)
) 