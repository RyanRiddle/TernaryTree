#|
TERNARY TREE CREATOR

Authors: Ryan Riddle (ryan.riddle@uky.edu)
	 Brad Elliott (bradelliott23@gmail.com)


This program accepts a list as input and inserts the values of the 
list, one at a time, into a ternary tree.  It prints the resulting
tree as a list.  Children are represented as sublists.

|#


;Inserts an atom a into a list L, returns a list
(defun insert(L a)
	(cond
		((null L) (cons a nil))		;if the list is empty, return a list containing a
		((null (cdr L)) (cond		;if the list only has one value
					((null (car L)) (cons a nil))	;if the only value of the list is null, return list containing a
					;if a <= the value in L, return a list of two values with a in front
					((<= a (car L)) (cons nil (cons a (cons nil (cons (car L) '(nil))))))
					;else a > the value in L, returns a list with the first of L and then a
					(T   (cons nil (cons (car L) (cons nil (cons a  '(nil))))))
				)
		)
		;know L has at least two elements
		;if a <= the first element, return the list L with a inserted into the left child node
		;by recursively inserting a into the left sublist
		((<= a (cadr L)) (cons (insert (car L) a) (cdr L)))
		;if the first element < a <= second element, return the list L with a inserted into the middle child node
		;by recursively inserting a into the middle sublist
		((<= a (cadddr L)) (cons (car L) (cons (cadr L) (cons (insert (caddr L) a) (cdddr L)))))
		;else second element < a, return list L with a inserted as the right child node
		;by recursively inserting a into the right sublist
		(T 	;(cond
			;	((atom (car (cddddr L))) (cons (car L) (cons (cadr L) (cons (caddr L) (cons (cadddr L) (cons (insert (car (cddddr L)) a ) nil))))))
			;	(T
  			 (cons (car L) (cons (cadr L) (cons (caddr L) (cons (cadddr L) (cons(insert (car (cddddr L)) a ) nil))))	;))
			)
		)
	)
)

;reads the passed list and calls insert on the values
(defun readlist(L)
	(cond
		;base case; if L only has one element, return L
		((null (cadr L)) L)
		;L is bigger than one element, recurse on cdr, inserting car L into the result
		(T (insert (readlist (cdr L)) (car L)))
	)
)

;reverses a list; this function was shown in class
(defun reverseit(L)
	(cond
		((null L) nil)
		((atom L) (cons L nil))
		(T (append (reverseit (cdr L)) (cons (car L) nil)))
	)
)

#|(defun ren(L) ;Remove Extra Nils
	(cond
		((null L) '()) ;When L is nil, return nothing
		((and (null (cdr L)) (not (null (car L)))) L) ;When L is a non-nil atom, return L
		((null (cddr L)) L) ;When L is a list of exactly two atoms, return L
		(T (append (ren (car L)) (cons (cadr L) (append (ren (caddr L)) (cons (cadddr L) (car (cddddr L)))))))
	)
)|#

;driver
(defun main(L)
	;reverses the list given and calls readlist on the reversed list
	(readlist (reverseit L))
)
