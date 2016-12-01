;; check for each element in list and if its list calll this function again, otherwise it will check for atom and merge it.
(defun drop-all (atm lst)
	(setq lst (drop-value atm lst))
    (loop for i in lst collect 
    	(if (consp i) 
        	(drop-all atm i)
        	(cond ((atom i) i))
      	)
    )
)

(defun drop-value (atm lst)
	(cond ((atom lst) lst)
  		((equal (car lst) atm) (drop-value atm (cdr lst)))
		(t (cons (car lst)(drop-value atm (cdr lst))))
	)
)

;; check for each atom in list whether it is number or not using loop

(defun find-nums (L)
	(setq numslist '())
	(loop for x in (resc L)
		do (
			if (numberp x)
			(setq numslist (append numslist (cons x nil)))
		)
	)
	(write numslist)
)

;; resc function will make nested list to single list

(defun resc (lst)
	(cond ((null lst) nil)
		((atom lst) (list lst))
		(t (append (resc (car lst)) (resc (cdr lst))))
	)
)




