(defun last-el (list)
	(cond ((null (cdr list)) (car list))
		(t (last-el (cdr list)))))

(defun task7 (arg)
	(cond ((numberp arg) (sqrt arg))
	((not (atom arg)) (last-el arg))
	((atom arg) arg)))

(defun task4 (list1 list2) 
	(cond ((and (not (atom list1)) (> (car list1) 0)) list2)
		(t (cons (car list2) (cdr list1)))))

