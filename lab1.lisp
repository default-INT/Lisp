(defun fact1 (n)
(cond ((zerop n) 1)
 (t (* n (fact1 (- n 1))))))

(defun delete-first-el (list)
	(cond ((null list) nil))
	(car list))

(defun delete-last-el (list)
	(cond ((null (car list)) nil)
		(t (cons (car list) (delete-last-el (cdr list))))))

(defun zero-func (list)
	(cond ((null list) nil)
		(t (cons (if (> (car list) 0) (car list) 0) (zero-func (cdr list))))))

(defun delete-zero (list)
	(cond ((null list) nil)
		((= (car list) 0) (delete-zero (cdr list)))
		(t (cons (car list) (delete-zero (cdr list))))))

(defun dup-atom (list x)
	(cond ((or (not (atom x)) (null list)) nil)
		((eq (car list) x) (cons x (car list)))
		(t (cons (car list) (dup-atom (cdr list) x)))))

(defun counter (list a)
  (cond ((null list) 0)
        ((eq a (car list)) (+ 1 (counter (cdr list) a)))
        (t (counter (cdr list) a))))

(defun task1 (n)
	(print (mod n 10)))