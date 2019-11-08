(defun fact1 (n)
(cond ((zerop n) 1)
 (t (* n (fact1 (- n 1))))))

(defun delete-first-el (list)
	(cond ((null list) nil))
	(cdr list))

(defun delete-last-el (list)
	(cond ((null (cdr list)) nil)
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
		((eq (car list) x) (cons (car list) (cons (car list) (dup-atom (cdr list) x))))
		(t (cons (car list) (dup-atom (cdr list) x)))))

(defun counter (list a)
  (cond ((null list) 0)
        ((eq a (car list)) (+ 1 (counter (cdr list) a)))
        (t (counter (cdr list) a))))
; exe 1/3
(defun task1 (n)
	(cond ((< n 0) (negativ-dev (* n -1)))
		(t (positiv-dev n))))

(defun negativ-dev (n)
	(cond ((= (+ (floor n 10) 0) 0) (* n -1))
		(t (negativ-dev (+ (floor n 10) 0)))))

(defun positiv-dev (n)
	(cond ((= (+ (floor n 10) 0) 0) n)
		(t (positiv-dev (+ (floor n 10) 0)))))

(defun task3-sum (n)
	(cond ((= (mod n 10) 0) (mod n 10)) 
		(t (+ (mod n 10) (task3-sum (+ (floor n 10) 0))))))

(defun task3-count (n)
	(cond ((= (mod n 10) 0) 0) 
		(t (+ 1 (task3-count (+ (floor n 10) 0))))))

(defun task2 (n &optional (r '(1)) (p 2))
  (cond ((> p (/ n 2)) (cons n r))
        ((zerop (rem n p)) (task2 n (cons p r) (+ p 1)))
        (t (task2 n r (+ p 1))))) 

(defun task4 (n m &optional (r nil) (p 1))
  (cond ((or (> p (/ n 2)) (> p (/ m 2))) r)
  		((and (= p 1) (zerop (rem m n))) (task4 n m (cons n r) (+ p 1)))
  		((and (= p 1) (zerop (rem n m))) (task4 n m (cons m r) (+ p 1)))
        ((and (zerop (rem n p)) (zerop (rem m p))) (task4 n m (cons p r) (+ p 1)))
        (t (task4 n m r (+ p 1)))))  

(defun task5 (list)
	(cond ((null list) nil)
		(t (car list) (task5 (cdr list))))) ;GCD

(defun replace(mylist x)
(cond
(
(null mylist) nil
)
(
	(eq x (car mylist)) (cons (car mylist) (cons (car mylist) (replace (cdr mylist) x)))
)
(
	(cons (car mylist) (replace (cdr mylist) x ) )
)
)
)

(defun task5 (mylist)
	(cond ((null mylist) nil)
		((atom (cdr (cdr mylist))) (gcd (car mylist)(car (cdr mylist))))
		(t (gcd (car mylist) (task5 (cdr mylist))))))
