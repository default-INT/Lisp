(defun fact1 (n)
(cond ((zerop n) 1)
 (t (* n (fact1 (- n 1))))))

(defun task1 (list1 list2 &optional (i 1))
	(cond ((and (null list1) (null list2)) nil)
		((and (null list2) (not (null list1))) (cons (car list1) nil))
		((and (null list1) (not (null list2))) (cons (car list2) nil))
		((= (mod i 2) 0) (cons (car list2) (task1 list1 (cdr list2) (+ i 1))))
		(t (cons (car list1) (task1 (cdr list1) list2 (+ i 1))))))

(defun task2 (mylist m n &optional (i 1))
	(cond ((null mylist) nil)
		((and (>= i m) (<= i n)) (task2 (cdr mylist) m n (+ i 1)))
		(t (cons (car mylist) (task2 (cdr mylist) m n (+ i 1))))))

(defun task3 (mylist)
	(cond ((null mylist) nil)
		((not (numberp (car mylist))) (task3 (cdr mylist)))
		(t (cons (car mylist) (task3 (cdr mylist))))))

(defun task4 (mylist)
	(cond ((null mylist) nil)
		((> (car mylist) 0) (cons "Положительное" (task4 (cdr mylist))))
		((< (car mylist) 0) (cons "Отрицательное" (task4 (cdr mylist))))
		((= (car mylist) 0) (cons "Ноль" (task4 (cdr mylist))))))

(defun task5 (mylist m n &optional (i 1))
	(cond ((null mylist) nil)
		((= i m) (task5 (cdr mylist) m n (+ i 1)))
		((= i n) (task5 (cdr mylist) m n (+ i 1)))
		(t (cons (car mylist) (task5 (cdr mylist) m n (+ i 1))))))

(defun task6 (mylist &optional (i 1))
	(cond ((null mylist) nil)
		((= i 1) (task6 (cdr mylist) (+ i 1)))
		((null (cdr mylist)) nil)
		(t (cons (car mylist) (task6 (cdr mylist) (+ i 1))))))

;for task7
(defun delete-not2 (mylist &optional (i 1))
		(cond ((null mylist) nil)
			((= (mod i 2) 0) (cons (car mylist) (delete-not2 (cdr mylist) (+ i 1))))
			(t (delete-not2 (cdr mylist) (+ i 1)))))
;for task7
(defun delete2 (mylist &optional (i 1))
		(cond ((null mylist) nil)
			((= (mod i 2) 0) (delete2 (cdr mylist) (+ i 1)))
			(t (cons (car mylist) (delete2 (cdr mylist) (+ i 1))))))

(defun task7new (mylist)
	(cond ((null mylist) nil)
		(t (list (delete2 mylist) (delete-not2 mylist)))))

(defun task8 (mylist)
	(cond ((null mylist) nil)
		((< (car mylist) 0) (task8 (cdr mylist)))
		(t (cons (car mylist) (task8 (cdr mylist))))))

(defun rev-pod (mylist)
	(cond ((null mylist) nil)
		((not (atom (car mylist))) (cons (reverse (rev-pod (car mylist))) (rev-pod (cdr mylist))))
		(t (cons (car mylist) (rev-pod (cdr mylist))))))

(defun task9 (mylist)
	(cond ((null mylist) nil)
		(t (reverse (rev-pod mylist)))))

(defun task10 (mylist)
	(cond ((null mylist) nil)
		((and (> (car mylist) d) (< (car mylist) k)) (task10 (cdr mylist)))
		(t (cons (car mylist) (task10 (cdr mylist))))))