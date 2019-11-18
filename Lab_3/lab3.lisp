;task1 -----------------------------------------

(defun task1 (mylist)
	(cond ((null mylist) "number list")
		((numberp (car mylist)) (task1 (cdr mylist)))
		(t "not number list")))

;task2 -----------------------------------------

(defun task2 (mylist &optional (minim (car mylist)))
	(cond ((null mylist) minim)
		((and (> (car mylist) 0) (< (car mylist) minim)) (task2 (cdr mylist) (car mylist)))
		(t (task2 (cdr mylist) minim))))

;task3 -----------------------------------------

(defun check-down (mylist &optional (last-el (car mylist)))
	(cond ((null mylist) "the sequence decreases monotonously")
		((>= last-el (car mylist)) (check-down (cdr mylist) (car mylist)))
		(t "the sequence not monotonously")))

(defun check-up (mylist &optional (last-el (car mylist)))
	(cond ((null mylist) "the sequence monotonously increases")
		((<= last-el (car mylist)) (check-up (cdr mylist) (car mylist)))
		(t (check-down mylist))))

(defun task3 (mylist)
	(cond ((null mylist) "list empty")
		(t (check-up mylist))))

;task4 -----------------------------------------

(defun rev-pod (mylist)
	(cond ((null mylist) nil)
		((not (atom (car mylist))) (cons (reverse (rev-pod (car mylist))) (rev-pod (cdr mylist))))
		(t (cons (car mylist) (rev-pod (cdr mylist))))))

(defun rev-list (mylist)
	(cond ((null mylist) nil)
		(t (reverse (rev-pod mylist)))))

(defun task4 (list1 list2 &optional (list3 nil))
	(cond ((and (null list1) (null list2)) (rev-list list3))
		((null list1) (task4 nil (cdr list2) (cons (car list2) list3)))
		((null list2) (task4 (cdr list1) nil (cons (car list1) list3)))
		((> (car list1) (car list2)) (task4 list1 (cdr list2) (cons (car list2) list3)))
		(t (task4 (cdr list1) list2 (cons (car list1) list3)))))
;task5 -----------------------------------------

(defun max-el (mylist)
	(cond ((null mylist) nil)
		((atom (cdr (cdr mylist))) (max (car mylist)(car (cdr mylist))))
		(t (max (car mylist) (max-el (cdr mylist))))))

(defun min-el (mylist)
	(cond ((null mylist) nil)
		((atom (cdr (cdr mylist))) (min (car mylist)(car (cdr mylist))))
		(t (min (car mylist) (max-el (cdr mylist))))))

(defun delete-not2 (mylist &optional (i 1))
		(cond ((null mylist) nil)
			((= (mod i 2) 0) (cons (car mylist) (delete-not2 (cdr mylist) (+ i 1))))
			(t (delete-not2 (cdr mylist) (+ i 1)))))

(defun delete2 (mylist &optional (i 1))
		(cond ((null mylist) nil)
			((= (mod i 2) 0) (delete2 (cdr mylist) (+ i 1)))
			(t (cons (car mylist) (delete2 (cdr mylist) (+ i 1))))))

(defun task5 (mylist)
	(cond ((null mylist) nil)
		(t (+ (max-el (delete-not2 mylist)) (min-el (delete2 mylist))))))

;task6 -----------------------------------------

(defun task6 (A N)
	(* A (* (- A N) (* (- A (* N 2)) (- A (* N N))))))

;task7 -----------------------------------------
(defun mysin (x N &optional (u x) (i 1))
	(cond ((= i N) (func x i))
		(t (+ (mysin x N (+ i 1))) u)))

(defun func (x i)
	(/ (* x x -1) (* 2 i (+ (* 2 i) 1))))

(defun mysin (x n)
  (let ((s 0) (a x))
     (dotimes (i n s)
       (setq s (+ s a))
       (setq a (* a (func x (+ i 1)))))))

;task8 -----------------------------------------

(defun calc-posl (EPS &optional (i 1))   
	(if (< (/ 1 i) EPS) 0
	    (+ (/ 1 i) (calc-posl EPS (+ 1 i) ) )   ) ) 

;task9 -----------------------------------------

(defun func (x)
	(+ x (- (log (+ x 0.5)) 0.5)))

(defun task9 (EPS &optional (x 0) (a 0) (b 2.0))
	(cond ((<= (abs (- a b)) EPS) x)
		(t (progn  
				(setq c (/ (+ a b) 2))
				(if (<= (* (func a) (func c)) 0) (setq b c)
					(setq a c))
				(task9 EPS (/ (+ a b) 2) a b)
				)) ))

;task10 ----------------------------------------

(defun task10 (mylist)
	(cond ((null mylist) 1)
		((not (atom (car mylist))) (* (task10 (car mylist)) (task10 (cdr mylist))))
		((not (= (car mylist) 0)) (* (car mylist) (task10 (cdr mylist))))
		(t (task10 (cdr mylist))) ))