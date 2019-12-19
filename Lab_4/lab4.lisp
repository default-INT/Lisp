;task1 -----------------------------------------

(defun task1 (mylist &optional (avg 0) (n 0))
	(cond ((null mylist) (/ avg n))
		((not (numberp (car mylist))) (task1 (cdr mylist) (+ (task1 (car mylist)) avg) (+ n 1)))
		((< (car mylist) 0) (task1 (cdr mylist) (+ (car mylist) avg) (+ n 1)))
		(t (task1 (cdr mylist) avg n))))

;task2 -----------------------------------------

(defun quad (a b)
	(+ (* a a) (* b b)))

(defun task2 (num &optional (a 0) (b 0))
	(cond ((= (quad a b) num) (format t "~a^2+~a^2=~a" a b num))
		((< b num) (task2 num a (+ b 1)))
		((and (= b num) (< a num)) (task2 num (+ a 1) 0))
		((and (= b num) (= a num)) "impossible") ))

(defun task2 (num &optional (a 0) (b 0))
	(cond ((= (quad a b) num) (format t "~a^2+~a^2=~a" a b num))
		((>= a num) "impossible")
		((< b num) (task2 num a (+ b 1)))
		((< a num) (task2 num (+ a 1) 0))
		(t (format t "~a ~a" a b))))

;task3 -----------------------------------------

(defun quad (a b c)
	(+ (* a a) (* b b) (* c c)))

(defun task3 (num &optional (a 0) (b 0) (c 0))
	(cond ((= (quad a b c) num) (format t "~a^2+~a^2+~a^2=~a" a b c num))
		((and (>= a num) (>= b num) (>= c num)) "impossible")
		((< c num) (task3 num a b (+ c 1)))
		((and (= c num) (< b num)) (task3 num a (+ b 1) 0))
		((= b num) (task3 num (+ a 1) 0 0))))

;task4 -----------------------------------------


(defun quad (a b c d)
	(+ (* a a) (* b b) (* c c) (* d d)))

(defun task4 (num &optional (a 0) (b 0) (c 0) (d 0))
	(cond ((= (quad a b c d) num) (format t "~a^2+~a^2+~a^2+~a^2=~a" a b c d num))
		((and (>= a num) (>= b num) (>= c num) (>= d num)) "impossible")
		((< d num) (task4 num a b c (+ d 1)))
		((and (< c num) (< b num)) (task4 num a b (+ c 1) 0))
		((and (= c num) (< b num)) (task4 num a (+ b 1) 0 0))
		((= b num) (task4 num (+ a 1) 0 0 0))))

;task5 -----------------------------------------

(defun check-down (mylist &optional (last-el (car (cdr mylist))))
	(cond ((null mylist) T)
		((and (not (numberp (car mylist))) (check-down (cdr mylist))) )
		((<= last-el (car mylist)) (check-down (cdr mylist) (car (cdr mylist))  ))
		(NIL)))

(defun check-up (mylist &optional (last-el (car (cdr mylist))) (check T))
	(cond ((null mylist) T)
		((and (not (numberp (car mylist))) (check-up (car mylist))) (check-up (cdr mylist) (car (cdr mylist)) NIL))
		((>= last-el (car mylist)) (check-up (cdr mylist) (car (cdr mylist)) NIL))
		(check (check-down mylist))
		(NIL)))

(defun task5 (mylist)
	(cond ((null mylist) NIL)
		(t (check-up mylist))))

;task6 -----------------------------------------

(defun deep (mylist)
	(cond ((null mylist) 0)
		((listp (car mylist)) (max (+ 1 (deep (car mylist))) (deep (cdr mylist)))) 
		(t (deep (cdr mylist)))))

;task7 -----------------------------------------

(defun task7 ()
	(do ((x 0.1 (+ x 0.15)) (n 0 (+ n 1)) ) 
		((>= x 15.1) (/ x n)) ))

;task8 -----------------------------------------

(defun task8 (mylist &optional (s 0) (d 0))
	(progn
		(dolist (i mylist) 
			(setq s (+ i s)))
		(setq s (/ s (length mylist)))
		(dolist (i mylist)
			(setq d (+ (* (- i s) (- i s)) d)))
		(setq d (/ d (length mylist)))
		(print s)
		(print d)))

;task9 -----------------------------------------

(defun con (mylist add n &optional (i 0))
	(cond ((null mylist) NIL)
		((>= i n) (cons (car add) (con )))
		))

(defun task9 (list1 list2 &optional (result NIL))
	(cond ((= (length list1) (length list2)) (mapcan 'list list1 list2))
		((> (length list1) (length list2)) 
			(con (mapcan 'list list1 list2) list1 (length list2)))
		))

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