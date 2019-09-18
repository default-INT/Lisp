(defun delete_first(mylist)
 (cond 
   ((null (cdr mylist)) nil)
   (t (cdr mylist))
 )
)

(defun delete_end(list)                      
(cond                                          
((null (cdr list)) nil)                        
(t (cons (car list) 
(delete_end (cdr list))))))


(defun delete_first_end(list)
(
 (t (delete_first (list))
 (delete_end (list)))
))

(defun zeros (w)
  (cond ((null w) nil)
        ((< (car w) 0) (cons 0 (zeros (cdr w))))
        ((cons (car w) (zeros (cdr w))))))
 
(defun delete_end(list)                      
(cond                                          
((null (cdr list)) nil)                        
(t (cons (car list) 
(delete_end (cdr list))))))
