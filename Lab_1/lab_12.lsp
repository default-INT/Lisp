(defun delete_first(mylist)
 (cond 
   ((null (cdr mylist)) nil)
   (t (cdr mylist))
 )
)

(defun delete_end(list)                      
(cond                                          
((null (cdr list)) nil)                        
(t (cons (car list) (delete_end (cdr list))))))


(defun delete_first_end(list)
(
 (delete_first 'list)
 (delete_end 'list)
)
)