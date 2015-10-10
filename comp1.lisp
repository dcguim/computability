(defun proj (n k list)
	   (if (equal n (list-length list)) 
	       (nth k list)))

(defun comp (g h x)
	     (funcall g (funcall h x)))

(defun zero (n)
  (* 0 n))

(defun suc (n)
  (1+ n))

;Still trying to make this work
;(defun primrec (g h)
;  (labels ((f (&rest args)
;	     (if (zerop (cadr args))
;		 (funcall g (car args))
;		 (funcall h (car args) (1- (cadr args)) (f (car args) (1- (cadr args)))))))#'f))

(defun somar (n x)
	    (if (zerop n)
	       (proj 2 1 (list n x)) ;g
	       (suc (proj 3 0 (list (somar (1- n) x) (1- n) x))))) ;h

(defun mult (n x)
  (if (zerop n) 				       
      (zero x) 
      (somar (proj 3 0 (list (mult (1- n) x) (1- n) x))
	     (proj 3 2 (list (mult (1- n) x) (1- n) x))))) 

(defun monus-1-aux (n x) 
  (if (zerop n)
      (zero x)
      (proj 3 1 (list (monus-1-aux (1- n) x) (1- n) x))))

(defun monus-1 (x)
  (monus-1-aux x x))

(defun subt (x y)
 (if (zerop y)
     ; (proj 3 1 (list (subt x (1- y)) x (1- y))) parece que o lisp evalua os
     ; argumentos antes de realizar o operador proj pois ao rodar a função
     ; subt com o código acima foi retornado:
     ; "Control stack guard page temporarily disabled: proceed with caution"
     ; mesmo que a função recursiva não fosse chamada, ou seja, early evaluation
     x
     (monus-1 (proj 3 0 (list (subt x (1- y)) x (1- y)))))))

(defun expo (x n)
  (if (zerop n)
      (suc (zero x))
      (mult (proj 3 1 (list (expo x (1- n)) x (1- n)))
	    (proj 3 0 (list (expo x (1- n)) x (1- n))))))
