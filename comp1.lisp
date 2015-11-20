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

;;(defun subtract (x y)
;; (if (zerop y)
;;   "(proj 3 1 (list (subt x (1- y)) x (1- y))) parece que o lisp evalua os
;;    argumentos antes de realizar o operador proj pois ao rodar a função
;;    subt com o código acima foi retornado:
;;    \"Control stack guard page temporarily disabled: proceed with caution\"
;;    mesmo que a função recursiva não fosse chamada, ou seja, early evaluation
;;    x
;;   (monus-1 (proj 3 0 (list (subt x (1- y)) x (1- y)))))))"

(defun expo (x n)
  (if (zerop n)
      (suc (zero x))
      (mult (proj 3 1 (list (expo x (1- n)) x (1- n)))
	    (proj 3 0 (list (expo x (1- n)) x (1- n))))))

(defun pos (key seq)
  "Essa função não trata do caso onde a chave(key) fornecida
  não se encontra na sequencia(seq)"
  (let ((tam-no-key (length key)))
     (subseq seq 
       (+ (search key seq) tam-no-key))))

(defun pre (key seq)
  "Essa função espera que exista a chave na sequencia"
  (subseq seq 0 (search key seq)))

(defun zubst (new old seq)
  (concatenate 'string (pre old seq) new (pos old seq)))

(defun est-atual (c)
  (concatenate 'string "Q" (pre "S" (pos "Q" c))))

(defun simb-lido (c)
  (concatenate 'string "S" 
	       (pre "S" (pos "S" (pos "Q" c)))))

(defun prox-est (q s m) 
  (pre "," (pos (concatenate 'string "<" q "," s ",") m)))

(defun prox-simb (q s m)
  (concatenate 'string "S" 
	       (pre "," (pos "S" (pos (concatenate 'string "<" q "," s ",") m)))))

(defun direcao (q s m)
   (pre ">" (pos "," (pos "S" (pos (concatenate 'string "<" q "," s ",") m)))))

(defun ult-simb (s)
  (if (find #\S s :test #'equal)
      (ult-simb (pos "S" s))
      (concatenate 'string "S" s)))

(defun simb-esq-lido (c) 
       (ult-simb (pre "Q" c)))

(defun ztep (m c)
  (let* ((q (est-atual c)) 
	 (s (simb-lido c))
	 (ps (prox-simb q s m))
	 (se (simb-esq-lido c))
	 (pq (prox-est q s m)))
    (if (equal (direcao q s m) "D")
	(zubst (concatenate 'string ps pq) (concatenate 'string q s) c)
	(zubst (concatenate 'string pq se ps) (concatenate 'string se q s) c)))) 

(defun zteps (m c n)
  (if (equal n 0)
      c
      (zteps m (ztep m c) (monus-1 n))))

(defun est-final (m)
  (pos "#" (pre "<" m)))

(defun passos-parada (m c acc)
  (let ((qf (est-final m))
	(q (est-atual c)))
	(if (equal q qf)
	    acc
	    (passos-parada m (ztep m c) (suc acc)))))

(defun run (m x)
  (let ((qx (concatenate 'string "QI" x)))
    (zteps m qx (passos-parada m qx 0))))
