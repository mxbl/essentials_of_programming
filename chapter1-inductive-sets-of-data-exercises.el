
;; example from page 2
(defun in-S? (n)
  "Return t if n in S, nil otherwise. S := {3n+2|n in N}"
  (if (= 2 n)
      t
    (if (>= (- n 3) 2)
	(in-S? (- n 3))
      nil)))

;; test
(in-S? 12)
(>= 4 3)

;; Exercise 1.15
;; (duple n x) -> return a list with n copies of x
(defun duple (n x)
  (if (= n 0)
      ()
    (cons x (duple (- n 1) x))))

(duple 3 'test)
(duple 3 '(t t))
(cons () (cons 2 (cons 3 ())))

;; Exercise 1.16
;; grammar for input:
;;
;;   lst ::= ()
;;       ::= ((sym sym)) . lst
;;
;; (invert lst), where lst is a list of 2-lists -> return a list with each 2-list reversed
(defun invert (lst)
  (if (null lst)
      ()
    (cons (list (nth 1 (car lst)) (nth 0 (car lst)))
	  (invert (cdr lst)))))

(equal
 '((a b) (a b) (a b))
 (invert '((b a) (b a) (b a)))) ; => t

;; Exercise 1.17
;; (down lst) -> wraps around parantheses around each top-level element
(defun down (lst)
  (if (null lst)
      ()
    (cons (list (car lst))
	  (down (cdr lst)))))

(down '(1 2 3)) (list '(3 (a)))
(list '(a b))

;; Exercise 1.18
;; grammar for s-list:
;;
;; S-list ::= ()
;;        ::= (S-exp . S-list)
;; S-exp  ::= Symbol | S-list
;; 
;; (swapper s1 s2 slist) -> return slist with all accurrences of s1 replaced by s2 and vice versa
(defun swapper (s1 s2 slist)
  (if (null slist) ()
    (let* ((e (car slist))
	   (subs (cond
		  ((and (symbolp e) (equal e s1)) s2)
		  ((and (symbolp e) (equal e s2)) s1)
		  ((if (symbolp e)
		       e
		     ;; otherwise e is a list
		     (swapper s1 s2 e))))))
      (cons subs
	    (swapper s1 s2 (cdr slist))))))

(defun swapper (s1 s2 slist)
  (if (null slist) ()
    (cons (swapper-sexp s1 s2 (car slist))
	  (swapper s1 s2 (cdr slist)))))

(defun swapper-sexp (s1 s2 sexp)
  (if (symbolp sexp)
      (cond
       ((equal s1 sexp) s2)
       ((equal s2 sexp) s1)
       (t
	sexp)) ; ?? FIXME
    (swapper s1 s2 sexp)))

(swapper 'x 'y '(y x (x (y)) a))
(swapper 'x 'y '(x y (a x y (a x y)) x a b))

(list 'x)

;; symbolp / listp
(listp 'test) ; => nil
(listp '(a b)) ; => t
(symbolp 'test) ; => t
(symbolp '(a b)) ; => nil

(cond
 (nil nil)
 (t "test"))

(let ((s1 'x)
      (s2 'x))
  (if (and (symbolp s1)
	   (symbolp s2)
	   (equal s1 s2))
      t
    nil))
