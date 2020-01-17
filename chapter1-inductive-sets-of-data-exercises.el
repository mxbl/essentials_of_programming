
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

;; Excercise 1.19
;; (list-set lst n x) -> return a list like lst, except that the n-th element, using zero-based indexing, is x 
(defun list-set (lst n x)
  (if (= n 0)
      (cons x (cdr lst))
    (cons (car lst)
	  (list-set (cdr lst) (- n 1) x))))

(list-set '(1 2 3) 4 '(0 1 2))

;; Excercise 1.20
;; (count-occurrences s slist) -> return the number of occurrences of s in slist
;; NOTE: slist is a list of symbols, numbers or strings as list members break the function
(defun count-occurrences (s slist)
  (if (null slist) 0
    (+ (count-occurrences-sexp s (car slist))
       (count-occurrences s (cdr slist)))))

(defun count-occurrences-sexp (s sexp)
  (if (symbolp sexp)
      (cond
       ((equal s sexp) 1)
       (t
	0))
    (count-occurrences s sexp)))

(count-occurrences 'x '(a b c x d e x f x))
(numberp 1) ; => t
(numberp 'x) ; => nil
(stringp "") ; => t

;; Excercise 1.21
;; (product los1 los2) -> calculate the cartesian product of those two list of symbols
;; (a b c) x (x y) => ((a x) (a y) (b y) (b y) (c x) (c y))
(defun product (los1 los2)
  (if (null los1) ()
    (append (product-aux (car los1) los2)
	    (product (cdr los1) los2))))

(defun product-aux (s los)
  (if (null los) ()
    (cons (list s (car los))
	  (product-aux s (cdr los)))))

(product '(a b c) '(x y))
(product '(a) '(x y))

(append '((a x) (a y)) '((b x) (b y)))
(cons '(a b c) '(x y))
