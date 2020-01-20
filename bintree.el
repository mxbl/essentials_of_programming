
;; binary tree Definition 1.1.7 / grammar:
;;
;; Bintree ::= Int | (Symbol Bintree Bintree)
;;
;; examples:
;;
;; 1
;; (foo 1 2)
;; (bar 1 (foo 1 2))
;; (baz
;;   (bar 1 (foo 1 2))
;;   (biz 4 5)
;;

;; Excercise 1.31 - write the following procedures

(defun leaf (val)
  (when (numberp val)
    val))

;; testing
(leaf 1)
(leaf 'foo) ; => nil

(defun interior-node (label lson rson)
  (when (and (symbolp label)
	     (or (numberp lson) (listp lson))
	     (or (numberp rson) (listp rson)))
    (list label lson rson)))

;; testing
(interior-node 'foo (leaf 1) (leaf 2))
(interior-node 'foo (leaf 1) (interior-node 'bar (leaf 1) (leaf 2)))

(defun leaf? (tree)
  (numberp tree))

;; testing
(leaf? (leaf 1))
(leaf? 1)

(defun label (tree)
  (nth 0 tree))

(defun lson (tree)
  (nth 1 tree))

(defun rson (tree)
  (nth 2 tree))

;; NOTE: should work on leafs and interior nodes
(defun contents-of (tree)
  (if (leaf? tree) tree
    (list (lson tree)
	  (rson tree))))

;; Exercise 1.32
;; (double-tree tree) -> return a copy of tree with all containing values doubled
;; grammar: Bintree ::= Int | (Symbol Bintree Bintree)
(defun double-tree (tree)
  (if (leaf? tree) (leaf (* tree 2))
    (interior-node (label tree)
		   (double-tree (lson tree))
		   (double-tree (rson tree)))))

;; testing
(let ((d1 '(foo 1 2))
      (d2 '(bar 1 (foo 1 2)))
      (d3 '(baz
	    (bar 1 (foo 1 2))
	    (biz 4 5))))
  (double-tree d3))


;; Binary Search Tree
;; grammar: Binary-Search-Tree ::= () | (Int Binary-Search-Tree Binary-Search-Tree)
;; context-sensitive constraints:
;;  - all the keys in the left subtree are less than the key in the current node
;;  - all the keys in the right subtree are greater than the key in the current node

;; Exercise 1.34
(defun path (n tree)
  (if (null tree) ()
    (cond
     ((< n (car tree)) (cons 'left (path n (lson tree))))
     ((> n (car tree)) (cons 'right (path n (rson tree))))
     (t ()
	))))

(cons 'left '(right))
(let ((data '(14 (7 () (12 () ()))
		 (26 (20 (17 () ())
			 ())
		     (31 () ())))))

  ;; should return: right left left
  (path 17 data))

;; Exercise 1.35
(defun number-leaves (tree)
  (nth 1 (number-leaves-aux 0 tree)))

(defun number-leaves-aux (n tree)
  (if (leaf? tree) (list (+ n 1) (leaf n))
    (let* ((left  (number-leaves-aux-2 n (lson tree)))
	   (right (number-leaves-aux-2 (car left) (rson tree))))
      (list (car right) (interior-node (label tree)
				       (nth 1 left)
				       (nth 1 right))))))

(nth 1 (list 1 (leaf 1)))
(number-leaves
 (interior-node 'foo
		(leaf 1)
		(leaf 2)))

(number-leaves
 (interior-node 'foo
		(interior-node 'bar
			       (leaf 26)
			       (leaf 12))
		(interior-node 'baz
			       (leaf 11)
			       (interior-node 'quux
					      (leaf 117)
					      (leaf 14)))))
