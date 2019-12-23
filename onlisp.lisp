;; Common lisp has distinct namespaces for variables and functions
;; e.g it is a LISP-2

;; If we have overlapping namespaces, we can use the following
;; accessors to get the value from the correct namespace

;; define a procedure `my-double'
(defun my-double (x) (* x 2))
;; define a variable `my-double'
(setq my-double 2)

;; accessors for specific namespace values
(symbol-value 'my-double)
(symbol-function 'my-double)

;; functional arguments
(apply #'+ '(1 2))

(apply (symbol-function '+) '(1 2))

(apply #'(lambda (x y) (+ x y)) ' (1 2))

;; apply can take any number of arguments, with the function in the
;; first position being applied to the list made by consing the rest
;; of the arguments onto the list given last

(apply #'+ 1 '(2))
(apply #'+ 1 2 '(3))

;; if we dont want to give arguments as a list, use funcall instead

(funcall #'+ 1 2)

(mapcar #'(lambda (x) (+ x 10))
        '(1 2 3))

(mapcar #'+
        '(1 2 3)
        '(10 100 1000))

;; popular higher order functions are `mapcar', `sort', `remove-if'

(sort '(1 4 2 5 6 7 3) #'<)

(remove-if #'evenp '(1 2 3 4 5 6 7))

(defun my-remove-if (fn lst)
  (if (null lst)
      nil
      (if (funcall fn (car lst))
          (out-remove-if fn (cdr lst))
          (cons (car lst) (my-remove-if fn (cdr lst))))))

;; note how the above definition does not sharpquote fn, sharpquotes
;; are only for referring to the function named by a symbol


;; Functions as Properties

(defun behave (animal)
  (case animal
    (dog (wag-tail)
     (bark))
    (rat (scurry)
     (squeak))
    (cat (rub-legs)
     (scratch-carpet))))

;; how to add a new type of animal?

;; we could just simply extend the case statement in the first
;; example, but this quickly becomes cumbersome and unmaintainable.

;; what we really want is to generalize this pattern

(defun behave (animal)
  (funcall (get animal 'behavior)))

;; here we define the behavior of an individual animal as a function
;; stored on the property list of its name

(setf (get 'dog 'behavior)
      #'(lambda ()
          (wag-tail)
          (bark)))

;; this method is more flexible, but accessing property lists is slow,
;; and the use of interpreted function is suboptimal, we will
;; investigate how to use structures and compiled functions to
;; maintain flexibility while emphasizing speed


;; lexical scoping in common lisp gives us closures, a combination of
;; a function and an environment

;; closures are so common place that it's easy to use them without
;; being aware

;; n is free, i.e binding comes from surrounding environment
(defun list+ (lst n)
  (mapcar #'(lambda (x) (+ x n))
          lst))

;; closures can be considered functions with local state (funny
;; enough, isn't that the definition of an object?)

(let ((counter 0))
  (defun new-id () (incf counter))
  (defun reset-id () (setq counter 0)))

;; the two inner defuns share the same counter, in their own lexical
;; scope

(defun make-adder (n)
  #'(lambda (x) (+ x n)))

(setf add2 (make-adder 2)
      add10 (make-adder 10))

;; this version of make-adder returns closures
(defun make-adderb (n)
  #'(lambda (x &optional change)
      (if change
          (setq n x)
          (+ x n))))

(setf addx (make-adderb 1))
(funcall addx 3)

;; notice the &optional change argument in the make-adderb definition
(funcall addx 100 t)
(funcall addx 3)


(defun make-dbms (db)
  (list
   #'(lambda (key)
       (cdr (assoc key db)))
   #'(lambda (key val)
       (push (cons key val) db)
       key)
   #'(lambda (key)
       (setf db (delete key db :key #'car))
       key)))

(setf cities (make-dbms '((boston . us) (paris . france))))

;; this essentially allows us to hide away all of our data behind the
;; interface defined by the closures in `make-dbms'

(defun lookup (key db)
  (funcall (car db) key))


;; mapcar applies a function to every element of a list, and returns a
;; list of with the results
(mapcar #'(lambda (x) (+ 2 x))
        '(2 5 7 3))

;; `labels' can be considered a `let' lambdas
(labels ((inc (x) (1+ x)))
  (inc 3))


;; use labels here to define an inner recursive function, that can
;; capture the bindings for obj
(defun count-instances (obj lsts)
  (labels ((instances-in (lst)
             (if (consp lst)
                 (+ (if (eq (car lst) obj) 1 0)
                    (instances-in (cdr lst)))
                 0)))
    (mapcar #'instances-in lsts)))


;; tail-recursive function definitions are preferable in pretty much
;; every case, because they do not grow the stack indefinitely, and
;; can return immediately after execution is complete, rather than
;; passing work back up a deeply nested recursive tree

;; adding an accumulator to a function can aid in translating it into
;; a tail-recursive function

(defun our-length (lst)
  (labels ((rec (lst acc)
             (if (null lst)
                 acc
                 (rec (cdr lst) (1+ acc)))))
    (rec lst 0)))


;; compiler optimizations may not be active by default, so we can also
;; explicitly announce them

;; (proclaim '(optimize speed))

(defun tail-sum (n)
  (labels ((rec (n acc)
             (if (= n 0)
                 acc
                 (rec (1- n) (+ acc n)))))))


(defun triangle (n)
  (labels ((tri (c n)
             (declare (type fixnum n c))
             (if (zerop n)
                 c
                 (tri (the fixnum (+ n c))
                      (the fixnum (- n 1))))))
    (tri 0 n)))
