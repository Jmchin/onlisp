; Common lisp has distinct namespaces for variables and functions
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



;;; Chapter 3 -- Functional Programming
;;; ----------------------------------------------------------------------


;; programs which evolve rather than being developed are the kinds of
;; programs we are striving to learn to write in this book

;; when we talk about functional programming, we mean input->output
;; rather than mutating state via destructive changes

;; to illustrate the difference between functional programming and
;; traditional imperative programming...

(defun bad-reverse (lst)
  (let* ((len (length lst))
         (ilimit (truncate (/ len 2))))
    (do ((i 0 (1+ i))
         (j (1- len) (1- j)))
        ((>= i ilimit))
      (rotatef (nth i lst) (nth j lst)))))

;; Bad code like this is infectious. Because it mutates state, its
;; callers must be prepared and expecting that. If you are writing
;; in a mostly functional style, a single definition like this could
;; prove painful

;; before moving forward, lets try to write the good reverse function

(defun my-reverse (lst acc)
  (if (null lst)
      acc
      (my-reverse (cdr lst) (cons (car lst) acc))))

;; here is the definition given in the book

(defun good-reverse (lst)
  (labels ((rev (lst acc)
             (if (null lst)
                 acc
                 (rev (cdr lst) (const (car lst) acc)))))
    (rev lst nil)))


;; Some quick observations from having typed both of those definitions
;; out slowly, is that the good-reverse definition is better design,
;; because the call does not require an initial empty list, as my
;; definition did. The writer achieved this by definining an inner
;; definition local to good-reverse that allowed us to provide our own
;; default value for the implementation. This is good, the user
;; shouldn't need to know about minor details like that.

;; Make sure to internalize the shape of functional code as opposed to
;; imperative code. Get into the habit of writing code that works with
;; returned values whenever possible, instead of mutating state or
;; other side-effect

;; IMPORTANT: most lisp operators are intended to be called for their
;; RETURN VALUES. This is important for program design. Whenever you
;; call an operator, you need to capture that result in some binding
;; if you need it later. A good rule of thumb is, if we want to mutate
;; something, use setf.

;; Also, be wary of let*, as its semantics quickly allow a programmer
;; to fall into an imperative style


;; A common need for side effects in other programming languages is
;; the need to return multiple values. Most languages only support a
;; single value return. Common Lisp however allows us to return
;; multiple values

(truncate 26.1875)

;; When calling code is only expecting a single value, the first value
;; returned is used. If we want to capture multiple return values from
;; a procedure, we need to use `multiple-value-bind'

(multiple-value-bind (int frac) (truncate 26.21875)
  (list int frac))

;; To return multiple values, use the `values' operator

(defun powers (x)
  (values x (sqrt x) (expt x 2)))

;; Use these two in conjunction to leverage functional programming.
;; Instead of mutating state directly, return multiple values that can
;; be used by the caller


;; functional vs imperative -- functional (tells computer what you
;; want), imperative (tells computer what to do)

(defun fun (x)
  (list 'a (expt (car x) 2)))

(defun imp (x)
  (let (y sqr)
    (setf y (car x))
    (setf sqr (expt y 2))
    (list 'a sqr)))

(defun qualify (expr)
  (nconc (copy-list expr) (list 'maybe)))


;; convention -- invocation owns objects it receives as RETURN values,
;; but not objects passed to it as ARGUMENTS

(defun ok (x)
  (nconc (list 'a x) (list 'c)))

;; nconc might destructively concatenate its arguments, but because
;; each argument is freshly consed, this is a moot point

(defun not-ok (x)
  (nconc (list 'a) x (list 'c)))



;; terrible design, because although it has no side-effects, its
;; results are purely contingent upon a global variable, whose value
;; could be anything at the time this procedure is called
(defun anything (x)
  (+ x *anything*))


;;; Chapter 4 -- Utility Functions
;;; ----------------------------------------------------------------------

(defun all-nicknames (names)
  (if (null names)
      nil
      (nconc (nicknames (car names)
                        (all-nicknames (cdr names))))))

(mapcan #'nicknames people)


(let ((town (find-if #'bookshops towns)))
  (values town (bookshops town)))

(defun find-books (towns)
  (if (null towns)
      nil
      (let ((shops (bookshops (car towns))))
        (if shops
            (values (car towns) shops)
            (find-books (cdr towns))))))

;; define the general, and pass the specific as an argument. use this
;; general pattern of abstraction to build the language up toward the
;; application
(defun find2 (fn lst)
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
        (if val
            (values (car lst) val)
            (find2 fn (cdr lst))))))

;; some utility functions for operating on lists
(proclaim '(inline last1 single append1 conc1 mklist))

(defun last1 (lst)
  (car (last lst)))

(defun single (lst)
  (and (consp lst) (not (cdr lst))))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun conc1 (lst obj)
  (nconc lst (list obj)))

(defun mklist (obj)
  (if (listp obj) obj (list obj)))

(defun longer (x y)
  (labels ((compare (x y)
             (and (consp x)
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
        (> (length x) (length y)))))

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

(defun group (source n)
  (when (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defun flatten (lst)
  (labels ((rec (lst acc)
             (cond ((null lst) acc)
                   ((atom lst) (cons lst acc))
                   (t
                    (rec (car lst) (rec (cdr lst) acc))))))
    (rec lst nil)))

(defun prune (pred tree)
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree))
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc)))
                   (t
                    (rec (cdr tree)
                         (if (funcall pred (car tree))
                             acc
                             (cons (car tree) acc)))))))
    (rec tree nil)))

(defun find2 (fn lst)
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
        (if val
            (values (car lst) val)
            (find2 fn (cdr lst))))))

(defun before (x y lst &key (test #'eql))
  "Recursively CDR down LST, applying TEST to each CAR. If Y passes
the test first, return nil, else return the remainder of the
untraversed list."
  (and lst
       (let ((first (car lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t
                (before x y (cdr lst) :test test))))))

(defun after (x y lst &key (test #'eql))
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))

(defun duplicate (obj lst &key (test #'eql))
  "Returns a list at the point OBJ is first duplicated, else NIL"
  (member obj
          (cdr (member obj lst :test test)) :test test))

(defun split-if (pred lst)
  "Returns 2 lists split at the point where PRED is True"
  (let ((acc nil))
    (do ((src lst (cdr src)))
        ((or (null src) (funcall pred (car src)))
         (values (nreverse acc) src))
      (push (car src) acc))))

(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setf wins obj
                    max score))))
        (values wins max))))

(defun best (fn lst)
  (if (null lst)
      nil
      (let ((wins (car lst)))
        (dolist (obj (cdr lst))
          (if (funcall fn obj wins)
              (setf wins obj)))
        wins)))

(defun mostn (fn lst)
  (if (null lst)
      (values nil nil)
      (let ((result (list (car lst)))
            (max (funcall fn (car lst))))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (cond ((> score max)
                   (setf max score
                         result (list obj)))
                  ((= score max)
                   (push obj result)))))
        (values (nreverse result) max))))

;;;; MAPPING
;;;; ----------------------------------------------------------------------

(defun map0-n (fn n)
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  (mapa-b fn 1 n))

(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

(defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))

(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))

(defun mapcars (fn &rest lsts)
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
        (push (funcall fn obj) result)))
    (nreverse result)))

(defun rmapcar (fn &rest args)
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar
             #'(lambda (&rest args)
                 (apply #'rmapcar fn args))
             args)))

;; (rmapcar #'princ '(1 2 (3 4 (5) 6) 7 (8 9)))
;; (rmapcar #'+ '(1 (2 (3) 4)) '(10 (20 (30) 40)))

;;;; I/O
;;;; ----------------------------------------------------------------------

(defun readlist (&rest args)
  (values (read-from-string
           (concatenate 'string "(" (apply #'read-line args) ")"))))

(defun prompt (&rest args)
  (apply #'format *query-io* args)
  (read *query-io*))

(defun break-loop (fn quit &rest args)
  (format *query-io* "Entering break-loop. ~%")
  (loop
    (let ((in (apply #'prompt args)))
      (if (funcall quit in)
          (return)
          (format *query-io* "~A~%" (funcall fn in))))))

;;;; Strings and Symbols
;;;; ----------------------------------------------------------------------

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun reread (&rest args)
  (values (read-from-string (apply #'mkstr args))))

(defun explode (sym)
  (map 'list #'(lambda (c)
                 (intern (make-string 1
                                      :initial-element c)))
       (symbol-name sym)))

;;;; Returning Functions
;;;; ----------------------------------------------------------------------

;; use `complement' when you need to negate a predicate

(defvar *!equivs* (make-hash-table))

;; functions and their destructive counterparts are usually known
;; before runtime, so for maximum efficiency define `!' as a macro or
;; provide a read macro for it
(defun ! (fn)
  "Returns the destructuve equivalent of fn if defined in *!equivs*"
  (or (gethash fn *!equivs*) fn))

(defun def! (fn fn!)
  "Maps a function to its destructive equivalent"
  (setf (gethash fn *!equivs*) fn!))

;;;; Memoizing
;;;; ----------------------------------------------------------------------

(defun memoize (fn)
  "Returns a closure which caches the results of applying fn"
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
              val
              (setf (gethash args cache)
                    (apply fn args)))))))

(defun compose (&rest fns)
  ""
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))

(defun fif (if then &optional else)
  #'(lambda (x)
      (if (funcall if x)
          (funcall then x)
          (if else (funcall else x)))))

(defun fint (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fint fns)))
        #'(lambda (x)
            (and (funcall fn x) (funcall chain x))))))

(defun fun (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fun fns)))
        #'(lambda (x)
            (or (funcall fn x) (funcall chain x))))))

;; because `not' is a lisp function, we can define complement in terms
;; of it, using composition

;; (defun complement (pred) (compose #'not p)

(defun our-length (lst)
  (if (null lst)
      0
      (1+ (our-length (cdr lst)))))

(defun our-every (fn lst)
  (if (null lst)
      t
      (and (funcall fn (car lst))
           (our-every fn (cdr lst)))))

;; consider the two functions defined above, and the similiarities in
;; their structure. Both functions recur down the cdr of their list
;; arguments, and return a base value when the list is exhausted. This
;; pattern comes up so often that any experiened programmer can
;; instantly see the shared structure and will be wondering if we can
;; capture this abstraction cleanly

(defun lrec (rec &optional base)
  "Constructs a list recursing function. REC is a function of two
arguments, the car of the list being recurred upon, and the function
embodying the recursive call."
  (labels ((self (lst)
             (if (null lst)
                 (if (functionp base)
                     (funcall base)
                     base)
                 (funcall rec (car lst) #'(lambda ()
                                            (self (cdr lst)))))))
    #'self))

;; copy-list
(lrec #'(lambda (x f) (cons x (funcall f))))

;; remove-duplicates
(lrec #'(lambda (x f) (adjoin x (funcall f))))

;; find-if, for some function fn
(lrec #'(lambda (x f) (if (fn x) x (funcall f))))

;; some, for some function fn
(lrec #'(lambda (x f) (or (fn x) (funcall f))))

;;; Note: While lrec may be rather readable for an experienced lisper,
;;; it will usually produce inoptimal code because it is not accouting
;;; for a tail-recursive solution

;;;; Recursion on Subtrees
;;;; ----------------------------------------------------------------------

(defun rfind-if (fn tree)
  (if (atom tree)
      (and (funcall fn tree) tree)
      (or (rfind-if fn (car tree))
          (if (cdr tree) (rfind-if fn (cdr tree))))))
