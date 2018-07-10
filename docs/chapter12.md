# Chapter 12
## Compiling Logic Programs

The end of [chapter 11](B978008057115750011X.xhtml) introduced a new, more efficient representation for logic variables.
It would be reasonable to build a new version of the Prolog interpreter incorporating this representation.
However, [chapter 9](B9780080571157500091.xhtml) has taught us that compilers run faster than interpreters and are not that much harder to build.
Thus, this chapter will present a Prolog compiler that translates from Prolog to Lisp.

Each Prolog predicate will be translated into a Lisp function, and we will adopt the convention that a predicate called with a different number of arguments is a different predicate.
If the symbol `p` can be called with either one or two arguments, we will need two Lisp functions to implement the two predicates.
Following Prolog tradition, these will be called `p/1` and `p/2`.

The next step is to decide what the generated Lisp code should look like.
It must unify the head of each clause against the arguments, and if the unification succeeds, it must call the predicates in the body.
The difficult part is that the choice points have to be remembered.
If a call to a predicate in the first clause fails, we must be able to return to the second clause and try again.

This can be done by passing in a *success continuation* as an extra argument to every predicate.
This continuation represents the goals that remain unsolved, the `other-goals` argument of `prove`.
For each clause in the predicate, if all the goals in a clause succeed, then we should call the success continuation.
If a goal fails, we don't do anything special; we just go on to the next clause.
There is one complication: after failing we have to undo any bindings made by `unify!`.
Consider an example.
The clauses

[ ](#){:#l0010}`(<− (likes Robin cats))`
!!!(p) {:.unnumlist}

`(<− (likes Sandy ?x) (likes ?x cats))`
!!!(p) {:.unnumlist}

`(<− (likes Kim ?x) (likes ?x Lee) (likes ?x Kim))`
!!!(p) {:.unnumlist}

could be compiled into this:

[ ](#){:#l0015}`(defun likes/2 (?arg1 ?arg2 cont)`
!!!(p) {:.unnumlist}

` ;; First clause:`
!!!(p) {:.unnumlist}

` (if (and (unify!
?arg1 'Robin) (unify!
?arg2 'cats))`
!!!(p) {:.unnumlist}

`   (funcall cont))`
!!!(p) {:.unnumlist}

` (undo-bindings)`
!!!(p) {:.unnumlist}

` ;; Second clause:`
!!!(p) {:.unnumlist}

` (if (unify!
?argl 'Sandy)`
!!!(p) {:.unnumlist}

`   (likes/2 ?arg2 'cats cont))`
!!!(p) {:.unnumlist}

` (undo-bindings)`
!!!(p) {:.unnumlist}

` ;; Third clause:`
!!!(p) {:.unnumlist}

` (if (unify!
?argl 'Kim)`
!!!(p) {:.unnumlist}

`   (likes/2 ?arg2 'Lee`
!!!(p) {:.unnumlist}

`     #'(lambda () (likes/2 ?arg2 'Kim cont))))))`
!!!(p) {:.unnumlist}

In the first clause, we just check the two arguments and, if the unifications succeed, call the continuation directly, because the first clause has no body.
In the second clause, `likes/2` is called recursively, to see if `?arg2` likes `cats`.
If this succeeds, then the original goal succeeds, and the continuation `cont` is called.
In the third clause, we have to call `likes/2` recursively again, this time requesting that it check if `?arg2` likes `Lee`.
If this check succeeds, then the continuation will be called.
In this case, the continuation involves another call to `likes/2`, to check if `?arg2` likes `Kim`.
If this succeeds, then the original continuation, `cont`, will finally be called.

Recall that in the Prolog interpreter, we had to append the list of pending goals, `other-goals`, to the goals in the body of the clause.
In the compiler, there is no need to do an `append.` Instead, the continuation cont represents the other-goals, and the body of the clause is represented by explicit calls to functions.

Note that the code for `likes/2` given before has eliminated some unnecessary calls to `unify!`.
The most obvious implementation would have one call to `unify!` for each argument.
Thus, for the second clause, we would have the code:

[ ](#){:#l0020}`(if (and (unify!
?argl 'Sandy) (unify!
?arg2 ?x))`
!!!(p) {:.unnumlist}

` (likes/2 ?x 'cats cont))`
!!!(p) {:.unnumlist}

where we would need a suitable let binding for the variable `?x`.

## [ ](#){:#st0010}12.1 A Prolog Compiler
{:#s0010}
{:.h1hd}

This section presents the compiler summarized in [figure 12.1](#f0010).
At the top level is the function `prolog-compile`, which takes a symbol, looks at the clauses defined for that symbol, and groups the clauses by arity.
Each symbol/arity is compiled into a separate Lisp function by `compile-predicate`.

![f12-01-9780080571157](images/B9780080571157500121/f12-01-9780080571157.jpg)     
Figure 12.1
!!!(span) {:.fignum}
Glossary for the Prolog Compiler
[ ](#){:#l9025}`(defun prolog-compile (symbol &optional`
!!!(p) {:.unnumlist}

`      (clauses (get-clauses symbol)))`
!!!(p) {:.unnumlist}

` "Compile a symbol; make a separate function for each arity."`
!!!(p) {:.unnumlist}

` (unless (null clauses)`
!!!(p) {:.unnumlist}

`  (let ((arity (relation-arity (clause-head (first clauses)))))`
!!!(p) {:.unnumlist}

`   ;; Compile the clauses with this arity`
!!!(p) {:.unnumlist}

`   (compile-predicate`
!!!(p) {:.unnumlist}

`    symbol arity (clauses-with-arity clauses #'= arity))`
!!!(p) {:.unnumlist}

`   ;; Compile all the clauses with any other arity`
!!!(p) {:.unnumlist}

`   (prolog-compile`
!!!(p) {:.unnumlist}

`    symbol (clauses-with-arity clauses #'/= arity)))))`
!!!(p) {:.unnumlist}

Three utility functions are included here:

[ ](#){:#l0030}`(defun clauses-with-arity (clauses test arity)`
!!!(p) {:.unnumlist}

` "Return all clauses whose head has given arity."`
!!!(p) {:.unnumlist}

` (find-all arity clauses`
!!!(p) {:.unnumlist}

`     :key #'(lambda (clause)`
!!!(p) {:.unnumlist}

`        (relation-arity (clause-head clause)))`
!!!(p) {:.unnumlist}

`     :test test))`
!!!(p) {:.unnumlist}

`(defun relation-arity (relation)`
!!!(p) {:.unnumlist}

` "The number of arguments to a relation.`
!!!(p) {:.unnumlist}

` Example: (relation-arity '(p a b c)) => 3"`
!!!(p) {:.unnumlist}

` (length (args relation)))`
!!!(p) {:.unnumlist}

`(defun args (x) "The arguments of a relation" (rest x))`
!!!(p) {:.unnumlist}

The next step is to compile the clauses for a given predicate with a fixed arity into a Lisp function.
For now, that will be done by compiling each clause indepently and wrapping them in a `lambda` with the right parameter list.

[ ](#){:#l0035}`(defun compile-predicate (symbol arity clauses)`
!!!(p) {:.unnumlist}

` "Compile all the clauses for a given symbol/arity`
!!!(p) {:.unnumlist}

` into a single LISP function."`
!!!(p) {:.unnumlist}

` (let ((predicate (make-predicate symbol arity))`
!!!(p) {:.unnumlist}

`    (parameters (make-parameters arity°)))`
!!!(p) {:.unnumlist}

`  (compile`
!!!(p) {:.unnumlist}

`   (eval`
!!!(p) {:.unnumlist}

`    '(defun ,predicate (,©parameters cont)`
!!!(p) {:.unnumlist}

`      ..(mapcar #'(lambda (clause)`
!!!(p) {:.unnumlist}

`            (compile-clause parameters clause 'cont))`
!!!(p) {:.unnumlist}

`       clauses))))))`
!!!(p) {:.unnumlist}

`(defun make-parameters (arity)`
!!!(p) {:.unnumlist}

` "Return the list (?arg1 ?arg2 … ?arg-arity)"`
!!!(p) {:.unnumlist}

` (loop for i from 1 to arity`
!!!(p) {:.unnumlist}

`    collect (new-symbol '?arg i)))`
!!!(p) {:.unnumlist}

`(defun make-predicate (symbol arity)`
!!!(p) {:.unnumlist}

` "Return the symbol: symbol/arity"`
!!!(p) {:.unnumlist}

` (symbol symbol '/ arity))`
!!!(p) {:.unnumlist}

Now for the hard part: we must actually generate the code for a clause.
Here again is an example of the code desired for one clause.
We'll start by setting as a target the simple code:

[ ](#){:#l0040}`(<− (likes Kim ?x) (likes ?x Lee) (likes ?x Kim))`
!!!(p) {:.unnumlist}

`(defun likes/2 (?arg1 ?arg2 cont)`
!!!(p) {:.unnumlist}

` …`
!!!(p) {:.unnumlist}

` (if (and (unify!
?argl 'Kim) (unify!
?arg2 ?x)`
!!!(p) {:.unnumlist}

`   (likes/2 ?arg2 'Lee`
!!!(p) {:.unnumlist}

`      #'(lambda () (likes/2 ?x 'Kim))))`
!!!(p) {:.unnumlist}

 …)
!!!(p) {:.unnumlist}

but we'll also consider the possibility of upgrading to the improved code:

[ ](#){:#l0045}`(defun likes/2 (?arg1 ?arg2 cont)`
!!!(p) {:.unnumlist}

` …`
!!!(p) {:.unnumlist}

` (if (unify!
?arg1 'Kim)`
!!!(p) {:.unnumlist}

   `(likes/2 ?arg2 'Lee`
!!!(p) {:.unnumlist}

      `#'(lambda () (likes/2 ?arg2 'Kim))))`
!!!(p) {:.unnumlist}

 …)
!!!(p) {:.unnumlist}

One approach would be to write two functions, `compile-head` and `compile-body`, and then combine them into the code (if *head body*).
This approach could easily generate the prior code.
However, let's allow ourselves to think ahead a little.
If we eventually want to generate the improved code, we will need some communication between the head and the body.
We will have to know that the head decided not to compile the unification of `?arg2` and `?x`, but because of this, the body will have to substitute `?arg2` for `?x`.
That means that the `compile-head` function conceptually returns two values: the code for the head, and an indication of substitutions to perform in the body.
This could be handled by explicitly manipulating multiple values, but it seems complicated.

An alternate approach is to eliminate `compile-head` and just write `compile-body`.
This is possible if we in effect do a source-code transformation on the clause.
Instead of treating the clause as:

[ ](#){:#l0050}`(<− (likes Kim ?x)`
!!!(p) {:.unnumlist}

` (likes ?x Lee) (likes ?x Kim))`
!!!(p) {:.unnumlist}

we transform it to the equivalent:

[ ](#){:#l0055}`(<− (likes ?arg1 ?arg2)`
!!!(p) {:.unnumlist}

` (= ?arg1 Kim) (= ?arg2 ?x) (likes ?x Lee) (likes ?x Kim))`
!!!(p) {:.unnumlist}

Now the arguments in the head of the clause match the arguments in the function `likes/2`, so there is no need to generate any code for the head.
This makes things simpler by eliminating `compile-head`, and it is a better decomposition for another reason: instead of adding optimizations to `compile-head`, we will add them to the code in `compile`-`body` that handles =.
That way, we can optimize calls that the user makes to =, in addition to the calls introduced by the source-code transformation.

To get an overview, the calling sequence of functions will turn out to be as follows:

[ ](#){:#l0060}`prolog-compile`
!!!(p) {:.unnumlist}

` compile-predicate`
!!!(p) {:.unnumlist}

`  compile-clause`
!!!(p) {:.unnumlist}

`   compile-body`
!!!(p) {:.unnumlist}

`    compile-call`
!!!(p) {:.unnumlist}

`    compile-arg`
!!!(p) {:.unnumlist}

`    compile-unify`
!!!(p) {:.unnumlist}

`      compile-arg`
!!!(p) {:.unnumlist}

where each function calls the ones below it that are indented one level.
We have already defined the first two functions.
Here then is our first version of `compile-clause`:

[ ](#){:#l0065}`(defun compile-clause (parms clause cont)`
!!!(p) {:.unnumlist}

` "Transform away the head.
and compile the resulting body."`
!!!(p) {:.unnumlist}

` (compile-body`
!!!(p) {:.unnumlist}

`  (nconc`
!!!(p) {:.unnumlist}

`   (mapcar #'make-= parms (args (clause-head clause)))`
!!!(p) {:.unnumlist}

`   (clause-body clause))`
!!!(p) {:.unnumlist}

`  cont))`
!!!(p) {:.unnumlist}

`(defun make-= (x y) '(= .x .y))`
!!!(p) {:.unnumlist}

The bulk of the work is in `compile-body`, which is a little more complicated.
There are three cases.
If there is no body, we just call the continuation.
If the body starts with a call to =, we compile a call to `unify!`.
Otherwise, we compile a call to a function, passing in the appropriate continuation.

However, it is worthwhile to think ahead at this point.
If we want to treat = specially now, we will probably want to treat other goals specially later.
So instead of explicitly checking for =, we will do a data-driven dispatch, looking for any predicate that has a `prolog-compiler-macro` property attached to it.
Like Lisp compiler macros, the macro can decline to handle the goal.
We will adopt the convention that returning `:pass` means the macro decided not to handle i t, and thus it should be compiled as a normal goal.

[ ](#){:#l0070}`(defun compile-body (body cont)`
!!!(p) {:.unnumlist}

` "Compile the body of a clause."`
!!!(p) {:.unnumlist}

` (if (null body)`
!!!(p) {:.unnumlist}

`  '(funcall ,cont)`
!!!(p) {:.unnumlist}

`  (let* ((goal (first body))`
!!!(p) {:.unnumlist}

`    (macro (prolog-compiler-macro (predicate goal)))`
!!!(p) {:.unnumlist}

`    (macro-val (if macro`
!!!(p) {:.unnumlist}

`        (funcall macro goal (rest body) cont))))`
!!!(p) {:.unnumlist}

`  (if (and macro (not (eq macro-val :pass)))`
!!!(p) {:.unnumlist}

`    macro-val`
!!!(p) {:.unnumlist}

`    (compile-call`
!!!(p) {:.unnumlist}

`     (make-predicate (predicate goal)`
!!!(p) {:.unnumlist}

`        (relation-arity goal))`
!!!(p) {:.unnumlist}

`     (mapcar #'(lambda (arg) (compile-arg arg))`
!!!(p) {:.unnumlist}

`      (args goal))`
!!!(p) {:.unnumlist}

`     (if (null (rest body))`
!!!(p) {:.unnumlist}

`       cont`
!!!(p) {:.unnumlist}

`       '#'(lambda ()`
!!!(p) {:.unnumlist}

`        ,(compile-body (rest body) cont))))))))`
!!!(p) {:.unnumlist}

`(defun compile-call (predicate args cont)`
!!!(p) {:.unnumlist}

` "Compile a call to a prolog predicate."`
!!!(p) {:.unnumlist}

` '(,predicate ,@args ,cont))`
!!!(p) {:.unnumlist}

`(defun prolog-compiler-macro (name)`
!!!(p) {:.unnumlist}

` "Fetch the compiler macro for a Prolog predicate."`
!!!(p) {:.unnumlist}

` ;; Note NAME is the raw name, not the name/arity`
!!!(p) {:.unnumlist}

` (get name 'prolog-compiler-macro))`
!!!(p) {:.unnumlist}

`(defmacro def-prolog-compiler-macro (name arglist &body body)`
!!!(p) {:.unnumlist}

` "Define a compiler macro for Prolog."`
!!!(p) {:.unnumlist}

` '(setf (get '.name 'prolog-compiler-macro)`
!!!(p) {:.unnumlist}

`    #'(lambda .arglist .,body)))`
!!!(p) {:.unnumlist}

`(def-prolog-compiler-macro = (goal body cont)`
!!!(p) {:.unnumlist}

` (let ((args (args goal)))`
!!!(p) {:.unnumlist}

`  (if (/= (length args) 2)`
!!!(p) {:.unnumlist}

`    :pass`
!!!(p) {:.unnumlist}

`    '(if ,(compile-unify (first args) (second args))`
!!!(p) {:.unnumlist}

`      ,(compile-body body cont)))))`
!!!(p) {:.unnumlist}

`(defun compile-unify (x y)`
!!!(p) {:.unnumlist}

` "Return code that tests if var and term unify."`
!!!(p) {:.unnumlist}

` '(unify!
,(compile-arg x) ,(compile-arg y)))`
!!!(p) {:.unnumlist}

All that remains is `compile-arg`, a function to compile the arguments to goals in the body.
There are three cases to consider, as shown in the compilation to the argument of `q` below:

[ ](#){:#t0010}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `1 (<− (p ?x) (q ?x))` | `(q/1 ?x cont)` |
| `2 (<− (p ?x) (q (f a b)))` | `(q/1 '(f a b) cont)` |
| `3 (<− (p ?x) (q (f ?x b)))` | `(q/1 (list 'f ?x 'b) cont)` |

In case 1, the argument is a variable, and it is compiled as is.
In case 2, the argument is a constant expression (one without any variables) that compiles into a quoted expression.
In case 3, the argument contains a variable, so we have to generate code that builds up the expression.
Case 3 is actually split into two in the list below: one compiles into a call to `list`, and the other a call to `cons`.
It is important to remember that the goal `(q (f ?x b))` does *not* involve a call to the function `f`.
Rather, it involves the term `(f ?x b)`, which is just a list of three elements.

[ ](#){:#l0075}`(defun compile-arg (arg)`
!!!(p) {:.unnumlist}

` "Generate code for an argument to a goal in the body."`
!!!(p) {:.unnumlist}

` (cond ((variable-p arg) arg)`
!!!(p) {:.unnumlist}

`    ((not (has-variable-p arg)) ".arg)`
!!!(p) {:.unnumlist}

`    ((proper-listp arg)`
!!!(p) {:.unnumlist}

`      '(list .,(mapcar #'compile-arg arg)))`
!!!(p) {:.unnumlist}

`    (t '(cons ,(compile-arg (first arg))`
!!!(p) {:.unnumlist}

`        ,(compile-arg (rest arg))))))`
!!!(p) {:.unnumlist}

`(defun has-variable-p (x)`
!!!(p) {:.unnumlist}

` "Is there a variable anywhere in the expression x?"`
!!!(p) {:.unnumlist}

` (find-if-anywhere #'variable-p x))`
!!!(p) {:.unnumlist}

`(defun proper-listp (x)`
!!!(p) {:.unnumlist}

` "Is x a proper (non-dotted) list?"`
!!!(p) {:.unnumlist}

` (or (null x)`
!!!(p) {:.unnumlist}

`  (and (consp x) (proper-listp (rest x)))))`
!!!(p) {:.unnumlist}

Let's see how it works.
We will consider the following clauses:

[ ](#){:#l0080}`(<− (likes Robin cats))`
!!!(p) {:.unnumlist}

`(<− (likes Sandy ?x) (likes ?x cats))`
!!!(p) {:.unnumlist}

`(<− (likes Kim ?x) (likes ?x Lee) (likes ?x Kim))`
!!!(p) {:.unnumlist}

`(<− (member ?item (?item .
?rest)))`
!!!(p) {:.unnumlist}

`(<− (member ?item (?x .
?rest)) (member ?item ?rest))`
!!!(p) {:.unnumlist}

Here's what `prolog-compile` gives us:

[ ](#){:#l0085}`(DEFUN LIKES/2 (?ARG1 ?ARG2 CONT)`
!!!(p) {:.unnumlist}

` (IF (UNIFY!
?ARG1 'ROBIN)`
!!!(p) {:.unnumlist}

`  (IF (UNIFY!
?ARG2 'CATS)`
!!!(p) {:.unnumlist}

`   (FUNCALL CONT)))`
!!!(p) {:.unnumlist}

` (IF (UNIFY!
?ARG1 'SANDY)`
!!!(p) {:.unnumlist}

`  (IF (UNIFY!
?ARG2 ?X)`
!!!(p) {:.unnumlist}

`   (LIKES/2 ?X 'CATS CONT)))`
!!!(p) {:.unnumlist}

` (IF (UNIFY!
?ARG1 'KIM)`
!!!(p) {:.unnumlist}

`  (IF (UNIFY!
?ARG2 ?X)`
!!!(p) {:.unnumlist}

`   (LIKES/2 ?X 'LEE (LAMBDA ()`
!!!(p) {:.unnumlist}

`      (LIKES/2 ?X 'KIM CONT))))))`
!!!(p) {:.unnumlist}

`(DEFUN MEMBER/2 (?ARG1 ?ARG2 CONT)`
!!!(p) {:.unnumlist}

` (IF (UNIFY!
?ARG1 ?ITEM)`
!!!(p) {:.unnumlist}

`  (IF (UNIFY!
?ARG2 (CONS ?ITEM ?REST))`
!!!(p) {:.unnumlist}

`   (FUNCALL CONT)))`
!!!(p) {:.unnumlist}

` (IF (UNIFY!
?ARG1 ?ITEM)`
!!!(p) {:.unnumlist}

`  (IF (UNIFY!
?ARG2 (CONS ?X ?REST))`
!!!(p) {:.unnumlist}

`   (MEMBER/2 ?ITEM ?REST CONT))))`
!!!(p) {:.unnumlist}

## [ ](#){:#st0015}12.2 Fixing the Errors in the Compiler
{:#s0015}
{:.h1hd}

There are some problems in this version of the compiler:

* [ ](#){:#l0090}• We forgot to undo the bindings after each call to `unify!`.

* • The definition of `undo-bindings` !
defined previously requires as an argument an index into the `*trail*` array.
So we will have to save the current top of the trail when we enter each function.

* • Local variables, such as `?x`, were used without being introduced.
They should be bound to new variables.

Undoing the bindings is simple: we add a single line to `compile-predicate,` a call to the function `maybe-add-undo-bindings.` This function inserts a call to `undo-bindings!` after every failure.
If there is only one clause, no undoing is necessary, because the predicate higher up in the calling sequence will do it when it fails.
If there are multiple clauses, the function wraps the whole function body in a let that captures the initial value of the trail's fill pointer, so that the bindings can be undone to the right point.
Similarly, we can handle the unbound-variable problem by wrapping a call to `bind-unbound-vars` around each compiled clause:

[ ](#){:#l0095}`(defun compile-predicate (symbol arity clauses)`
!!!(p) {:.unnumlist}

` "Compile all the clauses for a given symbol/arity`
!!!(p) {:.unnumlist}

` into a single LISP function."`
!!!(p) {:.unnumlist}

` (let ((predicate (make-predicate symbol arity))`
!!!(p) {:.unnumlist}

`    (parameters (make-parameters arity)))`
!!!(p) {:.unnumlist}

`  (compile`
!!!(p) {:.unnumlist}

`   (eval`
!!!(p) {:.unnumlist}

`    '(defun .predicate (.©parameters cont)`
!!!(p) {:.unnumlist}

`     .,(maybe-add-undo-bindings       ;***`
!!!(p) {:.unnumlist}

`       (mapcar #'(lambda (clause)`
!!!(p) {:.unnumlist}

`            (compile-clause parameters`
!!!(p) {:.unnumlist}

`                  clause 'cont))`
!!!(p) {:.unnumlist}

`            clauses)))))))`
!!!(p) {:.unnumlist}

`(defun compile-clause (parms clause cont)`
!!!(p) {:.unnumlist}

` "Transform away the head, and compile the resulting body."`
!!!(p) {:.unnumlist}

` (bind-unbound-vars      ;***`
!!!(p) {:.unnumlist}

`  parms      ;***`
!!!(p) {:.unnumlist}

`  (compile-body`
!!!(p) {:.unnumlist}

`   (nconc`
!!!(p) {:.unnumlist}

`    (mapcar #'make-= parms (args (clause-head clause)))`
!!!(p) {:.unnumlist}

`    (clause-body clause))`
!!!(p) {:.unnumlist}

`   cont)))`
!!!(p) {:.unnumlist}

`(defun maybe-add-undo-bindings (compiled-exps)`
!!!(p) {:.unnumlist}

` "Undo any bindings that need undoing.`
!!!(p) {:.unnumlist}

` If there are any, bind the trail before we start."`
!!!(p) {:.unnumlist}

` (if (length=1 compiled-exps)`
!!!(p) {:.unnumlist}

`  compiled-exps`
!!!(p) {:.unnumlist}

`  '((let ((old-trail (fill-pointer *trail*)))`
!!!(p) {:.unnumlist}

`    ,(first compiled-exps)`
!!!(p) {:.unnumlist}

`    ,@(loop for exp in (rest compiled-exps)`
!!!(p) {:.unnumlist}

`      collect '(undo-bindings!
old-trail)`
!!!(p) {:.unnumlist}

`      collect exp)))))`
!!!(p) {:.unnumlist}

`(defun bind-unbound-vars (parameters exp)`
!!!(p) {:.unnumlist}

` "If there are any variables in exp (besides the parameters)`
!!!(p) {:.unnumlist}

` then bind them to new vars."`
!!!(p) {:.unnumlist}

` (let ((exp-vars (set-difference (variables-in exp)`
!!!(p) {:.unnumlist}

`        parameters)))`
!!!(p) {:.unnumlist}

`  (if exp-vars`
!!!(p) {:.unnumlist}

`   '(let ,(mapcar #'(lambda (var) '(.var (?)))`
!!!(p) {:.unnumlist}

`      exp-vars)`
!!!(p) {:.unnumlist}

`     ,exp)`
!!!(p) {:.unnumlist}

`   exp)))`
!!!(p) {:.unnumlist}

With these improvements, here's the code we get for `likes` and `member`:

[ ](#){:#l0100}`(DEFUN LIKES/2 (?ARG1 ?ARG2 CONT)`
!!!(p) {:.unnumlist}

` (LET ((OLD-TRAIL (FILL-POINTER *TRAIL*)))`
!!!(p) {:.unnumlist}

`  (IF (UNIFY!
?ARG1 'ROBIN)`
!!!(p) {:.unnumlist}

`   (IF (UNIFY!
?ARG2 'CATS)`
!!!(p) {:.unnumlist}

`      (FUNCALL CONT)))`
!!!(p) {:.unnumlist}

`  (UNDO-BINDINGS!
OLD-TRAIL)`
!!!(p) {:.unnumlist}

`  (LET ((?X (?)))`
!!!(p) {:.unnumlist}

`   (IF (UNIFY!
?ARG1 'SANDY)`
!!!(p) {:.unnumlist}

`    (IF (UNIFY!
?ARG2 ?X)`
!!!(p) {:.unnumlist}

`      (LIKES/2 ?X 'CATS CONT))))`
!!!(p) {:.unnumlist}

`  (UNDO-BINDINGS!
OLD-TRAIL)`
!!!(p) {:.unnumlist}

`  (LET ((?X (?)))`
!!!(p) {:.unnumlist}

`   (IF (UNIFY!
?ARG1 'KIM)`
!!!(p) {:.unnumlist}

`    (IF (UNIFY!
?ARG2 ?X)`
!!!(p) {:.unnumlist}

`      (LIKES/2 ?X 'LEE (LAMBDA ()`
!!!(p) {:.unnumlist}

`          (LIKES/2 ?X 'KIM CONT))))))))`
!!!(p) {:.unnumlist}

`(DEFUN MEMBER/2 (?ARG1 ?ARG2 CONT)`
!!!(p) {:.unnumlist}

` (LET ((OLD-TRAIL (FILL-POINTER *TRAIL*)))`
!!!(p) {:.unnumlist}

`  (LET ((?ITEM (?))`
!!!(p) {:.unnumlist}

`      (?REST (?)))`
!!!(p) {:.unnumlist}

`   (IF (UNIFY!
?ARG1 ?ITEM)`
!!!(p) {:.unnumlist}

`      (IF (UNIFY!
?ARG2 (CONS ?ITEM ?REST))`
!!!(p) {:.unnumlist}

`            (FUNCALL CONT))))`
!!!(p) {:.unnumlist}

`  (UNDO-BINDINGS!
OLD-TRAIL)`
!!!(p) {:.unnumlist}

`  (LET ((?X (?))`
!!!(p) {:.unnumlist}

`      (?
ITEM (?))`
!!!(p) {:.unnumlist}

`      (?REST (?)))`
!!!(p) {:.unnumlist}

`  (IF (UNIFY!
?ARG1 ?ITEM)`
!!!(p) {:.unnumlist}

`   (IF (UNIFY!
?ARG2 (CONS ?X ?REST))`
!!!(p) {:.unnumlist}

`            (MEMBER/2 ?ITEM ?REST CONT))))))`
!!!(p) {:.unnumlist}

## [ ](#){:#st0020}12.3 Improving the Compiler
{:#s0020}
{:.h1hd}

This is fairly good, although there is still room for improvement.
One minor improvement is to eliminate unneeded variables.
For example, `?rest` in the first clause of `member` and `?x` in the second clause are bound to new variables—the result of the (?) call—and then only used once.
The generated code could be made a little tighter by just putting (?) inline, rather than binding it to a variable and then referencing that variable.
There are two parts to this change: updating `compile-arg` to compile an anonymous variable inline, and changing the <- macro so that it converts all variables that only appear once in a clause into anonymous variables:

[ ](#){:#l0105}`(defmacro <− (&rest clause)`
!!!(p) {:.unnumlist}

` "Add a clause to the data base."`
!!!(p) {:.unnumlist}

` '(add-clause ',(make-anonymous clause)))`
!!!(p) {:.unnumlist}

`(defun compile-arg (arg)`
!!!(p) {:.unnumlist}

` "Generate code for an argument to a goal in the body."`
!!!(p) {:.unnumlist}

` (cond ((eq arg '?) '(?))      ;***`
!!!(p) {:.unnumlist}

`   ((variable-p arg) arg)`
!!!(p) {:.unnumlist}

`   ((not (has-variable-p arg)) '',arg)`
!!!(p) {:.unnumlist}

`   ((proper-listp arg)`
!!!(p) {:.unnumlist}

`   '(list (mapcar #'compile-arg arg)))`
!!!(p) {:.unnumlist}

`   (t '(cons ,(compile-arg (first arg))`
!!!(p) {:.unnumlist}

`          ,(compile-arg (rest arg))))))`
!!!(p) {:.unnumlist}

`(defun make-anonymous (exp &optional`
!!!(p) {:.unnumlist}

`             (anon-vars (anonymous-variables-in exp)))`
!!!(p) {:.unnumlist}

` "Replace variables that are only used once with ?."`
!!!(p) {:.unnumlist}

` (cond ((consp exp)`
!!!(p) {:.unnumlist}

`   (reuse-cons (make-anonymous (first exp) anon-vars)`
!!!(p) {:.unnumlist}

`     (make-anonymous (rest exp) anon-vars)`
!!!(p) {:.unnumlist}

`     exp))`
!!!(p) {:.unnumlist}

`((member exp anon-vars) '?)`
!!!(p) {:.unnumlist}

`(t exp)))`
!!!(p) {:.unnumlist}

Finding anonymous variables is tricky.
The following function keeps two lists: the variables that have been seen once, and the variables that have been seen twice or more.
The local function `walk` is then used to walk over the tree, recursively considering the components of each cons cell and updating the two lists as each variable is encountered.
This use of local functions should be remembered, as well as an alternative discussed in [exercise 12.23](#p4625) on [page 428](#p428).

[ ](#){:#l0110}`(defun anonymous-variables-in (tree)`
!!!(p) {:.unnumlist}

` "Return a list of all variables that occur only once in tree."`
!!!(p) {:.unnumlist}

` (let ((seen-once nil)`
!!!(p) {:.unnumlist}

`      (seen-more nil))`
!!!(p) {:.unnumlist}

`  (labels ((walk (x)`
!!!(p) {:.unnumlist}

`      (cond`
!!!(p) {:.unnumlist}

`        ((variable-p x)`
!!!(p) {:.unnumlist}

`          (cond ((member x seen-once)`
!!!(p) {:.unnumlist}

`               (setf seen-once (delete x seen-once))`
!!!(p) {:.unnumlist}

`               (push x seen-more))`
!!!(p) {:.unnumlist}

`            ((member x seen-more) nil)`
!!!(p) {:.unnumlist}

`            (t (push x seen-once))))`
!!!(p) {:.unnumlist}

`        ((consp x)`
!!!(p) {:.unnumlist}

`          (walk (first x))`
!!!(p) {:.unnumlist}

`          (walk (rest x))))))`
!!!(p) {:.unnumlist}

`   (walk tree)`
!!!(p) {:.unnumlist}

`   seen-once)))`
!!!(p) {:.unnumlist}

Now `member` compiles into this:

[ ](#){:#l0115}`(DEFUN MEMBER/2 (?ARG1 ?ARG2 CONT)`
!!!(p) {:.unnumlist}

 `(LET ((OLD-TRAIL (FILL-POINTER *TRAIL*)))`
!!!(p) {:.unnumlist}

  `(LET ((?ITEM (?)))`
!!!(p) {:.unnumlist}

   `(IF (UNIFY!
?ARG1 ?ITEM)`
!!!(p) {:.unnumlist}

    `(IF (UNIFY!
?ARG2 (CONS ?ITEM (?)))`
!!!(p) {:.unnumlist}

        `(FUNCALL CONT))))`
!!!(p) {:.unnumlist}

  `(UNDO-BINDINGS!
OLD-TRAIL)`
!!!(p) {:.unnumlist}

`  (LET ((?ITEM (?))`
!!!(p) {:.unnumlist}

`    (?REST (?)))`
!!!(p) {:.unnumlist}

`   (IF (UNIFY!
?ARG1 ?ITEM)`
!!!(p) {:.unnumlist}

`    (IF (UNIFY!
?ARG2 (CONS (?) ?REST))`
!!!(p) {:.unnumlist}

`      (MEMBER/2 ?ITEM ?REST CONT))))))`
!!!(p) {:.unnumlist}

## [ ](#){:#st0025}12.4 Improving the Compilation of Unification
{:#s0025}
{:.h1hd}

Now we turn to the improvement of `compile-unify`.
Recall that we want to elimina te certain calls to `unify!` so that, for example, the first clause of `member:`

[ ](#){:#l0120}`(<− (member ?item (?item .
?rest)))`
!!!(p) {:.unnumlist}

compiles into:

[ ](#){:#l9120}`(LET ((?ITEM (?)))`
!!!(p) {:.unnumlist}

` (IF (UNIFY!
?ARG1 ?ITEM)`
!!!(p) {:.unnumlist}

`  (IF (UNIFY!
?ARG2 (CONS ?ITEM (?)))`
!!!(p) {:.unnumlist}

`    (FUNCALL CONT))))`
!!!(p) {:.unnumlist}

when it could compile to the more efficient:

[ ](#){:#l9420}`(IF (UNIFY!
?ARG2 (CONS ?ARG1 (?)))`
!!!(p) {:.unnumlist}

`  (FUNCALL CONT))`
!!!(p) {:.unnumlist}

Eliminating the unification in one goal has repercussions in other goals later on, so we will need to keep track of expressions that have been unified together.
We have a design choice.
Either `compile-unify` can modify a global state variable, or it can return multiple values.
On the grounds that global variables are messy, we make the second choice: `compile-unify` will take a binding list as an extra argument and will return two values, the actual code and an updated binding list.
We will expect that other related functions will have to be modified to deal with these multiple values.

When `compile-unify` is first called in our example clause, it is asked to unify `?argl` and `?item`.
We want it to return no code (or more precisely, the trivially true test, t).
For the second value, it should return a new binding list, with `?item` bound to `?arg1.` That binding will be used to replace `?item` with `?arg1` in subsequent code.

How do we know to bind `?item` to `?arg1` rather than the other way around?
Because `?arg1` is already bound to something—the value passed in to `member.` We don't know what this value is, but we can't ignore it.
Thus, the initial binding list will have to indicate that the parameters are bound to something.
A simple convention is to bind the parameters to themselves.
Thus, the initial binding list will be:

[ ](#){:#l0125}`((?arg1 .?arg1) (?arg2 .
?arg2))`
!!!(p) {:.unnumlist}

We saw in the previous chapter ([page 354](B978008057115750011X.xhtml#p354)) that binding a variable to itself can lead to problems; we will have to be careful.

Besides eliminating unifications of new variables against parameters, there are quite a few other improvements that can be made.
For example, unifications involving only constants can be done at compile time.
The call `(= (f a) (f a ))` always succeeds, while `(= 3 4)` always fails.
In addition, unification of two cons cells can be broken into components at compile time: `(= (f ?x) (f a))` reduces to `(= ?x a)` and `(= f f)`, where the latter trivially succeeds.
We can even do some occurs checking at compile time: `(= ?x (f ?x))` should fail.

The following table lists these improvements, along with a breakdown for the cases of unifying a bound `(?arg1)` or unbound `(?x)` variable agains another expression.
The first column is the unification call, the second is the generated code, and the third is the bindings that will be added as a resuit of the call:

[ ](#){:#t0015}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| | Unification | Code | Bindings |
| 1 | `(= 3 3)` | `t` | `—` |
| 2 | `(= 3 4)` | `nil` | `—` |
| 3 | `(= (f ?x) (?p 3))` | `t` | `(?x . 3) (?p . f)` |
| 4 | `(= ?arg1 ?y)` | `t` | `(?y . ?arg1)` |
| 5 | `(= ?arg1 ?arg2)` | `(unify! ?arg1 ?arg2)` | `(?arg1 . ?arg2)` |
| 6 | `(= ?arg1 3)` | `(unify! ?arg1 3)` | `(?arg1 . 3)` |
| 7 | `(= ?arg1 (f ? y))` | `(unify! ?arg1 . . . )` | `(?y . ?y)` |
| 8 | `(= ?x ?y)` | `t` | `(?y . ?y)` |
| 9 | `(= ?x 3)` | `t` | `(?x . 3)` |
| 10 | `(= ?x (f ? y))` | `(unify! ?x . . . )` | `(?y . ?y)` |
| 11 | `(= ?x (f ? x))` | `nil` | `—` |
| 12 | `(= ?x ?)` | `t` | `—` |

![t0015](images/B9780080571157500121/t0015.png)

From this table we can craft our new version of `compile-unify`.
The first part is fairly easy.
It takes care of the first three cases in this table and makes sure that `compile-unify-variable` is called with a variable as the first argument for the other cases.

[ ](#){:#l0130}`(defun compile-unify (x y bindings)`
!!!(p) {:.unnumlist}

` "Return 2 values: code to test if x and y unify,`
!!!(p) {:.unnumlist}

` and a new binding list."`
!!!(p) {:.unnumlist}

` (cond`
!!!(p) {:.unnumlist}

`  ;; Unify constants and conses:          ; Case`
!!!(p) {:.unnumlist}

`  ((not (or (has-variable-p x) (has-variable-p y)))          ; 1.2`
!!!(p) {:.unnumlist}

`   (values (equal x y) bindings))`
!!!(p) {:.unnumlist}

`  ((and (consp x) (consp y))          ; 3`
!!!(p) {:.unnumlist}

`   (multiple-value-bind (code1 bindings1)`
!!!(p) {:.unnumlist}

`     (compile-unify (first x) (first y) bindings)`
!!!(p) {:.unnumlist}

`    (multiple-value-bind (code2 bindings2)`
!!!(p) {:.unnumlist}

`      (compile-unify (rest x) (rest y) bindings1)`
!!!(p) {:.unnumlist}

`     (values (compile-if code1 code2) bindings2))))`
!!!(p) {:.unnumlist}

`  ;; Here x or y is a variable.
Pick the right one:`
!!!(p) {:.unnumlist}

`  ((variable-p x) (compile-unify-variable x y bindings))`
!!!(p) {:.unnumlist}

`  (t      (compile-unify-variable y x bindings))))`
!!!(p) {:.unnumlist}

`(defun compile-if (pred then-part)`
!!!(p) {:.unnumlist}

` "Compile a Lisp IF form.
No else-part allowed."`
!!!(p) {:.unnumlist}

` (case pred`
!!!(p) {:.unnumlist}

`  ((t) then-part)`
!!!(p) {:.unnumlist}

`  ((nil) nil)`
!!!(p) {:.unnumlist}

`  (otherwise '(if .pred .then-part))))`
!!!(p) {:.unnumlist}

The function `compile-unify-variable` following is one of the most complex we have seen.
For each argument, we see if it has a binding (the local variables `xb` and `yb`), and then use the bindings to get the value of each argument (`x1` and `y1`).
Note that for either an unbound variable or one bound to itself, `x` will equal `x1` (and the same for `y` and `y1`).
If either of the pairs of values is not equal, we should use the new ones (`x1` or `y1`), and the clause commented deref does that.
After that point, we just go through the cases, one at a time.
It turns out that it was easier to change the order slightly from the preceding table, but each clause is commented with the corresponding number:

[ ](#){:#l0135}`(defun compile-unify-variable (x y bindings)`
!!!(p) {:.unnumlist}

` "X is a variable, and Y may be."`
!!!(p) {:.unnumlist}

` (let* ((xb (follow-binding x bindings))`
!!!(p) {:.unnumlist}

`   (x1 (if xb (cdr xb) x))`
!!!(p) {:.unnumlist}

`   (yb (if (variable-p y) (follow-binding y bindings)))`
!!!(p) {:.unnumlist}

`   (y1 (if yb (cdr yb) y)))`
!!!(p) {:.unnumlist}

` (cond       ; Case:`
!!!(p) {:.unnumlist}

`  ((or (eq x '?) (eq y '?)) (values t bindings))      ; 12`
!!!(p) {:.unnumlist}

`  ((not (and (equal x x1) (equal y y1)))      ; deref`
!!!(p) {:.unnumlist}

`    (compile-unify x1 y1 bindings))`
!!!(p) {:.unnumlist}

`  ((find-anywhere x1 y1) (values nil bindings))      ; 11`
!!!(p) {:.unnumlist}

`  ((consp y1)      ; 7.10`
!!!(p) {:.unnumlist}

`  (values '(unify!
,xl ,(compile-arg y1 bindings))`
!!!(p) {:.unnumlist}

`      (bind-variables-in y1 bindings)))`
!!!(p) {:.unnumlist}

`  ((not (null xb))`
!!!(p) {:.unnumlist}

`  ;; i.e.
x is an ?arg variable`
!!!(p) {:.unnumlist}

`  (if (and (variable-p y1) (null yb))`
!!!(p) {:.unnumlist}

`   (values 't (extend-bindings y1 x1 bindings))      ; 4`
!!!(p) {:.unnumlist}

`   (values '(unify!
,xl .(compile-arg y1 bindings))`
!!!(p) {:.unnumlist}

`      (extend-bindings x1 y1 bindings))))      ; 5.6`
!!!(p) {:.unnumlist}

`  ((not (null yb))`
!!!(p) {:.unnumlist}

`   (compile-unify-variable y1 x1 bindings))`
!!!(p) {:.unnumlist}

`  (t (values 't (extend-bindings x1 y1 bindings))))))      ; 8.9`
!!!(p) {:.unnumlist}

Take some time to understand just how this function works.
Then go on to the following auxiliary functions:

[ ](#){:#l0140}`(defun bind-variables-in (exp bindings)`
!!!(p) {:.unnumlist}

` "Bind all variables in exp to themselves.
and add that to`
!!!(p) {:.unnumlist}

` bindings (except for variables already bound)."`
!!!(p) {:.unnumlist}

` (dolist (var (variables-in exp))`
!!!(p) {:.unnumlist}

`  (unless (get-binding var bindings)`
!!!(p) {:.unnumlist}

`   (setf bindings (extend-bindings var var bindings))))`
!!!(p) {:.unnumlist}

` bindings)`
!!!(p) {:.unnumlist}

`(defun follow-binding (var bindings)`
!!!(p) {:.unnumlist}

` "Get the ultimate binding of var according to bindings."`
!!!(p) {:.unnumlist}

` (let ((b (get-binding var bindings)))`
!!!(p) {:.unnumlist}

`  (if (eq (car b) (cdr b)) b`
!!!(p) {:.unnumlist}

`    b`
!!!(p) {:.unnumlist}

`      (or (follow-binding (cdr b) bindings)`
!!!(p) {:.unnumlist}

`          b))))`
!!!(p) {:.unnumlist}

Now we need to integrate the new `compile-unify` into the rest of the compiler.
The problem is that the new version takes an extra argument and returns an extra value, so all the functions that call it need to be changed.
Let's look again at the calling sequence:

[ ](#){:#l0145}`prolog-compile`
!!!(p) {:.unnumlist}

` compile-predicate`
!!!(p) {:.unnumlist}

`  compile-clause`
!!!(p) {:.unnumlist}

`   compile-body`
!!!(p) {:.unnumlist}

`    compile-call`
!!!(p) {:.unnumlist}

`    compile-arg`
!!!(p) {:.unnumlist}

`     compile-unify`
!!!(p) {:.unnumlist}

`      compile-arg`
!!!(p) {:.unnumlist}

First, going downward, we see that `compile-arg` needs to take a binding list as an argument, so that it can look up and substitute in the appropriate values.
But it will not alter the binding list, so it still returns one value:

[ ](#){:#l0150}`(defun compile-arg (arg bindings)`
!!!(p) {:.unnumlist}

` "Generate code for an argument to a goal in the body."`
!!!(p) {:.unnumlist}

` (cond ((eq arg '?) '(?))`
!!!(p) {:.unnumlist}

`   ((variable-p arg)`
!!!(p) {:.unnumlist}

`    (let ((binding (get-binding arg bindings)))`
!!!(p) {:.unnumlist}

`      (if (and (not (null binding))`
!!!(p) {:.unnumlist}

`        (not (eq arg (binding-val binding))))`
!!!(p) {:.unnumlist}

`       (compile-arg (binding-val binding) bindings)`
!!!(p) {:.unnumlist}

`       arg)))`
!!!(p) {:.unnumlist}

`    ((not (find-if-anywhere #'variable-p arg)) ",arg)`
!!!(p) {:.unnumlist}

`    ((proper-listp arg)`
!!!(p) {:.unnumlist}

`     '(1ist .,(mapcar #'(lambda (a) (compile-arg a bindings))`
!!!(p) {:.unnumlist}

`          arg)))`
!!!(p) {:.unnumlist}

`       (t '(cons ,(compile-arg (first arg) bindings)`
!!!(p) {:.unnumlist}

`         ,(compile-arg (rest arg) bindings)))))`
!!!(p) {:.unnumlist}

Now, going upward, `compile-body` needs to take a binding list and pass it on to various functions:

[ ](#){:#l0155}`(defun compile-body (body cont bindings)`
!!!(p) {:.unnumlist}

` "Compile the body of a clause."`
!!!(p) {:.unnumlist}

` (cond`
!!!(p) {:.unnumlist}

`  ((null body)`
!!!(p) {:.unnumlist}

`   '(funcall .cont))`
!!!(p) {:.unnumlist}

`  (t (let* ((goal (first body))`
!!!(p) {:.unnumlist}

`     (macro (prolog-compiler-macro (predicate goal)))`
!!!(p) {:.unnumlist}

`     (macro-val (if macro`
!!!(p) {:.unnumlist}

`        (funcall macro goal (rest body)`
!!!(p) {:.unnumlist}

`          cont bindings))))`
!!!(p) {:.unnumlist}

`   (if (and macro (not (eq macro-val :pass)))`
!!!(p) {:.unnumlist}

`     macro-val`
!!!(p) {:.unnumlist}

`     (compile-call`
!!!(p) {:.unnumlist}

`      (make-predicate (predicate goal)`
!!!(p) {:.unnumlist}

`         (relation-arity goal))`
!!!(p) {:.unnumlist}

`      (mapcar #'(lambda (arg)`
!!!(p) {:.unnumlist}

`        (compile-arg arg bindings))`
!!!(p) {:.unnumlist}

`       (args goal))`
!!!(p) {:.unnumlist}

`      (if (null (rest body))`
!!!(p) {:.unnumlist}

`       cont`
!!!(p) {:.unnumlist}

`        '#'(lambda ()`
!!!(p) {:.unnumlist}

`         .(compile-body`
!!!(p) {:.unnumlist}

`           (rest body) cont`
!!!(p) {:.unnumlist}

`           (bind-new-variables bindings goal))))))))))`
!!!(p) {:.unnumlist}

The function `bind-new-variables` takes any variables mentioned in the goal that have not been bound yet and binds these variables to themselves.
This is because the goal, whatever it is, may bind its arguments.

[ ](#){:#l0160}`(defun bind-new-variables (bindings goal)`
!!!(p) {:.unnumlist}

` "Extend bindings to include any unbound variables in goal."`
!!!(p) {:.unnumlist}

` (let ((variables (remove-if #'(lambda (v) (assoc v bindings))`
!!!(p) {:.unnumlist}

`          (variables-in goal))))`
!!!(p) {:.unnumlist}

`   (nconc (mapcar #'self-cons variables) bindings)))`
!!!(p) {:.unnumlist}

`(defun self-cons (x) (cons x x))`
!!!(p) {:.unnumlist}

One of the functions that needs to be changed to accept a binding list is the compiler macro for =:

[ ](#){:#l0165}`(def-prolog-compi1er-macro = (goal body cont bindings)`
!!!(p) {:.unnumlist}

` "Compile a goal which is a call to =."`
!!!(p) {:.unnumlist}

` (let ((args (args goal)))`
!!!(p) {:.unnumlist}

`  (if (/= (length args) 2)`
!!!(p) {:.unnumlist}

`    :pass ;; decline to handle this goal`
!!!(p) {:.unnumlist}

`    (multiple-value-bind (code1 bindings1)`
!!!(p) {:.unnumlist}

`      (compile-unify (first args) (second args) bindings)`
!!!(p) {:.unnumlist}

`     (compile-if`
!!!(p) {:.unnumlist}

`      code1`
!!!(p) {:.unnumlist}

`      (compile-body body cont bindings1))))))`
!!!(p) {:.unnumlist}

The last step upward is to change `compile-clause` so that it starts everything off by passing in to `compile-body` a binding list with all the parameters bound to themselves:

[ ](#){:#l0170}`(defun compile-clause (parms clause cont)`
!!!(p) {:.unnumlist}

` "Transform away the head, and compile the resulting body."`
!!!(p) {:.unnumlist}

` (bind-unbound-vars`
!!!(p) {:.unnumlist}

`  parms`
!!!(p) {:.unnumlist}

`  (compile-body`
!!!(p) {:.unnumlist}

`    (nconc`
!!!(p) {:.unnumlist}

`      (mapcar #'make-= parms (args (clause-head clause)))`
!!!(p) {:.unnumlist}

`      (clause-body clause))`
!!!(p) {:.unnumlist}

`    cont`
!!!(p) {:.unnumlist}

`    (mapcar #'self-cons parms)))) ;***`
!!!(p) {:.unnumlist}

Finally, we can see the fruits of our efforts:

[ ](#){:#l0175}`(DEFUN MEMBER/2 (?ARG1 ?ARG2 CONT)`
!!!(p) {:.unnumlist}

` (LET ((OLD-TRAIL (FILL-POINTER *TRAIL*)))`
!!!(p) {:.unnumlist}

`  (IF (UNIFY!
?ARG2 (CONS ?ARG1 (?)))`
!!!(p) {:.unnumlist}

`      (FUNCALL CONT))`
!!!(p) {:.unnumlist}

`  (UNDO-BINDINGS!
OLD-TRAIL)`
!!!(p) {:.unnumlist}

`  (LET ((?REST (?)))`
!!!(p) {:.unnumlist}

`    (IF (UNIFY!
?ARG2 (CONS (?) ?REST))`
!!!(p) {:.unnumlist}

`        (MEMBER/2 ?ARG1 ?REST CONT)))))`
!!!(p) {:.unnumlist}

` (DEFUN LIKES/2 (?ARG1 ?ARG2 CONT)`
!!!(p) {:.unnumlist}

`  (LET ((OLD-TRAIL (FILL-POINTER *TRAIL*)))`
!!!(p) {:.unnumlist}

`    (IF (UNIFY!
?ARG1 'ROBIN)`
!!!(p) {:.unnumlist}

`        (IF (UNIFY!
?ARG2 'CATS)`
!!!(p) {:.unnumlist}

`          (FUNCALL CONT)))`
!!!(p) {:.unnumlist}

`    (UNDO-BINDINGS!
OLD-TRAIL)`
!!!(p) {:.unnumlist}

`    (IF (UNIFY!
?ARG1 'SANDY)`
!!!(p) {:.unnumlist}

`      (LIKES/2 ?ARG2 'CATS CONT))`
!!!(p) {:.unnumlist}

`    (UNDO-BINDINGS!
OLD-TRAIL)`
!!!(p) {:.unnumlist}

`    (IF (UNIFY!
?ARG1 'KIM)`
!!!(p) {:.unnumlist}

`      (LIKES/2 ?ARG2 'LEE (LAMBDA ()`
!!!(p) {:.unnumlist}

`            (LIKES/2 ?ARG2 'KIM CONT))))))`
!!!(p) {:.unnumlist}

## [ ](#){:#st0030}12.5 Further Improvements to Unification
{:#s0030}
{:.h1hd}

Could `compile-unify` be improved yet again?
If we insist that it call `unify!,` it seems that it can't be made much better.
However, we could improve it by in effect compiling `unify!.` This is a key idea in the Warren Abstract Machine, or WAM, which is the most commonly used model for Prolog compilers.

We call `unify!` in four cases (5, 6, 7, and 10), and in each case the first argument is a variable, and we know something about the second argument.
But the first thing `unify!` does is redundantly test if the first argument is a variable.
We could eliminate unnecessary tests by calling more specialized functions rather than the general-purpose function `unify!`.
Consider this call:

[ ](#){:#l0180}`(unify!
?arg2 (cons ?arg1 (?)))`
!!!(p) {:.unnumlist}

If `?arg2` is an unbound variable, this code is appropriate.
But if `?arg2` is a constant atom, we should fail immediately, without allowing `cons` and `?` to generate garbage.
We could change the test to:

[ ](#){:#l0185}`(and (consp-or-variable-p ?arg2)`
!!!(p) {:.unnumlist}

`  (unify-first!
?arg2 ?arg1)`
!!!(p) {:.unnumlist}

`  (unify-rest!
?arg2 (?)))`
!!!(p) {:.unnumlist}

with suitable definitions for the functions referenced here.
This change should speed execution time and limit the amount of garbage generated.
Of course, it makes the generated code longer, so that could slow things down if the program ends up spending too much time bringing the code to the processor.

**Exercise 12.1 [h]** Write definitions for `consp-or-variable-p, unify-first!,` and `unify-rest!`, and change the compiler to generate code like that outlined previously.
You might want to look at the function `compile-rule` in [section 9.6](B9780080571157500091.xhtml#s0035), starting on [page 300](B9780080571157500091.xhtml#p300).
This function compiled a call to `pat-match` into individual tests; now we want to do the same thing to `unify!`.
Run some benchmarks to compare the altered compiler to the original version.

**Exercise 12.2 [h]** We can gain some more efficiency by keeping track of which variables have been dereferenced and calling an appropriate unification function: either one that dereferences the argument or one that assumes the argument has already been dereferenced.
Implement this approach.

**Exercise 12.3 [m]** What code is generated for `(= (f (g ?x) ?y) (f ?y (?p a)))?`What more efficient code represents the same unification?
How easy is it to change the compiler to get this more efficient result?

**Exercise 12.4 [h]** In retrospect, it seems that binding variables to themselves, as in `(?argl .
?argl`), was not such a good idea.
It complicates the meaning of bindings, and prohibits us from using existing tools.
For example, I had to use `find-anywhere` instead of `occur-check` for case 11, because `occur-check` expects a noncircular binding list.
But find-anywhere does not do as complete a job as `occur-check`.
Write a version of `compile-unify` that returns three values: the code, a noncircular binding list, and a list of variables that are bound to unknown values.

**Exercise 12.5 [h]** An alternative to the previous exercise is not to use binding lists at ail.
Instead, we could pass in a list of equivalence classes—that is, a list of lists, where each sublist contains one or more elements that have been unified.
In this approach, the initial equivalence class list would be `((?arg1) (?arg2))`.
After unifying `?arg1` with `?x`, `?arg2` with `?y`, and `?x` with 4, the list would be ( `(4 ?arg1 ?x) (?arg2 ?y))`.
This assumes the convention that the canonical member of an equivalence class (the one that will be substituted for all others) cornes first.
Implement this approach.
What advantages and disadvantages does it have?

## [ ](#){:#st0035}12.6 The User Interface to the Compiler
{:#s0035}
{:.h1hd}

The compiler can translate Prolog to Lisp, but that does us no good unless we can conveniently arrange to compile the right Prolog relations and call the right Lisp functions.
In other words, we have to integrate the compiler with the `<−` and `?` macros.
Surprisingly, we don't need to change these macros at all.
Rather, we will change the functions these macros call.
When a new clause is entered, we will enter the clause's predicate in the list `*uncompiled*`.
This is a one-line addition to `add-clause:`

[ ](#){:#l0190}`(defvar *uncompiled* nil`
!!!(p) {:.unnumlist}

`      "Prolog symbols that have not been compiled.")`
!!!(p) {:.unnumlist}

`(defun add-clause (clause)`
!!!(p) {:.unnumlist}

` "Add a clause to the data base, indexed by head's predicate."`
!!!(p) {:.unnumlist}

` ;; The predicate must be a non-variable symbol.`
!!!(p) {:.unnumlist}

` (let ((pred (predicate (clause-head clause))))`
!!!(p) {:.unnumlist}

`   (assert (and (symbolp pred) (not (variable-p pred))))`
!!!(p) {:.unnumlist}

`   (pushnew pred *db-predicates*)`
!!!(p) {:.unnumlist}

`   (pushnew pred *uncompiled*)      ;***`
!!!(p) {:.unnumlist}

`   (setf (get pred 'clauses)`
!!!(p) {:.unnumlist}

`       (nconc (get-clauses pred) (list clause)))`
!!!(p) {:.unnumlist}

`     pred))`
!!!(p) {:.unnumlist}

Now when a query is made, the ?- macro expands into a call to `top-level-prove.` The list of goals in the query, along with the `show-prolog-vars` goal, is added as the sole clause for the relation `top-level-query.` Next, that query, along with any others that are on the uncompiled list, are compiled.
Finally, the newly compiled top-level query function is called.

[ ](#){:#l0195}`(defun top-level-prove (goals)`
!!!(p) {:.unnumlist}

` "Prove the list of goals by compiling and calling it."`
!!!(p) {:.unnumlist}

` ;; First redefine top-level-query`
!!!(p) {:.unnumlist}

` (clear-predicate 'top-level-query)`
!!!(p) {:.unnumlist}

` (let ((vars (delete '?
(variables-in goals))))`
!!!(p) {:.unnumlist}

`  (add-clause '((top-level-query)`
!!!(p) {:.unnumlist}

`        ,@goals`
!!!(p) {:.unnumlist}

`       (show-prolog-vars ,(mapcar #'symbol-name vars)`
!!!(p) {:.unnumlist}

`          ,vars))))`
!!!(p) {:.unnumlist}

` ;; Now run it`
!!!(p) {:.unnumlist}

` (run-prolog 'top-level-query/0 #'ignore)`
!!!(p) {:.unnumlist}

` (format t "~&No.")`
!!!(p) {:.unnumlist}

` (values))`
!!!(p) {:.unnumlist}

`(defun run-prolog (procedure cont)`
!!!(p) {:.unnumlist}

` "Run a 0-ary prolog procedure with a given continuation."`
!!!(p) {:.unnumlist}

` ;; First compile anything else that needs it`
!!!(p) {:.unnumlist}

` (prolog-compi1e-symbols)`
!!!(p) {:.unnumlist}

` ;; Reset the trail and the new variable counter`
!!!(p) {:.unnumlist}

` (setf (fill-pointer *trail*) 0)`
!!!(p) {:.unnumlist}

` (setf *var-counter* 0)`
!!!(p) {:.unnumlist}

` ;; Finally.
call the query`
!!!(p) {:.unnumlist}

` (catch 'top-level-prove`
!!!(p) {:.unnumlist}

`   (funcall procedure cont)))`
!!!(p) {:.unnumlist}

`(defun prolog-compile-symbols &optional (symbols *uncompiled*))`
!!!(p) {:.unnumlist}

` "Compile a list of Prolog symbols.`
!!!(p) {:.unnumlist}

` By default.
the list is all symbols that need it."`
!!!(p) {:.unnumlist}

` (mapc #'prolog-compile symbols)`
!!!(p) {:.unnumlist}

` (setf *uncompiled* (set-difference *uncompiled* symbols)))`
!!!(p) {:.unnumlist}

`(defun ignore (&rest args)`
!!!(p) {:.unnumlist}

` (declare (ignore args))`
!!!(p) {:.unnumlist}

` nil)`
!!!(p) {:.unnumlist}

Note that at the top level, we don't need the continuation to do anything.
Arbitrarily, we chose to pass in the function `ignore`, which is defined to ignore its arguments.
This function is useful in a variety of places; some programmers will proclaim it inline and then use a call to `ignore` in place of an ignore declaration:

[ ](#){:#l0200}`(defun third-arg (x y z)`
!!!(p) {:.unnumlist}

` (ignore x y)`
!!!(p) {:.unnumlist}

` z)`
!!!(p) {:.unnumlist}

The compiler's calling convention is different from the interpreter, so the primitives need to be redefined.
The old definition of the primitive `show-prolog-vars` had three parameters: the list of arguments to the goal, a binding list, and a list of pending goals.
The new definition of `show-prolog-vars/2` also has three parameters, but that is just a coincidence.
The first two parameters are the two separate arguments to the goal: a list of variable names and a list of variable values.
The last parameter is a continuation function.
To continue, we call that function, but to fail, we throw to the catch point set up in `top-level-prove`.

[ ](#){:#l0205}`(defun show-prolog-vars/2 (var-names vars cont)`
!!!(p) {:.unnumlist}

` "Display the variables, and prompt the user to see`
!!!(p) {:.unnumlist}

` if we should continue.
If not, return to the top level."`
!!!(p) {:.unnumlist}

` (if (null vars)`
!!!(p) {:.unnumlist}

`  (format t "~&Yes")`
!!!(p) {:.unnumlist}

`  (loop for name in var-names`
!!!(p) {:.unnumlist}

`    for var in vars do`
!!!(p) {:.unnumlist}

`    (format t "~&~a = "a" name (deref-exp var))))`
!!!(p) {:.unnumlist}

` (if (continue-p)`
!!!(p) {:.unnumlist}

`  (funcall cont)`
!!!(p) {:.unnumlist}

`  (throw 'top-level-prove nil)))`
!!!(p) {:.unnumlist}

`(defun deref-exp (exp)`
!!!(p) {:.unnumlist}

` "Build something equivalent to EXP with variables dereferenced."`
!!!(p) {:.unnumlist}

` (if (atom (deref exp))`
!!!(p) {:.unnumlist}

`  exp`
!!!(p) {:.unnumlist}

`  (reuse-cons`
!!!(p) {:.unnumlist}

`   (deref-exp (first exp))`
!!!(p) {:.unnumlist}

`   (deref-exp (rest exp))`
!!!(p) {:.unnumlist}

`   exp)))`
!!!(p) {:.unnumlist}

With these definitions in place, we can invoke the compiler automatically just by making a query with the ?
- macro.

**Exercise 12.6 [m]** Suppose you define a predicate `p`, which calls `q`, and then define `q`.
In some implementations of Lisp, when you make a query like `(?
- (p ?x))`, you may get a warning message like `"function q/1 undefined"` before getting the correct answer.
The problem is that each function is compiled separately, so warnings detected during the compilation of `p/1` will be printed right away, even if the function `q/1` will be defined later.
In ANSI Common Lisp there is a way to delay the printing of warnings until a series of compilations are done: wrap the compilation with the macro `with-compi`l`ation-unit.` Even if your implementation does not provide this macro, it may provide the same functionality under a different name.
Find out if `with-compilation-unit` is already defined in your implementation, or if it can be defined.

## [ ](#){:#st0040}12.7 Benchmarking the Compiler
{:#s0040}
{:.h1hd}

Our compiled Prolog code runs the zebra puzzle in 17.4 seconds, a 16-fold speed-up over the interpreted version, for a rate of 740 LIPS.

Another popular benchmark is Lisp's reverse function, which we can code as the rev relation:

[ ](#){:#l0210}`(<− (rev () ()))`
!!!(p) {:.unnumlist}

`(<− (rev (?x .
?a) ?b) (rev ?a ?c) (concat ?c (?x) ?b))`
!!!(p) {:.unnumlist}

`(<− (concat () ?1 ?1)`
!!!(p) {:.unnumlist}

`(<− (concat (?x .
?a) ?b (?x .
?c)) (concat ?a ?b ?c))`
!!!(p) {:.unnumlist}

rev uses the relation concat, which stands for concatenation, (`concat ?a ?b ?c`)is true when `?a` concatenated to `?b` yields `?c`.
This relationlike name is preferred over more procedural names like append.
But `rev` is very similar to the following Lisp definitions:

[ ](#){:#l0215}`(defun rev (1)`
!!!(p) {:.unnumlist}

` (if (null 1)`
!!!(p) {:.unnumlist}

`  nil`
!!!(p) {:.unnumlist}

`  (app (rev (rest 1 ))`
!!!(p) {:.unnumlist}

`    (list (first 1)))))`
!!!(p) {:.unnumlist}

`(defun app (x y)`
!!!(p) {:.unnumlist}

` (if (null x)`
!!!(p) {:.unnumlist}

`  y`
!!!(p) {:.unnumlist}

`   (cons (first x)`
!!!(p) {:.unnumlist}

`    (app (rest x) y))))`
!!!(p) {:.unnumlist}

Both versions are inefficient.
It is possible to write an iterative version of `reverse` that does no extra consing and is tail-recursive:

[ ](#){:#l0220}`(<− (irev ?l ?r) (irev3 ?l () ?r))`
!!!(p) {:.unnumlist}

`(<− (irev3 (?x .
?l) ?so-far ?r) (irev3 ?l (?x .
?so-far) ?r))`
!!!(p) {:.unnumlist}

`(<− (irev3 () ?r ?r))`
!!!(p) {:.unnumlist}

The Prolog `irev` is equivalent to this Lisp program:

[ ](#){:#l0225}`(defun irev (list) (irev2 list nil))`
!!!(p) {:.unnumlist}

`(defun irev2 (list so-far)`
!!!(p) {:.unnumlist}

` (if (consp list)`
!!!(p) {:.unnumlist}

`   (irev2 (rest list) (cons (first list) so-far))`
!!!(p) {:.unnumlist}

`   so-far))`
!!!(p) {:.unnumlist}

The following table shows times in seconds to execute these routines on lists of length 20 and 100, for both Prolog and Lisp, both interpreted and compiled.
(Only compiled Lisp could execute rev on a 100-element list without running out of stack space.) Times for the zebra puzzle are also included, although there is no Lisp version of this program.

[ ](#){:#t0020}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| Problem | Interp. Prolog | Comp. Prolog | Speed-up | Interp. Lisp | Comp. Lisp |
| `zebra` | 278.000 | 17.241 | 16 | — | — |
| `rev 20` | 4.24 | .208 | 20 | .241 | .0023 |
| `rev 100` | — | — | — | — | .0614 |
| `irev 20` | .22 | .010 | 22 | .028 | .0005 |
| `irev 100` | 9.81 | .054 | 181 | .139 | .0014 |

![t0020](images/B9780080571157500121/t0020.png)

This benchmark is too small to be conclusive, but on these examples the Prolog compiler is 16 to 181 times faster than the Prolog interpreter, slightly faster than interpreted Lisp, but still 17 to 90 times slower than compiled Lisp.
This suggests that the Prolog interpreter cannot be used as a practical programming tool, but the Prolog compiler can.

Before moving on, it is interesting to note that Prolog provides for optional arguments automatically.
Although there is no special syntax for optional arguments, an often-used convention is to have two versions of a relation, one with *n* arguments and one with *n —* 1.
A single clause for the *n —* 1 case provides the missing, and therefore "optional," argument.
In the following example, `irev/2` can be considered as a version of `irev/3` where the missing optional argument is ().

[ ](#){:#l0230}`(<− (irev ?l ?r) (irev ?l () ?r))`
!!!(p) {:.unnumlist}

`(<− (irev (?x .
?l ) ?so-far ?r) (irev ?l (?x .
?so-far) ?r))`
!!!(p) {:.unnumlist}

`(<− (irev () ?r ?r))`
!!!(p) {:.unnumlist}

This is roughly equivalent to the following Lisp verison:

[ ](#){:#l0235}`(defun irev (list &optional (so-far nil))`
!!!(p) {:.unnumlist}

` (if (consp list)`
!!!(p) {:.unnumlist}

`   (irev (rest list) (cons (first list) so-far))`
!!!(p) {:.unnumlist}

`   so-far))`
!!!(p) {:.unnumlist}

## [ ](#){:#st0045}12.8 Adding More Primitives
{:#s0045}
{:.h1hd}

Just as a Lisp compiler needs machine instructions to do input/output, arithmetic, and the like, so our Prolog system needs to be able to perform certain primitive actions.
For the Prolog interpreter, primitives were implemented by function symbols.
When the interpreter went to fetch a list of clauses, if it got a function instead, it called that function, passing it the arguments to the current relation, the current bindings, and a list of unsatisfied goals.
For the Prolog compiler, primitives can be installed simply by writing a Lisp function that respects the convention of taking a continuation as the final argument and has a name of the form *symbol/arity.* For example, here's an easy way to handle input and output:

[ ](#){:#l0240}`(defun read/1 (exp cont)`
!!!(p) {:.unnumlist}

` (if (unify!
exp (read))`
!!!(p) {:.unnumlist}

`   (funcall cont)))`
!!!(p) {:.unnumlist}

`(defun write/1 (exp cont)`
!!!(p) {:.unnumlist}

` (write (deref-exp exp) :pretty t)`
!!!(p) {:.unnumlist}

` (funcall cont))`
!!!(p) {:.unnumlist}

Calling `(write ?x)` will always succeed, so the continuation will always be called.
Similarly, one could use `(read ?x)` to read a value and unify it with `?x`.
If `?x` is unbound, this is the same as assigning the value.
However, it is also possible to make a call like `(read (?x + ?y))`, which succeeds only if the input is a three-element list with + in the middle.
It is an easy extension to define `read/2` and `write/2` as relations that indicate what stream to use.
To make this useful, one would need to define `open/2` as a relation that takes a pathname as one argument and gives a stream back as the other.
Other optional arguments could also be supported, if desired.

The primitive nl outputs a newline:

[ ](#){:#l0245}`(defun nl/0 (cont) (terpri) (funcall cont))`
!!!(p) {:.unnumlist}

We provided special support for the unification predicate, =.
However, we could have simplified the compiler greatly by having a simple definition for `=/2`:

[ ](#){:#l0250}`(defun =/2 (?arg1 ?arg2 cont)`
!!!(p) {:.unnumlist}

` (if (unify!
?arg1 ?arg2)`
!!!(p) {:.unnumlist}

`  (funcall cont)))`
!!!(p) {:.unnumlist}

In fact, if we give our compiler the single clause:

[ ](#){:#l0255}(<- (= ?x `?x))`
!!!(p) {:.unnumlist}

it produces just this code for the definition of `=/ 2`.
There are other equality predicates to worry about.
The predicate `= =/2` is more like equal in Lisp.
It does no unification, but instead tests if two structures are equal with regard to their elements.
A variable is considered equal only to itself.
Here's an implementation:

[ ](#){:#l0260}`(defun =/2 (?arg1 ?arg2 cont)`
!!!(p) {:.unnumlist}

` "Are the two arguments EQUAL with no unification,`
!!!(p) {:.unnumlist}

` but with dereferencing?
If so, succeed."`
!!!(p) {:.unnumlist}

` (if (deref-equal ?arg1 ?arg2)`
!!!(p) {:.unnumlist}

`  (funcall cont)))`
!!!(p) {:.unnumlist}

`(defun deref-equal (x y)`
!!!(p) {:.unnumlist}

` "Are the two arguments EQUAL with no unification,`
!!!(p) {:.unnumlist}

` but with dereferencing?"`
!!!(p) {:.unnumlist}

` (or (eql (deref x) (deref y))`
!!!(p) {:.unnumlist}

`  (and (consp x)`
!!!(p) {:.unnumlist}

`   (consp y)`
!!!(p) {:.unnumlist}

`   (deref-equal (first x) (first y))`
!!!(p) {:.unnumlist}

`   (deref-equal (rest x) (rest y)))))`
!!!(p) {:.unnumlist}

One of the most important primitives is `call`.
Like `funcall` in Lisp, `call` allows us to build up a goal and then try to prove it.

[ ](#){:#l0265}`(defun call/1 (goal cont)`
!!!(p) {:.unnumlist}

` "Try to prove goal by calling it."`
!!!(p) {:.unnumlist}

` (deref goal)`
!!!(p) {:.unnumlist}

` (apply (make-predicate (first goal)`
!!!(p) {:.unnumlist}

`     (length (args goal)))`
!!!(p) {:.unnumlist}

`   (append (args goal) (list cont))))`
!!!(p) {:.unnumlist}

This version of `call` will give a run-time error if the goal is not instantiated to a list whose first element is a properly defined predicate; one might want to check for that, and fail silently if there is no defined predicate.
Here's an example of `call` where the goal is legal:

[ ](#){:#l0270}`> (?- (= ?p member) (call (?p ?x (a b c))))`
!!!(p) {:.unnumlist}

`?P = MEMBER`
!!!(p) {:.unnumlist}

`?X = A;`
!!!(p) {:.unnumlist}

`?P = MEMBER`
!!!(p) {:.unnumlist}

`?X = B;`
!!!(p) {:.unnumlist}

`?P = MEMBER`
!!!(p) {:.unnumlist}

`?X = C;`
!!!(p) {:.unnumlist}

`No.`
!!!(p) {:.unnumlist}

Now that we have `call`, a lot of new things can be implemented.
Here are the logical connectives and and or:

[ ](#){:#l0275}`(<− (or ?a ?b) (call ?a))`
!!!(p) {:.unnumlist}

`(<− (or ?a ?b) (call ?b))`
!!!(p) {:.unnumlist}

`(<− (and ?a ?b) (call ?a) (call ?b))`
!!!(p) {:.unnumlist}

Note that these are only binary connectives, not the *n*-ary special forms used in Lisp.
Also, this definition negates most of the advantage of compilation.
The goals inside an and or or will be interpreted by `call`, rather than being compiled.

We can also define `not,` or at least the normal Prolog `not,` which is quite distinct from the logical `not.` In fact, in some dialects, `not` is written \+, which is supposed to be ![u12-07-9780080571157](images/B9780080571157500121/u12-07-9780080571157.jpg) , that is, "can not be derived." The interpretation is that if goal G can not be proved, then (`not G` ) is true.
Logically, there is a difference between (`not G` ) being true and being unknown, but ignoring that difference makes Prolog a more practical programming language.
See [Lloyd 1987](B9780080571157500285.xhtml#bb0745) for more on the formal semantics of negation in Prolog.

Here's an implementation of `not/1`.
Since it has to manipulate the trail, and we may have other predicates that will want to do the same, we'll package up what was done in `maybe-add-undo-bindings` into the macro `with-undo-bindings:`

[ ](#){:#l0280}`(defmacro with-undo-bindings (&body body)`
!!!(p) {:.unnumlist}

` "Undo bindings after each expression in body except the last."`
!!!(p) {:.unnumlist}

` (if (length=1 body)`
!!!(p) {:.unnumlist}

`  (first body)`
!!!(p) {:.unnumlist}

`  '(let ((old-trail (fill-pointer *trail*)))`
!!!(p) {:.unnumlist}

`   ,(first body)`
!!!(p) {:.unnumlist}

`    ,@(loop for exp in (rest body)`
!!!(p) {:.unnumlist}

`        collect '(undo-bindings!
old-trail)`
!!!(p) {:.unnumlist}

`        collect exp))))`
!!!(p) {:.unnumlist}

`(defun not/1 (relation cont)`
!!!(p) {:.unnumlist}

` "Negation by failure: If you can't prove G.
then (not G) true."`
!!!(p) {:.unnumlist}

` ;; Either way, undo the bindings.`
!!!(p) {:.unnumlist}

` (with-undo-bindings`
!!!(p) {:.unnumlist}

`  (call/1 relation #'(lambda () (return-from not/1 nil)))`
!!!(p) {:.unnumlist}

`  (funcall cont)))`
!!!(p) {:.unnumlist}

Here's an example where `not` works fine:

[ ](#){:#l0285}`> (?- (member ?x (a b c)) (not (= ?x b)))`
!!!(p) {:.unnumlist}

`?X = A;`
!!!(p) {:.unnumlist}

`?X = C;`
!!!(p) {:.unnumlist}

`No.`
!!!(p) {:.unnumlist}

Now see what happens when we simply reverse the order of the two goals:

[ ](#){:#l0290}`> (?- (not (= ?x b)) (member ?x (a b c)))`
!!!(p) {:.unnumlist}

`No.`
!!!(p) {:.unnumlist}

The first example succeeds unless `?x` is bound to `b.` In the second example, `?x` is unbound at the start, so `(= ?x b )` succeeds, the not fails, and the `member` goal is never reached.
So our implementation of `not` has a consistent procedural interpretation, but it is not equivalent to the declarative interpretation usually given to logical negation.
Normally, one would expect that `a` and `c` would be valid solutions to the query, regardless of the order of the goals.

One of the fundamental differences between Prolog and Lisp is that Prolog is relational: you can easily express individual relations.
Lisp, on the other hand, is good at expressing collections of things as lists.
So far we don't have any way of forming a collection of objects that satisfy a relation in Prolog.
We can easily iterate over the objects; we just can't gather them together.
The primitive `bagof` is one way of doing the collection.
In general, `(bagof ?x (p ?x) ?bag)` unifies `?bag` with a list of all `?x's` that satisfy `(p ?x)`.
If there are no such `?x's`, then the call to `bagof` fails.
A *bag* is an unordered collection with duplicates allowed.
For example, the bag {*a*, *b, a*} is the same as the bag {*a*, *a*, *b*}, but different from {*a*, *b*}.
Bags stands in contrast to *sets,* which are unordered collections with no duplicates.
The set {*a*, *b*} is the same as the set {*b*, *a*}.
Here is an implementation of `bagof:`

[ ](#){:#l0295}`(defun bagof/3 (exp goal resuit cont)`
!!!(p) {:.unnumlist}

` "Find all solutions to GOAL, and for each solution,`
!!!(p) {:.unnumlist}

` collect the value of EXP into the list RESULT."`
!!!(p) {:.unnumlist}

` ;; Ex: Assume (p 1) (p 2) (p 3).
Then:`
!!!(p) {:.unnumlist}

` ;: (bagof ?x (p ?x) ?1) => ?1 = (1 2 3)`
!!!(p) {:.unnumlist}

` (let ((answers nil))`
!!!(p) {:.unnumlist}

` (call/1 goal #'(lambda ()`
!!!(p) {:.unnumlist}

`   (push (deref-copy exp) answers)))`
!!!(p) {:.unnumlist}

` (if (and (not (null answers))`
!!!(p) {:.unnumlist}

`  (unify!
resuit (nreverse answers)))`
!!!(p) {:.unnumlist}

` (funcall cont))))`
!!!(p) {:.unnumlist}

` (defun deref-copy (exp)`
!!!(p) {:.unnumlist}

` "Copy the expression, replacing variables with new ones.`
!!!(p) {:.unnumlist}

` The part without variables can be returned as is."`
!!!(p) {:.unnumlist}

` (sublis (mapcar #'(lambda (var) (cons (deref var) (?))`
!!!(p) {:.unnumlist}

`  (unique-find-anywhere-if #'var-p exp))`
!!!(p) {:.unnumlist}

` exp))`
!!!(p) {:.unnumlist}

Below we use `bagof` to collect a list of everyone Sandy likes.
Note that the result is a bag, not a set: Sandy appears more than once.

[ ](#){:#l0300}`> (?- (bagof ?who (likes Sandy ?who) ?bag))`
!!!(p) {:.unnumlist}

`?WHO = SANDY`
!!!(p) {:.unnumlist}

`?BAG = (LEE KIM ROBIN SANDY CATS SANDY);`
!!!(p) {:.unnumlist}

`No.`
!!!(p) {:.unnumlist}

In the next example, we form the bag of every list of length three that has `A` and `B` as members:

[ ](#){:#l9000}`> (?- (bagof ?l (and (length ?l (1 + (1 + (1 + 0))))`
!!!(p) {:.unnumlist}

`   (and (member a ?l) (member b ?l)))`
!!!(p) {:.unnumlist}

`  ?bag))`
!!!(p) {:.unnumlist}

`?L = (?5 ?8 ?11 ?68 ?66)`
!!!(p) {:.unnumlist}

`?BAG = ((A B ?17) (A ?21 B) (B A ?31) (?38 A B) (B ?48 A) (?52 B A))`
!!!(p) {:.unnumlist}

`No.`
!!!(p) {:.unnumlist}

Those who are disappointed with a bag containing multiple versions of the same answer may prefer the primitive `setof`, which does the same computation as `bagof` but then discards the duplicates.

[ ](#){:#l0305}`(defun setof/3 (exp goal resuit cont)`
!!!(p) {:.unnumlist}

` "Find all unique solutions to GOAL, and for each solution,`
!!!(p) {:.unnumlist}

` collect the value of EXP into the list RESULT."`
!!!(p) {:.unnumlist}

` ;; Ex: Assume (p 1) (p 2) (p 3).
Then:`
!!!(p) {:.unnumlist}

` ;; (setof ?x (p ?x) ?l ) => ?l = (1 2 3)`
!!!(p) {:.unnumlist}

` (let ((answers nil))`
!!!(p) {:.unnumlist}

` (call/1 goal #'(lambda ()`
!!!(p) {:.unnumlist}

`   (push (deref-copy exp) answers)))`
!!!(p) {:.unnumlist}

` (if (and (not (null answers))`
!!!(p) {:.unnumlist}

`  (unify!
resuit (delete-duplicates`
!!!(p) {:.unnumlist}

`    answers`
!!!(p) {:.unnumlist}

`    :test #'deref-equal)))`
!!!(p) {:.unnumlist}

` (funcall cont))))`
!!!(p) {:.unnumlist}

Prolog supports arithmetic with the operator `is`.
For example, `(is ?x (+ ?y 1))` unifies `?x` with the value of `?y` plus one.
This expression fails if `?y` is unbound, and it gives a run-time error if `?y` is not a number.
For our version of Prolog, we can support not just arithmetic but any Lisp expression:

[ ](#){:#l0310}`(defun is/2 (var exp cont)`
!!!(p) {:.unnumlist}

` ;; Example: (is ?x (+ 3 (* ?y (+ ?z 4))))`
!!!(p) {:.unnumlist}

` ;; Or even: (is (?x ?y ?x) (cons (first ?z) ?l))`
!!!(p) {:.unnumlist}

` (if (and (not (find-if-anywhere #'unbound-var-p exp))`
!!!(p) {:.unnumlist}

`  (unify!
var (eval (deref-exp exp))))`
!!!(p) {:.unnumlist}

` (funcall cont)))`
!!!(p) {:.unnumlist}

`(defun unbound-var-p (exp)`
!!!(p) {:.unnumlist}

` "Is EXP an unbound var?"`
!!!(p) {:.unnumlist}

` (and (var-p exp) (not (bound-p exp))))`
!!!(p) {:.unnumlist}

As an aside, we might as well give the Prolog programmer access to the function `unbound-var-p`.
The standard name for this predicate is `var/1`:

[ ](#){:#l0315}`(defun var/1 (?arg1 cont)`
!!!(p) {:.unnumlist}

` "Succeeds if ?arg1 is an uninstantiated variable."`
!!!(p) {:.unnumlist}

` (if (unbound-var-p ?arg1)`
!!!(p) {:.unnumlist}

` (funcall cont)))`
!!!(p) {:.unnumlist}

The is primitive fails if any part of the second argument is unbound.
However, there are expressions with variables that can be solved, although not with a direct call to `eval`.
For example, the following goal could be solved by binding `?x` to `2`:

[ ](#){:#l0320}`(solve (= 12 (* (+ ?x 1) 4)))`
!!!(p) {:.unnumlist}

We might want to have more direct access to Lisp from Prolog.
The problem with `is` is that it requires a check for unbound variables, and it calls `eval` to evaluate arguments recursively.
In some cases, we just want to get at Lisp's `apply`, without going through the safety net provided by is.
The primitive `lisp` does that.
Needless to say, `lisp` is not a part of standard Prolog.

[ ](#){:#l0325}`(defun lisp/2 (?result exp cont)`
!!!(p) {:.unnumlist}

` "Apply (first exp) to (rest exp), and return the result."`
!!!(p) {:.unnumlist}

` (if (and (consp (deref exp))`
!!!(p) {:.unnumlist}

`  (unify!
?result (apply (first exp) (rest exp))))`
!!!(p) {:.unnumlist}

` (funcall cont)))`
!!!(p) {:.unnumlist}

**Exercise 12.7 [m]** Define the primitive `solve/1`, which works like the function `solve` used in student ([page 225](B9780080571157500078.xhtml#p225)).
Decide if it should take a single equation as argument or a list of equations.

**Exercise 12.8 [h]** Assume we had a goal of the form `(solve (= 12 (* (+ ?x 1) 4)))`.
Rather than manipulate the equation when `solve/1` is called at run time, we might prefer to do part of the work at compile time, treating the call as if it were `(solve (= ?x 2))`.
Write a Prolog compiler macro for `solve`.
Notice that even when you have defined a compiler macro, you still need the underlying primitive, because the predicate might be invoked through a `call/1`.
The same thing happens in Lisp: even when you supply a compiler macro, you still need the actual function, in case of a `funcall` or `apply`.

**Exercise 12.9 [h]** Which of the predicates `call`, and, `or`, `not`, or `repeat` could benefit from compiler macros?
Write compiler macros for those predicates that could use one.

**Exercise 12.10 [m]** You might have noticed that `call/1` is inefficient in two important ways.
First, it calls `make-predicate`, which must build a symbol by appending strings and then look the string up in the Lisp symbol table.
Alter `make-predicate` to store the predicate symbol the first time it is created, so it can do a faster lookup on subsequent calls.
The second inefficiency is the call to append.
Change the whole compiler so that the continuation argument comes first, not last, thus eliminating the need for append in `call`.

**Exercise 12.11 [s]** The primitive `true/0` always succeeds, and `fail/0` always fails.
Define these primitives.
Hint: the first corresponds to a Common Lisp function, and the second is a function already defined in this chapter.

**Exercise 12.12 [s]** Would it be possible to write `= =/2` as a list of clauses rather than as a primitive?

**Exercise 12.13 [m]** Write a version of `deref-copy` that traverses the argument expression only once.

## [ ](#){:#st0050}12.9 The Cut
{:#s0050}
{:.h1hd}

In Lisp, it is possible to write programs that backtrack explicitly, although it can be awkward when there are more than one or two backtrack points.
In Prolog, backtracking is automatic and implicit, but we don't yet know of any way to *avoid* backtracking.
There are two reasons why a Prolog programmer might want to disable backtracking.
First, keeping track of the backtrack points takes up time and space.
A programmer who knows that a certain problem has only one solution should be able to speed up the computation by telling the program not to consider the other possible branches.
Second, sometimes a simple logical specification of a problem will yield redundant solutions, or even some unintended solutions.
It may be that simply pruning the search space to eliminate some backtracking will yield only the desired answers, while restructuring the program to give all and only the right answers would be more difficult.
Here's an example.
Suppose we wanted to define a predicate, `max/3`, which holds when the third argument is the maximum of the first two arguments, where the first two arguments will always be instantiated to numbers.
The straightforward definition is:

[ ](#){:#l0330}`(<− (max ?x ?y ?x) (>= ?x ?y))`
!!!(p) {:.unnumlist}

`(<− (max ?x ?y ?y) (< ?x ?y))`
!!!(p) {:.unnumlist}

Declaratively, this is correct, but procedurally it is a waste of time to compute the < relation if the >= has succeeded: in that case the < can never succeed.
The cut symbol, written !, can be used to stop the wasteful computation.
We could write:

[ ](#){:#l0335}`(<− (max ?x ?y ?x) (>= ?x ?y) !)`
!!!(p) {:.unnumlist}

`(<− (max ?x ?y ?y))`
!!!(p) {:.unnumlist}

The cut in the first clause says that if the first clause succeeds, then no other clauses will be considered.
So now the second clause can not be interpreted on its own.
Rather, it is interpreted as "if the first clause fails, then the `max` of two numbers is the second one."

In general, a cut can occur anywhere in the body of a clause, not just at the end.
There is no good declarative interpretation of a cut, but the procedural interpretation is two-fold.
First, when a cut is "executed" as a goal, it always succeeds.
But in addition to succeeding, it sets up a fence that cannot be crossed by subsequent backtracking.
The cut serves to cut off backtracking both from goals to the right of the cut (in the same clause) and from clauses below the cut (in the same predicate).
Let's look at a more abstract example:

[ ](#){:#l0340}`(<− (p) (q) (r) !
(s) (t))`
!!!(p) {:.unnumlist}

`(<− (p) (s))`
!!!(p) {:.unnumlist}

In processing the first clause of `p`, backtracking can occur freely while attempting to solve `q` and `r`.
Once `r` is solved, the cut is encountered.
From that point on, backtracking can occur freely while solving `s` and `t`, but Prolog will never backtrack past the cut into `r`, nor will the second clause be considered.
On the other hand, if `q` or `r` failed (before the cut is encountered), then Prolog would go on to the second clause.

Now that the intent of the cut is clear, let's think of how it should be implemented.
We'll look at a slightly more complex predicate, one with variables and multiple cuts:

[ ](#){:#l0345}`(<− (p ?x a) !
(q ?x))`
!!!(p) {:.unnumlist}

`(<− (p ?x b) (r ?x) !
(s ?x))`
!!!(p) {:.unnumlist}

We have to arrange it so that as soon as we backtrack into a cut, no more goals are considered.
In the first clause, when `q/1` fails, we want to return from `p/2` immediately, rather than considering the second clause.
Similarly, the first time `s/1` fails, we want to return from `p/2`, rather than going on to consider other solutions to `r/1`.
Thus, we want code that looks something like this:

[ ](#){:#l0350}`(defun p/2 (argl arg2 cont)`
!!!(p) {:.unnumlist}

` (let ((old-trail (fill-pointer *trail*)))`
!!!(p) {:.unnumlist}

`  (if (unify!
arg2 'a)`
!!!(p) {:.unnumlist}

`   (progn (q/1 argl cont)`
!!!(p) {:.unnumlist}

`     (return-from p/2 nil)))`
!!!(p) {:.unnumlist}

`  (undo-bindings!
old-trail)`
!!!(p) {:.unnumlist}

`  (if (unify!
arg2 'b)`
!!!(p) {:.unnumlist}

`   (r/1 argl #'(lambda ()`
!!!(p) {:.unnumlist}

`       (progn (s/1 argl cont)`
!!!(p) {:.unnumlist}

`        (return-from p/2 nil)))))))`
!!!(p) {:.unnumlist}

We can get this code by making a single change to `compile-body:` when the first goal in a body (or what remains of the body) is the cut symbol, then we should generate a `progn` that contains the code for the rest of the body, followed by a `return-from` the predicate being compiled.
Unfortunately, the name of the predicate is not available to `compile-body.` We could change `compile-clause` and `compile-body` to take the predicate name as an extra argument, or we could bind the predicate as a special variable in `compile-predicate`.
I choose the latter:

[ ](#){:#l0355}`(defvar *predicate* nil`
!!!(p) {:.unnumlist}

` "The Prolog predicate currently being compiled")`
!!!(p) {:.unnumlist}

`(defun compile-predicate (symbol arity clauses)`
!!!(p) {:.unnumlist}

` "Compile all the clauses for a given symbol/arity`
!!!(p) {:.unnumlist}

` into a single LISP function."`
!!!(p) {:.unnumlist}

` (let ((*predicate* (make-predicate symbol arity)) ;***`
!!!(p) {:.unnumlist}

`   (parameters (make-parameters arity)))`
!!!(p) {:.unnumlist}

` (compile`
!!!(p) {:.unnumlist}

`  (eval`
!!!(p) {:.unnumlist}

`   '(defun ,*predicate* (,©parameters cont) ;***`
!!!(p) {:.unnumlist}

`    .,(maybe-add-undo-bindings`
!!!(p) {:.unnumlist}

`     (mapcar #'(lambda (clause)`
!!!(p) {:.unnumlist}

`        (compile-clause parameters`
!!!(p) {:.unnumlist}

`          clause 'cont))`
!!!(p) {:.unnumlist}

`       clauses)))))))`
!!!(p) {:.unnumlist}

`(defun compile-body (body cont bindings)`
!!!(p) {:.unnumlist}

` "Compile the body of a clause."`
!!!(p) {:.unnumlist}

` (cond`
!!!(p) {:.unnumlist}

` ((null body)`
!!!(p) {:.unnumlist}

`  '(funcall ,cont))`
!!!(p) {:.unnumlist}

` ((eq (first body) '!) ;***`
!!!(p) {:.unnumlist}

`  '(progn ,(compile-body (rest body) cont bindings) ;***`
!!!(p) {:.unnumlist}

`    (return-from ,*predicate* nil))) ;***`
!!!(p) {:.unnumlist}

` (t (let* ((goal (first body))`
!!!(p) {:.unnumlist}

`    (macro (prolog-compiler-macro (predicate goal)))`
!!!(p) {:.unnumlist}

`    (macro-val (if macro`
!!!(p) {:.unnumlist}

`        (funcall macro goal (rest body)`
!!!(p) {:.unnumlist}

`          contbindings))))`
!!!(p) {:.unnumlist}

`   (if (and macro (not (eq macro-val :pass)))`
!!!(p) {:.unnumlist}

`    macro-val`
!!!(p) {:.unnumlist}

`    '(,(make-predicate (predicate goal)`
!!!(p) {:.unnumlist}

`        (relation-arity goal))`
!!!(p) {:.unnumlist}

`      ,@(mapcar #'(lambda (arg)`
!!!(p) {:.unnumlist}

`       (compile-arg arg bindings))`
!!!(p) {:.unnumlist}

`      (args goal))`
!!!(p) {:.unnumlist}

`      , (if (null (rest body))`
!!!(p) {:.unnumlist}

`      cont`
!!!(p) {:.unnumlist}

`      '#'(lambda ()`
!!!(p) {:.unnumlist}

`        ,(compile-body`
!!!(p) {:.unnumlist}

`        (rest body) cont`
!!!(p) {:.unnumlist}

`        (bind-new-variables bindings goal))))))))))`
!!!(p) {:.unnumlist}

**Exercise 12.14 [m]** Given the definitions below, figure out what a call to `test-cut` will do, and what it will write:

[ ](#){:#l0360}`(<− (test-cut) (p a) (p b) !
(p c) (p d))`
!!!(p) {:.unnumlist}

`(<− (test-cut) (p e))`
!!!(p) {:.unnumlist}

`(<− (p ?x) (write (?x 1)))`
!!!(p) {:.unnumlist}

`(<− (p ?x) (write (?x 2)))`
!!!(p) {:.unnumlist}

Another way to use the cut is in a *repeat/fail* loop.
The predicate repeat is defined with the following two clauses:

[ ](#){:#l0365}`(<− (repeat))`
!!!(p) {:.unnumlist}

`(<− (repeat) (repeat))`
!!!(p) {:.unnumlist}

An alterna te definition as a primitive is:

[ ](#){:#l0370}`(defun repeat/0 (cont)`
!!!(p) {:.unnumlist}

` (loop (funcall cont)))`
!!!(p) {:.unnumlist}

Unfortunately, `repeat` is one of the most abused predicates.
Several Prolog books present programs like this:

[ ](#){:#l0375}`(<− (main)`
!!!(p) {:.unnumlist}

` (write "Hello.")`
!!!(p) {:.unnumlist}

` (repeat)`
!!!(p) {:.unnumlist}

` (write "Command: ")`
!!!(p) {:.unnumlist}

` (read ?command)`
!!!(p) {:.unnumlist}

` (process ?command)`
!!!(p) {:.unnumlist}

` (= ?command exit)`
!!!(p) {:.unnumlist}

` (write "Good bye."))`
!!!(p) {:.unnumlist}

The intent is that commands are read one at a time, and then processed.
For each command except `exit, process` takes the appropriate action and then fails.
This causes a backtrack to the repeat goal, and a new command is read and processed.
When the command is `exit`, the procedure returns.

There are two reasons why this is a poor program.
First, it violates the principle of referential transparency.
Things that look alike are supposed to be alike, regardless of the context in which they are used.
But here there is no way to tell that four of the six goals in the body comprise a loop, and the other goals are outside the loop.
Second, it violates the principle of abstraction.
A predicate should be understandable as a separate unit.
But here the predicate process can only be understood by considering the context in which it is called: a context that requires it to fail after processing each command.
As [Richard O'Keefe 1990](B9780080571157500285.xhtml#bb0925) points out, the correct way to write this clause is as follows:

[ ](#){:#l0380}`(<− (main)`
!!!(p) {:.unnumlist}

` (write "Hello.")`
!!!(p) {:.unnumlist}

` (repeat)`
!!!(p) {:.unnumlist}

`   (write "Command: ")`
!!!(p) {:.unnumlist}

`   (read ?command)`
!!!(p) {:.unnumlist}

`   (process ?command)`
!!!(p) {:.unnumlist}

`   (or (= ?command exit) (fail))`
!!!(p) {:.unnumlist}

` !`
!!!(p) {:.unnumlist}

` (write "Good bye."))`
!!!(p) {:.unnumlist}

The indentation clearly indicates the limits of the repeat loop.
The loop is terminated by an explicit test and is followed by a cut, so that a calling program won't accidently backtrack into the loop after it has exited.
Personally, I prefer a language like Lisp, where the parentheses make constructs like loops explicit and indentation can be done automatically.
But O'Keefe shows that well-structured readable programs can be written in Prolog.

The if-then and if-then-else constructions can easily be written as clauses.
Note that the if-then-else uses a cut to commit to the `then` part if the test is satisfied.

[ ](#){:#l0385}`(<− (if ?test ?then) (if ?then ?else (fail)))`
!!!(p) {:.unnumlist}

`(<− (if ?test ?then ?else)`
!!!(p) {:.unnumlist}

` (call ?test)`
!!!(p) {:.unnumlist}

` !`
!!!(p) {:.unnumlist}

` (call ?then))`
!!!(p) {:.unnumlist}

`(<− (if ?test ?then ?else)`
!!!(p) {:.unnumlist}

` (call ?else))`
!!!(p) {:.unnumlist}

The cut can be used to implement the nonlogical `not`.
The following two clauses are often given before as the definition of `not`.
Our compiler succesfully turns these two clauses into exactly the same code as was given before for the primitive `not/1`:

[ ](#){:#l0390}`(<− (not ?p) (call ?p) !
(fail))`
!!!(p) {:.unnumlist}

`(<− (not ?p))`
!!!(p) {:.unnumlist}

## [ ](#){:#st0055}12.10 “Real” Prolog
{:#s0055}
{:.h1hd}

The Prolog-In-Lisp system developed in this chapter uses Lisp syntax because it is intended to be embedded in a Lisp system.
Other Prolog implementations using Lisp syntax include micro-Prolog, Symbolics Prolog, and LMI Prolog.

However, the majority of Prolog systems use a syntax closer to traditional mathematical notation.
The following table compares the syntax of "standard" Prolog to the syntax of Prolog-In-Lisp.
While there is currently an international committee working on standardizing Prolog, the final report has not yet been released, so different dialects may have slightly different syntax.
However, most implementations follow the notation summarized here.
They derive from the Prolog developed at the University of Edinburgh for the DEC-10 by David H.
D.
Warren and his colleagues.
The names for the primitives in the last section are also taken from Edinburgh Prolog.

[ ](#){:#t0025}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| | Prolog | Prolog-In-Lisp |
| atom | `lower` | `const` |
| variable | `Upper` | `?var` |
| anonymous | `-` | `?` |
| goal | `p(Var,const)` | `(p ?var const)` |
| rule | `p(X) :- q(X).` | `(<- (p ?x) (q ?x))` |
| fact | `p(a).` | `(<- (p a))` |
| query | `?- p(X).` | `(?- (p ?x))` |
| list | `[a,b,c]` | `(a b c)` |
| cons | `[a| Rest]` | `(a . ?rest)` |
| nil | `[]` | `()` |
| and | `p(X). q(X)` | `(and (p ?x) (q ?x)>` |
| or | `P(X): q(X)` | `(or (p ?x) (q ?x))` |
| not | `\+ p(X)` | `(not (p ?x))` |

We have adopted Lisp's bias toward lists; terms are built out of atoms, variables, and conses of other terms.
In real Prolog cons cells are provided, but terms are usually built out of *structures*, not lists.
The Prolog term `p(a,b)` corresponds to the Lisp vector `#(p/2 a b)`, not the list `(p a b)`.
A minority of Prolog implementations use *structure sharing.* In this approach, every non-atomic term is represented by a skeleton that contains place holders for variables and a header that points to the skeleton and also contains the variables that will fill the place holders.
With structure sharing, making a copy is easy: just copy the header, regardless of the size of the skeleton.
However, manipulating terms is complicated by the need to keep track of both skeleton and header.
See [Boyer and Moore 1972](B9780080571157500285.xhtml#bb0110) for more on structure sharing.

Another major difference is that real Prolog uses the equivalent of failure continuations, not success continuations.
No actual continuation, in the sense of a closure, is built.
Instead, when a choice is made, the address of the code for the next choice is pushed on a stack.
Upon failure, the next choice is popped off the stack.
This is reminiscent of the backtracking approach using Scheme's `call/cc` facility outlined on [page 772](B9780080571157500224.xhtml#p772).

**Exercise 12.15 [m]** Assuming an approach using a stack of failure continuations instead of success continuations, show what the code for `p` and `member` would look like.
Note that you need not pass failure continuations around; you can just push them onto a stack that `top-level-prove` will invoke.
How would the cut be implemented?
Did we make the right choice in implementing our compiler with success continuations, or would failure continuations have been better?

## [ ](#){:#st0060}12.11 History and References
{:#s0060}
{:.h1hd}

As described in [chapter 11](B978008057115750011X.xhtml), the idea of logic programming was fairly well understood by the mid-1970s.
But because the implementations of that time were slow, logic programming did not catch on.
It was the Prolog compiler for the DEC-10 that made logic programming a serious alternative to Lisp and other general-purpose languages.
The compiler was developed in 1977 by David H.
D.
Warren with Fernando Pereira and Luis Pereira.
See the paper by [Warren (1979)](B9780080571157500285.xhtml#bb1325) and by all three (1977).

Unfortunately, David H.
D.
Warren's pioneering work on compiling Prolog has never been published in a widely accessible form.
His main contribution was the description of the Warren Abstract Machine (WAM), an instruction set for compiled Prolog.
Most existing compilers use this instruction set, or a slight modification of it.
This can be done either through byte-code interpretation or through macroexpansion to native machine instructions.
[Aït-Kaci 1991](B9780080571157500285.xhtml#bb0020) provides a good tutorial on the WAM, much less terse than the original ([Warren 1983](B9780080571157500285.xhtml#bb1330)).
The compiler presented in this chapter does not use the WAM.
Instead, it is modeled after Mark [Stickel's (1988)](B9780080571157500285.xhtml#bb1200) theorem prover.
A similar compiler is briefly sketched by Jacques [Cohen 1985](B9780080571157500285.xhtml#bb0225).

## [ ](#){:#st0065}12.12 Exercises
{:#s0065}
{:.h1hd}

**Exercise 12.16 [m]** Change the Prolog compiler to allow implicit `calls`.
That is, if a goal is not a cons cell headed by a predicate, compile it as if it were a `call`.
The clause:

[ ](#){:#l0395}`(<- (p ?x ?y) (?x c) ?y)`
!!!(p) {:.unnumlist}

should be compiled as if it were:

[ ](#){:#l0400}`(<− (p ?x ?y) (call (?x c)) (call ?y))`
!!!(p) {:.unnumlist}

**Exercise 12.17 [h]** Here are some standard Prolog primitives:

* [ ](#){:#l0405}• `get/1` Read a single character and unify it with the argument.

* • `put/1` Print a single character.

* • `nonvar/1, /=, /==` The opposites of `var, = and = =` , respectively.

* • `integer/1` True if the argument is an integer.

* • `atom/1` True if the argument is a symbol (like Lisp's `symbol p`).

* • `atomic/1` True if the argument is a number or symbol (like Lisp's `atom`).

* • <,>,=<,>= Arithmetic comparison; succeeds when the arguments are both instantiated to numbers and the comparison is true.

* • `listing/0` Print out the clauses for all defined predicates.

* • `listing/1` Print out the clauses for the argument predicate.

Implement these predicates.
In each case, decide if the predicate should be implemented as a primitive or a list of clauses, and if it should have a compiler macro.

There are some naming conflicts that need to be resolved.
Terms like `atom` have one meaning in Prolog and another in Lisp.
Also, in Prolog the normal notation is \= and \==, not /= and /==.
For Prolog-In-Lisp, you need to decide which notations to use: Prolog's or Lisp's.

**Exercise 12.18 [s]** In Lisp, we are used to writing n-ary calls like `(< 1 n 10 ) or (= x y z )`.
Write compiler macros that expand n-ary calls into a series of binary calls.
For example, `(< 1 n 10)` should expand into `(and (< 1 n) (< n 10))`.

**Exercise 12.19 [m]** One feature of Lisp that is absent in Prolog is the `quote` mechanism.
Is there a use for `quote?` If so, implement it; if not, explain why it is not needed.

**Exercise 12.20 [h]** Write a tracing mechanism for Prolog.
Add procedures `p-trace` and `p-untrace` to trace and untrace Prolog predicates.
Add code to the compiler to generate calls to a printing procedure for goals that are traced.
In Lisp, we have to trace procedures when they are called and when they return.
In Prolog, there are four cases to consider: the call, successful completion, backtrack into subsequent clauses, and failure with no more clauses.
We will call these four `cases call`, `exit`, `redo,` and `fail`, respectively.
If we traced `member,` we would expect tracing output to look something like this:

[ ](#){:#l0410}`> (?- (member ?x (a b c d)) (fail))`
!!!(p) {:.unnumlist}

` CALL MEMBER: ?1 (A B C D)`
!!!(p) {:.unnumlist}

` EXIT MEMBER: A (A B C D)`
!!!(p) {:.unnumlist}

` REDO MEMBER: ?1 (A B C D)`
!!!(p) {:.unnumlist}

`  CALL MEMBER: ?1 (B C D)`
!!!(p) {:.unnumlist}

`  EXIT MEMBER: B (B C D)`
!!!(p) {:.unnumlist}

`  REDO MEMBER: ?1 (B C D)`
!!!(p) {:.unnumlist}

`   CALL MEMBER: ?1 (C D)`
!!!(p) {:.unnumlist}

`   EXIT MEMBER: C (C D)`
!!!(p) {:.unnumlist}

`   REDO MEMBER: ?1 (C D)`
!!!(p) {:.unnumlist}

`    CALL MEMBER: ?1 (D)`
!!!(p) {:.unnumlist}

`    EXIT MEMBER: D (D)`
!!!(p) {:.unnumlist}

`    REDO MEMBER: ?1 (D)`
!!!(p) {:.unnumlist}

`     CALL MEMBER: ?1 NIL`
!!!(p) {:.unnumlist}

`     REDO MEMBER: ?1 NIL`
!!!(p) {:.unnumlist}

`     FAIL MEMBER: ?1 NIL`
!!!(p) {:.unnumlist}

`    FAIL MEMBER: ?1 (D)`
!!!(p) {:.unnumlist}

`   FAIL MEMBER: ?1 (C D)`
!!!(p) {:.unnumlist}

`  FAIL MEMBER: ?1 (B C D)`
!!!(p) {:.unnumlist}

` FAIL MEMBER: ?1 (A B C D)`
!!!(p) {:.unnumlist}

`No.`
!!!(p) {:.unnumlist}

**Exercise 12.21 [m]** Some Lisp systems are very slow at compiling functions.
`KCL` is an example; it compiles by translating to `C` and then calling the `C` compiler and assembler.
In `KCL` it is best to compile only code that is completely debugged, and run interpreted while developing a program.

Alter the Prolog compiler so that calling the Lisp compiler is optional.
In all cases, Prolog functions are translated into Lisp, but they are only compiled to machine language when a variable is set.

**Exercise 12.22 [d]** Some Prolog systems provide the predicate `freeze` to "freeze" a goal until its variables are instantiated.
For example, the goal `(freeze x (> x 0))` is interpreted as follows: if `x` is instantiated, then just evaluate the goal `(> x 0)`, and succeed or fail depending on the result.
However, if `x` is unbound, then succeed and continue the computation, but remember the goal `(> x 0)` and evaluate it as soon as `x` becomes instantiated.
Implement freeze.

**Exercise 12.23 [m]** Write a recursive version of `anonymous-variables-in` that does not use a local function.

## [ ](#){:#st0070}12.13 Answers
{:#s0070}
{:.h1hd}

**Answer 12.6** Here's a version that works for Texas Instruments and Lucid implementations:

[ ](#){:#l0415}`(defmacro with-compilation-unit (options &body body)`
!!!(p) {:.unnumlist}

` "Do the body, but delay compiler warnings until the end."`
!!!(p) {:.unnumlist}

` ;; This is defined in Common Lisp the Language, 2nd ed.`
!!!(p) {:.unnumlist}

` '(,(read-time-case`
!!!(p) {:.unnumlist}

`  #+TI 'compi1er:compi1er-warnings-context-bind`
!!!(p) {:.unnumlist}

`  #+Lucid 'with-deferred-warnings`
!!!(p) {:.unnumlist}

`    'progn)`
!!!(p) {:.unnumlist}

`  .,body))`
!!!(p) {:.unnumlist}

`(defun prolog-compile-symbols (&optional (symbols *uncompiled*))`
!!!(p) {:.unnumlist}

` "Compile a list of Prolog symbols.`
!!!(p) {:.unnumlist}

` By default, the list is all symbols that need it."`
!!!(p) {:.unnumlist}

` (with-compilation-unit ()`
!!!(p) {:.unnumlist}

` (mapc #'prolog-compile symbols)`
!!!(p) {:.unnumlist}

` (setf *uncompiled* (set-difference *uncompiled* symbols))))`
!!!(p) {:.unnumlist}

**Answer 12.9** Macros for `and` and `or` are very important, since these are commonly used.
The macro for `and` is trivial:

[ ](#){:#l0420}`(def-prolog-compiler-macro and (goal body cont bindings)`
!!!(p) {:.unnumlist}

` (compile-body (append (args goal) body) cont bindings))`
!!!(p) {:.unnumlist}

The macro for or is trickier:

[ ](#){:#l0425}`(def-prolog-compiler-macro or (goal body cont bindings)`
!!!(p) {:.unnumlist}

` (let ((disjuncts (args goal)))`
!!!(p) {:.unnumlist}

`  (case (length disjuncts)`
!!!(p) {:.unnumlist}

`   (0 fail)`
!!!(p) {:.unnumlist}

`   (1 (compile-body (cons (first disjuncts) body) cont bindings))`
!!!(p) {:.unnumlist}

`   (t (let ((fn (gensym "F")))`
!!!(p) {:.unnumlist}

`    '(flèt ((,fn () ,(compile-body body cont bindings)))`
!!!(p) {:.unnumlist}

`     .,(maybe-add-undo-bindings`
!!!(p) {:.unnumlist}

`      (loop for g in disjuncts collect`
!!!(p) {:.unnumlist}

`       (compile-body (list g) '#',fn`
!!!(p) {:.unnumlist}

`        bindings)))))))))`
!!!(p) {:.unnumlist}

**Answer 12.11**`true/0` is `funcall` : when a goal succeeds, we call the continuation, `fail/0` is `ignore`: when a goal fails, we ignore the continuation.
We could also define compiler macros for these primitives:

[ ](#){:#l0430}`(def-prolog-compiler-macro true (goal body cont bindings)`
!!!(p) {:.unnumlist}

` (compile-body body cont bindings))`
!!!(p) {:.unnumlist}

`(def-prolog-compiler-macro fail (goal body cont bindings)`
!!!(p) {:.unnumlist}

` (declare (ignore goal body cont bindings))`
!!!(p) {:.unnumlist}

` nil)`
!!!(p) {:.unnumlist}

**Answer 12.13**

[ ](#){:#l0435}`(defun deref-copy (exp)`
!!!(p) {:.unnumlist}

` "Build a copy of the expression, which may have variables.`
!!!(p) {:.unnumlist}

` The part without variables can be returned as is."`
!!!(p) {:.unnumlist}

` (let ((var-alist nil ))`
!!!(p) {:.unnumlist}

`  (labels`
!!!(p) {:.unnumlist}

`   ((walk (exp)`
!!!(p) {:.unnumlist}

`    (deref exp)`
!!!(p) {:.unnumlist}

`    (cond ((consp exp)`
!!!(p) {:.unnumlist}

`     (reuse-cons (walk (first exp))`
!!!(p) {:.unnumlist}

`       (walk (rest exp))`
!!!(p) {:.unnumlist}

`       exp))`
!!!(p) {:.unnumlist}

`     ((var-p exp)`
!!!(p) {:.unnumlist}

`     (let ((entry (assoc exp var-alist)))`
!!!(p) {:.unnumlist}

`      (if (not (null entry))`
!!!(p) {:.unnumlist}

`      (cdr entry)`
!!!(p) {:.unnumlist}

`      (let ((var-copy (?)))`
!!!(p) {:.unnumlist}

`        (push (cons exp var-copy) var-alist)`
!!!(p) {:.unnumlist}

`        var-copy))))`
!!!(p) {:.unnumlist}

`     (t exp))))`
!!!(p) {:.unnumlist}

`  (walk exp))))`
!!!(p) {:.unnumlist}

**Answer 12.14** In the first clause of `test-cut`, all four calls to `p` will succeed via the first clause of `p`.
Then backtracking will occur over the calls to `(p c)` and `(p d)`.
All four combinations of `1` and `2` succeed.
After that, backtracking would normally go back to the call to `(p b)`.
But the cut prevents this, and the whole `(test-cut)` goal fails, without ever considering the second clause.
Here's the actual output:

[ ](#){:#l0440}`(?- (test-cut))`
!!!(p) {:.unnumlist}

`(A 1)(B 1)(C 1) (D 1)`
!!!(p) {:.unnumlist}

`Yes;`
!!!(p) {:.unnumlist}

`(D 2)`
!!!(p) {:.unnumlist}

`Yes;`
!!!(p) {:.unnumlist}

`(C 2)(D 1)`
!!!(p) {:.unnumlist}

`Yes;`
!!!(p) {:.unnumlist}

`(D 2)`
!!!(p) {:.unnumlist}

`Yes;`
!!!(p) {:.unnumlist}

`No.`
!!!(p) {:.unnumlist}

**Answer 12.17** For example:

[ ](#){:#l0445}`(defun >/2 (x y cont)`
!!!(p) {:.unnumlist}

` (if (and (numberp (deref x)) (numberp (deref y)) (> x y))`
!!!(p) {:.unnumlist}

`  (funcall cont)))`
!!!(p) {:.unnumlist}

`(defun numberp/1 (x cont)`
!!!(p) {:.unnumlist}

` (if (numberp (deref x))`
!!!(p) {:.unnumlist}

`  (funcall cont)))`
!!!(p) {:.unnumlist}

**Answer 12.19** Lisp uses quote in two ways: to distinguish a symbol from the value of the variable represented by that symbol, and to distinguish a literal list from the value that would be returned by evaluating a function call.
The first distinction Prolog makes by a lexical convention: variables begin with a question mark in our Prolog, and they are capitalized in real Prolog.
The second distinction is not necessary because Prolog is relational rather than functional.
An expression is a goal if it is a member of the body of a clause, and is a literal if it is an argument to a goal.

**Answer 12.20** Hint: Here's how `member` could be augmented with calls to a procedure, `prolog-trace`, which will print information about the four kinds of tracing events:

[ ](#){:#l0450}`(defun member/2 (?arg1 ?arg2 cont)`
!!!(p) {:.unnumlist}

` (let ((old-trail (fill-pointer *tra1l*))`
!!!(p) {:.unnumlist}

`   (exit-cont #'(lambda ()`
!!!(p) {:.unnumlist}

`     (prolog-trace 'exit 'member ?arg1 ?arg2 )`
!!!(p) {:.unnumlist}

`     (funcall cont))))`
!!!(p) {:.unnumlist}

`  (prolog-trace 'call 'member ?arg1 ?arg2)`
!!!(p) {:.unnumlist}

`  (if (unify!
?arg2 (cons ?arg1 (?)))`
!!!(p) {:.unnumlist}

`   (funcall exit-cont))`
!!!(p) {:.unnumlist}

`  (undo-bindings!
old-trail)`
!!!(p) {:.unnumlist}

`  (prolog-trace 'redo 'member ?arg1 ?arg2)`
!!!(p) {:.unnumlist}

`  (let ((?rest (?)))`
!!!(p) {:.unnumlist}

`   (if (unify!
?arg2 (cons (?) ?rest))`
!!!(p) {:.unnumlist}

`   (member/2 ?arg1 ?rest exit-cont)))`
!!!(p) {:.unnumlist}

`  (prolog-trace 'fail 'member ?arg1 ?arg2)))`
!!!(p) {:.unnumlist}

The definition of `prolog-trace` is:

[ ](#){:#l9005}`(defvar *prolog-trace-indent* 0)`
!!!(p) {:.unnumlist}

`(defun prolog-trace (kind predicate &rest args)`
!!!(p) {:.unnumlist}

` (if (member kind '(call redo))`
!!!(p) {:.unnumlist}

` (incf *prolog-trace-indent* 3))`
!!!(p) {:.unnumlist}

` (format t "~&~VT~a ~ a:~{ ~ a ~}"`
!!!(p) {:.unnumlist}

`   *prolog-trace-indent* kind predicate args)`
!!!(p) {:.unnumlist}

` (if (member kind '(fail exit))`
!!!(p) {:.unnumlist}

` (decf *prolog-trace-indent* 3)))`
!!!(p) {:.unnumlist}

**Answer 12.23**

[ ](#){:#l0455}`(defun anonymous-variables-in (tree)`
!!!(p) {:.unnumlist}

` "Return a list of all variables that occur only once in tree."`
!!!(p) {:.unnumlist}

` (values (anon-vars-in tree nil nil)))`
!!!(p) {:.unnumlist}

`(defun anon-vars-in (tree seen-once seen-more)`
!!!(p) {:.unnumlist}

` "Walk the data structure TREE, returning a list of variables`
!!!(p) {:.unnumlist}

` seen once, and a list of variables seen more than once."`
!!!(p) {:.unnumlist}

` (cond`
!!!(p) {:.unnumlist}

`  ((consp tree)`
!!!(p) {:.unnumlist}

`  (multiple-value-bind (new-seen-once new-seen-more)`
!!!(p) {:.unnumlist}

`   (anon-vars-in (first tree) seen-once seen-more)`
!!!(p) {:.unnumlist}

`   (anon-vars-in (rest tree) new-seen-once new-seen-more)))`
!!!(p) {:.unnumlist}

`  ((not (variable-p tree)) (values seen-once seen-more))`
!!!(p) {:.unnumlist}

`  ((member tree seen-once)`
!!!(p) {:.unnumlist}

`  (values (delete tree seen-once) (cons tree seen-more)))`
!!!(p) {:.unnumlist}

`  ((member tree seen-more)`
!!!(p) {:.unnumlist}

`  (values seen-once seen-more))`
!!!(p) {:.unnumlist}

`  (t (values (cons tree seen-once) seen-more))))`
!!!(p) {:.unnumlist}

