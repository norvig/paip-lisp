# Chapter 22
## Scheme: An Uncommon Lisp

> The best laid schemes o' mice an' men

> –Robert Burns (1759–1796)

This chapter presents the Scheme dialect of Lisp and an interpreter for it.
While it is not likely that you would use this interpreter for any serious programming, understanding how the interpreter works can give you a better appreciation of how Lisp works, and thus make you a better programmer.
A Scheme interpreter is used instead of a Common Lisp one because Scheme is simpler, and also because Scheme is an important language that is worth knowing about.

Scheme is the only dialect of Lisp besides Common Lisp that is currently flourishing.
Where Common Lisp tries to standardize all the important features that are in current use by Lisp programmers, Scheme tries to give a minimal set of very powerful features that can be used to implement the others.
It is interesting that among all the programming languages in the world, Scheme is one of the smallest, while Common Lisp is one of the largest.
The Scheme manual is only 45 pages (only 38 if you omit the example, bibliography, and index), while *Common Lisp the Language*, 2d edition, is 1029 pages.
Here is a partial list of the ways Scheme is simpler than Common Lisp:

[ ](#){:#l0010}1. Scheme has fewer built-in functions and special forms.
!!!(p) {:.numlist}

2. Scheme has no special variables, only lexical variables.
!!!(p) {:.numlist}

3. Scheme uses the same name space for functions and variables (and everything else).
!!!(p) {:.numlist}

4. Scheme evaluates the function part of a function call in exactly the same way as the arguments.
!!!(p) {:.numlist}

5. Scheme functions can not have optional and keyword parameters.
However, they can have the equivalent of a `&rest` parameter.
!!!(p) {:.numlist}

6. Scheme has no `block, return, go, orthrow`; a single function `(call/cc)` replaces all of these (and does much more).
!!!(p) {:.numlist}

7. Scheme has no packages.
Lexical variables can be used to implement package-like structures.
!!!(p) {:.numlist}

8. Scheme, as a standard, has no macros, although most implementations provide macros as an extension.
!!!(p) {:.numlist}

9. Scheme has no special forms for looping; instead it asks the user to use recursion and promises to implement the recursion efficiently.
!!!(p) {:.numlist}

The five main special forms in Scheme are `quote` and `if`, which are just as in Common Lisp; `begin` and `set!`, which are just different spellings for `progn` and `setq`; and `lambda`, which is as in Common Lisp, except that it doesn’t require a # 'before it.
In addition, Scheme allows variables, constants (numbers, strings, and characters), and function calls.
The function call is different because the function itself is evaluated in the same way as the arguments.
In Common Lisp, (`f x`) means to look up the function binding of `f` and apply that to the value of `x`.
In Scheme, `(f x)` means to evaluate `f` (in this case by looking up the value of the variable `f` ), evaluate `x` (by looking up the value of the variable in exactly the same way) and then apply the function to the argument.
Any expression can be in the function position, and it is evaluated just like the arguments.
Another difference is that Scheme uses `#t` and `#f` for true and false, instead of `t` and `nil`.
The empty list is denoted by `()`, and it is distinct from the false value, #f.
There are also minor lexical differences in the conventions for complex numbers and numbers in different bases, but these can be ignored for all the programs in this book.
Also, in Scheme a single macro, `define`, serves to define both variables and functions.

[ ](#){:#t0010}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| Scheme | Common Lisp |
| *var* | *var* |
| *constant* | *constant* |
| (`quote`*x*) or '*x* | (`quote`*x*) or '*x* |
| (`begin`*x*…) | (`progn`*x*…) |
| (`set!`*var x*) | (`setq`*var x*) |
| (`if`*pab*) | (`if`*pab*) |
| (`lambda`*parms x*…) | `#'` (`lambda`*parms x*…) |
| (*fn arg*…) | (*fn arg*…) or (`funcall`*fn arg*…) |
| `#t` | `t` |
| `#f` | `nil` |
| `( )` | `nil` |
| (`define`*varexp*) | (`defparameter`*var exp*) |
| (`define` (*fnparm*…) *body*) | (`defun`*fn* (*parm*…) *body*) |

**Exercise 22**.**1** [**s**] What does the following expression evaluate to in Scheme?
How many errors does it have as a Common Lisp expression?

[ ](#){:#l0015}`((if (= (+ 2 2) 4)`
!!!(p) {:.unnumlist}

`   (lambda (x y) (+ (* x y) 12))`
!!!(p) {:.unnumlist}

`   cons)`
!!!(p) {:.unnumlist}

` 5`
!!!(p) {:.unnumlist}

` 6)`
!!!(p) {:.unnumlist}

A great many functions, such as `car`, `cdr`, `cons`, `append`, +, `*`, and `list` are the same (or nearly the same) in both dialects.
However, Scheme has some spelling conventions that are different from Common Lisp.
Most Scheme mutators, like `set`!, end in ‘`!`’ Common Lisp has no consistent convention for this; some mutators start with `n` (`nreverse, nsubst, nintersection`) while others have idiosyncratic names (`delete versus remove`).
Scheme would use consistent names–`reverse`!
and `remove`!
–if these functions were defined at all (they are not defined in the standard).
Most Scheme predicates end in '`?`', not '`p`'.
This makes predicates more obvious and eliminates the complicated conventions for adding a hyphen before the `p`.[1](#fn0010){:#xfn0010} The only problem with this convention is in spoken language: is `equal?` pronounced “equal-question-mark” or “equal-q” or perhaps equal, with rising intonation?
This would make Scheme a tone language, like Chinese.

In Scheme, it is an error to apply `car` or `cdr` to the empty list.
Despite the fact that Scheme has `cons`, it calls the result a `pair` rather than a cons cell, so the predicate is `pair?`, not `consp`.

Scheme recognizes not all lambda expressions will be “functions” according to the mathematical definition of function, and so it uses the term “procedure” instead.
Here is a partial list of correspondences between the two dialects:

[ ](#){:#t0015}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| Scheme Procedure | Common Lisp Function |
| `char-ready?` | `listen` |
| `char?` | `characterp` |
| `eq?` | `eq` |
| `equal?` | `equal` |
| `eqv?` | `eql` |
| `even?` | `evenp` |
| `for-each` | `mapc` |
| `integer?` | `integerp` |
| `list->string` | `coerce` |
| `list->vector` | `coerce` |
| `list-ref` | `nth` |
| `list-tail` | `nthcdr` |
| `map` | `mapcar` |
| `negative?` | `minusp` |
| `pair?` | `consp` |
| `procedure?` | `functionp` |
| `set!` | `setq` |
| `set-car!` | `replaca` |
| `vector-set!` | `setf` |
| `string-set!` | `setf` |

## [ ](#){:#st0010}22.1 A Scheme Interpreter
{:#s0010}
{:.h1hd}

As we have seen, an interpreter takes a program (or expression) as input and returns the value computed by that program.
The Lisp function `eval` is thus an interpreter, and that is essentially the function we are trying to write in this section.
We have to be careful, however, in that it is possible to confuse the notions of interpreter and compiler.
A compiler takes a program as input and produces as output a translation of that program into some other language–usually a language that can be directly (or more easily) executed on some machine.
So it is also possible to write `eval` by compiling the argument and then interpreting the resulting machine-level program.
Most modern Lisp systems support both possibilities, although some only interpret code directly, and others compile all code before executing it.
To make the distinction clear, we will not write a function called `eval`.
Instead, we will write versions of two functions: `interp`, a Scheme interpreter, and, in the next chapter, `comp`, a Scheme compiler.

An interpreter that handles the Scheme primitives is easy to write.
In the interpreter `interp`, the main conditional has eight cases, corresponding to the five special forms, symbols, other atoms, and procedure applications (otherwise known as function calls).
For the moment we will stick with `t` and `nil` instead of `#t` and `#f`.
After developing a simple interpreter, we will add support for macros, then develop a tail-recursive interpreter, and finally a continuation-passing interpreter.
(These terms will be defined when the time cornes.).
The glossary for `interp` is in [figure 22.1](#f0010).

![f22-01-9780080571157](images/B9780080571157500224/f22-01-9780080571157.jpg)     
Figure 22.1
!!!(span) {:.fignum}
Glossary for the Scheme Interpreter
The simple interpreter has eight cases to worry about: (1) If the expression is a symbol, look up its value in the environment.
(2) If it is an atom that is not a symbol (such as a number), just return it.
Otherwise, the expression must be a list.
(3) If it starts with `quote`, return the quoted expression.
(4) If it starts with `begin`, interpret each subexpression, and return the last one.
(5) If it starts with `set!`, interpret the value and then set the variable to that value.
(6) If it starts with `if`, then interpret the conditional, and depending on if it is true or not, interpret the then-part or the else-part.
(7) If it starts with `lambda`, build a new procedure–a closure over the current environment.
(8) Otherwise, it must be a procedure application.
Interpret the procedure and all the arguments, and apply the procedure value to the argument values.

[ ](#){:#l0020}`(defun interp (x &optional env)`
!!!(p) {:.unnumlist}

` "Interpret (evaluate) the expression x in the environment env."`
!!!(p) {:.unnumlist}

` (cond`
!!!(p) {:.unnumlist}

`  ((symbolp x) (get-var x env))`
!!!(p) {:.unnumlist}

`  ((atom x) x)`
!!!(p) {:.unnumlist}

`  ((case (first x)`
!!!(p) {:.unnumlist}

`   (QUOTE (second x))`
!!!(p) {:.unnumlist}

`   (BEGIN (last1 (mapcar #'(lambda (y) (interp y env))`
!!!(p) {:.unnumlist}

`                (rest x))))`
!!!(p) {:.unnumlist}

`   (SET! (set-var!
(second x) (interp (third x) env) env))`
!!!(p) {:.unnumlist}

`   (IF   (if (interp (second x) env)`
!!!(p) {:.unnumlist}

`          (interp (third x) env)`
!!!(p) {:.unnumlist}

`          (interp (fourth x) env)))`
!!!(p) {:.unnumlist}

`   (LAMBDA (let ((parms (second x))`
!!!(p) {:.unnumlist}

`           (code (maybe-add 'begin (rest2 x))))`
!!!(p) {:.unnumlist}

`         #'(lambda (&rest args)`
!!!(p) {:.unnumlist}

`           (interp code (extend-env parms args env)))))`
!!!(p) {:.unnumlist}

`   (t   ;; a procedure application`
!!!(p) {:.unnumlist}

`        (apply (interp (first x) env)`
!!!(p) {:.unnumlist}

`           (mapcar #'(lambda (v) (interp v env))`
!!!(p) {:.unnumlist}

`                (rest x))))))))`
!!!(p) {:.unnumlist}

An environment is represented as an association list of variable/value pairs, except for the global environment, which is represented by values on the `global-val` property of symbols.
It would be simpler to represent the global environment in the same way as local environments, but it is more efficient to use property lists than one big global a-list.
Furthermore, the global environment is distinct in that every symbol is implicitly defined in the global environment, while local environments only contain variables that are explicitly mentioned (in a `lambda` expression).

As an example, suppose we interpret the function call `(f 1 2 3)`, and that the functions `f` has been defined by the Scheme expression:

[ ](#){:#l0025}`(set!
f (lambda (a b c) (+ a (g b c))))`
!!!(p) {:.unnumlist}

Then we will interpret `( f 1 2 3 )` by interpreting the body of `f` with the environment:

[ ](#){:#l0030}`((a 1) (b 2) (c 3))`
!!!(p) {:.unnumlist}

Scheme procedures are implemented as Common Lisp functions, and in fact all the Scheme data types are implemented by the corresponding Common Lisp types.
Iinclude the function `init-scheme- interp` to initialize a few global values and repeat the definitions of `last1` and `length=1`:

[ ](#){:#l0035}`(defun set-var!
(var val env)`
!!!(p) {:.unnumlist}

` "Set a variable to a value, in the given or global environment."`
!!!(p) {:.unnumlist}

` (if (assoc var env)`
!!!(p) {:.unnumlist}

`       (setf (second (assoc var env)) val)`
!!!(p) {:.unnumlist}

`       (set-global-var!
var val))`
!!!(p) {:.unnumlist}

` val)`
!!!(p) {:.unnumlist}

[ ](#){:#l0040}`(defun get-var (var env)`
!!!(p) {:.unnumlist}

` "Get the value of a variable, from the given or global environment."`
!!!(p) {:.unnumlist}

`  (if (assoc var env)`
!!!(p) {:.unnumlist}

`        (second (assoc var env))`
!!!(p) {:.unnumlist}

`        (get-global-var var)))`
!!!(p) {:.unnumlist}

[ ](#){:#l0045}`(defun set-global-var!
(var val)`
!!!(p) {:.unnumlist}

` (setf (get var 'global-val) val))`
!!!(p) {:.unnumlist}

[ ](#){:#l0050}`(defun get-global-var (var)`
!!!(p) {:.unnumlist}

` (let* ((default "unbound")`
!!!(p) {:.unnumlist}

`      (val (get var 'global-val default)))`
!!!(p) {:.unnumlist}

`   (if (eq val default)`
!!!(p) {:.unnumlist}

`     (error "Unbound scheme variable: ~ a" var`
!!!(p) {:.unnumlist}

`     val)))`
!!!(p) {:.unnumlist}

[ ](#){:#l0055}`(defun extend-env (vars vals env)`
!!!(p) {:.unnumlist}

` "Add some variables and values to an environment."`
!!!(p) {:.unnumlist}

` (nconc (mapcar #' list vars vals) env))`
!!!(p) {:.unnumlist}

[ ](#){:#l0060}`(defparameter *scheme-procs*`
!!!(p) {:.unnumlist}

` '(+−*/=<><=>= cons car cdr not append list read member`
!!!(p) {:.unnumlist}

`  (null?
null) (eq?
eq) (equal?
equal) (eqv?
eql)`
!!!(p) {:.unnumlist}

`  (write prin1) (display princ) (newline terpri)))`
!!!(p) {:.unnumlist}

[ ](#){:#l0065}`(defun init-scheme-interp ()`
!!!(p) {:.unnumlist}

` "Initialize the scheme interpreter with some global variables."`
!!!(p) {:.unnumlist}

` ;; Define Scheme procedures as CL functions:`
!!!(p) {:.unnumlist}

` (mapc #'init-scheme-proc *scheme-procs*)`
!!!(p) {:.unnumlist}

` ;; Define the Boolean 'constants'.
Unfortunately, this won't`
!!!(p) {:.unnumlist}

` ;; stop someone from saying: (set!
t nil)`
!!!(p) {:.unnumlist}

` (set-global-var!
t t)`
!!!(p) {:.unnumlist}

` (set-global-var!
nil nil))`
!!!(p) {:.unnumlist}

[ ](#){:#l0070}`(defun init-scheme-proc (f)`
!!!(p) {:.unnumlist}

` "Define a Scheme procedure as a corresponding CL function."`
!!!(p) {:.unnumlist}

` (if (listp f)`
!!!(p) {:.unnumlist}

`       (set-global-var!
(first f) (symbol-function (second f)))`
!!!(p) {:.unnumlist}

`       (set-global-var!
f (symbol-function f))))`
!!!(p) {:.unnumlist}

[ ](#){:#l0075}`(defun maybe-add (op exps &optional if-nil)`
!!!(p) {:.unnumlist}

` "For example, (maybe-add 'and exps t) returns`
!!!(p) {:.unnumlist}

` t if exps is nil, exps if there is only one,`
!!!(p) {:.unnumlist}

` and (and exp1 exp2…) if there are several exps."`
!!!(p) {:.unnumlist}

` (cond ((null exps) if-nil)`
!!!(p) {:.unnumlist}

`       ((length=1 exps) (first exps))`
!!!(p) {:.unnumlist}

`       (t (cons op exps))))`
!!!(p) {:.unnumlist}

[ ](#){:#l0080}`(defun length=1 (x)`
!!!(p) {:.unnumlist}

` "Is x a list of length 1?"`
!!!(p) {:.unnumlist}

` (and (consp x) (null (cdr x))))`
!!!(p) {:.unnumlist}

[ ](#){:#l0085}`(defun lastl (list)`
!!!(p) {:.unnumlist}

` "Return the last element (not last cons cell) of list"`
!!!(p) {:.unnumlist}

` (first (last list)))`
!!!(p) {:.unnumlist}

To test the interpreter, we add a simple read-eval-print loop:

[ ](#){:#l0090}`(defun scheme ()`
!!!(p) {:.unnumlist}

` "A Scheme read-eval-print loop (using interp)"`
!!!(p) {:.unnumlist}

` (init-scheme-interp)`
!!!(p) {:.unnumlist}

` (loop (format t "~&==> ")`
!!!(p) {:.unnumlist}

`    (print (interp (read) nil))))`
!!!(p) {:.unnumlist}

And now we’re ready to try out the interpreter.
Note the Common Lisp prompt is “>,” while the Scheme prompt is “==>.”

[ ](#){:#l0095}`> (scheme)`
!!!(p) {:.unnumlist}

`==> (+ 2 2)`
!!!(p) {:.unnumlist}

`4`
!!!(p) {:.unnumlist}

`==> ((if (= 1 2) * +) 3 4)`
!!!(p) {:.unnumlist}

`7`
!!!(p) {:.unnumlist}

`==> ((if (= 1 1) * +) 3 4)`
!!!(p) {:.unnumlist}

`12`
!!!(p) {:.unnumlist}

`==> (set!
fact (lambda (n)`
!!!(p) {:.unnumlist}

`        (if (= n 0) 1`
!!!(p) {:.unnumlist}

`          (* n (fact (− n 1))))))`
!!!(p) {:.unnumlist}

`#<DTP-LEXICAL-CLOSURE 36722615 >`
!!!(p) {:.unnumlist}

`==> (fact 5)`
!!!(p) {:.unnumlist}

`120`
!!!(p) {:.unnumlist}

`==> (set!
table (lambda (f start end)`
!!!(p) {:.unnumlist}

`          (if (<= start end)`
!!!(p) {:.unnumlist}

`            (begin`
!!!(p) {:.unnumlist}

`             (write (list start (f start)))`
!!!(p) {:.unnumlist}

`             (newline)`
!!!(p) {:.unnumlist}

`             (table f (+ start 1) end)))))`
!!!(p) {:.unnumlist}

`#<DTP-LEXICAL-CLOSURE 41072172 >`
!!!(p) {:.unnumlist}

`==> (table fact 1 10)`
!!!(p) {:.unnumlist}

`(1 1)`
!!!(p) {:.unnumlist}

`(2 2)`
!!!(p) {:.unnumlist}

`(3 6)`
!!!(p) {:.unnumlist}

`(4 24)`
!!!(p) {:.unnumlist}

`(5 120)`
!!!(p) {:.unnumlist}

`(6 720)`
!!!(p) {:.unnumlist}

`(7 5040)`
!!!(p) {:.unnumlist}

`(8 40320)`
!!!(p) {:.unnumlist}

`(9 362880)`
!!!(p) {:.unnumlist}

`(10 3628800)`
!!!(p) {:.unnumlist}

`NIL`
!!!(p) {:.unnumlist}

`==> (table (lambda (x) (* x x x)) 5 10)`
!!!(p) {:.unnumlist}

`(5 125)`
!!!(p) {:.unnumlist}

`(6 216)`
!!!(p) {:.unnumlist}

`(7 343)`
!!!(p) {:.unnumlist}

`(8 512)`
!!!(p) {:.unnumlist}

`(9 729)`
!!!(p) {:.unnumlist}

`(10 1000)`
!!!(p) {:.unnumlist}

`NIL`
!!!(p) {:.unnumlist}

`==> [ABORT]`
!!!(p) {:.unnumlist}

## [ ](#){:#st0015}22.2 Syntactic Extension with Macros
{:#s0015}
{:.h1hd}

Scheme has a number of other special forms that were not listed above.
Actually, Scheme uses the term “syntax” where we have been using “special form.” The remaining syntax can be defined as “derived expressions” in terms of the five primitives.
The Scheme standard does not recognize a concept of macros, but it is clear that a “derived expression” is like a macro, and we will implement them using macros.
The following forms are used (nearly) identically in Scheme and Common Lisp:

[ ](#){:#l0100}`let let* and or do cond case`
!!!(p) {:.unnumlist}

One difference is that Scheme is less lenient as to what counts as a binding in `let`, `let*` and `do`.
Every binding must be `(`*var init*`)`; just `(`*var*`)` or *var* is not allowed.
In do, a binding can be either (*var init step*) or (*var init*).
Notice there is no `do*`.
The other difference is in `case` and `cond`.
Where Common Lisp uses the symbol `t` or `otherwise` to mark the final case, Scheme uses `else`.
The final three syntactic extensions are unique to Scheme:

[ ](#){:#l0105}`(define *var val*)   *or*     (define (*proc*-*name arg*…) *body*…)`
!!!(p) {:.unnumlist}

`(delay *expression*)`
!!!(p) {:.unnumlist}

`(letrec ((*var init*)…) *body*…)`
!!!(p) {:.unnumlist}

`define` is a combination of `defun` and `defparameter`.
In its first form, it assigns a value to a variable.
Since there are no special variables in Scheme, this is no different than using `set!`.
(There is a difference when the `define` is nested inside another definition, but that is not yet considered.) In the second form, it defines a function.
`delay` is used to delay evaluation, as described in [section 9.3](B9780080571157500091.xhtml#s0020), page 281.
`letrec` is similar to `let`.
The difference is that all the *init* forms are evaluated in an environment that includes all the *vars*.
Thus, `letrec` can be used to define local recursive functions, just as `labels` does in Common Lisp.

The first step in implementing these syntactic extensions is to change `interp` to allow macros.
Only one clause has to be added, but we’ll repeat the whole definition:

[ ](#){:#l0110}`(defun interp (x &optional env)`
!!!(p) {:.unnumlist}

`  "Interpret (evaluate) the expression x in the environment env.`
!!!(p) {:.unnumlist}

`  This version handles macros."`
!!!(p) {:.unnumlist}

`  (cond`
!!!(p) {:.unnumlist}

`   ((symbolp x) (get-var x env))`
!!!(p) {:.unnumlist}

`   ((atom x) x)`
!!!(p) {:.unnumlist}

`   ((scheme-macro (first x)) ;***`
!!!(p) {:.unnumlist}

`    (interp (scheme-macro-expand x) env)) ;***`
!!!(p) {:.unnumlist}

`  ((case (first x)`
!!!(p) {:.unnumlist}

`       (QUOTE (second x))`
!!!(p) {:.unnumlist}

`(BEGIN (lastl (mapcar #'(lambda (y) (interp y env))`
!!!(p) {:.unnumlist}

`              (rest x))))`
!!!(p) {:.unnumlist}

`(SET! (set-var!
(second x) (interp (third x) env) env))`
!!!(p) {:.unnumlist}

`(IF  (if (interp (second x) env)`
!!!(p) {:.unnumlist}

`        (interp (third x) env)`
!!!(p) {:.unnumlist}

`        (interp (fourth x) env)))`
!!!(p) {:.unnumlist}

`(LAMBDA (let ((parms (second x))`
!!!(p) {:.unnumlist}

`         (code (maybe-add 'begin (rest2 x))))`
!!!(p) {:.unnumlist}

`     #'(lambda (&rest args)`
!!!(p) {:.unnumlist}

`         (interp code (extend-env parms args env)))))`
!!!(p) {:.unnumlist}

`(t     ;; a procedure application`
!!!(p) {:.unnumlist}

`      (apply (interp (first x) env)`
!!!(p) {:.unnumlist}

`          (mapcar #'(lambda (v) (interp v env))`
!!!(p) {:.unnumlist}

`                 (rest x))))))))`
!!!(p) {:.unnumlist}

Now we provide a mechanism for defining macros.
The macro definitions can be in any convenient language; the easiest choices are Scheme itself or Common Lisp.
I have chosen the latter.
This makes it clear that macros are not part of Scheme itself but rather are used to implement Scheme.
If we wanted to offer the macro facility to the Scheme programmer, we would make the other choice.
(But then we would be sure to add the backquote notation, which is so useful in writing macros.) `def-scheme-macro` (which happens to be a macro itself) provides a way of adding new Scheme macros.
It does that by storing a Common Lisp function on the `scheme-macro` property of a symbol.
This function, when given a list of arguments, returns the code that the macro call should expand into.
The function `scheme-macro` tests if a symbol has a macro attached to it, and `scheme-macro-expand` does the actual macro-expansion:

[ ](#){:#l0115}`(defun scheme-macro (symbol)`
!!!(p) {:.unnumlist}

` (and (symbolp symbol) (get symbol 'scheme-macro)))`
!!!(p) {:.unnumlist}

[ ](#){:#l0120}`(defmacro def-scheme-macro (name parmiist &body body)`
!!!(p) {:.unnumlist}

` "Define a Scheme macro."`
!!!(p) {:.unnumlist}

` '(setf (get ',name 'scheme-macro)`
!!!(p) {:.unnumlist}

`    #'(lambda .parmlist ..body)))`
!!!(p) {:.unnumlist}

[ ](#){:#l0125}`(defun scheme-macro-expand (x)`
!!!(p) {:.unnumlist}

` "Macro-expand this Scheme expression."`
!!!(p) {:.unnumlist}

` (if (and (listp x) (scheme-macro (first x)))`
!!!(p) {:.unnumlist}

`       (scheme-macro-expand`
!!!(p) {:.unnumlist}

`        (apply (scheme-macro (first x)) (rest x)))`
!!!(p) {:.unnumlist}

`       x))`
!!!(p) {:.unnumlist}

Here are the definitions of nine important macros in Scheme:

[ ](#){:#l0130}`(def-scheme-macro let (bindings &rest body)`
!!!(p) {:.unnumlist}

` '((lambda .(mapcar #'first bindings) .
,body)`
!!!(p) {:.unnumlist}

`  .,(mapcar #'second bindings)))`
!!!(p) {:.unnumlist}

[ ](#){:#l0135}`(def-scheme-macro let* (bindings &rest body)`
!!!(p) {:.unnumlist}

` (if (null bindings)`
!!!(p) {:.unnumlist}

`       '(begin .,body)`
!!!(p) {:.unnumlist}

`       '(let (,(first bindings))`
!!!(p) {:.unnumlist}

`     (let* ,(rest bindings) .
,body))))`
!!!(p) {:.unnumlist}

[ ](#){:#l0140}`(def-scheme-macro and (&rest args)`
!!!(p) {:.unnumlist}

` (cond ((null args) 'T)`
!!!(p) {:.unnumlist}

`     ((length=1 args) (first args))`
!!!(p) {:.unnumlist}

`     (t '(if ,(first args)`
!!!(p) {:.unnumlist}

`          (and .
,(rest args))))))`
!!!(p) {:.unnumlist}

[ ](#){:#l0145}`(def-scheme-macro or (&rest args)`
!!!(p) {:.unnumlist}

` (cond ((null args) 'nil)`
!!!(p) {:.unnumlist}

`    ((length=1 args) (first args))`
!!!(p) {:.unnumlist}

`    (t (let ((var (gensym)))`
!!!(p) {:.unnumlist}

`        '(let ((,var ,(first args)))`
!!!(p) {:.unnumlist}

`         (if ,var ,var (or .
,(rest args))))))))`
!!!(p) {:.unnumlist}

[ ](#){:#l0150}`(def-scheme-macro cond (&rest clauses)`
!!!(p) {:.unnumlist}

` (cond ((null clauses) nil)`
!!!(p) {:.unnumlist}

`     ((length=1 (first clauses))`
!!!(p) {:.unnumlist}

`      '(or ,(first clauses) (cond .,(rest clauses))))`
!!!(p) {:.unnumlist}

`     ((starts-with (first clauses) 'else)`
!!!(p) {:.unnumlist}

`      '(begin .,(rest (first clauses))))`
!!!(p) {:.unnumlist}

`     (t '(if ,(first (first clauses))`
!!!(p) {:.unnumlist}

`          (begin .,(rest (first clauses)))`
!!!(p) {:.unnumlist}

`          (cond .,(rest clauses))))))`
!!!(p) {:.unnumlist}

[ ](#){:#l0155}`(def-scheme-macro case (key &rest clauses)`
!!!(p) {:.unnumlist}

` (let ((key-val (gensym "KEY")))`
!!!(p) {:.unnumlist}

`  '(let ((,key-val ,key))`
!!!(p) {:.unnumlist}

`   (cond ,@(mapcar`
!!!(p) {:.unnumlist}

`        #'(lambda (clause)`
!!!(p) {:.unnumlist}

`          (if (starts-with clause 'else)`
!!!(p) {:.unnumlist}

`            clause`
!!!(p) {:.unnumlist}

`            '((member ,key-val ',(first clause))`
!!!(p) {:.unnumlist}

`                .,(rest clause))))`
!!!(p) {:.unnumlist}

`        clauses)))))`
!!!(p) {:.unnumlist}

[ ](#){:#l0160}`(def-scheme-macro define (name &rest body)`
!!!(p) {:.unnumlist}

` (if (atom name)`
!!!(p) {:.unnumlist}

`       '(begin (set!
,name .
,body) ',name)`
!!!(p) {:.unnumlist}

`       '(define ,(first name)`
!!!(p) {:.unnumlist}

`     (lambda ,(rest name) .
,body))))`
!!!(p) {:.unnumlist}

[ ](#){:#l0165}`(def-scheme-macro delay (computation)`
!!!(p) {:.unnumlist}

` '(lambda () ,computation))`
!!!(p) {:.unnumlist}

[ ](#){:#l0170}`(def-scheme-macro letrec (bindings &rest body)`
!!!(p) {:.unnumlist}

` '(let ,(mapcar #'(lambda (v) (list (first v) nil)) bindings)`
!!!(p) {:.unnumlist}

`    ,@(mapcar #'(lambda (v) '(set!
.
,v)) bindings)`
!!!(p) {:.unnumlist}

`   .,body))`
!!!(p) {:.unnumlist}

We can test out the macro facility:

[ ](#){:#l0175}`> (scheme-macro-expand '(and p q)) => (IF P (AND Q))`
!!!(p) {:.unnumlist}

`> (scheme-macro-expand '(and q)) Q`
!!!(p) {:.unnumlist}

`> (scheme-macro-expand '(let ((x 1) (y 2)) (+ x y)))`⇒
!!!(p) {:.unnumlist}

`((LAMBDA (X Y) (+ X Y)) 1 2)`
!!!(p) {:.unnumlist}

[ ](#){:#l0180}`> (scheme-macro-expand`
!!!(p) {:.unnumlist}

`  '(letrec`
!!!(p) {:.unnumlist}

`    ((even?
(lambda (x) (or (= x 0) (odd?
(− x 1)))))`
!!!(p) {:.unnumlist}

`     (odd?
(lambda (x) (even?
(− x 1)))))`
!!!(p) {:.unnumlist}

`    (even?
z)))`⇒
!!!(p) {:.unnumlist}

`(LET ((EVEN?
NIL)`
!!!(p) {:.unnumlist}

`       (ODD?
NIL))`
!!!(p) {:.unnumlist}

` (SET!
EVEN?
(LAMBDA (X) (OR (= X 0) (ODD?
(− X 1)))))`
!!!(p) {:.unnumlist}

` (SET!
ODD?
(LAMBDA (X) (EVEN?
(− X 1))))`
!!!(p) {:.unnumlist}

` (EVEN?
Z))`
!!!(p) {:.unnumlist}

`> (scheme)`
!!!(p) {:.unnumlist}

`==> (define (reverse 1)`
!!!(p) {:.unnumlist}

`   (if (null?
1) nil`
!!!(p) {:.unnumlist}

`      (append (reverse (cdr 1)) (list (car 1)))))`
!!!(p) {:.unnumlist}

`REVERSE`
!!!(p) {:.unnumlist}

`==> (reverse '(a b c d))`
!!!(p) {:.unnumlist}

`(D C B A)`
!!!(p) {:.unnumlist}

`==> (let* ((x 5) (y (+ x x)))`
!!!(p) {:.unnumlist}

`   (if (or (= x 0) (and (< 0 y) (< y 20)))`
!!!(p) {:.unnumlist}

`      (list x y)`
!!!(p) {:.unnumlist}

`      (+ y x)))`
!!!(p) {:.unnumlist}

`(5 10)`
!!!(p) {:.unnumlist}

The macro `define` is just like `set!`, except that it returns the symbol rather than the value assigned to the symbol.
In addition, `define` provides an optional syntax for defining functions–it serves the purposes of both `defun` and `defvar`.
The syntax (`define` (*fn*.
*args*).*body*) is an abbreviation for (`define`*fn* (`lambda`*args*.
*body*)).

In addition, Scheme provides a notation where `define` can be used inside a function definition in a way that makes it work like `let` rather than `set!.`

The advantage of the macro-based approach to special forms is that we don’t have to change the interpreter to add new special forms.
The interpreter remains simple, even while the language grows.
This also holds for the compiler, as we see in the next section.

## [ ](#){:#st0020}22.3 A Properly Tail-Recursive Interpreter
{:#s0020}
{:.h1hd}

Unfortunately, the interpreter presented above can not lay claim to the name Scheme, because a true Scheme must be properly tail-recursive.
Our interpreter is tail- recursive only when run in a Common Lisp that is tail-recursive.
To see the problem, consider the following Scheme procedure:

[ ](#){:#l0185}`(define (traverse lyst)`
!!!(p) {:.unnumlist}

` (if lyst (traverse (cdr lyst))))`
!!!(p) {:.unnumlist}

Trace the function `interp` and execute `(interp '(traverse '(a b c d)))`.
The nested calls to `interp` go 16 levels deep.
In general, the level of nesting is 4 plus 3 times the length of the list.
Each call to `interp` requires Common Lisp to allocate some storage on the stack, so for very long lists, we will eventually run out of storage.
To earn the name Scheme, a language must guarantee that such a program does not run out of storage.

The problem, in this example, lies in two places.
Everytime we interpret an `if` form or a procedure call, we descend another recursive level into `interp`.
But that extra level is not necessary.
Consider the `if` form.
It is certainly necessary to call `interp` recursively to decide if the test is true or not.
For the sake of argument, let’s say the test is true.
Then we call `interp` again on the *then* part.
This recursive call will return a value, which will then be immediately returned as the value of the original call as well.

The alternative is to replace the recursive call to `interp` with a renaming of variables, followed by a `goto` statement.
That is, instead of calling `interp` and thereby binding a new instance of the variable `x` to the *then* part, we just assign the *then* part to `x`, and branch to the top of the `interp` routine.
This works because we know we have no more use for the old value of `x`.
A similar technique is used to eliminate the recursive call for the last expression in a `begin` form.
(Many programmers have been taught the “structured programming” party line that `goto` statements are harmful.
In this case, the `goto` is necessary to implement a low-level feature efficiently.)

The final thing we need to do is explicitly manage Scheme procedures.
Instead of implementing Scheme procedures as Common Lisp closures, we will define a structure, `proc`, to contain the code, environment, parameter list, and optionally the name of the procedure.
Then when we are evaluating a procedure call, we can assign the body of the procedure to `x` rather than recursively calling `interp`.

[ ](#){:#l0190}`(defstruct (proc (:print-function print-proc))`
!!!(p) {:.unnumlist}

` "Represent a Scheme procedure"`
!!!(p) {:.unnumlist}

` code (env nil)(name nil) (parms nil))`
!!!(p) {:.unnumlist}

The following is a properly tail-recursive interpreter.
The macro `prog` sets up a `tagbody` within which we can use `go` statements to branch to labels, and it also sets up a `block` from which we can return a value.
It can also bind variables like `let`, although in this usage, the variable list is empty.
Any symbol within the body of a `prog` is considered a label.
In this case, the label : `INTERP` is the target of the branch statements `(GO : INTERP)`.
I use uppercase to indicate that go-to statements are being used, but this convention has not been widely adopted.

[ ](#){:#l0195}`(defun interp (x &optional env)`
!!!(p) {:.unnumlist}

` "Evaluate the expression x in the environment env.`
!!!(p) {:.unnumlist}

` This version is properly tail-recursive."`
!!!(p) {:.unnumlist}

` (prog ()`
!!!(p) {:.unnumlist}

`  :INTERP`
!!!(p) {:.unnumlist}

`  (return`
!!!(p) {:.unnumlist}

`   (cond`
!!!(p) {:.unnumlist}

`    ((symbolp x) (get-var x env))`
!!!(p) {:.unnumlist}

`    ((atom x) x)`
!!!(p) {:.unnumlist}

`    ((scheme-macro (first x))`
!!!(p) {:.unnumlist}

`     (setf x (scheme-macro-expand x)) (go :INTERP))`
!!!(p) {:.unnumlist}

`    ((case (first x)`
!!!(p) {:.unnumlist}

`      (QUOTE (second x))`
!!!(p) {:.unnumlist}

`      (BEGIN (pop x) ; pop off the BEGIN to get at the args`
!!!(p) {:.unnumlist}

`           ;; Now interpret all but the last expression`
!!!(p) {:.unnumlist}

`           (loop while (rest x) do (interp (pop x) env))`
!!!(p) {:.unnumlist}

`           ;; Finally, rename the last expression as x`
!!!(p) {:.unnumlist}

`           (setf x (first x))`
!!!(p) {:.unnumlist}

`           (GO :INTERP))`
!!!(p) {:.unnumlist}

`      (SET!  (set-var!
(second x) (interp (third x) env) env))`
!!!(p) {:.unnumlist}

`      (IF       (setf x (if (interp (second x) env)`
!!!(p) {:.unnumlist}

`                (third x)`
!!!(p) {:.unnumlist}

`                (fourth x)))`
!!!(p) {:.unnumlist}

`           ;; That is, rename the right expression as x`
!!!(p) {:.unnumlist}

`           (GO :INTERP))`
!!!(p) {:.unnumlist}

`      (LAMBDA (make-proc :env env :parms (second x)`
!!!(p) {:.unnumlist}

`                :code (maybe-add 'begin (rest2 x))))`
!!!(p) {:.unnumlist}

`      (t   ;; a procedure application`
!!!(p) {:.unnumlist}

`          (let ((proc (interp (first x) env))`
!!!(p) {:.unnumlist}

`             (args (mapcar #'(lambda (v) (interp v env))`
!!!(p) {:.unnumlist}

`                           (rest x))))`
!!!(p) {:.unnumlist}

`           (if (proc-p proc)`
!!!(p) {:.unnumlist}

`              ;; Execute procedure with rename+goto`
!!!(p) {:.unnumlist}

`              (progn`
!!!(p) {:.unnumlist}

`               (setf x (proc-code proc))`
!!!(p) {:.unnumlist}

`               (setf env (extend-env (proc-parms proc) args`
!!!(p) {:.unnumlist}

`                                     (proc-env proc)))`
!!!(p) {:.unnumlist}

`               (GO :INTERP))`
!!!(p) {:.unnumlist}

`              ;; else apply primitive procedure`
!!!(p) {:.unnumlist}

`              (apply proc args))))))))))`
!!!(p) {:.unnumlist}

`(defun print-proc (proc &optional (stream *standard-output*) depth)`
!!!(p) {:.unnumlist}

` (declare (ignore depth))`
!!!(p) {:.unnumlist}

` (format stream "{~a}" (or (proc-name proc) '??)))`
!!!(p) {:.unnumlist}

By tracing the tail-recursive version of `interp`, you can see that calls to `traverse` descend only three recursive levels of `interp`, regardless of the length of the list traversed.

Note that we are not claiming that this interpreter allocates no storage when it makes tail-recursive calls.
Indeed, it wastes quite a bit of storage in evaluating arguments and building environments.
The claim is that since the storage is allocated on the heap rather than on the stack, it can be reclaimed by the garbage collector.
So even if `traverse` is applied to an infinitely long list (i.e., a circular list), the interpreter will never run out of space–it will always be able to garbage-collect and continue.

There are many improvements that could be made to this interpreter, but effort is better spent in improving a compiler rather than an interpreter.
The next chapter does just that.

## [ ](#){:#st0025}22.4 Throw, Catch, and Call/cc
{:#s0025}
{:.h1hd}

Tail-recursion is crucial to Scheme.
The idea is that when the language is guaranteed to optimize tail-recursive calls, then there is no need for special forms to do iteration.
All loops can be written using recursion, without any worry of overflowing the runtime stack.
This helps keep the language simple and rules out the `goto` statement, the scourge of the structured programming movement.
However, there are cases where some kind of nonlocal exit is the best alternative.
Suppose that some unexpected event happens deep inside your program.
The best action is to print an error message and pop back up to the top level of your program.
This could be done trivially with a goto-like statement.
Without it, every function along the calling path would have to be altered to accept either a valid result or an indication of the exceptional condition, which just gets passed up to the next level.

In Common Lisp, the functions `throw` and `catch` are provided for this kind of nonlocal exit.
Scott Zimmerman, the perennial world Frisbee champion, is also a programmer for a Southern California firm.
He once told me, “I’m starting to learn Lisp, and it must be a good language because it’s got `throw` and `catch` in it.” Unfortunately for `Scott`, `throw` and `catch` don’t refer to Frisbees but to transfer of control.
They are both special forms, with the following syntax:

[ ](#){:#l0200}`(catch tag body…)`
!!!(p) {:.unnumlist}

`(throw tag value)`
!!!(p) {:.unnumlist}

The first argument to `catch` is a tag, or label.
The remaining arguments are evaluated one at a time, and the last one is returned.
Thus, `catch` is much like `progn`.
The difference is that if any code in the dynamic extent of the body of the `catch` evaluates the special form `throw`, then control is immediately passed to the enclosing `catch` with the same tag.

For example, the form

[ ](#){:#l0205}`(catch 'tag`
!!!(p) {:.unnumlist}

` (print 1) (throw 'tag 2) (print 3))`
!!!(p) {:.unnumlist}

prints `1` and returns `2`, without going on to print `3`.
A more representative example is:

[ ](#){:#l0210}`(defun print-table (l)`
!!!(p) {:.unnumlist}

` (catch 'not-a-number (mapcar #'print-sqrt-abs l)))`
!!!(p) {:.unnumlist}

[ ](#){:#l0215}`(defun print-sqrt-abs (x)`
!!!(p) {:.unnumlist}

` (print (sqrt (abs (must-be-number x)))))`
!!!(p) {:.unnumlist}

[ ](#){:#l0220}`(defun must-be-number (x)`
!!!(p) {:.unnumlist}

` (if (numberp x) x`
!!!(p) {:.unnumlist}

`   (throw 'not-a-number "huh?")))`
!!!(p) {:.unnumlist}

[ ](#){:#l0225}`> (print-table '(1 4 –9 x 10 20))`
!!!(p) {:.unnumlist1}

`1`
!!!(p) {:.unnumlist1}

`2`
!!!(p) {:.unnumlist1}

`3`
!!!(p) {:.unnumlist1}

`"huh?"`
!!!(p) {:.unnumlist1}

Here `print-table` calls `print-sqrt-abs`, which calls `must-be-number`.
The first three times all is fine and the values 1,2,3 get printed.
The next time `x` is not a number, so the value `“huh?”` gets thrown to the tag `not-a-number` established by `catch` in `f`.
The throw bypasses the pending calls to `abs`, `sqrt`, and `print`, as well as the rest of the call to `mapcar`.

This kind of control is provided in Scheme with a very general and powerful procedure, `call-with-current-continuation`, which is often abbreviated `call/cc.
call/cc` is a normal procedure (not a special form like `throw` and `catch`) that takes a single argument.
Let’s call the argument `computation`.
`computation` must be a procedure of one argument.
When `call/cc` is invoked, it calls `computation`, and whatever `computation` returns is the value of the call to `call/cc`.
The trick is that the procedure `computation` also takes an argument (which we’ll call `cc`) that is another procedure representing the current continuation point.
If `cc` is applied to some value, that value is returned as the value of the call to `call/cc`.
Here are some examples:

[ ](#){:#l0230}`> (scheme)`
!!!(p) {:.unnumlist}

`=> (+ 1 (call/cc (lambda (cc) (+ 20 300))))`
!!!(p) {:.unnumlist}

`321`
!!!(p) {:.unnumlist}

This example ignores `cc` and just computes `(+ 1 (+ 20 300 ))`.
More precisely, it is equivalent to:

[ ](#){:#l0235}`((lambda (val) (+ 1 val))`
!!!(p) {:.unnumlist}

` (+ 20 300))`
!!!(p) {:.unnumlist}

The next example does make use of `cc`:

[ ](#){:#l0240}`=> (+ 1 (call/cc (lambda (cc) (+ 20 (cc 300)))))`
!!!(p) {:.unnumlist}

`301`
!!!(p) {:.unnumlist}

This passes `300` to `cc`, thus bypassing the addition of `20`.
It effectively throws `300` out of the computation to the catch point established by `call/cc`.
It is equivalent to:

[ ](#){:#l0245}`((lambda (val) (+ 1 val))`
!!!(p) {:.unnumlist}

` 300)`
!!!(p) {:.unnumlist}

or to:

[ ](#){:#l0250}`((lambda (val) (+ 1 val))`
!!!(p) {:.unnumlist}

` (catch 'cc`
!!!(p) {:.unnumlist}

`  ((lambda (v) (+ 20 v))`
!!!(p) {:.unnumlist}

`   (throw 'cc 300))))`
!!!(p) {:.unnumlist}

Here’s how the `throw/catch` mechanism would look in Scheme:

[ ](#){:#l0255}`(define (print-table l )`
!!!(p) {:.unnumlist}

` (call/cc`
!!!(p) {:.unnumlist}

`  (lambda (escape)`
!!!(p) {:.unnumlist}

`   (set!
not-a-number escape)`
!!!(p) {:.unnumlist}

`   (map print-sqrt-abs l))))`
!!!(p) {:.unnumlist}

[ ](#){:#l0260}`(define (print-sqrt-abs x)`
!!!(p) {:.unnumlist}

` (write (sqrt (abs (must-be-number x)))))`
!!!(p) {:.unnumlist}

[ ](#){:#l0265}`(define (must-be-number x)`
!!!(p) {:.unnumlist}

` (if (numberp x) x`
!!!(p) {:.unnumlist}

`   (not-a-number "huh?")))`
!!!(p) {:.unnumlist}

[ ](#){:#l0270}`(define (map fn l)`
!!!(p) {:.unnumlist}

` (if (null?
l)`
!!!(p) {:.unnumlist}

`   '()`
!!!(p) {:.unnumlist}

`   (cons (fn (first l))`
!!!(p) {:.unnumlist}

`       (map fn (rest 1)))))`
!!!(p) {:.unnumlist}

The ability to return to a pending point in the computation is useful for this kind of error and interrupt handling.
However, the truly amazing, wonderful thing about `call/cc` is the ability to return to a continuation point more than once.
Consider a slight variation:

[ ](#){:#l0275}`=> (+ 1 (call/cc (lambda (cc)`
!!!(p) {:.unnumlist}

`           (set!
old-cc cc)`
!!!(p) {:.unnumlist}

`           (+ 20 (cc 300)))))`
!!!(p) {:.unnumlist}

`301`
!!!(p) {:.unnumlist}

`=> (old-cc 500)`
!!!(p) {:.unnumlist}

`501`
!!!(p) {:.unnumlist}

Here, we first computed 301, just as before, but along the way saved `cc` in the global variable `old-cc`.
Afterward, calling `(old-cc 500)` returns (for the second time) to the point in the computation where 1 is added, this time returning `501`.
The equivalent Common Lisp code leads to an error:

[ ](#){:#l0280}`> (+ 1 (catch 'tag (+ 20 (throw 'tag 300))))`
!!!(p) {:.unnumlist}

`301`
!!!(p) {:.unnumlist}

`> (throw 'tag 500)`
!!!(p) {:.unnumlist}

`*Error*: *there was no pending CATCH for the tag TAG*`
!!!(p) {:.unnumlist}

In other words, `call/cc`'s continuations have indefinite extent, while throw/catch tags only have dynamic extent.

We can use `cal1/cc` to implement automatic backtracking (among other things).
Suppose we had a special form, `amb`, the “ambiguous” operator, which returns one of its arguments, chosen at random.
We could write:

[ ](#){:#l0285}`(define (integer) (amb 1 (+ 1 (integer))))`
!!!(p) {:.unnumlist}

and a call to `integer` would return some random positive integer.
In addition, suppose we had a function, `fail`, which doesn’t return at all but instead causes execution to continue at a prior `amb` point, with the other choice taken.
Then we could write succinct[2](#fn0015){:#xfn0015} backtracking code like the following:

[ ](#){:#l0290}`(define (prime)`
!!!(p) {:.unnumlist}

` (let ((n (integer)))`
!!!(p) {:.unnumlist}

` (if (prime?
n) n (fail))))`
!!!(p) {:.unnumlist}

If `prime?` is a predicate that returns true only when its argument is a prime number, then prime will always return some `prime` number, decided by generating random integers.
While this looks like a major change to the language–adding backtracking and nondeterminism–it turns out that `amb` and `fail` can be implemented quite easily with `cal1/cc`.
First, we need to make `amb` be a macro:

[ ](#){:#l0295}`(def-scheme-macro amb (x y)`
!!!(p) {:.unnumlist}

` '(random-choice (lambda () ,x) (lambda () ,y))))`
!!!(p) {:.unnumlist}

The rest is pure Scheme.
We maintain a list of `backtrack-points`, which are implemented as functions of no arguments.
To backtrack, we just call one of these functions.
That is what `fail` does.
The function `choose-first` takes two functions and pushes the second, along with the proper continuation, on `backtrack-points`, and then calls the first, returning that value.
The function `random-choice` is what `amb` expands into: it decides which choice is first, and which is second.
(Note that the convention in Scheme is to write global variables like `backtrack-points` without asterisks.)

[ ](#){:#l0300}`(define backtrack-points nil)`
!!!(p) {:.unnumlist}

[ ](#){:#l0305}`(define (fail)`
!!!(p) {:.unnumlist}

` (let ((last-choice (car backtrack-points)))`
!!!(p) {:.unnumlist}

`  (set!
backtrack-points (cdr backtrack-points))`
!!!(p) {:.unnumlist}

`  (last-choice)))`
!!!(p) {:.unnumlist}

[ ](#){:#l0310}`(define (random-choice f g)`
!!!(p) {:.unnumlist}

` (if (= 1 (random 2))`
!!!(p) {:.unnumlist}

`   (choose-first f g)`
!!!(p) {:.unnumlist}

`   (choose-first g f)))`
!!!(p) {:.unnumlist}

[ ](#){:#l0315}`(define (choose-first f g)`
!!!(p) {:.unnumlist}

` (call/cc`
!!!(p) {:.unnumlist}

`  (lambda (k)`
!!!(p) {:.unnumlist}

`   (set!
backtrack-points`
!!!(p) {:.unnumlist}

`      (cons (lambda () (k (g))) backtrack-points))`
!!!(p) {:.unnumlist}

`   (f))))`
!!!(p) {:.unnumlist}

This implements chronological backtracking, as in Prolog.
However, we actually have the freedom to do other kinds of backtracking as well.
Instead of having `fail` take the first element of `backtrack-points`, we could choose a random element instead.
Or, we could do some more complex analysis to choose a good backtrack point.

`call/cc` can be used to implement a variety of control structures.
As another example, many Lisp implementations provide a `reset` function that aborts the current computation and returns control to the top-level read-eval-print loop.
reset can be defined quite easily using `call/cc`.
The trick is to capture a continuation that is at the top level and save it away for future use.
The following expression, evaluated at the top level, saves the appropriate continuation in the value of reset:

[ ](#){:#l0320}`(call/cc (lambda (cc) (set!
reset (lambda ()`
!!!(p) {:.unnumlist}

`                (cc "Back to top level")))))`
!!!(p) {:.unnumlist}

**Exercise 22.2 [m]** Can you implement `call/cc` in Common Lisp?

**Exercise 22.3 [s]** Can you implement `amb` and `fail` in Common Lisp?

**Exercise 22.4 [m]**`fail` could be written

`(define (fail) ((pop backtrack-points)))` if we had the pop macro in Scheme.

Write `pop.`

## [ ](#){:#st0030}22.5 An Interpreter Supporting Call/cc
{:#s0030}
{:.h1hd}

It is interesting that the more a host language has to offer, the easier it is to write an interpreter.
Perhaps the hardest part of writing a Lisp interpreter (or compiler) is garbage collection.
By writing our interpreter in Lisp, we bypassed the problem all together–the host language automatically collects garbage.
Similarly, if we are using a Common Lisp that is properly tail-recursive, then our interpreter will be too, without taking any special steps.
If not, the interpreter must be rewritten to take care of tail-recursion, as we have seen above.

It is the same with `call/cc`.
If our host language provides continuations with indefinite extent, then it is trivial to implement `call/cc`.
If not, we have to rewrite the whole interpreter, so that it explicitly handles continuations.
The best way to do this is to make `interp` a function of three arguments: an expression, an environment, and a continuation.
That means the top level will have to change too.
Rather than having `interp` return a value that gets printed, we just pass it the function `print` as a continuation:

[ ](#){:#l0325}`(defun scheme ()`
!!!(p) {:.unnumlist}

`  "A Scheme read-eval-print loop (using interp).`
!!!(p) {:.unnumlist}

`  Handles call/cc by explicitly passing continuations."`
!!!(p) {:.unnumlist}

`  (init-scheme-interp)`
!!!(p) {:.unnumlist}

`  (loop (format t "~&==> ")`
!!!(p) {:.unnumlist}

`       (interp (read) nil #'print)))`
!!!(p) {:.unnumlist}

Now we are ready to tackle `interp`.
For clarity, we will base it on the non-tail-recursive version.
The cases for symbols, atoms, macros, and `quote` are almost the same as before.
The difference is that the result of each computation gets passed to the continuation, `cc`, rather than just being returned.

The other cases are all more complex, because they all require explicit representation of continuations.
That means that calls to `interp` cannot be nested.
Instead, we call `interp` with a continuation that includes another call to `interp`.
For example, to interpret (`if p x y`), we first call `interp` on the second element of the form, the predicate `p`.
The continuation for this call is a function that tests the value of `p` and interprets either `x` or `y` accordingly, using the original continuation for the recursive call to `interp`.
The other cases are similar.
One important change is that Scheme procedures are implemented as Lisp functions where the first argument is the continuation:

[ ](#){:#l0330}`(defun interp (x env cc)`
!!!(p) {:.unnumlist}

` "Evaluate the expression x in the environment env,`
!!!(p) {:.unnumlist}

` and pass the result to the continuation cc."`
!!!(p) {:.unnumlist}

` (cond`
!!!(p) {:.unnumlist}

`  ((symbolp x) (funcall cc (get-var x env)))`
!!!(p) {:.unnumlist}

`  ((atom x) (funcall cc x))`
!!!(p) {:.unnumlist}

`  ((scheme-macro (first x))`
!!!(p) {:.unnumlist}

`   (interp (scheme-macro-expand x) env cc))`
!!!(p) {:.unnumlist}

`  ((case (first x)`
!!!(p) {:.unnumlist}

`     (QUOTE (funcall cc (second x)))`
!!!(p) {:.unnumlist}

`     (BEGIN (interp-begin (rest x) env cc))`
!!!(p) {:.unnumlist}

`(SET!  (interp (third x) env`
!!!(p) {:.unnumlist}

`          #'(lambda (val)`
!!!(p) {:.unnumlist}

`             (funcall cc (set-var!
(second x)`
!!!(p) {:.unnumlist}

`                                    val env)))))`
!!!(p) {:.unnumlist}

`(IF   (interp (second x) env`
!!!(p) {:.unnumlist}

`          #'(lambda (pred)`
!!!(p) {:.unnumlist}

`             (interp (if pred (third x) (fourth x))`
!!!(p) {:.unnumlist}

`                env cc))))`
!!!(p) {:.unnumlist}

`(LAMBDA (let ((parms (second x))`
!!!(p) {:.unnumlist}

`         (code (maybe-add 'begin (rest2 x))))`
!!!(p) {:.unnumlist}

`       (funcall`
!!!(p) {:.unnumlist}

`        cc`
!!!(p) {:.unnumlist}

`        #'(lambda (cont &rest args)`
!!!(p) {:.unnumlist}

`          (interp code`
!!!(p) {:.unnumlist}

`               (extend-env parms args env)`
!!!(p) {:.unnumlist}

`               cont)))))`
!!!(p) {:.unnumlist}

`(t   (interp-call x env cc))))))`
!!!(p) {:.unnumlist}

A few auxiliary functions are defined, in the same continuation-passing style:

[ ](#){:#l0335}`(defun interp-begin (body env cc)`
!!!(p) {:.unnumlist}

` "Interpret each element of BODY, passing the last to CC."`
!!!(p) {:.unnumlist}

` (interp (first body) env`
!!!(p) {:.unnumlist}

`     #'(lambda (val)`
!!!(p) {:.unnumlist}

`       (if (null (rest body))`
!!!(p) {:.unnumlist}

`           (funcall cc val)`
!!!(p) {:.unnumlist}

`           (interp-begin (rest body) env cc)))))`
!!!(p) {:.unnumlist}

[ ](#){:#l0340}`(defun interp-call (call env cc)`
!!!(p) {:.unnumlist}

` "Interpret the call (f x…) and pass the result to CC."`
!!!(p) {:.unnumlist}

` (map-interp call env`
!!!(p) {:.unnumlist}

`         #'(lambda (fn-and-args)`
!!!(p) {:.unnumlist}

`           (apply (first fn-and-args)`
!!!(p) {:.unnumlist}

`                cc`
!!!(p) {:.unnumlist}

`                (rest fn-and-args)))))`
!!!(p) {:.unnumlist}

[ ](#){:#l0345}`(defun map-interp (list env cc)`
!!!(p) {:.unnumlist}

` "Interpret each element of LIST, and pass the list to CC."`
!!!(p) {:.unnumlist}

` (if (null list)`
!!!(p) {:.unnumlist}

`    (funcall cc nil)`
!!!(p) {:.unnumlist}

`    (interp (first list) env`
!!!(p) {:.unnumlist}

`         #'(lambda (x)`
!!!(p) {:.unnumlist}

`           (map-interp (rest list) env`
!!!(p) {:.unnumlist}

`                #'(lambda (y)`
!!!(p) {:.unnumlist}

`                (funcall cc (cons x y))))))))`
!!!(p) {:.unnumlist}

Because Scheme procedures expect a continuation as the first argument, we need to redefine `init-scheme-proc` to install procedures that accept and apply the continuation:

[ ](#){:#l0350}`(defun init-scheme-proc (f)`
!!!(p) {:.unnumlist}

` "Define a Scheme primitive procedure as a CL function."`
!!!(p) {:.unnumlist}

` (if (listp f)`
!!!(p) {:.unnumlist}

`    (set-global-var!
(first f)`
!!!(p) {:.unnumlist}

`               #'(lambda (cont &rest args)`
!!!(p) {:.unnumlist}

`                (funcall cont (apply (second f) args))))`
!!!(p) {:.unnumlist}

`    (init-scheme-proc (list f f))))`
!!!(p) {:.unnumlist}

We also need to define `call/cc`.
Think for a moment about what `call/cc` must do.
Like all Scheme procedures, it takes the current continuation as its first argument.
The second argument is a procedure–a computation to be performed.
`call/cc` performs the computation by calling the procedure.
This is just a normal call, so it uses the current continuation.
The tricky part is what `call/cc` passes the computation as its argument.
It passes an escape procedure, which can be invoked to return to the same point that the original call to `call/cc` would have returned to.
Once the working of `call/cc` is understood, the implementation is obvious:

[ ](#){:#l0355}`(defun call/cc (cc computation)`
!!!(p) {:.unnumlist}

` "Make the continuation accessible to a Scheme procedure."`
!!!(p) {:.unnumlist}

` (funcall computation cc`
!!!(p) {:.unnumlist}

`      ;; Package up CC into a Scheme function:`
!!!(p) {:.unnumlist}

`      #'(lambda (cont val)`
!!!(p) {:.unnumlist}

`        (declare (ignore cont))`
!!!(p) {:.unnumlist}

`        (funcall cc val))))`
!!!(p) {:.unnumlist}

`;; Now install call/cc in the global environment`
!!!(p) {:.unnumlist}

`(set-global-var!
'call/cc #'call/cc)`
!!!(p) {:.unnumlist}

`(set-global-var!
'call-with-current-continuation #'call/cc)`
!!!(p) {:.unnumlist}

## [ ](#){:#st0035}22.6 History and References
{:#s0035}
{:.h1hd}

Lisp interpreters and AI have a long history together.
MIT AI Lab Memo No.
1 ([McCarthy 1958](B9780080571157500285.xhtml#bb0790)) was the first paper on Lisp.
McCarthy’s students were working on a Lisp compiler, had written certain routines–`read`, `print`, etc.–`in` assembly language, and were trying to develop a full Lisp interpreter in assembler.
Sometime around the end of 1958, McCarthy wrote a theoretical paper showing that Lisp was powerful enough to write the universal function, `eval`.
A programmer on the project, Steve Russell, saw the paper, and, according to McCarthy:

> Steve Russell said, look, why don’t I program this `eval` and–you remember the interpreter–and I said to him, ho, ho, you’re confusing theory with practice, this `eval` is intended for reading not for Computing.
But he went ahead and did it.
That is, he compiled the `eval` in my paper into 704 machine code fixing bugs and then advertised this as a Lisp interpreter, which it certainly was.[3](#fn0020){:#xfn0020}

So the first Lisp interpreter was the result of a programmer ignoring his boss’s advice.
The first compiler was for the Lisp 1.5 system ([McCarthy et al.
1962](B9780080571157500285.xhtml#bb0815)).
The compiler was written in Lisp; it was probably the first compiler written in its own language.

Allen’s *Anatomy of lisp* (1978) was one of the first overviews of Lisp implementation techniques, and it remains one of the best.
However, it concentrates on the dynamic-scoping Lisp dialects that were in use at the time.
The more modem view of a lexically scoped Lisp was documented in an influential pair of papers by Guy Steele ([1976a](B9780080571157500285.xhtml#bb1130),[b](B9780080571157500285.xhtml#bb1135)).
His papers “Lambda: the ultimate goto” and “Compiler optimization based on viewing lambda as rename plus goto” describe properly tail-recursive interpreters and compilers.

The Scheme dialect was invented by Gerald Sussman and Guy Steele around 1975 (see their MIT AI Memo 349).
The *Revised*4*Report on the Algorithmic Language Scheme* ([Clinger et al.
1991](B9780080571157500285.xhtml#bb0205)) is the definitive reference manual for the current version of Scheme.

[Abelson and Sussman (1985)](B9780080571157500285.xhtml#bb0010) is probably the best introduction to computer science ever written.
It may or may not be a coincidence that it uses Scheme as the programming language.
It includes a Scheme interpreter.
Winston and Horn’s *Lisp* (1989) also develops a Lisp interpreter.

The `amb` operator for nondeterministic choice was proposed by [John McCarthy (1963)](B9780080571157500285.xhtml#bb0800) and used in SCHEMER !!!(span) {:.smallcaps} ([Zabih et al.
1987](B9780080571157500285.xhtml#bb1440)), a nondeterministic Lisp.
[Ruf and Weise (1990)](B9780080571157500285.xhtml#bb1015) present another implementation of backtracking in Scheme that incorporates all of logic programming.

## [ ](#){:#st0040}22.7 Exercises
{:#s0040}
{:.h1hd}

**Exercise 22.5 [m]** While Scheme does not provide full-blown support for optional and keyword arguments, it does support rest parameters.
Modify the interpreter to support the Scheme syntax for rest parameters:

[ ](#){:#t0020}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| Scheme | Common Lisp |
| (`lambda x`*body*) | (`lambda` (`&rest x`) *body*) |
| (`lambda (x y . z)`*body*) | (`lambda` (`x y &rest z`) *body*) |

**Exercise 22.6 [h]** The representation of environments is somewhat wasteful.
Currently it takes 3*n* cons cells to represent an environment with *n* variables.
Change the representation to take less space.

**Exercise 22.7 [m]** As we’ve implemented macros, they need to be expanded each time they are encountered.
This is not so bad for the compiler–you expand the source code and compile it, and then never refer to the source code again.
But for the interpreter, this treatment of macros is most unsatisfactory: the work of macroexpansion must be done again and again.
How can you eliminate this duplicated effort?

**Exercise 22.8 [m]** It turns out Scheme allows some additional syntax in `let` and `cond`.
First, there is the “named-let” expression, which binds initial values for variables but also defines a local function that can be called within the body of the `let`.
Second, `cond` recognizes the symbol => when it is the second element of a cond clause, and treats it as a directive to pass the value of the test (when it is not false) to the third element of the clause, which must be a function of one argument.
Here are two examples:

[ ](#){:#l0365}`(define (fact n)`
!!!(p) {:.unnumlist}

` ;; Iterative factorial; does not grow the stack`
!!!(p) {:.unnumlist}

` (let loop ((result 1) (i n))`
!!!(p) {:.unnumlist}

`  (if (= i 0) result (loop (* result i) (− i 1)))))`
!!!(p) {:.unnumlist}

[ ](#){:#l0370}`(define (lookup key alist)`
!!!(p) {:.unnumlist}

` ;; Find key’s value in alist`
!!!(p) {:.unnumlist}

` (cond ((assoc key alist) => cdr)`
!!!(p) {:.unnumlist}

`     (else #f)))`
!!!(p) {:.unnumlist}

These are equivalent to:

[ ](#){:#l0375}`(define (fact n)`
!!!(p) {:.unnumlist}

` (letrec`
!!!(p) {:.unnumlist}

`  ((loop (lambda (result i)`
!!!(p) {:.unnumlist}

`        (if (= i 0)`
!!!(p) {:.unnumlist}

`          result`
!!!(p) {:.unnumlist}

`          (loop (* result i) (− i 1))))))`
!!!(p) {:.unnumlist}

`  (loop 1 n)))`
!!!(p) {:.unnumlist}

[ ](#){:#l0380}`(define (lookup key alist)`
!!!(p) {:.unnumlist}

` (let ((g0030 (assoc key alist)))`
!!!(p) {:.unnumlist}

`  (if g0030`
!!!(p) {:.unnumlist}

`    (cdr g0030)`
!!!(p) {:.unnumlist}

`    #f)))`
!!!(p) {:.unnumlist}

Write macro definitions for `let` and `cond` allowing these variations.

**Exercise 22.9 [h]** Some Scheme implementations permit `define` statements inside the body of a `lambda` (and thus of a `define`, `let`, `let*`, or `letrec` as well).
Here is an example:

[ ](#){:#l0385}`(define (length l)`
!!!(p) {:.unnumlist}

` (define (len l n)`
!!!(p) {:.unnumlist}

`  (if (null?
l) n (len (cdr l) (+ n 1))))`
!!!(p) {:.unnumlist}

` (len l 0))`
!!!(p) {:.unnumlist}

The internal definition of len is interpreted not as defining a global name but rather as defining a local name as if with `letrec`.
The above definition is equivalent to:

[ ](#){:#l0390}`(define (length l)`
!!!(p) {:.unnumlist}

` (letrec ((len (lambda (l n)`
!!!(p) {:.unnumlist}

`           (if (null?
l) n (len (cdr l) (+ n 1))))))`
!!!(p) {:.unnumlist}

`  (len l 0)))`
!!!(p) {:.unnumlist}

Make changes to the interpreter to allow this kind of internal definition.

**Exercise 22.10** Scheme programmers are often disdainful of the `function` or `#`' notation in Common Lisp.
Is it possible (without changing the compiler) to make Common Lisp accept `(lambda ( ) … )` instead of `#` ' `(lambda ( ) … )` and `fn` instead of `#`'`fn?`

**Exercise 22.11 [m]** The top level of the continuation-passing version of `scheme` includes the call: `(interp (read)``nil` #'`print)`.
Will this always result in some value being printed?
Or is it possible that the expression read might call some escape function that ignores the value without printing anything?

**Exercise 22.12 [h]** What would have to be added or changed to turn the Scheme interpreter into a Common Lisp interpreter?

**Exercise 22.13 [h]** How would you change the interpreter to allow for multiple values?
Explain how this would be done both for the first version of the interpreter and for the continuation-passing version.

## [ ](#){:#st0045}22.8 Answers
{:#s0045}
{:.h1hd}

**Answer 22.2** There is no way to implement a full `call/cc` to Common Lisp, but the following works for cases where the continuation is only used with dynamic extent:

[ ](#){:#l0395}`(defun call/cc (computation)`
!!!(p) {:.unnumlist}

` "Call computation.
passing it the current continuation.`
!!!(p) {:.unnumlist}

` The continuation has only dynamic extent."`
!!!(p) {:.unnumlist}

` (funcall computation #'(lambda (x) (return-from call/cc x))))`
!!!(p) {:.unnumlist}

**Answer 22.3** No.
`fail` requires continuations with dynamic extent.

**Answer 22.5** We need only modify `extend` - `env` to know about an atomic `vars` list.
While we’re at it, we might as well add some error checking:

[ ](#){:#l0400}`(defun extend-env (vars vals env)`
!!!(p) {:.unnumlist}

` "Add some variables and values to an environment."`
!!!(p) {:.unnumlist}

` (cond ((null vars)`
!!!(p) {:.unnumlist}

`     (assert (null vals) ( ) "Too many arguments supplied")`
!!!(p) {:.unnumlist}

`     env)`
!!!(p) {:.unnumlist}

`     ((atom vars)`
!!!(p) {:.unnumlist}

`      (cons (list vars vals) env))`
!!!(p) {:.unnumlist}

`     (t (assert (rest vals) ( ) "Too few arguments supplied")`
!!!(p) {:.unnumlist}

`       (cons (list (first vars) (first vals))`
!!!(p) {:.unnumlist}

`           (extend-env (rest vars) (rest vals) env)))))`
!!!(p) {:.unnumlist}

**Answer 22.6** Storing the environment as an association list, `((*var val*)…)`, makes it easy to look up variables with `assoc`.
We could save one cons cell per variable just by changing to `((*var* .
*val*)…)`.
But even better is to switch to a different representation, one presented by Steele and Sussman in *The Art of the Interpreter* (1978).
In this representation we switch from a single list of var/val pairs to a list of frames, where each frame is a var-list/val-list pair.
It looks like this:

[ ](#){:#l0405}`(((*var*…) .
(*val*…))`
!!!(p) {:.unnumlist}

` ((*var*…) .
(*val*…))`
!!!(p) {:.unnumlist}

`…)`
!!!(p) {:.unnumlist}

Now `extend-env` is trivial:

[ ](#){:#l0410}`(defun extend-env (vars vals env)`
!!!(p) {:.unnumlist}

` "Add some variables and values to an environment."`
!!!(p) {:.unnumlist}

` (cons (cons vars vals) env))`
!!!(p) {:.unnumlist}

The advantage of this approach is that in most cases we already have a list of variables (the procedure’s parameter list) and values (from the `mapcar` of `interp` over the arguments).
So it is cheaper to just cons these two lists together, rather than arranging them into pairs.
Of course, `get-var` and `set-var`!
become more complex.

**Answer 22.7** One answer is to destructively alter the source code as it is macro-expanded, so that the next time the source code is interpreted, it will already be expanded.
The following code takes care of that:

[ ](#){:#l0415}`(defun scheme-macro-expand (x)`
!!!(p) {:.unnumlist}

` (displace x (apply (scheme-macro (first x)) (rest x))))`
!!!(p) {:.unnumlist}

[ ](#){:#l0420}`(defun displace (old new)`
!!!(p) {:.unnumlist}

` "Destructively change old cons-cell to new value."`
!!!(p) {:.unnumlist}

` (if (consp new)`
!!!(p) {:.unnumlist}

`    (progn (setf (car old) (car new))`
!!!(p) {:.unnumlist}

`           (setf (cdr old) (cdr new))`
!!!(p) {:.unnumlist}

`           old)`
!!!(p) {:.unnumlist}

`    (displace old '(begin ,new))))`
!!!(p) {:.unnumlist}

One drawback to this approach is that the user’s source code is actually changed, which may make debugging confusing.
An alternative is to expand into something that keeps both the original and macro-expanded code around:

[ ](#){:#l0425}`(defun displace (old new)`
!!!(p) {:.unnumlist}

` "Destructively change old to a DISPLACED structure."`
!!!(p) {:.unnumlist}

` (setf (car old) 'DISPLACED)`
!!!(p) {:.unnumlist}

` (setf (cdr old) (list new old))`
!!!(p) {:.unnumlist}

` old)`
!!!(p) {:.unnumlist}

This means that `DISPLACED` is a new special form, and we need a clause for it in the interpreter.
It would look something like this:

[ ](#){:#l0430}`(case (first x)`
!!!(p) {:.unnumlist}

` …`
!!!(p) {:.unnumlist}

` (DISPLACED (interp (second x) env))`
!!!(p) {:.unnumlist}

` …`
!!!(p) {:.unnumlist}

We’d also need to modify the printing routines to print just `old` whenever they see `(displaced old new)`.

**Answer 22.8**

[ ](#){:#l0435}`(def-scheme-macro let (vars &rest body)`
!!!(p) {:.unnumlist}

` (if (symbolp vars)`
!!!(p) {:.unnumlist}

`    ;; named let`
!!!(p) {:.unnumlist}

`    (let ((f vars) (vars (first body)) (body (rest body)))`
!!!(p) {:.unnumlist}

`     '(letrec ((,f (lambda ,(mapcar #'first vars) .,body)))`
!!!(p) {:.unnumlist}

`        (,f .,(mapcar #'second vars))))`
!!!(p) {:.unnumlist}

`    ;; "regular" let`
!!!(p) {:.unnumlist}

`    '((lambda ,(mapcar #'first vars) .
,body)`
!!!(p) {:.unnumlist}

`     .
,(mapcar #'second vars)))))`
!!!(p) {:.unnumlist}

[ ](#){:#l0440}`(def-scheme-macro cond (&rest clauses)`
!!!(p) {:.unnumlist}

` (cond ((null clauses) nil)`
!!!(p) {:.unnumlist}

`     ((length=1 (first clauses))`
!!!(p) {:.unnumlist}

`      '(or ,(first clauses) (cond .,(rest clauses))))`
!!!(p) {:.unnumlist}

`     ((starts-with (first clauses) 'else)`
!!!(p) {:.unnumlist}

`      '(begin .,(rest (first clauses))))`
!!!(p) {:.unnumlist}

`     ((eq (second (first clauses)) '=>)`
!!!(p) {:.unnumlist}

`      (assert (= (length (first clauses)) 3))`
!!!(p) {:.unnumlist}

`      (let ((var (gensym)))`
!!!(p) {:.unnumlist}

`      '(let ((,var ,(first (first clauses))))`
!!!(p) {:.unnumlist}

`        (if ,var (,(third (first clauses)) ,var)`
!!!(p) {:.unnumlist}

`             (cond .,(rest clauses))))))`
!!!(p) {:.unnumlist}

`     (t '(if ,(first (first clauses))`
!!!(p) {:.unnumlist}

`          (begin .,(rest (first clauses)))`
!!!(p) {:.unnumlist}

`          (cond .,(rest clauses)))))))`
!!!(p) {:.unnumlist}

**Answer 22.10** It is easy to define `lambda` as a macro, eliminating the need for `#'(lambda …)`:

[ ](#){:#l0445}`(defmacro lambda (args &rest body)`
!!!(p) {:.unnumlist}

` '(function (lambda .args .@body)))`
!!!(p) {:.unnumlist}

If this were part of the Common Lisp standard, I would gladly use it.
But because it is not, I have avoided it, on the grounds that it can be confusing.

It is also possible to write a new function-defining macro that would do the following type of expansion:

[ ](#){:#l0450}`(defn double (x) (* 2 x)) =>`
!!!(p) {:.unnumlist}

`(defparameter double (defun double (x) (* 2 x)))`
!!!(p) {:.unnumlist}

This makes `double` a special variable, so we can write `double` instead of `#′double`.
But this approach is not recommended–it is dangerous to define special variables that violate the asterisk convention, and the Common Lisp compiler may not be able to optimize special variable references the way it can `function` special forms.
Also, this approach would not interact properly with `flet` and `labels`.

----------------------

[1](#xfn0010){:#np0010} One writes `numberp` because there is no hyphen in `number` but `random-state-p` because there is a hyphen in `random-state`.
However, `defstruct` concatenates `-p` in all its predicates, regardless of the presence of a hyphen in the structure’s name.
!!!(p) {:.ftnote1}

[2](#xfn0015){:#np0015} although inefficient
!!!(p) {:.ftnote1}

[3](#xfn0020){:#np0020} McCarthy’s words from a talk on the history of Lisp, 1974, recorded by [Stoyan (1984)](B9780080571157500285.xhtml#bb1205).
!!!(p) {:.ftnote1}

