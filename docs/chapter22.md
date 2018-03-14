# Chapter 22 {docsify-ignore}
<a id='page-753'></a>

Scheme: An Uncommon Lisp 

The best laid schemes o' mice an' men 

-Robert Burns (1759-1796) 

r I 1 his chapter presents the Scheme dialect of Lisp and an interpreter for it. While it is not 

I likely that you would use this interpreter for any serious programming, understanding 

JL how the interpreter works can give you a better appreciation of how Lisp works, and 
thus make you a better programmer. A Scheme interpreter is used instead of a Common Lisp 
one because Scheme is simpler, and also because Scheme is an important language that is worth 
knowing about. 

Scheme is the only dialect of Lisp besides Common Lisp that is currently flourishing. Where 
Common Lisp tries to standardize all the important features that are in current use by Lisp 
programmers. Scheme tries to give a minimal set of very powerful features that can be used to 
implement the others. It is interesting that among all the programming languages in the world. 
Scheme is one of the smallest, while Common Lisp is one of the largest. The Scheme manual 
is only 45 pages (only 38 if you omit the example, bibliography, and index), while Common Lisp 
the Language, 2d edition, is 1029 pages. Here is a partial list of the ways Scheme is simpler than 
Common Lisp: 

<a id='page-754'></a>

1. Scheme has fewer buUt-in functions and special forms. 
2. Scheme has no special variables, only lexical variables. 
3. Scheme uses the same name space for functions and variables (and everything 
else). 
4. Scheme evaluates the function part of a function call in exactly the same way 
as the arguments. 
5. Scheme functions can not have optional and keyword parameters. However, 
they can have the equivalent of a &rest parameter. 
6. Scheme has no block, return, go, or throw; a single function (cal 1 /cc) replaces 
all of these (and does much more). 
7. Scheme has no packages. Lexical variables can be used to implement packagelike 
structures. 
8. Scheme, as a standard, has no macros, although most implementations provide 
macros as an extension. 
9. Scheme has no special forms for looping; instead it asks the user to use recursion 
and promises to implement the recursion efficiently. 
The five main special forms in Scheme are quote and if, which are just as in 
Common Lisp; begin and setl, which are just different spellings for progn and 
setq; and 1 ambda, which is as in Common Lisp, except that it doesn't require a 
#' before it. In addition. Scheme allows variables, constants (numbers, strings, and 
characters), and function calls. The function call is different because the function 
itself is evaluated in the same way as the arguments. In Common Lisp, (fx) means 
to look up the function binding of f and apply that to the value of x. In Scheme, (fx) 
means to evaluate f (in this case by looking up the value of the variable f), evaluate 
X (by looking up the value of the variable in exactly the same way) and then apply 
the function to the argument. Any expression can be in the function position, and 
it is evaluated just like the arguments. Another difference is that Scheme uses #t 
and #f for true and false, instead of t and nil. The empty list is denoted by (), and 
it is distinct from the false value, #f. There are also minor lexical differences in the 
conventions for complex numbers and numbers in different bases, but these can be 
ignored for all the programs in this book. Also, in Scheme a single macro, def i ne, 
serves to define both variables and functions. 

<a id='page-755'></a>

Scheme Common Lisp 

var var 

constant constant 

(quotes) or *x (quotes) or 'x 

(beginx..) (progn X..) 

(set! varx) (setq varx) 

(ifpab) (ifpab) 

(lambda parms x...) #'( 1 ambda parmsx...) 

{fn arg.) (fnarg...) or (funcall fnarg...) 

#t t 

#f nil 

() nil 

(define varexp) (defparameter varexp) 

(define ifnparm...) body) (defun fniparm...) body) 

&#9635; Exercise 22.1 [s] What does the following expression evaluate to in Scheme? How 
many errors does it have as a Common Lisp expression? 

((if (= (+ 2 2) 4) 
(lambda (x y) (+ (* . y) 12)) 
cons) 

5 

6) 

A great many functions, such as car, cdr, cons, append, +, *, and list are 
the same (or nearly the same) in both dialects. However, Scheme has some spelling 
conventions that are different from Common Lisp. Most Scheme mutators, like 
set!, end in'!'. Common Lisp has no consistent convention for this; some mutators 
start with . (nreverse, nsubst, nintersection) while others have idiosyncratic 
names (del ete versus remove). Scheme would use consistent names - reverse! and 
remove! - if these functions were defined at all (they are not defined in the standard). 
Most Scheme predicates end in '?', not 'p'. This makes predicates more obvious 
and eliminates the complicated conventions for adding a hyphen before the p.^ The 
only problem with this convention is in spoken language: is equal ? pronounced 
"equal-question-mark" or "equal-q" or perhaps equal, with rising intonation? This 
would make Scheme a tone language, like Chinese. 

^One writes numberp because there is no hyphen in number but random-state-. because 
there is a hyphen in random-state. However, defstruct concatenates -p in all its predicates, 
regardless of the presence of a hyphen in the structure's name. 

<a id='page-756'></a>

In Scheme, it is an error to apply car or cdr to the empty list. Despite the fact that 
Scheme has cons, it calls the result a pai r rather than a cons cell, so the predicate is 
pair?, not consp. 

Scheme recognizes not all lambda expressions will be "functions" according to 
the mathematical definition of function, and so it uses the term "procedure" instead. 
Here is a partial list of correspondences between the two dialects: 

Scheme Procedure Common Lisp Ftmction 

char-ready? listen 
char? characterp 
eq? eq 
equal? equal 
eqv? eql 
even? evenp 
for-each mapc 
integer? integerp 
list->string coerce 
list->vector coerce 
list-ref nth 
list-tail nthcdr 
map mapcar 
negative? minusp 
pair? consp 
procedure? functionp 
set! setq 
set-car! replaca 
vector-set! setf 
string-set! setf 

22.1 A Scheme Interpreter 
As we have seen, an interpreter takes a program (or expression) as input and returns 
the value computed by that program. The Lisp function eval is thus an interpreter, 
and that is essentially the function we are trying to write in this section. We have 
to be careful, however, in that it is possible to confuse the notions of interpreter and 
compiler. A compiler takes a program as input and produces as output a translation 
of that program into some other language - usually a language that can be directly 
(or more easily) executed on some machine. So it is also possible to write eval by 
compiling the argument and then interpreting the resulting machine-level program. 
Most modern Lisp systems support both possibilities, although some only interpret 

<a id='page-757'></a>
code directly, and others compile all code before executing it. To make the distinction 
clear, we will not write a function called eval. Instead, we will write versions of two 
functions: interp, a Scheme interpreter, and, in the next chapter, comp, a Scheme 
compiler. 

An interpreter that handles the Scheme primitives is easy to write. In the interpreter 
interp, the main conditional has eight cases, corresponding to the five 
special forms, symbols, other atoms, and procedure applications (otherwise known 
as function calls). For the moment we will stick with t and .i 1 instead of #t and 
#f. After developing a simple interpreter, we will add support for macros, then 
develop a tail-recursive interpreter, and finally a continuation-passing interpreter. 
(These terms will be defined when the time comes.). The glossary for i nterp is in 
figure 22.1. 

scheme 
interp 
def-scheme-macro 

*scheme-procs* 

set-var! 
get-var 
set-global-var! 

get-global-var 
extend-env 
init-scheme-interp 
init-scheme-proc 
scheme-macro 
scheme-macro-expand 
maybe-add 
print-proc 

proc 

interp-begin 
interp-call 
map-interp 
call/cc 

lastl 
length=1 

Top-Level Fimctions 

A Scheme read-interp-print loop. 
Interpret (evaluate) an expression in an environment. 
Define a Scheme macro. 

Special Variables 

Some procedures to store in the global environment. 

Auxiliary Functions 

Set a variable to a value. 
Get the value of a variable in an environment. 
Set a global variable to a value. 

Get the value of a variable fron the global environment. 
Add some variables and values to an environment. 
Initialize some global variables. 
Define a primitive Scheme procedure. 
Retrieve the Scheme macro for a symbol. 
Macro-expand a Scheme expression. 
Add an element to the front of a non-singleton list. 
Print a procedure. 

Data Type (tail-recursive version only) 

A Scheme procedure. 

Functions (continuation version only) 

Interpret a begi . expression. 
Interpret a function application. 
Map i nterp over a list. 
call with current continuation. 

Previously Defined Functions 

Select the last element of a list. 
Is this a list of length1? 

Figure 22.1: Glossary for the Scheme Interpreter 

<a id='page-758'></a>

The simple interpreter has eight cases to worry about: (1) If the expression is a 
symbol, look up its value in the environment. (2) If it is an atom that is not a symbol 
(such as a number), just return it. Otherwise, the expression must be a list. (3) If it 
starts with quote, return the quoted expression. (4) If it starts with beg i ., interpret 
each subexpression, and return the last one. (5) If it starts with set 1, interpret the 
value and then set the variable to that value. (6) If it starts with i f, then interpret 
the conditional, and depending on if it is true or not, interpret the then-part or the 
else-part. (7) If it starts with 1 ambda, build a new procedure - a closure over the ctu*rent 
environment. (8) Otherwise, it must be a procedure application. Interpret the 
procedure and all the arguments, and apply the procedure value to the argument 
values. 

(defun interp (x &optiona1 env) 

"Interpret (evaluate) the expression . in the environment env." 

(cond 

((symbolp x) (get-var . env)) 

((atom x) x) 

((case (first x) 

(QUOTE (second x)) 
(BEGIN (lastl (mapcar #*(lambda (y) (interp y env)) 
(rest x)))) 
(SET! (set-var! (second x) (interp (third x) env) env)) 
(IF (if (interp (second x) env) 
(interp (third x) env) 
(interp (fourth x) env))) 

(LAMBDA (let ((parms (second x)) 
(code (maybe-add 'begin (rest2 x)))) 
#*(lambda (&rest args) 
(interp code (extend-env parms args env))))) 
(t ;; a procedure application 
(apply (interp (first x) env) 
(mapcar #'(lambda (v) (interp . env)) 
(rest x)))))))) 

An environment is represented as an association list of variable/value pairs, except 
for the global environment, which is represented by values on the gl obal - val 
property of symbols. It would be simpler to represent the global environment 
in the same way as local environments, but it is more efficient to use property 
lists than one big global a-list. Furthermore, the global environment is distinct 
in that every symbol is implicitly defined in the global environment, while local 
environments only contain variables that are explicitly mentioned (in a 1 ambda expression). 


<a id='page-759'></a>
As an example, suppose we interpret the function call (f 1 2 3), and that the 
functions f has been defined by the Scheme expression: 

(set! f (lambda (a b c) (+ a (g b c)))) 

Then we will interpret (f 1 2 3) by interpreting the body of f with the environment: 

((a 1) (b 2) (c 3)) 

Scheme procedures are implemented as Common Lisp functions, and in fact all the 
Scheme data types are implemented by the corresponding Common Lisp types. I 
include the function i .i t -s eherne - i . te rp to initialize a few global values and repeat 
the definitions of last1 and length=1: 

(defun set-var! (var val env) 
"Set a variable to a value, in the given or global environment." 
(if (assoc var env) 

(setf (second (assoc var env)) val) 
(set-global-var! var val)) 
val) 

(defun get-var (var env) 
"Get the value of a variable, from the given or global environment, 

(if (assoc var env) 
(second (assoc var env)) 
(get-global-var var))) 

(defun set-global-var! (var val) 
(setf (get var 'global-val) val)) 

(defun get-global-var (var) 
(let* ((default "unbound") 
(val (get var 'global-val default))) 

(if (eq val default) 
(error "Unbound scheme variable: '"a" var) 
val))) 

(defun extend-env (vars vals env) 
"Add some variables and values to an environment." 
(nconc (mapcar #'list vars vals) env)) 

(defparameter *scheme-procs* 

.(+-'/=<><=>= cons car cdr not append list read member 
(null? null) (eq? eq) (equal? equal) (eqv? eql) 
(write prinl) (display princ) (newline terpri))) 

<a id='page-760'></a>

(defun init-scheme-interp () 
"Initialize the scheme interpreter with some global variables." 
Define Scheme procedures as CL functions: 
(mapc #*init-scheme-proc *scheme-procs*) 

Define the Boolean 'constants*. Unfortunately, this won't 
;; stop someone from saying: (setl t nil) 
(set-global-var! t t) 
(set-global-vari nil nil)) 

(defun init-scheme-proc (f) 
"Define a Scheme procedure as a corresponding CL function." 
(if (listp f) 

(set-global-var! (first f) (symbol-function (second f))) 
(set-global-var! f (symbol-function f)))) 

(defun maybe-add (op exps &optional if-nil) 
"For example, (maybe-add 'and exps t) returns 
t if exps is nil, exps if there is only one, 
and (and expl exp2...) if there are several exps." 
(cond ((null exps) if-nil) 

((length=1 exps) (first exps)) 
(t (cons op exps)))) 

(defun length=1 (x) 
"Is X a list of length 1?" 
(and (consp x) (null (cdr x)))) 

(defun lastl (list) 
"Return the last element (not last cons cell) of list" 
(first (last list))) 

To test the interpreter, we add a simple read-eval-print loop: 

(defun scheme () 
"A Scheme read-eval-print loop (using interp)" 
(init-scheme-interp) 
(loop (format t ""&==> ") 

(print (interp (read) nil)))) 

And now we're ready to try out the interpreter. Note the Common Lisp prompt is 
">," while the Scheme prompt is "==>." 

> (scheme) 
==> (+ 2 2) 
4 

==> ((if (= 1 2) * +) 3 4) 
7 

<a id='page-761'></a>

= => ((if (= 1 1) * +) 3 4) 
12 

==> (setl fact (lambda (n) 
(if (= . 0) 1 
(* . (fact (- . 1)))))) 
#<DTP-LEXICAL-CLOSURE 36722615> 

==> (fact 5) 
120 

==> (setl table (lambda (f start end) 
(if (<= start end) 

(begin 
(write (list start (f start))) 
(newline) 
(table f (+ start 1) end))))) 

#<DTP-LEXICAL-CLOSURE 41072172> 

==> (table fact 1 10) 
(1 1) 
(2 2) 
(3 6) 
(4 24) 
(5 120) 
(6 720) 
(7 5040) 
(8 40320) 
(9 362880) 

(10 3628800) 

NIL 

==> (table (lambda (x) (* . . .)) 5 10) 
(5 125) 

(6 216) 
(7 343) 
(8 512) 
(9 729) 

(10 1000) 

NIL 

= => [ABORT] 

<a id='page-762'></a>

22.2 Syntactic Extension with Macros 
Scheme has a number of other special forms that were not listed above. Actually, 
Scheme uses the term "syntax" where we have been using "special form." The remaining 
syntax can be defined as "derived expressions" in terms of the five primitives. 
The Scheme standard does not recognize a concept of macros, but it is clear that a 
"derived expression" is like a macro, and we will implement them using macros. The 
following forms are used (nearly) identically in Scheme and Common Lisp: 

let let* and or do cond case 

One difference is that Scheme is less lenient as to what counts as a binding in 1 et, 
1 et* and do. Every binding must be (var init); just (var) or var is not allowed. In do, 
a binding can be either (var init step) or (var init). Notice there is no do*. The other 
difference is in ca se and cond. Where Common Lisp uses the symbol t or otherwi se 
to mark the final case. Scheme uses el se. The final three syntactic extensions are 
unique to Scheme: 

(define var val) or (define (proc-name arg...) body...) 

(delay expression) 

(letrec {{varinit)...) body...) 

define is a combination of defun and defparameter. In its first form, it assigns a 
value to a variable. Since there are no special variables in Scheme, this is no different 
than using set!. (There is a difference when the def i ne is nested inside another 
definition, but that is not yet considered.) In the second form, it defines a function, 
del ay is used to delay evaluation, as described in section 9.3, [page 281](chapter9.md#page-281). 1 etrec is 
similar to 1 et. The difference is that all the init forms are evaluated in an environment 
that includes all the pars. Thus, 1 etrec can be used to define local recursive functions, 
just as 1 abel s does in Common Lisp. 

The first step in implementing these syntactic extensions is to change i nterp to 
allow macros. Only one clause has to be added, but we'll repeat the whole definition: 

(defun interp (x &optional env) 

"Interpret (evaluate) the expression . in the environment env. 

This version handles macros." 

(cond 

((symbolp x) (get-var . env)) 

((atom x) x) 

((scheme-macro (first x)) ;*** 

(interp (scheme-macro-expand x) env)) ;*** 

((case (first x) 

(QUOTE (second x)) 

<a id='page-763'></a>
(BEGIN (last l (mapcar #'(lambda (y) (interp y env)) 
(rest X))) ) 
(SET! (set-var ! (second x) (interp (third x) env) env)) 
(IF (if (interp (second x) env) 
(interp (third x) env) 
(interp (fourth x) env))) 

(LAMBDA (let ((parms (second x)) 
(code (maybe-add 'begin (rest2 x)))) 
#'(lambda (&rest args) 
(interp code (extend-env parms args env))))) 
(t ;; a procedure application 
(apply (interp (first x) env) 
(mapcar #*(lambda (v) (interp . env)) 
(rest X)))))))) 

Now we provide a mechanism for defining macros. The macro definitions can be in 
any convenient language; the easiest choices are Scheme itself or Common Lisp. I 
have chosen the latter. This makes it clear that macros are not part of Scheme itself but 
rather are used to implement Scheme. If we wanted to offer the macro facility to the 
Scheme programmer, we would make the other choice. (But then we would be sure to 
add the backquote notation, which is so useful in writing macros.) def -s cheme - mac ro 
(which happens to be a macro itself) provides a way of adding new Scheme macros. 
It does that by storing a Common Lisp function on the scheme-macro property of 
a symbol. This^furiction, when given a list of ai-gumehts, returns the code that the 
macro call should expand into. The function scheme-macro tests if a symbol has a 
macro attached to it, and scheme-macro-expand does the actual macro-expansion: 

(defun scheme-macro (symbol) 
(and (symbolp symbol) (get symbol 'scheme-macro))) 

(defmacro def-scheme-macro (name parmlist &body body) 
"Define a Scheme macro." 
'(setf (get '.name 'scheme-macro) 

#'(lambda .parmlist ..body))) 

(defun scheme-macro-expand (x) 
"Macro-expand this Scheme expression." 
(if (and distp x) (scheme-macro (first x))) 

(scheme-macro-expand 
(apply (scheme-macro (first x)) (rest x))) 

X)) 

<a id='page-764'></a>

Here are the definitions of nine important macros in Scheme: 

(def-scheme-macro let (bindings &rest body) 
'((lambda ,(mapcar #'first bindings) . .body) 
..(mapcar #*second bindings))) 

(def-scheme-macro let* (bindings &rest body) 

(if (null bindings) 
'(begin ..body) 
'(let (.(first bindings)) 

(let* .(rest bindings) . .body)))) 

(def-scheme-macro and (&rest args) 

(cond ((null args) *T) 
((length=1 args) (first args)) 
(t '(if .(first args) 

(and . .(rest args)))))) 

(def-scheme-macro or (&rest args) 

(cond ((null args) 'nil) 
((length=1 args) (first args)) 
(t (let ((var (gensym))) 

'(let ((.var .(first args))) 
(if ,var .var (or . .(rest args)))))))) 
(def-scheme-macro cond (&rest clauses) 
(cond ((null clauses) nil) 
((length=1 (first clauses)) 
'(or .(first clauses) (cond ..(rest clauses)))) 
((starts-with (first clauses) 'else) 
'(begin ..(rest (first clauses)))) 

(t '(if .(first (first clauses)) 
(begin ..(rest (first clauses))) 
(cond ..(rest clauses)))))) 

(def-scheme-macro case (key &rest clauses) 
(let ((key-val (gensym "KEY"))) 
'(let ((.key-val .key)) 
(cond .(mapcar 
#*(lambda (clause) 

(if (starts-with clause 'else) 
clause 
'((member ,key-val '.(first clause)) 

..(rest clause)))) 
clauses))))) 

(def-scheme-macro define (name &rest body) 
(if (atom name) 
'(begin (setl .name . .body) '.name) 

'(define .(first name) 
(lambda .(rest name) . .body)))) 
<a id='page-765'></a>
(def-scheme-macro delay (computation) 
'(lambda () .computation)) 

(def-scheme-macro letrec (bindings &rest body) 

'(let .(mapcar #'(lambda (v) (list (first v) nil)) bindings) 
.(mapcar #*(lambda (v) '(set! ..v)) bindings) 
..body)) 

We can test out the macro faciUty: 

> (scheme-macro-expand '(and . q)) (IF . (AND Q)) 

> (scheme-macro-expand '(and q)) => Q 

> (scheme-macro-expand '(let ((x 1) (y 2)) (+ . y))) 
((LAMBDA (X Y) (+ . Y)) 1 2) 

> (scheme-macro-expand 
'(letrec 
((even? (lambda (.) (or (= . 0) (odd? (-. 1))))) 
(odd? (lambda (.) (even? (-. 1))))) 
(even? .))) 
(LET ((EVEN? NIL) 

(ODD? NIL)) 
(SET! EVEN? (LAMBDA (X) (OR (= X 0) (ODD? (-X 1))))) 
(SET! ODD? (LAMBDA (X) (EVEN? (- X 1)))) 
(EVEN? Z)) 

> (scheme) 
==> (define (reverse 1) 
(if (null? 1) nil 
(append (reverse (cdr 1)) (list (car 1))))) 
REVERSE 

==> (reverse '(a b c d)) 
(D C . A) 

==> (let* ((X 5) (y (+ x x))) 

(if (or (= x 0) (and (< O y) (< y 20))) 
(list X y) 
(+ y .))) 

(5 10) 

The macro def i ne is just like set !, except that it returns the symbol rather than the 
value assigned to the symbol. In addition, def i ne provides an optional syntax for 
defining functions - it serves the purposes of both defun and defvar. The syntax 

(define {fn. args). >7ody) is an abbreviation for (define (lambda args. body)). 

<a id='page-766'></a>

In addition. Scheme provides a notation where def i ne can be used inside a function 
definition in a way that makes it work like 1 et rather than set!. 

The advantage of the macro-based approach to special forms is that we don't have 
to change the interpreter to add new special forms. The interpreter remains simple, 
even while the language grows. This also holds for the compiler, as we see in the next 
section. 

22.3 A Properly Tail-Recursive Interpreter 
Unfortunately, the interpreter presented above can not lay claim to the name Scheme, 
because a true Scheme must be properly tail-recursive. Our interpreter is tail-
recursive only when run in a Common Lisp that is tail-recursive. To see the problem, 
consider the following Scheme procedure: 

(define (traverse lyst) 
(if lyst (traverse (cdr lyst)))) 

Trace the function interp and execute (interp '(traverse '(a b c d))). The 
nested calls to i nterp go 16 levels deep. In general, the level of nesting is 4 plus 3 
times the length of the hst. Each call to interp requires Common Lisp to allocate 
some storage on the stack, so for very long lists, we will eventually run out of storage. 
To earn the name Scheme, a language must guarantee that such a program does not 
run out of storage. 

The problem, in this example, lies in two places. Everytime we interpret an i f 
form or a procedure call, we descend another recursive level into i nterp. But that 
extra level is not necessary. Consider the i f form. It is certainly necessary to call 
i nterp recursively to decide if the test is true or not. For the sake of argument, let's 
say the test is true. Thenwecall i nterp again on the i/zen part This recursive call will 
return a value, which will then be immediately returned as the value of the original 
call as well. 

The alternative is to replace the recursive call to interp with a renaming of 
variables, followed by a goto statement. That is, instead of calling interp and thereby 
binding a new instance of the variable . to the then part, we just assign the then part 
to X, and branch to the top of the i nterp routine. This works because we know we 
have no more use for the old value of x. A similar technique is used to eliminate the 
recursive call for the last expression in a beg i . form. (Many programmers have been 
taught the "structured programming" party line that goto statements are harmful. In 
this case, the goto is necessary to implement a low-level feature efficiently.) 

<a id='page-767'></a>
The final thing we need to do is explicitly manage Scheme procedures. Instead 
of implementing Scheme procedures as Common Lisp closures, we will define a 
structure, . roc, to contain the code, environment, parameter list, and optionally the 
name of the procedure. Then when we are evaluating a procedure call, we can assign 
the body of the procedure to x rather than recursively calling i nterp. 

(defstruct (proc (rprint-function print-proc)) 
"Represent a Scheme procedure" 
code (env nil) (name nil) (parms nil)) 

The following is a properly tail-recursive interpreter. The macro prog sets up a 
tagbody within which we can use go statements to branch to labels, and it also sets 
up a bl ock from which we can return a value. It can also bind variables like 1 et, 
although in this usage, the variable list is empty. Any symbol within the body of a 
prog is considered a label. In this case, the label : INTERP is the target of the branch 
statements (GO :I NTERP). I use uppercase to indicate that go-to statements are being 
used, but this convention has not been widely adopted. 

(defun interp (x &optional env) 
"Evaluate the expression . in the environment env. 
This version is properly tail-recursive." 
(prog () 

:INTERP 
(return 

(cond 
((symbolp x) (get-var . env)) 
((atom x) x) 
((scheme-macro (first x)) 

(setf X (scheme-macro-expand x)) (go :INTERP)) 
((case (first x) 

(QUOTE (second x)) 
(BEGIN (pop x) ; pop off the BEGIN to get at the args 
Now interpret all but the last expression 
(loop while (rest x) do (interp (pop x) env)) 
Finally, rename the last expression as . 
(setf X (firs t X)) 
(GO :INTERP)) 
(SETI (set-varl (second x) (interp (third x) env) env)) 
(IF (setf X (if (interp (second x) env) 
(third X) 
(fourth X))) 
That is . rename the right expression as . 
(GO :INTERP)) 

(LAMBDA (make-proc :env env :parms (second x) 
:code (maybe-add 'begin (rest2 x)))) 

<a id='page-768'></a>

(t a procedure application 
(let ((proc (interp (first x) env)) 
(args (mapcar #*(lambda (v) (interp . env)) 
(rest X)))) 
(if (proc-p proc) 
Execute procedure with rename+goto 

(progn 
(setf X (proc-code proc)) 
(setf env (extend-env (proc-parms proc) args 

(proc-env proc))) 
(GO :INTERP)) 
else apply primitive procedure 
(apply proc args)))))))))) 

(defun print-proc (proc &optional (stream *standard-output*) depth) 
(declare (ignore depth)) 
(format stream "{~a}" (or (proc-name proc) '??))) 

By tracing the tail-recursive version of interp, you can see that calls to traverse 
descend only three recursive levels of interp, regardless of the length of the list 
traversed. 

Note that we are not claiming that this interpreter allocates no storage when 
it makes tail-recursive calls. Indeed, it wastes quite a bit of storage in evaluating 
arguments and building environments. The claim is that since the storage is allocated 
on the heap rather than on the stack, it can be reclaimed by the garbage collector. So 
even if traverse is applied to an infinitely long list (i.e., a circular list), the interpreter 
will never run out of space - it will always be able to garbage-collect and continue. 

There are many improvements that could be made to this interpreter, but effort 
is better spent in improving a compiler rather than an interpreter. The next chapter 
does just that. 

22.4 Throw, Catch, and Call/cc 
Tail-recursion is crucial to Scheme. The idea is that when the language is guaranteed 
to optimize tail-recursive calls, then there is no need for special forms to do iteration. 
All loops can be written using recursion, without any worry of overflowing the nm-
time stack. This helps keep the language simple and rules out the goto statement, the 
scourge of the structured programming movement. However, there are cases where 
some kind of nonlocal exit is the best alternative. Suppose that some unexpected 
event happens deep inside your program. The best action is to print an error message 
and pop back up to the top level of your program. This could be done trivially with a 
goto-like statement. Without it, every function along the calling path would have to 

<a id='page-769'></a>
be altered to accept either a valid result or an indication of the exceptional condition, 
which just gets passed up to the next level. 

In Common Lisp, the functions throw and catch are provided for this kind of 
nonlocal exit. Scott Zimmerman, the perennial world Frisbee champion, is also 
a programmer for a Southern California firm. He once told me, "I'm starting to 
learn Lisp, and it must be a good language because it's got throw and catch in it." 
Unfortunately for Scott, throw and catch don't refer to Frisbees but to transfer of 
control. They are both special forms, with the following syntax: 

(catch tag body...) 

(throw tag value) 

The first argument to catch is a tag, or label. The remaining arguments are evaluated 
one at a time, and the last one is returned. Thus, catch is much like progn. The 
difference is that if any code in the dynamic extent of the body of the catch evaluates 
the special form throw, then control is immediately passed to the enclosing catch 
with the same tag. 

For example, the form 

(catch 'tag 
(print 1) (throw 'tag 2) (print 3)) 

prints 1 and returns 2, without going on to print 3. A more representative example 
is: 

(defun print-table (1) 
(catch 'not-a-number (mapcar #*print-sqrt-abs 1))) 

(defun print-sqrt-abs (x) 
(print (sqrt (abs (must-be-number x))))) 

(defun must-be-number (x) 
(if (numberp x) . 
(throw 'not-a-number "huh?"))) 

> (print-table '(1 4 -9 . 10 20)) 

1 

2 

3 
"huh?" 

Here pri nt-tablecalls print-sqrt-abs, which callsmust-be-number. Thefirstthree 
times all is fine and the values 1,2,3 get printed. The next time . is not a number, so 
the value "huh?" gets thrown to the tag not-a-number established by catch in f. The 

<a id='page-770'></a>

throw bypasses the pending calls to abs, sqrt, and print, as well as the rest of the 
call to mapcar. 

This kind of control is provided in Scheme with a very general and powerful 
procedure, cal 1 -with-current-continuati on, which is often abbreviated cal 1 /cc. 
cal 1 /cc is a normal procedure (not a special form like throw and catch) that takes 
a single argument. Let's call the argument computation, computation must be a 
procedure of one argument. When cal 1 /cc is invoked, it calls computation, and 
whatever computat i on returns is the value of the call to call /cc. The trick is that the 
procedure computati on also takes an argument (which we'll call cc) that is another 
procedure representing the current continuation point. If cc is applied to some value, 
that value is returned as the value of the call to call / cc. Here are some examples: 

> (scheme) 
=> (+ 1 (call/cc (lambda (cc) (+ 20 300)))) 
321 

This example ignores cc and just computes (+ 1 (+ 20 300)). More precisely, it is 
equivalent to: 

((lambda (val) (+ 1 val)) 
(+ 20 300)) 

The next example does make use of cc: 

=> (+ 1 (call/cc (lambda (cc) (+ 20 (cc 300))))) 
301 

This passes 300 to cc, thus bypassing the addition of 20. It effectively throws 300 out 
of the computation to the catch point estabUshed by cal 1 / cc. It is equivalent to: 

((lambda (val) (+ 1 val)) 
300) 

or to: 

((lambda (val) (+ 1 val)) 
(catch 'cc 
((lambda (v) (+ 20 v)) 
(throw 'cc 300)))) 

<a id='page-771'></a>
Here's how the throw/catch mechanism would look in Scheme: 

(define (print-table 1) 
(call/cc 

(lambda (escape) 
(set! not-a-number escape) 
(map print-sqrt-abs 1)))) 

(define (print-sqrt-abs x) 
(write (sqrt (abs (must-be-number x))))) 

(define (must-be-number x) 
(if (numberp x) . 
(not-a-number "huh?"))) 

(define (map fn 1) 
(if (null? 1) 
.() 

(cons (fn (first D) 
(map fn (rest 1))))) 

The ability to return to a pending point in the computation is useful for this kind of 
error and interrupt handling. However, the truly amazing, wonderful thing about 
cal 1 /cc is the ability to return to a continuation point more than once. Consider a 
slight variation: 

=> (+ 1 (call/cc (lambda (cc) 
(set! old-cc cc) 
(+ 20 (cc 300))))) 

301 

=> (old-cc 500) 
501 

Here, we first computed 301, just as before, but along the way saved cc in the global 
variable old-cc. Afterward, calling (old-cc 500) returns (for the second time) to the 
point in the computation where 1 is added, this time returning 501. The equivalent 
Common Lisp code leads to an error: 

> (+ 1 (catch 'tag (+ 20 (throw 'tag 300)))) 
301 

> (throw 'tag 500) 

Error: there was no pending CATCH for the tag TAG 

In other words, cal 1 /cc's continuations have indefinite extent, while throw/catch 
tags only have dynamic extent. 

<a id='page-772'></a>

We can use cal 1 /cc to implement automatic backtracking (among other things). 
Suppose we had a special form, amb, the "ambiguous" operator, which returns one of 
its arguments, chosen at random. We could write: 

(define (integer) (amb 1 (+ 1 (integer)))) 

and a call to integer would return some random positive integer. In addition, 
suppose we had a function, fail, which doesn't return at all but instead causes 
execution to continue at a prior amb point, with the other choice taken. Then we could 
write succinct^ backtracking code like the following: 

(define (prime) 
(let ((n (integer))) 
(if (prime? n) . (fail)))) 

If pri me? is a predicate that returns true only when its argument is a prime number, 
then prime will always return some prime number, decided by generating random 
integers. While this looks like a major change to the language - adding backtracking 
and nondeterminism - it turns out that amb and fa i 1 can be implemented quite easily 
with cal 1 /cc. First, we need to make amb be a macro: 

(def-scheme-macro amb (x y) 
'(random-choice (lambda () ,x) (lambda () .y)))) 

The rest is pure Scheme. We maintain a Ust of backtrack-points, which are implemented 
as functions of no arguments. To backtrack, we just call one of these 
functions. Thatis what fail does. The function choose-first takes two functions 
and pushes the second, along with the proper continuation, on backtrack-points, 
and then calls the first, returning that value. The function random-choi ce is what 
amb expands into: it decides which choice is first, and which is second. (Note that 
the convention in Scheme is to write global variables like backt rack- poi nts without 
asterisks.) 

(define backtrack-points nil) 

(define (fail) 

(let ((last-choice (car backtrack-points))) 

(setl backtrack-points (cdr backtrack-points)) 

(last-choice))) 

although inefficient 

<a id='page-773'></a>
(define (random-choice f g) 

(if (= 1 (random 2)) 
(choose-first f g) 
(choose-first g f))) 

(define (choose-first f g) 
(call/cc 
(lambda (k) 
(set! backtrack-points 
(cons (lambda () (k (g))) backtrack-points)) 
(f)))) 

This implements chronological backtracking, as in Prolog. However, we actually 
have the freedom to do other kinds of backtracking as well. Instead of having f ai1 
take the first element of backtrack-points, we could choose a random element 
instead. Or, we could do some more complex analysis to choose a good backtrack 
point. 

cal 1 / cc can be used to implement a variety of control structures. As another 
example, many Lisp implementations provide a re s et function that aborts the current 
computation and returns control to the top-level read-eval-print loop, reset can be 
defined quite easily using cal 1 /cc. The trick is to capture a continuation that is at 
the top level and save it away for future use. The following expression, evaluated at 
the top level, saves the appropriate continuation in the value of reset: 

(call/cc (lambda (cc) (set! reset (lambda () 
(cc "Back to top level"))))) 

&#9635; Exercise 22.2 [m] Can you implement cal 1 /cc in Common Lisp? 

&#9635; Exercise 22.3 [s] Can you implement amb and fai1 in Common Lisp? 

&#9635; Exercise 22.4 [m] f ai 1 could be written 
(define (fail) ((pop backtrack-points))) if we had the pop macro in Scheme. 
Write pop. 

22.5 An Interpreter Supporting Call/cc 
It is interesting that the more a host language has to offer, the easier it is to write 
an interpreter. Perhaps the hardest part of writing a Lisp interpreter (or compiler) 
is garbage collection. By writing our interpreter in Lisp, we bypassed the problem 

<a id='page-774'></a>

all together - the host language automatically collects garbage. Similarly, if we are 
using a Common Lisp that is properly tail-recursive, then our interpreter will be too, 
without taking any special steps. If not, the interpreter must be rewritten to take care 
of tail-recursion, as we have seen above. 

It is the same with cal 1 /cc. If our host language provides continuations with 
indefinite extent, then it is trivial to implement cal 1 /cc. If not, we have to rewrite 
the whole interpreter, so that it explicitly handles continuations. The best way to do 
this is to make i . te rp a function of three arguments: an expression, an environment, 
and a continuation. That means the top level will have to change too. Rather than 
having i nterp return a value that gets printed, we just pass it the function pri nt as 
a continuation: 

(defun scheme () 
"A Scheme read-eval-print loop (using interp). 
Handles call/cc by explicitly passing continuations." 
(init-scheme-interp) 
(loop (format t ""&==> ") 

(interp (read) nil #'print))) 

Nowweareready to tackle i nterp. For clarity, we will base it on the non-tail-recursive 
version. The cases for symbols, atoms, macros, and quote are almost the same as 
before. The difference is that the result of each computation gets passed to the 
continuation, cc, rather than just being returned. 

The other cases are all more complex, because they all require explicit representation 
of continuations. That means that calls to i nterp cannot be nested. Instead, 
we call i nterp with a continuation that includes another call to i nterp. For example, 
to interpret (If . . y), we first call interp on the second element of the form, 
the predicate p. The continuation for this call is a function that tests the value of 
. and interprets either . or y accordingly, using the original continuation for the 
recursive call to i nterp. The other cases are similar. One important change is that 
Scheme procedures are implemented as Lisp functions where the first argument is 
the continuation: 

(defun interp (x env cc) 
"Evaluate the expression . in the environment env. 
and pass the result to the continuation cc." 
(cond 

((symbolp x) (funcall cc (get-var . env))) 
((atom x) (funcall cc x)) 
((scheme-macro (first x)) 

(interp (scheme-macro-expand x) env cc)) 

((case (first x) 
(QUOTE (funcall cc (second x))) 
(BEGIN (interp-begin (rest x) env cc)) 

<a id='page-775'></a>
(SET! (interp (third x) env 
#*(lambda (val) 
(funcall cc (set-var! (second x) 
val env))))) 
(IF (interp (second x) env 
#.(lambda (pred) 
(interp (if pred (third x) (fourth x)) 
env cc)))) 
(LAMBDA (let ((parms (second x)) 
(code (maybe-add 'begin (rest2 x)))) 

(funcall 
cc 
#*(lambda (cont &rest args) 

(interp code 
(extend-env parms args env) 
cont))))) 

(t (interp-call . env cc)))))) 

A few auxiliary functions are defined, in the same continuation-passing style: 

(defun interp-begin (body env cc) 
"Interpret each element of BODY, passing the last to CC." 
(interp (first body) env 

#.(lambda (val) 

(if (null (rest body)) 
(funcall cc val) 
(interp-begin (rest body) env cc))))) 

(defun interp-call (call env cc) 
"Interpret the call (f x...) and pass the result to CC." 
(map-interp call env 

#'(lambda (fn-and-args) 

(apply (first fn-and-args) 
cc 
(rest fn-and-args))))) 

(defun map-interp (list env cc) 
"Interpret each element of LIST, and pass the list to CC." 
(if (null list) 

(funcall cc nil) 
(interp (first list) env 
#'(lambda (x) 
(map-interp (rest list) env 
#'(lambda (y) 
(funcall cc (cons . y)))))))) 

<a id='page-776'></a>

Because Scheme procedures expect a continuation as the first argument, we need to 
redefine init-scheme-proc to install procedures that accept and apply the 
continuation: 

(defun init-scheme-proc (f) 
"Define a Scheme primitive procedure as a CL function." 
(if (listp f) 

(set-global-var! (first f) 
#'(lambda (cont &rest args) 
(funcall cont (apply (second f) args)))) 
(init-scheme-proc (list f f)))) 

We also need to define cal 1 /cc. Think for a moment about what cal 1 /cc must do. 
Like all Scheme procedures, it takes the current continuation as its first argument. 
The second argument is a procedure - computation to be performed, call/cc 
performs the computation by calling the procedure. This is just a normal call, 
so it uses the current continuation. The tricky part is what call/cc passes the 
computation as its argument. It passes an escape procedure, which can be invoked 
to return to the same point that the original call to cal 1 / cc would have returned to. 
Once the working of cal 1 /cc is understood, the implementation is obvious: 

(defun call/cc (cc computation) 
"Make the continuation accessible to a Scheme procedure." 
(funcall computation cc 

;; Package up CC into a Scheme function: 

#.(lambda (cont val) 
(declare (ignore cont)) 
(funcall cc val)))) 

Now install call/cc in the global environment 
(set-global-var! 'call/cc #'can/cc) 
(set-global-var! 'call-with-current-continuation #'call/cc) 

22.6 History and References 
Lisp interpreters and AI have a long history together. MIT AI Lab Memo No. 1 
(McCarthy 1958) was the first paper on Lisp. McCarthy's students were working 
on a Lisp compiler, had written certain routines - read, print, etc. - in assembly 

<a id='page-777'></a>
language, and were trying to develop a full Lisp interpreter in assembler. Sometime 
around the end of 1958, McCarthy wrote a theoretical paper showing that Lisp was 
powerful enough to write the universal function, eva 1. A programmer on the project, 
Steve Russell, saw the paper, and, according to McCarthy: 

Steve Russell said, look, lohy don't I program this eval and-you remember the 
interpreter-and I said to him, ho, ho, you're confusing theory with practice, this 
eval is intended for reading not for computing. But he went ahead and did it. 
That is, he compiled the eval in my paper into 704 machine code fixing bugs 
and then advertised this as a Lisp interpreter, which it certainly was.^ 

So the first Lisp interpreter was the result of a programmer ignoring his boss's 
advice. The first compiler was for the Lisp 1.5 system (McCarthy et al. 1962). The 
compiler was written in Lisp; it was probably the first compiler written in its own 
language. 

Allen's Anatomy of Lisp (1978) was one of the first overviews of Lisp implementation 
techniques, and it remains one of the best. However, it concentrates on the 
dynamic-scoping Lisp dialects that were in use at the time. The more modern view 
of a lexically scoped Lisp was documented in an influential pair of papers by Guy 
Steele (1976a,b). His papers "Lambda: the ultimate goto" and "Compiler optimization 
based on viewing lambda as rename plus goto" describe properly tail-recursive 
interpreters and compilers. 

The Scheme dialect was invented by Gerald Sussman and Guy Steele around 
1975 (see their MIT AI Memo 349). The Revised^ Report on the Algorithmic Language 
Scheme (dinger et al. 1991) is the definitive reference manual for the current version 
of Scheme. 

Abelson and Sussman (1985) is probably the best introduction to computer science 
ever written. It may or may not be a coincidence that it uses Scheme as the 
programming language. It includes a Scheme interpreter. Winston and Horn's Lisp 
(1989) also develops a Lisp interpreter. 

The amb operator for nondeterministic choice was proposed by John McCarthy 
(1963) and used in SCHEMER (Zabih et al. 1987), a nondeterministic Lisp. Ruf 
and Weise (1990) present another implementation of backtracking in Scheme that 
incorporates all of logic programming. 

^McCarthy's words from a talk on the history of Lisp, 1974, recorded by Stoyan (1984). 

<a id='page-778'></a>

22.7 Exercises 
&#9635; Exercise 22.5 [m] While Scheme does not provide full-blown support for optional 
and keyword arguments, it does support rest parameters. Modify the interpreter to 
support the Scheme syntax for rest parameters: 
Scheme(lambda(lambda 
. body) 
(x y . .) body) 
Common Lisp 
(lambda (&rest x) 
(lambda (x y &rest 
body) 
z) body) 

&#9635; Exercise 22.6 [h] The representation of environments is somewhat wasteful. Currently 
it takes 3n cons cells to represent an environment with . variables. Change 
the representation to take less space. 

&#9635; Exercise 22.7 [m] As we've implemented macros, they need to be expanded each 
time they are encountered. This is not so bad for the compiler - you expand the 
source code and compile it, and then never refer to the source code again. But for 
the interpreter, this treatment of macros is most unsatisfactory: the work of macro-
expansion must be done again and again. How can you eliminate this duplicated 
effort? 

&#9635; Exercise 22.8 [m] It turns out Scheme allows some additional syntax in 1 et and 
cond. First, there is the "named-let" expression, which binds initial values for variables 
but also defines a local function that can be called within the body of the 1 et. 
Second, cond recognizes the symbol => when it is the second element of a cond clause, 
and treats it as a directive to pass the value of the test (when it is not false) to the 
third element of the clause, which must be a function of one argument. Here are two 
examples: 
(define (fact n) 
Iterative factorial: does not grow the stack 
(let loop ((result 1) (i n)) 
(if (= i 0) result (loop (* result i) (-i 1))))) 
(define (lookup key alist) 
:: Find key's value in alist 
(cond ((assoc key alist) => cdr) 
(else #f))) 
These are equivalent to: 

<a id='page-779'></a>

(define (fact n) 
(letrec 
((loop (lambda (result i) 

(if (= i 0) 
result 
(loop (* result i) (-i 1)))))) 

(loop 1 n))) 

(define (lookup key alist) 
(let ((g0030 (assoc key alist))) 

(if gOOSO 
(cdr g0030) 
#f))) 

Write macro definitions for 1 et and cond allowing these variations. 

&#9635; Exercise 22.9 Pi] Some Scheme implementations permit def i ne statements inside 
the body of a 1 ambda (and thus of a def i ne, 1 et, 1 et*, or 1 etrec as well). Here is an 
example: 

(define (length 1) 
(define (len 1 n) 
(if (null? 1) . (len (cdr 1) (+ . 1)))) 
(len 1 0)) 

The internal definition of len is interpreted not as defining a global name but rather 

as defining a local name as if with 1 etrec. The above definition is equivalent to: 

(define (length 1) 
(letrec (den (lambda (1 n) 
(if (null? 1) . (len (cdr 1) (+ . 1)))))) 
(len 1 0))) 

Make changes to the interpreter to allow this kind of internal definition. 

&#9635; Exercise 22.10 Scheme programmers are often disdainful of the function or # ' 
notation in Common Lisp. Is it possible (without changing the compiler) to make 
Common Lisp accept (1 ambda () ...) instead of # ' (1 ambda () ...) and f . 
instead of #'fn? 

&#9635; Exercise 22.11 [m] The top level of the continuation-passing version of scheme 
includes the call: (i nterp (read) nil #' pr int). Will this always result in some 

<a id='page-780'></a>

value being printed? Or is it possible that the expression read might call some escape 
function that ignores the value without printing anything? 

&#9635; Exercise 22.12 [h] What would have to be added or changed to turn the Scheme 
interpreter into a Common Lisp interpreter? 

&#9635; Exercise 22.13 \h] How would you change the interpreter to allow for multiple 
values? Explain how this would be done both for the first version of the interpreter 
and for the continuation-passing version. 

22.8 Answers 
Answer 22.2 There is no way to implement a full ca . / cc to Common Lisp, but the 
following works for cases where the continuation is only used with dynamic extent: 

(defun call/cc (computation) 
"Call computation, passing it the current continuation. 
The continuation has only dynamic extent." 
(funcall computation #'(lambda (x) (return-from call/cc x)))) 

Answer 22.3 No. fail requires continuations with dynamic extent. 

Answer 22.5 We need only modify extend - en . to know about an atomic vars list. 
While we're at it, we might as well add some error checking: 

(defun extend-env (vars vals env) 
"Add some variables and values to an environment." 
(cond ((null vars) 

(assert (null vals) () "Too many arguments supplied") 
env) 
((atom vars) 
(cons (list vars vals) env)) 
(t (assert (rest vals) () "Too few arguments supplied") 
(cons (list (first vars) (first vals)) 
(extend-env (rest vars) (rest vals) env))))) 

<a id='page-781'></a>

Answer 22.6 Storing the environment as an association list, {{var val),,.), makes 
it easy to look up variables with assoc. We could save one cons cell per variable 
just by changing to {{var . val)..,). But even better is to switch to a different 
representation, one presented by Steele and Sussman in The Art of the Interpreter 
(1978). In this representation we switch from a single list of var/val pairs to a list of 
frames, where each frame is a var-list/val-list pair. It looks like this: 

{{{var...) . {val...)) 
{{var...) . {val...)) 
...) 

Now extend-env is trivial: 

(defun extend-env (vars vals env) 
"Add some variables and values to an environment." 
(cons (cons vars vals) env)) 

The advantage of this approach is that in most cases we already have a list of 
variables (the procedure's parameter list) and values (from the mapcar of interp 
over the arguments). So it is cheaper to just cons these two lists together, rather than 
arranging them into pairs. Of course, get - va r and set - va r! become more complex. 

Answer 22.7 One answer is to destructively alter the source code as it is macro-
expanded, so that the next time the source code is interpreted, it will already be 
expanded. The following code takes care of that: 

(defun scheme-macro-expand (x) 
(displace . (apply (scheme-macro (first x)) (rest x)))) 

(defun displace (old new) 
"Destructively change old cons-cell to new value." 
(if (consp new) 

(progn (setf (car old) (car new)) 
(setf (cdr old) (cdr new)) 
old) 

(displace old '(begin .new)))) 

One drawback to this approach is that the user's source code is actually changed, 

which may make debugging confusing. An alternative is to expand into something 

that keeps both the original and macro-expanded code around: 

<a id='page-782'></a>

(defun displace (old new) 
"Destructively change old to a DISPLACED structure." 
(setf (car old) 'DISPLACED) 
(setf (cdr old) (list new old)) 
old) 

This means that DISPLACED is a new special form, and we need a clause for it in the 
interpreter. It would look something like this: 

(case (first x) 

(DISPLACED (interp (second x) env)) 

We'd also need to modify the printing routines to print just ol d whenever they see 
(displaced old new). 

Answer 22.8 

(def-scheme-macro let (vars &rest body) 
(if (symbolp vars) 
named let 
(let ((f vars) (vars (first body)) (body (rest body))) 
'(letrec ((,f (lambda .(mapcar #'first vars) ..body))) 
(.f ..(mapcar #*second vars)))) 
"regular" let 
'((lambda .(mapcar #'first vars) . .body) 
. .(mapcar #*second vars))))) 

(def-scheme-macro cond (&rest clauses) 
(cond ((null clauses) nil) 
((length=1 (first clauses)) 
'(or .(first clauses) (cond ..(rest clauses)))) 
((starts-with (first clauses) 'else) 

'(begin..(rest (first clauses)))) 
((eq (second (first clauses)) *=>) 
(assert (= (length (first clauses)) 3)) 
(let ((var (gensym))) 

'(let ((.var .(first (first clauses)))) 
(if .var (.(third (first clauses)) .var) 
(cond ..(rest clauses)))))) 
(t '(if .(first (first clauses)) 
(begin ..(rest (first clauses))) 
(cond ..(rest clauses))))))) 

<a id='page-783'></a>

Answer 22.10 It is easy to define . ambda as a macro, eliminating the need for 
#.(lambda ...): 

(defmacro lambda (args &rest body) 
'(function (lambda ,args .body))) 

If this were part of the Common Lisp standard, I would gladly use it. But because it 
is not, I have avoided it, on the grounds that it can be confusing. 
It is also possible to write a new function-defining macro that would do the 
following type of expansion: 

(defn double (x) (* 2 x)) 
(defparameter double (defun double (x) (* 2 x))) 

This makes doubl e a special variable, so we can write doubl e instead of # 'doubl e. 
But this approach is not recommended - it is dangerous to define special variables 
that violate the asterisk convention, and the Common Lisp compiler may not be able 
to optimize special variable references the way it canfunction special forms. Also, 
this approach would not interact properly with f 1 et and 1 abel s. 

