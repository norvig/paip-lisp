# Chapter 12 {docsify-ignore}
<a id='page-388'></a>

Compiling Logic 
Programs 

V I 1 he end of chapter 11 introduced a new, more efficient representation for logic variables. 

I It would be reasonable to build a new version of the Prolog interpreter incorporating 

JL this representation. However, chapter 9 has taught us that compilers run faster than 
interpreters and are not that much harder to build. Thus, this chapter will present a Prolog 
compiler that translates from Prolog to Lisp. 

Each Prolog predicate will be translated into a Lisp function, and we will adopt the convention 
that a predicate called with a different number of arguments is a different predicate. If the symbol 
. can be called with either one or two arguments, we will need two Lisp functions to implement 
the two predicates. Following Prolog tradition, these will be called p/1 and p/2. 

The next step is to decide what the generated Lisp code should look like. It must unify 
the head of each clause against the arguments, and if the unification succeeds, it must call the 
predicates in the body. The difficult part is that the choice points have to be remembered. If 
a call to a predicate in the first clause fails, we must be able to return to the second clause and 
try again. 

<a id='page-389'></a>

This can be done by passing in a success continuation as an extra argument to 
every predicate. This continuation represents the goals that remain unsolved, the 
other-goal s argument of prove. For each clause in the predicate, if all the goals iri a 
clause succeed, then we should call the success continuation. If a goal fails, we don't 
do anything special; we just go on to the next clause. There is one complication: after 
failing we have to undo any bindings made by uni fy I. Consider an example. The 
clauses 

(<- (likes Robin cats)) 

(<- (likes Sandy ?x) (likes ?x cats)) 

(<- (likes Kim ?x) (likes ?x Lee) (likes ?x Kim)) 

could be compiled into this: 

(defun likes/2 (?argl ?arg2 cont) 
First clause: 
(if (and (unify! ?argl 'Robin) (unify! ?arg2 'cats)) 
(funcall cont)) 
(undo-bindings) 
Second clause: 
(if (unify! ?argl 'Sandy) 
(likes/2 ?arg2 'cats cont)) 
(undo-bindings) 
Third clause: 
(if (unify! ?argl 'Kim) 
(likes/2 ?arg2 'Lee 
#'(lambda () (likes/2 ?arg2 'Kim cont)))))) 

In the first clause, we just check the two arguments and, if the unifications succeed, 

call the continuation directly, because the first clause has no body. In the second 

clause, 1 i kes/2 is called recursively, to see if ?arg2 likes cats. If this succeeds, then 

the original goal succeeds, and the continuation cont is called. In the third clause, 

we have to call1 i kes/2 recursively again, this time requesting that it check if ?arg2 

likes Lee. If this check succeeds, then the continuation will be called. In this case, 

the continuation involves another call to 1 i kes/2, to check if ?arg2 likes Kim. If this 

succeeds, then the original continuation, cont, will finally be called. 

Recall that in the Prolog interpreter, we had to append the list of pending goals, 
other-goal s, to the goals in the body of the clause. In the compiler, there is no need 
to do an append. Instead, the continuation cont represents the other-goals, and the 
body of the clause is represented by explicit calls to functions. 

<a id='page-390'></a>

Note that the code for 1 i kes/2 given before has eUminated some unnecessary 
calls to uni fy!. The most obvious implementation would have one call to uni fy 1 for 
each argument. Thus, for the second clause, we would have the code: 

(if (and (unify! ?argl 'Sandy) (unifyl ?arg2 ?x)) 
(likes/2 ?x 'cats cont)) 

where we would need a suitable 1 et binding for the variable ?x. 

12.1 A Prolog Compiler 
This section presents the compiler summarized in figure 12.1. At the top level is 
the function prol og-compi 1 e, which takes a symbol, looks at the clauses defined for 
that symbol, and groups the clauses by arity. Each symbol/arity is compiled into a 
separate Lisp function by compi 1 e-predi cate. 

(defun prolog-compile (symbol &optional 

(clauses (get-clauses symbol))) 
"Compile a symbol; make a separate function for each arity." 
(unless (null clauses) 

(let ((arity (relation-arity (clause-head (first clauses))))) 
;; Compile the clauses with this arity 
(compile-predicate 

symbol arity (clauses-with-arity clauses #'= arity)) 
;; Compile all the clauses with any other arity 
(prolog-compile 

symbol (clauses-with-arity clauses #'/= arity))))) 

Three utility functions are included here: 

(defun clauses-with-arity (clauses test arity) 
"Return all clauses whose head has given arity." 
(find-all arity clauses 

:key #'(lambda (clause) 
(relation-arity (clause-head clause))) 
rtest test)) 

(defun relation-arity (relation) 
"The number of arguments to a relation. 
Example: (relation-arity '(p a b c)) => 3" 
(length (args relation))) 

(defun args (x) "The arguments of a relation" (rest x)) 

The next step is to compile the clauses for a given predicate with a fixed arity into a 

<a id='page-391'></a>
?-

nrail* 

var 

top-level-prove 
run-prolog 

prOlog-compi1e-symbols 
prolog-compile 
compile-predicate 
compile-clause 
compile-body 
compile-call 
compile-arg 
compile-unify 

clauses-with-arity 
relation-arity 
args 
make-parameters 
make-predicate 
make-= 
def-prolog-compi1er-macro 
prolog-compi1er-macro 
has-variable-p 
proper-listp 
maybe-add-undo-bindings 
bind-unbound-vars 
make-anonymous 
anonymous-variables-in 
compile-if 
compile-unify-vari able 
bind-variables-in 
follow-binding 
bind-new-variables 
ignore 

unify! 
undo-bindings! 
binding-val 
symbol 
new-symbol 
find-anywhere 

Top-Level Functions 

Make a query, but compile everything first. 

Special Variables 

A list of all bindings made so far. 

Data Types 

A box for a variable; can be destructively modified. 

Major Functions 

New version compiles everything first. . 
Compile everything and call a Prolog function. 
Compile a list of Prolog symbols. 
Compile a symbol; make a separate function for each arity. 
Compile all the clauses for a given symbol/arity. 
Transform away the head and compile the resulting body. 
Compile the body of a clause. 
Compile a call to a Prolog predicate. 
Generate code for an argument to a goal in the body. 
Return code that tests if var and term unify. 

Auxiliary Functions 

Return all clauses whose head has given arity. 
The number of arguments to a relation. 
The arguments of a relation. 
Build a list of parameters. 
Build a symbol of the form name/ari ty. 
Build a unification relation. 
Define a compiler macro for Prolog. 
Fetch the compiler macro for a Prolog predicate. 
Is there a variable anywhere in the expression x? 
IsX a proper (non-dotted) list? 
Undo any bindings that need undoing. 
Add a let if needed. 
Replace variables that are only used once with ?. 
A list of anonymous variables. 
Compile an IF form. No else-part allowed. 
Compile the unification of a var. 
Bind all variables in exp to themselves. 
Get the ultimate binding of var according to bindings. 

Extend bindings to include any unbound variables. 
Do nothing—ignore the arguments. 

Previously Defined Fimctions 

Destructive unification (see section 11.6). 

Use the trail to backtrack, undoing bindings. 
Pick out the value part of a var/val binding. 
Create or find an interned symbol. 
Create a new uninterned symbol. 
Does item occur anywhere in tree? 

Figure 12.1: Glossary for the Prolog Compiler 

<a id='page-392'></a>

Lisp function. For now, that will be done by compiling each clause indepently and 
wrapping them in a 1 ambda with the right parameter list. 

(defun compile-predicate (symbol arity clauses) 
"Compile all the clauses for a given symbol/arity 
into a single LISP function." 
(let ((predicate (make-predicate symbol arity)) 

(parameters (make-parameters arity))) 
(compile 
(eval 
'(defun .predicate (,parameters cont) 
..(mapcar #*(lambda (clause) 
(compile-clause parameters clause 'cont)) 
clauses)))))) 

(defun make-parameters (arity) 
"Return the list (?argl ?arg2 ... ?arg-arity)" 
(loop for i from 1 to arity 

collect (new-symbol '?arg i))) 

(defun make-predicate (symbol arity) 
"Return the symbol: symbol/arity" 
(symbol symbol V arity)) 

Now for the hard part: we must actually generate the code for a clause. Here again 
is an example of the code desired for one clause. We'll start by setting as a target the 
simple code: 

(<- (likes Kim ?x) (likes ?x Lee) (likes ?x Kim)) 

(defun likes/2 (?argl ?arg2 cont) 

(if (and (unify! ?argl *Kim) (unify! ?arg2 ?x) 
(likes/2 ?arg2 'Lee 
#'(lambda () (likes/2 ?x 'Kim)))) 

...) 

but we'll also consider the possibility of upgrading to the improved code: 

(defun likes/2 (?argl ?arg2 cont) 

(if (unify! ?argl 'Kim) 
(likes/2 ?arg2 'Lee 
#'(lambda () (likes/2 ?arg2 'Kim)))) 

...) 

One approach would be to write two functions, compi 1 e-head and compi 1 e-body. 

<a id='page-393'></a>
and then combine them into the code ( i f head body). This approach could easily 
generate the prior code. However, let's allow ourselves to think ahead a little. If we 
eventually want to generate the improved code, we will need some communication 
between the head and the body. We will have to know that the head decided not 
to compile the unification of ?arg2 and ?x, but because of this, the body will have 
to substitute ?arg2 for ?x. That means that the compi 1 e - head function conceptually 
returns two values: the code for the head, and an indication of substitutions to 
perform in the body. This could be handled by explicitly manipulating multiple 
values, but it seems complicated. 

An alternate approach is to eliminate compi 1 e - head and just write compi 1 e - body. 
This is possible if we in effect do a source-code transformation on the clause. Instead 
of treating the clause as: 

(<- (likes Kim ?x) 
(likes ?x Lee) (likes ?x Kim)) 

we transform it to the equivalent: 

(<- (likes ?argl ?arg2) 
(= ?argl Kim) (= ?arg2 ?x) (likes ?x Lee) (likes ?x Kim)) 

Now the arguments in the head of the clause match the arguments in the function 
1 i kes/2, so there is no need to generate any code for the head. This makes things 
simpler by eliminating compi 1 e-head, and it is a better decomposition for another 
reason: instead of adding optimizations to compi 1 e-head, we will add them to the 
code in compi 1 e-body that handles =. That way, we can optimize calls that the user 
makes to =, in addition to the calls introduced by the source-code transformation. 

To get an overview, the calling sequence of functions will turn out to be as follows: 

prolog-compile 
compile-predicate 
compile-clause 

compile-body 
compile-call 
compile-arg 
compile-unify 

compile-arg 

where each function calls the ones below it that are indented one level. We have already 
defined the first two functions. Here thenisourfirstversionof compi 1 e-cl ause: 

<a id='page-394'></a>

(defun compile-clause (parms clause cont) 
"Transform away the head, and compile the resulting body." 
(compile-body 

(nconc 
(mapcar #'make-= parms (args (clause-head clause))) 
(clause-body clause)) 

cont)) 

(defun make-= (x y) *(= .x .y)) 

The bulk of the work is in compi 1 e - body, which is a little more complicated. There are 
three cases. If there is no body, we just call the continuation. If the body starts with 
a call to =, we compile a call to uni fy!. Otherwise, we compile a call to a function, 
passing in the appropriate continuation. 

However, it is worthwhile to think ahead at this point. If we want to treat = 
specially now, we will probably want to treat other goals specially later. So instead 
of explicitly checking for =, we will do a data-driven dispatch, looking for any predicate 
that has a prol og-compi 1 er-macro property attached to it. Like Lisp compiler 
macros, the macro can decline to handle the goal. We will adopt the convenhon that 
returning .-pass means the macro decided not to handle it, and thus it should be 
compiled as a normal goal. 

(defun compile-body (body cont) 
"Compile the body of a clause." 
(if (null body) 

Mfuncall .cont) 

(let* ((goal (first body)) 
(macro (prolog-compiler-macro (predicate goal))) 
(macro-val (if macro 

(funcall macro goal (rest body) cont)))) 

(if (and macro (not (eq macro-val :pass))) 
macro-val 
(compile-cal 1 

(make-predicate (predicate goal) 
(relation-arity goal)) 
(mapcar #'(lambda (arg) (compile-arg arg)) 
(args goal)) 

(if (null (rest body)) 
cont 
'#'(lambda () 

.(compile-body (rest body) cont)))))))) 

(defun compile-call (predicate args cont) 
"Compile a call to a prolog predicate." 
'(.predicate .@args .cont)) 

<a id='page-395'></a>
(defun prolog-compiler-macro (name) 
"Fetch the compiler macro for a Prolog predicate." 
Note NAME is the raw name, not the name/arity 
(get name 'prolog-compiler-macro)) 

(defmacro def-prolog-compi1er-macro (name arglist &body body) 
"Define a compiler macro for Prolog." 
'(setf (get ',name 'prolog-compiler-macro) 

#'(lambda .arglist ..body))) 

(def-prolog-compi1er-macro = (goal body cont) 
(let ((args (args goal))) 
(if (/= (length args) 2) 
.-pass 

'(if.(compile-unify (first args) (second args)) 
.(compile-body body cont))))) 
(defun compile-unify (x y) 
"Return code that tests if var and term unify." 
'(unify! .(compile-arg x) .(compile-arg y))) 

All that remains is compi1 e-arg, a function to compile the arguments to goals in the 
body. There are three cases to consider, as shown in the compilation to the argument 
of q below: 

1 (<- (p ?x) (q ?x)) (q/1 ?x cont) 

2 (<- (p ?x) (q (f a b))) (q/1 '(f a b) cont) 

3 (<- (p ?x) (q (f ?x b))) (q/1 (list 'f ?x 'b) cont) 

In case 1, the argument is a variable, and it is compiled as is. In case 2, the argument 
is a constant expression (one without any variables) that compiles into a quoted 
expression. In case 3, the argument contains a variable, so we have to generate code 
that builds up the expression. Case 3 is actually split into two in the list below: one 
compiles into a call to 1 i st, and the other a call to cons. It is important to remember 
that the goal (q (f ?x b)) does not involve a call to the function f. Rather, it involves 
the term (f ?x b), which is just a list of three elements. 

(defun compile-arg (arg) 
"Generate code for an argument to a goal in the body." 
(cond ((variable-p arg) arg) 

((not (has-variable-p arg)) ".arg) 
((proper-listp arg) 
'(list ..(mapcar #'compile-arg arg))) 
(t '(cons .(compile-arg (first arg)) 
.(compile-arg (rest arg)))))) 

<a id='page-396'></a>

(defun has-variable-p (x) 
"Is there a variable anywhere in the expression x?" 
(find-if-anywhere #'variable-p x)) 

(defun proper-listp (x) 
"Is X a proper (non-dotted) list? " 
(or (null x) 

(and (consp x) (proper-listp (rest x))))) 

Let's see how it works. We will consider the following clauses: 

(<- (likes Robin cats)) 
(<- (likes Sandy ?x) (likes ?x cats)) 
(<- (likes Kim ?x) (likes ?x Lee) (likes ?x Kim)) 

(<- (member ?item (?item . ?rest))) 
(<- (member ?item (?x . ?rest)) (member Titem ?rest)) 

Here's what prol og-compi 1 e gives us: 

(DEFUN LIKES/2 (7ARG1 ?ARG2 CONT) 
(IF (UNIFY! ?ARG1 'ROBIN) 
(IF (UNIFY! 7ARG2 'CATS) 
(FUNCALL CONT))) 
(IF (UNIFY! ?ARG1 'SANDY) 
(IF (UNIFY! ?ARG2 ?X) 
(LIKES/2 ?X 'CATS CONT))) 
(IF (UNIFY! 7ARG1 'KIM) 
(IF (UNIFY! ?ARG2 ?X) 
(LIKES/2 ?X 'LEE (LAMBDA () 
(LIKES/2 ?X 'KIM CONT)))))) 

(DEFUN MEMBER/2 (7ARG1 7ARG2 CONT) 
(IF (UNIFY! 7ARG1 7ITEM) 
(IF (UNIFY! 7ARG2 (CONS 7ITEM 7REST)) 
(FUNCALL CONT))) 
(IF (UNIFY! 7ARG1 7ITEM) 
(IF (UNIFY! 7ARG2 (CONS 7X 7REST)) 
(MEMBER/2 7ITEM 7REST CONT)))) 

<a id='page-397'></a>
12.2 Fixing the Errors in the Compiler 
There are some problems in this version of the compiler: 

* We forgot to undo the bindings after each call to uni fy!. 
* The definition of undo-bi ndi ngs! defined previously requires as an argument 
an index into the *trai 1 * array. So we will have to save the current top of the 
trail when we enter each function. 
* Local variables, such as ?x, were used without being introduced. They should 
be bound to new variables. 
Undoing the bindings is simple: we add a single line to compile-predicate, 
a call to the function maybe-add-undo-bindings. This function inserts a call to 
undo-bi ndi ngs! after every failure. If there is only one clause, no undoing is necessary, 
because the predicate higher up in the calling sequence will do it when it fails. 
If there are multiple clauses, the function wraps the whole function body in a . et 
that captures the initial value of the trail's fill pointer, so that the bindings can be 
undone to the right point. Similarly, we can handle the unbound-variable problem 
by wrapping a call to bind - unbound- va rs around each compiled clause: 

(defun compile-predicate (symbol arity clauses) 
"Compile all the clauses for a given symbol/arity 
into a single LISP function." 
(let ((predicate (make-predicate symbol arity)) 

(parameters (make-parameters arity))) 
(compile 
(eval 
'(defun .predicate (,parameters cont) 
(maybe-add-undo-bindings 
(mapcar #*(lambda (clause) 
(compile-clause parameters 
clause 'cont)) 
clauses))))))) 

(defun compile-clause (parms clause cont) 
"Transform away the head, and compile the resulting body." 
(bind-unbound-vars 

parms 
(compi1e-body 

(nconc 
(mapcar #'make-= parms (args (clause-head clause))) 
(clause-body clause)) 

cont))) 

<a id='page-398'></a>

(defun maybe-add-undo-bindings (compiled-exps) 
"Undo any bindings that need undoing. 
If there are any, bind the trail before we start." 
(if (length=1 compiled-exps) 

compiled-exps 

'((let ((old-trail (fill-pointer nrail*))) 
,(first compiled-exps) 
,@(loop for exp in (rest compiled-exps) 
collect '(undo-bindings! old-trail) 
collect exp))))) 

(defun bind-unbound-vars (parameters exp) 
"If there are any variables in exp (besides the parameters) 
then bind them to new vars." 
(let ((exp-vars (set-difference (variables-in exp) 

parameters))) 
(if exp-vars 
'(let .(mapcar #'(lambda (var) *(.var (?))) 
exp-vars) 
,exp) 
exp))) 

With these improvements, here's the code we get for 1 i kes and member: 

(DEFUN LIKES/2 (?ARG1 ?ARG2 CONT) 
(LET ((OLD-TRAIL (FILL-POINTER *TRAIL*))) 
(IF (UNIFY! ?ARG1 'ROBIN) 
(IF (UNIFY! ?ARG2 'CATS) 

(FUNCALL CONT))) 
(UNDO-BINDINGS! OLD-TRAIL) 
(LET ((?X (?))) 

(IF (UNIFY! ?ARG1 'SANDY) 
(IF (UNIFY! ?ARG2 ?X) 

(LIKES/2 ?X 'CATS CONT)))) 
(UNDO-BINDINGS! OLD-TRAIL) 
(LET ((?X (?))) 

(IF (UNIFY! ?ARG1 'KIM) 
(IF (UNIFY! ?ARG2 ?X) 
(LIKES/2 ?X 'LEE (LAMBDA () 
(LIKES/2 ?X 'KIM CONT)))))))) 

<a id='page-399'></a>

(DEFUN MEMBER/2 (?ARG1 ?ARG2 CONT) 
(LET ((OLD-TRAIL (FILL-POINTER *TRAIL*))) 
(LET ((?ITEM (?)) 
(?RE$T (?))) 
(IF (UNIFY! ?ARG1 ?ITEM) 
(IF (UNIFY! ?ARG2 (CONS ?ITEM ?REST)) 

(FUNCALL CONT)))) 
(UNDO-BINDINGS! OLD-TRAIL) 
(LET ((?X (?)) 

(?ITEM (?)) 
(?REST (?))) 
(IF (UNIFY! ?ARG1 ?ITEM) 
(IF (UNIFY! ?ARG2 (CONS ?X ?REST)) 
(MEMBER/2 ?ITEM ?REST CONT)))))) 

12.3 Improving the Compiler 
This is fairly good, although there is still room for improvement. One minor improvement 
is to eliminate unneeded variables. For example, ?rest in the first clause of 
member and ?x in the second clause are bound to new variables—the result of the (?) 
call—and then only used once. The generated code could be made a little tighter by 
just putting (?) inline, rather than binding it to a variable and then referencing that 
variable. There are two parts to this change: updating compi 1 e-arg to compile an 
anonymous variable inline, and changing the < - macro so that it converts all variables 
that only appear once in a clause into anonymous variables: 

(defmacro <- (&rest clause) 
"Add a clause to the data base." 
'(add-clause '.(make-anonymous clause))) 

(defun compile-arg (arg) 
"Generate code for an argument to a goal in the body." 
(cond ((eq arg '?) '(?)) 

((variable-p arg) arg) 
((not (has-variable-p arg)) ",arg) 
((proper-listp arg) 

'(list ..(mapcar #'compile-arg arg))) 
(t '(cons .(compile-arg (first arg)) 
.(compile-arg (rest arg)))))) 
(defun make-anonymous (exp &optional 
(anon-vars (anonymous-variables-in exp))) 
"Replace variables that are only used once with ?. " 
(cond ((consp exp) 

(reuse-cons (make-anonymous (first exp) anon-vars) 

<a id='page-400'></a>

(make-anonymous (rest exp) anon-vars) 

exp)) 
((member exp anon-vars) *?) 
(t exp))) 

Finding anonymous variables is tricky. The following function keeps two lists: the 
variables that have been seen once, and the variables that have been seen twice 
or more. The local function wal k is then used to walk over the tree, recursively 
considering the components of each cons cell and updating the two lists as each 
variable is encountered. This use of local functions should be remembered, as well 
as an alternative discussed in exercise 12.23 on [page 428](chapter12.md#page-428). 

(defun anonymous-variables-in (tree) 
"Return a list of all variables that occur only once in tree." 
(let ((seen-once nil) 

(seen-more nil)) 
(labels ((walk (x) 
(cond 
((variable-p x) 

(cond ((member . seen-once) 
(setf seen-once (delete . seen-once)) 
(push . seen-more)) 

((member . seen-more) nil) 
(t (push . seen-once)))) 

((consp x) 
(walk (first x)) 
(walk (rest x)))))) 

(walk tree) 
seen-once))) 

Now member compiles into this: 

(DEFUN MEMBER/2 (?ARG1 ?ARG2 CONT) 
(LET ((OLD-TRAIL (FILL-POINTER nRAIL*))) 
(LET ((?ITEM (?))) 
(IF (UNIFY! ?ARG1 ?ITEM) 
(IF (UNIFY! ?ARG2 (CONS ?ITEM (?))) 

(FUNCALL CONT)))) 
(UNDO-BINDINGS! OLD-TRAIL) 
(LET ((?ITEM (?)) 

(?REST (?))) 
(IF (UNIFY! ?ARG1 ?ITEM) 
(IF (UNIFY! ?ARG2 (CONS (?) ?REST)) 
(MEMBER/2 ?ITEM ?REST CONT)))))) 

<a id='page-401'></a>
12.4 Improving the Compilation of Unification 
Now we turn to the improvement of compi1 e - un i f y. Recall that we want to eliminate 
certain calls to uni fy ! so that, for example, the first clause of member: 

(<- (member ?item (?item . ?rest))) 

compiles into: 

(LET ((?ITEM (?))) 
(IF (UNIFY! ?ARG1 ?ITEM) 
(IF (UNIFY! ?ARG2 (CONS ?ITEM (?))) 
(FUNCALL CONT)))) 

when it could compile to the more efficient: 

(IF (UNIFY! ?ARG2 (CONS ?ARG1 (?))) 
(FUNCALL CONT)) 

Eliminating the unification in one goal has repercussions in other goals later on, so 
we will need to keep track of expressions that have been unified together. We have 
a design choice. Either compi1 e-unify can modify a global state variable, or it can 
return multiple values. On the grounds that global variables are messy, we make the 
second choice: compi 1 e- uni fy will take a binding list as an extra argument and will 
return two values, the actual code and an updated binding list. We will expect that 
other related functions will have to be modified to deal with these multiple values. 

When compi le -unify is first called in our example clause, it is asked to unify 
?argl and ?item. We want it to return no code (or more precisely, the trivially true 
test, t). For the second value, it should return a new binding list, with ?i tem bound 
to ?argl. That binding will be used to replace ?i tem with ?argl in subsequent code. 

How do we know to bind ?item to ?argl rather than the other way around? 
Because ?argl is already bound to something—the value passed in to member. We 
don't know what this value is, but we can't ignore it. Thus, the initial binding list will 
have to indicate that the parameters are bound to something. A simple convention 
is to bind the parameters to themselves. Thus, the initial binding list will be: 

((?argl . ?argl) (?arg2 . ?arg2)) 

We saw in the previous chapter ([page 354](chapter11.md#page-354)) that binding a variable to itself can lead to 
problems; we will have to be careful. 
Besides eliminating unifications of new variables against parameters, there are 
quite a few other improvements that can be made. For example, unifications involv


<a id='page-402'></a>

ing only constants can be done at compile time. The call (= (f a) (f a)) always 
succeeds, while (=3 4) always fails. In addition, unification of two cons cells can 
be broken into components at compile time: (= (f ?x) (f a)) reduces to (= ?x 

a) and (= f f), where the latter trivially succeeds. We can even do some occm-s 
checking at compile time: (= ?x (f ?x)) should fail. 
The following table lists these improvements, along with a breakdown for the 
cases of unifying a bound (? a rg1) or unbound (?x) variable agains another expression. 
The first column is the unification call, the second is the generated code, and the third 
is the bindings that will be added as a result of the call: 

Unification Code Bindings 
1 (= 3 3) t — 
2 (= 3 4) nil — 
3 (= (f ?x) (?p 3)) t (?x . 3) (?p . f) 
4 (= ?argl ?y) t (?y . ?argl) 
5 (= ?argl ?arg2) (unify! ?argl ?arg2) (?argl . ?arg2) 
6 (= ?argl 3) (unify! ?argl 3) (?argl . 3) 
7 (= ?argl (f ?y)) (unify! ?argl ...) (?y . ?y) 
8 (= ?x ?y) t (?x . ?y) 
9 (= ?x 3) t (?x . 3) 

10 (= ?x (f ?y)) (unify! ?x ...) (?y . ?y) 
11 (= ?x (f ?x)) nil — 
12 (= ?x ?) t 

-

From this table we can craft our new version of compi1 e-uni fy. The first part 
is fairly easy. It takes care of the first three cases in this table and makes stue 
that compi 1 e-uni fy-vari abl e is called with a variable as the first argument for the 
other cases. 

(defun compile-unify (x y bindings) 
"Return 2 values: code to test if . and y unify, 
and a new binding list." 
(cond 

Unify constants and conses: ; Case 
((not (or (has-variable-p x) (has-variable-p y))) ; 1,2 
(values (equal . y) bindings)) 
((and (consp x) (consp y)) : 3 
(multiple-value-bind (codel bindingsl) 
(compile-unify (first x) (first y) bindings) 
(multiple-value-bind (code2 bindings2) 
(compile-unify (rest x) (rest y) bindingsl) 
(values (compile-if codel code2) bindings2)))) 

Here . or y is a variable. Pick the right one: 
((variable-p x) (compi1e-unify-variable . y bindings)) 
(t (compile-unify-variab1e y . bindings)))) 

<a id='page-403'></a>
(defun compile-if (pred then-part) 
"Compile a Lisp IF form. No else-part allowed." 
(case pred 

((t) then-part) 
((nil) nil) 
(otherwise *(if ,pred .then-part)))) 

The function compi 1 e - uni fy - va r i abl e following is one of the most complex we have 

seen. For each argument, we see if it has a binding (the local variables xb and yb), 

and then use the bindings to get the value of each argument (xl and y 1). Note that for 

either an unbound variable or one bound to itself, . will equal xl (and the same for y 

andyl). If either of the pairs of values is not equal, we should use the new ones (xl or 

y 1), and the clause commented deref does that. After that point, we just go through 

the cases, one at a time. It turns out that it was easier to change the order slightly from 

the preceding table, but each clause is commented with the corresponding number: 

(defun compile-unify-vari able (x y bindings) 
"X is a variable, and Y may be." 
(let* ((xb (follow-binding . bindings)) 

(xl (if xb (cdr xb) x)) 
(yb (if (variable-p y) (follow-binding y bindings))) 
(yl (if yb (cdr yb) y))) 

(cond ; Case: 
((or (eq . *?) (eq y *?)) (values t bindings)) ; 12 
((not (and (equal . xl) (equal y yl))) ; deref 

(compile-unify xl yl bindings)) 
((find-anywhere xl yl) (values nil bindings)) ; 11 
((consp yl) ; 7.10 

(values '(unifyl .xl .(compile-arg yl bindings)) 
(bind-variables-in yl bindings))) 

((not (null xb)) 
;.. i.e. X is an ?arg variable 
(if (and (variable-p yl) (null yb)) 

(values 't (extend-bindings yl xl bindings)) ; 4 
(values '(unify! .xl ,(compile-arg yl bindings)) 
(extend-bindings xl yl bindings)))) ; 5.6 
((not (null yb)) 
(compile-unify-variable yl xl bindings)) 
(t (values 't (extend-bindings xl yl bindings)))))) ; 8.9 

Take some time to understand just how this function works. Then go on to the 
following auxiliary functions: 

<a id='page-404'></a>

(defun bind-variables-in (exp bindings) 
"Bind all variables in exp to themselves, and add that to 
bindings (except for variables already bound)." 
(dolist (var (variables-in exp)) 

(unless (get-binding var bindings) 
(setf bindings (extend-bindings var var bindings)))) 
bindings) 

(defun follow-binding (var bindings) 
"Get the ultimate binding of var according to bindings." 
(let ((b (get-binding var bindings))) 

(if (eq (car b) (cdr b)) 
b 
(or (follow-binding (cdr b) bindings) 

b)))) 

Now we need to integrate the new compi 1 e - uni f y into the rest of the compiler. The 
problem is that the new version takes an extra argument and returns an extra value, 
so all the functions that call it need to be changed. Let's look again at the calling 
sequence: 

prolog-compile 
compile-predicate 
compile-clause 

compile-body 
compile-call 
compile-arg 

compile-unify 
compile-arg 

First, going downward, we see that compi 1 e-arg needs to take a binding Ust as an 
argument, so that it can look up and substitute in the appropriate values. But it will 
not alter the binding list, so it still returns one value: 

(defun compile-arg (arg bindings) 
"Generate code for an argument to a goal in the body." 
(cond ((eq arg *?) '(?)) 

((variable-p arg) 
(let ((binding (get-binding arg bindings))) 
(if (and (not (null binding)) 

(not (eq arg (binding-val binding)))) 
(compile-arg (binding-val binding) bindings) 
arg))) 

((not (find-if-anywhere #'variable-p arg)) ".arg) 
((proper-listp arg) 

'(list ..(mapcar #*(lambda (a) (compile-arg a bindings)) 
<a id='page-405'></a>

arg))) 
(t '(cons ,(compile-arg (first arg) bindings) 
.(compile-arg (rest arg) bindings))))) 

Now, going upward, compile-body needs to take a binding list and pass it on to 
various functions: 

(defun compile-body (body cont bindings) 
"Compile the body of a clause." 
(cond 

((null body) 
'(funcall .cont)) 

(t (let* ((goal (first body)) 
(macro (prolog-compiler-macro (predicate goal))) 
(macro-val (if macro 

(funcall macro goal (rest body) 
contbindings)))) 

(if (and macro (not (eq macro-val rpass))) 
macro-val 
(compile-cal1 

(make-predicate (predicate goal) 
(relation-arity goal)) 
(mapcar #*(lambda (arg) 
(compile-arg arg bindings)) 
(args goal)) 

(if (null (rest body)) 
cont 
'#'(lambda () 

.(compile-body 
(rest body) cont 
(bind-new-variables bindings goal)))))))))) 

The function bind -new-variables takes any variables mentioned in the goal that 
have not been bound yet and binds these variables to themselves. This is because 
the goal, whatever it is, may bind its arguments. 

(defun bind-new-variables (bindings goal) 
"Extend bindings to include any unbound variables in goal." 
(let ((variables (remove-if #'(lambda (v) (assoc . bindings)) 

(variables-in goal)))) 
(nconc (mapcar #*self-cons variables) bindings))) 

(defun self-cons (x) (cons . .)) 

One of the functions that needs to be changed to accept a binding list is the compiler 
macro for =: 

<a id='page-406'></a>

(def-prolog-compiler-macro = (goal body cont bindings) 
"Compile a goal which is a call to =. " 
(let ((args (args goal))) 

(if (/= (length args) 2) 
:pass decline to handle this goal 
(multiple-value-bind (codel bindingsl) 

(compile-unify (first args) (second args) bindings) 

(compile-if 
codel 
(compile-body body cont bindingsl)))))) 

The last step upward is to change compi 1 e-cl ause so that it starts everything off by 
passingin to comp i 1 e - body a binding list with all the parameters bound to themselves: 

(defun compile-clause (parms clause cont) 
"Transform away the head, and compile the resulting body." 
(bind-unbound-vars 

parms 

(compile-body 

(nconc 
(mapcar #*make-= parms (args (clause-head clause))) 
(clause-body clause)) 

cont 

(mapcar #'self-cons parms)))) 

Finally, we can see the fruits of our efforts: 

(DEFUN MEMBER/2 (?ARG1 ?ARG2 CONT) 
(LET ((OLD-TRAIL (FILL-POINTER *TRAIL*))) 
(IF (UNIFYl ?ARG2 (CONS ?ARG1 (?))) 

(FUNCALL CONT)) 
(UNDO-BINDINGS! OLD-TRAIL) 
(LET ((?REST (?))) 

(IF (UNIFY! ?ARG2 (CONS (?) ?REST)) 
(MEI^BER/2 ?ARG1 ?REST CONT))))) 

(DEFUN LIKES/2 (?ARG1 ?ARG2 CONT) 
(LET ((OLD-TRAIL (FILL-POINTER *TRAIL*))) 
(IF (UNIFY! ?ARG1 'ROBIN) 
(IF (UNIFY! ?ARG2 'CATS) 

(FUNCALL CONT))) 
(UNDO-BINDINGS! OLD-TRAIL) 
(IF (UNIFY! ?ARG1 'SANDY) 

(LIKES/2 ?ARG2 'CATS CONT)) 
(UNDO-BINDINGS! OLD-TRAIL) 
(IF (UNIFY! ?ARG1 'KIM) 

(LIKES/2 ?ARG2 'LEE (LAMBDA () 
(LIKES/2 ?ARG2 'KIM CONT)))))) 

<a id='page-407'></a>
12.5 Further Improvements to Unification 
Could compile-unify be improved yet again? If we insist that it call unifyl, it 
seems that it can't be made much better. However, we could improve it by in effect 
compiling unify!. This is a key idea in the Warren Abstract Machine, or WAM, 
which is the most commonly used model for Prolog compilers. 

We call uni fy! in four cases (5, 6, 7, and 10), and in each case the first argument 
is a variable, and we know something about the second argument. But the first 
thing uni fy! does is redundantly test if the first argument is a variable. We could 
eliminate unnecessary tests by calling more specialized functions rather than the 
general-purpose function uni fy!. Consider this call: 

(unify! ?arg2 (cons ?argl (?))) 

If ?arg2 is an unbound variable, this code is appropriate. But if ?arg2 is a constant 
atom, we should fail immediately, without allowing cons and ? to generate garbage. 
We could change the test to: 

(and (consp-or-variable-p ?arg2) 
(unify-first! ?arg2 ?argl) 
(unify-rest! ?arg2 (?))) 

with suitable definitions for the functions referenced here. This change should 
speed execution time and limit the amount of garbage generated. Of course, it makes 
the generated code longer, so that could slow things down if the program ends up 
spending too much time bringing the code to the processor. 

&#9635; Exercise 12.1 [h] Write definitions for consp-or-variable-p, unify-firstl, and 
uni fy - rest!, and change the compiler to generate code like that outlined previously. 
You might want to look at the function compile-rule in section 9.6, starting on 
[page 300](chapter9.md#page-300). This function compiled a call to pat-match into individual tests; now we 
want to do the same thing to uni fy!. Run some benchmarks to compare the altered 
compiler to the original version. 

&#9635; Exercise 12.2 [h] We can gain some more efficiency by keeping track of which 
variables have been dereferenced and calling an appropriate unification function: 
either one that dereferences the argument or one that assumes the argument has 
already been dereferenced. Implement this approach. 

&#9635; Exercise 12.3 [m] What code is generated for (= (f (g ?x) ?y) (f ?y (?p a)))? 

<a id='page-408'></a>

What more efficient code represents the same unification? How easy is it to change 

the compiler to get this more efficient result? 

&#9635; Exercise 12.4 [h] In retrospect, it seems that binding variables to themselves, as 
in (?argl . ?argl), was not such a good idea. It complicates the meaning of 
bindings, and prohibits us from using existing tools. For example, I had to use 
find-anywhere instead of occur-check for case 11, because occur-check expects 
a noncircular binding list. But find-anywhere does not do as complete a job as 
occur-check. Write a version of compi 1 e - uni fy that returns three values: the code, 
a noncircular binding list, and a list of variables that are bound to unknown values. 

&#9635; Exercise 12.5 [h] An alternative to the previous exercise is not to use binding lists at 
all. Instead, we could pass in a list of equivalence classes—that is, a list of lists, where 
each sublist contains one or more elements that have been unified. In this approach, 
the initial equivalence class Hst would be ((?argl) (?arg2)). After unifying ?argl 
with ?x,?arg2 with ?y, and ?x with 4, the list would be ((4 ?argl ?x) (?arg2 ?y)). 
This assumes the convention that the canonical member of an equivalence class (the 
one that will be substituted for all others) comes first. Implement this approach. 
What advantages and disadvantages does it have? 

12.6 The User Interface to the Compiler 
The compiler can translate Prolog to Lisp, but that does us no good unless we can 
conveniently arrange to compile the right Prolog relations and call the right Lisp 
functions. In other words, we have to integrate the compiler with the <- and ? 
macros. Surprisingly, we don't need to change these macros at all. Rather, we 
will change the functions these macros call. When a new clause is entered, we will 
enter the clause's predicate in the list *uncompi 1 ed*. This is a one-line addition to 
add-clause: 

(defvar *uncompiled* nil 
"Prolog symbols that have not been compiled.") 

(defun add-clause (clause) 
"Add a clause to the data base, indexed by head's predicate." 
;; The predicate must be a non-variable symbol, 
(let ((pred (predicate (clause-head clause)))) 

(assert (and (symbolp pred) (not (variable-p pred)))) 
(pushnew pred *db-predicates*) 
(pushnew pred *uncompiled*) 
(setf (get pred 'clauses) 

<a id='page-409'></a>

(nconc (get-clauses pred) (list clause))) 
pred)) 

Now when a query is made, the ?- macro expands into a call to top-level - prove. 
The Hst of goals in the query, along with the show-prol og-vars goal, is added as the 
sole clause for the relation top -1 evel - query. Next, that query, along with any others 
that are on the uncompiled list, are compiled. Finally, the newly compiled top-level 
query function is called. 

(defun top-level-prove (goals) 
"Prove the list of goals by compiling and calling it." 

First redefine top-level-query 
(clear-predicate 'top-level-query) 
(let ((vars (delete *? (variables-in goals)))) 

(add-clause *((top-level-query) 
,goals 
(show-prolog-vars ,(mapcar #'symbol-name vars) 

.vars)))) 
;; Now run it 
(run-prolog 'top-level-query/0 #'ignore) 
(format t "~&No.") 
(values)) 

(defun run-prolog (procedure cont) 
"Run a O-ary prolog procedure with a given continuation." 

First compile anything else that needs it 
(prolog-compi1e-symbols) 
;; Reset the trail and the new variable counter 
(setf (fill-pointer nrail*) 0) 
(setf *var-counter* 0) 
;; Finally, call the query 
(catch 'top-level-prove 

(funcall procedure cont))) 

(defun prolog-compi1e-symbols (&optional (symbols *uncompiled*)) 
"Compile a list of Prolog symbols. 
By default, the list is all symbols that need it." 
(mapc #'prolog-compile symbols) 
(setf *uncompiled* (set-difference *uncompiled* symbols))) 

(defun ignore (&rest args) 
(declare (ignore args)) 
nil) 

Note that at the top level, we don't need the continuation to do anything. Arbitrarily, 
we chose to pass in the function ignore, which is defined to ignore its arguments. 

<a id='page-410'></a>

This function is useful in a variety of places; some programmers will proclaim it 
inline and then use a call to i gnore in place of an ignore declaration: 

(defun third-arg (x y .) 
(ignore . y) 
.) 

The compiler's calling convention is different from the interpreter, so the primitives 
need to be redefined. The old definition of the primitive show-prol og- va rs had three 
parameters: the list of arguments to the goal, a binding list, and a list of pending 
goals. The new definition ofshow-prolog-vars/2 also has three parameters, but that 
is just a coincidence. The first two parameters are the two separate arguments to the 
goal: a list of variable names and a list of variable values. The last parameter is a 
continuation function. To continue, we call that function, but to fail, we throw to the 
catch point set up in top-1 evel - prove. 

(defun show-prolog-vars/2 (var-names vars cont) 
"Display the variables, and prompt the user to see 
if we should continue. If not, return to the top level." 
(if (null vars) 

(format t "~&Yes") 

(loop for name in var-names 
for var in vars do 
(format t "~&~a = '^a" name (deref-exp var)))) 

(if (continue-p) 
(funcall cont) 
(throw 'top-level-prove nil))) 

(defun deref-exp (exp) 
"Build something equivalent to EXP with variables dereferenced." 
(if (atom (deref exp)) 

exp 

(reuse-cons 
(deref-exp (first exp)) 
(deref-exp (rest exp)) 
exp))) 

With these definitions in place, we can invoke the compiler automatically just by 
making a query with the ? - macro. 

&#9635; Exercise 12.6 [m] Suppose you define a predicate p, which calls q, and then define 

q.In some implementations of Lisp, when you make a query like (? - (. ?x)), you 
may get a warning message like "function q/1 undef i ned" before getting the correct 
<a id='page-411'></a>

answer. The problem is that each function is compiled separately, so warnings detected 
during the compilation of p/1 will be printed right away, even if the function 
q/1 will be defined later. In ANSI Common Lisp there is a way to delay the printing 
of warnings until a series of compilations are done: wrap the compilation with the 
macro wi th - compi1 at i on - uni t. Even if your implementation does not provide this 
macro, it may provide the same functionality under a different name. Find out if 
with-compilation-unit is already defined in your implementation, or if it can be 
defined. 

12.7 Benchmarking the Compiler 
Our compiled Prolog code runs the zebra puzzle in 17.4 seconds, a 16-fold speed-up 
over the interpreted version, for a rate of 740 LIPS. 
Another popular benchmark is Lisp's reverse function, which we can code as 
the rev relation: 

(<- (rev () ())) 

(<- (rev (?x . ?a) ?b) (rev ?a ?c) (concat ?c (?x) ?b)) 

(<- (concat () ?1 ?1)) 

(<- (concat (?x . ?a) ?b (?x . ?c)) (concat ?a ?b ?c)) 

rev uses the relation concat, which stands for concatenation, (concat ?a ?b ?c)is 
true when ?a concatenated to ?b yields ?c. This relationlike name is preferred over 
more procedural names like append. But rev is very similar to the following Lisp 
definitions: 

(defun rev (1) 

(if (null 1) 
nil 
(app (rev (rest 1)) 

(list (first 1))))) 

(defun app (x y) 

(if (null X) 
y 
(cons (first x) 

(app (rest x) y)))) 

Both versions are inefficient. It is possible to write an iterative version of reverse 
that does no extra consing and is tail-recursive: 

<a id='page-412'></a>

(<- (irev ?1 ?r) (irev3 ?1 () ?r)) 

(<- (irevS (?x . ?1) ?so-far ?r) (irevS ?1 (?x ?so-far) ?r)) 
(<- (irev3 () ?r ?r)) 

The Prolog i rev is equivalent to this Lisp program: 

(defun irev (list) (irev2 list nil)) 

(defun irev2 (list so-far) 

(if (consp list) 
(irev2 (rest list) (cons (first list) so-far)) 
so-far)) 

The following table shows times in seconds to execute these routines on lists of length 
20 and 100, for both Prolog and Lisp, both interpreted and compiled. (Only compiled 
Lisp could execute rev on a 100-element list without running out of stack space.) 
Times for the zebra puzzle are also included, although there is no Lisp version of 
this program. 

Interp. Comp. Interp. Comp. 
Problem Prolog Prolog Speed-up Lisp Lisp 

zebra 278.000 17.241 16 — — 
rev 20 4.24 .208 20 .241 .0023 
rev 100 — — — — .0614 
irev 20 .22 .010 22 .028 .0005 
irev 100 9.81 .054 181 .139 .0014 

This benchmark is too small to be conclusive, but on these examples the Prolog 
compiler is 16 to 181 times faster than the Prolog interpreter, slightly faster than 
interpreted Lisp, but still 17 to 90 times slower than compiled Lisp. This suggests 
that the Prolog interpreter cannot be used as a practical programming tool, but the 
Prolog compiler can. 

Before moving on, it is interesting to note that Prolog provides for optional arguments 
automatically. Although there is no special syntax for optional arguments, an 
often-used convention is to have two versions of a relation, one with . arguments 
and one with . - 1. A single clause for the . — 1 case provides the missing, and 
therefore "optional," argument. In the following example, i rev /2 can be considered 
as a version of i rev/3 where the missing optional argument is (). 

(<- (irev ?1 ?r) (irev ?1 () ?r)) 
(<- (irev (?x . ?1) ?so-far ?r) (irev ?1 (?x ?so-far) ?r)) 
(<- (irev () ?r ?r)) 

This is roughly equivalent to the following Lisp verison: 

<a id='page-413'></a>

(defun irev (list &optional (so-far nil)) 

(if (consp list) 
(irev (rest list) (cons (first list) so-far)) 
so-far)) 

12.8 Adding More Primitives 
Just as a Lisp compiler needs machine instructions to do input/output, arithmetic, 
and the like, so our Prolog system needs to be able to perform certain primitive actions. 
For the Prolog interpreter, primitives were implemented by function symbols. When 
the interpreter went to fetch a list of clauses, if it got a function instead, it called that 
function, passing it the arguments to the current relation, the current bindings, and 
a list of unsatisfied goals. For the Prolog compiler, primitives can be installed simply 
by writing a Lisp function that respects the convention of taking a continuation as 
the final argument and has a name of the form symbol/arity. For example, here's an 
easy way to handle input and output: 

(defun read/1 (exp cont) 
(if (unify! exp (read)) 
(funcall cont))) 

(defun write/1 (exp cont) 
(write (deref-exp exp) :pretty t) 
(funcall cont)) 

Calling (write ?x) will always succeed, so the continuation will always be called. 
Similarly, one could use (read ?x) to read a value and unify it with ?x. If ?x is 
unbound, this is the same as assigning the value. However, it is also possible to make 
a call like (read (?x + ?y)), which succeeds only if the input is a three-element list 
with + in the middle. It is an easy extension to define read/2 and wr i te/2 as relations 
that indicate what stream to use. To make this useful, one would need to define 
open/2 as a relation that takes a pathname as one argument and gives a stream back 
as the other. Other optional arguments could also be supported, if desired. 

The primitive nl outputs a newline: 

(defun nl/0 (cont) (terpri) (funcall cont)) 

We provided special support for the unification predicate, =. However, we could 
have simplified the compiler greatly by having a simple definition for =/2: 

<a id='page-414'></a>

(defun =/2 (?argl ?arg2 cont) 
(if (unify! ?argl ?arg2) 
(funcall cont))) 

In fact, if we give our compiler the single clause: 

(<- (= ?x ?x)) 

it produces just this code for the definition of =/ 2. There are other equaUty predicates 
to worry about. The predicate= =/2 is more like equal in Lisp. It does no unification, 
but instead tests if two structures are equal with regard to their elements. A variable 
is considered equal only to itself. Here's an implementation: 

(defun =/2 (?argl ?arg2 cont) 
"Are the two arguments EQUAL with no unification, 
but with dereferencing? If so, succeed." 
(if (deref-equal ?argl ?arg2) 

(funcall cont))) 

(defun deref-equal (x y) 
"Are the two arguments EQUAL with no unification, 
but with dereferencing?" 
(or (eql (deref x) (deref y)) 

(and (consp x) 
(consp y) 
(deref-equal (first x) (first y)) 
(deref-equal (rest x) (rest y))))) 

One of the most important primitives is cal 1. Like funcall in Lisp, cal 1 allows us 
to build up a goal and then try to prove it. 

(defun cal 1/1 (goal cont) 
"Try to prove goal by calling it." 
(deref goal) 
(apply (make-predicate (first goal) 

(length (args goal))) 
(append (args goal) (list cont)))) 

This version of cal 1 will give a run-time error if the goal is not instantiated to a list 
whose first element is a properly defined predicate; one might want to check for that, 
and fail silently if there is no defined predicate. Here's an example of call where the 
goal is legal: 

<a id='page-415'></a>
> (?- (= ?p member) (call (?p ?x (a b c)))) 
?P = MEMBER 
?X = A; 
?P = MEMBER 
?X = B; 
?P = MEMBER 
?X = C; 
No. 

Now that we have ca 11, a lot of new things can be implemented. Here are the logical 
connectives and and or: 

(<- (or ?a ?b) (call ?a)) 

(<- (or ?a ?b) (call ?b)) 

(<- (and ?a ?b) (call ?a) (call ?b)) 

Note that these are only binary connectives, not the n-ary special forms used in Lisp. 
Also, this definition negates most of the advantage of compilation. The goals inside 
an and or or will be interpreted by cal 1, rather than being compiled. 

We can also define not, or at least the normal Prolog not, which is quite distinct 
from the logical not. In fact, in some dialects, not is written \+, which is supposed to 
be reminiscent of the logical symbol I/, that is, "can not be derived." The interpretation 
is that if goal G can not be proved, then (not G) is true. Logically, there is a difference 
between (not G) being true and being unknown, but ignoring that difference makes 
Prolog a more practical programming language. See Lloyd 1987 for more on the 
formal semantics of negation in Prolog. 

Here's an implementation of not/L Since it has to manipulate the trail, and we 

may have other predicates that will want to do the same, we'll package up what was 

done in maybe-add-undo-bindings into the macro with-undo-bindings: 

(defmacro with-undo-bindings (&body body) 
"Undo bindings after each expression in body except the last." 
(if (length=1 body) 

(first body) 

'(let ((old-trail (fill-pointer nrail*))) 
.(first body) 
.(loop for exp in (rest body) 

collect *(undo-bindings! old-trail) 
collect exp)))) 

(defun not/1 (relation cont) 
"Negation by failure: If you can't prove G. then (not G) true." 
;; Either way. undo the bindings, 
(with-undo-bindings 

(call/1 relation #'(lambda () (return-from not/1 nil))) 

(funcall cont))) 

<a id='page-416'></a>

Here's an example where not works fine: 

> (?- (member ?x (a b c)) (not (= ?x b))) 
?X = A; 
?X = C; 
No. 

Now see what happens when we simply reverse the order of the two goals: 

> (?- (not (= ?x b)) (member ?x (a b c))) 
No. 

The first example succeeds unless ?x is bound to b. In the second example, ?x is 
unbound at the start, so (= ?x b) succeeds, the not fails, and the member goal is never 
reached. So our implementation of not has a consistent procedural interpretation, 
but it is not equivalent to the declarative interpretation usually given to logical negation. 
Normally, one would expect that a and c would be valid solutions to the query, 
regardless of the order of the goals. 

One of the fundamental differences between Prolog and Lisp is that Prolog is 
relational: you can easily express individual relations. Lisp, on the other hand, is 
good at expressing collections of things as lists. So far we don't have any way of 
forming a collection of objects that satisfy a relation in Prolog. We can easily iterate 
over the objects; we just can't gather them together. The primitive bagof is one way 
of doing the collection. In general, (bagof ?x (p ?x) ?bag) unifies ?bag with a list 
of all ?x's that satisfy (. ?x). If there are no such ?x's, then the call to bagof fails. A 
bagis an unordered collection with duplicates allowed. For example, the bag {a, 6, a} 
is the same as the bag {a, a, 6}, but different from {a, 6}. Bags stands in contrast to 
sets, which are unordered collections with no duplicates. The set {a, 6} is the same 
as the set {6, a}. Here is an implementation of bagof: 

(defun bagof/3 (exp goal result cont) 
"Find all solutions to GOAL, and for each solution, 
collect the value of EXP into the list RESULT." 
;; Ex: Assume (p 1) (p 2) (p 3). Then: 
;: (bagof ?x (p ?x) ?1) => ?1= (1 2 3) 
(let ((answers nil)) 

(call/1 goal #'(lambda () 
(push (deref-copy exp) answers))) 
(if (and (not (null answers)) 
(unify! result (nreverse answers))) 
(funcall cont)))) 

<a id='page-417'></a>
(defun deref-copy (exp) 
"Copy the expression, replacing variables with new ones. 
The part without variables can be returned as is. " 
(sublis (mapcar #'(lambda (var) (cons (deref var) (?)) 

(unique-find-anywhere-if #'var-p exp)) 
exp)) 

Below we use bagof to collect a list of everyone Sandy likes. Note that the result is a 
bag, not a set: Sandy appears more than once. 

> (?- (bagof ?who (likes Sandy ?who) ?bag)) 
?WHO = SANDY 

?BAG = (LEE KIM ROBIN SANDY CATS SANDY); 

No. 

In the next example, we form the bag of every list of length three that has A and . as 
members: 

> (?- (bagof ?1 (and (length ?1 (1+ (1+ (1+ 0)))) 
(and (member a ?1) (member b ?1))) 

?bag)) 
?L = (?5 ?8 ?11 ?68 ?66) 
?BAG = ((A . ?17) (A ?21 B) (B A ?31) (?38 A B) (B ?48 A) (?52 . A)) 
No. 

Those who are disappointed with a bag containing multiple versions of the same 
answer may prefer the primitive setof, which does the same computation as bagof 
but then discards the duplicates. 

(defun setof/3 (exp goal result cont) 
"Find all unique solutions to GOAL, and for each solution, 
collect the value of EXP into the list RESULT." 
;; Ex: Assume (p 1) (p 2) (p 3). Then: 
;; (setof ?x (p ?x) ?1) = > ?1 = (1 2 3) 
(let ((answers nil)) 

(call/1 goal #'(lambda () 
(push (deref-copy exp) answers))) 
(if (and (not (null answers)) 

(unify! result (delete-duplicates 
answers 
:test #*deref-equal))) 

(funcall cont)))) 

Prolog supports arithmetic with the operator is. For example, (Is ?x (+ ?y 1)) 
unifies ?x with the value of ?y plus one. This expression fails if ?y is unbound, and it 

<a id='page-418'></a>

gives a run-time error if ?y is not a number. For our version of Prolog, we can support 
not just arithmetic but any Lisp expression: 

(defun is/2 (var exp cont) 
Example: (is ?x (+ 3 (* ?y (+ ?z 4)))) 
Or even: (is (?x ?y ?x) (cons (first ?z) ?1)) 

(if (and (not (find-if-anywhere #*unbound-var-p exp)) 
(unify 1 var (eval (deref-exp exp)))) 
(funcall cont))) 

(defun unbound-var-p (exp) 
"Is EXP an unbound var?" 
(and (var-p exp) (not (bound-p exp)))) 

As an aside, we might as well give the Prolog programmer access to the function 
unbound -var-p. The standard name for this predicate is va r/1: 

(defun var/1 (?argl cont) 
"Succeeds if ?argl is an uninstantiated variable." 
(if (unbound-var-p ?argl) 

(funcall cont))) 

The is primitive fails if any part of the second argument is unbound. However, there 
are expressions with variables that can be solved, although not with a direct call to 
eval. For example, the following goal could be solved by binding ?x to 2: 

(solve (= 12 (* (+ ?x 1) 4))) 

We might want to have more direct access to Lisp from Prolog. The problem with 
is is that it requires a check for unbound variables, and it calls eval to evaluate 
arguments recursively. In some cases, we just want to get at Lisp's apply, without 
going through the safety net provided by i s. The primitive lisp does that. Needless 
to say,1 i sp is not a part of standard Prolog. 

(defun lisp/2 (?result exp cont) 
"Apply (first exp) to (rest exp), and return the result." 
(if (and (consp (deref exp)) 

(unify! ?result (apply (first exp) (rest exp)))) 
(funcall cont))) 

&#9635; Exercise 12.7 [m] Define the primitive solve/1, which works like the function 
sol ve used in student ([page 225](chapter7.md#page-225)). Decide if it should take a single equation as 
argument or a list of equations. 

<a id='page-419'></a>
&#9635; Exercise 12.8 [h] Assumewehadagoalof the form (solve (= 12 (* (+ ?x 1) 
4))). Rather than manipulate the equation when sol ve/1 is called at run time, we 
might prefer to do part of the work at compile time, treating the call as if it were 
(solve (= ?x 2)). Write a Prolog compiler macro for sol ve. Notice that even when 
you have defined a compiler macro, you still need the underlying primitive, because 
the predicate might be invoked through a cal 1 / I. The same thing happens in Lisp: 
even when you supply a compiler macro, you still need the actual function, in case 
of a funcall or apply. 

&#9635; Exercise 12.9 [h] Which of the predicates call, and, or, not, or repeat could 
benefit from compiler macros? Write compiler macros for those predicates that 
could use one. 

&#9635; Exercise 12.10 [m] You might have noticed that ca 11 /1 is inefficient in two important 
ways. First, it calls make-predi cate, which must build a symbol by appending 
strings and then look the string up in the Lisp symbol table. Alter make-predi cate 
to store the predicate symbol the first time it is created, so it can do a faster lookup 
on subsequent calls. The second inefficiency is the call to append. Change the whole 
compiler so that the continuation argument comes first, not last, thus eliminating 
the need for append in cal 1. 

&#9635; Exercise 12.11 [s] The primitive true/0 always succeeds, and f a i 1 /O always fails. 
Define these primitives. Hint: the first corresponds to a Common Lisp function, and 
the second is a function already defined in this chapter. 

&#9635; Exercise 12.12 [s] Would it be possible to write = =/ 2 as a list of clauses rather than 
as a primitive? 

&#9635; Exercise 12.13 [m] Write a version of deref - copy that traverses the argument expression 
only once. 

<a id='page-420'></a>

12.9 The Cut 
In Lisp, it is possible to write programs that backtrack explicitly, although it can 
be awkward when there are more than one or two backtrack points. In Prolog, 
backtracking is automatic and implicit, but we don't yet know of any way to avoid 
backtracking. There are two reasons why a Prolog programmer might want to disable 
backtracking. First, keeping track of the backtrack points takes up time and space. 
A programmer who knows that a certain problem has only one solution should be 
able to speed up the computation by telling the program not to consider the other 
possible branches. Second, sometimes a simple logical specification of a problem 
will yield redundant solutions, or even some unintended solutions. It may be that 
simply pruning the search space to eliminate some backtracking will yield only 
the desired answers, while restructuring the program to give all and only the right 
answers would be more difficult. Here's an example. Suppose we wanted to define 
a predicate, max/3, which holds when the third argument is the maximum of the 
first two arguments, where the first two arguments will always be instantiated to 
numbers. The straightforward definition is: 

(<- (max ?x ?y ?x) (>= ?x ?y)) 
(<- (max ?x ?y ?y) ??x ?y)) 

Declaratively, this is correct, but procedurally it is a waste of time to compute the < 
relation if the >= has succeeded: in that case the < can never succeed. The cut symbol, 
written !, can be used to stop the wasteful computation. We could write: 

(<- (max ?x ?y ?x) (>= ?x ?y) !) 
(<- (max ?x ?y ?y)) 

The cut in the first clause says that if the first clause succeeds, then no other clauses 
will be considered. So now the second clause can not be interpreted on its own. 
Rather, it is interpreted as "if the first clause fails, then the max of two numbers is the 
second one." 

In general, a cut can occur anywhere in the body of a clause, not just at the end. 
There is no good declarative interpretation of a cut, but the procedural interpretation 
is two-fold. First, when a cut is "executed" as a goal, it always succeeds. But in 
addition to succeeding, it sets up a fence that cannot be crossed by subsequent 
backtracking. The cut serves to cut off backtracking both from goals to the right of 
the cut (in the same clause) and from clauses below the cut (in the same predicate). 
Let's look at a more abstract example: 

(<- (p) (q) (r) ! (s) (t)) 
(<- (p) (s)) 

<a id='page-421'></a>

In processing the first clause of p, backtracking can occur freely while attempting 
to solve q and r. Once r is solved, the cut is encountered. From that point on, 
backtracking can occur freely while solving s and t, but Prolog will never backtrack 
past the cut into r, nor will the second clause be considered. On the other hand, if 
q or . failed (before the cut is encountered), then Prolog would go on to the second 
clause. 

Now that the intent of the cut is clear, let's think of how it should be implemented. 
We'll look at a slightly more complex predicate, one with variables and multiple cuts: 

(<- (p ?x a) ! (q ?x)) 

(<- (p ?x b) (r ?x) ! (s ?x)) 

We have to arrange it so that as soon as we backtrack into a cut, no more goals 
are considered. In the first clause, when q/1 fails, we want to return from p/2 
immediately, rather than considering the second clause. Similarly, the first time s /1 
fails, we want to return from p/2, rather than going on to consider other solutions to 
r/1. Thus, we want code that looks something like this: 

(defun p/2 (argl arg2 cont) 
(let ((old-trail (fil 1-pointer nrail*))) 
(if (unify! arg2 'a) 
(progn (q/1 argl cont) 

(return-from p/2 nil))) 
(undo-bindings! old-trail) 
(if (unify! arg2 'b) 

(r/1 argl #'(lambda () 
(progn (s/1 argl cont) 
(return-from p/2 nil))))))) 

We can get this code by making a single change to compi 1 e - body: when the first goal 

in a body (or what remains of the body) is the cut symbol, then we should generate a 

progn that contains the code for the rest of the body, followed by a return-from the 

predicate being compiled. Unfortunately, the name of the predicate is not available 

to compi 1 e-body. We could change compile-clause and compi 1 e-body to take the 

predicate name as an extra argument, or we could bind the predicate as a special 

variable in compi 1 e-predi cate. I choose the latter: 

(defvar ^predicate* nil 
"The Prolog predicate currently being compiled") 

<a id='page-422'></a>

(defun compile-predicate (symbol arity clauses) 
"Compile all the clauses for a given symbol/arity 
into a single LISP function." 
(let ((^predicate* (make-predicate symbol arity)) 

(parameters (make-parameters arity))) 
(compile 
(eval 
'(defun ,*predicate* (.parameters cont) 
(maybe-add-undo-bindings 
(mapcar #*(lambda (clause) 
(compile-clause parameters 
clause *cont)) 
clauses))))))) 

(defun compile-body (body cont bindings) 
"Compile the body of a clause." 
(cond 

((null body) 
'(funcall .cont)) 
((eq (first body) .) 
'(progn .(compile-body (rest body) cont bindings) 
(return-from .*predicate* nil))) 

(t (let* ((goal (first body)) 
(macro (prolog-compiler-macro (predicate goal))) 
(macro-val (if macro 

(funcall macro goal (rest body) 
contbindings)))) 

(if (and macro (not (eq macro-val .-pass))) 
macro-val 
'(.(make-predicate (predicate goal) 

(relation-arity goal)) 
.(mapcar #'(lambda (arg) 
(compile-arg arg bindings)) 
(args goal)) 

.(if (null (rest body)) 
cont 
'#*(lambda () 

.(compile-body 
(rest body) cont 
(bind-new-variables bindings goal)))))))))) 

&#9635; Exercise 12.14 [m] Given the definitions below, figure out what a call to test - cut 
will do, and what it will write: 

(<- (test-cut) (p a) (p b) ! (p c) (p d)) 
(<- (test-cut) (p e)) 

<a id='page-423'></a>
(<- (p ?x) (write (?x 1))) 
(<- (p ?x) (write (?x 2))) 

Another way to use the cut is in a repeat/fail loop. The predicate repeat is defined 
with the following two clauses: 

(<- (repeat)) 
(<- (repeat) (repeat)) 

An alternate definition as a primitive is: 

(defun repeat/0 (cont) 
(loop (funcall cont))) 

Unfortunately, repeat is one of the most abused predicates. Several Prolog books 
present programs like this: 

(<- (main) 
(write "Hello.") 
(repeat) 
(write "Command: ") 
(read ?command) 
(process ?command) 
(= ?command exit) 
(write "Good bye.")) 

The intent is that commands are read one at a time, and then processed. For each 
command except exit, process takes the appropriate action and then fails. This 
causes a backtrack to the repeat goal, and a new command is read and processed. 
When the command is ex i t, the procedure returns. 

There are two reasons why this is a poor program. First, it violates the principle of 
referential transparency. Things that look alike are supposed to be alike, regardless 
of the context in which they are used. But here there is no way to tell that four of the six 
goals in the body comprise a loop, and the other goals are outside the loop. Second, 
it violates the principle of abstraction. A predicate should be understandable as a 
separate unit. But here the predicate process can only be understood by considering 
the context in which it is called: a context that requires it to fail after processing each 
command. As Richard O'Keefe 1990 points out, the correct way to write this clause 
is as follows: 

<a id='page-424'></a>

(<- (main) 
(write "Hello.") 
(repeat) 

(write "Command: ") 
(read ?command) 
(process ?command) 
(or (= ?command exit) (fail)) 

(write "Good bye.")) 

The indentation clearly indicates the limits of the repeat loop. The loop is terminated 
by an explicit test and is followed by a cut, so that a calling program won't accidently 
backtrack into the loop after it has exited. Personally, I prefer a language like Lisp, 
where the parentheses make constructs like loops explicit and indentation can be 
done automatically. But O'Keefe shows that well-structured readable programs can 
be written in Prolog. 

The if-then and if-then-else constructions can easily be written as clauses. Note 
that the if-then-else uses a cut to commit to the then part if the test is satisfied. 

(<- (if ?test ?then) (if ?then ?else (fail))) 

(<- (if ?test ?then ?else) 
(call ?test) 

(call ?then)) 

(<- (if ?test ?then ?else) 
(call ?else)) 

The cut can be used to implement the nonlogical not. The following two clauses are 
often given before as the definition of not. Our compiler succesfuUy turns these two 
clauses into exactly the same code as was given before for the primitive not/1: 

(<- (not ?p) (call ?p) I (fail)) 
(<- (not ?p)) 

12.10 '^ear Prolog 
The Prolog-In-Lisp system developed in this chapter uses Lisp syntax because it is 
intended to be embedded in a Lisp system. Other Prolog implementations using 
Lisp syntax include micro-Prolog, Symbolics Prolog, and LMI Prolog. 

<a id='page-425'></a>
However, the majority of Prolog systems use a syntax closer to traditional mathematical 
notation. The following table compares the syntax of "standard" Prolog to 
the syntax of Prolog-In-Lisp. While there is currently an international committee 
working on standardizing Prolog, the final report has not yet been released, so different 
dialects may have slightly different syntax. However, most implementations 
follow the notation summarized here. They derive from the Prolog developed at the 
University of Edinburgh for the DEC-10 by David H. D. Warren and his colleagues. 
The names for the primitives in the last section are also taken from Edinburgh Prolog. 

Prolog Prolog-In-Lisp 
atom lower const 
variable Upper ?var 
anonymous -? 
goal p(Var,const) (p ?var const) 
rule p(X) q(X). (<- (p ?x) (q ?x)) 
fact p(a). (<- (p a)) 
query ?- p(X). (?- (p ?x)) 
list [a.b.c] (a b c) 
cons [a 1 Rest] (a . ?rest) 
nil [] () 
and p(X), q(X) (and (p ?x) (q ?x)) 
or p(X): q(X) (or (p ?x) (q ?x)) 
not \+ p(X) (not (p ?x)) 

We have adopted Lisp's bias toward lists; terms are built out of atoms, variables, 
and conses of other terms. In real Prolog cons cells are provided, but terms are 
usually built out of structures, not lists. The Prolog term p(a,b) corresponds to the 
Lisp vector #( p/2 a b), not the list (. a b). A minority of Prolog implementations 
use structure sharing. In this approach, every non-atomic term is represented by 
a skeleton that contains place holders for variables and a header that points to the 
skeleton and also contains the variables that will fill the place holders. With structure 
sharing, making a copy is easy: just copy the header, regardless of the size of the 
skeleton. However, manipulating terms is complicated by the need to keep track of 
both skeleton and header. See Boyer and Moore 1972 for more on structure sharing. 

Another major difference is that real Prolog uses the equivalent of failure contin


uations, not success continuations. No actual continuation, in the sense of a closure, 

is built. Instead, when a choice is made, the address of the code for the next choice 

is pushed on a stack. Upon failure, the next choice is popped off the stack. This is 

reminiscent of the backtracking approach using Scheme's cal 1 /cc facility outlined 

on [page 772](chapter22.md#page-772). 

<a id='page-426'></a>

&#9635; Exercise 12.15 [m] Assuming an approach using a stack of failure continuations 
instead of success continuations, show what the code for . and member would look 
like. Note that you need not pass failure continuations around; you can just push 
them onto a stack that top-1 evel - prove will invoke. How would the cut be implemented? 
Did we make the right choice in implementing our compiler with success 
continuations, or would failure continuations have been better? 

12.11 History and References 
As described in chapter 11, the idea of logic programming was fairly well understood 
by the mid-1970s. But because the implementations of that time were slow, logic 
programming did not catch on. It was the Prolog compiler for the DEC-10 that made 
logic programming a serious alternative to Lisp and other general-purpose languages. 
The compiler was developed in 1977 by David H. D. Warren with Fernando Pereira 
and Luis Pereira. See the paper by Warren (1979) and by all three (1977). 

Unfortunately, David H. D. Warren's pioneering work on compiling Prolog has 
never been published in a widely accessible form. His main contribution was the 
description of the Warren Abstract Machine (WAM), an instruction set for compiled 
Prolog. Most existing compilers use this instruction set, or a slight modification 
of it. This can be done either through byte-code interpretation or through macro-
expansion to native machine instructions. Ait-Kaci 1991 provides a good tutorial on 
the WAM, much less terse than the original (Warren 1983). The compiler presented in 
this chapter does not use the WAM. Instead, it is modeled after Mark Stickel's (1988) 
theorem prover. A similar compiler is briefly sketched by Jacques Cohen 1985. 

12.12 Exercises 
&#9635; Exercise 12.16 [m] Change the Prolog compiler to allow implicit calls. That is, if 
a goal is not a cons cell headed by a predicate, compile it as if it were a cal 1. The 
clause: 

(<- (p ?x ?y) (?x c) ?y) 

should be compiled as if it were: 

(<- (p ?x ?y) (call (?x c)) (call ?y)) 

<a id='page-427'></a>
&#9635; Exercise 12.17 [h] Here are some standard Prolog primitives: 

* get/1 Read a single character and unify it with the argument. 
* put/1 Print a single character. 
* nonvar/1, /=, /==Theoppositesof var, = and==, respectively. 
* i nteger/1 True if the argument is an integer. 
* atom/1 True if the argument is a symbol (like Lisp's symbol p). 
* atomi c/1 True if the argument is a number or symbol (like Lisp's atom). 
.<,>,=<,> = Arithmetic comparison; succeeds when the arguments are both 
instantiated to numbers and the comparison is true. 

* listing/0 Print out the clauses for all defined predicates. 
* 1 i sti ng/1 Print out the clauses for the argument predicate. 
Implement these predicates. In each case, decide if the predicate should be 
implemented as a primitive or a list of clauses, and if it should have a compiler 
macro. 

There are some naming conflicts that need to be resolved. Terms like atom have 
one meaning in Prolog and another in Lisp. Also, in Prolog the normal notation is \ = 
and \==, not / = and /==. For Prolog-In-Lisp, you need to decide which notations to 
use: Prolog's or Lisp's. 

&#9635; Exercise 12.18 [s] In Lisp, we are used to writing n-ary calls like (< 1 . 10)or(= 
. y .). Write compiler macros that expand n-ary calls into a series of binary calls. 
Forexample, (< 1 . 10) should expand into (and (< 1 n) (< . 10)). 

&#9635; Exercise 12.19 [m] One feature of Lisp that is absent in Prolog is the quote mechanism. 
Is there a use for quote? If so, implement it; if not, explain why it is not 
needed. 

&#9635; Exercise 12.20 [h] Write a tracing mechanism for Prolog. Add procedures . -1 ra ce 
and p-untrace to trace and untrace Prolog predicates. Add code to the compiler to 
generate calls to a printing procedure for goals that are traced. In Lisp, we have to 
trace procedures when they are called and when they return. In Prolog, there are 
four cases to consider: the call, successful completion, backtrack into subsequent 
clauses, and failure with no more clauses. We will call these four cases cal 1, exi t. 

<a id='page-428'></a>

redo, and f ai 1 , respectively. If we traced member, we would expect tracing output to 
look something like this: 

> (? - (member ?x (a b c d)) (fail)) 
CALL MEMBER: ?1 (A . C D) 
EXIT MEMBER: A (A . C D) 
REDO MEMBER: ?1 (A . C D) 

CALL MEMBER: 11 (B C D) 

EXIT MEMBER: . (B C D) 

REDO MEMBER: ?1 (B C D) 

CALL MEMBER: 11 (C D) 
EXIT MEMBER: C (C D) 
REDO MEMBER: ?1 (C D) 

CALL MEMBER: 11 (D) 
EXIT MEMBER: D (D) 
REDO MEMBER: ?1 (D) 

CALL MEMBER: 11 NIL 
REDO MEMBER: 11 NIL 
FAIL MEMBER: 11 NIL 

FAIL MEMBER: 11 (D) 
FAIL MEMBER: 11 (C D) 
FAIL MEMBER: 11 (B C D) 
FAIL MEMBER: ?1 (A . C D) 
No. 

&#9635; Exercise 12.21 [m] Some Lisp systems are very slow at compiling functions. KCL 
is an example; it compiles by translating to C and then calling the C compiler and 
assembler. In KCL it is best to compile only code that is completely debugged, and 
run interpreted while developing a program. 
Alter the Prolog compiler so that calling the Lisp compiler is optional. In all cases, 
Prolog functions are translated into Lisp, but they are only compiled to machine 
language when a variable is set. 

&#9635; Exercise 12.22 [d] Some Prolog systems provide the predicate freeze to "freeze" a 
goal until its variables are instantiated. For example, the goal (freeze . (> . 0)) 
is interpreted as follows: if x is instantiated, then just evaluate the goal (> . 0), and 
succeed or fail depending on the result. However, if . is unbound, then succeed and 
continue the computation, but remember the goal (> . 0) and evaluate it as soon as 
X becomes instantiated. Implement freeze. 

&#9635; Exercise 12.23 [m] Write a recursive version of anonymous - va r i abl es -1. that does 
not use a local function. 

<a id='page-429'></a>

12.13 Answers 
Answer 12.6 Here's a version that works for Texas Instruments and Lucid implementations: 


(defmacro with-compilation-unit (options &body body) 
"Do the body, but delay compiler warnings until the end." 
This is defined in Common Lisp the Language, 2nd ed. 

*(.(read-time-case 
#+TI * compi1 er:compi1er-warni ngs-context-bi nd 
#+Lucid *with-deferred-warnings 

'progn) 
..body)) 

(defun prolog-compile-symbols (&optional (symbols *uncompiled*)) 
"Compile a list of Prolog symbols. 
By default, the list is all symbols that need it." 
(with-compilation-unit () 

(mapc #*prolog-compile symbols) 

(setf *uncompiled* (set-difference *uncompiled* symbols)))) 

Answer 12.9 Macros for and and or are very important, since these are commonly 
used. The macro for and is trivial: 

(def-prolog-compiler-macro and (goal body cont bindings) 
(compile-body (append (args goal) body) cont bindings)) 

The macro for or is trickier: 

(def-prolog-compiler-macro or (goal body cont bindings) 
(let ((disjuncts (args goal))) 

(case (length disjuncts) 
(0 fail) 
(1 (compile-body (cons (first disjuncts) body) cont bindings)) 
(t (let ((fn (gensym "F"))) 

'(flet ((,fn () ,(compile-body body cont bindings))) 
.,(maybe-add-undo-bindings 
(loop for g in disjuncts collect 
(compile-body (list g) *#',fn 
bindings))))))))) 

<a id='page-430'></a>

Answer 12.11 true /0 is funcall: when a goal succeeds, we call the continuation, 
fai 1 /O is i gnore: when a goal fails, we ignore the continuation. We could also define 
compiler macros for these primitives: 

(def-prolog-compi1er-macro true (goal body cont bindings) 
(compile-body body cont bindings)) 

(def-prolog-compiler-macro fail (goal body cont bindings) 
(declare (ignore goal body cont bindings)) 
nil) 

Answer 12.13 

(defun deref-copy (exp) 
"Build a copy of the expression, which may have variables. 
The part without variables can be returned as is. " 
(let ((var-alist nil)) 

(labels 

((walk (exp) 
(deref exp) 
(cond ((consp exp) 

(reuse-cons (walk (first exp)) 
(walk (rest exp)) 
exp)) 

((var-p exp) 
(let ((entry (assoc exp var-alist))) 

(if (not (null entry)) 
(cdr entry) 
(let ((var-copy (?))) 

(push (cons exp var-copy) var-alist) 
var-copy)))) 
(t exp)))) 
(walk exp)))) 

<a id='page-431'></a>
Answer 12.14 In the first clause of test - cut, all four calls to . will succeed via the 
first clause of p. Then backtracking will occur over the calls to (. c) and (. d). All 
four combinations of 1 and 2 succeed. After that, backtracking would normally go 
back to the call to (p b). But the cut prevents this, and the whole (test-cut) goal 
fails, without ever considering the second clause. Here's the actual output: 

(?- (test-cut)) 

(A 1)(B 1)(C 1)(D 1) 

Yes; 

(D 2) 

Yes; 

(C 2)(D 1) 

Yes; 

(D 2) 

Yes; 

No. 

Answer 12.17 Forexample: 

(defun >/2 (x y cont) 
(if (and (numberp (deref x)) (numberp (deref y)) (> . y)) 
(funcall cont))) 

(defun numberp/1 (x cont) 
(if (numberp (deref x)) 
(funcall cont))) 

Answer 12.19 Lisp uses quote in two ways: to distinguish a symbol from the value 
of the variable represented by that symbol, and to distinguish a literal list from the 
value that would be returned by evaluating a function call. The first distinction Prolog 
makes by a lexical convention: variables begin with a question mark in our Prolog, 
and they are capitalized in real Prolog. The second distinction is not necessary 
because Prolog is relational rather than functional. An expression is a goal if it is a 
member of the body of a clause, and is a literal if it is an argument to a goal. 

<a id='page-432'></a>

Answer 12.20 Hint: Here's how member could be augmented with calls to a procedure, 
pro! og-trace, which will print information about the four kinds of tracing 
events: 

(defun member/2 (?argl ?arg2 cont) 
(let ((old-trail (fill-pointer nrail*)) 

(exit-cont #*(lambda () 
(prolog-trace 'exit 'member ?argl ?arg2 ) 
(funcall cont)))) 

(prolog-trace 'call 'member ?argl ?arg2) 
(if (unify! ?arg2 (cons ?argl (?))) 

(funcall exit-cont)) 
(undo-bindings! old-trail) 
(prolog-trace 'redo 'member ?argl ?arg2) 
(let ((?rest (?))) 

(if (unify! ?arg2 (cons (?) ?rest)) 
(member/2 ?argl ?rest exit-cont))) 
(prolog-trace 'fail 'member ?argl ?arg2))) 

The definition of prol og-trace is: 

(defvar *prolog-trace-indent* 0) 

(defun prolog-trace (kind predicate &rest args) 
(if (member kind '(call redo)) 
(incf *prolog-trace-indent* 3)) 
(format t "~rvra ~a:~{ ~a~}" 
*prolog-trace-indent* kind predicate args) 
(if (member kind '(fail exit)) 
(decf *prolog-trace-indent* 3))) 

<a id='page-433'></a>
Answer 12.23 

(defun anonymous-variables-in (tree) 
"Return a list of all variables that occur only once in tree." 
(values (anon-vars-in tree nil nil))) 

(defun anon-vars-in (tree seen-once seen-more) 
"Walk the data structure TREE, returning a list of variables 
seen once, and a list of variables seen more than once." 
(cond 
((consp tree) 
(multiple-value-bind (new-seen-once new-seen-more) 
(anon-vars-in (first tree) seen-once seen-more) 

(anon-vars-in (rest tree) new-seen-once new-seen-more))) 
((not (variable-p tree)) (values seen-once seen-more)) 
((member tree seen-once) 

(values (delete tree seen-once) (cons tree seen-more))) 
((member tree seen-more) 
(values seen-once seen-more)) 
(t (values (cons tree seen-once) seen-more)))) 

