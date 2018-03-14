# Chapter 25 {docsify-ignore}
<a id='page-866'></a>

Troubleshooting 

Perhaps if we wrote programs from childhood on, 
as adults we'd be able to read them. 

- Alan Peril's 

w 
w 
hen you buy a new appUance such as a television, it comes with an instruction 
booklet that lists troubleshooting hints in the following form: 

PROBLEM: Nothing works. 

Diagnosis: Power is off. 

Remedy: Plug in outlet and turn on power switch. 

If your Lisp compiler came without such a handy instruction booklet, this chapter may be of 
some help. It lists some of the most common difficulties that Lisp programmers encounter. 

<a id='page-867'></a>

25.1 Nothing Happens 
PROBLEM: You type an expression to Lisp's read-eval-print loop and get no response - 
no result, no prompt. 

Diagnosis: There are two likely reasons why output wasn't printed: either Lisp is still 
doing read or it is still doing eval. These possibilities can be broken down further 
into four cases: 

Diagnosis: If the expression you type is incomplete. Lisp will wait for more input 
to complete it. An expression can be incomplete because you have left off a right 
parenthesis (or inserted an extra left parenthesis). Or you may have started a string, 
atom, or comment without finishing it. This is particularly hard to spot when the error 
spans multiple lines. A string begins and ends with double-quotes: "string"; an 
atom containing unusual characters can be delimited by vertical bars: I AN ATOM I; 
and a comment can be of the form # I a comment I #. Here are four incomplete 
expressions: 

(+ (* 3 (sqrt 5) 1) 
(format t "~&X=''a, Y=~a. . y) 
(get .strange-atom 'prop) 
(if (= X 0) #1 test if X is zero 

y 

X) 

Remedy: Add a ), ", I, and I #, respectively. Or hit the interrupt key and type the 
input again. 

Diagnosis: Your program may be waiting for input. 

Remedy: Never do a (read) without first printing a prompt of some kind. If the 
prompt does not end with a newline, a call to f i ni sh-output is also in order. In fact, 
it is a good idea to call a function that is at a higher level than read. Several systems 
define the function prompt-and- read. Here is one version: 

(defun prompt-and-read (ctl-string &rest args) 
"Print a prompt and read a reply." 
(apply #'format t ctl-string args) 
(finish-output) 
(read)) 

Diagnosis: The program may be caught in an infinite loop, either in an explicit 1 oop 
or in a recursive function. 

<a id='page-868'></a>

Remedy: Interrupt the computation, get a back trace, and see what functions are 
active. Check the base case and loop variant on active functions and loops. 

Diagnosis: Even a simple expression like (mapc #'sqrt 1 ist) or (length list) 
will cause an infinite loop if 1 i st is an infinite list - that is, a list that has some tail 
that points back to itself. 

Remedy: Be very careful any time you modify a structure with nconc, del ete, setf, 
and so forth. 

PROBLEM: You get a new prompt from the read-eval-print loop, but no output was 
printed. 

Diagnosis: The expression you evaluated must have returned no values at all, that 
is, the resuh (values). 

25.2 Change to Variable Has No Effect 
PROBLEM: You redefined a variable, but the new value was ignored. 

Diagnosis: Altering a variable by editing and re-evaluating a defvar form will not 
change the variable's value, def va r only assigns an initial value when the variable is 
unbound. 

Remedy: Use setf to update the variable, or change the defvar to a defparameter. 

Diagnosis: Updating a locally bound variable will not affect a like-named variable 
outside that binding. For example, consider: 

(defun check-ops (*ops*) 
(if (null *ops*) 
(setf *ops* *default-ops*)) 
(mapcar #'check-op *ops*)) 

If check - ops is called with a null argument, the *ops* that is a parameter of check - ops 
will be updated, but the global *ops* will not be, even if it is declared special. 

Remedy: Don't shadow variables you want to update. Use a different name for the 
local variable. It is important to distinguish special and local variables. Stick to the 
naming convention for special variables: they should begin and end with asterisks. 
Don't forget to introduce a binding for all local variables. The following excerpt from 
a recent textbook is an example of this error: 

<a id='page-869'></a>
(defun test () 
(setq X 'test-data) ; Warning! 
(solve-probl em x)) ; Don't do this. 

This function should have been written: 

(defun test () 
(let ((x 'test-data)) ; Do this instead. 
(solve-problem x))) 

25.3 Change to Function Has No Effect 
PROBLEM: You redefined a function, but the change was ignored. 

Diagnosis: When you change a macro, or a function that has been declared inline, 
the change will not necessarily be seen by users of the changed function. (It depends 
on the implementation.) 

Remedy: Recompile after changing a macro. Don't use inline functions until everything 
is debugged. (Use (declare (not i nl i ne f)) to cancel an inline declaration). 

Diagnosis: If you change a normal (non-inline) function, that change will be seen by 
code that refers to the function by name, but not by code that refers to the old value 
of the function itself. Consider: 

(defparameter *scorer* #'score-fn) 

(defparameter *printer* *print-fn) 

(defun show (values) 

(funcall Sprinter* 
(funcall *scorer* values) 
(reduce #'better values))) 

Now suppose that the definitions of score - fn, pri nt -f n, and better are all changed. 
Does any of the prior code have to be recompiled? The variable *pri nter* can stay 
as is. When it is funcalled, the symbol pri nt-f . will be consulted for the current 
functional value. Within show, the expression # ' better is compiled into code that 
will get the current version of bette r, so it too is safe. However, the variable *s co r e r* 
must be changed. Its value is the old definition of score - fn. 

Remedy: Re-evaluate the definition of *scorer*. It is unfortunate, but this problem 
encourages many programmers to use symbols where they really mean functions. 
Symbols will be coerced to the global function they name when passed to funcall 

<a id='page-870'></a>

or apply, but this can be the source of another error. In the following example, the 
symbol local - fn will not refer to the locally bound function. One needs to use 
#'local - fn to refer to it. 

(flat (docal-fn (x) ...)) 
(mapcar *local-fn list)) 

Diagnosis: If you changed the name of a function, did you change the name everywhere? 
For example, if you decide to change the name of pr 1 nt - f . to pr i nt - function 
but forget to change the value of *pri nter*, then the old function will be called. 

Remedy: Use your editor's global replace command. To be even safer, redefine 
obsolete functions to call error. The following function is handy for this purpose: 

(defun make-obsolete (fn-name) 
"Print an error if an obsolete function is called." 
(setf (symbol-function fn-name) 

#*(lambda (&rest args) 
(declare (ignore args)) 
(error "Obsolete function.")))) 

Diagnosis: Are you using 1 abel s and f 1 et properly? Consider again the function 
repl ace-?-vars, which was defined in section 11.3 to replace an anonymous logic 
variable with a unique new variable. 

(defun replace-?-vars (exp) 
"Replace any ? within exp with a var of the form ?123." 
(cond ((eq exp *?) (gensym "?")) 

((atom exp) exp) 
(t (cons (replace-?-vars (first exp)) 
(replace-?-vars (rest exp)))))) 

It might occur to the reader that gensyming a different variable each time is wasteful. 
The variables must be unique in each clause, but they can be shared across clauses. 
So we could generate variables in the sequence ?1, ?2, intern them, and thus 
reuse these variables in the next clause (provided we warn the user never to use 
such variable names). One way to do that is to introduce a local variable to hold the 
variable number, and then a local function to do the computation: 

<a id='page-871'></a>

(defun replace-?-vars (exp) 
"Replace any ? within exp with a var of the form ?123." 
.. Buggy Version *** 
(let ((n 0)) 

(flet 
((replace-?-vars (exp) 

(cond ((eq exp *?) (symbol '? (incf n))) 
((atom exp) exp) 
(t (cons (replace-?-vars (first exp)) 

(replace-?-vars (rest exp))))))) 
(replace-?-vars exp)))) 

This version doesn't work. The problem is that f 1 et, like 1 et, defines a new function 
within the body of the f 1 et but not within the new function's definition. So two 
lessons are learned here: use 1 abel s instead of f 1 et to define recursive functions, 
and don't shadow a function definition with a local definition of the same name (this 
second lesson holds for variables as well). Let's fix the problem by changing 1 abel s 
tof 1 et and naming the local function recurse: 

(defun replace-?-vars (exp) 
"Replace any ? within exp with a var of the form ?123." 
.. Buggy Version *** 
(let ((n 0)) 

(labels 
((recurse (exp) 

(cond ((eq exp '?) (symbol '? (incf n))) 
((atom exp) exp) 
(t (cons (replace-?-vars (first exp)) 

(replace-?-vars (rest exp))))))) 
(recurse exp)))) 

Annoyingly, this version still doesn't work! This time, the problem is carelessness; 
we changed the repl ace-? - vars to recurse in two places, but not in the two calls in 
the body of recurse. 

Remedy: In general, the lesson is to make sure you call the right function. If there 
are two functions with similar effects and you call the wrong one, it can be hard to 
see. This is especially true if they have similar names. 

PROBLEM: Your closures don't seem to be working. 

Diagnosis: You may be erroneously creating a lambda expression by consing up 
code. Here's an example from a recent textbook: 

<a id='page-872'></a>

(defun make-specialization (c) 
(let (pred newc) 

(setf (get newc 'predicate) 
'(lambda (obj) ; Warning! 
(and ,(cons pred '(obj)) ; Don't do this. 
(apply '.(get c 'predicate) (list obj))))) 

...)) 

Strictly speaking, this is legal according to Common Lisp the Language, although in 
ANSI Common Lisp it will not he legal to use a list beginning with 1 ambda as a function. 
But in either version, it is a bad idea to do so. A list beginning with 1 ambda is just that: 
a list, not a closure. Therefore, it cannot capture lexical variables the way a closure 
does. 

Remedy: The correct way to create a closure is to evaluate a call to the special form 
function, or its abbreviation, #'. Here is a replacement for the code beginning with 

*(1 ambda Note that it is a closure, closed over pred and c. Also note that it gets 
the predi cate each time it is called; thus, it is safe to use even when predicates are 
being changed dynamically. The previous version would not work when a predicate 
is changed. 
#'(lambda (obj) ; Do this instead. 
(and (funcall pred obj) 
(funcall (get c 'predicate) obj))) 

It is important to remember that function (and thus #') is a special form, and thus 
only returns the right value when it is evaluated. A common error is to use # ' notation 
in positions that are not evaluated: 

(defvar *obscure-fns* '(#'cis #'cosh #'ash #'bit -orc2)) ; wrong 

This does not create a list of four functions. Rather, it creates a list of four sublists; 
the first subUst is (function eis). It is an error to funcall or apply such an object. 
The two correct ways to create a Ust of functions are shown below. The first assures 
that each function special form is evaluated, and the second uses function names 
instead of functions, thus relying on funcall or apply to coerce the names to the 
actual functions. 

(defvar *obscure-fns* (list #'cis #'cosh #'ash #'bit -orc2)) 
(defvar *obscure-fns* '(eis cosh ash bit-orc2)) 

Another common error is to expect # ' i f or # Or to return a function. This is an error 

<a id='page-873'></a>
because special forms are just syntactic markers. There is no function named i f or 

or; they should be thought of as directives that tell the compiler what to do with a 

piece of code. 

By the way, the function make - specialization above is bad not only for its lack of 
function but also for its use of backquote. The following is a better use of backquote: 

'(lambda (obj) 
(and (,pred obj) 
(,(get c 'predicate) obj))) 

25.4 Values Change "by Themselves'' 
PROBLEM: You deleted/removed something, but it didn't take effect. For example: 

> (setf numbers '(1 2 3 4 5)) ^ (1 2 3 4 5) 

> (remove 4 numbers) (1 2 3 5) 

> numbers ^(12345) 

> (delete 1 numbers) =^(2 3 4 5) 

> numbers =^(12345) 

Remedy: Use (setf numbers (delete 1 numbers)). Note that remove is a nondestructive 
function, so it will never alter its arguments, del ete is destructive, but 
when asked to delete the first element of a list, it returns the rest of the list, and thus 
does not alter the list itself. That is why setf is necessary. Similar remarks hold for 
nconc, sort, and other destructive operations. 

PROBLEM: You created a hundred different structures and changed a field in one of 
them. Suddenly, all the other ones magically changed! 

Diagnosis: Different structures may share identical subfields. For example, suppose 
you had: 

(defstruct block 
(possible-colors '(red green blue)) 
...) 

<a id='page-874'></a>

(setf bl (make-block)) 
(setf b2 (make-block)) 

(delete 'green (block-possible-colors bl)) 

Both bl and b2 share the initial Hst of possible colors. The del ete function modifies 
this shared list, so green is deleted from b2's possible colors Hst just as surely as it is 
deleted from bl's. 

Remedy: Don't share pieces of data that you want to alter individually. In this case, 
either use remove instead of delete, or allocate a different copy of the Hst to each 
instance: 

(defstruct block 

(possible-colors (list 'red 'green 'blue)) 

...) 

Remember that the initial value field of a defstruct is an expression that is evaluated 
anew each time make-bl ock is called. It is incorrect to think that the initial form is 
evaluated once when the defstruct is defined. 

25.5 Built-in Functions Don't Find Elements 
PROBLEM: You tried (find item 1 ist), and you know it is there, but it wasn't 
found. 

Diagnosis: By default, many built-in functions use eql as an equality test, fi nd is 
one of them. If i tern is, say, a list that is equal but not eql to one of the elements of 
list, it will not be found. 

Remedy: Use (find Item list :test #'equal) 

Diagnosis: If the i tern is nil, then nil will be returned whether it is found or not. 

Remedy: Use member or posi ti on instead of f i nd whenever the item can be nil. 

25.6 Multiple Values Are Lost 
PROBLEM: You only get one of the multiple values you were expecting. 

Diagnosis: In certain contexts where a value must be tested by Lisp, multiple values 
are discarded. For example, consider: 

<a id='page-875'></a>
(or (mv-1 x) (mv-2 x)) 
(and (mv-1 x) (mv-2 x)) 
(cond ((mv-1 x)) 

(t (mv-2 X))) 

In each case, if mv -2 returns multiple values, they will all be passed on. But if mv -1 
returns multiple values, only the first value will be passed on. This is true even in 
the last clause of a cond. So, while the final clause (t (mv-2 .)) passes on multiple 
values, the final clause ((mv -2 .)) would not. 

Diagnosis: Multiple values can be inadvertently lost in debugging as well. Suppose 
I had: 

(multiple-value-bind (a b c) 
(mv-1 x) 
...) 

Now, if I become curious as to what mv -1 returns, I might change this code to: 

(multiple-value-bind (a b c) 
(print (mv-1 x)) debugging output 

...) 

Unfortunately, print will see only the first value returned by mv-1, and will return 
only that one value to be bound to the variable a. The other values will be discarded, 
and b and c will be bound to ni 1. 

25.7 Declarations Are Ignored 
PROBLEM: Your program uses 1024 . 1024 arrays of floating-point numbers. But 
you find that it takes 15 seconds just to initialize such an array to zeros! Imagine how 
inefficient it is to actually do any computation! Here is your function that zeroes an 
array: 

(defun zero-array (arr) 
"Set the 1024x1024 array to all zeros." 
(declare (type (array float) arr)) 
(dotimes (i 1024) 

(dotimes (j 1024) 
(setf (aref arr i j) 0.0)))) 

Diagnosis: The main problem here is an ineffective declaration. The type (array 

<a id='page-876'></a>

f 1 oat) does not help the compiler, because the array could be displaced to an array 
of another type, and because f 1 oat encompasses both single- and double-precision 
floating-point numbers. Thus, the compiler is forced to allocate storage for a new 
copy of the number 0.0 for each of the million elements of the array. The function is 
slow mainly because it generates so much garbage. 

Remedy: The following version uses a much more effective type declaration: a 
simple array of single-precision numbers. It also declares the size of the array and 
turns safety checks off. It runs in under a second on a SPARCstation, which is slower 
than optimized C, but faster than unoptimized C. 

(defun zero-array (arr) 

"Set the array to all zeros." 

(declare (type (simple-array single-float (1024 1024)) arr) 

(optimize (speed 3) (safety 0))) 

(dotimes (i 1024) 

(dotimes (j 1024) 

(setf (aref arr i j) 0.0)))) 

Another common error is to use something like (simple-vector fixnum) asatype 
specifier. It is a quirk of Common Lisp that the simpl e-vector type specifier only 
accepts a size, not a type, while the array, vector and simple-array specifiers all 
accept an optional type followed by an optional size or list of sizes. To specify a 
simplevectoroffixnums,use (simple-array fixnum (*)). 

To be precise, simple-vector means (simple-array t (*)). This means that 
simple-vector cannot be used in conjunction with any other type specifier. A 
commonmistakeis to think that the type (and simple-vector (vector fixnum)) 
is equivalent to (simple-array fixnum (*)), a simple, one-dimensional vector 
of fixnums. Actually, it is equivalent to (simple-array t (*)), a simple one-
dimensional array of any type elements. To eliminate this problem, avoid simpl e-
vector altogether. 

25.8 My Lisp Does the Wrong Thing 
When all else fails, it is tempting to shift the blame for an error away from your own 
code and onto the Common Lisp implementation. It is certainly true that errors are 
found in existing implementations. But it is also true that most of the time. Common 
Lisp is merely doing something the user did not expect rather than something that is 
in error. 

For example, a common "bug report" is to complain about read - from- str 1 ng. A 
user might write: 

<a id='page-877'></a>
(read-from-string "a b c" :start 2) 

expecting the expression to start reading at position 2 and thus return b. In fact, this 
expression returns a. The angry user thinks the implementation has erroneously 
ignored the : start argument and files a bug report,^ only to get back the following 
explanation: 

The function read-from-string takes two optional arguments, eof-errorp and 
eof-val ue, in addition to the keyword arguments. Thus, in the expression above, 
: start is taken as the value of eof-errorp, with 2 as the value of eof-val ue. The 
correct answer is in fact to read from the start of the string and return the very first 
form, a. 

The functions read-from-string and parse-namestring are the only built-in 
functions that have this problem, because they are the only ones that have both 
optional and keyword arguments, with an even number of optional arguments. 
The functions wr i te -1 i ne and write-string have keyword arguments and a single 
optional argument (the stream), so if the stream is accidently omitted, an error will 
be signaled. (If you type (write-1 ine str :start 4), the system will complain 
either that: s ta rt is not a stream or that 4 is not a keyword.) 

The moral is this: functions that have both optional and keyword arguments 

are confusing. Take care when using existing functions that have this problem, and 

abstain from using both in your own functions. 

25.9 How to Find the Function You Want 
Veteran Common Lisp programmers often experience a kind of software deja vu: 
they believe that the code they are writing could be done by a built-in Common Lisp 
function, but they can't remember the name of the function. 

Here's an example: while coding up a problem I realized I needed a function that, 
given the lists (abed) and (cd), would return (a b), that is, the part of the first 
list without the second list. I thought that this was the kind of function that might 
be in the standard, but I didn't know what it would be called. The desired function 
is similar to set-difference, so I looked that up in the index of Common Lisp the 
Language and was directed to [page 429](chapter12.md#page-429). I browsed through the section on "using lists 
as sets" but found nothing appropriate. However, I was reminded of the function 
but! ast, which is also similar to the desired function. The index directed me to 
[page 422](chapter12.md#page-422) for butl ast, and on the same page I found 1 di f f, which was exactly the 
desired function. It might have been easier to find (and remember) if it were called 
1 i st-di ff erence, but the methodology of browsing near similar functions paid off. 

^This misunderstanding has shown up even in published articles, such as Baker 1991. 

<a id='page-878'></a>

If you think you know part of the name of the desired function, then you can 
use apropos to find it. For example, suppose I thought there was a function to push 
a new element onto the front of an array. Looking under array, push-array, and 
array- push in the index yields nothing. But I can turn to Lisp itself and ask: 

> (apropos "push") 
PUSH Macro (VALUE PLACE), pi ist 
PUSHNEW Macro (VALUE PLACE &KEY ...). pi ist 
VECTOR-PUSH function (NEW-ELEMENT VECTOR), pi is t 
VECTOR-PUSH-EXTEND function (DATA VECTOR &OPTIONAL ...), pi ist 

This should be enough to remind me that vector-push is the answer. If not, I can get 
more information from the manual or from the online functions documentati on or 
describe: 

> (documentation 'vector-push 'function) 
"Add NEW-ELEMENT as an element at the end of VECTOR. 
The fill pointer (leader element 0) is the index of the next 
element to be added. If the array is full, VECTOR-PUSH returns 
NIL and the array is unaffected; use VECTOR-PUSH-EXTEND instead 
if you want the array to grow automatically." 

Another possibility is to browse through existing code that performs a similar purpose. 
That way, you may find the exact function you want, and you may get additional 
ideas on how to do things differently. 

25.10 Syntax of LOOP 
1 oop by itself is a powerful programming language, one with a syntax quite different 
from the rest of Lisp. It is therefore important to exercise restraint in using 1 oop, lest 
the reader of your program become lost. One simple rule for limiting the complexity 
of loops is to avoid the with and and keywords. This eliminates most problems 
dealing with binding and scope. 

When in doubt, macro-expand the loop to see what it actually does. But if you 
need to macro-expand, then perhaps it would be clearer to rewrite the loop with more 
primitive constructs. 

25.11 Syntax of COND 
For many programmers, the special form cond is responsible for more syntax errors 
than any other, with the possible exception of 1 oop. Because most cond-clause start 

<a id='page-879'></a>
with two left parentheses, beginners often come to the conclusion that every clause 
must. This leads to errors like the following: 

(let ((entry (assoc item list))) 
(cond ((entry (process entry))) 
...)) 

Here entry is a variable, but the urge to put in an extra parenthesis means that the 
cond-clause attempts to call entry as a function rather than testing its value as a 
variable. 

The opposite problem, leaving out a parenthesis, is also a source of error: 

(cond (lookup item list) 
(t nil)) 

In this case, 1 ookup is accessed as a variable, when the intent was to call it as a 
function. In Common Lisp this will usually lead to an unbound variable error, but in 
Scheme this bug can be very difficult to pin down: the value of 1 ookup is the function 
itself, and since this is not null, the test will succeed, and the expression will return 
list without complaining. 

The moral is to be careful with cond, especially when using Scheme. Note that 
i f is much less error prone and looks just as nice when there are no more than two 
branches. 

25.12 Syntax of CASE 
In a case special form, each clause consists of a key or list of keys, followed by the 
value of that case. The thing to watch out for is when the key is t, otherwi se, or ni 1. 
For example: 

(case letter 
(s ...) 
(t ...) 
(u ...)) 

Here the t is taken as the default clause; it will always succeed, and all subsequent 
clauses will be ignored. Similarly, using a () or ni 1 as a key will not have the desired 
effect: it will be interpreted as an empty key hst. If you want to be completely safe, 
you can use a list of keys for every clause.^ This is a particularly good idea when you 

^Scheme requires a list of keys in each clause. Now you know why. 

<a id='page-880'></a>

write a macro that expands into a case. The following code correctly tests for t and 
nil keys: 

(case letter 
((s) ...) 
((t) ...) 
((u) ...) 
((nil) ...)) 

25.13 Syntax of LET and LET* 
A common error is leaving off a layer of parentheses in 1 et, just like in cond. Another 
error is to refer to a variable that has not yet been bound in a 1 et. To avoid this 
problem, use 1 et* whenever a variable's initial binding refers to a previous variable. 

25.14 Problems with Macros 
In section 3.2 we describeda four-part approach to the design of macros: 

* Decide if the macro is really necessary. 
* Write down the syntax of the macro. 
* Figure out what the macro should expand into. 
* Use defmacro to implement the syntax/expansion correspondence. 
This section shows the problems that can arise in each part, starting with the first: 

* Decide if the macro is really necessary. 
Macros extend the rules for evaluating an expression, while function calls obey the 
rules. Therefore, it can be a mistake to define too many macros, since they can make 
it more difficult to understand a program. A common mistake is to define macros 
that do not violate the usual evaluation rules. One recent book on AI programming 
suggests the following: 

(defmacro binding-of (binding) ; Warning! 
'(cadr .binding)) ; Don't do this. 

The only possible reason for this macro is an unfounded desire for efficiency. Always 
use an inl ine function instead of a macro for such cases. That way you get the 

<a id='page-881'></a>
efficiency gain, you have not introduced a spurious macro, and you gain the ability to 
apply or map the function #' bi ndi ng-of, something you could not do with a macro: 

(proclaim '(inline binding-of)) 

(defun binding-of (binding) ; Do this instead. 
(second binding)) 

* Write down the syntax of the macro. 
Try to make your macro follow conventions laid down by similar macros. For example, 
if your macro defines something, it should obey the conventions of defvar, 
defstruct, def mac r o, and the rest: start with the letters def, take the name of the thing 
to be defined as the first argument, then a lambda-list if appropriate, then a value or 
body. It would be nice to allow for optional declarations and documentation strings. 

If your macro binds some variables or variablelike objects, use the conventions 

laid down by 1 et, 1 et*, and 1 abel s: allow for a list of variable or (variable init-val) 

pairs. If you are iterating over some kind of sequence, follow dotimes and dol i st. 

For example, here is the syntax of a macro to iterate over the leaves of a tree of conses: 

(defmacro dotree ((var tree &optional result) &body body) 
"Perform body with var bound to every leaf of tree, 
then return result. Return and Go can be used in body." 
...) 

* Figure out what the macro should expand into. 
* Use defmacro to implement the syntax/expansion correspondence. 
There are a number of things to watch out for in figuring out how to expand a macro. 

First, make sure you don't shadow local variables. Consider the following definition 

for pop- end, a function to pop off and return the last element of a list, while updating 

the list to no longer contain the last element. The definition uses last1, which was 

defined on [page 305](chapter9.md#page-305) to return the last element of a list, and the built-in function 

nbutl ast returns all but the last element of a list, destructively altering the list. 

(defmacro pop-end (place) ; Warning!Buggy! 
"Pop and return last element of the list in PLACE." 
'(let ((result (lastl .place))) 

(setf .place (nbutlast .place)) 

result)) 

This will do the wrong thing for (pop-end result), or for other expressions that 

mention the variable resul t. The solution is to use a brand new local variable that 

could not possibly be used elsewhere: 

<a id='page-882'></a>

(defmacro pop-end (place) ; Lessbuggy 
"Pop and return last element of the list in PLACE." 
(let ((result (gensym))) 

'(let ((.result (lastl .place))) 
(setf .place (nbutlast .place)) 
.result))) 
There is still the problem of shadowing local functions. For example, a user who 
writes: 

(flet ((lastl (x) (sqrt x))) 
(pop-end list) 
...) 

will be in for a surprise, pop-end will expand into code that calls last1, but since 
lastl has been locally defined to be something else, the code won't work. Thus, the 
expansion of the macro violates referential transparency. To be perfectly safe, we 
could try: 

(defmacro pop-end (place) ; Lessbuggy 
"Pop and return last element of the list in PLACE." 
(let ((result (gensym))) 

'(let ((.result (funcall .#'lastl .place))) 
(setf .place (funcall .#'nbutlast .place)) 
.result))) 
This approach is sometimes used by Scheme programmers, but Common Lisp programmers 
usually do not bother, since it is rarer to define local functions in Common 
Lisp. Indeed, in Common Lisp the Language, 2d edition, it was explicitly stated (page 

260) that a user function cannot redefine or even bind any built-in function, variable, 
or macro. Even if it is not prohibited in your implementation, redefining or binding 
a built-in function is confusing and should be avoided. 
Common Lisp programmers expect that arguments will be evaluated in left-to-
right order, and that no argument is evaluated more than once. Our definition of 
pop-end violates the second of these expectations. Consider: 

(pop-end (aref lists (incf i))) = 

(LET ((#:G3096 (LASTl (AREF LISTS (INCF I))))) 
(SETF (AREF LISTS (INCF I)) (NBUTLAST (AREF LISTS (INCF I)))) 
#:G3096) 

This increments i three times, when it should increment it only once. We could fix 
this by introducing more local variables into the expansion: 

<a id='page-883'></a>
(let* ((tempi (incf i)) 
(temp2 (AREF LISTS tempi)) 
(temp3 (LASTl temp2))) 

(setf (aref lists tempi) (nbutlast temp2)) 
temp3) 

This kind of left-to-right argument processing via local variables is done automatically 
by the Common Lisp setf mechanism. Fortunately, the mechanism is easy to use. 
We can redefine pop-end to call pop directly: 

(defmacro pop-end (place) 
"Pop and return last element of the list in PLACE." 
'(pop (last .place))) 

Now all we need to do is define the setf method for 1 as t. Here is a simple definition. 
It makes use of the function last2, which returns the last two elements of a list. In 
ANSI Common Lisp we could use (last list 2), but with a pre-ANSI compiler we 
need to define last2: 

(defsetf last (place) (value) 
'(setf (cdr (last2 .place)) .value)) 

(defun last2 (list) 
"Return the last two elements of a list. " 
(if (null (rest2 list)) 

list 
(last2 (rest list)))) 

Here are some macro-expansions of calls to pop-end and to the setf method for 
last. Different compilers will produce different code, but they will always respect 
the left-to-right, one-evaluation-only semantics: 

> (pop-end (aref (foo lists) (incf i))) = 
(LET ((G0128 (AREF (FOO LISTS) (SETQ I (+ I 1))))) 

(PR061 
(CAR (LAST G0128)) 
(SYSiSETCDR (LAST2 G0128) (CDR (LAST G0128))))) 

> (setf (last (append . y)) 'end) = 
(SYSiSETCDR (LAST2 (APPEND X Y)) 'END) 

Unfortunately, there is an error in the setf method for last. It assumes that the 
list will have at least two elements. If the Ust is empty, it is probably an error, but if 
alisthasexactlyoneelement, then (setf (last list) val) should have the same 
effect as (setf list val). But there is no way to do that with defsetf, because the 

<a id='page-884'></a>

setf method defined by def setf never sees list itself. Instead, it sees a local variable 
that is automatically bound to the value of list. In other words, def setf evaluates the 
list and val for you, so that you needn't worry about evaluating the arguments out of 
order, or more than once. 

To solve the problem we need to go beyond the simple def setf macro and delve 
into the complexities of def i ne-setf-method, one of the trickiest macros in all of 
Common Lisp, def i ne-setf-method defines a setf method not by writing code 
directly but by specifying five values that will be used by Common Lisp to write the 
code for a call to setf. The five values give more control over the exact order in 
which expressions are evaluated, variables are bound, and results are returned. The 
five values are: (1) a list of temporary, local variables used in the code; (2) a list of 
values these variables should be bound to; (3) a list of one variable to hold the value 
specified in the call to setf; (4) code that will store the value in the proper place; (5) 
code that will access the value of the place. This is necessary for variations of setf 
like i ncf and pop, which need to both access and store. 

In the following setf method for last, then, we are defining the meaning of 
(setf (last place) value). We keep track of all the variables and values needed 
to evaluate pi ace, and add to that three more local variables: last2-var will hold 
the last two elements of the list, last2-p will be true only if there are two or more 
elements in the list, and last - va r will hold the form to access the last element of the 
list. We also make up a new variable, resul t, to hold the val ue. The code to store 
the value either modifies the cdr of last2-var, if the list is long enough, or it stores 
directly into pi ace. The code to access the value just retrieves 1 diSt-\/ar. 

(define-setf-method last (place) 
(multiple-value-bind (temps vals stores store-form access-form) 
(get-setf-method place) 

(let ((result (gensym)) 
(last2-var (gensym)) 
(last2-p (gensym)) 
(last-var (gensym))) 

Return 5 vals: temps vals stores store-form access-form 

(values 
'(.temps Jast2-var .last2-p .last-var) 
'(.@vals (last2 .access-form) 

(= (length .last2-var) 2) 

(if .last2-p (rest .last2-var) .access-form)) 
(list result) 
'(if .last2-p 

(setf (cdr .last2-var) .result) 
(let ((.(first stores) .result)) 
.store-form)) 
last-var)))) 

<a id='page-885'></a>
It should be mentioned that setf methods are very useful and powerful things. It 
is often better to provide a setf method for an arbitrary function, f, than to define 
a special setting function, say, set-f. The advantage of the setf method is that it 
can be used in idioms like incf and pop, in addition to setf itself. Also, in ANSI 
Common Lisp, it is permissible to name a function with #' (setf f), so you can also 
use map or apply the setf method. Most setf methods are for functions that just 
access data, but it is permissible to define setf methods for functions that do any 
computation whatsoever. As a rather fanciful example, here is a setf method for the 
square-root function. It makes (setf (sqrt x) 5) be almost equivalent to (setf . 
(* 5 5)); the difference is that the first returns 5 while the second returns 25. 

(define-setf-method sqrt (num) 
(multiple-value-bind (temps vals stores store-form access-form) 
(get-setf-method num) 
(let ((store (gensym))) 

(values temps 
vals 
(list store) 

'(let ((.(first stores) (* .store .store))) 
.store-form 
.store) 
'(sqrt .access-form))))) 

Turning from setf methods back to macros, another hard part about writing portable 
macros is anticipating what compilers might warn about. Let's go back to the dotree 
macro. Its definition might look in part like this: 

(defmacro dotree ((var tree &optional result) &body body) 
"Perform body with var bound to every leaf of tree, 
then return result. Return and Go can be used in body." 

'(let ((.var)) 
.body)) 

Now suppose a user decides to count the leaves of a tree with: 

(let ((count 0)) 
(dotree (leaf tree count) 
(incf count))) 

The problem is that the variable leaf is not used in the body of the macro, and 
a compiler may well issue a warning to that effect. To make matters worse, a 
conscientious user might write: 

<a id='page-886'></a>

(let ((count 0)) 

(dotree (leaf tree count) 

(declare (ignore leaf)) 

(incf count))) 

The designer of a new macro must decide if declarations are allowed and must make 
sure that compiler warnings will not be generated unless they are warranted. 

Macros have the full power of Lisp at their disposal, but the macro designer must 
remember the purpose of a macro is to translate macro code into primitive code, 
and not to do any computations. Consider the following macro, which assumes that 
translate - rul e-body is defined elsewhere: 

(defmacro defrule (name &body body) ; Warning! buggy! 

"Define a new rule with the given name." 

(setf (get name 'rule) 

*#*(lambda () ,(translate-rule-body body)))) 

The idea is to store a function under the rul e property of the rule's name. But this 
definition is incorrect because the function is stored as a side effect of expanding the 
macro, rather than as an effect of executing the expanded macro code. The correct 
definition is: 

(defmacro defrule (name &body body) 
"Define a new rule with the given name." 

'(setf (get '.name 'rule) 
#'(lambda () .(translate-rule-body body)))) 
Beginners sometimes fail to see the difference between these two approaches, because 
they both have the same result when interpreting a file that makes use of 
def rul e. But when the file is compiled and later loaded into a different Lisp image, 
the difference becomes clear: the first definition erroneously stores the function 
in the compiler's image, while the second produces code that correctly stores the 
function when the code is loaded. 

Beginning macro users have asked, "How can I have a macro that expands into 
code that does more than one thing? Can I splice in the results of a macro?" 

If by this the beginner wants a macro that just does two things, the answer is 
simply to use a progn. There will be no efficiency problem, even if the progn forms 
are nested. That is, if macro-expansion results in code like: 

(progn (progn (progn a b) c) (progn d e)) 

the compiler will treat it the same as (progn abode). 

<a id='page-887'></a>
On the other hand, if the beginner wants a macro that returns two values, the 
proper form is val ues, but it must be understood that the calling function needs to 
arrange specially to see both values. There is no way around this limitation. That is, 
there is no way to write a macro-or a function for that matter-that will "splice in" its 
results to an arbitrary call. For example, the function f 1 oor returns two values (the 
quotient and remainder), as does i ntern (the symbol and whether or not the symbol 
already existed). But we need a special form to capture these values. For example, 
compare: 

> (list (floor 11 5) (intern ..))=^(2 X) 

> (multiple-value-call #*list 
(floor 11 5) (intern 'x))=^(2 1 X -.INTERNAL) 

25.15 A Style Guide to Lisp 
In a sense, this whole book is a style guide to writing quality Lisp programs. But this 
section attempts to distill some of the lessons into a set of guidelines. 

When to Define a Function 

Lisp programs tend to consist of many short functions, in contrast to some languages 
that prefer a style using fewer, longer functions. New functions should be introduced 
for any of the following reasons: 

1. For a specific, easily stated purpose. 
2. To break up a function that is too long. 
3. When the name would be useful documentation. 
4. When it is used in several places. 
In (2), it is interesting to consider what "too long" means. Charniak et al. (1987) 
suggested that 20 lines is the limit. But now that large bit-map displays have replaced 
24-line terminals, function definitions have become longer. So perhaps one screenful 
is a better limit than 20 lines. The addition of f 1 et and 1 abel s also contributes to 
longer function definitions. 

<a id='page-888'></a>

When to Define a Special Variable 

In general, it is a good idea to minimize the use of special variables. Lexical variables 
are easier to understand, precisely because their scope is limited. Try to limit special 
variables to one of the following uses: 

1. For parameters that are used in many functions spread throughout a program. 
2. For global, persistant, mutable data, such as a data base of facts. 
3. For infrequent but deeply nested use. 
An example of (3) might be a variable like ^standard-output*, which is used by 
low-level priniting functions. It would be confusing to have to pass this variable 
around among all your high-level functions just to make it available to pri nt. 

When to Bind a Lexical Variable 

In contrast to special variables, lexical variables are encouraged. You should feel free 
to introduce a lexical variable (with a 1 et, 1 ambda or defun) for any of the following 
reasons: 

1. To avoid typing in the same expression twice. 
2. To avoid computing the same expression twice. 
3. When the name would be useful documentation. 
4. To keep the indentation manageable. 
How to Choose a Name 

Your choice of names for functions, variables, and other objects should be clear, 
meaningful, and consistent. Some of the conventions are listed here: 

1. Use mostly letters and hyphens, and use full words: del ete -file . 
2. You can introduce an abbreviation if you are consistent: get-dtree, dtree-
fetch. For example, this book uses f . consistently as the abbreviation for 
"function." 
3. Predicates end in -p (or ? in Scheme), unless the name is already a predicate: 
variable-p, occurs-in. 
4. Destructive functions start with . (or end in ! in Scheme): nreverse. 
<a id='page-889'></a>
5. Generalized variable-setting macros end in f: setf, incf. (Push is an exception.) 
6. Slot selectors created by defstruct are of the form type-slot. Use this for 
non-def s t r uct selectors as well: cha r- bi ts. 
7. Many functions have the form action-object: copy -1i st, del ete -f i1 e. 
8. Other functions have the form object-modifier: 1 ist-length, char-lessp. Be 
consistent in your choice between these two forms. Don't have pri nt-edge 
and vertex-pri nt in the same system. 
9. A function of the form modulename-functionname is an indication that packages 
are needed. Use parser: pri nt-tree instead of parser-print-tree. 
10. Special variables have asterisks: *db*, *print-length*. 
11. Constants do not have asterisks: pi, most-positive-fixnum. 
12. Parameters are named by type: (defun length (sequence) ...) or by purpose: 
(defun subsetp (subset superset) ...) or both: (defun / (number 
&rest denominator-numbers) ...) 
13. Avoid ambiguity. A variable named last-node could have two meanings; use 
previ ous - node or f i nal - node instead. 
14. A name like propagate-constraints-to-neighboring-vertexes is too long, 
while prp-con is too short. In deciding on length, consider how the name will 
be used: propagate-constraints is just right, because a typical call will be 
(propagate-constrai nts vertex), so it will be obvious what the constraints 
are propagating to. 
Deciding on the Order of Parameters 

Once you have decided to define a function, you must decide what parameters it will 
take, and in what order. In general, 

1. Put important parameters first (and optional ones last). 
2. Make it read like prose if possible: (push element stack). 
3. Group similar parameters together. 
Interestingly, the choice of a parameter list for top-level functions (those that the 

user is expected to call) depends on the environment in which the user will function. 

In many systems the user can type a keystroke to get back the previous input to the top 

<a id='page-890'></a>

level, and can then edit that input and re-execute it. In these systems it is preferable 
to have the parameters that are likely to change be at the end of the parameter list, so 
that they can be easily edited. On systems that do not offer this kind of editing, it is 
better to either use ke3word parameters or make the highly variable parameters first 
in the list (with the others optional), so that the user will not have to type as much. 

Many users want to have required keyword parameters. It turns out that all 
keyword parameters are optional, but the following trick is equivalent to a required 
keyword parameter. First we define the function requi red to signal an error, and 
then we use a call to requi red as the default value for any keyword that we want to 
make required: 

(defun required () 
(error "A required keyword argument was not supplied.")) 

(defun fn (x &key (y (required))) 
...) 

25.16 Dealing with Files, Packages, and Systems 
While this book has covered topics that are more advanced than any other Lisp text 
available, it is still concerned only with programming in the small: a single project at 
a time, capable of being implemented by a single programmer. More challenging is 
the problem of programming in the large: building multiproject, multiprogranuner 
systems that interact well. 

This section briefly outlines an approach to organizing a larger project into manageable 
components, and how to place those components in files. 

Every system should have a separate file that defines the other files that comprise 
the system. I recommend defining any packages in that file, although others put 
package definitions in separate files. 

The following is a sample file for the mythical system Project-X. Each entry in the 
file is discussed in turn. 

1. The first line is a comment known as the mode line. The text editor emacs will 
parse the characters between delimiters to discover that the file contains 
Lisp code, and thus the Lisp editing commands should be made available. The 
dialect of Lisp and the package are also specified. This notation is becoming 
widespread as other text editors emulate emacs's conventions. 
2. Each file should have a description of its contents, along with information on 
the authors and what revisions have taken place. 
<a id='page-891'></a>
3. Comments with four semicolons (;;;;) denote header lines. Many text editors 
supply a command to print all such lines, thus achieving an outline of the major 
parts of a file. 
4. The first executable form in every file should be an i n-package. Here we use 
the user package. We will soon create the proj ect-x package, and it will be 
used in all subsequent files. 
5. We want to define the Project-X system as a collection of files. Unfortunately, 
Common Lisp provides no way to do that, so we have to load our own system-
definition functions explicitly with a call to 1 oad. 
6. The call to def i ne - system specifies the files that make up Project-X. We provide 
a name for the system, a directory for the source and object files, and a list of 
modules that make up the system. Each module is a list consisting of the module 
name (a symbol) followed by a one or more files (strings or pathnames). We 
have used keywords as the module names to eliminate any possible name 
conflicts, but any symbol could be used. 
7. The call to def package defines the package proj ect-x. For more on packages, 
see section 24.1. 
8. The final form prints instructions on how to load and run the system. 
Mode: Lisp; Syntax: Common-Lisp; Package: User 

(Brief description of system here.) 

;; Define the Project-X system, 

(in-package "USER") 

(load "/usr/norvig/defsys.lisp") ; load define-system 

(define-system ;; Define the system Project-X 
:naaje :project-x 
:source-dir "/usr/norvig/project-x/*.lisp" 
:object-dir "/usr/norvig/project-x/*.bin" 
:modules '((:macros "header" "macros") 

(:main "parser" "transformer" "optimizer" 
"commands" "database" "output") 
(:windows "xwindows" "clx" "client"))) 

(defpackage :project-x ;; Define the package Project-X 
(:export "DEFINE-X" "DO-X" "RUN-X") 
(:nicknames "PX") 
(:use common-lisp)) 

<a id='page-892'></a>

(format *debug-io* "~& To load the Project-X system, type 
(make-system :name :project-x) 
To run the system, type 
(project-x:run-x)") 

Each of the files that make up the system will start like this: 

;;: -*- Mode: Lisp; Syntax: Common-Lisp; Package: Project-X -*


(in-package "PROJECT-X") 

Now we need to provide the system-definition functions, def ine-system 
and make-system. The idea is that def ine-system is used to define the files that 
make up a system, the modules that the system is comprised of, and the files that 
make up each module. It is necessary to group files into modules because some 
files may depend on others. For example, all macros, special variables, constants, 
and inline functions need to be both compiled and loaded before any other files that 
reference them are compiled. In Project-X, all defvar, defparameter, defconstant, 
and defstruct^ forms are put in the file header, and all defmacro forms are put in the 
file macros. Together these two files form the first module, named : mac ros, which 
will be loaded before the other two modules (: ma i . and : wi ndows) are compiled and 
loaded. 

define-system also provides a place to specify a directory where the source 
and object files will reside. For larger systems spread across multiple directories, 
def i ne-system will not be adequate. 

Here is the first part of the file defsys.lisp, showing the definition of 
def i ne-system and the structure sys. 

;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: User -*


;;;; A Facility for Defining Systems and their Components 

(in-package "USER") 

(defvar ^systems* nil "List of all systems defined.") 

(defstruct sys 
"A system containing a number of source and object files." 
name source-dir object-dir modules) 

^defstruct forms are put here because they may create inline functions. 

<a id='page-893'></a>
(defun define-system (&key name source-dir object-dir modules) 
"Define a new system." 
;; Delete any old system of this name, and add the new one. 
(setf *systems* (delete name *systems* :test #*string-equal 

:key #'sys-name)) 

(push (make-sys 
:name (string name) 
:source-dir (pathname source-dir) 
:object-dir (pathname object-dir) 
:modules *((:all (mapcar #'first modules)) ..modules)) 

*systems*) 
name) 

The function make - sy s t em is used to compile and/or load a previously defined system. 
The name supplied is used to look up the definition of a system, and one of three 
actions is taken on the system. The keyword : cl oad means to compile and then load 
files. :1 oad means to load files; if there is an object (compiled) file and it is newer than 
the source file, then it will be loaded, otherwise the soiu-ce file will be loaded. Finally, 
: update means to compile just those source files that have been changed since their 
corresponding source files were last altered, and to load the new compiled version. 

(defun make-system (&key (module :all) (action :cload) 

(name (sys-name (first *systems*)))) 
"Compile and/or load a system or one of its modules." 
(let ((system (find name *systems* :key #'sys-name 

:test #'string-equal))) 
(check-type system (not null)) 
(check-type action (member :cload -.update -.load)) 
(with-compilation-unit () (sys-action module system action)) 

(defun sys-action (x system action) 
"Perform the specified action to . in this system. 
X can be a module name (symbol), file name (string) 
or a list." 
(typecase . 

(symbol (let ((files (rest (assoc . (sys-modules system))))) 

(if (null files) 
(warn "No files for module "a" x) 
(sys-action files system action)))) 

(list (dolist (file x) 
(sys-action file system action))) 
((string pathname) 
(let ((source (merge-pathnames 
X (sys-source-dir system))) 
(object (merge-pathnames 
X (sys-object-dir system)))) 
(case action 

<a id='page-894'></a>

(reload (compile-file source) (load object)) 
(lupdate (unless (newer-file-p object source) 

(compile-file source)) 
(load object)) 
(.-load (if (newer-file-p object source) 
(load object) 
(load source)))))) 
(t (warn "Don't know how to ~a "a in system "a" 
action X system)))) 

To support this, we need to be able to compare the write dates on files. This is not 
hard to do, since Common Lisp provides the function f i 1 e-wri te-date. 

(defun newer-file-p (filel file2) 

"Is filel newer than (written later than) file2? " 

(>-num (if (probe-file filel) (file-write-date filel)) 

(if (probe-file file2) (file-write-date file2)))) 

(defun >-num (x y) 
"True if X and y are numbers, and . > y." 
(and (numberp x) (numberp y) (> . y))) 

25.17 Portability Problems 
Programming is difficult. All programmers know the frustration of trpng to get a 
program to work according to the specification. But one thing that really defines the 
professional programmer is the ability to write portable programs that will work on 
a variety of systems. A portable program not only must work on the computer it 
was tested on but also must anticipate the difference between your computer and 
other ones. To do this, you must understand the Common Lisp specification in the 
abstract, not just how it is implemented on your particular machine. 

There are three ways in which Common Lisp systems can vary: in the treatment 
of "is an error" situations, in the treatment of unspecified results, and in extensions 
to the language. 

Common Lisp the Language specifies that it "is an error" to pass a non-number to 
an arithmetic function. For example, it is an error to evaluate (+ nil 1). However, 
it is not specified what should be done in this situation. Some implementations may 
signal an error, but others may not. An implementation would be within its right to 
return 1, or any other number or non-number as the result. 

An unsuspecting programmer may code an expression that is an error but still 
computes reasonable results in his or her implementation. A common example is 
applying get to a non-symbol. This is an error, but many implementations will 

<a id='page-895'></a>
just return nil, so the programmer may write (get . * prop) when (if (symbol . 
.) (get . 'prop) nil) is actually needed for portable code. Another common 
problem is with subseq and the sequence functions that take : end keywords. It is an 
error if the : end parameter is not an integer less than the length of the sequence, but 
many implementations will not complain if : end is nil or is an integer greater than 
the length of the sequence. 

The Common Lisp specification often places constraints on the result that a 
function must compute, without fully specifying the result. For example, both of the 
following are valid results: 

> (union '(a b c) '(b c d)) =i> (A . C D) 
> (union *(a b c) '(b c d)) (D A . C) 

A program that relies on one order or the other will not be portable. The same warning 
applies to i ntersecti on and set-di ff erence. Many functions do not specify how 
much the result shares with the input. The following computation has only one 
possible printed result: 

> (remove 'x '(a b c d)) ^ (A . C D) 

However, it is not specified whether the output is eq or only equal to the second 

input. 

Input/output is particularly prone to variation, as different operating systems 
can have very different conceptions of how I/O and the file system works. Things 
to watch out for are whether read-char echoes its input or not, the need to include 
finish-output, andvariationinwherenewlines are needed, particularly with respect 
to the top level. 

Finally, many implementations provide extensions to Common Lisp, either by 
adding entirely new functions or by modifying existing functions. The programmer 
must be careful not to use such extensions in portable code. 

25.18 Exercises 
&#9635; Exercise 25.1 Pi] On your next programming project, keep a log of each bug you 
detect and its eventual cause and remedy. Classify each one according to the taxonomy 
given in this chapter. What kind of mistakes do you make most often? How 
could you correct that? 

&#9635; Exercise 25.2 [s-d] Take a Common Lisp program and get it to work with a different 
compiler on a different computer. Make sure you use conditional compilation read 

<a id='page-896'></a>

macros (#+ and #-) so that the program will work on both systems. What did you 

have to change? 

&#9635; Exercise 25.3 [m] Write a setf method for i f that works like this: 

(setf (if test (first x) y) (+ 2 3)) = 
(let ((temp (+ 2 3))) 

(if test 
(setf (first x) temp) 
(setf y temp))) 

You will need to use def i ne-setf-method, not defsetf. (Why?) Make sure you 
handle the case where there is no else part to the i f. 

&#9635; Exercise 25.4 [h] Write a setf method for 1 ookup, a function to get the value for a 
key in an association list. 

(defun lookup (key alist) 
"Get the cdr of key's entry in the association list." 
(cdr (assoc key alist))) 

25.19 Answers 
Answer 25.4 Here is the setf method for 1 ookup. It looks for the key in the a-list, 
and if the key is there, it modifies the cdr of the pair containing the key; otherwise it 
adds a new key/value pair to the front of the a-list. 

(define-setf-method lookup (key alist-place) 
(multiple-value-bind (temps vals stores store-form access-form) 
(get-setf-method alist-place) 

(let ((key-var (gensym)) 
(pair-var (gensym)) 
(result (gensym))) 

(values 
'(.key-var .temps .pair-var) 
'(.key .vals (assoc .key-var .access-form)) 
'(.result) 

'(if.pair-var 
(setf (cdr .pair-var) .result) 
(let ((.(first stores) 
(aeons .key-var .result .access-form))) 

.store-form 

.result)) 

'(cdr .pair-var))))) 

