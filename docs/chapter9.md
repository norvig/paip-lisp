# Chapter 9 {docsify-ignore}
<a id='page-265'></a>

Efficiency issues 

ALisp programmer knows the value of every thing, 
but the cost of nothing. 

-Alan J. Perils 

Lisp is not inherently less efficient than other 
high-level languages. 

—Richard J. Fateman 

O
O
ne of the reasons Lisp has enjoyed a long history is because it is an ideal language for 
what is now called rapid-prototyping—developing a program quickly, with little regards 
for details. That is what we have done so far in this book: concentrated on getting a 
working algorithm. Unfortunately, when a prototype is to be turned into a production-quality 
program, details can no longer be ignored. Most "real" AI programs deal with large amounts of 
data, and with large search spaces. Thus, efficiency considerations become very important. 

However, this does not mean that writing an efficient program is fundamentaly different 
from writing a working program. Ideally, developing an efficient program should be a three-step 
process. First, develop a working program, using proper abstractions so that the program will be 
easy to change if necessary. Second,instrument the program to determine where it is spending 
most of the time. Third, replace the slow parts with faster versions, while maintaining the 
program's correctness. 

<a id='page-266'></a>

The term efficiency will be used primarily to talk about the speed or run time of a 
program. To a lesser extent, efficiency is also used to refer to the space or amount of 
storage consumed by a program. We will also talk about the cost of a program. This 
is partly a use of the metaphor "time is money," and partly rooted in actual monetary 
costs—if a critical program runs unacceptably slowly, you may need to buy a more 
expensive computer. 

Lisp has been saddled with a reputation as an "inefficient language." Strictly 
speaking, it makes no sense to call a language efficient or inefficient. Rather, it is only 
a particular implementation of the language executing a particular program that can be 
measured for efficiency. So saying Lisp is inefficient is partly a historical claim: some 
past implementations have been inefficient. It is also partly a prediction: there are 
some reasons why future implementations are expected to suffer from inefficiencies. 
These reasons mainly stem from Lisp's flexibility. Lisp allows many decisions to be 
delayed until run time, and that can make the run time take longer. In the past decade, 
the "efficiency gap" between Lisp and "conventional languages" Uke FORTRAN or 
C has narrowed. Here are the reasons—some deserved, some not—behind Lisp's 
reputation for inefficiency: 

* Early implementations were interpreted rather than compiled, which made 
them inherently inefficient. Common Lisp implementations have compilers, 
so this is no longer a problem. While Lisp is (primarily) no longer an interpreted 
language, it is still an interactive language, so it retains its flexibility. 
* Lisp has often been used to write interpreters for embedded languages, thereby 
compounding the problem. Consider this quote from Cooper and Wogrin's 
(1988) book on the rule-based programming language OPS5: 
The efficiency of implementations that compile rules into executable code 
compares favorably to that of programs wntten in most sequential languages 
such as FORTRAN or Pascal Implementations that compile rules 
into data structures to be interpreted, as do many Lisp-based ones, could be 
noticeably slower. 

Here Lisp is guilty by association. The fallacious chain of reasoning is: Lisp has 
been used to write interpreters; interpreters are slow; therefore Lisp is slow. 
While it is true that Lisp makes it very easy to write interpreters, it also makes 
it easy to write compilers. This book is the first that concentrates on using Lisp 
as both the implementation and target language for compilers. 

* Lisp encourages a style with lots of function calls, particularly recursive calls. 
In some older systems, function calls were expensive. But it is now understood 
that a function call can be compiled into a simple branch instruction, and that 
<a id='page-267'></a>

many recursive calls can be made no more expensive than an equivalent iterative 
loop (see chapter 22). It is also possible to instruct a Common Lisp compiler 
to compile certain functions inline, so there is no calling overhead at all. 

On the other hand, many Lisp systems require two fetches instead of one to find 
the code for a function, and thus will be slower. This extra level of indirection 
is the price paid for the freedom of being able to redefine functions without 
reloading the whole program. 

Run-time type-checking is slow. Lisp provides a repertoire of generic functions. 
For example, we can write (+ x y) without bothering to declare if . and y are integers, 
floatingpoint, bignums, complex numbers, rationals, or some combination 
of the above. This is very convenient, but it means that type checks must be 
made at run time, so the generic+will be slower than, say, a 16-bit integer addition 
with no check for overflow. If efficiency is important. Common Lisp allows 
the programmer to include declarations that can eUminate run-time checks. 

In fact, once the proper declarations are added. Lisp can be as fast or faster 
than conventional languages. Fateman (1973) compared the FORTRAN cube 
root routine on the PDP-10 to a MacLisp transliteration. The MacLisp version 
produced almost identical numerical code, but was 18% faster overall, due to 
a superior function-calling sequence.^ The epigraph at the beginning of this 
chapter is from this article. 

Berlin and Weise (1990) show that with a special compilation technique called 
partial evaluation, speeds 7 to 90 times faster than conventionally compiled code 
can be achieved. Of course, partial evaluation could be used in any language, 
but it is very easy to do in Lisp. 

The fact remains that Lisp objects must somehow represent their type, and 
even with declarations, not all of this overhead can be eliminated. Most Lisp 
implementations optimize access to lists and fixnums but pay the price for the 
other, less commonly used data types. 

Lisp automatically manages storage, and so it must periodically stop and collect 
the unused storage, or garbage. In early systems, this was done by periodically 
sweeping through all of memory, resulting in an appreciable pause. Modern 
systems tend to use incremental garbage-collection techniques, so pauses are 
shorter and usually unnoticed by the user (although the pauses may still be too 
long for real-time applications such as controlling a laboratory instrument). 
The problem with automatic garbage collection these days is not that it is 
slow-in fact, the automatic systems do about as well as handcrafted storage 

^One could say that the FORTRAN compiler was "broken." This underscores the problem 

of defining the efficiency of a language-do we judge by the most popular compiler, by the best 
compiler available, or by the best compiler imaginable? 

<a id='page-268'></a>

allocation. The problem is that they make it convenient for the programmer 
to generate a lot of garbage in the first place. Programmers in conventional 
languages, who have to clean up their own garbage, tend to be more careful 
and use static rather than dynamic storage more often. If garbage becomes a 
problem, the Lisp programmer can just adopt these static techniques. 

Lisp systems are big and leave little room for other programs. Most Lisp systems 
are designed to be complete environments, within which the programmer 
does all program development and execution. For this kind of operation, it 
makes sense to have a large language like Common Lisp with a huge set of 
tools. However, it is becoming more common to use Lisp as just one component 
in a computing environment that may include UNIX, X Windows, emacs, 
and other interacting programs. In this kind of heterogeneous environment, 
it would be useful to be able to define and run small Lisp processes that do 
not include megabytes of unused tools. Some recent compilers support this 
option, but it is not widely available yet. 

Lisp is a complicated high-level language, and it can be difficult for the programmer 
to anticipate the costs of various operations. In general, the problem 
is not that an efficient encoding is impossible but that it is difficult to arrive at 
that efficient encoding. In a language like C, the experienced programmer has 
a pretty good idea how each statement will compile into assembly language 
instructions. But in Lisp, very similar statements can compile into widely different 
assembly-level instructions, depending on subtle interactions between 
the declarations given and the capabilities of the compiler. Page 318 gives an 
example where adding a declaration speeds up a trivial function by 40 times. 
Nonexperts do not understand when such declarations are necessary and are 
frustrated by the seeming inconsistencies. With experience, the expert Lisp 
programmer eventually develops a good "efficiency model," and the need for 
such declarations becomes obvious. Recent compilers such as CMU's Python 
provide feedback that eases this learning process. 

In summary. Lisp makes it possible to write programs in a wide variety of styles, 

some efficient, some less so. The programmer who writes Lisp programs in the 

same style as C programs will probably find Lisp to be of comparable speed, perhaps 

slightly slower. The programmer who uses some of the more dynamic features of 

Lisp typically finds that it is much easier to develop a working program. Then, if 

the resulting program is not efficient enough, there will be more time to go back 

and improve critical sections. Deciding which parts of the program use the most 

resources is called instrumentation. It is foolhardy to try to improve the efficiency of 

a program without first checking if the improvement will make a real difference. 
One route to efficiency is to use the Lisp prototype as a specification and reimplement 
that specification in a lower-level language, such as C or C++. Some commercial 

<a id='page-269'></a>

AI vendors are taking this route. An alternative is to use Lisp as the language for both 
the prototype and the final implementation. By adding declarations and making 
minor changes to the original program, it is possible to end up with a Lisp program 
that is similar in efficiency to a C program. 

There are four very general and language-independent techniques for speeding 
up an algorithm: 

* Caching the results of computations for later reuse. 
* Compiling so that less work is done at run time. 
* Delaying the computation of partial results that may never be needed. 
* Indexing a data structure for quicker retrieval. 
This chapter covers each of the four techniques in order. It then addresses the 
important problem of instrumentation. The chapter concludes with a case study of 
the s i mpl i fy program. The techniques outlined here result in a 130-fold speed-up in 
this program. 

Chapter 10 concentrates on lower-level "tricks" for improving efficiency further. 

9.1 Caching Results of Previous Computations: 
Memoization 
We start with a simple mathematical function to demonstrate the advantages of 
caching techniques. Later we will demonstrate more complex examples. 

The Fibonacci sequence is defined as the numbers 1,1,2,3,5,8,... where each 
number is the sum of the two previous numbers. The most straightforward function 
to compute the nth number in this sequence is as follows: 

(defun fib (n) 
"Compute the nth number in the Fibonacci sequence." 
(if (<= . 1) 1 

(+ (fib (- . D) (fib (- . 2))))) 

The problem with this function is that it computes the same thing over and over 
again. To compute (fib 5) means computing (fib 4) and (fib 3), but (fib 4) 
also requires (fib 3), they both require (fib 2), and so on. There are ways to rewrite 
the function to do less computation, but wouldn't it be nice to write the function as 
is, and have it automatically avoid redundant computation? Amazingly, there is 
a way to do just that. The idea is to use the function fib to build a new function 
that remembers previously computed results and uses them, rather than recompute 

<a id='page-270'></a>

them. This process is called memoization. The function memo below is a higher-order 
function that takes a function as input and returns a new function that will compute 
the same results, but not do the same computation twice. 

(defun memo (fn) 
"Return a memo-function of fn." 
(let ((table (make-hash-table))) 

#'(lambda (x) 
(multiple-value-bind (val found-p) 
(gethash . table) 

(if found-p 
val 
(setf (gethash . table) (funcall fn x))))))) 

The expression (memo #'fib) will produce a function that remembers its results 
between calls, so that, for example, if we apply it to 3 twice, the first call will do the 
computation of (f i b 3), but the second will just look up the result in a hash table. 
With f i b traced, it would look like this: 

> (setf memo-fib (memo #'fib)) ^ #<CLOSURE -67300731> 

> (funcall memo-fib 3) 
(1 ENTER FIB: 3) 

(2 ENTER FIB: 2) 
(3 ENTER FIB: 1) 
(3 EXIT FIB: 1) 
(3 ENTER FIB: 0) 
(3 EXIT FIB: 1) 

(2 EXIT FIB: 2) 
(2 ENTER FIB: 1) 
(2 EXIT FIB: 1) 

(1 EXIT FIB: 3) 
3 

> (funcall memo-fib 3) 3 

The second time we call memo -fi b with 3 as the argument, the answer is just retrieved 
rather than recomputed. But the problem is that during the computation of (fib 
3), we still compute (f i b 2) multiple times. It would be better if even the internal, 
recursive calls were memoized, but they are calls to f i b, which is unchanged, not to 
memo -fib . We can solve this problem easily enough with the function memoi ze: 

<a id='page-271'></a>
(defun memoize (fn-name) 
"Replace fn-name's global definition with a memoized version." 
(setf (symbol-function fn-name) (memo (symbol-function fn-name)))) 

When passed a symbol that names a function, memoi ze changes the global definition 
of the function to a memo-function. Thus, any recursive calls will go first to the 
memo-function, rather than to the original function. This is just what we want. In 
the following, we contrast the memoized and unmemoized versions of f i b. First, a 
call to (fi b 5) with f i b traced: 

> (fib 5) 
(1 ENTER FIB: 5) 
(2 ENTER FIB: 4) 
(3 ENTER FIB: 3) 

(4 ENTER FIB: 2) 
(5 ENTER FIB: 1) 
(5 EXIT FIB: 1) 
(5 ENTER FIB: 0) 
(5 EXIT FIB: 1) 

(4 EXIT FIB: 2) 
(4 ENTER FIB: 1) 
(4 EXIT FIB: 1) 

(3 EXIT FIB: 3) 

(3 ENTER FIB: 2) 
(4 ENTER FIB: 1) 
(4 EXIT FIB: 1) 
(4 ENTER FIB: 0) 
(4 EXIT FIB: 1) 

(3 EXIT FIB: 2) 
(2 EXIT FIB: 5) 
(2 ENTER FIB: 3) 

(3 ENTER FIB: 2) 
(4 ENTER FIB: 1) 
(4 EXIT FIB: 1) 
(4 ENTER FIB: 0) 
(4 EXIT FIB: 1) 

(3 EXIT FIB: 2) 
(3 ENTER FIB: 1) 
(3 EXIT FIB: 1) 

(2 EXIT FIB: 3) 
(1 EXIT FIB: 8) 
8 

We see that (fib 5) and (fib 4) are each computed once, but (fi b 3) is computed 
twice, (fib 2)threetimes,and (fib 1) five times. Below we call (memoize 'fib) and 
repeat the calculation. This time, each computation is done only once. Furthermore, 

<a id='page-272'></a>

when the computation of (f i b 5) is repeated, the answer is returned immediately 
with no intermediate computation, and a further call to (f i b 6) can make use of the 
valueofCfib 5). 

> (memoize 'fib) => #<CLOSURE 76626607> 

> (fib 5) 

(1 ENTER FIB: 5) 

(2 ENTER FIB: 4) 

(3 ENTER FIB: 3) 

(4 ENTER FIB: 2) 

(5 ENTER FIB: 1) 

(5 EXIT FIB: 1) 

(5 ENTER FIB: 0) 

(5 EXIT FIB: 1) 

(4 EXIT FIB: 2) 

(3 EXIT FIB: 3) 

(2 EXIT FIB: 5) 

(1 EXIT FIB: 8) 

8 

> (fib 5) ^ 8 

> (fib 6) => 

(1 ENTER FIB: 6) 

(1 EXIT FIB: 13) 

13 

Understanding why this works requires a clear understanding of the distinction 
between functions and function names. The original (defun fib ...) form does two 
things: builds a function and stores it as the symbol -function value of f i b. Within 
that function there are two references to f i b; these are compiled (or interpreted) as 
instructions to fetch the symbol - function of f i b and apply it to the argument. 

What memo i ze does is fetch the original function and transform it with memo to a 
function that, when called, will first look in the table to see if the answer is already 
known. If not, the original function is called, and a new value is placed in the table. 
The trick is that memoi ze takes this new function and makes it the symbol - function 
value of the function name. This means that all the references in the original function 
will now go to the new function, and the table will be properly checked on each 
recursive call. One further complication to memo: the function gethash returns both 
the value found in the table and an indicator of whether the key was present or not. 
We use mul ti pi e - va 1 ue - bi nd to capture both values, so that we can distinguish the 
case when nil is the value of the function stored in the table from the case where 
there is no stored value. 

If you make a change to a memoized function, you need to recompile the original 
definition, and then redo the call to memoize. In developing your program, rather 

<a id='page-273'></a>

than saying (memoize *f), it might be easier to wrap appropriate definitions in a 
memoi ze form as follows: 

(memoize 
(defun f (X) ...) 
) 

Or define a macro that combines defun and memoi ze: 

(defmacro defun-memo (fn args &body body) 
"Define a memoized function." 
*(memoize (defun ,fn ,args . ,body))) 

(defun-memo f (x) ...) 

Both of these approaches rely on the fact that defun returns the name of the function 
defined. 

. (fib n) unmemoized memoized memoized up to 
25 121393 1.1 .010 0 
26 196418 1.8 .001 25 
27 317811 2.9 .001 26 
28 514229 4.7 .001 27 
29 832040 8.2 .001 28 
30 1346269 12.4 .001 29 
31 2178309 20.1 .001 30 
32 3524578 32.4 .001 31 
33 5702887 52.5 .001 32 
34 9227465 81.5 .001 33 
50 2.0el0 &mdash; .014 34 
100 5.7e20 &mdash; .031 50 
200 4.5e41 &mdash; .096 100 
500 2.2el04 &mdash; .270 200 
1000 7.0e208 &mdash; .596 500 
1000 7.0e208 &mdash; .001 1000 
1000 7.0e208 &mdash; .876 0 

Now we show a table giving the values of (f i b .) for certain n, and the time in 
seconds to compute the value, before and after (memoi ze ' f i b). For larger values 
of ., approximations are shown in the table, although f i b actually returns an exact 
integer. With the unmemoized version, I stopped at . = 34, because the times were 
getting too long. For the memoized version, even . = 1000 took under a second. 

<a id='page-274'></a>

Note there are three entries for (f i b 1000). The first entry represents the incremental 
computation when the table contains the memoized values up to 500, the 
second entry shows the time for a table lookup when (fib 1000) is already computed, 
and the third entry is the time for a complete computation starting with an 
empty table. 

It should be noted that there are two general approaches to discussing the efficiency 
of an algorithm. One is to time the algorithm on representative inputs, as we 
did in this table. The other is to analyze the asymptotic complexity of the algorithm. For 
the f i b problem, an asymptotic analysis considers how long it takes to compute (fib 
n) as n approaches infinity. The notation 0(/(n)) is used to describe the complexity. 
For example, the memoized version f i b is an 0(n) algorithm because the computation 
time is bounded by some constant times n, for any value of n. The unmemoized 
version, it turns out, is O (1.7^), meaning computing f i b of n+1 can take up to 1.7 times 
as long as f i b of n. In simpler terms, the memoized version has linear complexity, 
while the unmemoized version has exponential complexity. Exercise 9.4 ([page 308](chapter9.md#page-308)) 
describes where the 1.7 comes from, and gives a tighter bound on the complexity. 

The version of memo presented above is inflexible in several ways. First, it only 
works for functions of one argument. Second, it only returns a stored value for 
arguments that are eql, because that is how hash tables work by default. For some 
applications we want to retrieve the stored value for arguments that are equa 1. Third, 
there is no way to delete entries from the hash table. In many applications there are 
times when it would be good to clear the hash table, either because it has grown too 
large or because we have finished a set of related problems and are moving on to a 
new problem. 

The versions of memo and memoi ze below handle these three problems. They are 
compatible with the previous version but add three new keywords for the extensions. 
The name keyword stores the hash table on the property list of that name, so it can 
be accessed by cl ear-memoi ze. The test kejword tells what kind of hash table to 
create: eq, eql, or equal. Finally, the key keyword tells which arguments of the 
function to index under. The default is the first argument (to be compatible with the 
previous version), but any combination of the arguments can be used. If you want 
to use all the arguments, specify 1 dent i ty as the key. Note that if the key is a Ust of 
arguments, then you will have to use equal hash tables. 

(defun memo (fn name key test) 

"Return a memo-function of fn. " 

(let ((table (make-hash-table :test test))) 

(setf (get name 'memo) table) 

#'(lambda (&rest args) 

(let ((k (funcall key args))) 

(multiple-value-bind (val found-p) 

(gethash k table) 

(if found-p val 

<a id='page-275'></a>
(setf (gethash k table) (apply fn args)))))))) 

(defun memoize (fn-name &key (key #*first) (test #'eql)) 
"Replace fn-name's global definition with a memoized version." 
(setf (symbol-function fn-name) 

(memo (symbol-function fn-name) fn-name key test))) 

(defun clear-memoize (fn-name) 
"Clear the hash table from a memo function." 
(let ((table (get fn-name 'memo))) 

(when table (clrhash table)))) 

9.2 Compiling One Language into Another 
In chapter 2 we defined a new language—the language of grammar rules—which was 
processed by an interpreter designed especially for that language. An interpreter is 
a program that looks at some data structure representing a "program" or sequence 
of rules of some sort and interprets or evaluates those rules. This is in contrast to a 
compiler, which translates some set of rules in one language into a program in another 
language. 

The function generate was an interpreter for the "language" defined by the set of 
grammar rules. Interpreting these rules is straightforward, but the process is somewhat 
inefficient, in that generate must continually search through the *gramma r* to 
find the appropriate rule, then count the length of the right-hand side, and so on. 

A compiler for this rule-language would take each rule and translate it into a function. 
These functions could then call each other with no need to search through the 
*grammar*. We implement this approach with the function compi 1 e - rul e. It makes 
use of the auxiliary functions one-of and rule-lhs and rule-rhs from [page 40](chapter2.md#page-40), 
repeated here: 

(defun rule-lhs (rule) 
"The left-hand side of a rule." 
(first rule)) 

(defun rule-rhs (rule) 
"The right-hand side of a rule." 
(rest (rest rule))) 

(defun one-of (set) 
"Pick one element of set, and make a list of it." 
(list (random-elt set))) 

<a id='page-276'></a>

(defun random-elt (choices) 
"Choose an element from a list at random." 
(elt choices (random (length choices)))) 

The function compile-rule turns a rule into a function definition by building up 
Lisp code that implements all the actions that generate would take in interpreting 
the rule. There are three cases. If every element of the right-hand side is an atom, 
then the rule is a lexical rule, which compiles into a call to one-of to pick a word at 
random. If there is only one element of the right-hand side, then bui 1 d - code is called 
to generate code for it. Usually, this will bea call to append to build up a list. Finally, 
if there are several elements in the right-hand side, they are each turned into code 
by build-code; are given a number by build-cases; and then a case statement is 
constructed to choose one of the cases. 

(defun compile-rule (rule) 
"Translate a grammar rule into a LISP function definition." 
(let ((rhs (rule-rhs rule))) 

'(defun ,(rule-lhs rule) () 

.(cond ((every #*atom rhs) *(one-of '.rhs)) 
((length=1 rhs) (build-code (first rhs))) 
(t '(case (random .(length rhs)) 

.(build-cases 0 rhs))))))) 

(defun build-cases (number choices) 
"Return a list of case-clauses" 
(when choices 

(cons (list number (build-code (first choices))) 
(build-cases (+ number 1) (rest choices))))) 

(defun build-code (choice) 
"Append together multiple constituents" 
(cond ((null choice) nil) 

((atom choice) (list choice)) 
((length=1 choice) choice) 
(t '(append .(mapcar #'build-code choice))))) 

(defun length=1 (x) 
"Is X a list of length 1?" 
(and (consp x) (null (rest x)))) 

The Lisp code built by compile-rule must be compiled or interpreted to make it 
available to the Lisp system. We can do that with one of the following forms. 
Normally we would want to call compi1 e, but during debugging it may be easier 
not to. 

<a id='page-277'></a>
(dolist (rule ^grammar*) (eval (compile-rule rule))) 
(dolist (rule *grammar*) (compile (eval (compile-rule rule)))) 

One frequent way to use compilation is to define a macro that expands into the code 
generated by the compiler. That way, we just type in calls to the macro and don't 
have to worry about making sure all the latest rules have been compiled. We might 
implement this as follows: 

(defmacro defrule (&rest rule) 
"Define a grammar rule" 
(compile-rule rule)) 

(defrule Sentence -> (NP VP)) 

(defrule NP -> (Art Noun)) 

(defrule VP -> (Verb NP)) 

(defrule Art -> the a) 

(defrule Noun -> man ball woman table) 

(defrule Verb -> hit took saw liked) 

Actually, the choice of using one big list of rules (like *g r ammar *) versus using individual 
macros to define rules is independent of the choice of compiler versus interpreter. 
Wecould justas easily definedef rule simply to push the ruleonto*grammar*. Macros 
like def rul e are useful when you want to define rules in different places, perhaps in 
several separate files. The def parameter method is appropriate when all the rules 
can be defined in one place. 

We can see the Lisp code generated by compi 1 e - rul e in two ways: by passing it 
a rule directly: 

> (compile-rule '(Sentence -> (NP VP))) 
(DEFUN SENTENCE () 
(APPEND (NP) (VP))) 

> (compile-rule '(Noun -> man ball woman table)) 
(DEFUN NOUN () 
(ONE-OF '(MAN BALL WOMAN TABLE))) 

or by macroexpanding a def rul e expression. The compiler was designed to produce 
the same code we were writing in our first approach to the generation problem (see 
[page 35](chapter2.md#page-35)). 

<a id='page-278'></a>

> (macroexpand '(defrule Adj* -> () Adj (AdJ Adj*))) 
(DEFUN ADJ* () 

(CASE (RANDOM 3) 
(0 NIL) 
(1 (ADJ)) 
(2 (APPEND (ADJ) (ADJ*))))) 

Interpreters are usually easier to write than compilers, although in this case, even 
the compiler was not too difficult. Interpreters are also inherently more flexible than 
compilers, because they put off making decisions until the last possible moment. 
For example, our compiler considers the right-hand side of a rule to be a list of words 
only if every element is an atom. In all other cases, the elements are treated as 
nonterminals. This could cause problems if we extended the definition of Noun to 
include the compound noun "chow chow": 

(defrule Noun -> man ball woman table (chow chow)) 

The rule would expand into the following code: 

(DEFUN NOUN () 

(CASE (RANDOM 5) 
(0 (MAN)) 
(1 (BALD) 
(2 (WOMAN)) 
(3 (TABLE)) 
(4 (APPEND (CHOW) (CHOW))))) 

The problem is that ma. and ball and all the others are suddenly treated as functions, 
not as literal words. So we would get a run-time error notifying us of undefined 
functions. The equivalent rule would cause no trouble for the interpreter, which waits 
until it actually needs to generate a symbol to decide if it is a word or a nonterminal. 
Thus, the semantics of rules are different for the interpreter and the compiler, and 
we as program implementors have to be very careful about how we specify the actual 
meaning of a rule. In fact, this was probably a bug in the interpreter version, since 
it effectively prohibits words like "noun" and "sentence" from occurring as words if 
they are also the names of categories. One possible resolution of the conflict is to 
say that an element of a right-hand side represents a word if it is an atom, and a list 
of categories if it is a list. If we did indeed settle on that convention, then we could 
modify both the interpreter and the compiler to comply with the convention. Another 
possibility would be to represent words as strings, and categories as symbols. 

The flip side of losing run-time flexibility is gaining compile-time diagnostics. For 
example, it turns out that on the Common Lisp system I am currently using, I get 
some useful error messages when I try to compile the buggy version of Noun: 

<a id='page-279'></a>
> (defrule Noun -> man ball woman table (chow chow)) 
The following functions were referenced but don't seem defined: 

CHOW referenced by NOUN 

TABLE referenced by NOUN 

WOMAN referenced by NOUN 

BALL referenced by NOUN 

MAN referenced by NOUN 
NOUN 

Another problem with the compilation scheme outlined here is the possibility of name 
clashes. Under the interpretation scheme, the only names used were the function 
generate and the variable ^grammar*. With compilation, every left-hand side of a 
rule becomes the name of a function. The grammar writer has to make sure he or 
she is not using the name of an existing Lisp function, and hence redefining it. Even 
worse, if more than one grammar is being developed at the same time, they cannot 
have any functions in common. If they do, the user will have to recompile with 
every switch from one grammar to another. This may make it difficult to compare 
grammars. The best away around this problem is to use the Common Lisp idea of 
packages, but for small exercises name clashes can be avoided easily enough, so we 
will not explore packages until section 24.1. 

The major advantage of a compiler is speed of execution, when that makes a 
difference. For identical grammars running in one particular implementation of 
Common Lisp on one machine, our interpreter generates about 75 sentences per 
second, while the compiled approach turns out about 200. Thus, it is more than twice 
as fast, but the difference is negligible unless we need to generate many thousands of 
sentences. In section 9.6 we will see another compiler with an even greater speed-up. 

The need to optimize the code produced by your macros and compilers ultimately 
depends on the quality of the underlying Lisp compiler. For example, consider the 
following code: 

> (defun fl (n 1) 
(let ((11 (first D) 
(12 (second 1))) 
(expt (* 1 (+ . 0)) 
(- 4 (length (list 11 12)))))) 
Fl 

> (defun f2 (n 1) (* . n)) F2 

> (disassemble 'fl ) 
6 PUSH ARG 10 ; . 
7 MOVEM PDL-PUSH 
8 * PDL-POP 
9 RETURN PDL-POP 
Fl 

<a id='page-280'></a>

> (disassemble 'f2) 

6 PUSH ARGO ; . 

7 MOVEM PDL-PUSH 

8 * PDL-POP 

9 RETURN PDL-POP 

F2 

This particular Lisp compiler generates the exact same code for f 1 and f 2. Both 
fimctions square the argument n, and the four machine instructions say, "Take the 
0th argument, make a copy of it, multiply those two numbers, and return the result." 
It's clear the compiler has some knowledge of the basic Lisp functions. In the case 
of f 1, it was smart enough to get rid of the local variables 11 and 12 (and their 
initialization), as well as the calls to first, second, length, and 1 i st and most of the 
arithmetic. The compiler could do this because it has knowledge about the functions 
length and 1 i st and the arithmetic functions. Some of this knowledge might be in 
the form of simplification rules. 

As a user of this compiler, there's no need for me to write clever macros or 
compilers that generate streamlined code as seen in f 2; I can blindly generate code 
with possible inefficiencies Uke those in f 1, and assume that the Lisp compiler 
will cover up for my laziness. With another compiler that didn't know about such 
optimizations, I would have to be more careful about the code I generate. 

9.3 Delaying Computation 
Back on [page 45](chapter2.md#page-45), we saw a program to generate all strings derivable from a grammar. 
One drawback of this program was that some grammars produce an infinite number 
of strings, so the program would not terminate on those grammars. 

It turns out that we often want to deal with infinite sets. Of course, we can't 
enumerate all the elements of an infinite set, but we should be able to represent the 
set and pick elements out one at a time. In other words, we want to be able to specify 
how a set (or other object) is constructed, but delay the actual construction, perhaps 
doing it incrementally over time. This soimds like a job for closures: we can specify 
the set constructor as a function, and then call the function some time later. We will 
implement this approach with the sjmtax used in Scheme—the macro del ay builds a 
closure to be computed later, and the function force calls that function and caches 
away the value. We use structures of type del ay to implement this. A delay structure 
has two fields: the value and the function. Initially, the value field is undefined, and 
the function field holds the closure that will compute the value. The first time the 
delay is forced, the function is called, and its result is stored in the value field. The 
function field is then set to nil to indicate that there is no need to call the function 
again. The function force checks if the fimction needs to be called, and returns the 

<a id='page-281'></a>

value. If force is passed an argument that is not a delay, it just returns the argument. 

(defstruct delay (value nil) (function nil)) 

(defmacro delay (&rest body) 
"A computation that can be executed later by FORCE." 
*(make-delay :function #'(lambda () . .body))) 

(defun force (x) 
"Find the value of x. by computing if it is a delay." 
(if (not (delay-p x)) 

. 
(progn 
(when (delay-function x) 
(setf (delay-value x) 
(funcall (delay-function x))) 
(setf (delay-function x) nil)) 
(delay-value x)))) 

Here's an example of the use of del ay. The list . is constructed using a combination 
of normal evaluation and delayed evaluation. Thus, the 1 is printed when . is created, 
but the 2 is not: 

> (setf X (list (print 1) (delay (print 2)))) 

1 

(1 #S(DELAY .-FUNCTION (LAMBDA () (PRINT 2)))) 

The second element is evaluated (and printed) when it is forced. But then forcing it 
again just retrieves the cached value, rather than calling the function again: 

> (force (second x)) 

2 

2 

> X => (1 #S(DELAY :VALUE 2)) 

> (force (second x)) 2 

Now let's see how delays can be used to build infinite sets. An infinite set will be 
considered a special case of what we will call a pipe: a list with a first component 
that has been computed, and a rest component that is either a normal list or a 
delayed value. Pipes have also been called delayed Usts, generated lists, and (most 
commonly) streams. We will use the term pipe because stream already has a meaning 
in Common Lisp. The bookArtificial Intelligence Programming (Charniak et al. 1987) 

<a id='page-282'></a>

also calls these structures pipes, reserving streams for delayed structures that do not 
cache computed results. 

To distinguish pipes from lists, we will use the accessors head and tai 1 instead 
of first and rest. We will also use empty-pipe instead of ni 1, make-pipe instead 
of cons, and pipe-el t instead of el t. Note that make-pipe is a macro that delays 
evaluation of the tail. 

(defmacro make-pipe (head tail) 
"Create a pipe by evaluating head and delaying tail." 
'(cons .head (delay .tail))) 

(defconstant empty-pipe nil) 

(defun head (pipe) (first pipe)) 
(defun tail (pipe)(force (rest pipe))) 

(defun pipe-elt (pipe i) 
"The i-th element of a pipe. 0-based" 
(if (= i 0) 

(head pipe) 
(pipe-elt (tail pipe) (-i 1)))) 

Here's a function that can be used to make a large or infinite sequence of integers 
with delayed evaluation: 

(defun integers (&optional (start 0) end) 
"A pipe of integers from START to END. 
If END is nil. this is an infinite pipe." 
(if (or (null end) (<= start end)) 

(make-pipe start (integers (+ start 1) end)) 
nil)) 

And here is an example of its use. The pipe c represents the numbers from 0 to infinity. 
When it is created, only the zeroth element, 0, is evaluated. The computation 
of the other elements is delayed. 

> (setf c (integers 0)) ^ (0 . #S(DELAY :FUNCTION #<CLOSURE -77435477>)) 

> (pipe-elt c 0) =.> 0 

Calling pi pe - el t to look at the third element causes the first through third elements 
to be evaluated. The numbers 0 to 3 are cached in the correct positions, and further 
elements remain unevaluated. Another call to pi pe-el t with a larger index would 
force them by evaluating the delayed function. 

<a id='page-283'></a>
> (pipe-elt c 3) ^ 3 

> c => 

(0 . #S(DELAY 
:VALUE 
(1 . #S(DELAY 
:VALUE 
(2 . #S(DELAY 
:VALUE 
(3 . #S(DELAY 
:FUNCTION 
#<CLOSURE -77432724>))))))) 
While this seems to work fine, there is a heavy price to pay. Every delayed value must 
be stored in a two-element structure, where one of the elements is a closure. Thus, 
there is some storage wasted. There is also some time wasted, as ta . or pi pe-elt 
must traverse the structures. 

An alternate representation for pipes is as(value. closure) pairs, where the closure 
values are stored into the actual cons cells as they are computed. Previously we 
needed structures of type del ay to distinguish a delayed from a nondelayed object, 
but in a pipe we know the rest can be only one of three things: nil, a list, or a delayed 
value. Thus, we can use the closures directly instead of using del ay structures, if we 
have some way of distinguishing closures from lists. Compiled closures are atoms, so 
they can always be distinguished from lists. But sometimes closures are implemented 
as lists beginning with 1 ambda or some other implementation-dependent symbol.^ 
The built-in function functionp is defined to be true of such lists, as well as of all 
symbols and all objects returned by compi 1 e. But using functionp means that we 
can not have a pipe that includes the symbol 1 ambda as an element, because it will be 
confused for a closure: 

> (functionp (last '(theta iota kappa lambda))) ^ . 

If we consistently use compiled functions, then we could eliminate the problem by 
testing with the built-in predicate compi 1 ed-function-p. The following definitions 
do not make this assumption: 

(defmacro make-pipe (head tail) 
"Create a pipe by evaluating head and delaying tail." 
'(cons .head #'(lambda () .tail))) 

^In KCL, the symbol 1 ambda -cl osure is used, and in Allegro, it is excl:. 1 exi cal - cl osure. 

<a id='page-284'></a>

(defun tail (pipe) 
"Return tail of pipe or list, and destructively update 
the tail if it is a function." 
(if (functionp (rest pipe)) 

(setf (rest pipe) (funcall (rest pipe))) 
(rest pipe))) 

Everything else remains the same. If we recompile integers (because it uses the 
macro make -pi pe), we see the following behavior. First, creation of the infinite pipe 
c is similar: 

> (setf c (integers 0)) (0 , #<CLOSURE 77350123>) 

> (pipe-elt c 0) => 0 

Accessing an element of the pipe forces evaluation of all the intervening elements, 
and as before leaves subsequent elements unevaluated: 

> (pipe-elt c 5) => 5 

> c => (0 1 2 3 4 5 . #<CLOSURE 77351636> 

Pipes can also be used for finite lists. Here we see a pipe of length 11: 

> (setf i (integers 0 10)) => (0 . #<CLOSURE 77375357>) 

> (pipe-elt i 10) ^ 10 

> (pipe-elt i 11) => NIL 

> i ^ (0 1 2 3 4 5 6 7 8 9 10) 

Clearly, this version wastes less space and is much neater about cleaning up after 
itself. In fact, a completely evaluated pipe turns itself into a list! This efficiency was 
gained at the sacrifice of a general principle of program design. Usually we strive 
to build more complicated abstractions, like pipes, out of simpler ones, like delays. 
But in this case, part of the functionality that delays were providing was duplicated 
by the cons cells that make up pipes, so the more efficient implementation of pipes 
does not use delays at all. 

Here are some more utility functions on pipes: 

(defun enumerate (pipe &key count key (result pipe)) 
"Go through all (or count) elements of pipe, 
possibly applying the KEY function. (Try PRINT.)" 

Returns RESULT, which defaults to the pipe itself, 
(if (or (eq pipe empty-pipe) (eql count 0)) 

<a id='page-285'></a>
result 

(progn 
(unless (null key) (funcall key (head pipe))) 
(enumerate (tail pipe) :count (if count (- count 1)) 

:key key :result result)))) 

(defun filter (pred pipe) 
"Keep only items in pipe satisfying pred." 
(if (funcall pred (head pipe)) 

(make-pipe (head pipe) 
(filter pred (tail pipe))) 
(filter pred (tail pipe)))) 

And here's an application of pipes: generating prime numbers using the sieve of 
Eratosthenes algorithm: 

(defun sieve (pipe) 
(make-pipe (head pipe) 
(filter #'(lambda (x) (/= (mod . (headpipe)) 0)) 
(sieve (tail pipe))))) 

(defvar *primes* (sieve (integers 2))) 

> *primes* ^ (2 . #<CLOSURE 3075345>) 

> (enumerate *primes* icount 10) => 
(2 3 5 7 11 13 17 19 23 29 31 . #<CLOSURE 5224472> 

Finally, let's return to the problem of generating all strings in a grammar. First we're 
going to need some more utility functions: 

(defun map-pipe (fn pipe) 
"Map fn over pipe, delaying all but the first fn call." 
(if (eq pipe empty-pipe) 

empty-pipe 
(make-pipe (funcall fn (head pipe)) 
(map-pipe fn (tail pipe))))) 

(defun append-pipes (x y) 
"Return a pipe that appends the elements of . and y." 
(if (eq X empty-pipe) 

y 
(make-pipe (head x) 
(append-pipes (tail x) y)))) 

<a id='page-286'></a>

(defun mappend-pipe (fn pipe) 
"Lazily map fn over pipe, appending results." 
(if (eq pipe empty-pipe) 

empty-pipe 
(let ((X (funcall fn (head pipe)))) 
(make-pipe (head x) 
(append-pipes (tail x) 
(mappend-pipe 
fn (tail pipe))))))) 

Now we can rewrite generate-all and combine-all to use pipes instead of lists. 
Everything else is the same as on [page 45](chapter2.md#page-45). 

(defun generate-all (phrase) 
"Generate a random sentence or phrase" 
(if (listp phrase) 

(if (null phrase) 
(list nil) 
(combine-all-pipes 

(generate-all (first phrase)) 
(generate-all (rest phrase)))) 
(let ((choices (rule-rhs (assoc phrase *grammar*)))) 

(if choices 
(mappend-pipe #*generate-all choices) 
(list (list phrase)))))) 

(defun combine-all-pipes (xpipe ypipe) 
"Return a pipe of pipes formed by appending a y to an x" 
;; In other words, form the cartesian product, 
(mappend-pipe 

#'(lambda (y) 
(map-pipe #'(lambda (x) (append-pipes . y)) 
xpipe)) 
ypipe)) 

With these definitions, here's the pipe of all sentences from *grammar2* (from 
[page 43](chapter2.md#page-43)): 

> (setf ss (generate-all 'sentence)) 
((THE . #<CLOSURE 27265720>) . #<CLOSURE 27266035> 

<a id='page-287'></a>

> (enumerate ss rcount 5) =i> 

((THE . #<CLOSURE 27265720>) 

(A . #<CLOSURE 27273143> 
(THE . #<CLOSURE 27402545>) 
(A . #<CLOSURE 27404344>) 
(THE . #<CLOSURE 27404527>) 
(A . #<CLOSURE 27405473> . #<CLOSURE 27405600>) 
> (enumerate ss .-count 5 :key #'enumerate) 

((THE MAN HIT THE MAN) 
(A MAN HIT THE MAN) 
(THE BIG MAN HIT THE MAN) 
(A BIG MAN HIT THE MAN) 
(THE LITTLE MAN HIT THE MAN) 
(THE . #<CLOSURE 27423236>) . #<CL0SURE 27423343> 

> (enumerate (pipe-elt ss 200)) 

(THE ADIABATIC GREEN BLUE MAN HIT THE MAN) 

While we were able to represent the infinite set of sentences and enumerate instances 
of it, we still haven't solved all the problems. For one, this enumeration will never 
get to a sentence that does not have "hit the man" as the verb phrase. We will see 
longer and longer lists of adjectives, but no other change. Another problem is that 
left-recursive rules will still cause infinite loops. For example, if the expansion for 
Adj*hadbeen (Adj* -> (Adj*Adj) ()) instead of (Adj* -> () (Adj Adj*)), 
then the enumeration would never terminate, because pipes need to generate a first 
element. 

We have used delays and pipes for two main purposes: to put off until later 
computations that may not be needed at all, and to have an expHcit representation of 
large or infinite sets. It should be mentioned that the language Prolog has a different 
solution to the first problem (but not the second). As we shall see in chapter 11, Prolog 
generates solutions one at a time, automatically keeping track of possible backtrack 
points. Where pipes allow us to represent an infinite number of alternatives in the 
data, Prolog allows us to represent those alternatives in the program itself. 

&#9635; Exercise 9.1 [h] When given a function f and a pipe p. mappend-pipe returns a 
new pipe that will eventually enumerate all of ( f (first .)), then all of ( f (second 
.)), and so on. This is deemed "unfair" if ( f (first .)) has an infinite number of 
elements. Define a function that will fairly interleave elements, so that all of them are 
eventually enumerated. Show that the function works by changing generate -a 11 to 
work with it. 

<a id='page-288'></a>

9.4 Indexing Data 
Lisp makes it very easy to use lists as the universal data structure. A list can represent 
a set or an ordered sequence, and a list with sublists can represent a tree or graph. 
For rapid prototyping, it is often easiest to represent data in lists, but for efficiency 
this is not always the best idea. To find an element in a list of length . will take n/2 
steps on average. This is true for a simple list, an association list, or a property list. 
If . can be large, it is worth looking at other data structures, such as hash tables, 
vectors, property lists, and trees. 

Picking the right data structure and algorithm is as important in Lisp as it is in 
any other programming language. Even though Lisp offers a wide variety of data 
structures, it is often worthwhile to spend some effort on building just the right data 
structure for frequently used data. For example. Lisp's hash tables are very general 
and thus can be inefficient. You may want to build your own hash tables if, for 
example, you never need to delete elements, thus making open hashing an attractive 
possibility. We will see an example of efficient indexing in section 9.6 ([page 297](chapter9.md#page-297)). 

9.5 Instrumentation: Deciding What 
to Optimize 
Because Lisp is such a good rapid-prototyping language, we can expect to get a 
working implementation quickly. Before we go about trying to improve the efficiency 
of the implementation, it is a good idea to see what parts are used most often. 
Improving little-used features is a waste of time. 

The minimal support we need is to count the number of calls to selected functions, 
and then print out the totals. This is called profiling the functions.^ For each function 
to be profiled, we change the definition so that it increments a counter and then calls 
the original function. 

Most Lisp systems have some built-in profiling mechanism. If your system has 
one, by all means use it. The code in this section is provided for those who lack such 
a feature, and as an example of how functions can be manipulated. The following is 
a simple profiling facility. For each profiled function, it keeps a count of the number 
of times it is called under the prof i 1 e - count property of the function's name. 

^The terms metering and monitoring are sometimes used instead of profiling. 

<a id='page-289'></a>
(defun profilel (fn-name) 

"Make the function count how often it is called" 
First save away the old, unprofiled function 
Then make the name be a new function that increments 
a counter and then calls the original function 

(let ((fn (symbol-function fn-name))) 
(setf (get fn-name 'unprofiled-fn) fn) 
(setf (get fn-name 'profile-count) 0) 
(setf (symbol-function fn-name) 

(profiled-fn fn-name fn)) 
fn-name)) 

(defun unprofilel (fn-name) 
"Make the function stop counting how often it is called." 
(setf (symbol-function fn-name) (get fn-name 'unprofiled-fn)) 
fn-name) 

(defun profiled-fn (fn-name fn) 
"Return a function that increments the count." 
#'(lambda (&rest args) 

(incf (get fn-name 'profile-count)) 

(apply fn args))) 

(defun profile-count (fn-name) (get fn-name 'profile-count)) 

(defun profile-report (fn-names &optional (key #'profile-count)) 
"Report profiling statistics on given functions." 
(loop for name in (sort fn-names #'> :key key) do 

(format t "~&~7D ~A" (profile-count name) name))) 

That's all we need for the bare-bones functionality. However, there are a few ways 
we could improve this. First, it would be nice to have macros that, like trace and 
untrace, allow the user to profile multiple functions at once and keep track of what 
has been profiled. Second, it can be helpful to see the length of time spent in each 
function, as well as the number of calls. 

Also, it is important to avoid profiling a function twice, since that would double 
the number of calls reported without alerting the user of any trouble. Suppose we 
entered the following sequence of commands: 

(defun f (X) (g x)) 
(profilel 'f) 
(profilel 'f) 

Then the definition of f would be roughly: 

<a id='page-290'></a>

(lambda (&rest args) 
(incf (get 'f 'profile-count)) 
(apply #'(lambda (&rest args) 

(incf (get 'f 'profile-count)) 
(apply #'(lambda (x) (g x)) 
args)) 
args)) 

The result is that any call to f will eventually call the original f, but only after 
incrementing the count twice. 

Another consideration is what happens when a profiled function is redefined by 
the user. The only way we could ensure that a redefined function would continue 
profiling would be to change the definition of the macro defun to look for functions 
that should be profiled. Changing system functions like defun is a risky prospect, 
and in Common Lisp the Language, 2d edition, it is explicitly disallowed. Instead, 
we'll do the next best thing: ensure that the next call to prof i 1 e will reprofile any 
functions that have been redefined. We do this by keeping track of both the original 
unprofiled function and the profiled function. We also keep a list of all functions 
that are currently profiled. 

In addition, we will count the amount of time spent in each function. However, 
the user is cautioned not to trust the timing figures too much. First, they include the 
overhead cost of the profiling facility. This can be significant, particularly because 
the facility conses, and thus can force garbage collections that would not otherwise 
have been done. Second, the resolution of the system clock may not be fine enough 
to make accurate timings. For functions that take about 1/10 of a second or more, the 
figures will be reliable, but for quick functions they may not be. 

Here is the basic code for prof i 1 e and unprof i 1 e: 

(defvar *profiled-functions* nil 
"Function names that are currently profiled") 

(defmacro profile (&rest fn-names) 
"Profile fn-names. With no args, list profiled functions." 
'(mapcar #'profilel 

(setf *profiled-functions* 
(union *profiled-functions* ',fn-names)))) 

(defmacro unprofile (&rest fn-names) 
"Stop profiling fn-names. With no args, stop all profiling." 
'(progn 

(mapcar #'unprofilel 
,(if fn-names ",fn-names '*profiled-functions*)) 
(setf *profiled-functions* 
.(if (null fn-names) 
nil 

<a id='page-291'></a>
'(set-difference *profiled-functions* 
*,fn-names))))) 

The idiom *',fn-names deserves comment, since it is common but can be confusing 
at first. It may be easier to understand when written in the equivalent form 
' (quote ,fn-names). As always, the backquote builds a structure with both constant 
and evaluated components. In this case, the quote is constant and the variable 
fn-names is evaluated. In MacLisp, the function kwote was defined to serve this 
purpose: 

(defun kwote (x) (list 'quote x)) 

Now we need to change prof i1 el and unprof i1 el to do the additional bookkeeping: 
For prof i1 el, there are two cases. If the user does a prof i 1 el on the same function 
name twice in a row, then on the second time we will notice that the current function 
is the same as the functioned stored under the profiled-fn property, so nothing 
more needs to be done. Otherwise, we create the profiled function, store it as the 
current definition of the name under the prof i1 ed-f . property, save the unprofiled 
function, and initialize the counts. 

(defun profilel (fn-name) 

"Make the function count how often it is called" 
First save away the old, unprofiled function 
Then make the name be a new function that increments 
a counter and then calls the original function 

(let ((fn (symbol-function fn-name))) 
(unless (eq fn (get fn-name 'profiled-fn)) 
(let ((new-fn (profiled-fn fn-name fn))) 

(setf (symbol-function fn-name) new-fn 
(get fn-name 'profiled-fn) new-fn 
(get fn-name 'unprofiled-fn) fn 
(get fn-name 'profile-time) 0 
(get fn-name 'profile-count) 0)))) 

fn-name) 

(defun unprofilel (fn-name) 
"Make the function stop counting how often it is called." 
(setf (get fn-name 'profile-time) 0) 
(setf (get fn-name 'profile-count) 0) 
(when (eq (symbol-function fn-name) (get fn-name 'profiled-fn)) 

;; normal case: restore unprofiled version 
(setf (symbol-function fn-name) 
(get fn-name 'unprofiled-fn))) 
fn-name) 

<a id='page-292'></a>

Now we look into the question of timing. There is a built-in Common Lisp function, 
get-internal -real -time, that returns the elapsed time since the Lisp session 
started. Because this can quickly become a bignum, some implementations 
provide another timing function that wraps around rather than increasing forever, 
but which may have a higher resolution than get-internal - real - time. For example, 
on TI Explorer Lisp Machines, get-internal-real-time measures 1/60second 
intervals, while time:microsecond-time measures l/l,000,000-second intervals, 
but the value returned wraps around to zero every hour or so. The function 
time:microsecond-time-difference is used to compare two of these numbers 
with compensation for wraparound, as long as no more than one wraparound 
has occurred. 

In the code below, I use the conditional read macro characters #+ and #- to define 
the right behavior on both Explorer and non-Explorer machines. We have seeen 
that # is a special character to the reader that takes different action depending on 
the following character. For example, #' f . is read as (function f .). The character 
sequence #+ is defined so that ^+feature expression reads as expression if thefeature is 
defined in the current implementation, and as nothing at all if it is not. The sequence 
#- acts in just the opposite way. For example, on a TI Explorer, we would get the 
following: 

> '(hi #+TI t #+Symbolics s #-Explorer e #-Mac m) ^ (HI . .) 

The conditional read macro characters are used in the following definitions: 

(defun get-fast-time () 
"Return the elapsed time. This may wrap around; 
use FAST-TIME-DIFFERENCE to compare." 
#+Explorer (time:microsecond-time) ; do this on an Explorer 
#-Explorer (get-internal-real-time)) ; do this on a non-Explorer 

(defun fast-time-difference (end start) 
"Subtract two time points." 
#+Explorer (time:microsecond-time-difference end start) 
#-Explorer (- end start)) 

(defun fast-time->seconds (time) 
"Convert a fast-time interval into seconds." 
#+Explorer (/ time 1000000.0) 
#-Explorer (/ time internal-time-units-per-second)) 

The next step is to update prof i1 ed - f . to keep track of the timing data. The simplest 
way to do this would be to set a variable, say start, to the time when a function is 
entered, run the function, and then increment the function's time by the difference between 
the current time and start. The problem with this approach is that every func


<a id='page-293'></a>
tion in the call stack gets credit for the time of each called function. Suppose the function 
f calls itself recursively five times, with each call and return taking place a second 
apart, so that the whole computation takes nine seconds. Then f will be charged nine 
seconds for the outer call, seven seconds for the next call, and so on, for a total of 
25 seconds, even though in reality it only took nine seconds for all of them together. 

A better algorithm would be to charge each function only for the time since the 
last call or return. Then f would only be charged the nine seconds. The variable 
*prof i le-call -stack* is used to holdastack of functionname/entry time pairs. This 
stack is manipulated by prof i 1 e-enter and prof i 1 e-exi t to get the right timings. 

The functions that are used on each call to a profiled function are declared inline. 
In most cases, a call to a function compiles into machine instructions that set up the 
argument list and branch to the location of the function's definition. With an i nl i ne 
function, the body of the function is compiled in line at the place of the function 
call. Thus, there is no overhead for setting up the argument list and branching to the 
definition. An i nl i ne declaration can appear anywhere any other declaration can 
appear. In this case, the function proel aim is used to register a global declaration. 
Inline declarations are discussed in more depth on [page 317](chapter10.md#page-317). 

(proclaim '(inline profile-enter profile-exit inc-profile-time)) 

(defun profiled-fn (fn-name fn) 
"Return a function that increments the count, and times." 
#'(lambda (&rest args) 

(profile-enter fn-name) 

(multiple-value-progl 
(apply fn args) 
(profile-exit fn-name)))) 

(defvar *profile-call-stack* nil) 

(defun profile-enter (fn-name) 
(incf (get fn-name 'profile-count)) 
(unless (null *profile-call-stack*) 

Time charged against the calling function: 
(inc-profile-time (first *profile-call-stack*) 

(car (first *profile-call-stack*)))) 
;; Put a new entry on the stack 
(push (cons fn-name (get-fast-time)) 

*profi le-call-stack*)) 

(defun profile-exit (fn-name) 
Time charged against the current function: 
(inc-profile-time (pop *profile-call-stack*) 
fn-name) 
Change the top entry to reflect current time 
(unless (null *profile-call-stack*) 
(setf (cdr (first *profile-call-stack*)) 
(get-fast-time)))) 

<a id='page-294'></a>

(defun inc-profile-time (entry fn-name) 
(incf (get fn-name 'profile-time) 
(fast-time-difference (get-fast-time) (cdr entry)))) 

Finally, we need to update prof i 1 e- report to print the timing data as well as the 
counts. Note that the default f .- names is a copy of the global list. That is because we 
pass f .- names to sort, which is a destructive function. We don't want the global list 
to be modified as a result of this sort. 

(defun profile-report (&optional 
(fn-names (copy-list *profiled-functions*)) 
(key #'profile-count)) 

"Report profiling statistics on given functions." 
(let ((total-time (reduce #'+ (mapcar #'profile-time fn-names)))) 
(unless (null key) 
(setf fn-names (sort fn-names #'> :key key))) 
(format t "~&Total elapsed time: ~d seconds." 

(fast-time->seconds total-time)) 
(format t "~& Count Sees Time% Name") 
(loop for name in fn-names do 

(format t "~&~7D ~6,2F  ~3d% ~A" 
(profile-count name) 
(fast-time->seconds (profile-time name)) 
(round (/ (profile-time name) total-time) .01) 
name)))) 

(defun profile-time (fn-name) (get fn-name 'profile-time)) 

These functions can be used by calling profile, then doing some representative com


putation, then calling prof i 1 e - report, and finally unprof i 1 e. It can be convenient 

to provide a single macro for doing all of these at once: 

(defmacro with-profiling (fn-names &rest body) 

'(progn 
(unprofile . ,fn-names) 
(profile . .fn-names) 
(setf *profile-call-stack* nil) 
(unwind-protect 

(progn . .body) 
(profile-report',fn-names) 
(unprofile . .fn-names)))) 

Note the use of unwi nd - protect to produce the report and call unprof i 1 e even if the 
computation is aborted, unwind-protect is a special form that takes any number 
of arguments. It evaluates the first argument, and if all goes well it then evaluates 

<a id='page-295'></a>
the other arguments and returns the first one, just like progl. But if an error occurs 
during the evaluation of the first argument and computation is aborted, then the 
subsequent arguments (called cleanup forms) are evaluated anyway. 

9.6 A Case Study in Efficiency: The 
SIMPLIFY Program 
Suppose we wanted to speed up the simplify program of chapter 8. This section 
shows how a combination of general techniques—memoizing, indexing, and 
compiling—can be used to speed up the program by a factor of 130. Chapter 15 will 
show another approach: replace the algorithm with an entirely different one. 

The first step to a faster program is defining a benchmark, a test suite representing 
a typical work load. The following is a short list of test problems (and their answers) 
that are typical of the s impli f y task. 

(defvar nest-data* (mapcar #'infix->prefix 

'((d (a*x^2 + b*x + c)/dx) 
(d ((a * . 2 + b * . + c) / x) / d x) 
(d ((a * . ^ 3 + b * . ^ 2 + c * . + d) / . ^ 5) / d x) 
((sin (X + X)) * (sin (2 * x)) + (cos (d (x ^ 2) / d x)) ^ 1) 
(d (3 * X + (cos X) / X) / d X)))) 
(defvar ^^answers* (mapcar #'simplify *test-data*)) 

The function test-it runs through the test data, making sure that each answer is 
correct and optionally printing profiling data. 

(defun test-it (&optional (with-profiling t)) 
"Time a test run, and make sure the answers are correct." 
(let ((answers 

(if with-profiling 
(with-profiling (simplify simplify-exp pat-match 
match-variable variable-p) 
(mapcar #*simplify nest-data*)) 

(time (mapcar #'simplify *test-data*))))) 
(mapc #*assert-equal answers *answers*) 
t)) 

(defun assert-equal (x y) 
"If X is not equal to y, complain." 
(assert (equal . y) (. y) 

"Expected "a to be equal to ~a" . y)) 

Here are the results of (test - i t) with and without profiling: 

<a id='page-296'></a>

> (test-it nil) 
Evaluation of (MAPCAR #'SIMPLIFY *TEST-DATA*) took 6.612 seconds. 

> (test-it t) 
Total elapsed time: 22.819614 seconds. 

Count Sees Time% Name 
51690 11.57 51% PAT-MATCH 
37908 8.75 38% VARIABLE-P 
1393 0.32 1% MATCH-VARIABLE 
906 0.20 1% SIMPLIFY 
274 1.98 9% SIMPLIFY-EXP 

Running the test takes 6.6 seconds normally, although the time triples when the 
profiling overhead is added in. It should be clear that to speed things up, we have 
to either speed up or cut down on the number of calls to pat-match or vari abl e-p, 
since together they account for 89% of the calls (and 89% of the time as well). We 
will look at three methods for achieving both those goals. 

Memoization 

Consider the rule that transforms (x + x) into (2 * .). Once this is done, we have 
to simplify the result, which involves resimplifying the components. If x were some 
complex expression, this could be time-consuming, and it will certainly be wasteful, 
because . is already simplified and cannot change. We have seen this type of problem 
before, and the solution is memoization: make simpl i fy remember the work it has 
done, rather than repeating the work. We can just say: 

(memoize 'simplify :test #'equal) 

Two questions are unclear: what kind of hash table to use, and whether we should 
clear the hash table between problems. The simplifier was timed for all four combinations 
of eq or equal hash tables and resetting or nonresetting between problems. 
The fastest result was equal hashing and nonresetting. Note that with eq hashing, 
the resetting version was faster, presumably because it couldn't take advantage of 
the common subexpressions between examples (since they aren't eq). 

hashing resetting time 
none — 6.6 
equal yes 3.8 
equal no 3.0 
eq yes 7.0 
eq no 10.2 

<a id='page-297'></a>
This approach makes the function simpl i fy remember the work it has done, in 
a hash table. If the overhead of hash table maintenance becomes too large, there is 
an alternative: make the data remember what simplify has done. This approach was 
taken in MACSYMA: it represented operators as lists rather than as atoms. Thus, instead 
of (* 2 X), MACSYMA would use ((*) 2 .). The simplification function would 
destructively insert a marker into the operator list. Thus, the result of simplifying 2x 
would be ((* s i mp) 2 .). Then, when the simplifier was called recursively on this 
expression, it would notice the s i mp marker and return the expression as is. 

The idea of associating memoization information with the data instead of with the 
function will be more efficient unless there are many functions that all want to place 
their marks on the same data. The data-oriented approach has two drawbacks: it 
doesn't identify structures that are equal but not eq, and, because it requires explicitly 
altering the data, it requires every other operation that manipulates the data to know 
about the markers. The beauty of the hash table approach is that it is transparent; no 
code needs to know that memoization is taking place. 

Indexing 

We currently go through the entire list of rules one at a time, checking each rule. This 
is inefficient because most of the rules could be trivially ruled out—if only they were 
indexed properly. The simplest indexing scheme would be to have a separate list 
of rules indexed under each operator. Instead of having simpl ify-exp check each 
member of *s i mpl i f i cat i on - rul es*, it could look only at the smaller list of rules for 
the appropriate operator. Here's how: 

(defun simplify-exp (exp) 
"Simplify using a rule, or by doing arithmetic, 
or by using the simp function supplied for this operator. 
This version indexes simplification rules under the operator." 
(cond ((simplify-by-fn exp)) 

((rule-based-translator exp (rules-for (exp-op exp)) 
:rule-if #'exp-lhs :rule-then #'exp-rhs 
:action #'(lambda (bindings response) 

(simplify (sublis bindings response))))) 
((evaluable exp) (eval exp)) 
(t exp))) 

(defvar *rules-for* (make-hash-table :test #*eq)) 

(defun main-op (rule) (exp-op (exp-lhs rule))) 

<a id='page-298'></a>

(defun index-rules (rules) 
"Index all the rules under the main op." 
(clrhash *rules-for*) 
(dolist (rule rules) 

:; nconc instead of push to preserve the order of rules 
(setf (gethash (main-op rule) *rules-for*) 
(nconc (gethash (main-op rule) *rules-for*) 
(list rule))))) 

(defun rules-for (op) (gethash op *rules-for*)) 

(i ndex-rules *s i mpli fi cati on-rul es*) 

Timing the memoized, indexed version gets us to .98 seconds, down from 6.6 seconds 
for the original code and 3 seconds for the memoized code. If this hadn't helped, we 
could have considered more sophisticated indexing schemes. Instead, we move on 
to consider other means of gaining efficiency. 

&#9635; Exercise 9.2 [m] The list of rules for each operator is stored in a hash table with 
the operator as key. An alternative would be to store the rules on the property list 
of each operator, assuming operators must be symbols. Implement this alternative, 
and time it against the hash table approach. Remember that you need some way of 
clearing the old rules—trivial with a hash table, but not automatic with property lists. 

Compilation 

You can look at simpl i fy-exp as an interpreter for the simplification rule language. 
One proven technique for improving efficiency is to replace the interpreter with a 
compiler. Forexample, the rule (x + . = 2 * .) could be compiled into something 
like: 

(lambda (exp) 
(if (and (eq (exp-op exp) '+) (equal (exp-lhs exp) (exp-rhs exp))) 
(make-exp :op '* :lhs 2 :rhs (exp-rhs exp)))) 

This eliminates the need for consing up and passing around variable bindings, and 
should be faster than the general matching procedure. When used in conjunction 
with indexing, the individual rules can be simpler, because we already know we have 
the right operator. For example, with the above rule indexed under "->-", it could now 
be compiled as: 

<a id='page-299'></a>
(lambda (exp) 
(if (equal (exp-lhs exp) (exp-rhs exp)) 
(make-exp :op '* :lhs 2 :rhs (exp-lhs exp)))) 

It is important to note that when these functions return nil, it means that they 
have failed to simplify the expression, and we have to consider another means of 
simplification. 

Another possibility is to compile a set of rules all at the same time, so that the 
indexing is in effect part of the compiled code. As an example, I show here a small set 
of rules and a possible compilation of the rule set. The generated function assumes 
that . is not an atom. This is appropriate because we are replacing simpl 1 fy-exp, 
not simpl ify. Also, we will return nil to indicate that . is already simplified. I 
have chosen a slightly different format for the code; the main difference is the 1 et 
to introduce variable names for subexpressions. This is useful especially for deeply 
nested patterns. The other difference is that I explicitly build up the answer with a 
call to 1 i St, rather than make-exp. This is normally considered bad style, but since 
this is code generated by a compiler, I wanted it to be as efficient as possible. If the 
representation of the exp data type changed, we could simply change the compiler; a 
much easier task than hunting down all the references spread throughout a human-
written program. The comments following were not generated by the compiler. 

(x * 1 = x) 
(1 * . = x) 
(x * 0 = 0) 
(0 * . = 0) 
(X * X = . ^ 2) 

(lambda (x) 
(let ((xT (exp-lhs x)) 
(xr (exp-rhs x))) 
(or (if (eql xr *1) 
xl) 
(if (eql xl *1) 
xr) 
(if (eql xr *0) 
.) 
(if (eql xl .) 
.) 
(if (equal xr xl) 
(list Xl '2))))) 

; (x 1 = X) 
; (1 ' X = X) 
; (X ' 0 = 0) 
; (0 ' X = 0) 
: (X * X = X ^ 2) 

I chose this format for the code because I imagined (and later show) that it would be 
fairly easy to write the compiler for it. 

<a id='page-300'></a>

The Single-Rule Compiler 

Here I show the complete single-rule compiler, to be followed by the indexed-rule-set 
compiler. The single-rule compiler works like this: 

> (compile-rule '(= (+ . x) (* 2 x))) 
(LAMBDA (X) 
(IF (OP? X '+) 
(LET ((XL (EXP-LHS X)) 
(XR (EXP-RHS X))) 
(IF (EQUAL XR XL) 
(SIMPLIFY-EXP (LIST '* '2 XL)))))) 

Given a rule, it generates code that first tests the pattern and then builds the right-
hand side of the rule if the pattern matches. As the code is generated, correspondences 
are built between variables in the pattern, like x, and variables in the generated 
code, like xl. These are kept in the association Ust *bi ndi ngs*. The matching can be 
broken down into four cases: variables that haven't been seen before, variables that 
have been seen before, atoms, and lists. For example, the first time we run across 
. in the rule above, no test is generated, since anything can match x. But the entry 

(x . xl) is added to the *bi ndi ngs* Hst to mark the equivalence. When the second . 
is encountered, the test (equal xr xl) is generated. 
Organizing the compiler is a little tricky, because we have to do three things at 
once: return the generated code, keep track of the *b i ndi ngs*, andkeep track of what 
to do "next"—that is, when a test succeeds, we need to generate more code, either 
to test further, or to build the result. This code needs to know about the bindings, 
so it can't be done before the first part of the test, but it also needs to know where it 
should be placed in the overall code, so it would be messy to do it after the first part 
of the test. The answer is to pass in a function that will tell us what code to generate 
later. This way, it gets done at the right time, and ends up in the right place as well. 
Such a function is often called a continuation, because it tells us where to continue 
computing. In our compiler, the variable consequent is a continuation function. 

The compiler is called compi 1 e - rul e. It takes a rule as an argument and returns 
a lambda expression that implements the rule. 

(defvar *bindings* nil 
"A list of bindings used by the rule compiler.") 

(defun compile-rule (rule) 
"Compile a single rule." 
(let ((*bindings* nil)) 

'(lambda (x) 
,(compile-exp 'x (exp-lhs rule) ; . is the lambda parameter 
(delay (build-exp (exp-rhs rule) 

<a id='page-301'></a>
^bindings*)))))) 

All the work is done by compi 1 e-exp, which takes three arguments: a variable that 
will represent the input in the generated code, a pattern that the input should be 
matched against, and a continuation for generating the code if the test passes. There 
are five cases: (1) If the pattern is a variable in the list of bindings, then we generate 
an equality test. (2) If the pattern is a variable that we have not seen before, then 
we add it to the binding list, generate no test (because anything matches a variable) 
and then generate the consequent code. (3) If the pattern is an atom, then the match 
succeeds only if the input is eql to that atom. (4) If the pattern is a conditional like 
(?i s . numberp), then we generate the test (numberp .). Other such patterns could 
be included here but have not been, since they have not been used. Finally, (5) if the 
pattern is a list, we check that it has the right operator and arguments. 

(defun compile-exp (var pattern consequent) 
"Compile code that tests the expression, and does consequent 
if it matches. Assumes bindings in *bindings*." 
(cond ((get-binding pattern *bindings*) 

Test a previously bound variable 
*(if (equal ,var ,(lookup pattern *bindings*)) 
,(force consequent))) 

((variable-p pattern) 
;; Add a new bindings; do type checking if needed, 
(push (cons pattern var) *bindings*) 
(force consequent)) 

((atom pattern) 
Match a literal atom 
*(if (eql ,var pattern) 
,(force consequent))) 
((starts-with pattern '?is) 
(push (cons (second pattern) var) *bindings*) 

'(if(,(third pattern) ,var) 
,(force consequent))) 
;; So, far, only the ?is pattern is covered, because 
;; it is the only one used in simplification rules. 
;; Other patterns could be compiled by adding code here. 

Or we could switch to a data-driven approach, 
(t Check the operator and arguments 
'(if (op? ,var *,(exp-op pattern)) 
,(compile-args var pattern consequent))))) 

The function compi 1 e - a rgs is used to check the arguments to a pattern. It generates 
a 1 et form binding one or two new variables (for a unary or binary expression), and 
then calls compi 1 e-exp to generate code that actually makes the tests. It just passes 
along the continuation, consequent, to compi 1 e-exp. 

<a id='page-302'></a>

(defun compile-args (var pattern consequent) 
"Compile code that checks the arg or args, and does consequent 
if the arg(s) match." 

First make up variable names for the arg(s). 
(let ((L (symbol var 'D) 
(R (symbol var 'R))) 
(if (exp-rhs pattern) 
;; two arg case 

'(let ((,L (exp-lhs ,var)) 
(,R (exp-rhs ,var))) 
,(compile-exp L (exp-lhs pattern) 
(delay 
(compile-exp R (exp-rhs pattern) 
consequent)))) 
one arg case 

'(let ((,L (exp-lhs ,var))) 
,(compile-exp L (exp-lhs pattern) consequent))))) 
The remaining functions are simpler, bui 1 d-exp generates code to build the right-
hand side of a rule, op? tests if its first argument is an expression with a given 
operator, and symbol constructs a new symbol. Also given is new-symbol, although 
it is not used in this program. 

(defun build-exp (exp bindings) 
"Compile code that will build the exp, given the bindings." 
(cond ((assoc exp bindings) (rest (assoc exp bindings))) 

((variable-p exp) 
(error "Variable ~a occurred on right-hand side,~ 

but not left." exp)) 
((atom exp) ",exp) 
(t (let ((new-exp (mapcar #*(lambda (x) 

(build-exp . bindings)) 
exp))) 

'(simplify-exp (list .,new-exp)))))) 
(defun op? (exp op) 
"Does the exp have the given op as its operator?" 
(and (exp-p exp) (eq (exp-op exp) op))) 

(defun symbol (&rest args) 
"Concatenate symbols or strings to form an interned symbol" 
(intern (format nil "-{-a^}" args))) 

(defun new-symbol (&rest args) 
"Concatenate symbols or strings to form an uninterned symbol" 
(make-symbol (format nil "'"{^a"}" args))) 

<a id='page-303'></a>
Here are some examples of the compiler: 

> (compile-rule '(= (log (^ e x)) x)) 
(LAMBDA (X) 
(IF (OP? X 'LOG) 
(LET ((XL (EXP-LHS X))) 
(IF (OP? XL 
(LET ((XLL (EXP-LHS XL)) 
(XLR (EXP-RHS XL))) 
(IF (EQL XLL .) 
XLR)))))) 

> (compile-rule (simp-rule '(n * (m * x) = (n * m) * x))) 
(LAMBDA (X) 
(IF (OP? X .*) 
(LET ((XL (EXP-LHS X)) 
(XR (EXP-RHS X))) 
(IF (NUMBERP XL) 
(IF (OP? XR '*) 
(LET ((XRL (EXP-LHS XR)) 
(XRR (EXP-RHS XR))) 
(IF (NUMBERP XRL) 
(SIMPLIFY-EXP 

(LIST .* 
(SIMPLIFY-EXP (LIST '* XL XRL)) 
XRR))))))))) 

The Rule-Set Compiler 

The next step is to combine the code generated by this single-rule compiler to generate 
more compact code for sets of rules. We'll divide up the complete set of rules into 
subsets based on the main operator (as we did with the rules-for function), and 
generate one big function for each operator. We need to preserve the order of the 
rules, so only certain optimizations are possible, but if we make the assumption 
that no function has side effects (a safe assumption in this application), we can 
still do pretty well. We'll use the simp-fn facility to install the one big function for 
each operator. 

The function compi1 e - rul e- set takes an operator, finds all the rules for that operator, 
and compiles each rule individually. (It uses compi1 e -i ndexed -rule rather than 
compi 1 e -rul e, because it assumes we have already done the indexing for the main operator.) 
After each rule has been compiled, they are combined with combi ne- rul es, 
which merges similar parts of rules and concatenates the different parts. The result 
is wrapped in a 1 ambda expression and compiled as the final simplification function 
for the operator. 

<a id='page-304'></a>

(defun compile-rule-set (op) 
"Compile all rules indexed under a given main op. 
and make them into the simp-fn for that op." 
(set-simp-fn op 

(compile nil 
'(lambda (x) 
.(reduce #'combine-rules 
(mapcar #*compile-indexed-rule 
(rules-for op))))))) 

(defun compile-indexed-rule (rule) . 
"Compile one rule into lambda-less code, 
assuming indexing of main op." 
(let ((*bindings* nil)) 

(compile-args 
'x (exp-lhs rule) 

(delay (build-exp (exp-rhs rule) ^bindings*))))) 

Here are two examples of what compi 1 e - i ndexed - rul e generates: 
> (compile-indexed-rule '(= (log 1) 0)) 
(LET ((XL (EXP-LHS X))) 
(IF (EQL XL .) 
.)) 

> (compile-indexed-rule *(= (log (" e x)) x)) 
(LET ((XL (EXP-LHS X))) 
(IF (OP? XL *n 
(LET ((XLL (EXP-LHS XL)) 
(XLR (EXP-RHS XL))) 
(IF (EQL XLL .) 
XLR)))) 

Thenextstepis to combine several of these rules into one. The function comb i ne- rul es 
takes two rules and merges them together as much as possible. 

(defun combine-rules (a b) 

"Combine the code for two rules into one, maintaining order." 
In the default case, we generate the code (or a b), 
but we try to be cleverer and share common code, 
on the assumption that there are no side-effects, 

(cond ((and distp a) distp b) 
(= (length a) (length b) 3) 
(equal (first a) (first b)) 
(equal (second a) (second b))) 

;; a=(f . y), b=(f . .) => (f . (combine-rules y .)) 
This can apply when f=IF or f=LET 

<a id='page-305'></a>
(list (first a) (second a) 

(combine-rules((matching-ifs a b) 

'(if .(second a) 
.(combine-rules.(combine-rules

((starts-with a Or) 

 (third a) (third b)))) 

 (third a) (third b)) 
(fourth a) (fourth b)))) 

a=(or ... (if . y)). b=(if . .) => 
(or ... (if . (combine-rules y .))) 

else 
a=(or ...) b => (or ... b) 
(if (matching-ifs (lastl a) b) 
(append (butlast a) 
(list (combine-rules(append a (list b)))) 
(t a. b => (or a b) 
'(or .a .b)))) 

(defun matching-ifs (a b) 

 (lastl a) b))) 

"Are a and b if statements with the same predicate?" 
(and (starts-with a 'if) (starts-with b 'if) 
(equal (second a) (second b)))) 

(defun lastl (list) 
"Return the last element (not last cons cell) of list" 
(first (last list))) 

Here is what combi ne- rul es does with the two rules generated above: 

> (combine-rules 
'(let ((xl (exp-lhs x))) (if (eql xl .) .)) 
'(let ((xl (exp-lhs x))) 

(if (op? xl '^) 
(let ((xll (exp-lhs xD) 
(xlr (exp-rhs xl))) 
(if (eql xll 'e) xlr))))) 
(LET ((XL (EXP-LHS X))) 
(OR (IF (EQL XL .) .) 
(IF (OP? XL "^) 
(LET ((XLL (EXP-LHS XL)) 
(XLR (EXP-RHS XL))) 
(IF (EQL XLL .) XLR))))) 

Now we run the compiler by calling compi 1 e-all - rul es-indexed and show the 
combined compiled simplification function for 1 og. The comments were entered by 
hand to show what simplification rules are compiled where. 

<a id='page-306'></a>

(defun compile-all-rules-indexed (rules) 
"Compile a separate fn for each operator, and store it 
as the simp-fn of the operator." 
(index-rules rules) 
(let ((all-ops (delete-duplicates (mapcar #*main-op rules)))) 

(mapc #'compile-rule-set all-ops))) 

> (compile-all-rules-indexed *simplification-rules*) 
(SIN COS LOG ^ * / - + D) 

> (simp-fn 'log) 
(LAMBDA (X) 
(LET ((XL (EXP-LHS X))) 
(OR (IF (EQL XL .) 
.) logl = 0 
(IF (EQL XL .) 
'UNDEFINED) log 0 -undefined 
(IF (EQL XL '.) 
.) loge = l 
(IF (OP? XL '^) 
(LET ((XLL (EXP-LHS XL)) 
(XLR (EXP-RHS XL))) 
(IF (EQL XLL .) 
XLR)))))) lloge"" = X 

If we want to bypass the rule-based simplifier altogether, we can change si mp1 i fy- exp 
once again to eliminate the check for rules: 

(defun simplify-exp (exp) 
"Simplify by doing arithmetic, or by using the simp function 
supplied for this operator. Do not use rules of any kind." 
(cond ((simplify-by-fn exp)) 

((evaluable exp) (eval exp)) 
(t exp))) 

At last, we are in a position to run the benchmark test on the new compiled code; the 
function test -it runs in about .15 seconds with memoization and .05 without. Why 
would memoization, which helped before, now hurt us? Probably because there is a 
lot of overhead in accessing the hash table, and that overhead is only worth it when 
there is a lot of other computation to do. 

We've seen a great improvement since the original code, as the following table 
summarizes. Overall, the various efficiency improvements have resulted in a 130fold 
speed-up—we can do now in a minute what used to take two hours. Of course, 
one must keep in mind that the statistics are only good for this one particular set of 

<a id='page-307'></a>
test data on this one machine. It is an open question what performance you will get 
on other problems and on other machines. 
The following table summarizes the execution time and number of function calls 
on the test data: 

original memo memo+index memo+comp comp 
run time (sees) 6.6 3.0 .98 .15 .05 
speed-up — 2 7 44 130 
calls 
pat-match 51690 20003 5159 0 0 
variable-p 37908 14694 4798 0 0 
match-vari able 1393 551 551 0 0 
simplify 906 408 408 545 906 
simplify-exp 274 118 118 118 274 

9.7 History and References 
The idea of memoization was introduced by Donald Michie 1968. He proposed 
using a list of values rather than a hash table, so the savings was not as great. In 
mathematics, the field of dynamic programming is really just the study of how to 
compute values in the proper order so that partial results will already be cached away 
when needed. 

A large part of academic computer science covers compilation; Aho and Ullman 
1972 is just one example. The technique of compiling embedded languages (such as 
the language of pattern-matching rules) is one that has achieved much more attention 
in the Lisp community than in the rest of computer science. See Emanuelson and 
Haraldsson 1980, for an example. 

Choosing the right data structure, indexing it properly, and defining algorithms 
to operate on it is another important branch of computer science; Sedgewick 1988 is 
one example, but there are many worthy texts. 

Delaying computation by packaging it up in a 1 ambda expression is an idea that 
goes back to Algol's use of thunks—a mechanism to implement call-by-name parameters, 
essentially by passing functions of no arguments. The name thunk comes from 
the fact that these functions can be compiled: the system does not have to think 
about them at run time, because the compiler has already thunk about them. Peter 
Ingerman 1961 describes thunks in detail. Abelson and Sussman 1985 cover delays 
nicely. The idea of eliminating unneeded computation is so attractive that entire languages 
have built around the concept of lazy evaluation—don't evaluate an expression 
until its value is needed. See Hughes 1985 or Field and Harrison 1988. 

<a id='page-308'></a>

9.8 Exercises 
&#9635; Exercise 9.3 [d] In this chapter we presented a compiler for s i mp1 i fy. It is not too 
much harder to extend this compiler to handle the full power of pat-match. Instead 
of looking at expressions only, allow trees with variables in any position. Extend and 
generalize the definitions of compi 1 e -rul e and compi 1 e - rul e-set so that they can 
be used as a general tool for any application program that uses pat-match and/or 
rule-based -trans1 ator. Make sure that the compiler is data-driven, so that the 
programmer who adds a new kind of pattern to pat-match can also instruct the 
compiler how to deal with it. One hard part will be accounting for segment variables. 
It is worth spending a considerable amount of effort at compile time to make this 
efficient at run time. 

&#9635; Exercise 9.4 [m] Define the time to compute (fib n) without memoization as T<sub>n</sub>. 
Write a formula to express T<sub>n</sub>. Given that T<sub>25</sub> &asymp; 1.1 seconds, predict T<sub>100</sub>.

&#9635; Exercise 9.5 [m] Consider a version of the game of Nim played as follows: there is 
a pile of . tokens. Two players alternate removing tokens from the pile; on each turn 
a player must take either one, two, or three tokens. Whoever takes the last token 
wins. Write a program that, given n, returns the number of tokens to take to insure 
a win, if possible. Analyze the execution times for your program, with and without 
memoization. 

&#9635; Exercise 9.6 [m] A more complicated Nim-like game is known as Grundy's game. 
The game starts with a single pile of . tokens. Each player must choose one pile and 
split it into two uneven piles. The first player to be unable to move loses. Write a 
program to play Grundy's game, and see how memoization helps. 

&#9635; Exercise 9.7 [h] This exercise describes a more challenging one-person game. In 
this game the player rolls a six-sided die eight times. The player forms four two-digit 
decimal numbers such that the total of the four numbers is as high as possible, but 
not higher than 170. A total of 171 or more gets scored as zero. 

The game would be deterministic and completely boring if not for the requirement 
that after each roll the player must immediately place the digit in either the ones or 
tens column of one of the four numbers. 

Here is a sample game. The player first rolls a 3 and places it in the ones column 
of the first number, then rolls a 4 and places it in the tens column, and so on. On the 
last roll the player rolls a 6 and ends up with a total of 180. Since this is over the limit 
of 170, the player's final score is 0. 

<a id='page-309'></a>

roll 3 4 6 6 3 5 3 6 

1st num. -3 43 43 43 43 43 43 43 

2nd num. -6 -6 36 36 36 36 

-

-

3rd num. -6 -6 -6 36 36 

-

4th num. -5 -5 65 
total 03 43 49 55 85 90 120 0 

Write a function that allows you to play a game or a series of games. The function 
should take as argument a function representing a strategy for playing the game. 

&#9635; Exercise 9.8 [h] Define a good strategy for the dice game described above. (Hint: 
my strategy scores an average of 143.7.) 

&#9635; Exercise 9.9 [m] One problem with playing games involving random numbers is 
the possibility that a player can cheat by figuring out what random is going to do next. 
Read the definition of the function random and describe how a player could cheat. 
Then describe a countermeasure. 

&#9635; Exercise 9.10 [m] On [page 292](chapter9.md#page-292) we saw the use of the read-time conditionals, and 
# -, where #+ is the read-time equivalent of when, and #- is the read-time equivalent 
of unless. Unfortunately, there is no read-time equivalent of case. Implement one. 

&#9635; Exercise 9.11 [h] Write a compiler for ELIZA that compiles all the rules at once into 
a single function. How much naore efficient is the compiled version? 

&#9635; Exercise 9.12 [d] Write some rules to simplify Lisp code. Some of the algebraic 
simplification rules will still be valid, but new ones will be needed to simplify nonalgebraic 
functions and special forms. (Since ni1 is a valid expression in this domain, 
you will have to deal with the semipredicate problem.) Here are some example rules 
(using prefix notation): 

= (+ . 0) .) 
= 'nil nil) 
= (car (cons . y)) .) 
= (cdr (cons . y)) y) 
= (if t . y) .) 
= (if nil X y) y) 
= (length nil) 0) 
= (expt y (?if X numberp)) (expt (expt y (/ . 2)) 2)) 

<a id='page-310'></a>

&#9635; Exercise 9.13 [m] Consider the following two versions of the sieve of Eratosthenes 
algorithm. The second explicitly binds a local variable. Is this worth it? 

(defun sieve (pipe) 
(make-pipe (head pipe) 
(filter #*(lambda (x)(/= (mod . (headpipe)) 0)) 
(sieve (tail pipe))))) 

(defun sieve (pipe) 
(let ((first-num (head pipe))) 
(make-pipe first-num 
(filter #'(lambda (x) (/= (mod . first-num) 0)) 
(sieve (tail pipe)))))) 

9.9 Answers 
Answer 9.4 Let Fn denote (fib .). Then the time to compute Fn, Tn, is a small 
constant for . < 1, and is roughly equal to Tn-\ plus Tn-i for larger n. Thus, Tn is 
roughly proportional to Fn'. 

T<sub>n</sub> = F<sub>n</sub> T<sub>i</sub> / F<sub>i</sub> 

We could use some small value of Ti to calculate Tioo if we knew Fioo- Fortunately, 
we can use the equation: 

where &phi; = ^J{5))/2 &asymp; 1.618. This equation was derived by de Moivre in 1718 
(see Knuth, Donald E. Fundamental Algorithms, pp. 78-83), but the number . has a 
long interesting history. Euclid called it the "extreme and mean ratio," because the 
ratio of A to . is the ratio of A -h J5 to A if A/JB is .. In the Renaissance it was called 
the "divine proportion," and in the last century it has been known as the "golden 
ratio," because a rectangle with sides in this ratio can be divided into two smaller 
rectangles that both have the same ratio between sides. It is said to be a pleasing 
proportion when employed in paintings and architecture. Putting history aside, 
given T25 &asymp; 1.1sec we can now calculate: 

T<sub>100</sub> &asymp; &phi;<sup>100</sup> 1.1sec/&phi;<sup>25</sup> &asymp; 5 x10<sup>15</sup>sec 

which is roughly 150 million years. We can also see that the timing data in the table 
fits the equation fairly well. However, we would expect some additional time for 
larger numbers because it takes longer to add and garbage collect bignums than 
fixnums. 

<a id='page-311'></a>

Answer 9.5 First we'll define the notion of a forced win. This occurs either when 
there are three or fewer tokens left or when you can make a move that gives your 
opponent a possible loss. A possible loss is any position that is not a forced win. If 
you play perfectly, then a possible loss for your opponent will in fact be a win for you, 
since there are no ties. See the functions wi . and 1 oss below. Now your strategy 
should be to win the game outright if there are three or fewer tokens, or otherwise 
to choose the largest number resulting in a possible loss for your opponent. If there 
is no such move available to you, take only one, on the grounds that your opponent 
is more likely to make a mistake with a larger pile to contend with. This strategy is 
embodied in the function nim below. 

(defun win (n) 
"Is a pile of . tokens a win for the player to move?" 
(or (<= . 3) 

(loss (- . D) 
(loss (- . 2)) 
(loss (- . 3)))) 

(defun loss (n) (not (win n))) 

(defun nim (n) 
"Play Nim: a player must take 1-3; taking the last one wins." 
(cond ((<= . 3) n) ; an immediate win 
(doss (- . 3)) 3) ; an eventual win 
(doss (- . 2)) 2) ; an eventual win 
(doss (- . 1)) 1) ; an eventual win 
(t 1))) ; a loss; the 1 is arbitrary 

(memoize doss) 

From this we are able to produce a table of execution times (in seconds), with and 
without memoization. Only 1 oss need be memoized. (Why?) Do you have a good 
explanation of the times for the unmemoized version? What happens if you change 
the order of the loss clauses in wi . and/or . i m? 

Answer 9.6 We start by defining a function, moves, which generates all possible 
moves from a given position. This is done by considering each pile of . tokens within 
a set of piles s. Any pile bigger than two tokens can be split. We take care to eliminate 
duplicate positions by sorting each set of piles, and then removing the duplicates. 

(defun moves (s) 
"Return a list of all possible moves in Grundy's game" 
;; S is a list of integers giving the sizes of the piles 
(remove-duplicates 

(loop for . in s append (make-moves . s)) 

:test #'equal)) 

<a id='page-312'></a>

(defun make-moves (n s) 
(when (>= . 2) 
(let ((s/n (remove . s icount 1))) 
(loop for i from 1 to (- (ceiling . 2) 1) 
collect (sort* (list* i (-ni) s/n) 
#'>)))) 

(defun sort* (seq pred &key key) 
"Sort without altering the sequence" 
(sort (copy-seq seq) pred :key key)) 

This time a loss is defined as a position from which you have no moves, or one from 
which your opponent can force a win no matter what you do. A winning position 
is one that is not a loss, and the strategy is to pick a move that is a loss for your 
opponent, or if you can't, just to play anything (here we arbitrarily pick the first move 
generated). 

(defun loss (s) 
(let ((choices (moves s))) 
(or (null choices) 
(every #'win choices)))) 

(defun win (s) (not (loss s))) 

(defun grundy (s) 
(let ((choices (moves s))) 
(or (find-if #'loss choices) 
(first choices)))) 

Answer 9.7 The answer assumes that a strategy function takes four arguments: 
the current die roll, the score so far, the number of remaining positions in the tens 
column, and the number of remaining positions in the ones column. The strategy 
function should return 1 or 10. 

(defun play-games (&optional (n-games 10) (player 'make-move)) 
"A driver for a simple dice game. In this game the player 
rolls a six-sided die eight times. The player forms four 
two-digit decimal numbers such that the total of the four 
numbers is as high as possible, but not higher than 170. 
A total of 171 or more gets scored as zero. After each die 
is rolled, the player must decide where to put it. 
This function returns the player's average score." 
(/ (loop repeat n-games summing (play-game player 0 4 4)) 

(float n-games))) 

<a id='page-313'></a>

(defun play-game (player &optional (total 0) (tens 4) (ones 4)) 

(cond ((or (> total 170) <tens 0) (< ones 0)) 0) 
((and (= tens 0) (= ones 0)) total) 
(t (let ((die (roll-die))) 

(case (funcall player die total tens ones) 
(1 (play-game player {+ total die) 
tens (- ones 1))) 
(10 (play-game player (+ total (* 10 die)) 
(- tens 1) ones)) 

(t 0)))))) 

(defun roll-die () (+ 1 (random 6))) 

So, the expression (play-games 5 #'make-move) would play five games with a 
strategy called make-move. This returns only the average score of the games; if you 
want to see each move as it is played, use this function: 

(defun show (player) 
"Return a player that prints out each move it makes." 
#'(lambda (die total tens ones) 

(when (= total 0) (fresh-line)) 

(let ((move (funcall player die total tens ones))) 
(incf total (* die move)) 
(format t "~2d->~3d I ~@[*~]" (* move die) total (> total 170)) 
move))) 

and call (pi ay-games 5 (show #'make-moves)). 

Answer 9.9 The expression (random 6 (make-random-state)) returns the next 
number that rol 1 -di e will return. To guard against this, we can make rol 1 -di e use 
a random state that is not accessible through a global variable: 

(let ((state (make-random-state t))) 
(defun roll-die () (+ 1 (random 6 state)))) 

Answer 9.10 Because this has to do with read-time evaluation, it must be implemented 
as a macro or read macro. Here's one way to do it: 

(defmacro read-time-case (first-case &rest other-cases) 
"Do the first case, where normally cases are 
specified with #+ or possibly #- marks." 
(declare (ignore other-cases)) 
first-case) 

<a id='page-314'></a>

A fanciful example, resurrecting a number of obsolete Lisps, follows: 

(defun get-fast-time 0 

(read-time-case 
#+Explorer (ti me:mi crosecond-ti me) 
#+Franz (sysitime) 
#+(or PSL UCI) (time) 

#+YKT (currenttime) 
#+MTS (status 39) 
#+Interlisp (clock 1) 
#+Lispl.5 (tempus-fugit) 
otherwise 
(get-internal-real-time)) ) 

Answer 9.13 Yes. Computing (head pipe) may be a trivial computation, but it 
will be done many times. Binding the local variable makes sure that it is only done 
once. In general, things that you expect to be done multiple times should be moved 
out of delayed functions, while things that may not be done at all should be moved 
inside a delay. 

