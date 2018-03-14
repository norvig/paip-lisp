# Chapter 3 {docsify-ignore}
<a id='page-48'></a>

Overview of Lisp 

No doubt about it. Common Lisp is a big language. 

—Guy L. Steele, Jr. 
Foreword to Koschman 1990 

I 1 his chapter briefly covers the most important special forms and functions in Lisp. It 
can be safely skipped or skimmed by the experienced Common Lisp programmer 

but is required reading for the novice Lisp progranuner, or one who is new to the 
Common Lisp dialect. 

r. 

This chapter can be used as a reference source, but the definitive reference is Steele's Common 
Lisp the Language, 2d edition, which should be consulted whenever there is any confusion. Since 
that book is 25 times longer than this chapter, it is clear that we can only touch on the important 
highlights here. More detailed coverage is given later in this book as each feature is used in a 
real program. 

<a id='page-49'></a>
3.1 A Guide to Lisp Style 
The beginning Common Lisp programmer is often overwhelmed by the number of 
options that the language provides. In this chapter we show fourteen different ways 
to find the length of a list. How is the programmer to choose between them? One 
answer is by reading examples of good programs—as illustrated in this book—and 
copying that style. In general, there are six maxims that every programmer should 
follow: 

* Be specific. 
* Use abstractions. 
* Be concise. 
* Use the provided tools. 
* Don't be obscure. 
* Be consistent. 
These require some explanation. 

Using the most specific form possible makes it easier for your reader to understand 
your intent. For example, the conditional special form when is more specific than i f. 
The reader who sees a when knows to look for only one thing: the clause to consider 
when the test is true. The reader who sees an i f can rightfully expect two clauses: 
one for when the test is true, and one for when it is false. Even though it is possible 
to use i f when there is only one clause, it is preferable to use when, because when is 
more specific. 

One important way of being specific is using abstractions. Lisp provides very 
general data structures, such as lists and arrays. These can be used to implement 
specific data structures that your program will use, but you should not make the 
mistake of invoking primitive functions directly. If you define a list of names: 

(defvar *names* '((Robert E. Lee) ...)) 

then you should also define functions to get at the components of each name. To get 
at Lee,use (last-name (first *names*)),not (caddar *names*). 

Often the maxims are in concord. For example, if your code is trying to find an 
element in a list, you should use f 1 nd (or maybe f 1 nd-1 f), not 1 oop or do. f i nd is 
more specific than the general constructs 1 oop or do, it is an abstraction, it is more 
concise, it is a built-in tool, and it is simple to understand. 

<a id='page-50'></a>

Sometimes, however, the maxims are in confUct, and experience will tell you 
which one to prefer. Consider the following two ways of placing a new key/value 
pair on an association list:^ 

(push (cons key val) a-list) 
(setf a-list (aeons key val a-list)) 

The first is more concise. But the second is more specific, as it uses the aeons 
function, which is designed specifically for association lists. The decision between 
them probably hinges on obscurity: those who find aeons to be a familiar function 
would prefer the second, and those who find it obscure would prefer the first. 

A similar choice arises in the question of setting a variable to a value. Some prefer 
(setq X val) because it is most specific; others use (setf . val), feeling that it is 
more consistent to use a single form, setf, for all updating. Whichever choice you 
make on such issues, remember the sixth maxim: be consistent. 

3.2 Special Forms 
As noted in chapter 1, "special form" is the term used to refer both to Common Lisp's 
syntactic constructs and the reserved words that mark these constructs. The most 
commonly used special forms are: 

definitions conditional variables iteration other 
defun and let do declare 
defstruct case let* do* function 
defvar cond pop dolist progn 
defparameter if push dotimes quote 
defconstant or setf loop return 
defmacro unless incf trace 
labels when decf untrace 

To be precise, only declare, function. If, labels, let, let*, progn and quote 
are true special forms. The others are actually defined as macros that expand into 
calls to more primitive special forms and functions. There is no real difference to the 
programmer, and Common Lisp implementations are free to implement macros as 
special forms and vice versa, so for simplicity we will continue to use "special form" 
as a blanket term for both true special forms and built-in macros. 

^Association lists are covered in section 3.6. 

<a id='page-51'></a>
Special Forms for Definitions 

In this section we survey the special forms that can be used to introduce new global 
functions, macros, variables, and structures. We have already seen the defun form 
for defining functions; the def macro form is similar and is covered on [page 66](chapter3.md#page-66). 

(defun function-name (parameter...) " optional documentation" body...) 

(defmacro macro-name (parameter...) "optional documentation" body...) 

There are three forms for introducing special variables, defvar defines a special 
variable and can optionally be used to supply an initial value and a documentation 
string. The initial value is evaluated and assigned only if the variable does not yet 
have any value, def pa rameter is similar, except that the value is required, and it will 
be used to change any existing value, def constant is used to declare that a symbol 
will always stand for a particular value. 

(defvar vanable-name initial-value "optional documentation") 
(defparameter vanable-name value "optional documentation") 
(def constant variable-name value "optional documentation") 

All the def - forms define global objects. It is also possible to define local variables 
with 1 et, and to define local functions with 1 abel s, as we shall see. 

Most programming languages provide a way to group related data together into 
a structure. Common Lisp is no exception. The def struct special form defines a 
structure type (known as a record type in Pascal) and automatically defines functions 
to get at components of the structure. The general syntax is: 

(def struct structure-name "optional documentation" slot...) 

As an example, we could define a structure for names: 

(defstruct name 
first 
(middle nil) 
last) 

This automatically defines the constructor function make-name, the recognizer predicate 
name-p, and the accessor functions name-first, name-middle and name-last. 
The (middle nil) means that each new name built by make-name will have a middle 
name of ni 1 by default. Here we create, access, and modify a structure: 

<a id='page-52'></a>

> (setf b (make-name :first 'Barney :last 'Rubble)) => 
#S(NAME :FIRST BARNEY :LAST RUBBLE) 

> (name-first b) ^ BARNEY 

> (name-middle b) NIL 

> (name-last b) ^ RUBBLE 

> (name-p b) =. . 

> (name-p 'Barney) =. NIL ; only the results of make-name are names 

> (setf (name-middle b) 'Q) => Q 

> b #S(NAME :FIRST BARNEY .-MIDDLE Q :LAST RUBBLE) 

The printed representation of a structure starts with a #S and is followed by a list 
consisting of the type of the structure and alternating pairs of slot names and values. 
Do not let this representation fool you: it is a convenient way of printing the structure, 
but it is not an accurate picture of the way structures are represented internally. 
Structures are actually implemented much like vectors. For the name structure, the 
type would be in the zero element of the vector, the first name in the first element, 
middle in the second, and last in the third. This means structures are more efficient 
than lists: they take up less space, and any element can be accessed in a single step. 
In a list, it takes . steps to access the nth element. 

There are options that give more control over the structure itself and the individual 
slots. They will be covered later as they come up. 

Special Forms for Conditionals 

We have seen the special form i f, which has the form (i f test then-part else-part), 
where either the then-part or the else-part is the value, depending on the success of the 
test. Remember that only . i 1 counts as false; all other values are considered true for 
the purpose of conditionals. However, the constant t is the conventional value used 
to denote truth (unless there is a good reason for using some other value). 

There are actually quite a few special forms for doing conditional evaluation. 
Technically, i f is defined as a special form, while the other conditionals are macros, 
so in some sense 1 f is supposed to be the most basic. Some programmers prefer to 
use i f for most of their conditionals; others prefer cond because it has been around 
the longest and is versatile (if not particularly pretty). Finally, some programmers opt 
for a style more like English prose, and freely use when, unl ess, 1 f, and all the others. 

The following table shows how each conditional can be expressed in terms of 
1 f and cond. Actually, these translations are not quite right, because or, case, and 
cond take care not to evaluate any expression more than once, while the translations 
with i f can lead to multiple evaluation of some expressions. The table also has 

<a id='page-53'></a>
translations to cond. The syntax of cond is a series of cond-clauses, each consisting of 
a test expression followed by any number of result expressions: 

(cond {testresult...) 
{test result...) 

...) 

cond goes through the cond-clauses one at a time, evaluating each test expression. 
As soon as a test expression evaluates non-nil, the result expressions for that clause 
are each evaluated, and the last expression in the clause is the value of the whole 
cond. In particular, if a cond-clause consists of just a test and no result expressions, 
then the value of the cond is the test expression itself, if it is non-nil. If all of the test 
expressions evaluate to nil, then nil is returned as the value of the cond. A common 
idiom is to make the last cond-clause be (t result...). 

The forms when and unl ess operate like a single cond clause. Both forms consist 
of a test followed by any number of consequents, which are evaluated if the test is 
satisfied-that is, if the test is true for when or false for unl ess. 

The and form tests whether every one of a list of conditions is true, and or tests 
whether any one is true. Both evaluate the arguments left to right, and stop as soon 
as the final result can be determined. Here is a table of equivalences: 

conditional if form cond form 
(when test ah c) (if test (progn a be)) (cond {testaba)) 
(unless testxy) (if {nottest) (progn xy)) (cond {{not test) xy)) 
(and abc) (if a (if b c)) (cond(fl (cond {be)))) 
(or ahc) (if a a (if b b c)) (cond (a) {b) (c)) 
(case a {b c) (t x)) (if (eql a 'b) c x) (cond ((eql a 'b)c) {tx)) 

It is considered poor style to use and and or for anything other than testing a 
logical condition, when, unl ess, and 1 f can all be used for taking conditional action. 
For example: 

(and (> . 100) 
(princ "N is large.")) ; Bad style! 

(or (<= . 100) 
(princ "N is large.")) ; Even worse style! 

(cond ((> . 100) ; OK. but not MY preference 
(princ "N is large.")) 

(when (> . 100) 
(princ "N is large.")) ; Good style. 

When the main purpose is to return a value rather than take action, cond and i f 
(with explicit . i 1 in theelsecase)are preferred overwhenandunl ess, which implicitly 

<a id='page-54'></a>

return nil in the else case, when and unl ess are preferred when there is only one 
possibility, i f (or, for some people, cond) when there are two, and cond when there 
are more than two: 

(defun tax-bracket (income) 

"Determine what percent tax should be paid for this income." 

(cond ((< income 10000.00) 0.00) 

((< income 30000.00) 0.20) 
((< income 50000.00) 0.25) 
((< income 70000.00) 0.30) 
(t 0.35))) 

If there are several tests comparing an expression to constants, then case is appropriate. 
A case form looks like: 

(case expression 
(matchresult..)...) 

The expression is evaluated and compared to each successive match. As soon as one 
is eql, the result expressions are evaluated and the last one is returned. Note that the 
match expressions are not evaluated. If a match expression is a list, then case tests if 
the expression is eql to any member of the list. If a match expression is the symbol 
otherwi se (or the symbol t), then it matches anything. (It only makes sense for this 
otherwl se clause to be the last one.) 

There is also another special form, typecase, which compares the type of an 
expression against several possibilities and, like case, chooses the first clause that 
matches. In addition, the special forms ecase and etypecase are just like case and 
typecase except that they signal an error if there is no match. You can think of the e 
as standing for either "exhaustive" or "error." The forms cease and etypecase also 
signal errors, but they can be continuable errors (as opposed to fatal errors): the user 
is offered the chance to change the expression to something that satisfies one of the 
matches. Here are some examples of case forms and their cond equivalents: 

(case . (cond 
(1 10) ((eql . 1) 10) 
(2 20)) ((eql . 2) 20)) 
(typecase . (cond 
(number (abs x)) ((typep . 'number) (abs x)) 
(list (length x))) ((typep . 'list ) (length x))) 
(ecase . (cond 
(1 10) ((eql . 1) 10) 
(2 20)) ((eql . 2) 20) 
(t (error "no valid case"))) 

<a id='page-55'></a>
(etypecase . (cond 
(number (abs .)) ((typep . 'number) (abs x)) 
(list (length x))) ((typep . 'list ) (length x)) 
(t (error "no valid typecase"))) 

Special Forms for Dealing with Variables and Places 

The special form setf is used to assign a new value to a variable or place, much as an 
assignment statement with = or := is used in other languages. A place, or generalized 
variable is a name for a location that can have a value stored in it. Here is a table of 
corresponding assignment forms in Lisp and Pascal: 

Lisp /* Pascal */ 
(setf . 0) . := 0; 
(setf (aref A i j) 0) A[i,j] := 0; 
(setf (rest list ) nil) list\res t := nil ; 
(setf (name-middle b) 'Q) b\middle := "Q"; 

setf can be used to set a component of a structure as well as to set a variable. In 
languages like Pascal, the expressions that can appear on the left-hand side of an 
assignment statement are limited by the syntax of the language. In Lisp, the user can 
extend the expressions that are allowed in a s etf form using the special forms defs et f 
or define-setf-method. These are introduced on pages [514](chapter15.md#page-514) and [884](chapter25.md#page-884) respectively. 

There are also some built-in functions that modify places. For example, (rpl a cd 
list nil) has the same effect as (setf (rest list) nil), except that it returns 
list instead of ni 1. Most Common Lisp programmers prefer to use the setf forms 
rather than the specialized functions. 

If you only want to set a variable, the special form setq can be used instead. In 
this book I choose to use setf throughout, opting for consistency over specificity. 

The discussion in this section makes it seem that variables (and slots of structures) 
are assigned new values all the time. Actually, many Lisp programs do no 
assignments whatsoever. It is very common to use Lisp in a functional style where 
new variables may be introduced, but once a new variable is established, it never 
changes. One way to introduce a new variable is as a parameter of a function. It 
is also possible to introduce local variables using the special form 1 et. Following 
are the general 1 et form, along with an example. Each variable is bound to the 
corresponding value, and then the body is evaluated: 

<a id='page-56'></a>

(let((variablevalue)..,) (let ((x 40) 
body...) (y (+ 1 1))) 
(+ X y)) 42 

Defining a local variable with a 1 et form is really no different from defining parameters 
to an anonymous function. The former is equivalent to: 

((lambdei(variable..,) ((lambda (x y) 
body...) (+ X y)) 
value..,) 40 

(+ 1 D) 

First, all the values are evaluated. Then they are bound to the variables (the parameters 
of the lambda expression), and finally the body is evaluated, using those 
bindings. 

The special form 1 et* is appropriate when you want to use one of the newly 
introduced variables in a subsequent value computation. For example: 

(let* ((x 6) 
(y (* . .))) 
(+ . y)) 42 

We could not have used 1 et here, because then the variable . would be unbound 

during the computation of y's value. 

&#9635; Exercise 3.1 [m] Show a 1 ambda expression that is equivalent to the above 1 et* 
expression. You may need more than one 1 ambda. 

Because lists are so important to Lisp, there are special forms for adding and 
deleting elements from the front of a list—in other words, for treating a list as a stack. 
If 1 i st is the name of a location that holds a list, then (push A: 1 i st) will change 1 i st 
to have . as its first element, and (pop 1 i st) will return the first element and, as 
a side-effect, change 1 i st to no longer contain the first element, push and pop are 
equivalent to the following expressions: 

(push . list) = (setf list (cons . list)) 

(pop list) = (let ((result (first list))) 
(setf list (rest list)) 
result) 

Just as a Hst can be used to accumulate elements, a running sum can be used to 
accumulate numbers. Lisp provides two more special forms, 1 ncf and decf, that can 
be used to increment or decrement a sum. For both forms the first argument must 

<a id='page-57'></a>
be a location (a variable or other setf-able form) and the second argument, which 
is optional, is the number to increment or decrement by. For those who know C, 
(incf x) is equivalent to -H-X, and (incf . 2) is equivalent to x+=2. In Lisp the 
equivalence is: 

(incf x) = (incf . 1) = (setf . (+ . D) 
(decf x) = (decf . 1) = (setf . (- . D) 

When the location is a complex form rather than a variable. Lisp is careful to expand 
into code that does not evaluate any subform more than once. This holds for push, 
pop, 1 ncf, and decf. In the following example, we have a list of players and want 
to decide which player has the highest score, and thus has won the game. The 
structure pi ayer has slots for the player's score and number of wins, and the function 
determi ne -wi nner increments the winning player's w1 ns field. The expansion of the 
i ncf form binds a temporary variable so that the sort is not done twice. 

(defstruct player (score 0) (wins 0)) 

(defun determine-winner (players) 

"Increment the WINS for the player with highest score." 

(incf (player-wins (first (sort players #*> 

:key #'player-score))))) 

(defun determine-winner (players) 

"Increment the WINS for the player with highest score." 

(let ((temp (first (sort players #'> :key #'player-score)))) 

(setf (player-wins temp) (+ (player-wins temp) 1)))) 

Functions and Special Forms for Repetition 

Many languages have a small number of reserved words for forming iterative loops. 
For example, Pascal has whi 1 e, repeat, and for statements. In contrast, Conunon 
Lisp has an almost bewildering range of possibilities, as summarized below: 

dolist loop over elements of a list 
dot1mes loop over successive integers 
do, do* general loop, sparse syntax 
loop general loop, verbose syntax 
mapc. mapcar loop over elements of lists(s) 
some, every loop over list until condition 
find, reduce, efc. more specific looping functions 
recursion general repetition 

<a id='page-58'></a>

To explain each possibiUty, we will present versions of the function length, which 
returns the number of elements in a list. First, the special form dol i st can be used 
to iterate over the elements of a list. The syntax is: 

(dol i st (variable list optional-result) body...) 

This means that the body is executed once for each element of the list, with variable 
bound to the first element, then the second element, and so on. At the end, 
dol i st evaluates and returns the optional-result expression, or nil if there is no result 
expression. 

Below is a version of length usingdol i st. The 1 et form introduces anew variable, 
1 en, which is initially bound to zero. The dol i st form then executes the body once 
for each element of the list, with the body incrementing 1 en by one each time. This 
use is unusual in that the loop iteration variable, el ement, is not used in the body. 

(defun length1 (list ) 
(let (den 0)) start with LEN=0 
(dolist (element list ) and on each iteration 
(incf len)) increment LEN by 1 
len)) and return LEN 

It is also possible to use the optional result of dol i st, as shown below. While many 
programmers use this style, I find that it is too easy to lose track of the result, and so 
I prefer to place the result last explictly. 

(defun length1.1 (list) ; alternate version: 
(let (den 0)) ; (not my preference) 
(dolist (element list len) ; uses len as result here 
(incf len)))) 

The function mapc performs much the same operation as the special form dol i st. In 
the simplest case, mapc takes two arguments, the first a function, the second a list. It 
applies the function to each element of the list. Here is length using mapc: 

(defun lengthZ (list) 
(let (den 0)) ; start with LEN=0 
(mapc #'dambda (element) ; and on each iteration 
(incf len)) ; increment LEN by 1 
list) 
len)) ; and return LEN 

There are seven different mapping functions, of which the most useful are mapc and 
mapca r. mapca r executes the same function calls as mapc, but then returns the results 

<a id='page-59'></a>
in a list. 

There is also a dot i mes form, which has the syntax: 

(dot i mes (variable number optional-result) body,..) 
and executes the body with variable bound first to zero, then one, all the way up to 
number-1 (for a total of number times). Of course, dot i mes is not appropriate for 
implementing length, since we don't know the number of iterations ahead of time. 
There are two very general looping forms, do and 1 oop. The syntax of do is as 
follows: 

(do ((variable initial next)...) 
(exit-test result) 
body...) 

Each variable is initially bound to the initial value. If exit-test is true, then result is returned. 
Otherwise, the body is executed and each variable is set to the corresponding 
next value and exit-test is tried again. The loop repeats until exit-test is true. If a next 
value is omitted, then the corresponding variable is not updated each time through 
the loop. Rather, it is treated as if it had been bound with a 1 et form. 

Here is length implemented withdo,usingtwo variables, 1 en to count the number 
of elements, and 1 to go down the list. This is often referred to as cdr-ing down a list, 
because on each operation we apply the function cdr to the list. (Actually, here we 
have used the more mnemonic name rest instead of cdr.) Note that the do loop has 
no body! All the computation is done in the variable initialization and stepping, and 
in the end test. 

(defun lengths (list) 
(do (den 0 (+ len D) ; start with LEN=0. increment 
(1 list (rest 1))) ; ... on each iteration 
((null 1) len))) ; (until the end of the list) 

I find the do form a little confusing, because it does not clearly say that we are looping 
through a list. To see that it is indeed iterating over the list requires looking at both 
the variable 1 and the end test. Worse, there is no variable that stands for the current 
element of the Ust; we would need to say (first 1 ) to get at it. Both dol i st and 
mapc take care of stepping, end testing, and variable naming automatically. They are 
examples of the "be specific" principle. Because it is so unspecific, do will not be 
used much in this book. However, many good programmers use it, so it is important 
to know how to read do loops, even if you decide never to write one. 

The syntax of 1 oop is an entire language by itself, and a decidedly non-Lisp-like 
language it is. Rather than list all the possibilities for 1 oop, we will just give examples 

<a id='page-60'></a>

here, and refer the reader to Common Lisp the Language, 2d edition, or chapter 24.5 for 
more details. Here are three versions of length using 1 oop: 

(defun length4 (list) 
(loop for element in list ; go through each element 
count t)) ; counting each one 

(defun lengths (11st) 
(loop for element in list ; go through each element 
summing 1)) ; adding 1 each time 

(defun lengthe (list) 

(loop with len = 0 ; start with LEN=0 
until (null list) ; and (until end of list) 
for element = (pop list) ; on each iteration 
do (incf len) ; increment LEN by 1 
finally (return len))) ; and return LEN 

Every programmer learns that there are certain kinds of loops that are used again 
and again. These are often called programming idioms or cliches. An example is going 
through the elements of a list or array and doing some operation to each element. 
In most languages, these idioms do not have an explicit syntactic marker. Instead, 
they are implemented with a general loop construct, and it is up to the reader of the 
program to recognize what the programmer is doing. 

Lisp is unusual in that it provides ways to explicitly encapsulate such idioms, and 
refer to them with explicit syntactic and functional forms, dol 1 st and dotimes are 
two examples of this-they both follow the "be specific" principle. Most programmers 
prefer to use a dol i st rather than an equivalent do, because it cries out "this loop 
iterates over the elements of a list." Of course, the corresponding do form also says 
the same thing—but it takes more work for the reader to discover this. 

In addition to special forms like dol 1 st and dotimes, there are quite a few functions 
that are designed to handle common idioms. Two examples are count-If, 
which counts the number of elements of a sequence that satisfy a predicate, and 
position-If, which returns the index of an element satisfying a predicate. Both 
can be used to implement length. In length7 below, count -1f gives the number of 
elements in 11 st that satisfy the predicate true. Since true is defined to be always 
true, this gives the length of the list. 

(defun length? (list) 
(count-if #*true list)) 

(defun true (x) t) 

In lengthS, the function position -1 f finds the position of an element that satisfies 
the predicate true, starting from the end of the list. This will be the very last element 

<a id='page-61'></a>
of the list, and since indexing is zero-based, we add one to get the length. Admittedly, 
this is not the most straightforward implementation of length. 

(defun lengths (list) 

(if (null list) 
0 
(+ 1 (position-if #*true list :from-end t)))) 

A partial table of functions that implement looping idioms is given below. These 
functions are designed to be flexible enough to handle almost all operations on 
sequences. The flexibility comes in three forms. First, functions like mapcar can 
apply to an arbitrary number of lists, not just one: 

> (mapcar '(1 2 3)) => (-1 -2 -3) 
> (mapcar #'+ '(1 2) '(10 20)) (11 22) 
> (mapcar #'+ '(1 2) '(10 20) '(100 200)) => (111 222) 

Second, many of the functions accept keywords that allow the user to vary the test 
for comparing elements, or to only consider part of the sequence. 

> (remove 1 '(1 2 3 2 1 0 -1)) =4^ (2 3 2 0 -1) 

> (remove 1 '(1 2 3 2 1 0 -1) :key #'abs) ^(2320) 

> (remove 1 '(1 2 3 2 1 0 -1) :test #'<) =>(110 -1) 

> (remove 1 '(1 2 3 2 1 0 -1) rstart 4) (1 2 3 2 0 -1) 

Third, some have corresponding functions ending in -if or -if-not that take a 
predicate rather than an element to match against: 

> (remove-if #Oddp '(1 2 3 2 1 0 -1)) =^(2 2 0) 

> (remove-if-not #'oddp '(123210 -1)) =^(131 -1) 

> (find-if #'evenp '(123210 -1)) 2 

The following two tables assume these two values: 

(setf . '(a b c)) 

(setf y '(1 2 3)) 

The first table lists functions that work on any number of lists but do not accept 
keywords: 

<a id='page-62'></a>

(every #Oddp y) =..i 1 test if every element satisfies a predicate 
(some #Oddp y) => t test if some element satisfies predicate 
(mapcar y) =^(-1 -2 -3) apply function to each element and return result 
(mapc #'print y) prints 12 3 perform operation on each element 

The second table lists functions that have -if and -if-not versions and also 
accept keyword arguments: 

(member 2 y) =^(2 3) see if element is in list 
(count 'b x) =>1 count the number of matching elements 
(delete 1 y) =>(2 3) omit matching elements 
(find 2 y) ^2 find first element that matches 
(position 'a x) =^0 find index of element in sequence 
(reduce #'+ y) apply function to succesive elements 
(remove 2 y) =>(1 3) like del ete, but makes a new copy 
(substitute 4 2 y) =^(14 3) replace elements with new ones 

Repetition through Recursion 

Lisp has gained a reputation as a "recursive" language, meaning that Lisp encourages 
programmers to write functions that call themselves. As we have seen above, there is 
a dizzying number of functions and special forms for writing loops in Common Lisp, 
but it is also true that many programs handle repetition through recursion rather 
than with a syntactic loop. 

One simple definition of length is "the empty list has length 0, and any other list 
has a length which is one more than the length of the rest of the list (after the first 
element)." This translates directly into a recursive function: 

(defun length9 (list) 

(if (null list) 
0 
(+ 1 (length9 (rest list))))) 

This version of length arises naturally from the recursive definition of a list: "a list 
is either the empty list or an element consed onto another list." In general, most 
recursive functions derive from the recursive nature of the data they are operating 
on. Some kinds of data, like binary trees, are hard to deal with in anything but a 
recursive fashion. Others, like Hsts and integers, can be defined either recursively 
(leading to recursive functions) or as a sequence (leading to iterative functions). In 
this book, I tend to use the "list-as-sequence" view rather than the "list-as-first-and-
rest" view. The reason is that defining a hst as a first and a rest is an arbitrary and 
artificial distinction that is based on the implementation of lists that Lisp happens to 
use. But there are many other ways to decompose a list. We could break it into the last 

<a id='page-63'></a>
element and all-but-the-last elements, for example, or the first half and the second 

half. The "list-as-sequence" view makes no such artificial distinction. It treats all 

elements identically. 

One objection to the use of recursive functions is that they are inefficient, because 
the compiler has to allocate memory for each recursive call. This may be true for the 
function length9, but it is not necessarily true for all recursive calls. Consider the 
following definition: 

(defun length1O (list) 
(length1O-aux list 0)) 

(defun length1O-aux (sublist len-so-far) 

(if (null sublist) 
len-so-far 
(length1O-aux (rest sublist) (+ 1 len-so-far)))) 

length1O uses length1O - aux as an auxiliary function, passing it 0 as the length of the 
list so far. 1 engt hlO - a ux then goes down the list to the end, adding 1 for each element. 
The invariant relation is that the length of the sublist plus 1 en- so - fa r always equals 
the length of the original list. Thus, when the sublist is nil, then 1 en-so-f ar is the 
length of the original list. Variables like 1 en- so - fa r that keep track of partial results 
are called accumulators. Other examples of functions that use accumulators include 
f 1 a tten - a 11 on [page 329](chapter10.md#page-329); one- un known on page [page 237](chapter7.md#page-237); the Prolog predicates discussed
on [page 686](chapter20.md#page-686); and anonymous-variables-in on pages [page 400](chapter12.md#page-400) and [page 433](chapter12.md#page-433), which uses two 
accumulators. 

The important difference between length9 and length1O is when the addition 
is done. In length9, the function calls itself, then returns, and then adds 1. In 
length1O-aux, the function adds 1, then calls itself, then returns. There are no 
pending operations to do after the recursive call returns, so the compiler is free to 
release any memory allocated for the original call before making the recursive call. 
length1O-aux is called a tail-recursive function, because the recursive call appears as 
the last thing the function does (the tail). Many compilers will optimize tail-recursive 
calls, although not all do. (Chapter 22 treats tail-recursion in more detail, and points 
out that Scheme compilers guarantee that tail-recursive calls will be optimized.) 

Some find it ugly to introduce length 10 - a ux. For them, there are two alternatives. 

First, we could combine length1O and length1O-aux into a single function with an 

optional parameter: 

(defun length11 (list &optional (len-so-far 0)) 

(if (null list) 
len-so-far 
(length11 (rest list) (+ 1 len-so-far)))) 

<a id='page-64'></a>

Second, we could introduce a local function inside the definition of the main function. 
This is done with the special form 1 abel s: 

(defun length12 (the-list) 
(labels 
((length13 (list len-so-far) 

(if (null list) 
len-so-far 
(length1S (rest list) (+ 1 len-so-far))))) 

(length1S the-list 0))) 

In general, a 1 abel s form (or the similar f 1 et form) can be used to introduce one or 
more local functions. It has the following syntax: 

(labels 

((function-name {parameter...)function-body)...) 
body-of-labels) 

Other Special Forms 

A few more special forms do not fit neatly into any category. We have already seen 
the two special forms for creating constants and functions, quote and function. 
These are so common that they have abbreviations: 'x for (quote x) and #'f for 
(function f). 

The special form progn can be used to evaluate a sequence of forms and return 
the value of the last one: 

(progn (setf . 0) (setf . (+ . D) .) 1 

progn is the equivalent of a begin.. .end block in other languages, but it is used 
very infrequently in Lisp. There are two reasons for this. First, programs written 
in a functional style never need a sequence of actions, because they don't have side 
effects. Second, even when side effects are used, many special forms allow for a 
body which is a sequence—an implicit progn. I can only think of three places where 
a progn is justified. First, to implement side effects in a branch of a two-branched 
conditional, one could use either an i f with a progn, or a cond: 

(if (> X 100) (cond ((> . 100) 
(progn (print "too big") (print "too big") 
(setf X 100)) (setf . 100)) 

X) (t X)) 
<a id='page-65'></a>
If the conditional had only one branch, then when or unl ess should be used, since 
they allow an implicit progn. If there are more than two branches, then cond should 
be used. 

Second, progn is sometimes needed in macros that expand into more than one 
top-level form, as in the defun* macro on [page 326](chapter10.md#page-326), section 10.3. Third, a progn is 
sometimes needed in an unwi nd- protect, an advanced macro. An example of this is 
the wi th- resource macro on [page 338](chapter10.md#page-338), section 10.4. 

The forms trace and untrace are used to control debugging information about 
entry and exit to a function: 

> (trace length9) (LENGTH9) 

> (length9 '(a b c)) 
(1 ENTER LENGTH9: (ABO ) 
(2 ENTER LENGTH9: (BO ) 

(3 ENTER LENGTH9: (O) 
(4 ENTER LENGTH9: NIL) 
(4 EXIT LENGTH9: 0) 

(3 EXIT LENGTH9: 1) 

(2 EXIT LENGTH9: 2) 
(1 EXIT LENGTH9: 3) 
3 

> (untrace length9) => (LENGTH9) 

> (length9 '(a b c)) => 3 

Finally, the special form return can be used to break out of a block of code. Blocks are 
set up by the special form bl ock, or by the looping forms (do, do*, dol i st, dot i mes, or 
loop). For example, the following function computes the product of a list of numbers, 
but if any number is zero, then the whole product must be zero, so we immediately 
return zero from the dol i st loop. Note that this returns from the dol i st only, not 
from the function itself (although in this case, the value returned by dol i st becomes 
the value returned by the function, because it is the last expression in the function). I 
have used uppercase letters in RETURN to emphasize the fact that it is an unusual step 
to exit from a loop. 

(defun product (numbers) 
"Multiply all the numbers together to compute their product." 
(let ((prod D) 

(dolist (n numbers prod) 

(if (= . 0) 
(RETURN 0) 
(setf prod (* . prod)))))) 

<a id='page-66'></a>

Macros 

The preceding discussion has been somewhat cavalier with the term "special form." 
Actually, some of these special forms are really macros, forms that the compiler 
expands into some other code. Common Lisp provides a number of built-in macros 
and allows the user to extend the language by defining new macros. (There is no way 
for the user to define new special forms, however.) 

Macros are defined with the special form def ma c ro. Suppose we wanted to define 
a macro, whi 1 e, that would act like the whi 1 e loop statement of Pascal. Writing a 
macro is a four-step process: 

* Decide if the macro is really necessary. 
* Write down the syntax of the macro. 
* Figure out what the macro should expand into. 
* Use def macro to implement the syntax/expansion correspondence. 
The first step in writing a macro is to recognize that every time you write one, 
you are defining a new language that is just like Lisp except for your new macro. 
The programmer who thinks that way will rightfully be extremely frugal in defining 
macros. (Besides, when someone asks, "What did you get done today?" it sounds 
more impressive to say "I defined a new language and wrote a compiler for it" than 
to say "I just hacked up a couple of macros.") Introducing a macro puts much more 
memory strain on the reader of your program than does introducing a function, 
variable or data type, so it should not be taken lightly. Introduce macros only when 
there is a clear need, and when the macro fits in well with your existing system. As 

C.A.R. Hoare put it, "One thing the language designer should not do is to include 
untried ideas of his own." 
The next step is to decide what code the macro should expand into. It is a good 
idea to follow established Lisp conventions for macro syntax whenever possible. 
Look at the looping macros (dolist, dot i mes, do-symbols), the defining macros 
(defun, defvar, defparameter, defstruct), or the the I/O macros (with-open-file, 
with-open-stream, with-input-from-string), for example. If you follow the naming 
and syntax conventions for one of these instead of inventing your own conventions, 
you'll be doing the reader of your program a favor. For whi 1 e, a good syntax is: 

(while test body...) 

The third step is to write the code that you want a macro call to expand into: 

<a id='page-67'></a>
(loop 
(unless test (return nil)) 
body) 

The final step is to write the definition of the macro, using defmacro. A defmacro 
form is similar to a defun in that it has a parameter list, optional documentation 
string, and body. There are a few differences in what is allowed in the parameter list, 
which will be covered later. Here is a definition of the macro whi 1 e, which takes a 
test and a body, and builds up the 1 oop code shown previously: 

(defmacro while (test &rest body) 
"Repeat body while test is true." 
(list* .... 

(list 'unless test '(return nil)) 
body)) 

(The function 1 i st* is like 11 st, except that the last argument is appended onto the 
end of the list of the other arguments.) We can see what this macro expands into by 
using macroexpand, and see how it runs by typing in an example: 

> (macroexpand-1 '(while (< i 10) 
(print (* i i)) 
(setf i (+ i 1)))) ^ 

(LOOP (UNLESS (<I 10) (RETURN NIL)) 
(PRINT (* I I)) 
(SETF I (+ I 1))) 

> (setf i 7) => 7 

> (while (< i 10) 
(print (* i i)) 
(setf i (+ i 1))) => 

49 
64 
81 
NIL 

Section 24.6 ([page 853](chapter24.md#page-853)) describes a more complicated macro and some details on the 
pitfalls of writing complicated macros ([page 855](chapter24.md#page-855)). 

Backquote Notation 

The hardest part about defining whi 1 e is building the code that is the expansion of 
the macro. It would be nice if there was a more immediate way of building code. 
The following version of while following attempts to do just that. It defines the local 

<a id='page-68'></a>

variable code to be a template for the code we want, and then substitutes the real 
values of the variables test and body for the placeholders in the code. This is done 
with the function subst; (subst new old tree) substitutes new for each occurrence of 
old anywhere within tree. 

(defmacro while (test &rest body) 

"Repeat body while test is true." 

(let ((code '(loop (unless test (return nil)) . body))) 

(subst test 'test (subst body 'body code)))) 

The need to build up code (and noncode data) from components is so frequent that 
there is a special notation for it, the backquote notation. The backquote character 
"'" is similar to the quote character " . A backquote indicates that what follows is 
mostly a literal expression but may contain some components that are to be evaluated. 
Anything marked by a leading comma"," is evaluated and inserted into the structure, 
and anything marked with a leading " ,@" must evaluate to a Hst that is spliced into 
the structure: each element of the list is inserted, without the top-level parentheses. 
The notation is covered in more detail in section 23.5. Here we use the combination 
of backquote and comma to rewrite whi 1 e: 

(defmacro while (test &rest body) 

"Repeat body while test is true." 

'(loop (unless .test (return nil)) 

.body)) 

Here are some more examples of backquote. Note that at the end of a list,", @" has the 
same effect as "." followed by ",". In the middle of a list, only ", @" is a possibility. 

> (setf testl '(a test)) => (A TEST) 

> '(this is .testl) => (THIS IS (A TEST)) 

> '(this is .testl) =i> (THIS IS A TEST) 

> '(this is . .testl) (THIS IS A TEST) 

> '(this is .testl -- this is only .testl) 
(THIS IS A TEST THIS IS ONLY A TEST) 

This completes the section on special forms and macros. The remaining sections of 
this chapter give an overview of the important built-in functions in Common Lisp. 

<a id='page-69'></a>
3.3 Functions on Lists 

For the sake of example, assume we have the following assignments: 

(setf . '(a b c)) 
(setf y '(1 2 3)) 

The most important functions on lists are summarized here. The more complicated 
ones are explained more thoroughly when they are used. 

(first x) a first element of a list 

(second x) =>b second element of a list 

(third x) third element of a list 

(nth 0 x) => a nth element of a list, 0-based 

(rest x) => (b c) all but the first element 

(car x) => a another name for the first element of a list 

(cdr x) =>(b c) another name for all but the first element 

(last x) =i>(c) last cons cell in a list 

(length x) =^3 number of elements in a list 

(reverse x) =>(c b a) puts list in reverse order 

(cons 0 y) =>(0 1 2 3) add to front of list 

(append . y) =i>(a b c 1 2 3) append together elements 

(list . y) =>i{d b c) (1 2 3)) make a new list 

(list* 1 2 .) =>(1 2 a b c) append last argument to others 

(null nil) =>J predicate is true of the empty list 

(null x) =>nil ... and false for everything else 

distp x) =>T predicate is true of any list, including . i1 

distp 3) => nil ... and is false for nonlists 

(consp x) =>t predicate is true of non-nil lists 

(consp nil) =>nil ... and false for atoms, including . i1 

(equal . .) =^t true for lists that look the same 

(equal . y) nil ... and false for lists that look different 

(sort y #'>) =^(3 2 1) sort a list according to a comparison function 

(subseq . 1 2) => (B) subsequence with given start and end points 

We said that (cons a b) builds a longer list by adding element a to the front of list 
b, but what if b is not a list? This is not an error; the result is an object . such that 
(firstjc) =^a, (restjc) b, and where ;c prints as ia . b). This is known as dotted 
pair notation. If i? is a list, then the usual list notation is used for output rather than 
the dotted pair notation. But either notation can be used for input. 

So far we have been thinking of lists as sequences, using phrases like "a list of 
three elements." The list is a convenient abstraction, but the actual implementation 
of lists relies on lower-level building blocks called cons cells. A cons cell is a data 
structure with two fields: a first and a rest. What we have been calling "a list of 
three elements" can also be seen as a single cons cell, whose first field points to 

<a id='page-70'></a>

the first element and whose rest field points to another cons cell that is a cons cell 
representing a Ust of two elements. This second cons cell has a rest field that is a 
third cons cell, one whose rest field is nil. All proper lists have a last cons cell whose 
rest field is nil. Figure 3.1 shows the cons cell notation for the three-element list (one 
two three), as well as for the result of (cons One 'two). 

(ONE TWO THREE) (ONE . TWO) 

ONE TWO THREE ONE TWO 

Figure 3.1: Cons Cell Diagrams 

&#9635; Exercise 3.2 [s] The function cons can be seen as a special case of one of the other 
functions listed previously. Which one? 

&#9635; Exercise 3.3 [m] Write a function that will print an expression in dotted pair notation. 
Use the built-in function pri nc to print each component of the expression. 

&#9635; Exercise 3.4 [m] Write a function that, like the regular print function, will print an 
expression in dotted pair notation when necessary but will use normal list notation 
when possible. 

3.4 Equality and Internal Representation 
In Lisp there are five major equality predicates, because not all objects are created 
equally equal. The numeric equality predicate, =, tests if two numbers are the same. 
It is an error to apply = to non-numbers. The other equality predicates operate 
on any kind of object, but to understand the difference between them, we need to 
understand some of the internals of Lisp. 

When Lisp reads a symbol in two different places, the result is guaranteed to be 
the exact same symbol. The Lisp system maintains a symbol table that the function 
read uses to map between characters and symbols. But when a list is read (or built) 

<a id='page-71'></a>
in two different places, the results are not identically the same, even though the 
corresponding elements may be. This is because read calls cons to build up the list, 
and each call to cons returns a new cons cell. Figure 3.2 shows two lists, x and y, 
which are both equal to (one two), but which are composed of different cons cells, 
and hence are not identical. Figure 3.3 shows that the expression (rest x) does not 
generate new cons cells, but rather shares structure with x, and that the expression 
(cons ' zero x) generates exactly one new cons cell, whose rest is x. 

(setf X '(one two)) 

ONE TWO 

(setf y '(one two)) 

Figure 3.2: Equal But Nonidentical Lists 

(cons 'zero x) . (restx) 

1 
1 

ZERO ONE TWO 

Figure 3.3: Parts of Lists 

<a id='page-72'></a>

When two mathematically equal numbers are read (or computed) in two places, 
they may or may not be the same, depending on what the designers of your implementation 
felt was more efficient. In most systems, two equal fixnums will be identical, 
but equal numbers of other types will not (except possibly short floats). Common 
Lisp provides four equality predicates of increasing generality. All four begin with 
the letters eq, with more letters meaning the predicate considers more objects to be 
equal. The simplest predicate is eq, which tests for the exact same object. Next, 
eql tests for objects that are either eq or are equivalent numbers, equal tests for 
objects that are either eql or are lists or strings with eql elements. Finally, equal . 
is like equal except it also matches upper- and lowercase characters and numbers 
of different types. The following table summarizes the results of applying each of 
the four predicates to various values of . and y. The ? value means that the result 
depends on your implementation: two integers that are eql may or may not be eq. 

X eq eql equal equal .

y 
'x 'X . . . . 
. . . . .

? 

'(.) '(.) nil nil . . 

'"xy" '"xy" nil nil . . 

"'Xy" '".." nil nil nil . 

'0 ... nil nil nil . 
. . nil nil nil nil 
In addition, there are specialized equaUty predicates such as =, tree -equal, 
char-equal, and string-equal, which compare numbers, trees, characters, and 
strings, respectively. 

3.5 Functions on Sequences 
Common Lisp is in a transitional position halfway between the Lisps of the past 
and the Lisps of the future. Nowhere is that more apparent than in the sequence 
functions. The earliest Lisps dealt only with symbols, numbers, and lists, and 
provided Hst functions like append and length. More modern Lisps added support 
for vectors, strings, and other data types, and introduced the term sequence to refer 
to both vectors and lists. (A vector is a one-dimensional array. It can be represented 
more compactly than a list, because there is no need to store the rest pointers. It 
is also more efficient to get at the nth element of a vector, because there is no need 
to follow a chain of pointers.) Modern Lisps also support strings that are vectors of 
characters, and hence also a subtype of sequence. 

With the new data types came the problem of naming functions that operated 
on them. In some cases. Common Lisp chose to extend an old function: length can 

<a id='page-73'></a>
apply to vectors as well as lists. In other cases, the old names were reserved for the 
list functions, and new names were invented for generic sequence functions. For 
example, append and mapcar only work on lists, but concatenate and map work on 
any kind of sequence. In still other cases, new functions were invented for specific 
data types. For example, there are seven functions to pick the nth element out of a 
sequence. The most general is e 11, which works on any kind of sequence, but there are 
specific functions for lists, arrays, strings, bit vectors, simple bit vectors, and simple 
vectors. Confusingly, nth is the only one that takes the index as the first argument: 

(nth . list) 
ieM sequence n) 
{aref array n) 
{char string n) 
(bit bit vector n) 
(sb i t simple-hit vector .) 
(sV ref simple-vector .) 

The most important sequence functions are listed elsewhere in this chapter, depending 
on their particular purpose. 

3.6 Functions for Maintaining Tables 
Lisp lists can be used to represent a one-dimensional sequence of objects. Because 
they are so versatile, they have been put to other purposes, such as representing 
tables of information. The association list is a type of list used to implement tables. 
An association list is a list of dotted pairs, where each pair consists of a key and a value. 
Together, the list of pairs form a table: given a key, we can retrieve the corresponding 
value from the table, or verify that there is no such key stored in the table. Here's 
an example for looking up the names of states by their two-letter abbreviation. The 
function a s s oc is used. It returns the key/value pair (if there is one). To get the value, 
we just take the cdr of the result returned by assoc. 

(setf state-table 
'((AL . Alabama) (AK . Alaska) (AZ . Arizona) (AR . Arkansas))) 

> (assoc .. state-table) ^ (AK . ALASKA) 

> (cdr (assoc *AK state-table)) ^ ALASKA 

> (assoc 'TX state-table) => NIL 

If we want to search the table by value rather than by key, we can use rassoc: 

> (rassoc 'Arizona table) (AZ . ARIZONA) 

<a id='page-74'></a>

> (car (rassoc 'Arizona table)) => AZ 

Managing a table with assoc is simple, but there is one drawback: we have to search 
through the whole list one element at a time. If the list is very long, this may take 
a while. 

Another way to manage tables is with hash tables. These are designed to handle 
large amounts of data efficiently but have a degree of overhead that can make 
them inappropriate for small tables. The function gethash works much like get—it 
takes two arguments, a key and a table. The table itself is initialized with a call to 
make-hash-tab! e and modified with a setf of gethash: 

(setf table (make-hash-table)) 

(setf (gethash 'AL table) 'Alabama) 
(setf (gethash .. table) 'Alaska) 
(setf (gethash .. table) 'Arizona) 
(setf (gethash 'AR table) 'Arkansas) 

Here we retrieve values from the table: 

> (gethash .. table) ^ ALASKA 
> (gethash 'TX table) => NIL 

The function remhash removes a key/value pair from a hash table, cl rhash removes 
all pairs, and maphash can be used to map over the key/value pairs. The keys to hash 
tables are not restricted; they can be any Lisp object. There are many more details 
on the implementation of hash tables in Common Lisp, and an extensive Uterature 
on their theory. 

A third way to represent table is with property lists. A property list is a Hst of 
alternating key/value pairs. Property lists (sometimes called p-lists or plists) and 
association lists (sometimes called a-lists or alists) are similar: 

a-list; iikeyi . vah) {keyi . vali) ... {keyn . vain)) 

p-list: {key I val\ key 2 vah ... key . vain) 

Given this representation, there is little to choose between a-Hsts and p-lists. They 
are slightly different permutations of the same information. The difference is in how 
they are normally used. Every symbol has a property list associated with it. That 
means we can associate a property/value pair directly with a symbol. Most programs 
use only a few different properties but have many instances of property/value pairs 
for each property. Thus, each symbol's p-list wiH likely be short. In our example, 
we are only interested in one property: the state associated with each abbreviation. 

<a id='page-75'></a>
That means that the property lists will be very short indeed: one property for each 
abbreviation, instead of a list of 50 pairs in the association list implementation. 

Property values are retrieved with the function get, which takes two arguments: 
the first is a symbol for which we are seeking information, and the second is the 
property of that symbol that we are interested in. get returns the value of that 
property, if one has been stored. Property/value pairs can be stored under a symbol 
with a setf form. A table would be built as follows: 

(setf (get 'AL 'state) 'Alabama) 

(setf (get .. 'state) 'Alaska) 
(setf (get .. 'state) 'Arizona) 
(setf (get 'AR 'state) 'Arkansas) 

Now we can retrieve values with get: 

> (get .. 'state) => ALASKA 
> (get 'TX 'state) => NIL 

This will be faster because we can go immediately from a symbol to its lone property 
value, regardless of the number of symbols that have properties. However, if a given 
symbol has more than one property, then we still have to search linearly through the 
property list. As Abraham Lincoln might have said, you can make some of the table 
lookups faster some of the time, but you can't make all the table lookups faster all 
of the time. Notice that there is no equivalent of rassoc using property lists; if you 
want to get from a state to its abbreviation, you could store the abbreviation under a 
property of the state, but that would be a separate setf form, as in: 

(setf (get 'Arizona 'abbrev) *AZ) 

In fact, when source, property, and value are all symbols, there are quite a few 
possibilities for how to use properties. We could have mimicked the a-list approach, 
and Usted all the properties under a single symbol, using setf on the function 
symbol - pi i st (which gives a symbol's complete property list): 

(setf (symbol-piist 'state-table) 
'(AL Alabama AK Alaska AZ Arizona AR Arkansas)) 

> (get 'state-table 'AD => ALASKA 

> (get 'state-table 'Alaska) NIL 

Property lists have a long history in Lisp, but they are falling out of favor as new 
alternatives such as hash tables are introduced. There are two main reasons why 
property lists are avoided. First, because symbols and their property lists are global. 

<a id='page-76'></a>

it is easy to get conflicts when trying to put together two programs that use property 
lists. If two programs use the same property for different purposes, they cannot be 
used together. Even if two programs use different properties on the same symbols, 
they will slow each other down. Second, property lists are messy. There is no way to 
remove quickly every element of a table implemented with property Hsts. In contrast, 
this can be done trivially with cl rhash on hash tables, or by setting an association 
Hst to nil. 

3.7 Functions on Trees 
Many Common Lisp functions treat the expression ((a b) ((c)) (d e))as a 
sequence of three elements, but there are a few functions that treat it as a tree with 
five non-null leaves. The function copy - tree creates a copy of a tree, and tree - equa 1 
tests if two trees are equal by traversing cons cells, but not other complex data like 
vectors or strings. In that respect, tree-equal is similar to equal, but tree-equal is 
more powerful because it allows a : test keyword: 

> (setf tree '((a b) ((c)) (d e))) 

> (tree-equal tree (copy-tree tree)) . 

(defun same-shape-tree (a b) 
"Are two trees the same except for the leaves?" 
(tree-equal a b :test #*true)) 

(defun true (&rest ignore) t) 

> (same-shape-tree tree '((1 2) ((3)) (4 5))) ^ . 

> (same-shape-tree tree '((1 2) (3) (4 5))) => NIL 

Figure3.4shows thetree ((a b) ((c)) (d e)) as a cons ceU diagram. 

There are also two functions for substituting a new expression for an old one 
anywhere within a tree, subst substitutes a single value for another, while sub! i s 
takes a list of substitutions in the form of an association Hst of (old . new) pairs. 
Note that the order of old and new in the a-Hst for subl i s is reversed from the order 
of arguments to subst. The name subl i s is uncharacteristically short and confusing; 
a better name would be subst -1 i St. 

> (subst 'new 'old '(old ((very old))) ^ (NEW ((VERY NEW))) 

> (sublis '((old . new)) '(old ((very old))))=^ (NEW ((VERY NEW))) 

> (subst 'new 'old Old) => 'NEW 

<a id='page-77'></a>
(defun english->french (words) 

(sublis '((are . va) (book . libre) (friend . ami) 
(hello . bonjour) (how . comment) (my . mon) 
(red . rouge) (you . tu)) 

words)) 

> (english->french '(hello my friend - how are you today?)) 
(BONJOUR MON AMI - COMMENT VA TU TODAY?) 

((ab) ((c)) (de)) 

Figure 3.4: Cons Cell Diagram of a Tree 

<a id='page-78'></a>

3.8 Functions on Numbers 
The most commonly used functions on numbers are listed here. There are quite a 
few other numeric functions that have been omitted. 

(+ 4 2) =>6 add 
(- 4 2) =^Z subtract 
(* 4 2) ^8 multiply 
(/ 4 2) =>2 divide 
(> 100 99) greater than (also >=, greater than or equal to) 
(= 100 100) equal (also /=, not equal) 
(< 99 100) less than (also <=, less than or equal to) 
(random 100) =^42 random integer from 0 to 99 
(expt 4 2) =i>16 exponentiation (also exp, and 1 eg) 
(sin pi) ^0.0 sine function (also cos, tan, etc.) 
(asin 0) =>0.0 arcsine or sin~^ function (also acos, atan, etc.) 
(min 2 3 4) =>2 minimum (also max) 
(abs -3) =>3 absolute value 
(sqrt 4) square root 
(round 4.1) round off (also truncate, f 1 cor, cei 1 i ng) 
(rem 11 5) remainder (also mod) 

3.9 Functions on Sets 
One of the important uses of lists is to represent sets. Common Lisp provides 
functions that treat lists in just that way. For example, to see what elements the sets 
r = {a, 6, c,d} and s = {c, d, e} have in common, we could use: 

> (setf . '(a b c d)) (A . C D) 
> (setf s '(c d e))=. (C D E) 
> (intersection r s) = > (C D) 

This implementation returned (C D) as the answer, but another might return (DC). 

They are equivalent sets, so either is valid, and your program should not depend on 

the order of elements in the result. Here are the main functions on sets: 

(intersection r s) => (c d) find common elements of two sets 
(union r s) (a b c d e) find all elements in either of two sets 
(set-difference r s) =>(a b) find elements in one but not other set 
(member *d r) ^(d) check if an element is a member of a set 
(subsetp s r) =>nil see if all elements of one set are in another 
(adjoin 'b s) =^(b c d e) add an element to a set 
(adjoin 'c s) =>{c d e) ... but don't add duplicates 

<a id='page-79'></a>
It is also possible to represent a set with a sequence of bits, given a particular 
universe of discourse. For example, if every set we are interested in must be a subset 
of(a b c d e), then we can use the bit sequence 111 10 to represent (a b cd), 00000 
to represent the empty set, and 11001 to represent (a b e). The bit sequence can be 
represented in Common Lisp as a bit vector, or as an integer in binary notation. For 
example, (a be) would be the bit vector #* 11001 or the integer 25, which can also 
be written as #bllOOL 

The advantage of using bit sequences is that it takes less space to encode a set, 
assuming a small universe. Computation will be faster, because the computer's 
underlying instruction set will typically process 32 elements at a time. 

Common Lisp provides a full complement of functions on both bit vectors and 
integers. The following table lists some, their correspondence to the list functions. 

lists integers bit vectors 
intersection logand bit-and 
union logior bit-ior 
set-difference logandc2 bit-andc2 
member logbitp bit 
length logcount 
For example, 

(intersection '(a bed) '(a b e)) (A B) 
(bit-and #*11110 #*11001) #*11000 
(logand #bllllO #bll001) 24 = #bll000 

3.10 Destructive Functions 
In mathematics, a function is something that computes an output value given some 
input arguments. Functions do not "do" anything, they just compute results. For 
example, if I tell you that . = 4 and y = 5 and ask you to apply the function "plus" to 
X and y, I expect you to tell me 9. IfI then ask, "Now what is the value of x?" it would 
be surprising if . had changed. In mathematics, applying an operator to . can have 
no effect on the value of x. 

In Lisp, some functions are able to take effect beyond just computing the result. 
These "functions" are not functions in the mathematical sense,^ and in other languages 
they are known as "procedures." Of course, most of the Lisp functions are true 
mathematical functions, but the few that are not can cause great problems. They can 

^In mathematics, a function must associate a unique output value with each input value. 

<a id='page-80'></a>

also be quite useful in certain situations. For both reasons, they are worth knowing 
about. 
Consider the following: 

> (setf X '(a b c)) (A . C) 
> (setf y '(1 2 3)) => (1 2 3) 
> (append . y) => (A .C 1 2 3) 

append is a pure function, so after evaluating the call to append, we can rightfully 
expect that . and y retain their values. Now consider this: 

> (nconc X y) (A .C 1 2 3) 
> . => (A .C 1 2 3) 
> y (1 2 3) 

The function nconc computes the same result as append, but it has the side effect 
of altering its first argument. It is called a destructive function, because it destroys 
existing structures, replacing them with new ones. This means that there is quite 
a conceptual load on the programmer who dares to use nconc. He or she must be 
aware that the first argument may be altered, and plan accordingly. This is far more 
complicated than the case with nondestructive functions, where the programmer 
need worry only about the results of a function call. 

The advantage of nconc is that it doesn't use any storage. While append must 
make a complete copy of x and then have that copy end with y, nconc does not need 
to copy anything. Instead, it just changes the rest field of the last element of x to 
point to y. So use destructive functions when you need to conserve storage, but be 
aware of the consequences. 

Besides nconc, many of the destructive functions have names that start with 
n, including nreverse, nintersection, nunion, nset-difference, and nsubst. An 
important exception is del ete, which is the name used for the destructive version of 
remove. Of course, the setf special form can also be used to alter structures, but it 
is the destructive functions that are most dangerous, because it is easier to overlook 
their effects. 

&#9635; Exercise 3.5 [h] (Exercise in altering structure.) Write a program that will play the 
role of the guesser in the game Twenty Questions. The user of the program will have 
in mind any type of thing. The program will ask questions of the user, which must 
be answered yes or no, or "it" when the program has guessed it. If the program runs 
out of guesses, it gives up and asks the user what "it" was. At first the program will 
not play well, but each time it plays, it will remember the user's replies and use them 
for subsequent guesses. 

<a id='page-81'></a>
3.11 Overview of Data Types 
This chapter has been organized around functions, with similar functions grouped 
together. But there is another way of organizing the Common Lisp world: by considering 
the different data types. This is useful for two reasons. First, it gives an 
alternative way of seeing the variety of available functionality. Second, the data types 
themselves are objects in the Common Lisp language, and as we shall see, there are 
functions that manipulate data types. These are useful mainly for testing objects (as 
with the typecase macro) and for making declarations. 

Here is a table of the most commonly used data types: 

Type Example Explanation 
character #\c A single letter, number, or punctuation mark. 
number 42 The most common numbers are floats and integers. 
float 3.14159 A number with a decimal point. 
integer 42 A whole number, of either fixed or indefinite size: 
fixnum 123 An integer that fits in a single word of storage. 
bignum 123456789 An integer of unbounded size. 
function #'sin A function can be applied to an argument list. 
symbol sin Symbols can name fns and vars, and are themselves objects. 
null nil The object ni 1 is the only object of type null. 
keyword :key Keywords are a subtype of symbol. 
sequence (a b c) Sequences include lists and vectors. 
list (a b c) A list is either a cons or nul 1. 
vector #(a b c) A vector is a subtype of sequence. 
cons (a b c) A cons is a non-nil list. 
atom t An atom is anything that is not a cons. 
string "abc" A string is a type of vector of characters. 
array #lA(a b c) Arrays include vectors and higher-dimensional arrays. 
structure #S(type ... ) Structures are defined by defstruct. 
hash-table Hash tables are created by make-hash-tabl e. 

Almost every data type has a recognizer predicate—a function that returns true 
for only elements of that type. In general, a predicate is a function that always 
returns one of two values: true or false. In Lisp, the false value is ni 1 , and every 
other value is considered true, although the most common true value is t. In most 
cases, the recognizer predicate's name is composed of the type name followed by 

p:characterp recognizes characters, numberp recognizes numbers, and so on. For 
example, (numberp 3) returns t because 3 is a number, but (numberp "x") returns 
.i 1 because "." is a string, not a number. 
Unfortunately, Common Lisp is not completely regular. There are no recognizers 
for fixnums, bignums, sequences, and structures. Two recognizers, nul 1 and atom, 
do not end in p. Also note that there is a hyphen before the . in hash-table-p, 
because the type has a hyphen in it. In addition, all the recognizers generated by 
defstruct have a hyphen before the p. 

<a id='page-82'></a>

The function type - of returns the type of its argument, and typep tests if an object 
is of a specified type. The function subtypep tests if one type can be determined to 
be a subtype of another. For example: 

> (type-of 123) ^ FIXNUM 

> (typep 123 'fixnum) . 

> (typep 123 'number) . 

> (typep 123 'integer) => . 

> (typep 123.0 'integer) ^ NIL 

> (subtypep 'fixnum 'number) => . 

The hierarchy of types is rather complicated in Common Lisp. As the prior example 
shows, there are many different numeric types, and a number like 123 is considered 
to be of type fixnum, integer, and number. We will see later that it is also of type 
rational andt. 

The type hierarchy forms a graph, not just a tree. For example, a vector is both 
a sequence and an array, although neither array nor sequence are subtypes of each 
other. Similarly, nul 1 is a subtype of both symbol and 1 i st. 

The following table shows a number of more specialized data types that are not 
used as often: 

Type Example Explanation 

t 42 Every object is of type t. 

nil No object is of type nil. 

complex #C(0 1) Imaginary numbers. 

bit 0 Zero or one. 

rational 2/3 Rationals include integers and ratios. 

ratio 2/3 Exact fractional numbers. 

simple-array #lA(x y) An array that is not displaced or adjustable. 
readtable A mapping from characters to their meanings to read. 

package A collection of symbols that form a module. 

pathname #P'7usr/spool/mail" A file or directory name. 

stream A pointer to an open file; used for reading or printing. 
random-state A state used as a seed by random. 

In addition, there are even more specialized types, such as s ho r t -f 1 oa t, comp i 1 ed f 
uncti on, and bi t-vector. It is also possible to construct more exact types, such as 
(vector (integer 0 3) 100), which represents a vector of 100 elements, each of 
which is an integer from 0 to 3, inclusive. Section 10.1 gives more information on 
types and their use. 

While almost every type has a predicate, it is also true that there are predicates 
that are not type recognizers but rather recognize some more general condition. For 

<a id='page-83'></a>
example, oddp is true only of odd integers, and stri ng-greaterp is true if one string 
is alphabetically greater than another. 

3.12 Input/Output 
Input in Lisp is incredibly easy because a complete lexical and syntactic parser is 
available to the user. The parser is called read. It is used to read and return a single 
Lisp expression. If you can design your application so that it reads Lisp expressions, 
then your input worries are over. Note that the expression parsed by read need not 
be a legal evaluable Lisp expression. That is, you can read ("hello" cons zzz) just 
as well as (+ 2 2). In cases where Lisp expressions are not adequate, the function 
read-char reads a single character, and read-1 i ne reads everything up to the next 
newline and returns it as a string. 

To read from the terminal, the functions read, read-char, or read-line (with 
no arguments) return an expression, a character, and a string up to the end of line, 
respectively. It is also possible to read from a file. The function open or the macro 
with-open-stream can be used to open a file and associate it with a stream, Lisp's 
name for a descriptor of an input/output source. All three read functions take three 
optional arguments. The first is the stream to read from. The second, if true, causes 
an error to be signaled at end of file. If the second argument is nil, then the third 
argument indicates the value to return at end of file. 

Output in Lisp is similar to output in other languages, such as C. There are a 
few low-level functions to do specific kinds of output, and there is a very general 
function to do formatted output. The function print prints any object on a new line, 
with a space following it. pri nl will print any object without the new line and space. 
For both functions, the object is printed in a form that could be processed by read. 
Forexample, the string "hello there" would print as "hello there". Thefunction 
.r i.c is used to print in a human-readable format. The string in question would print 
as hel1 o there with pri nc—the quote marks are not printed. This means that read 
cannot recover the original form; read would interpret it as two symbols, not one 
string. The function wri te accepts eleven different keyword arguments that control 
whether it acts like pri nl or pri .c, among other things. 

The output functions also take a stream as an optional argument. In the following, 
we create the file "test.text" and print two expressions to it. Then we open the 
file for reading, and try to read back the first expression, a single character, and then 
two more expressions. Note that the read-char returns the character #\G, so the 
following read reads the characters OODBYE and turns them into a symbol. The final 
read hits the end of file, and so returns the specified value, eof. 

<a id='page-84'></a>

> (with-open-file (stream "test.text" idirectlon :output) 
(print '(hello there) stream) 
(princ 'goodbye stream)) 

GOODBYE ; and creates the file test.text 

> (with-open-file (stream "test.text" idirection .-input) 
(list (read stream) (read-char stream) (read stream) 
(read stream nil 'eof))) ^ 
((HELLO THERE) #\G OODBYE EOF) 

The function terpri stands for "terminate print line," and it skips to the next line. 
The function fresh -1 i ne also skips to the next line, unless it can be determined that 
the output is already at the start of a line. 

Common Lisp also provides a very general function for doing formatted output, 
called format. The first argument to format is always the stream to print to; use 
t to print to the terminal. The second argument is the format string. It is printed 
out verbatim, except for format directives, which begin with the character " ~". These 
directives tell how to print out the remaining arguments. Users of C's pri ntf function 
or FORTRAN'S format statement should be familiar with this idea. Here's 
an example: 

> (format t "hello, world") 
hello, world 
NIL 

Things get interesting when we put in additional arguments and include format 
directives: 

> (format t "~ra plus -^s is ~f" "two" "two" 4) 
two plus "two" is 4.0 
NIL 

Thedirective "~&" moves to a fresh line, "~a" printsthenextargumentas pri no would, 
" ~ s" prints the next argument as . r i .1 would, and " ~ f" prints a number in floatingpoint 
format. If the argument is not a number, then princ is used, format always 
returns nil. There are 26 different format directives. Here's a more complex example: 

> (let ((numbers '(12 3 4 5))) 
(format t "~&~{~r~" plus "} is ~@r" 

numbers (apply #'+ numbers))) 
one plus two plus three plus four plus five is XV 
NIL 

The directive "~r" prints the next argument, which should be a number, in English, 

<a id='page-85'></a>
and " ~@." prints a number as a roman numeral. The compound directive " ~{..."}" 
takes the next argument, which must be a list, and formats each element of the list 
according to the format string inside the braces. Finally, the directive exits 
from the enclosing "''i..."}" loop if there are no more arguments. You can see that 
format, like 1 oop, comprises almost an entire programming language, which, also 
like 1 oop, is not a very Lisplike language. 

3.13 Debugging Tools 
In many languages, there are two strategies for debugging: (1) edit the program to 
insert print statements, recompile, and try again, or (2) use a debugging program to 
investigate (and perhaps alter) the internal state of the running program. 

Common Lisp admits both these strategies, but it also offers a third: (3) add 
annotations that are not part of the program but have the effect of automatically 
altering the running program. The advantage of the third strategy is that once 
you are done you don't have to go back and undo the changes you would have 
introduced in the first strategy. In addition, Common Lisp provides functions that 
display information about the program. You need not rely solely on looking at the 
source code. 

We have already seen how trace and untrace can be used to provide debugging 
information ([page 65](chapter3.md#page-65)). Another useful tool is st e p, which can be used to halt execution 
before each subform is evaluated. The form (step expression) will evaluate and return 
expression, but pauses at certain points to allow the user to inspect the computation, 
and possibly change things before proceeding to the next step. The commands 
available to the user are implementation-dependent, but typing a ? should give you 
a list of commands. As an example, here we step through an expression twice, the 
first time giving commands to stop at each subevaluation, and the second time giving 
commands to skip to the next function call. In this implementation, the commands 
are control characters, so they do not show up in the output. All output, including 
the symbols <= and => are printed by the stepper itself; I have added no annotation. 

> (step (+ 3 4 (* 5 6 (/ 7 8)))) 

<i= (+ 3 4 (* 5 6 (/ 7 8))) 

<i= 4 =i> 4 
<^ (* 5 6 (/ 7 8)) 
<^ 5 ^ 5 

<^ (/ 7 8) 
7 7 
8 => 8 

^ (/ 7 8) 7/8 

<a id='page-86'></a>

^ (* 5 6 (/ 7 8)) 105/4 
<^ (+ 3 4 (* 5 6 (/ 7 8))) ^ 133/4 
133/4 

> (step (+ 3 4 (* 5 6 (/ 7 8)))) 

^ (+ 3 4 (* 5 6 (/ 7 8))) 
/: 7 8 => 7/8 
*: 5 6 7/8 105/4 
+: 3 4 105/4 133/4 

(+ 3 4 (* 5 6 (/ 7 8))) => 133/4 
133/4 

The functions descri be, i nspect, documentati on, and apropos provide information 
about the state of the current program, apropos prints information about all symbols 
whose name matches the argument: 

> (apropos 'string ) 
MAKE-STRING function (LENGTH &KEY INITIAL-ELEMENT) 
PRINl-TO-STRING function (OBJECT) 
PRINC-TO-STRING function (OBJECT) 
STRING function (X) 

Once you know what obj ect you are interested in, des c r i be can give more information 
on it: 

> (describe 'make-string) 
Symbol MAKE-STRING is in LISP package. 
The function definition is #<FUNCTION MAKE-STRING -42524322>: 

NAME: MAKE-STRING 
ARGLIST: (LENGTH &KEY INITIAL-ELEMENT) 
DOCUMENTATION: "Creates and returns a string of LENGTH elements, 

all set to INITIAL-ELEMENT." 
DEFINITION: (LAMBDA (LENGTH &KEY INITIAL-ELEMENT) 
(MAKE-ARRAY LENGTH :ELEMENT-TYPE 'CHARACTER 
:INITIAL-ELEMENT (OR INITIAL-ELEMENT 

#\SPACE))) 
MAKE-STRING has property INLINE: INLINE 
MAKE-STRING has property :SOURCE-FILE: #P"SYS:KERNEL; STRINGS" 

> (describe 1234.56) 
1234.56 is a single-precision floating-point number. 
Sign 0, exponent #o211. 23-bit fraction #06450754 

If all you want is a symbol's documentation string, the function documentati on will 
do the trick: 

<a id='page-87'></a>

> (documentation 'first 'function) => "Return the first element of LIST.' 
> (documentation 'pi 'variable) => "pi" 

If you want to look at and possibly alter components of a complex structure, 
then i nspect is the tool. In some implementations it invokes a fancy, window-based 
browser. 

Common Lisp also provides a debugger that is entered automatically when an 
error is signalled, either by an inadvertant error or by deliberate action on the part 
of the program. The details of the debugger vary between implementations, but 
there are standard ways of entering it. The function break enters the debugger 
after printing an optional message. It is intended as the primary method for setting 
debugging break points, break is intended only for debugging purposes; when a 
program is deemed to be working, all calls to break should be removed. However, 
it is still a good idea to check for unusual conditions with error, cerror, assert, or 
check -type, which will be described in the following section. 

3.14 Antibugging Tools 
It is a good idea to include antibugging checks in your code, in addition to doing normal 
debugging. Antibugging code checks for errors and possibly takes corrective action. 

The functions error and cerror are used to signal an error condition. These are 
intended to remain in the program even after it has been debugged. The function 
error takes a format string and optional arguments. It signals a fatal error; that is, it 
stops the program and does not offer the user any way of restarting it. For example: 

(defun average (numbers) 

(if (null numbers) 
(error "Average of the empty list is undefined.") 
(/ (reduce #'+ numbers) 

(length numbers)))) 

In many cases, a fatal error is a little drastic. The function cerror stands for continuable 
error, cerror takes two format strings; the first prints a message indicating 
what happens if we continue, and the second prints the error message itself, cerror 
does not actually take any action to repair the error, it just allows the user to signal 
that continuing is alright. In the following implementation, the user continues by 
typing : conti nue. In ANSI Common Lisp, there are additional ways of specifying 
options for continuing. 

<a id='page-88'></a>

(defun average (numbers) 
(if (null numbers) 
(progn 
(cerror "Use 0 as the average." 
"Average of the empty list is undefined.") 
0) 
(/ (reduce #'+ numbers) 
(length numbers)))) 

> (average '()) 
Error: Average of the empty list is undefined. 
Error signaled by function AVERAGE. 
If continued: Use 0 as the average. 
> :continue 
0 

In this example, adding error checking nearly doubled the length of the code. This 
is not unusual; there is a big difference between code that works on the expected 
input and code that covers all possible errors. Common Lisp tries to make it easier 
to do error checking by providing a few special forms. The form ecase stands for 
"exhaustive case" or "error case." It is like a normal case form, except that if none 
of the cases are satisfied, an error message is generated. The form cease stands for 
"continuable case." It is like ecase, except that the error is continuable. The system 
will ask for a new value for the test object until the user supplies one that matches 
one of the programmed cases. 

To make it easier to include error checks without inflating the length of the code 
too much. Common Lisp provides the special forms check-type and assert. As 
the name implies, check-type is used to check the type of an argument. It signals a 
continuable error if the argument has the wrong type. For example: 

(defun sqr (x) 
"Multiply . by itself." 
(check-type . number) 

(* X X)) 

If s qr is called with a non-number argument, an appropriate error message is printed: 

> (sqr "hello") 
Error: the argument X was "hello", which is not a NUMBER. 
If continued: replace X with new value 
> :continue 4 
16 

assert is more general than check-type. In the simplest form, assert tests an 

<a id='page-89'></a>
expression and signals an error if it is false. For example: 

(defun sqr (x) 
"Multiply X by itself." 
(assert (numberp x)) 

(* X X)) 

There is no possibility of continuing from this kind of assertion. It is also possible to 
give assert a list of places that can be modified in an attempt to make the assertion 
true. In this example, the variable . is the only thing that can be changed: 

(defun sqr (x) 
"Multiply X by itself." 
(assert (numberp x) (x)) 

(* X X)) 

If the assertion is violated, an error message will be printed and the user will be given 
the option of continuing by altering x. If . is given a value that satisfies the assertion, 
then the program continues, assert always returns nil. 

Finally, the user who wants more control over the error message can provide 
a format control string and optional arguments. So the most complex syntax for 
assert is: 

(assert test-form (place...) format-ctl-string format-arg...) 

Here is another example. The assertion tests that the temperature of the bear's 
porridge is neither too hot nor too cold. 

(defun eat-porridge (bear) 

(assert (< too-cold (temperature (bear-porridge bear)) too-hot) 
(bear (bear-porridge bear)) 
"~a's porridge is not just right: ~a" 
bear (hotness (bear-porridge bear))) 

(eat (bear-porridge bear))) 

In the interaction below, the assertion failed, and the programmer's error message 
was printed, along with two possibilities for continuing. The user selected one, typed 
in a call to make - por r i dge for the new value, and the function succesfully continued. 

<a id='page-90'></a>

> (eat-porridge momma-bear) 
Error: #<MOMMA BEAR>*s porridge is not just right: 39 
Restart actions (select using :continue): 

0: Supply a new value for BEAR 
1: Supply a new value for (BEAR-PORRIDGE BEAR) 
> :continue 1 
Form to evaluate and use to replace (BEAR-PORRIDGE BEAR): 
(make-porridge :temperature just-right) 
nil 
It may seem like wasted effort to spend time writing assertions that (if all goes well) 
will never be used. However, for all but the perfect programmer, bugs do occur, and 
the time spent antibugging will more than pay for itself in saving debugging time. 

Whenever you develop a complex data structure, such as some kind of data base, 
it is a good idea to develop a corresponding consistency checker. A consistency 
checker is a function that will look over a data structure and test for all possible 
errors. When a new error is discovered, a check for it should be incorporated into 
the consistency checker. Calling the consistency checker is the fastest way to help 
isolate bugs in the data structiu-e. 

In addition, it is a good idea to keep a list of difficult test cases on hand. That 
way, when the program is changed, it will be easy to see if the change reintroduces 
a bug that had been previously removed. This is called regression testing, and Waters 
(1991) presents an interesting tool for maintaining a suite of regression tests. But it 
is simple enough to maintain an informal test suite with a function that calls assert 
on a series of examples: 

(defun test-ex () 
"Test the program EX on a series of examples." 
(i nit-ex) ; Initialize the EX program first, 
(assert (equal (ex 3 4) 5)) 
(assert (equal (ex 5 0) 0)) 
(assert (equal (ex *x 0) 0))) 

Timing Tools 

A program is not complete just because it gives the right output. It must also deliver 
the output in a timely fashion. The form (t i me expression) can be used to see how 
long it takes to execute expression. Some implementations also print statistics on the 
amount of storage required. For example: 

> (defun f (n) (dotimes (i n) nil)) => F 

<a id='page-91'></a>
> (time (f 10000)) => NIL 
Evaluation of (F 10000) took 4.347272 Seconds of elapsed time, 
including 0.0 seconds of paging time for 0 faults, Consed 27 words. 

> (compile 'f) => F 

> (time (f 10000)) NIL 
Evaluation of (F 10000) took 0.011518 Seconds of elapsed time, 
including 0.0 seconds of paging time for 0 faults, Consed 0 words. 

This shows that the compiled version is over 300 times faster and uses less storage 
to boot. Most serious Common Lisp programmers work exclusively with compiled 
functions. However, it is usually a bad idea to worry too much about efficiency details 
while starting to develop a program. It is better to design a flexible program, get it to 
work, and then modify the most frequently used parts to be more efficient. In other 
words, separate the development stage from the fine-tuning stage. Chapters 9 and 
10 give more details on efficiency consideration, and chapter 25 gives more advice 
on debugging and antibugging techniques. 

3.15 Evaluation 
There are three functions for doing evaluation in Lisp: funcall, apply, and eval. 
funcall is used to apply a function to individual arguments, while apply is used 
to apply a function to a list of arguments. Actually, apply can be given one or 
more individual arguments before the final argument, which is always a Ust. eval 
is passed a single argument, which should be an entire form-a function or special 
form followed by its arguments, or perhaps an atom. The following five forms are 
equivalent: 

> (+ 1 2 3 4) 10 
> (funcall #'+12 3 4) ^ 10 
> (apply #'+ '(1 2 3 4))=^ 10 
> (apply #.+ 1 2 '(3 4)) => 10 
> (eval '(+12 3 4)) => 10 

In the past, eval was seen as the key to Lisp's flexibility. In modern Lisps with lexical 
scoping, such as Common Lisp, eval is used less often (in fact, in Scheme there is 
no eval at all). Instead, programmers are expected to use 1 ambda to create a new 
function, and then apply or funcall the function. In general, if you find yourself 
using eval, you are probably doing the wrong thing. 

<a id='page-92'></a>

3.16 Closures 
What does it mean to create a new function? Certainly every time a function (or #') 
special form is evaluated, a function is returned. But in the examples we have seen 
and in the following one, it is always the same function that is returned. 

> (mapcar #'(1ambda (x) (+ . .)) '(1 3 10)) =4>(2 6 20) 

Every time we evaluate the # * (1 ambda ...) form, it returns the function that doubles 
its argument. However, in the general case, a function consists of the body of the 
function coupled with any free lexical vanables that the function references. Such a 
pairing is called a lexical closure, or just a closure, because the lexical variables are 
enclosed within the function. Consider this example: 

(defun adder (c) 
"Return a function that adds c to its argument." 
#'(lambda (x) (+ . c))) 

> (mapcar (adder 3) '(1 3 10)) =^(4 6 13) 

> (mapcar (adder 10) '(1 3 10)) ^ (11 13 20) 

Each time we call adder with a different value for c, it creates a different function, 
the function that adds c to its argument. Since each call to adder creates a new local 
variable named c, each function returned by adder is a unique function. 

Here is another example. The function bank-account returns a closure that can 
be used as a representation of a bank account. The closure captures the local variable 
balance. The body of the closure provides code to access and modify the local 
variable. 

(defun bank-account (balance) 
"Open a bank account starting with the given balance." 
#'(lambda (action amount) 

(case action 
(deposit (setf balance (->' balance amount))) 
(withdraw (setf balance (- balance amount)))))) 

In the following, two calls to bank-account create two different closures, each with 
a separate value for the lexical variable bal a nee. The subsequent calls to the two 
closures change their respective balances, but there is no confusion between the two 
accounts. 

> (setf my-account (bank-account 500.00)) => #<CLOSURE 52330407> 

<a id='page-93'></a>
> (setf your-account (bank-account 250.00)) ^ #<CLOSURE 52331203> 

> (funcall my-account 'withdraw 75.00) 425.0 

> (funcall your-account 'deposit 250.00) ^ 500.0 

> (funcall your-account 'withdraw 100.00) 400.0 

> (funcall my-account 'withdraw 25.00) => 400.0 

This style of programming will be considered in more detail in chapter 13. 

3.17 Special Variables 
Common Lisp provides for two kinds of variables: lexical and special variables. For 
the beginner, it is tempting to equate the special variables in Common Lisp with 
global variables in other languages. Unfortunately, this is not quite correct and can 
lead to problems. It is best to understand Common Lisp variables on their own terms. 

By default. Common Lisp variables are lexical variables. Lexical variables are 
introduced by some syntactic construct like 1 et or defun and get their name from the 
fact that they may only be referred to by code that appears lexically within the body 
of the syntactic construct. The body is called the scope of the variable. 

So far, there is no difference between Common Lisp and other languages. The 
interesting part is when we consider the extent, or lifetime, of a variable. In other 
languages, the extent is the same as the scope: a new local variable is created when a 
block is entered, and the variable goes away when the block is exited. But because it 
is possible to create new functions—closures—in Lisp, it is therefore possible for code 
that references a variable to live on after the scope of the variable has been exited. 
Consider again the bank-account function, which creates a closure representing a 
bank account: 

(defun bank-account (balance) 

"Open a bank account starting with the given balance." 

#'(lambda (action amount) 

(case action 

(deposit (setf balance (+ balance amount))) 

(withdraw (setf balance (- balance amount)))))) 

The function introduces the lexical variable bal anee. The scope of bal anee is the 
body of the function, and therefore references to bal anee can occur only within this 
scope. What happens when ba. k -a ccount is called and exited? Once the body of the 
function has been left, no other code can refer to that instance of bal anee. The scope 
has been exited, but the extent of bal anee lives on. We can call the closure, and it 

<a id='page-94'></a>

can reference bal anee, because the code that created the closure appeared lexically 
within the scope of bal anee. 

In summary. Common Lisp lexical variables are different because they can be 
captured inside closures and referred to even after the flow of control has left their 
scope. 

Now we will consider special variables. A variable is made special by a def va r or 
defparameter form. For example, if we say 

(defvar *counter* 0) 

then we can refer to the special variable ^counter* anywhere in our program. This 
is just like a familiar global variable. The tricky part is that the global binding of 
*counter* can be shadowed by a local binding for that variable. In most languages, 
the local binding would introduce a local lexical variable, but in Common Lisp, special 
variables can be bound both locally and globally. Here is an example: 

(defun report () 
(format t "Counter = '^d " *counter*)) 

> (report) 
Counter = 0 
NIL 

> (let ((*counter* 100)) 

(report)) 
Counter = 100 
NIL 

> (report) 
Counter = 0 
NIL 

There are three calls to report here. In the first and third, report prints the global 
value of the special variable ^counter*. In the second call, the 1 et form introduces 
a new binding for the special variable ^counter*, which is again printed by report. 
Once the scope of the 1 et is exited, the new binding is disestablished, so the final 
call to report uses the global value again. 

In summary. Common Lisp special variables are different because they have 
global scope but admit the possibility of local (dynamic) shadowing. Remember: 
A lexical variable has lexical scope and indefinite extent. A special variable has 
indefinite scope and dynamic extent. 

The function call (symbol - value var), where var evaluates to a symbol, can be 
used to get at the current value of a special variable. To set a special variable, the 
following two forms are completely equivalent: 

<a id='page-95'></a>
(setf (symbol-valuePflr) t7fl/Me) 

(set var value) 

where both var and value are evaluated. There are no corresponding forms for 
accessing and setting lexical variables. Special variables set up a mapping between 
symbols and values that is accessible to the running program. This is unlike lexical 
variables (and all variables in traditional languages) where symbols (identifiers) 
have significance only while the program is being compiled. Once the program is 
running, the identifiers have been compiled away and cannot be used to access the 
variables; only code that appears within the scope of a lexical variable can reference 
that variable. 

&#9635; Exercise 3.6 [s] Given the following initialization for the lexical variable a and the 
special variable *b*, what will be the value of the 1 et form? 

(setf a 'global-a) 
(defvar *b* 'global-b) 

(defun fn () *b*) 

(let ((a 'local-a) 
(*b* 'local-b)) 
(list a *b* (fn) (symbol-value 'a) (symbol-value'*b*))) 

3.18 Multiple Values 
Throughout this book we have spoken of "the value returned by a function." Historically, 
Lisp was designed so that every function returns a value, even those functions 
that are more like procedures than like functions. But sometimes we want a single 
function to return more than one piece of information. Of course, we can do that by 
making up a list or structure to hold the information, but then we have to go to the 
trouble of defining the structure, building an instance each time, and then taking that 
instance apart to look at the pieces. Consider the function round. One way it can be 
used is to round off a floating-point number to the nearest integer. So (round 5.1) is 

5. Sometimes, though not always, the programmer is also interested in the fractional 
part. The function round serves both interested and disinterested programmers by 
returning two values: the rounded integer and the remaining fraction: 
> (round 5.1) 5 .1 

There are two values after the => because round returns two values. Most of the time. 

<a id='page-96'></a>

multiple values are ignored, and only the first value is used. So (* 2 (round 5.1)) 
is 10, just as if round had only returned a single value. If you want to get at multiple 
values, you have to use a special form, such as mul ti pi e-val ue-bi nd: 

(defun show-both (x) 
(multiple-value-bind (int rem) 
(round x) 
(format t "~f = ~d + ~f" . int rem))) 

> (show-both 5.1) 
5.1 = 5 + 0.1 

You can write functions of your own that return multiple values using the function 
val ues, which returns its arguments as multiple values: 

> (values 1 2 3) =i> 1 2 3 

Multiple values are a good solution because they are unobtrusive until they are 
needed. Most of the time when we are using round, we are only interested in the 
integer value. If round did not use multiple values, if it packaged the two values up 
into a list or structure, then it would be harder to use in the normal cases. 

It is also possible to return no values from a function with (values). This is 
sometimes used by procedures that are called for effect, such as printing. For 
example, descri be is defined to print information and then return no values: 

> (describe '.) 
Symbol X is in the USER package. 
It has no value, definition or properties. 

However, when (val ues) or any other expression returning no values is nested in 
a context where a value is expected, it still obeys the Lisp rule of one-value-per-
expression and returns nil. In the following example, descri be returns no values, 
but then 1 i st in effect asks for the first value and gets nil. 

> (list (describe 'x)) 
Symbol X is in AI LP package. 
It has no value, definition or properties. 
(NIL) 

<a id='page-97'></a>
3.19 More about Parameters 
Common Lisp provides the user with a lot of flexibility in specifying the parameters 
to a function, and hence the arguments that the function accepts. Following is a 
program that gives practice in arithmetic. It asks the user a series of . problems, 
where each problem tests the arithmetic operator op (which can be +, -, *, or /, or 
perhaps another binary operator). The arguments to the operator will be random 
integers from 0 to range. Here is the program: 

(defun math-quiz (op range n) 
"Ask the user a series of math problems." 
(dotimes (i .) 

(problem (random range) op (random range)))) 

(defun problem (x op y) 
"Ask a math problem, read a reply, and say if it is correct." 
(format t "~&How much is ~d ~a ~d?" . op y) 
(if (eql (read) (funcall op . y)) 

(princ "Correct!") 

(princ "Sorry, that's not right."))) 

and here is an example of its use: 

> (math-quiz '+ 100 2) 

How much is 32 + 60? 92 

Correct! 

How much is 91 + 19? 100 

Sorry, that's not right. 

One problem with the function math-qui . is that it requires the user to type three 
arguments: the operator, a range, and the number of iterations. The user must 
remember the order of the arguments, and remember to quote the operator. This is 
quite a lot to expect from a user who presumably is just learning to add! 

Common Lisp provides two ways of dealing with this problem. First, a programmer 
can specify that certain arguments are optional, and provide default values for 
those arguments. For example, in math- qui . we can arrange to make be the default 
operator, 100 be the default number range, and 10 be the default number of examples 
with the following definition: 

<a id='page-98'></a>

(defun math-quiz (&optional (op ''.-) (range 100) (n 10)) 

"Ask the user a series of math problems." 

(dotimes (i n) 

(problem (random range) op (random range)))) 

Now (math-quiz) means the same as (math-quiz '+ 100 10). If an optional 
parameter appears alone without a default value, then the default is ni 1. Optional 
parameters are handy; however, what if the user is happy with the operator and 
range but wants to change the number of iterations? Optional parameters are still 
position-dependent, so the only solution is to type in all three arguments: (ma th - qui. 

100 5). 

Common Lisp also allows for parameters that are position-independent. These 
keyword parameters are explicitly named in the function call. They are useful when 
there are a number of parameters that normally take default values but occasionally 
need specific values. For example, we could have defined math- qui . as: 

(defun math-quiz (&key (op '+) (range 100) (n 10)) 

"Ask the user a series of math problems." 

(dotimes (i n) 

(problem (random range) op (random range)))) 

Now (math-quiz :n 5) and (math-quiz :op '+ :n 5 -.range 100) mean the same. 
Keyword arguments are specified by the parameter name preceded by a colon, and 
followed by the value. The keyword/value pairs can come in any order. 

A symbol starting with a colon is called a keyword, and can be used anywhere, 
not just in argument lists. The term keyword is used differently in Lisp than in many 
other languages. For example, in Pascal, keywords (or reserved words) are syntactic 
symbols, like if, el se, begin, and end. In Lisp we call such symbols special form 
operators or just special forms. Lisp keywords are symbols that happen to reside in 
the keyword package."^ They have no special syntactic meaning, although they do 
have the unusual property of being self-evaluating: they are constants that evaluate 
to themselves, unlike other symbols, which evaluate to whatever value was stored in 
the variable named by the symbol. Keywords also happen to be used in specifying 
&key argument lists, but that is by virtue of their value, not by virtue of some syntax 
rule. It is important to remember that keywords are used in the function call, but 
normal nonkeyword symbols are used as parameters in the function definition. 

Just to make things a little more confusing, the symbols &opti onal, &rest, and 
&key are called lambda-list keywords, for historical reasons. Unlike the colon in real 
keywords, the & in lambda-list keywords has no special significance. Consider these 
annotated examples: 

Apackage is a symbol table: a mapping between strings and the symbols they name. 

<a id='page-99'></a>
> :xyz => :XYZ ;keywords are self-evaluating 

> &optional => ; lambda-list keywords are normal symbols 

Error: the symbol &optional has no value 

> '&optional &OPTIONAL 

> (defun f (&xyz) (+ &xyz &xyz)) F ;& has no significance 

> (f 3) =. 6 

> (defun f (:xyz) (+ :xyz :xyz)) ^ 

Error: the keyword :xyz appears in a variable list. 

Keywords are constants, and so cannot be used as names of variables. 

> (defun g (&key . y) (list . y)) G 

> (let ((keys *(:x :y :z))) ;keyword args can be computed 
ig (second keys) 1 (first keys) 2)) => (2 1) 

Many of the functions presented in this chapter take keyword arguments that make 
them more versatile. For example, remember the function f i nd, which can be used 
to look for a particular element in a sequence: 

> (find 3 *(1 2 3 4 -5 6.0)) => 3 

It turns out that find takes several optional keyword arguments. For example, 
suppose we tried to find 6 in this sequence: 

> (find 6 '(1 2 3 4 -5 6.0)) nil 

This fails because f i nd tests for equality with eql, and 6 is not eql to 6.0. However, 
6 is equal . to 6.0, so we could use the : test keyword: 

> (find 6 '(1 2 3 4 -5 6.0) :test #'equalp) ^ 6.0 

In fact, we can specify any binary predicate for the : test keyword; it doesn't have to 
be an equality predicate. For example, we could find the first number that 4 is less 
than: 

> (find 4 '(1 2 3 4 -5 6.0) :test #*<) 6.0 

Now suppose we don't care about the sign of the numbers; if we look for 5, we want 
to find the - 5. We can handle this with the key keyword to take the absolute value of 
each element of the list with the abs function: 

<a id='page-100'></a>

> (find 5 '(1 2 3 4 -5 6.0) ikey #'abs) -5 

Keyword parameters significantly extend the usefulness of built-in functions, and 
they can do the same for functions you define. Among the built-in functions, the most 
common keywords fall into two main groups: :tes t,:tes t - not and : key, which are 
used for matching functions, and : start, :end, and :from-end, which are used on 
sequence functions. Some functions accept both sets of keywords. {Common Lisp the 
Language, 2d edition, discourages the use of :test -not ke3words, although they are 
still a part of the language.) 

The matching functions include sub! i s, posi ti on, subst, uni on, i ntersecti on, 
set -difference, remove, remove-if, subsetp, assoc, find, and member. By default, 
each tests if some item is eql to one or more of a series of other objects. This test can 
be changed by supplying some other predicate as the argument to : test, or it can be 
reversed by specifying :tes t - not. In addition, the comparison can be made against 
some part of the object rather than the whole object by specifying a selector function 
as the : key argument. 

The sequence functions include remove, remove-if, position, and find. The 
most common type of sequence is the list, but strings and vectors can also be used as 
sequences. A sequence function performs some action repeatedly for some elements 
of a sequence. The default is to go through the sequence from beginning to end, but 
the reverse order can be specified with : from-end t, and a subsequence can be 
specifed by supplying a number for the : sta rt or : end keyword. The first element 
of a sequence is numbered 0, not 1, so be careful. 

As an example of keyword parameters, suppose we wanted to write sequence 
functions that are similar to find and find-if, except that they return a list of all 
matching elements rather than just the first matching element. We will call the 
new functions f i nd-a 11 and f i nd-a. - i f. Another way to look at these functions 
is as variations of remove. Instead of removing items that match, they keep all the 
items that match, and remove the ones that don't. Viewed this way, we can see 
that the function f i nd-a 11 - i f is actually the same function as remove- i f -not. It is 
sometimes useful to have two names for the same function viewed in different ways 
(like not and nul 1). The new name could be defined with a defun, but it is easier to 
just copy over the definition: 

(setf (symbol-function 'find-all-if) #'remove-if-not) 

Unfortunately, there is no built-in function that corresponds exactly to f i nd-a 11, so 
we will have to define it. Fortunately, remove can do most of the work. All we have 
to do is arrange to pass remove the complement of the : test predicate. For example, 
finding all elements that are equal to 1 in a list is equivalent to removing elements 
that are not equal to 1: 

<a id='page-101'></a>
> (setf nums '(1 2 3 2 D) (1 2 3 2 1) 

> (find-all 1 nums :test #'=) = (remove 1 nums rtest #V=) (1 1) 

Now what we need is a higher-order function that returns the complement of a 
function. In other words, given =, we want to return /=. This function is called 
compl ement in ANSI Common Lisp, but it was not defined in earlier versions, so it is 
given here: 

(defun complement (fn) 

"If FN returns y, then (complement FN) returns (not y). " 
This function is built-in in ANSI Common Lisp, 
but is defined here for those with non-ANSI compilers. 

#*(lambda (&rest args) (not (apply fn args)))) 

When find-all is called with a given :test predicate, all we have to do is call 
remove with the complement as the :test predicate. This is true even when the 
: test function is not specified, and therefore defaults to eql. We should also test 
for when the user specifies the : test-not predicate, which is used to specify that 
the match succeeds when the predicate is false. It is an error to specify both a : test 
and : test-not argument to the same call, so we need not test for that case. The 
definition is: 

(defun find-all (item sequence &rest keyword-args 

&key (test #*eql) test-not &aHow-other-keys) 
"Find all those elements of sequence that match item, 
according to the keywords. Doesn't alter sequence." 
(if test-not 

(apply #*remove item sequence 
:test-not (complement test-not) keyword-args) 
(apply #.remove item sequence 
:test (complement test) keyword-args))) 

The only hard part about this definition is understanding the parameter list. The 
&rest accumulates all the keyword/value pairs in the variable keyword-args. In 
addition to the &rest parameter, two specific keyword parameters, rtest and 
: test-not, are specified. Any time you put a &key in a parameter Ust, you need 
an &al 1 ow-other- keys if, in fact, other keywords are allowed. In this case we want 

to accept keywords like : sta rt and : key and pass them on to remove. 

All the keyword/value pairs will be accumulated in the Ust keyword - a rgs, including 
the rtest or rtest-not values. SowewiUhave: 

<a id='page-102'></a>
(find-all 1 nums ;test #'= :key #*abs) 
= (remove 1 nums :test (complement #*=) :test #'= :key #*abs) 
^ (1 1) 
Note that the call to remove will contain two : tes t keywords. This is not an error; 
Common Lisp declares that the leftmost value is the one that counts. 

&#9635; Exercise 3.7 [s] Why do you think the leftmost of two keys is the one that counts, 
rather than the rightmost? 

&#9635; Exercise 3.8 [m] Some versions of Kyoto Common Lisp (KCL) have a bug wherein 
they use the rightmost value when more than one keyword/value pair is specified 
for the same keyword. Change the definition of f i nd -a 11 so that it works in KCL. 
There are two more lambda-list keywords that are sometimes used by advanced 
programmers. First, within a macro definition (but not a function definition), the 
symbol &body can be used as a synonym for &rest. The difference is that &body 
instructs certain formatting programs to indent the rest as a body. Thus, if we 
defined the macro: 
(defmacro while2 (test &body body) 
"Repeat body while test is true." 
'(loop (if (not .test) (return nil)) 
. .body)) 
Then the automatic indentation of wh 11 e2 (on certain systems) is prettier than wh 11 e: 
(while (< i 10)
(print (* i D )
(setf i (+ i 1))) 
(while2 (< i 10) 
(print (* i i)) 
(setf i (+ i 1))) 
Finally, an &aux can be used to bind a new local variable or variables, as if bound 
with 1 et*. Personally, I consider this an abomination, because &aux variables are 
not parameters at all and thus have no place in a parameter list. I think they should 
be clearly distinguished as local variables with a 1 et. But some good programmers 
do use &aux, presumably to save space on the page or screen. Against my better 
judgement, I show an example: 
(defun length14 (list &aux (len 0)) 
(dolist (element lis t len) 
(incf len))) 

<a id='page-103'></a>
3.20 The Rest of Lisp 
There is a lot more to Common Lisp than what we have seen here, but this overview 
should be enough for the reader to comprehend the programs in the chapters to 
come. The serious Lisp programmer will further his or her education by continuing 
to consult reference books and online documentation. You may also find part V 
of this book to be helpful, particularly chapter 24, which covers advanced features 
of Common Lisp (such as packages and error handling) and chapter 25, which is a 
collection of troubleshooting hints for the perplexed Lisper. 

While it may be distracting for the beginner to be continually looking at some 
reference source, the alternative—to explain every new function in complete detail as 
it is introduced—would be even more distracting. It would interrupt the description 
of the AI programs, which is what this book is all about. 

3.21 Exercises 
&#9635; Exercise 3.9 [m] Write a version of length using the function reduce. 

&#9635; Exercise 3.10 [m] Use a reference manual or descri be to figure out what the functions 
1 cm and . reconc do. 

&#9635; Exercise 3.11 [m] There is a built-in Common Lisp function that, given a key, a 
value, and an association Hst, returns a new association list that is extended to 
include the key/value pair. What is the name of this function? 

&#9635; Exercise 3.12 [m] Write a single expression using format that will take a list of 
words and print them as a sentence, with the first word capitalized and a period after 
the last word. You will have to consult a reference to learn new format directives. 

3.22 Answers 
Answer 3.2 (consab) = (Mst*ab) 

<a id='page-104'></a>

Answer 3.3 

(defun dprint (x) 
"Print an expression in dotted pair notation.' 
(cond ((atom x) (princ x)) 

(t (princ "(") 
(dprint (first x)) 
(pr-rest (rest x)) 
(princ ")") 

X))) 

(defun pr-rest (x) 
(princ " . ") 
(dprint x)) 

Answer 3.4 Use the same dpri nt function defined in the last exercise, but change 
pr-rest. 

(defun pr-rest (x) 

(cond ((null x)) 
((atom x) (princ " . ") (princ x)) 
(t (princ " ") (dprint (first x)) (pr-rest (rest x))))) 

Answer 3.5 We will keep a data base called *db*. The data base is organized into 
a tree structure of nodes. Each node has three fields: the name of the object it 
represents, a node to go to if the answer is yes, and a node for when the answer is no. 
We traverse the nodes until we either get an "it" reply or have to give up. In the latter 
case, we destructively modify the data base to contain the new information. 

(defstruct node 
name 
(yes nil) 
(no nil)) 

(defvar *db* 

(make-node :name 'animal 
:yes (make-node :name 'mammal) 
:no (make-node 

:name 'vegetable 
:no (make-node :name 'mineral)))) 

<a id='page-105'></a>
(defun questions (&optional (node *db*)) 
(format t "~&Is it a ~a? " (node-name node)) 
(case (read) 

((y yes) (if (not (null (node-yes node))) 
(questions (node-yes node)) 
(setf (node-yes node) (give-up)))) 

((n no) (if (not (null (node-no node))) 
(questions (node-no node)) 
(setf (node-no node) (give-up)))) 

(it 'aha!) 
(t (format t "Reply with YES, NO, or IT if I have guessed it.") 
(questions node)))) 

(defun give-up () 
(format t "~&I give up - what is it? ") 
(make-node :name (read))) 

Here it is used: 

> (questions) 

Is it a ANIMAL? yes 

Is it a MAMMAL? yes 

I give up - what is it? bear 

#S(NODE :NAME BEAR) 

> (questions) 

Is it a ANIMAL? yes 
Is it a MAMMAL? no 

I give up - what is it? penguin 

#S(NODE :NAME PENGUIN) 

> (questions) 

Is it a ANIMAL? yes 
Is it a MAMMAL? yes 
Is it a BEAR? it 

AHA! 

Answer 3.6 The value is (LOCAL-A LOCAL-B LOCAL-B GLOBAL-A LOCAL-B). 

The 1 et form binds a lexically and *b* dynamically, so the references to a and 
*b* (including the reference to *b* within f n) all get the local values. The function 
symbol - value always treats its argument as a special variable, so it ignores the lexical 
binding for a and returns the global binding instead. However, the symbol - va1 ue of 
*b* is the local dynamic value. 

<a id='page-106'></a>

Answer 3.7 There are two good reasons: First, it makes it faster to search through 
the argument list: just search until you find the key, not all the way to the end. 
Second, in the case where you want to override an existing keyword and pass the 
argument list on to another function, it is cheaper to cons the new keyword/value 
pair on the front of a list than to append it to the end of a list. 

Answer 3.9 

(defun length-r (list) 
(reduce #*+ (mapcar #*(lambda (x) 1) list))) 

or more efficiently: 

(defun length-r (list) 
(reduce #'(lambda (x y) (+ . D) list 
rinitial-value 0)) 

or, with an ANSI-compliant Common Lisp, you can specify a : key 

(defun length-r (list) 
(reduce #'+ list :key #'(lambda (x) 1))) 

Answer 3.12 (format t '^@r{'^a'^^ '(this is a test)) 

