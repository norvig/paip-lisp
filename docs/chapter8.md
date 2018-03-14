# Chapter 8 {docsify-ignore}
<a id='page-238'></a>
CHAPTER 8 

Symbolic Mathematics: 
A Simplification Program 

Our life is frittered away by detail— 
Simplify, simplify. 
-Henry David Thoreau, Waiden (1854) 

"Symbolic mathematics" is to numerical mathematics as algebra is to arithmetic: it deals 
with variables and expressions rather than just numbers. Computers were first developed 
L..^primarily to solve arithmetic problems: to add up large columns of numbers, to multiply 
many-digit numbers, to solve systems of linear equations, and to calculate the trajectories of 
ballistics. Encouraged by success in these areas, people hoped that computers could also be used 
on more complex problems; to differentiate or integrate a mathematical expression and come 
up with another expression as the answer, rather than just a number. Several programs were 
developed along these lines in the 1960s and 1970s. They were used primarily by professional 
mathematicians and physicists with access to large mainframe computers. Recently, programs 
like MATHLAB, DERIVE, and .......... have given these capabilities to the average personal 
computer user. 

<a id='page-239'></a>

It is interesting to look at some of the history of symbolic algebra, beginning 
in 1963 with SAINT, James Slagle's program to do symbolic integration. Originally, 
SAINT was heralded as a triumph of AI. It used general problem-solving techniques, 
similar in kind to GPS, to search for solutions to difficult problems. The program 
worked its way through an integration problem by choosing among the techniques 
known to it and backing up when an approach failed to pan out. SAINT'S behavior 
on such problems was originally similar to (and eventually much better than) the 
performance of undergraduate calculus students. 

Over time, the AI component of symbolic integration began to disappear. Joel 
Moses implemented a successor to SAINT called SiN. It used many of the same techniques, 
but instead of relying on search to find the right combination of techniques, 
it had additional mathematical knowledge that led it to pick the right technique at 
each step, without any provision for backing up and trying an alternative. SiN solved 
more problems and was much faster than SAINT, although it was not perfect: it still 
occasionally made the wrong choice and failed to solve a problem it could have. 

By 1970, the mathematician R. Risch and others developed algorithms for indefinite 
integration of any expression involving algebraic, logarithmic, or exponential 
extensions of rational functions. In other words, given a "normal" function, the Risch 
algorithm will return either the indefinite integral of the function or an indication 
that no closed-form integral is possible in terms of elementary functions. Such work 
effectively ended the era of considering integration as a problem in search. 

SIN was further refined, merged with parts of the Risch algorithm, and put into the 
evolving MACSYMA^ program. For the most part, refinement of MACSYMA consisted 
of the incorporation of new algorithms. Few heuristics of any sort survive. Today 
MACSYMA is no longer considered an AI program. It is used daily by scientists and 
mathematicians, while ELIZA and STUDENT are now but historical footnotes. 

With ELIZA and STUDENT we were able to develop miniature programs that duplicated 
most of the features of the original. We won't even try to develop a program 
worthy of the name MACSYMA; instead we will settle for a modest program to do symbolic 
simplification, which we will call (simply) simpli f i er. Then, we will extend 
simpli f i er to do differentiation, and some integration problems. The idea is that 
given an expression like (2-1)a: + 0, we want the program to compute the simplified 
form X. 

According to the Mathematics Dictionary Qames and James 1949), the word "simplified" 
is "probably the most indefinite term used seriously in mathematics." The 
problem is that "simplified" is relative to what you want to use the expression for 
next. Which is simpler, x^ 3x + 2 or (x -\-l){x -\-2)? The first makes it easier to 

^MACSYMA is the Project MAC SYMbolic MAthematics program. Project MAC is the MIT 
research organization that was the precursor of MIT's Laboratory for Computer Science. 
MAC stood either for Machine-Aided Cognition or Multiple-Access Computer, according to 
one of their annual reports. The cynical have claimed that MAC really stood for Man Against 
Computer. 

<a id='page-240'></a>

integrate or differentiate, the second easier to find roots. We will be content to limit 
ourselves to "obvious" simplifications. For example, . is almost always preferable 
toIx + 0. 

8.1 Converting Infix to Prefix Notation 
We will represent simplifications as a list of rules, much like the rules for STUDENT 
and ELIZA. But since each simplification rule is an algebraic equation, we will store 
each one as an exp rather than as a rul e. To make things more legible, we will write 
each expression in infix form, but store them in the prefix form expected by exp. This 
requires an i nf i x->pref i . function to convert infix expressions into prefix notation. 
We have a choice as to how general we want our infix notation to be. Consider: 

(((a * (X ^ 2)) + (b * X)) + c) 
(a*x^2 + b*x + c) 
(a . ^ 2 + b . + c) 
a x^2 + b*x+c 

The first is fully parenthesized infix, the second makes use of operator precedence 
(multiplication binds tighter than addition and is thus performed first), and the third 
makes use of implicit multiplication as well as operator precedence. The fourth 
requires a lexical analyzer to break Lisp symbols into pieces. 

Suppose we only wanted to handle the fully parenthesized case. To write 
i nf i x->pref i ., one might first look at pref i x->i nf i . (on [page 228](chapter7.md#page-228)) trying to adapt 
it to our new purposes. In doing so, the careful reader might discover a surprise: 
infix->prefix and pref ix->inf ix are in fact the exact same function! Both leave 
atoms unchanged, and both transform three-element lists by swapping the exp-op 
and exp -1 hs. Both apply themselves recursively to the (possibly rearranged) input 
list. Once we discover this fact, it would be tempting to avoid writing i .f i . - >p ref i x, 
and just call pref i x->i nf i . instead. Avoid this temptation at all costs. Instead, define 
i nf i x->pref i . as shown below. The intent of your code will be clearer: 

(defun infix->prefix (infix-exp) 
"Convert fully parenthesized infix-exp to a prefix expression" 
;; Don't use this version for non-fully parenthesized exps! 
(prefix->infix infix-exp)) 

As we saw above, fully parenthesized infix can be quite ugly, with all those extra 
parentheses, so instead we will use operator precedence. There are a number of 
ways of doing this, but the easiest way for us to proceed is to use our previously 
defined tool rule-based-translator and its subtool, pat-match. Note that the third 

<a id='page-241'></a>
clause of infix->prefix, the one that calls rule-based-translator is unusual in 
that it consists of a single expression. Most cond-clauses have two expressions: a test 
and a result, but ones like this mean, "Evaluate the test, and if it is non-nil, return it. 
Otherwise go on to the next clause." 

(defun infix->prefix (exp) 
"Translate an infix expression into prefix notation." 
Note we cannot do implicit multiplication in this system 

(cond ((atom exp) exp) 
((= (length exp) 1) (infix->prefix (first exp))) 
((rule-based-translator exp *infix->prefix-rules* 

:rule-if #'rule-pattern :rule-then #*rule-response 

:action 

#*(lambda (bindings response) 
(sublis (mapcar 
#*(lambda (pair) 
(cons (first pair) 
(infix->prefix (rest pair)))) 
bindings) 
response)))) 
((symbolp (first exp)) 

(list (first exp) (infix->prefix (rest exp)))) 
(t (error "Illegal exp")))) 

Because we are doing mathematics in this chapter, we adopt the mathematical con


vention of using certain one-letter variables, and redefine vari abl e-p so that vari


ables are only the symbols m through z. 

(defun variable-p (exp) 
"Variables are the symbols . through Z." 
put x.y.z first to find them a little faster 
(member exp '(xyzmnopqrstuvw))) 

(pat-match-abbrev 'x+ *(?+ x)) 
(pat-match-abbrev 'y+ '(?+ y)) 

(defun rule-pattern (rule) (first rule)) 
(defun rule-response (rule) (second rule)) 

<a id='page-242'></a>

(defpa rameter *i nfi x->prefi x-rules* 
(mapcar #'expand-pat-match-abbrev 

'(((x+ = y+) (= X y)) 
((- x+) (- X)) 
((+X+) (+x)) 
((x+ + y+) (+ X y)) 
((x+ - y+) (-X y)) 
((x+ * y+) (* X y)) 
((x+ / y+) (/ X y)) 
((x+ ^ y+) r X y)))) 

"A list of rules, ordered by precedence.") 

8.2 Simplification Rules 
Now we are ready to define the simplification rules. We use the definition of the data 
types rule and exp ([page 221](chapter7.md#page-221)) and prefix->inf ix ([page 228](chapter7.md#page-228)) from STUDENT. They 
are repeated here: 

(defstruct (rule (:type list)) pattern response) 

(defstruct (exp (:type list) 
(.'constructor mkexp (Ihs op rhs))) 
op Ihs rhs) 

(defun exp-p (x) (consp x)) 
(defun exp-args (x) (rest x)) 

(defun prefix->infix (exp) 
"Translate prefix to infix expressions." 
(if (atom exp) exp 

(mapcar #'prefix->infix 

(if (binary-exp-p exp) 
(list (exp-lhs exp) (exp-op exp) (exp-rhs exp)) 
exp)))) 

(defun binary-exp-p (x) 
(and (exp-p x) (= (length (exp-args x)) 2))) 

We also use rule-based-translator ([page 188](chapter6.md#page-188)) once again, this time on a list of 

simplification rules. A reasonable list of simplification rules is shown below. This 

list covers the four arithmetic operators, addition, subtraction, multipHcation, and 

division, as well as exponentiation (raising to a power), denoted by the symbol 
Again, it is important to note that the rules are ordered, and that later rules will 
be applied only when earlier rules do not match. So, for example, 0/0 simplifies to 

<a id='page-243'></a>
undef i ned, and not to 1 or 0, because the rule for 0 / 0 comes before the other rules. 
See exercise 8.8 for a more complete treatment of this. 

(defparameter *simplification-rules* (mapcar #'infix->prefix *( 
(x + 0 = X) 
(0 + X = X) 
(x + X = 2 * X) 
(X -0 = X) 
(0 -X = - X) 
(X -X = 0) 
(--X = X) 
(X * 1 = X) 
(1 * X = X) 
(X * 0 = 0) 
(0 * X = 0) 
(X * X = X ^ 2) 
(X / 0 = undefined) 
(0 / X = 0) 
(X / 1 = X) 
(X / X = 1) 
(0 ^ 0 = undefined) 
(X ^ 0 = 1) 
(0 X = 0) 
(1 X = 1) 
(X ^ 1 = X) 
(X ^ -1 = 1 / X) 
(X * (y / X) = y) 
((y / X) * X = y) 
((y * X) / X = y) 
((X * y) / X = y) 
(x + -X = 0) 
((- X) + X = 0) 
(x + y -X = y) 
))) 

(defun " (x y) "Exponentiation" (expt . y)) 

We are now ready to go ahead and write the simplif ier. The main function, s i mp 11 f i e r 
will repeatedly print a prompt, read an input, and print it in simplified form. Input 
and output is in infix and the computation is in prefix, so we need to convert accordingly; 
the function simp does this, and the function simpl 1 fy takes care of a single 
prefix expression. It is summarized in figure 8.1. 

<a id='page-244'></a>

Top-Level Functions 

simplifier A read-simplify-print loop. 
simp Simplify an infix expression. 
simplify Simplify a prefix expression. 

Special Variables 

*infix->prefix-rules* Rules to translate from infix to prefix. 
*s i mpli fi cati on-rules* Rules to simplify an expression. 

Data Types 

exp A prefix expression. 

Atixiliary Functions 

simplify-exp Simplify a non-atomic prefix expression. 
infix->prefix Convert infix to prefix notation. 
variable-p The symbols m through . are variables. 

An alias for expt, exponentiation. 
evaluable Decide if an expression can be numerically evaluated. 
simp-rule Transform a rule into proper format. 
length=1 Is the argument a list of length 1? 

Previous Functions 

pat-match Match pattern against an input, (p. 180) 
rule-based-translator Apply a set of rules, (p. 189) 
pat-match-abbrev Define an abbreviation for use in pat-match. 

Figure 8.1: Glossary for the Simplifier 

Here is the program: 

(defun simplifier () 
"Read a mathematical expression, simplify it, and print the result." 
(loop 

(print 'simplifier>) 
(print (simp (read))))) 

(defun simp (inf) (prefix->infix (simplify (infix->prefix inf)))) 

(defun simplify (exp) 
"Simplify an expression by first simplifying its components." 
(if (atom exp) exp 

(simplify-exp (mapcar #'simplify exp)))) 

(defun simplify-exp (exp) 
"Simplify using a rule, or by doing arithmetic." 
(cond ((rule-based-translator exp *simplification-rules* 

:rule-if #'exp-lhs :rule-then #'exp-rhs 
.'action #'(lambda (bindings response) 

(simplify (sublis bindings response))))) 
((evaluable exp) (eval exp)) 
(t exp))) 

<a id='page-245'></a>

(defun evaluable (exp) 
"Is this an arithmetic expression that can be evaluated?" 
(and (every #'numberp (exp-args exp)) 

(or (member (exp-op exp) '(+-*/)) 
(and (eq (exp-op exp) '") 
(integerp (second (exp-args exp))))))) 

The function simplify assures that any compound expression will be simplified by 
first simplifying the arguments and then calling simplify-exp. This latter function 
searches through the simplification rules, much like use-eliza-rules and 
translate-to-expression. When it finds a match, simplify-exp substitutes in the 
proper variable values and calls simplify on the result, simplify-exp also has the 
ability to call eval to simplify an arithmetic expression to a number. As in STUDENT, 
it is for the sake of this eval that we require expressions to be represented as lists in 
prefix notation. Numeric evaluation is done after checking the rules so that the rules 
can intercept expressions like (/ 1 0) and simplify them to undefined. If we did the 
numeric evaluation first, these expressions would yield an error when passed to e va 1. 
Because Common Lisp supports arbitrary precision rational numbers (fractions), we 
are guaranteed there will be no round-off error, unless the input explicitly includes 
inexact (floating-point) numbers. Notice that we allow computations involving the 
four arithmetic operators, but exponentiation is only allowed if the exponent is an 
integer. That is because expressions like (" 4 1/2) are not guaranteed to return 2 
(the exact square root of 4); the answer might be 2.0 (an inexact number). Another 
problem is that -2 is also a square root of 4, and in some contexts it is the correct 
one to use. 

The following trace shows some examples of the simplifier in action. First we 
show that it can be used as a calculator; then we show more advanced problems. 

> (simplifier) 

SIMPLIFIER> (2 + 2) 

4 

SIMPLIFIER> (5 * 20 + 30 + 7) 

137 

SIMPLIFIER> (5 * . - (4 + 1) * x) 
0 

SIMPLIFIER> (y / . * (5 * . - (4 + 1) * .)) 
O 

SIMPLIFIER> ((4 - 3) * . + (y / y - 1) * .) 
X 

SIMPLIFIER> (1 * f(x) + 0) 
(F X) 
SIMPLIFIER> (3 * 2 * X) 
(3 * (2 * X)) 
SIMPLIFIER> [Abort] 
> 

<a id='page-246'></a>

Here we have terminated the loop by hitting the abort key on the terminal. (The details 
of this mechanism varies from one implementation of Common Lisp to another.) The 
simplifier seems to workfairlywell, although it errs on the last example: (3 * (2 * 
X)) should simplify to (6 * X). In the next section, we will correct that problem. 

8.3 Associativity and Commutativity 
We could easily add a rule to rewrite (3 * (2 *X))as((3 * 2) * X) and hence 
(6 * X). The problem is that this rule would also rewrite (X*(2*3))as((X* 

2) * 3), unless we had a way to limit the rule to apply only when it would group 
numbers together. Fortunately, pat-match does provide just this capability, with the 
?i s pattern. We could write this rule: 
(((?is . numberp) * ((?is m numberp) * x)) = ((n * m) * x)) 

This transforms (3 * (2 * x)) into ((3 * 2) * .), and hence into (6 * x). 
Unfortunately, the problem is not as simple as that. We also want to simplify ((2 * 

x) * (y* 3)) to (6 *(x * y)). We can do a better job of gathering numbers together 
by adopting three conventions. First, make numbers first in products: change . * 
3 to 3 * X. Second, combine numbers in an outer expression with a number in an 
inner expression: change 3 *(5*x)to(3*5)*x. Third, move numbers out 
of inner expressions whenever possible: change (3 *x) *yto3*(x*y) . We 
adopt similar conventions for addition, except that we prefer numbers last there: . 
+ 1 instead of 1 + x. 
Define . and m as numbers; s as a non-number: 
(pat-match-abbrev *n '(lis . numberp)) 
(pat-match-abbrev 'm '(?is m numberp)) 
(pat-match-abbrev *s '(?is s not-numberp)) 

(defun not-numberp (x) (not (numberp x))) 

(defun simp-rule (rule) 
"Transform a rule into proper format." 
(let ((exp (infix->prefix rule))) 

(mkexp (expand-pat-match-abbrev (exp-lhs exp)) 
(exp-op exp) (exp-rhs exp)))) 

<a id='page-247'></a>

(setf *simplification-rules* 
(append *simplification-rules* (mapcar #'simp-rule 

'((s * . = . * s) 
(. * (m * X) = (. * m) * .) 
(. * (. * y) = . * (. * y)) 
((. * .) * y = . * (. * y)) 
(. + s = s + .) 
((. + m) + . = . + . ->- m) 
(. + (y + .) = (. + y) + .) 
((. + .) + y = (. + y) + .))))) 

With the new rules in place, we are ready to try again. For some problems we get just 
the right answers: 

> (simplifier) 

SIMPLIFIER> (3 * 2 * x) 
(6 * X) 

SIMPLIFIER> (2 * . * . * 3) 
(6 * (. 2)) 

SIMPLIFIER> (2*x*3*y*4*z*5*6) 
(720 * (X * (Y * .))) 

SIMPLIFIER> (3 + . + 4 + .) 
((2 * .) + 7) 

$IMPLIFIER> (2 *. *3 *. *4*(1 /. )*5*6) 
(720 * .) 

Unfortunately, there are other problems that aren't simplified properly: 

SIMPLIFIER> (3 + . + 4 - x) 
((X + (4 - X)) + 3) 
SIMPLIFIER> (x + y + y + x) 
(X + (Y + (Y + .))) 
SIMPLIFIER> (3 * . + 4 * .) 
((3 * .) + (4 * .)) 

We will return to these problems in section 8.5. 

&#9635; Exercise 8.1 Verify that the set of rules just prior does indeed implement the desired 
conventions, and that the conventions have the proper effect, and always terminate. 
As an example of a potential problem, what would happen if we used the rule (. * 
. = . * .) instead of the rule (s * . = . * s)? 

<a id='page-248'></a>

8.4 Logs, Trig, and Differentiation 
In the previous section, we restricted ourselves to the simple arithmetic functions, 
so as not to intimidate those who are a little leery of complex mathematics. In this 
section, we add a little to the mathematical complexity, without having to alter the 
program itself one bit. Thus, the mathematically shy can safely skip to the next 
section without feeling they are missing any of the fun. 

We start off by representing some elementary properties of the logarithmic and 
trigonometric functions. The new rules are similar to the "zero and one" rules we 
needed for the arithmetic operators, except here the constants e and . i (e = 2.71828... 
and &pi; = 3.14159...) are important in addition to 0 and 1. We also throw in some rules 
relating logs and exponents, and for sums and differences of logs. The rules assume 
that complex numbers are not allowed. If they were, log (and even x^) would have 
multiple values, and it would be wrong to arbitrarily choose one of these values. 

(setf *simplification-rules* 

(append *simplification-ru1es* (mapcar #*simp-rule *( 
(log 1 = 0) 
(log 0 = undefined) 
(log e =1) 
(sin 0 =0) 
(sin pi = 0) 
(cos 0 =1) 
(cos pi = -1) 
(sin(pi / 2) =1) 
(cos(pi / 2) =0) 
(log (e " x) = x) 
(e ^ (log x) = x) 
((x ^ y) * (x ^ z) = . My + z)) 
((x ^ y) / (x M) = . My - z)) 
(log . + log y = log(x * y)) 
(log X - log y = log(x / y)) 
((sin X) ^ 2 + (cos X) ^ 2 = 1) 
)))) 

Now we would like to go a step further and extend the system to handle differentiation. 
This is a favorite problem, and one which has historical significance: in the 
summer of 1958 John McCarthy decided to investigate differentiation as an interesting 
symbolic computation problem, which was difficult to express in the primitive 
programming languages of the day. This investigation led him to see the importance 
of functional arguments and recursive functions in the field of symbolic computation. 
For example, McCarthy invented what we now call mapcar to express the idea 
that the derivative of a sum is the sum of the derivative function applied to each 
argument. Further work led McCarthy to the publication in October 1958 of MIT 

<a id='page-249'></a>
AI Lab Memo No. 1: "An Algebraic Language for the Manipulation of Symbolic 
Expressions/' which defined the precursor of Lisp. 

In McCarthy's work and in many subsequent texts you can see symbolic differentiation 
programs with a simplification routine tacked on the end to make the output 
more readable. Here, we take the opposite approach: the simplification routine is 
central, and differentiation is handled as just another operator, with its own set of 
simplification rules. We will require a new infix-to-prefix translation rule. While 
we're at it, we'll add a rule for indefinite integration as well, although we won't write 
simplification rules for integration yet. Here are the new notations: 

math infix prefix 

dy/dx d y / d X/ydx Int y d X

And here are the necessary infix-to-prefix rules: 

(defparameter *infix->prefix-rules* 
(mapcar #'expand-pat-match-abbrev 

 (d y x) 
(int y .) 

'(((x+ = y+) (= . y)) 
((- x+) (- .)) 
((+.+) (+x)) 
((X-H + y+) (+ X y)) 

((x+ - y+) (- . y)) 
((d y+ / d x) (d y x))
((Int y+ d x) (int y .))
((x+ * y+) (* . y)) 
((x+ / y+) (/ . y)) 
((x+ ^ y+) r . y))))) 

 New rule 
New rule 

Since the new rule for differentiation occurs before the rule for division, there won't 
be any confusion with a differential being interpreted as a quotient. On the other 
hand, there is a potential problem with integrals that contain d as a variable. The 
user can always avoid the problem by using (d) instead of d inside an integral. 

Now we augment the simplification rules, by copying a differentiation table out 
of a reference book: 

(setf *simplification-rules* 

(append *simplification-rules* (mapcar #*simp-rule '( 
(d . / d . =1) 
(d (u + V) / d . = (d u / d X) + (d V / d X)) 
(d (u - V) / d . = (d u / d X) - (d V / d x)) 
(d (- u) / d X = - (d u / d X)) 
(d (u * V) / d X = u * (d V / d X) + V * (d u / d X)) 
(d (u / V) / d X = (V * (d u / d X) - u * (d V / d X)) 

/ V ^ 2) 

<a id='page-250'></a>

(d (u ^ n) / d X = . * u Mn - 1) * (d u / d X)) 
(d (u V) / d X = V * u Mv - 1) * (d u / d X) 

+ u ^ V * (log u) * (d V / d x)) 
(d (log u) / d X = (d u / d X) / u) 
(d (sin u) / d X = (cos u) * (d u / d x)) 
(d (cos u) / d X = - (sin u) * (d u / d x)) 
(d (e ^ u) / d X = (e ^ u) * (d u / d X)) 
(d u / d X = 0))))) 
We have added a default rule, (d u / d . = 0); this should only apply when the 
expression u is free of the variable . (that is, when u is not a function of x). We could 
use ?1 f to check this, but instead we rely on the fact that differentiation is closed over 
the list of operators described here—as long as we don't introduce any new operators, 
the answer will always be correct. Note that there are two rules for exponentiation, 
one for the case when the exponent is a number, and one when it is not. This was 
not strictly necessary, as the second rule covers both cases, but that was the way the 
rules were written in the table of differentials I consulted, so I left both rules in. 

SIMPLIFIER> (d (x + x) / d x) 
2 
SIMPLIFIER> (d (a * x ^ 2 + b * x + c) / d x) 
((2 * (A * X)) + B) 
SIMPLIFIER> (d ((a * x ^ 2 + b * x + c) / x) / d x) 
((((A * (X ^ 2)) + ((B * X) + O) - (X * ((2 * (A * X)) + B))) 

/ (X ^ 2)) 
SIMPLIFIER> (log ((d (x + .) / d x) / 2)) 
0 
SIMPLIFIER> (log(x + x) - log x) 
(LOG 2) 
SIMPLIFIER> (x cos pi) 
(1 / X) 
SIMPLIFIER> (d (3 * x + (cos x) / x) / d x) 
((((COS X) - (X * (- (SIN X)))) / (X ^ 2)) + 3) 
SIMPLIFIER> (d ((cos x) / x) / d x) 
(((COS X) - (X * (- (SIN X)))) / (X ^ 2)) 
SIMPLIFIER> (d (3 * x ^ 2 + 2 * . + 1) / d x) 
((6 * X) + 2) 
SIMPiIFIER> (sin(x + x) ^ 2 + cos(d . ^ 2 / d x) ^ 2) 
1 
SIMPLIFIER> (sin(x + x) * sin(d . " 2 / d x) + 

cos(2 * X)* cos(x * d 2 * y / d y)) 
1 

The program handles differentiation problems well and is seemingly clever in its use 
of the identity sin^ . -f cos^ . = 1. 

<a id='page-251'></a>
8.5 Limits of Rule-Based Approaches 
In this section we return to some examples that pose problems for the simplifier. 
Here is a simple one: 

SIMPLIFIER> (x + y + y + .) => (X + (Y + (Y + X))) 

We would prefer 2 * (x + y). The problem is that, although we went to great trouble 
to group numbers together, there was no effort to group non-numbers. We could 
write rules of the form: 

(y + (y + x) = (2 * y) + x) 

(y + (x + y) = (2 * y) + x) 

Thesewouldworkfortheexampleathand, but they would not work for (. + y + . 

+ y + .). For that we would need more rules: 
(y + (. + (y + X)) = (2 * y) + X + .) 

(y + (. + (. + y)) = (2 * y) + . + .) 

(y + ((y + .) + .) = (2 * y) + . + .) 

(y + ((. + y) + .) = (2 * y) + . + .) 

.. handle all the cases, we would need an infinite number of rules. The pattern-
matching language is not powerful enough to express this succintly. It might help 
if nested sums (and products) were unnested; that is, if we allowed + to take an 
arbitrary number of arguments instead of just one. Once the arguments are grouped 
together, we could sort them, so that, say, all the ys appear before . and after x. Then 
like terms could be grouped together. We have to be careful, though. Consider these 
examples: 

SIMPLIFIER> (3 * . + 4 * x) 

((3 * X) + (4 * X)) 

SIMPLIFIER> (3 *x + y + x + 4 *x) 

((3 * X) + (Y + (X + (4 * X)))) 

We would want (3 * .) to sort to the same place as . and (4 * .) so that they could 
all be combined to (8 * x). In chapter 15, we develop a new version of the program 
that handles this problem. 

<a id='page-252'></a>

8.6 Integration 
So far, the algebraic manipulations have been straightforward. There is a direct 
algorithm for computing the derivative of every expression. When we consider 
integrals, or antiderivatives,^ the picture is much more complicated. As you may 
recall from freshman calculus, there is a fine art to computing integrals. In this 
section, we try to see how far we can get by encoding just a few of the many tricks 
available to the calculus student. 

The first step is to recognize that entries in the simplification table will not be 
enough. Instead, we will need an algorithm to evaluate or "simplify" integrals. 
We will add a new case to simpl ify-exp to check each operator to see if it has a 
simplification function associated with it. These simplification functions will be 
associated with operators through the functions set-simp-fn and simp-fn. If an 
operator does have a simplification function, then that function will be called instead 
of consulting the simplification rules. The simplification function can elect not to 
handle the expression after all by returning nil, in which case we continue with the 
other simplification methods. 

(defun simp-fn (op) (get op 'simp-fn)) 
(defun set-simp-fn (op fn) (setf (get op 'simp-fn) fn)) 

(defun simplify-exp (exp) 
"Simplify using a rule, or by doing arithmetic, 
or by using the simp function supplied for this operator." 
(cond ((simplify-by-fn exp)) 

((rule-based-translator exp *simplification-rules* 
:rule-if #'exp-lhs :rule-then #'exp-rhs 
raction #*(lambda (bindings response) 

(simplify (sublis bindings response))))) 
((evaluable exp) (eval exp)) 
(t exp))) 

(defun simplify-by-fn (exp) 
"If there is a simplification fn for this exp, 
and if applying it gives a non-null result, 
then simplify the result and return that." 
(let* ((fn (simp-fn (exp-op exp))) 

(result (if fn (funcall fn exp)))) 

(if (null result) 
nil 
(simplify result)))) 

Freshman calculus classes teach a variety of integration techniques. Fortunately, 
one technique—the derivative-divides technique—can be adopted to solve most of the 

^The term antiderivative is more correct, because of branch point problems. 

<a id='page-253'></a>
problems that come up at the freshman calculus level, perhaps 90% of the problems 
given on tests. The basic rule is: 

jf{x)dx =

 Jm^dx. 

As an example, consider / xsin{x^)dx. Using the substitution u = x^, we can 
differentiate to get du/dx = 2x, Then by applying the basic rule, we get: 

&int; x sin{x^2)dx = 1/2 &int; sin(u)du/dx dx = 1/2 &int sin(u) du. 

Assume we have a table of integrals that includes the rule / sin(x) dx = - cos(x). 
Then we can get the final answer: 

--cos{x^). 

Abstracting from this example, the general algorithm for integrating an expression 
y with respect to . is: 

1. Pick a factor of y, calling it f{u). 
2. Compute the derivative du/dx. 
3. Divide y by f{u) . du/dx, calling the quotient k. 
4. If/c is a constant (with respect to x), then the result is A: / f{u)du. 
This algorithm is nondeterrninistic, as there may be many factors of y. In our 
example, f{u) = sin(x^),w = x^, and du/dx = 2x. So k = \, and the answer is 

-^COs(x2). 

The first step in implementing this technique is to make sure that division is done 
correctly. We need to be able to pick out the factors of y, divide expressions, and then 
determine if a quotient is free of x. The function f acton* ze does this. It keeps a list 
of factors and a running product of constant factors, and augments them with each 
call to the local function f ac. 

<a id='page-254'></a>

(defun factorize (exp) 
"Return a list of the factors of exp^n, 
where each factor is of the form . y .)." 
(let ((factors nil) 

(constant 1)) 
(labels 
((fac (X n) 
(cond 
((numberp x) 
(setf constant (* constant (expt . .)))) 

((starts-with . **) 
(fac (exp-lhs x) n) 
(fac (exp-rhs x) n)) 

((starts-with . V) 
(fac (exp-lhs x) n) 
(fac (exp-rhs x) (- n))) 

((and (starts-with . *-) (length=1 (exp-args x))) 
(setf constant (- constant)) 
(fac (exp-lhs x) n)) 

((and (starts-with . '") (numberp (exp-rhs x))) 
(fac (exp-lhs x) (* . (exp-rhs x)))) 
(t (let ((factor (find . factors :key #'exp-lhs 
:test #'equal))) 

(if factor 
(incf (exp-rhs factor) n) 
(push . ,x ,n) factors))))))) 

;; Body of factorize: 
(fac exp 1) 
(case constant 

(0 *{r 0 1))) 
(1 factors) 
(t '{. .constant 1) ..factors)))))) 

factor i ze maps from an expression to a list of factors, but we also need unf actor i ze 
to turn a list back into an expression: 

(defun unfactorize (factors) 
"Convert a list of factors back into prefix form." 
(cond ((null factors) 1) 

((length=1 factors) (first factors)) 
(t .(* .(first factors) .(unfactorize (rest factors)))))) 

The derivative-divides method requires a way of dividing two expressions. We do this 
by factoring each expression and then dividing by cancelling factors. There may be 
cases where, for example, two factors in the numerator could be multiplied together 

<a id='page-255'></a>
to cancela factor in the denominator, but this possibility is not considered. It turns 
out that most problems from freshman calculus do not require such sophistication. 

(defun divide-factors (numer denom) 
"Divide a list of factors by another, producing a third." 
(let ((result (mapcar #'copy-list numer))) 

(dolist (d denom) 
(let ((factor (find (exp-lhs d) result :key #*exp-lhs 
:test #*equal))) 

(if factor 
(decf (exp-rhs factor) (exp-rhs d)) 
(push *r ,(exp-lhs d) .(- (exp-rhs d))) result)))) 

(delete 0 result :key #'exp-rhs))) 

Finally, the predicate free-of returns true if an expression does not have any occurrences 
of a particular variable in it. 

(defun free-of (exp var) 
"True if expression has no occurrence of var." 
(not (find-anywhere var exp))) 

(defun find-anywhere (item tree) 
"Does item occur anywhere in tree? If so. return it." 
(cond ((eql item tree) tree) 

((atom tree) nil) 
((find-anywhere item (first tree))) 
((find-anywhere item (rest tree))))) 

In factorize we made use of the auxiliary function length=1. The function call 
(length=1 x) is faster than (= (length x) 1) because the latter has to compute 
the length of the whole list, while the former merely has to see if the list has a rest 
element or not. 

(defun length=1 (x) 
"Is X a list of length 1?" 
(and (consp x) (null (rest x)))) 

Given these preliminaries, the function i ntegrate is fairly easy. We start with 
some simple cases for integrating sums and constant expressions. Then, we factor 
the expression and split the list of factors into two: a Ust of constant factors, and 
a list of factors containing x. (This is done with partition-if, a combination of 
remove-if and remove-if-not.) Finally, we call deri .-divides, giving it a chance 
with each of the factors. If none of them work, we return an expression indicating 
that the integral is unknown. 

<a id='page-256'></a>

(defun integrate (exp x) 
First try some trivial cases 

(cond 
((free-of exp x) *(* ,exp x)) Int c dx = c*x 
((starts-with exp *+) Int f + g = 

*(+ .(integrate (exp-lhs exp) x) Int f + Int g 
.(integrate (exp-rhs exp) x))) 
((starts-with exp *-) 

(ecase (length (exp-args exp)) 
(1 (integrate (exp-lhs exp) x)) Int -f = - Int f 
(2 '(- .(integrate (exp-lhs exp) x) Int f -g = 

.(integrate (exp-rhs exp) x))))) ; Int f - Int g 
Now move the constant factors to the left of the integral 
((multiple-value-bind (const-factors x-factors) 
(partition-if #'(lambda (factor) (free-of factor x)) 
(factorize exp)) 
(simplify 
.(* .(unfactorize const-factors) 
And try to integrate: 
.(cond ((null x-factors) x) 
((some #'(lambda (factor) 
(deriv-divides factor x-factors x)) 

x-factors)) 
;; <other methods here> 
(t *(int? .(unfactorize x-factors) .x))))))))) 

(defun partition-if (pred list) 
"Return 2 values: elements of list that satisfy pred. 
and elements that don't." 
(let ((yes-list nil) 

(no-list nil)) 
(dolist (item list) 

(if (funcall pred item) 
(push item yes-list) 
(push item no-list))) 

(values (nreverse yes-list) (nreverse no-list)))) 

<a id='page-257'></a>
Note that the place in integrate where other techniques could be added is 
marked. We will only implement the derivative-divides method. It turns out that 
the function is a little more complicated than the simple four-step algorithm outlined 
before: 

(defun deriv-divides (factor factors x) 
(assert (starts-with factor 
(let* ((u (exp-lhs factor)) ; factor = u^n 

(n (exp-rhs factor)) 
(k (divide-factors 
factors (factorize '(* .factor ,(deriv u x)))))) 
(cond ((free-of k x) 

Int k*u"n*du/dx dx = k*Int u"n du 
= k*u^(n+l)/(n+l) for . /= -1 
= k*log(u) for . = -1 

(if (= . -1) 
'(* .(unfactorize k) (log .u)) 
*(/ (* ,(unfactorize k) . ,u .(+ . 1))) 

,(+ . 1)))) 
((and (= . 1) (in-integral-table? u)) 
Int y'*f(y) dx = Int f(y) dy 

(let ((k2 (divide-factors 
factors 
(factorize *(* ,u ,(deriv (exp-lhs u) x)))))) 

(if (free-of k2 x) 
.(* .(integrate-from-table (exp-op u) (exp-lhs u)) 
.(unfactorize k2)))))))) 

There are three cases. In any case, all factors are of the form (" u .), so we separate 
the factor into a base, u, and exponent, n. li u or u"^ evenly divides the original 
expression (here represented as factors), then we have an answer. But we need to 
check the exponent, because / u'^du is u'^~^'^/{n -h 1) forn -1, but it is log(u) for 
. = -1, But there is a third case to consider. The factor may be something like ( 
(sin X 2)) 1), in which case we should consider /(..) = sin(x^). This case is 
handled with the help of an integral table. We don't need a derivative table, because 

we can just use the simplifier for that. 

(defun deriv (y x) (simplify *(d ,y .x))) 

(defun integration-table (rules) 
(dolist (i-rule rules) 
(let ((rule (infix->prefix i-rule))) 
(setf (get (exp-op (exp-lhs (exp-lhs rule))) 'int) 
rule)))) 

<a id='page-258'></a>

(defun in-integral-table? (exp) 
(and (exp-p exp) (get (exp-op exp) 'int))) 

(defun integrate-from-table (op arg) 
(let ((rule (get op 'int))) 
(subst arg (exp-lhs (exp-lhs (exp-lhs rule))) (exp-rhs rule)))) 

(integration-table 

'((Int log(x) d . = . * log(x) - .) 
(Int exp(x) d . = exp(x)) 
(Int sin(x) d X = - cos(x)) 
(Int cos(x) d X = sin(x)) 
(Int tan(x) d . = - log(cos(x))) 
(Int sinh(x) d . = cosh(x)) 
(Int cosh(x) d X = sinh(x)) 
(Int tanh(x) d . = log(cosh(x))) 
)) 

The last step is to install integrate as the simplification function for the operator 
Int. The obvious way to do this is: 

(set-simp-fn 'Int 'integrate) 

Unfortunately, that does not quite work. The problem is that integrate expects 
two arguments, corresponding to the two arguments y and xin ilnt y x). But the 
convention for simplification functions is to pass them a single argument, consisting 
of the whole expression (Int y jc). We could go back and edit simpl Ify-exp to 
change the convention, but instead I choose to make the conversion this way: 

(set-simp-fn 'Int #'(lambda (exp) 
(integrate (exp-lhs exp) (exp-rhs exp)))) 

Here are some examples, taken from chapters 8 and 9 of Calculus (Loomis 1974): 

SIMPLIFIER> (Int . * sin(x 2) d x) 
(1/2 * (- (COS (X ^ 2)))) 
SIMPLIFIER> (Int ((3 * . ^ 3) - 1 / (3 * . ^ 3)) d x) 
((3 * ((X ^ 4) / 4)) - (1/3 * ((X ^ -2) / -2))) 
SIMPLIFIER> (Int (3 * . + 2) ^ -2/3 d x) 
(((3 * X) + 2) 1/3) 
SIMPLIFIER> (Int sin(x) ^ 2 * cos(x) d x) 
(((SIN X) ^ 3) / 3) 
SIMPLIFIER> (Int sin(x) / (1 + cos(x)) d x) 
(-1 * (LOG ((COS X) + 1))) 
SIMPLIFIER> (Int (2 * . + 1) / (x ^ 2 + . - 1) d x) 

<a id='page-259'></a>

(LOG ((X ^ 2) + (X - 1))) 
SIMPLIFIER> (Int 8 * . ^ 2 / (x ^ 3 + 2) ^ 3 d x) 
(8 * ((1/3 * (((X ^ 3) + 2) -2)) / -2)) 

All the answers are correct, although the last one could be made simpler. One quick 
way to simplify such an expression is to factor and unfactor it, and then simplify 
again: 

(set-simp-fn 'Int 
#'(lambda (exp) 
(unfactorize 
(factorize 

(integrate (exp-lhs exp) (exp-rhs exp)))))) 

With this change, we get: 
SIMPLIFIER> (Int 8 * . ^ 2 / (x 3 + 2) ^ 3 d x) 
(-4/3 * (((X ^ 3) + 2) ^ -2)) 

8.7 History and References 
A brief history is given in the introduction to this chapter. An interesting point is that 
the history of Lisp and of symbolic algebraic manipulation are deeply intertwined. 
It is not too gross an exaggeration to say that Lisp was invented by John McCarthy 
to express the symbolic differentiation algorithm. And the development of the first 
high-quality Lisp system, MacLisp, was driven largely by the needs of MACSYMA, 
one of the first large Lisp systems. See McCarthy 1958 for early Lisp history and 
the differentiation algorithm, and Martin and Fateman 1971 and Moses (1975) for 
more details on MACSYMA. A comprehensive book on computer algebra systems 
is Davenport 1988. It covers the MACSYMA and REDUCE systems as well as the 
algorithms behind those systems. 

Because symbolic differentiation is historically important, it is presented in a 
number of text books, from the original Lisp 1.5 Primer (Weissman 1967) and Allen's 
influential Anatomy of Lisp (1978) to recent texts like Brooks 1985, Hennessey 1989, 
and Tanimoto 1990. Many of these books use rules or data-driven programming, 
but each treats differentiation as the main task, with simplification as a separate 
problem. None of them use the approach taken here, where differentiation is just 
another kind of simplification. 

The symbolic integration programs SAINT and SiN are covered in Slagle 1963 and 
Moses 1967, respectively. The mathematical solution to the problem of integration 

<a id='page-260'></a>

in closed term is addressed in Risch 1969, but be warned; this paper is not for the 
mathematically naive, and it has no hints on programming the algorithm. A better 
reference is Davenport et al. 1988. 
In this book, techniques for improving the efficiency of algebraic manipulation 
are covered in sections 9.6 and 10.4. Chapter 15 presents a reimplementation that 
does not use pattern-matching, and is closer to the techniques used in MACSYMA. 
8.8 Exercises 
&#9635; Exercise 8.2 [s] Some notations use the operator ** instead of " to indicate exponentiation. 
Fix i nf ix->pref i . so that either notation is allowed. 

&#9635; Exercise 8.3 [m] Can the system as is deal with imaginary numbers? What are 
some of the difficulties? 
&#9635; Exercise 8.4 [h] There are some simple expressions involving sums that are not 
handled by the i ntegrate function. The function can integrate axx^-\-bxx-\-c 
but not 5 X {a X x^ b X X + c) . Similarly, it can integrate x^ + 2x x^ -\- x^ but not 
(.2 + x)^^ and it can do + + . -h 1 but not (x^ -f 1) . (x + 1). Modify i ntegrate 
so that it expands out products (or small exponents) of sums. You will probably want 
to try the usual techniques first, and do the expansion only when that fails. 

&#9635; Exercise 8.5 [d] Another very general integration technique is called integration 
by parts. It is based on the rule: &int; udv = uv - &int; vdu 
So, for example, given 
X cos xdx/
we can take u = x,dv = cos xdx. Then we can determine . = sin . by integration, 
and come up with the solution: 
JX cos xdx = X sin ^ ~J ^ ^ ~ ^ ^^" ^ 

It is easy to program an integration by parts routine. The hard part is to program 
the control component. Integration by parts involves a recursive call to i ntegrate, 
and of all the possible ways of breaking up the original expression into a u and a dv, 

<a id='page-261'></a>

few, if any, will lead to a successful integration. One simple control rule is to allow 
integration by parts only at the top level, not at the recursive level. Implement this 
approach. 

&#9635; Exercise 8.6 [d] A more complicated approach is to try to decide which ways of 
breaking up the original expression are promising and which are not. Derive some 
heuristics for making this division, and reimplement i ntegrate to include a search 
component, using the search tools of chapter 6. 

Look in a calculus textbook to see how / sin^ xdx is evaluated by two integrations 
by parts and a division. Implement this technique as well. 

&#9635; Exercise 8.7 [m] Write simplification rules for predicate calculus expressions. For 
example, 

(true and . = x) 
(false and . = false) 
(true or X = true) 
(false or X = false) 

&#9635; Exercise 8.8 [m] The simplification rule (. / 0 = undef i ned) is necessary to avoid 
problems with division by zero, but the treatment of undef i ned is inadequate. For 
example, the expression ((0/0) - (0/0)) will simplify to zero, when it should 
simplify to undef i ned. Add rules to propagate undef i ned values and prevent them 
from being simplified away. 

&#9635; Exercise 8.9 [d] Extend the method used to handle undef i ned to handle +i nf i ni ty 
and -i nf ini ty as well. 

