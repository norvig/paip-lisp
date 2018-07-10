# Chapter 8
## Symbolic Mathematics: A Simplification Program

> *Our life is frittered away by detail….*

> *Simplify, simplify.*

> —Henry David Thoreau, *Walden* (1854)

“Symbolic mathematics” is to numerical mathematics as algebra is to arithmetic: it deals with variables and expressions rather than just numbers.
Computers were first developed primarily to solve arithmetic problems: to add up large columns of numbers, to multiply many-digit numbers, to solve systems of linear equations, and to calculate the trajectories of ballistics.
Encouraged by success in these areas, people hoped that computers could also be used on more complex problems; to differentiate or integrate a mathematical expression and come up with another expression as the answer, rather than just a number.
Several programs were developed along these lines in the 1960s and 1970s.
They were used primarily by professional mathematicians and physicists with access to large mainframe computers.
Recently, programs like MATHLAB !!!(span) {:.smallcaps} , DERIVE !!!(span) {:.smallcaps} , and MATHEMATICA !!!(span) {:.smallcaps} have given these capabilities to the average personal computer user.

It is interesting to look at some of the history of symbolic algebra, beginning in 1963 with SAINT !!!(span) {:.smallcaps} , James Slagle's program to do symbolic integration.
Originally, SAINT !!!(span) {:.smallcaps} was heralded as a triumph of AI.
It used general problem-solving techniques, similar in kind to GPS !!!(span) {:.smallcaps} , to search for solutions to difficult problems.
The program worked its way through an integration problem by choosing among the techniques known to it and backing up when an approach failed to pan out.
SAINT'S !!!(span) {:.smallcaps} behavior on such problems was originally similar to (and eventually much better than) the performance of undergraduate calculus students.

Over time, the AI component of symbolic integration began to disappear.
Joel Moses implemented a successor to SAINT !!!(span) {:.smallcaps} called SIN !!!(span) {:.smallcaps} .
It used many of the same techniques, but instead of relying on search to find the right combination of techniques, it had additional mathematical knowledge that led it to pick the right technique at each step, without any provision for backing up and trying an alternative.
SIN !!!(span) {:.smallcaps} solved more problems and was much faster than SAINT !!!(span) {:.smallcaps} , although it was not perfect: it still occasionally made the wrong choice and failed to solve a problem it could have.

By 1970, the mathematician R.
Risch and others developed algorithms for indefinite integration of any expression involving algebraic, logarithmic, or exponential extensions of rational functions.
In other words, given a “normal” function, the Risch algorithm will return either the indefinite integral of the function or an indication that no closed-form integral is possible in terms of elementary functions.
Such work effectively ended the era of considering integration as a problem in search.

SIN !!!(span) {:.smallcaps} was further refined, merged with parts of the Risch algorithm, and put into the evolving MACSYMA !!!(span) {:.smallcaps} [1](#fn0010){:#xfn0010} program.
For the most part, refinement of MACSYMA !!!(span) {:.smallcaps} consisted of the incorporation of new algorithms.
Few heuristics of any sort survive.
Today MACSYMA !!!(span) {:.smallcaps} is no longer considered an AI program.
It is used daily by scientists and mathematicians, while ELIZA !!!(span) {:.smallcaps} and STUDENT !!!(span) {:.smallcaps} are now but historical footnotes.

With ELIZA !!!(span) {:.smallcaps} and STUDENT !!!(span) {:.smallcaps} we were able to develop miniature programs that duplicated most of the features of the original.
We won't even try to develop a program worthy of the name MACSYMA !!!(span) {:.smallcaps} ; instead we will settle for a modest program to do symbolic simplification, which we will call (simply) `simplifier`.
Then, we will extend `simplifier` to do differentiation, and some integration problems.
The idea is that given an expression like (2 − 1)*x* + 0, we want the program to compute the simplified form *x*.

According to the *Mathematics Dictionary* (James and James 1949), the word “simplified” is “probably the most indefinite term used seriously in mathematics.” The problem is that “simplified” is relative to what you want to use the expression for next.
Which is simpler, *x*2 + 3*x* + 2 or (*x* + 1)(*x* + 2)?
The first makes it easier to integrate or differentiate, the second easier to find roots.
We will be content to limit ourselves to “obvious” simplifications.
For example, *x* is almost always preferable to 1*x* + 0.

## [ ](#){:#st0010}8.1 Converting Infix to Prefix Notation
{:#s0010}
{:.h1hd}

We will represent simplifications as a list of rules, much like the rules for STUDENT !!!(span) {:.smallcaps} and ELIZA !!!(span) {:.smallcaps} .
But since each simplification rule is an algebraic equation, we will store each one as an exp rather than as a `rule`.
To make things more legible, we will write each expression in infix form, but store them in the prefix form expected by `exp`.
This requires an `infix->prefix` function to convert infix expressions into prefix notation.
We have a choice as to how general we want our infix notation to be.
Consider:

[ ](#){:#l0010}`(((a * (x ^ 2)) + (b * x)) + c)`
!!!(p) {:.unnumlist}

`(a * x ^ 2 + b * x + c)`
!!!(p) {:.unnumlist}

`(a x ^ 2 + b x + c)`
!!!(p) {:.unnumlist}

`a x^2 + b*x+c`
!!!(p) {:.unnumlist}

The first is fully parenthesized infix, the second makes use of operator precedence (multiplication binds tighter than addition and is thus performed first), and the third makes use of implicit multiplication as well as operator precedence.
The fourth requires a lexical analyzer to break Lisp symbols into pieces.

Suppose we only wanted to handle the fully parenthesized case.
To write `infix->prefix`, one might first look at `prefix->infix` (on [page 228](B9780080571157500078.xhtml#p228)) trying to adapt it to our new purposes.
In doing so, the careful reader might discover a surprise: `infix->prefix` and `prefix->infix` are in fact the exact same function!
Both leave atoms unchanged, and both transform three-element lists by swapping the `exp-op` and `exp- 1hs`.
Both apply themselves recursively to the (possibly rearranged) input list.
Once we discover this fact, it would be tempting to avoid writing `infix->prefix`, and just call `prefix->infix` instead.
Avoid this temptation at all costs.
Instead, define `infix->prefix` as shown below.
The intent of your code will be clearer:

[ ](#){:#l0015}`(defun infix->prefix (infix-exp)`
!!!(p) {:.unnumlist}

` "Convert fully parenthesized infix-exp to a prefix expression"`
!!!(p) {:.unnumlist}

` ;; Don't use this version for non-fully parenthesized exps!`
!!!(p) {:.unnumlist}

 `(prefix->infix infix-exp))`
!!!(p) {:.unnumlist}

As we saw above, fully parenthesized infix can be quite ugly, with all those extra parentheses, so instead we will use operator precedence.
There are a number of ways of doing this, but the easiest way for us to proceed is to use our previously defined tool `rule-based-translator` and its subtool, `pat-match.` Note that the third clause of `infix->prefix`, the one that calls `rule-based-translator` is unusual in that it consists of a single expression.
Most cond-clauses have two expressions: a test and a result, but ones like this mean, "Evaluate the test, and if it is non-nil, return it.
Otherwise go on to the next clause."

[ ](#){:#l0020}`(defun infix->prefix (exp)`
!!!(p) {:.unnumlist}

` "Translate an infix expression into prefix notation."`
!!!(p) {:.unnumlist}

` ;; Note we cannot do implicit multiplication in this system`
!!!(p) {:.unnumlist}

` (cond ((atom exp) exp)`
!!!(p) {:.unnumlist}

`   ((= (length exp) 1) (infix->prefix (first exp)))`
!!!(p) {:.unnumlist}

`   ((rule-based-translator exp *infix->prefix-rules*`
!!!(p) {:.unnumlist}

`       :rule-if #'rule-pattern :rule-then #'rule-response`
!!!(p) {:.unnumlist}

`       :action`
!!!(p) {:.unnumlist}

`       #'(lambda (bindings response)`
!!!(p) {:.unnumlist}

`   (sublis (mapcar`
!!!(p) {:.unnumlist}

`     #'(lambda (pair)`
!!!(p) {:.unnumlist}

`      (cons (first pair)`
!!!(p) {:.unnumlist}

`       (infix->prefix (rest pair))))`
!!!(p) {:.unnumlist}

`     bindings)`
!!!(p) {:.unnumlist}

`     response))))`
!!!(p) {:.unnumlist}

`   ((symbolp (first exp))`
!!!(p) {:.unnumlist}

`   (list (first exp) (infix->prefix (rest exp))))`
!!!(p) {:.unnumlist}

`   (t (error "Illegal exp"))))`
!!!(p) {:.unnumlist}

Because we are doing mathematics in this chapter, we adopt the mathematical convention of using certain one-letter variables, and redefine `variable-p` so that variables are only the symbols `m` through `z`.

[ ](#){:#l0025}`(defun variable-p (exp)`
!!!(p) {:.unnumlist}

` "Variables are the symbols M through Z."`
!!!(p) {:.unnumlist}

` ;; put x,y,z first to find them a little faster`
!!!(p) {:.unnumlist}

` (member exp '(x y z m n o p q r s t u v w)))`
!!!(p) {:.unnumlist}

`(pat-match-abbrev 'x + '(?+ x))`
!!!(p) {:.unnumlist}

`(pat-match-abbrev 'y+ '(?+ y))`
!!!(p) {:.unnumlist}

`(defun rule-pattern (rule) (first rule))`
!!!(p) {:.unnumlist}

`(defun rule-response (rule) (second rule))`
!!!(p) {:.unnumlist}

`(defparameter *infix->prefix-rules*`
!!!(p) {:.unnumlist}

` (mapcar #'expand-pat-match-abbrev`
!!!(p) {:.unnumlist}

` '(((x+ = y+) (= x y))`
!!!(p) {:.unnumlist}

`  ((− x+) (− x))`
!!!(p) {:.unnumlist}

`  ((+ x+)  (+ x))`
!!!(p) {:.unnumlist}

`  ((x+ + y+) (+ x y))`
!!!(p) {:.unnumlist}

`  ((x+ − y+) (− x y))`
!!!(p) {:.unnumlist}

`  ((x+ * y+) (* x y))`
!!!(p) {:.unnumlist}

`  ((x+ / y+) (/ x y))`
!!!(p) {:.unnumlist}

`  ((x+ ^ y+) (^ x y))))`
!!!(p) {:.unnumlist}

` "A list of rules, ordered by precedence.")`
!!!(p) {:.unnumlist}

## [ ](#){:#st0015}8.2 Simplification Rules
{:#s0015}
{:.h1hd}

Now we are ready to define the simplification rules.
We use the definition of the data types rule and exp ([page 221](B9780080571157500078.xhtml#p221)) and `prefix->infix` ([page 228](B9780080571157500078.xhtml#p228)) from STUDENT !!!(span) {:.smallcaps} `.` They are repeated here:

[ ](#){:#l0030}`(defstruct (rule (:type list)) pattern response)`
!!!(p) {:.unnumlist}

`(defstruct (exp (:type list)`
!!!(p) {:.unnumlist}

`     (:constructor mkexp (lhs op rhs)))`
!!!(p) {:.unnumlist}

` op lhs rhs)`
!!!(p) {:.unnumlist}

`(defun exp-p (x) (consp x))`
!!!(p) {:.unnumlist}

`(defun exp-args (x) (rest x))`
!!!(p) {:.unnumlist}

`(defun prefix->infix (exp)`
!!!(p) {:.unnumlist}

` "Translate prefix to infix expressions."`
!!!(p) {:.unnumlist}

` (if (atom exp) exp`
!!!(p) {:.unnumlist}

`   (mapcar #'prefix->infix`
!!!(p) {:.unnumlist}

`   (if (binary-exp-p exp)`
!!!(p) {:.unnumlist}

`      (list (exp-lhs exp) (exp-op exp) (exp-rhs exp))`
!!!(p) {:.unnumlist}

`      exp))))`
!!!(p) {:.unnumlist}

`(defun binary-exp-p (x)`
!!!(p) {:.unnumlist}

` (and (exp-p x) (= (length (exp-args x)) 2)))`
!!!(p) {:.unnumlist}

We also use `rule-based-translator` ([page 188](B9780080571157500066.xhtml#p188)) once again, this time on a list of simplification rules.
A reasonable list of simplification rules is shown below.
This list covers the four arithmetic operators, addition, subtraction, multiplication, and division, as well as exponentiation (raising to a power), denoted by the symbol “^”

Again, it is important to note that the rules are ordered, and that later rules will be applied only when earlier rules do not match.
So, for example, 0 / 0 simplifies to `undefined,` and not to 1 or 0, because the rule for 0 / 0 cornes before the other rules.
See [exercise 8.8](#st0045) for a more complete treatment of this.

[ ](#){:#l0035}`(defparameter *simplification-rules* (mapcar #'infix->prefix '(`
!!!(p) {:.unnumlist}

` (x + 0 = x)`
!!!(p) {:.unnumlist}

` (0 + x = x)`
!!!(p) {:.unnumlist}

` (x + x = 2 * x)`
!!!(p) {:.unnumlist}

` (x − 0 = x)`
!!!(p) {:.unnumlist}

` (0 − x = - x)`
!!!(p) {:.unnumlist}

` (x − x = 0)`
!!!(p) {:.unnumlist}

` (− − x = x)`
!!!(p) {:.unnumlist}

` (x * 1 = x)`
!!!(p) {:.unnumlist}

` (x * x = x)`
!!!(p) {:.unnumlist}

` (x * 0 = 0)`
!!!(p) {:.unnumlist}

` (x * x = x)`
!!!(p) {:.unnumlist}

` (x * x = x ^ 2)`
!!!(p) {:.unnumlist}

` (x / 0 = undefined)`
!!!(p) {:.unnumlist}

` (0 / x = 0)`
!!!(p) {:.unnumlist}

` (x / 1 = x)`
!!!(p) {:.unnumlist}

` (x / x = 1)`
!!!(p) {:.unnumlist}

` (0 ^ 0 = undefined)`
!!!(p) {:.unnumlist}

` (x ^ 0 = 1)`
!!!(p) {:.unnumlist}

` (0 ^ x = 0)`
!!!(p) {:.unnumlist}

` (1 ^ x = 1)`
!!!(p) {:.unnumlist}

` (x ^ 1 = x)`
!!!(p) {:.unnumlist}

` (x ^ − 1 = 1 / x)`
!!!(p) {:.unnumlist}

` (x *(y / x) = y)`
!!!(p) {:.unnumlist}

` ((y / x)* x = y)`
!!!(p) {:.unnumlist}

` ((y * x) / x = y)`
!!!(p) {:.unnumlist}

` ((x * y) / x = y)`
!!!(p) {:.unnumlist}

` (x + − x = 0)`
!!!(p) {:.unnumlist}

` ((- x) + x = 0)`
!!!(p) {:.unnumlist}

` (x + y − x = y)`
!!!(p) {:.unnumlist}

` )))`
!!!(p) {:.unnumlist}

`(defun ^ (x y) "Exponentiation" (expt x y))`
!!!(p) {:.unnumlist}

We are now ready to go ahead and write the simplifier.
The main function, `simplifier` will repeatedly print a prompt, read an input, and print it in simplified form.
Input and output is in infix and the computation is in prefix, so we need to convert accordingly; the function simp does this, and the function `simplify` takes care of a single prefix expression.
It is summarized in [figure 8.1](#f0010).

![f08-01-9780080571157](images/B978008057115750008X/f08-01-9780080571157.jpg)     
Figure 8.1
!!!(span) {:.fignum}
Glossary for the Simplifier
Here is the program:

[ ](#){:#l0040}`(defun simplifier ()`
!!!(p) {:.unnumlist}

` "Read a mathematical expression, simplify it, and print the result."`
!!!(p) {:.unnumlist}

` (loop`
!!!(p) {:.unnumlist}

` (print 'simplifier >)`
!!!(p) {:.unnumlist}

` (print (simp (read)))))`
!!!(p) {:.unnumlist}

`(defun simp (inf) (prefix->infix (simplify (infix->prefix inf))))`
!!!(p) {:.unnumlist}

`(defun simplify (exp)`
!!!(p) {:.unnumlist}

` "Simplify an expression by first simplifying its components."`
!!!(p) {:.unnumlist}

` (if (atom exp) exp`
!!!(p) {:.unnumlist}

`   (simplify-exp (mapcar #'simplify exp))))`
!!!(p) {:.unnumlist}

`(defun simplify-exp (exp)`
!!!(p) {:.unnumlist}

` "Simplify using a rule, or by doing arithmetic."`
!!!(p) {:.unnumlist}

` (cond ((rule-based-translator exp *simplification-rules*`
!!!(p) {:.unnumlist}

`       :rule-if #'exp-lhs :rule-then #'exp-rhs`
!!!(p) {:.unnumlist}

`       :action #'(lambda (bindings response)`
!!!(p) {:.unnumlist}

`       (simplify (subiis bindings response)))))`
!!!(p) {:.unnumlist}

`   ((evaluable exp) (eval exp))`
!!!(p) {:.unnumlist}

`   (t exp)))`
!!!(p) {:.unnumlist}

`   (defun evaluable (exp)`
!!!(p) {:.unnumlist}

`       "Is this an arithmetic expression that can be evaluated?"`
!!!(p) {:.unnumlist}

`       (and (every #'numberp (exp-args exp))`
!!!(p) {:.unnumlist}

`     (or (member (exp-op exp) '(+ − */))`
!!!(p) {:.unnumlist}

`      (and (eq (exp-op exp) ’^`
!!!(p) {:.unnumlist}

`     (integerp (second (exp-args exp)))))))`
!!!(p) {:.unnumlist}

The function `simplify` assures that any compound expression will be simplified by first simplifying the arguments and then calling `simplify-exp.` This latter function searches through the simplification rules, much like `use-eliza-rules` and `translate-to-expression`.
When it finds a match, `simplify-exp` substitutes in the proper variable values and calls `simplify` on the result, `simplify-exp` also has the ability to call `eval` to simplify an arithmetic expression to a number.
As in STUDENT !!!(span) {:.smallcaps} , it is for the sake of this eval that we require expressions to be represented as lists in prefix notation.
Numeric evaluation is done *after* checking the rules so that the rules can intercept expressions like (/ 1 0) and simplify them to `undefined`.
If we did the numeric evaluation first, these expressions would yield an error when passed to eval.
Because Common Lisp supports arbitrary precision rational numbers (fractions), we are guaranteed there will be no round-off error, unless the input explicitly includes inexact (floating-point) numbers.
Notice that we allow computations involving the four arithmetic operators, but exponentiation is only allowed if the exponent is an integer.
That is because expressions like (^ 4 1/2) are not guaranteed to return 2 (the exact square root of 4); the answer might be 2.0 (an inexact number).
Another problem is that − 2 is also a square root of 4, and in some contexts it is the correct one to use.

The following trace shows some examples of the simplifier in action.
First we show that it can be used as a calculator; then we show more advanced problems.

[ ](#){:#l0045}`>(simplifier)`
!!!(p) {:.unnumlist}

`SIMPLIFIER > (2 + 2)`
!!!(p) {:.unnumlist}

`4`
!!!(p) {:.unnumlist}

`SIMPLIFIER > (5 * 20 + 30 + 7)`
!!!(p) {:.unnumlist}

`137`
!!!(p) {:.unnumlist}

`SIMPLIFIER > (5 * x - (4 + 1) * x)`
!!!(p) {:.unnumlist}

`0`
!!!(p) {:.unnumlist}

`SIMPLIFIER > (y / z * (5 * x - (4 + 1) * x))`
!!!(p) {:.unnumlist}

`0`
!!!(p) {:.unnumlist}

`SIMPLIFIER > ((4-3) * x + (y / y - 1) * z)`
!!!(p) {:.unnumlist}

`X`
!!!(p) {:.unnumlist}

`SIMPLIFIER > (1 * f(x) + 0)`
!!!(p) {:.unnumlist}

`(F X)`
!!!(p) {:.unnumlist}

`SIMPLIFIER > (3 * 2 * X)`
!!!(p) {:.unnumlist}

`(3 * (2 * X))`
!!!(p) {:.unnumlist}

`SIMPLIFIER > [Abort]`
!!!(p) {:.unnumlist}

`>`
!!!(p) {:.unnumlist}

Here we have terminated the loop by hitting the abort key on the terminal.
(The details of this mechanism varies from one implementation of Common Lisp to another.) The simplifier seems to work fairly well, although it errs on the last example: `(3 * (2 * X ) )` should simplify to `( 6 * X )`.
In the next section, we will correct that problem.

## [ ](#){:#st0020}8.3 Associativity and Commutativity
{:#s0020}
{:.h1hd}

We could easily add a rule to rewrite `(3 * (2 *X))` as `((3 * 2) * X)` andhence `(6 * X)`.
The problem is that this rule would also rewrite `(X*(2*3))` as `((X* 2) * 3)`, unless we had a way to limit the rule to apply only when it would group numbers together.
Fortunately, `pat-match` does provide just this capability, with the `?is` pattern.
We could write this rule:

[ ](#){:#l0050}`(((?is n numberp) * ((?is m numberp) * x)) = ((n * m) * x))`
!!!(p) {:.unnumlist}

This transforms `(3 * (2 * x))` into `((3 * 2) * x)`, and hence into `(6 * x)`.
Unfortunately, the problem is not as simple as that.
We also want to simplify `((2 * x) * (y * 3))` to `(6 *(x * y))`.
We can do a better job of gathering numbers together by adopting three conventions.
First, make numbers first in products: change `x * 3` to `3 * x`.
Second, combine numbers in an outer expression with a number in an inner expression: change `3 *(5 * x)` to `(3 * 5)* x`.
Third, move numbers out of inner expressions whenever possible: change `(3 * x) *y` to `3 *(x * y)`.
We adopt similar conventions for addition, except that we prefer numbers last there: `x + 1` instead of `l + x`.

[ ](#){:#l0055}`;; Define n and m as numbers; s as a non-number:`
!!!(p) {:.unnumlist}

`(pat-match-abbrev 'n '(?is n numberp))`
!!!(p) {:.unnumlist}

`(pat-match-abbrev 'm '(?is m numberp))`
!!!(p) {:.unnumlist}

`(pat-match-abbrev 's '(?is s not-numberp))`
!!!(p) {:.unnumlist}

`(defun not-numberp (x) (not (numberp x)))`
!!!(p) {:.unnumlist}

`(defun simp-rule (rule)`
!!!(p) {:.unnumlist}

` "Transform a rule into proper format."`
!!!(p) {:.unnumlist}

` (let ((exp (infix->prefix rule)))`
!!!(p) {:.unnumlist}

` (mkexp (expand-pat-match-abbrev (exp-lhs exp))`
!!!(p) {:.unnumlist}

`       (exp-op exp) (exp-rhs exp))))`
!!!(p) {:.unnumlist}

`(setf *simplification-rules*`
!!!(p) {:.unnumlist}

` (append *simplification-rules* (mapcar #'simp-rule`
!!!(p) {:.unnumlist}

` '((s * n = n * s)`
!!!(p) {:.unnumlist}

` (n * (m * x) = (n * m) * x)`
!!!(p) {:.unnumlist}

` (x * (n * y) = n * (x * y))`
!!!(p) {:.unnumlist}

` ((n * x) * y = n * (x * y))`
!!!(p) {:.unnumlist}

` (n + s = s + n)`
!!!(p) {:.unnumlist}

` ((x + m) + n = x + n + m)`
!!!(p) {:.unnumlist}

` (x + (y + n) = (x + y) + n)`
!!!(p) {:.unnumlist}

` ((x + n) + y = (x + y) + n)))))`
!!!(p) {:.unnumlist}

With the new rules in place, we are ready to try again.
For some problems we get just the right answers:

[ ](#){:#l0060}`> (simplifier)`
!!!(p) {:.unnumlist}

`SIMPLIFIER > (3 * 2 * x)`
!!!(p) {:.unnumlist}

`(6 * X)`
!!!(p) {:.unnumlist}

`SIMPLIFIER > (2 * x * x * 3)`
!!!(p) {:.unnumlist}

`(6 * (X ^ 2))`
!!!(p) {:.unnumlist}

`SIMPLIFIER > (2 * x * 3 * y * 4 * z * 5 * 6)`
!!!(p) {:.unnumlist}

`(720 * (X * (Y * Z)))`
!!!(p) {:.unnumlist}

`SIMPLIFIER > (3 + x + 4 + x)`
!!!(p) {:.unnumlist}

`((2 * X) + 7)`
!!!(p) {:.unnumlist}

`SIMPLIFIER > (2 * x * 3 * x * 4 * (l / x) * 5 * 6)`
!!!(p) {:.unnumlist}

`(720 * X)`
!!!(p) {:.unnumlist}

Unfortunately, there are other problems that aren't simplified properly:

[ ](#){:#l0065}`SIMPLIFIER > (3 + x + 4 - x)`
!!!(p) {:.unnumlist}

`((X + (4 - X)) + 3)`
!!!(p) {:.unnumlist}

`SIMPLIFIER > (x + y + y + x)`
!!!(p) {:.unnumlist}

`(X + (Y + (Y + X)))`
!!!(p) {:.unnumlist}

`SIMPLIFIER > (3 * x + 4 * x)`
!!!(p) {:.unnumlist}

`((3 * X) + (4 * X))`
!!!(p) {:.unnumlist}

We will return to these problems in [section 8.5](#s0030).

**Exercise 8.1** Verify that the set of rules just prior does indeed implement the desired conventions, and that the conventions have the proper effect, and always terminate.
As an example of a potential problem, what would happen if we used the rule `(x * n = n * x)` instead of the rule `(s * n = n * s)?`

## [ ](#){:#st0025}8.4 Logs, Trig, and Differentiation
{:#s0025}
{:.h1hd}

In the previous section, we restricted ourselves to the simple arithmetic functions, so as not to intimidate those who are a little leery of complex mathematics.
In this section, we add a little to the mathematical complexity, without having to alter the program itself one bit.
Thus, the mathematically shy can safely skip to the next section without feeling they are missing any of the fun.

We start off by representing some elementary properties of the logarithmic and trigonometric functions.
The new rules are similar to the "zero and one" rules we needed for the arithmetic operators, except here the constants e and `pi` (*e* = 2.71828… and *π* = 3.14159…) are important in addition to 0 and 1.
We also throw in some rules relating logs and exponents, and for sums and differences of logs.
The rules assume that complex numbers are not allowed.
If they were, log *ex* (and even *xy*) would have multiple values, and it would be wrong to arbitrarily choose one of these values.

[ ](#){:#l0070}`(setf *simplification-rules*`
!!!(p) {:.unnumlist}

` (append *simplification-rules* (mapcar #'simp-rule '(`
!!!(p) {:.unnumlist}

` (log 1          = 0)`
!!!(p) {:.unnumlist}

` (log 0          = undefined)`
!!!(p) {:.unnumlist}

` (log e          = 1)`
!!!(p) {:.unnumlist}

` (sin 0          = 0)`
!!!(p) {:.unnumlist}

` (sin pi         = 0)`
!!!(p) {:.unnumlist}

` (cos 0          = 1)`
!!!(p) {:.unnumlist}

` (cos pi         = −1)`
!!!(p) {:.unnumlist}

` (sin(pi / 2)    = 1)`
!!!(p) {:.unnumlist}

` (cos(pi / 2)    = 0)`
!!!(p) {:.unnumlist}

` (log (e ^ x)    = x)`
!!!(p) {:.unnumlist}

` (e ^ (log x)    = x)`
!!!(p) {:.unnumlist}

` ((x ^ y) * (x ^ z) = x ^ (y + z))`
!!!(p) {:.unnumlist}

` ((x ^ y) / (x ^ z) = x ^ (y - z))`
!!!(p) {:.unnumlist}

` (log x + log y = log(x * y))`
!!!(p) {:.unnumlist}

` (log x - log y = log(x / y))`
!!!(p) {:.unnumlist}

` ((sin x) ^ 2 + (cos x) ^ 2 = 1)`
!!!(p) {:.unnumlist}

` ))))`
!!!(p) {:.unnumlist}

Now we would like to go a step further and extend the system to handle differentiation.
This is a favorite problem, and one which has historical significance: in the summer of 1958 John McCarthy decided to investigate differentiation as an interesting symbolic computation problem, which was difficult to express in the primitive programming languages of the day.
This investigation led him to see the importance of functional arguments and recursive functions in the field of symbolic computation.
For example, McCarthy invented what we now call `mapcar` to express the idea that the derivative of a sum is the sum of the derivative function applied to each argument.
Further work led McCarthy to the publication in October 1958 of MIT AI Lab Memo No.
1: "An Algebraic Language for the Manipulation of Symbolic Expressions," which defined the precursor of Lisp.

In McCarthy's work and in many subsequent texts you can see symbolic differentiation programs with a simplification routine tacked on the end to make the output more readable.
Here, we take the opposite approach: the simplification routine is central, and differentiation is handled as just another operator, with its own set of simplification rules.
We will require a new infix-to-prefix translation rule.
While we're at it, we'll add a rule for indefinite integration as well, although we won't write simplification rules for integration yet.
Here are the new notations:

[ ](#){:#t0010}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| math | infix | prefix |
| *dy*/*dx* | `d y / d x` | `(d y x)` |
| *∫ ydx* | `Int y d x` | `(int y x)` |

And here are the necessary infix-to-prefix rules:

[ ](#){:#l0075}`(defparameter *infix->prefix-rules*`
!!!(p) {:.unnumlist}

`   (mapcar #'expand-pat-match-abbrev`
!!!(p) {:.unnumlist}

`     '(((x+ = y+) (= x y))`
!!!(p) {:.unnumlist}

`     ((− x+) (− x))`
!!!(p) {:.unnumlist}

`     ((+ x+) (+ x))`
!!!(p) {:.unnumlist}

`     ((x+ + y+) (+ x y))`
!!!(p) {:.unnumlist}

`     ((x+ − y+) (− x y))`
!!!(p) {:.unnumlist}

`     ((d y+ / d x) (d y x))    ;*** New rule`
!!!(p) {:.unnumlist}

`     ((Int y+ d x) (int y x))  ;*** New rule`
!!!(p) {:.unnumlist}

`     ((x+ * y+) (* x y))`
!!!(p) {:.unnumlist}

`     ((x+ / y+) (/ x y))`
!!!(p) {:.unnumlist}

`     ((x+ ^ y+) (^ x y)))))`
!!!(p) {:.unnumlist}

Since the new rule for differentiation occurs before the rule for division, there won't be any confusion with a differential being interpreted as a quotient.
On the other hand, there is a potential problem with integrals that contain `d` as a variable.
The user can always avoid the problem by using (`d`) instead of `d` inside an integral.

Now we augment the simplification rules, by copying a differentiation table out of a reference book:

[ ](#){:#l0080}`(setf *simplification-rules*`
!!!(p) {:.unnumlist}

` (append *simplification-rules* (mapcar #'simp-rule '(`
!!!(p) {:.unnumlist}

` (d x / d x  = 1)`
!!!(p) {:.unnumlist}

` (d (u + v) / d x = (d u / d x) + (d v / d x))`
!!!(p) {:.unnumlist}

` (d (u - v) / d x - (d u / d x) - (d v / d x))`
!!!(p) {:.unnumlist}

` (d (− u) / d x = - (d u / d x))`
!!!(p) {:.unnumlist}

` (d(u*v)/dx = u*(dv/dx) + v*(d u/d x))`
!!!(p) {:.unnumlist}

` (d (u / v) / d x = (v * (d u / d x) - u * (d v / d x))`
!!!(p) {:.unnumlist}

`      / v ^ 2)`
!!!(p) {:.unnumlist}

`(d (u ^ n) / d x = n * u ^ (n - 1) * (d u / d x))`
!!!(p) {:.unnumlist}

`(d (u ^ V) / d x = v * u ^ (v - 1) * (d u /d x)`
!!!(p) {:.unnumlist}

`     + u ^ v * (log u) * (d v / d x))`
!!!(p) {:.unnumlist}

`(d (log u) / d x = (d u / d x) / u)`
!!!(p) {:.unnumlist}

`(d (sin u) / d x = (cos u) * (d u / d x))`
!!!(p) {:.unnumlist}

`(d (cos u) / d x = - (sin u) * (d u / d x))`
!!!(p) {:.unnumlist}

`(d (e ^ u) / d x = (e ^ u) * (d u / d x))`
!!!(p) {:.unnumlist}

`(d u / d x  = 0)))))`
!!!(p) {:.unnumlist}

We have added a default rule, `(d u / d x = 0)`; this should only apply when the expression `u` is free of the variable `x` (that is, when u is not a function of `x`).
We could use `?if` to check this, but instead we rely on the fact that differentiation is closed over the list of operators described here–as long as we don't introduce any new operators, the answer will always be correct.
Note that there are two rules for exponentiation, one for the case when the exponent is a number, and one when it is not.
This was not strictly necessary, as the second rule covers both cases, but that was the way the rules were written in the table of differentials I consulted, so I left both rules in.

[ ](#){:#l0085}`SIMPLIFIER > (d (x + x) / d x)`
!!!(p) {:.unnumlist}

`2`
!!!(p) {:.unnumlist}

`SIMPLIFIER > (d (a * x ^ 2 + b * x + c) / d x)`
!!!(p) {:.unnumlist}

`((2 * (A * X)) + B)`
!!!(p) {:.unnumlist}

`SIMPLIFIER > (d ((a * x ^ 2 + b * x + c) / x) / d x)`
!!!(p) {:.unnumlist}

`((((A * (X ^ 2)) + ((B * X) + C)) - (X * ((2 * (A * X)) + B)))`
!!!(p) {:.unnumlist}

`/ (X ^ 2))`
!!!(p) {:.unnumlist}

`SIMPLIFIER > (log ((d (x + x) / d x) / 2))`
!!!(p) {:.unnumlist}

`0`
!!!(p) {:.unnumlist}

`SIMPLIFIER > (log(x + x) - log x)`
!!!(p) {:.unnumlist}

`(LOG 2)`
!!!(p) {:.unnumlist}

`SIMPLIFIER > (x ^ cos pi)`
!!!(p) {:.unnumlist}

`(1 / X)`
!!!(p) {:.unnumlist}

`SIMPLIFIER > (d (3 * x + (cos x) / x) / d x)`
!!!(p) {:.unnumlist}

`((((COS X) - (X * (− (SIN X)))) / (X ^ 2)) + 3)`
!!!(p) {:.unnumlist}

`SIMPLIFIER > (d ((cos x) / x) / d x)`
!!!(p) {:.unnumlist}

`(((COS X) - (X * (− (SIN X)))) / (X ^ 2))`
!!!(p) {:.unnumlist}

`SIMPLIFIER > (d (3 * x ^ 2 + 2 * x + 1) / d x)`
!!!(p) {:.unnumlist}

`((6 * X) + 2)`
!!!(p) {:.unnumlist}

`SIMPLIFIER > (sin(x + x) ^ 2 + cos(d x ^ 2 / d x) ^ 2)`
!!!(p) {:.unnumlist}

`1`
!!!(p) {:.unnumlist}

`SIMPLIFIER > (sin(x + x) * sin(d x ^ 2 / d x) +`
!!!(p) {:.unnumlist}

`   cos(2 * x) * cos(x * d 2 * y / d y))`
!!!(p) {:.unnumlist}

`1`
!!!(p) {:.unnumlist}

The program handles differentiation problems well and is seemingly clever in its use of the identity sin2*x* + cos2*x* = 1.

## [ ](#){:#st0030}8.5 Limits of Rule-Based Approaches
{:#s0030}
{:.h1hd}

In this section we return to some examples that pose problems for the simplifier.
Here is a simple one:

[ ](#){:#l0090}`SIMPLIFIER > (x + y + y + x)`⇒ `(X + (Y + (Y + X)))`
!!!(p) {:.unnumlist}

We would prefer `2 * (x + y)`.
The problem is that, although we went to great trouble to group numbers together, there was no effort to group non-numbers.
We could write rules of the form:

[ ](#){:#l0095}`(y + (y + x) = (2 * y) + x)`
!!!(p) {:.unnumlist}

`(y + (x + y) = (2 * y) + x)`
!!!(p) {:.unnumlist}

These would work for the example at hand, but they would not work for `(x + y + z + y + x)`.
For that we would need more rules:

[ ](#){:#l0100}`(y + (z + (y + x)) = (2 * y) + x + z)`
!!!(p) {:.unnumlist}

`(y + (z + (x + y)) = (2 * y) + x + z)`
!!!(p) {:.unnumlist}

`(y + ((y + x) + z) = (2 * y) + x + z)`
!!!(p) {:.unnumlist}

`(y + ((x + y) + z) = (2 * y) + x + z)`
!!!(p) {:.unnumlist}

To handle all the cases, we would need an infinite number of rules.
The pattern-matching language is not powerful enough to express this succintly.
It might help if nested sums (and products) were unnested; that is, if we allowed + to take an arbitrary number of arguments instead of just one.
Once the arguments are grouped together, we could sort them, so that, say, all the `ys` appear before `z` and after `x`.
Then like terms could be grouped together.
We have to be careful, though.
Consider these examples:

[ ](#){:#l0105}`SIMPLIFIER > (3 * x + 4 * x)`
!!!(p) {:.unnumlist}

`((3 * X) + (4 * X))`
!!!(p) {:.unnumlist}

`SIMPLIFIER > (3 * x + y + x + 4 * x)`
!!!(p) {:.unnumlist}

`((3 * X) + (Y + (X + (4 * X))))`
!!!(p) {:.unnumlist}

We would want `(3 * x)` to sort to the same place as `x` and `(4 * x )` so that they could all be combined to `(8 * x)`.
In [chapter 15](B9780080571157500157.xhtml), we develop a new version of the program that handles this problem.

## [ ](#){:#st0035}8.6 Integration
{:#s0035}
{:.h1hd}

So far, the algebraic manipulations have been straightforward.
There is a direct algorithm for Computing the derivative of every expression.
When we consider integrals, or antiderivatives,[2](#fn0015){:#xfn0015} the picture is much more complicated.
As you may recall from freshman calculus, there is a fine art to Computing integrals.
In this section, we try to see how far we can get by encoding just a few of the many tricks available to the calculus student.

The first step is to recognize that entries in the simplification table will not be enough.
Instead, we will need an algorithm to evaluate or "simplify" integrals.
We will add a new case to `simplify-exp` to check each operator to see if it has a simplification function associated with it.
These simplification functions will be associated with operators through the functions `set-simp-fn` and `simp-fn`.
If an operator does have a simplification function, then that function will be called instead of consulting the simplification rules.
The simplification function can elect not to handle the expression after all by returning nil, in which case we continue with the other simplification methods.

[ ](#){:#l0110}`(defun simp-fn (op) (get op 'simp-fn))`
!!!(p) {:.unnumlist}

`(defun set-simp-fn (op fn) (setf (get op 'simp-fn) fn))`
!!!(p) {:.unnumlist}

`(defun simplify-exp (exp)`
!!!(p) {:.unnumlist}

` "Simplify using a rule, or by doing arithmetic.`
!!!(p) {:.unnumlist}

` or by using the simp function supplied for this operator."`
!!!(p) {:.unnumlist}

` (cond ((simplify-by-fn exp)) ;***`
!!!(p) {:.unnumlist}

`   ((rule-based-translator exp *simplification-rules*`
!!!(p) {:.unnumlist}

`       :rule-if #'exp-lhs :rule-then #'exp-rhs`
!!!(p) {:.unnumlist}

`       :action #'(lambda (bindings response)`
!!!(p) {:.unnumlist}

`       (simplify (subiis bindings response)))))`
!!!(p) {:.unnumlist}

`  ((evaluable exp) (eval exp))`
!!!(p) {:.unnumlist}

`  (t exp)))`
!!!(p) {:.unnumlist}

`(defun simplify-by-fn (exp)`
!!!(p) {:.unnumlist}

` "If there is a simplification fn for this exp,`
!!!(p) {:.unnumlist}

` and if applying it gives a non-null result,`
!!!(p) {:.unnumlist}

` then simplify the result and return that."`
!!!(p) {:.unnumlist}

` (let* ((fn (simp-fn (exp-op exp)))`
!!!(p) {:.unnumlist}

`   (result (if fn (funcall fn exp))))`
!!!(p) {:.unnumlist}

` (if (null result)`
!!!(p) {:.unnumlist}

`   nil`
!!!(p) {:.unnumlist}

`   (simplify result))))`
!!!(p) {:.unnumlist}

Freshman calculus classes teach a variety of integration techniques.
Fortunately, one technique–the derivative-divides technique–can be adopted to solve most of the problems that come up at the freshman calculus level, perhaps 90% of the problems given on tests.
The basic rule is:

∫fxdx=∫fududxdx.

![si1_e](images/B978008057115750008X/si1_e.gif)

As an example, consider *∫ x* sin(*x*2) *dx*.
Using the substitution *u* = *x*2, we can differentiate to get *du*/*dx* = 2*x*.
Then by applying the basic rule, we get:

∫xsinx2dx=12∫sinududxdx=12∫sinudu.

![si2_e](images/B978008057115750008X/si2_e.gif)

Assume we have a table of integrals that includes the rule *∫* sin(*x*) *dx* = − cos(*x*).
Then we can get the final answer:

−12cosx2.

![si3_e](images/B978008057115750008X/si3_e.gif)

Abstracting from this example, the general algorithm for integrating an expression *y* with respect to *x* is:

[ ](#){:#l0115}1. Pick a factor of *y*, callingit *f*(*u*).
!!!(p) {:.numlist}

2. Compute the derivative *du*/*dx*.
!!!(p) {:.numlist}

3. Divide *y* by *f*(*u*) × *du*/*dx*, calling the quotient *k*.
!!!(p) {:.numlist}

4. If *k* is a constant (with respect to *x*), then the result is *k ∫ f*(*u*)*du*.
!!!(p) {:.numlist}

This algorithm is nondeterministic, as there may be many factors of *y*.
In our example, *f*(*u*) = sin(*x*2), *u* = *x*2, and *du*/*dx* = 2*x*.
So k=12 !!!(span) {:.hiddenClass} ![si4_e](images/B978008057115750008X/si4_e.gif), and the answer is −12cosx2 !!!(span) {:.hiddenClass} ![si5_e](images/B978008057115750008X/si5_e.gif).

The first step in implementing this technique is to make sure that division is done correctly.
We need to be able to pick out the factors of *y*, divide expressions, and then determine if a quotient is free of *x*.
The function `factorize` does this.
It keeps a list of factors and a running product of constant factors, and augments them with each call to the local function `fac`.

[ ](#){:#l0120}`(defun factorize (exp)`
!!!(p) {:.unnumlist}

` "Return a list of the factors of exp^n.`
!!!(p) {:.unnumlist}

` where each factor is of the form (^ y n)."`
!!!(p) {:.unnumlist}

` (let ((factors nil)`
!!!(p) {:.unnumlist}

`   (constant 1))`
!!!(p) {:.unnumlist}

` (labels`
!!!(p) {:.unnumlist}

`  ((fac (x n)`
!!!(p) {:.unnumlist}

`   (cond`
!!!(p) {:.unnumlist}

`       ((numberp x)`
!!!(p) {:.unnumlist}

`       (setf constant (* constant (expt x n))))`
!!!(p) {:.unnumlist}

`       ((starts-with x '*)`
!!!(p) {:.unnumlist}

`       (fac (exp-lhs x) n)`
!!!(p) {:.unnumlist}

`       (fac (exp-rhs x) n))`
!!!(p) {:.unnumlist}

`       ((starts-with x '/)`
!!!(p) {:.unnumlist}

`       (fac (exp-lhs x) n)`
!!!(p) {:.unnumlist}

`       (fac (exp-rhs x) (− n)))`
!!!(p) {:.unnumlist}

`       ((and (starts-with x '-) (length=l (exp-args x)))`
!!!(p) {:.unnumlist}

`       (setf constant (− constant))`
!!!(p) {:.unnumlist}

`       (fac (exp-lhs x) n))`
!!!(p) {:.unnumlist}

`       ((and (starts-with x ’^) (numberp (exp-rhs x)))`
!!!(p) {:.unnumlist}

`       (fac (exp-lhs x) (* n (exp-rhs x))))`
!!!(p) {:.unnumlist}

`       (t (let ((factor (find x factors :key #'exp-lhs`
!!!(p) {:.unnumlist}

`         :test #'equal)))`
!!!(p) {:.unnumlist}

`     (if factor`
!!!(p) {:.unnumlist}

`      (incf (exp-rhs factor) n)`
!!!(p) {:.unnumlist}

`      (push ‘(^ ,x ,n) factors)))))))`
!!!(p) {:.unnumlist}

`  ;; Body of factorize:`
!!!(p) {:.unnumlist}

`  (fac exp 1)`
!!!(p) {:.unnumlist}

`  (case constant`
!!!(p) {:.unnumlist}

`   (0 '((^ 0 1)))`
!!!(p) {:.unnumlist}

`   (1 factors)`
!!!(p) {:.unnumlist}

`   (t '((^ .constant 1) .,factors))))))`
!!!(p) {:.unnumlist}

`factorize` maps from an expression to a list of factors, but we also need `unfactorize` to turn a list back into an expression:

[ ](#){:#l0125}`(defun unfactorize (factors)`
!!!(p) {:.unnumlist}

` "Convert a list of factors back into prefix form."`
!!!(p) {:.unnumlist}

` (cond ((null factors) 1)`
!!!(p) {:.unnumlist}

`   ((length=l factors) (first factors))`
!!!(p) {:.unnumlist}

`   (t '(* .(first factors) .
(unfactorize (rest factors))))))`
!!!(p) {:.unnumlist}

The derivative-divides method requires a way of dividing two expressions.
We do this by factoring each expression and then dividing by cancelling factors.
There may be cases where, for example, two factors in the numerator could be multiplied together to cancel a factor in the denominator, but this possibility is not considered.
It turns out that most problems from freshman calculus do not require such sophistication.

[ ](#){:#l0130}`(defun divide-factors (numer denom)`
!!!(p) {:.unnumlist}

` "Divide a list of factors by another, producing a third."`
!!!(p) {:.unnumlist}

` (let ((result (mapcar #'copy-list numer)))`
!!!(p) {:.unnumlist}

` (dolist (d denom)`
!!!(p) {:.unnumlist}

`  (let ((factor (find (exp-lhs d) result :key #'exp-lhs`
!!!(p) {:.unnumlist}

`     :test #'equal)))`
!!!(p) {:.unnumlist}

`   (if factor`
!!!(p) {:.unnumlist}

`       (decf (exp-rhs factor) (exp-rhs d))`
!!!(p) {:.unnumlist}

`       (push ‘(^ ,(exp-lhs d) ,(- (exp-rhs d))) result))))`
!!!(p) {:.unnumlist}

` (delete 0 result :key #'exp-rhs)))`
!!!(p) {:.unnumlist}

Finally, the predicate `free-of` returns true if an expression does not have any occurrences of a particular variable in it.

[ ](#){:#l0135}`(defun free-of (exp var)`
!!!(p) {:.unnumlist}

` "True if expression has no occurrence of var."`
!!!(p) {:.unnumlist}

` (not (find-anywhere var exp)))`
!!!(p) {:.unnumlist}

`(defun find-anywhere (item tree)`
!!!(p) {:.unnumlist}

` "Does item occur anywhere in tree?
If so, return it."`
!!!(p) {:.unnumlist}

` (cond ((eql item tree) tree)`
!!!(p) {:.unnumlist}

`   ((atom tree) nil)`
!!!(p) {:.unnumlist}

`   ((find-anywhere item (first tree)))`
!!!(p) {:.unnumlist}

`   ((find-anywhere item (rest tree)))))`
!!!(p) {:.unnumlist}

In `factorize` we made use of the auxiliary function `length=1.` The function call `(length=l x)` is faster than `(= (length x) 1)` because the latter has to compute the length of the whole list, while the former merely has to see if the list has a `rest` element or not.

[ ](#){:#l0140}`(defun length=l (x)`
!!!(p) {:.unnumlist}

` "Is X a list of length 1?"`
!!!(p) {:.unnumlist}

` (and (consp x) (null (rest x))))`
!!!(p) {:.unnumlist}

Given these preliminaries, the function `integrate` is fairly easy.
We start with some simple cases for integrating sums and constant expressions.
Then, we factor the expression and split the list of factors into two: a list of constant factors, and a list of factors containing *x*.
(This is done with `partition-if`, a combination of `remove-if` and `remove-if-not`.) Finally, we call `deriv-divides`, giving it a chance with each of the factors.
If none of them work, we return an expression indicating that the integral is unknown.

[ ](#){:#l0145}`(defun integrate (exp x)`
!!!(p) {:.unnumlist}

`  ;; First try some trivial cases`
!!!(p) {:.unnumlist}

` (cond`
!!!(p) {:.unnumlist}

` ((free-of exp x) *(* ,exp x)) ; Int c dx = c*x`
!!!(p) {:.unnumlist}

` ((starts-with exp '+) ; Int f + g =`
!!!(p) {:.unnumlist}

` '(+ ,(integrate (exp-lhs exp) x) ; Int f + Int g`
!!!(p) {:.unnumlist}

`    ,(integrate (exp-rhs exp) x)))`
!!!(p) {:.unnumlist}

` ((starts-with exp '-)`
!!!(p) {:.unnumlist}

` (ecase (length (exp-args exp))`
!!!(p) {:.unnumlist}

`  (1 (integrate (exp-lhs exp) x)) ; Int - f = - Int f`
!!!(p) {:.unnumlist}

`  (2 '(- ,(integrate (exp-lhs exp) x) ; Int f - g =`
!!!(p) {:.unnumlist}

`    ,(integrate (exp-rhs exp) x))))) ; Int f - Int g`
!!!(p) {:.unnumlist}

` ;; Now move the constant factors to the left of the integral`
!!!(p) {:.unnumlist}

` ((multiple-value-bind (const-factors x-factors)`
!!!(p) {:.unnumlist}

`       (partition-if #'(lambda (factor) (free-of factor x))`
!!!(p) {:.unnumlist}

`     (factorize exp))`
!!!(p) {:.unnumlist}

`  (simplify`
!!!(p) {:.unnumlist}

`   '(* ,(unfactorize const-factors)`
!!!(p) {:.unnumlist}

`        ;; And try to integrate:`
!!!(p) {:.unnumlist}

`        ,(cond ((null x-factors) x)`
!!!(p) {:.unnumlist}

`     ((some #'(lambda (factor)`
!!!(p) {:.unnumlist}

`               (deriv-divides factor x-factors x))`
!!!(p) {:.unnumlist}

`            x-factors))`
!!!(p) {:.unnumlist}

`       ;; < other methods here >`
!!!(p) {:.unnumlist}

`       (t '(int?
,(unfactorize x-factors) ,x)))))))))`
!!!(p) {:.unnumlist}

`(defun partition-if (pred list)`
!!!(p) {:.unnumlist}

` "Return 2 values: elements of list that satisfy pred,`
!!!(p) {:.unnumlist}

` and elements that don't."`
!!!(p) {:.unnumlist}

` (let ((yes-1ist nil)`
!!!(p) {:.unnumlist}

`  (no-list nil))`
!!!(p) {:.unnumlist}

` (dolist (item list)`
!!!(p) {:.unnumlist}

`  (if (funcall pred item)`
!!!(p) {:.unnumlist}

`       (push item yes-list)`
!!!(p) {:.unnumlist}

`       (push item no-list)))`
!!!(p) {:.unnumlist}

` (values (nreverse yes-list) (nreverse no-list))))`
!!!(p) {:.unnumlist}

Note that the place in integrate where other techniques could be added is marked.
We will only implement the derivative-divides method.
It turns out that the function is a little more complicated than the simple four-step algorithm outlined before:

[ ](#){:#l0150}`(defun deriv-divides (factor factors x)`
!!!(p) {:.unnumlist}

` (assert (starts-with factor '^))`
!!!(p) {:.unnumlist}

` (let* ((u (exp-lhs factor)) ; factor = u^n`
!!!(p) {:.unnumlist}

` (n (exp-rhs factor))`
!!!(p) {:.unnumlist}

` (k (divide-factors`
!!!(p) {:.unnumlist}

`   factors (factorize '(* ,factor ,(deriv u x))))))`
!!!(p) {:.unnumlist}

` (cond ((free-of k x)`
!!!(p) {:.unnumlist}

`  ;; Int k*u^n*du/dx dx = k*Int u^n du`
!!!(p) {:.unnumlist}

`  ;;      = k*u^(n+1)/(n+1) for n /= -1`
!!!(p) {:.unnumlist}

`  ;;      = k*log(u) for n = -1`
!!!(p) {:.unnumlist}

`  (if (= n -1)`
!!!(p) {:.unnumlist}

`   '(* .(unfactorize k) (log ,u))`
!!!(p) {:.unnumlist}

`   '(/ (* ,(unfactorize k) (^ ,u ,(+ n 1)))`
!!!(p) {:.unnumlist}

`        ,(+ n 1))))`
!!!(p) {:.unnumlist}

`  ((and (= n 1) (in-integral-table?
u))`
!!!(p) {:.unnumlist}

`  ;; Int y'*f(y) dx = Int f(y) dy`
!!!(p) {:.unnumlist}

`  (let ((k2 (divide-factors`
!!!(p) {:.unnumlist}

`     factors`
!!!(p) {:.unnumlist}

`     (factorize '(* ,u ,(deriv (exp-lhs u) x))))))`
!!!(p) {:.unnumlist}

`   (if (free-of k2 x)`
!!!(p) {:.unnumlist}

`       '(* ,(integrate-from-table (exp-op u) (exp-lhs u))`
!!!(p) {:.unnumlist}

`      ,(unfactorize k2))))))))`
!!!(p) {:.unnumlist}

There are three cases.
In any case, all factors are of the form `(^ u n)`, so we separate the factor into a base, `u`, and exponent, `n`.
If *u* or *un* evenly divides the original expression (here represented as factors), then we have an answer.
But we need to check the exponent, because *∫ undu* is *u**n*+1/(*n* + 1) for *n*≠ − 1, but it is log (*u*) for *n* = − 1.
But there is a third case to consider.
The factor may be something like `(^ (sin (^ x 2)) 1)`, in which case we should consider *f*(*u*) = sin(*x*2).
This case is handled with the help of an integral table.
We don't need a derivative table, because we can just use the simplifier for that.

[ ](#){:#l0155}`(defun deriv (y x) (simplify '(d ,y ,x)))`
!!!(p) {:.unnumlist}

`(defun integration-table (rules)`
!!!(p) {:.unnumlist}

` (dolist (i-rule rules)`
!!!(p) {:.unnumlist}

` (let ((rule (infix->prefix i-rule)))`
!!!(p) {:.unnumlist}

`  (setf (get (exp-op (exp-lhs (exp-lhs rule))) 'int)`
!!!(p) {:.unnumlist}

`   rule))))`
!!!(p) {:.unnumlist}

`(defun in-integral-table?
(exp)`
!!!(p) {:.unnumlist}

` (and (exp-p exp) (get (exp-op exp) 'int)))`
!!!(p) {:.unnumlist}

`(defun integrate-from-table (op arg)`
!!!(p) {:.unnumlist}

` (let ((rule (get op 'int)))`
!!!(p) {:.unnumlist}

` (subst arg (exp-lhs (exp-lhs (exp-lhs rule))) (exp-rhs rule))))`
!!!(p) {:.unnumlist}

`(integration-table`
!!!(p) {:.unnumlist}

` '((Int log(x) d x = x * log(x) - x)`
!!!(p) {:.unnumlist}

` (Int exp(x) d x = exp(x))`
!!!(p) {:.unnumlist}

` (Int sin(x) d x = - cos(x))`
!!!(p) {:.unnumlist}

` (Int cos(x) d x = sin(x))`
!!!(p) {:.unnumlist}

` (Int tan(x) d x = - log(cos(x)))`
!!!(p) {:.unnumlist}

` (Int sinh(x) d x = cosh(x))`
!!!(p) {:.unnumlist}

` (Int cosh(x) d x = sinh(x))`
!!!(p) {:.unnumlist}

` (Int tanh(x) d x = log(cosh(x)))`
!!!(p) {:.unnumlist}

` ))`
!!!(p) {:.unnumlist}

The last step is to install integrate as the simplification function for the operator Int.
The obvious way to do this is:

[ ](#){:#l0160}`(set-simp-fn 'Int 'integrate)`
!!!(p) {:.unnumlist}

Unfortunately, that does not quite work.
The problem is that integrate expects two arguments, corresponding to the two arguments *`y`* and *`x`* in `( Int *y x*)`.
But the convention for simplification functions is to pass them a single argument, consisting of the whole expression `( Int *y x*)`.
We could go back and edit `simplify-exp` to change the convention, but instead I choose to make the conversion this way:

[ ](#){:#l0165}`(set-simp-fn 'Int #'(lambda (exp)`
!!!(p) {:.unnumlist}

`     (integrate (exp-lhs exp) (exp-rhs exp))))`
!!!(p) {:.unnumlist}

Here are some examples, taken from [chapters 8](#c0040) and [9](B9780080571157500091.xhtml) of *Calculus* ([Loomis 1974](B9780080571157500285.xhtml#bb0750)):

[ ](#){:#l0170}`SIMPLIFIER > (Int x * sin(x ^ 2) d x)`
!!!(p) {:.unnumlist}

`(1/2 * (- (COS (X ^ 2))))`
!!!(p) {:.unnumlist}

`SIMPLIFIER > (Int ((3 * x ^ 3) - 1 / (3 * x ^ 3)) d x)`
!!!(p) {:.unnumlist}

`((3 * ((X ^ 4) / 4)) - (1/3 * ((X ^ -2) / -2)))`
!!!(p) {:.unnumlist}

`SIMPLIFIER > (Int (3 * x + 2) ^ -2/3 d x)`
!!!(p) {:.unnumlist}

`(((3 * X) + 2) ^ 1/3)`
!!!(p) {:.unnumlist}

`SIMPLIFIER > (Int sin(x) ^ 2 * cos(x) d x)`
!!!(p) {:.unnumlist}

`(((SIN X) ^ 3) / 3)`
!!!(p) {:.unnumlist}

`SIMPLIFIER > (Int sin(x) / (1 + cos(x)) d x)`
!!!(p) {:.unnumlist}

`(-1 * (LOG ((COS X) + 1)))`
!!!(p) {:.unnumlist}

`SIMPLIFIER > (Int (2 * x + 1) / (x ^ 2 + x - 1) d x)`
!!!(p) {:.unnumlist}

`(LOG ((X ^ 2) + (X - 1)))`
!!!(p) {:.unnumlist}

`SIMPLIFIER > (Int 8 * x ^ 2 / (x ^ 3 + 2) ^ 3 d x)`
!!!(p) {:.unnumlist}

`(8 * ((1/3 * (((X ^ 3) + 2) ^ -2)) / -2))`
!!!(p) {:.unnumlist}

All the answers are correct, although the last one could be made simpler.
One quick way to simplify such an expression is to factor and unfactor it, and then simplify again:

[ ](#){:#l0175}`(set-simp-fn 'Int`
!!!(p) {:.unnumlist}

`  #'(lambda (exp)`
!!!(p) {:.unnumlist}

`   (unfactorize`
!!!(p) {:.unnumlist}

`    (factorize`
!!!(p) {:.unnumlist}

`     (integrate (exp-lhs exp) (exp-rhs exp))))))`
!!!(p) {:.unnumlist}

With this change, we get:

[ ](#){:#l0180}`SIMPLIFIER > (Int 8 * x ^ 2 / (x ^ 3 + 2) ^ 3 d x)`
!!!(p) {:.unnumlist}

`(-4/3 * (((X ^ 3) + 2) ^ -2))`
!!!(p) {:.unnumlist}

## [ ](#){:#st0040}8.7 History and References
{:#s0040}
{:.h1hd}

A brief history is given in the introduction to this chapter.
An interesting point is that the history of Lisp and of symbolic algebraic manipulation are deeply intertwined.
It is not too gross an exaggeration to say that Lisp was invented by John McCarthy to express the symbolic differentiation algorithm.
And the development of the first high-quality Lisp system, MacLisp, was driven largely by the needs of MACSYMA !!!(span) {:.smallcaps} , one of the first large Lisp systems.
See [McCarthy 1958](B9780080571157500285.xhtml#bb0790) for early Lisp history and the differentiation algorithm, and [Martin and Fateman 1971](B9780080571157500285.xhtml#bb0775) and [Moses (1975)](B9780080571157500285.xhtml#bb0875) for more details on MACSYMA !!!(span) {:.smallcaps} .
A comprehensive book on computer algebra systems is [Davenport 1988](B9780080571157500285.xhtml#bb0270).
It covers the MACSYMA !!!(span) {:.smallcaps} and REDUCE !!!(span) {:.smallcaps} systems as well as the algorithms behind those systems.

Because symbolic differentiation is historically important, it is presented in a number of text books, from the original Lisp 1.5 Primer ([Weissman 1967](B9780080571157500285.xhtml#bb1370)) and Allen's influential [*Anatomy of Lisp* (1978)](B9780080571157500285.xhtml#bb0040) to recent texts like [Brooks 1985](B9780080571157500285.xhtml#bb0135), [Hennessey 1989](B9780080571157500285.xhtml#bb0530), and [Tanimoto 1990](B9780080571157500285.xhtml#bb1220).
Many of these books use rules or data-driven programming, but each treats differentiation as the main task, with simplification as a separate problem.
None of them use the approach taken here, where differentiation is just another kind of simplification.

The symbolic integration programs SAINT !!!(span) {:.smallcaps} and SIN !!!(span) {:.smallcaps} are covered in [Slagle 1963](B9780080571157500285.xhtml#bb1115) and [Moses 1967](B9780080571157500285.xhtml#bb0870), respectively.
The mathematical solution to the problem of integration in closed term is addressed in [Risch 1969](B9780080571157500285.xhtml#bb0985), but be warned; this paper is not for the mathematically naive, and it has no hints on programming the algorithm.
A better reference is [Davenport et al.
1988](B9780080571157500285.xhtml#bb0270).

In this book, techniques for improving the efficiency of algebraic manipulation are covered in [sections 9.6](B9780080571157500091.xhtml#s0035) and [10.4](B9780080571157500108.xhtml#s0025).
[Chapter 15](B9780080571157500157.xhtml) presents a reimplementation that does not use pattern-matching, and is closer to the techniques used in MACSYMA !!!(span) {:.smallcaps} .

## [ ](#){:#st0045}8.8 Exercises
{:#s0045}
{:.h1hd}

**Exercise 8.2 [s]** Some notations use the operator ** instead of ^ to indicate exponentiation.
`Fix infix->prefix` so that either notation is allowed.

**Exercise 8.3 [m]** Can the system as is deal with imaginary numbers?
What are some of the difficulties?

**Exercise 8.4 [h]** There are some simple expressions involving sums that are not handled by the `integrate` function.
The function can integrate *a*× *x*2 + *b*× *x* + *c* but not 5 × (*a*× *x*2 + *b*× *x* + *c*).
Similarly, it can integrate *x*4 + 2 × *x*3 + *x*2 but not (*x*2 + *x*)2, and it can do *x*3 + *x*2 + *x* + 1 but not (*x*2 + 1) × (*x* + 1).
Modify `integrate` so that it expands out products (or small exponents) of sums.
You will probably want to try the usual techniques first, and do the expansion only when that fails.

**Exercise 8.5 [d]** Another very general integration technique is called integration by parts.
It is based on the rule:

∫udv=uv−∫vdu

![si6_e](images/B978008057115750008X/si6_e.gif)

So, for example, given

∫xcosxdx

![si7_e](images/B978008057115750008X/si7_e.gif)

we can take *u* = *x*, *dv* = cos *xdx*.
Then we can determine *v* = sin *x* by integration, and come up with the solution:

∫xcosxdx=xsinx−∫sinx×1dx=xsinx+cosx

![si8_e](images/B978008057115750008X/si8_e.gif)

It is easy to program an integration by parts routine.
The hard part is to program the control component.
Integration by parts involves a recursive call to `integrate`, and of all the possible ways of breaking up the original expression into a *u* and a *dv*, few, if any, will lead to a successful integration.
One simple control rule is to allow integration by parts only at the top level, not at the recursive level.
Implement this approach.

**Exercise 8.6 [d]** A more complicated approach is to try to decide which ways of breaking up the original expression are promising and which are not.
Derive some heuristics for making this division, and reimplement `integrate` to include a search component, using the search tools of [chapter 6](B9780080571157500066.xhtml).

Look in a calculus textbook to see how *∫* sin2*xdx* is evaluated by two integrations by parts and a division.
Implement this technique as well.

**Exercise 8.7 [m]** Write simplification rules for predicate calculus expressions.
For example,

[ ](#){:#l0185}`(true and x = x)`
!!!(p) {:.unnumlist}

`(false and x = false)`
!!!(p) {:.unnumlist}

`(true or x = true)`
!!!(p) {:.unnumlist}

`(false or x = false)`
!!!(p) {:.unnumlist}

**Exercise 8.8 [m]** The simplification rule `(x / 0 = undefined)` is necessary to avoid problems with division by zero, but the treatment of `undefined` is inadequate.
For example, the expression `((0 / 0) - (0 / 0))` will simplify to zero, when it should simplify to `undefined`.
Add rules to propagate `undefined` values and prevent them from being simplified away.

**Exercise 8.9 [d]** Extend the method used to handle `undefined` to handle `+ infinity` and `-infinity` as well.

----------------------

[1](#xfn0010){:#np0010}MACSYMA !!!(span) {:.smallcaps} is the Project MAC SYMbolic MAthematics program.
Project MAC is the MIT research organization that was the precursor of MIT's Laboratory for Computer Science.
MAC stood either for Machine-Aided Cognition or Multiple-Access Computer, according to one of their annual reports.
The cynical have claimed that MAC really stood for Man Against Computer.
!!!(p) {:.ftnote1}

[2](#xfn0015){:#np0015} The term antiderivative is more correct, because of branch point problems.
!!!(p) {:.ftnote1}

Part III
Tools and Techniques
!!!(p) {:.parttitle}

