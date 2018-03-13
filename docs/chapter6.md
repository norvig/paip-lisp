# Chapter 6 {docsify-ignore}
<a id='page-175'></a>

Building Software Tools 

Man is a tool-using animal 
Without tools he is nothing 
with tools he is all 

-Thomas Carlyle (1795-. 881) 

I
I
n chapters 4 and 5 we were concerned with building two particular programs, GPS and ELIZA. 
In this chapter, we will reexamine those two programs to discover some common patterns. 
Those patterns will be abstracted out to form reusable software tools that will prove helpful 
in subsequent chapters. 

6.1 An Interactive Interpreter Tool 
The structure of the function el i za is a common one. It is repeated below: 

(defun eliza () 
"Respond to user input using pattern matching rules." 
(loop 

(print 'eliza>) 

(print (flatten (use-eliza-rules (read)))))) 

<a id='page-176'></a>

Many other appHcations use this pattern, including Lisp itself. The top level of Lisp 
could be defined as: 

(defun lisp () 

(loop 

(print '>) 

(print (eval (read))))) 

The top level of a Lisp system has historically been called the "read-eval-print loop." 
Most modern Lisps print a prompt before reading input, so it should really be called 
the "prompt-read-eval-print loop," but there was no prompt in some early systems 
like MacLisp, so the shorter name stuck. If we left out the prompt, we could write a 
complete Lisp interpreter using just four symbols: 

(loop (print (eval (read)))) 

It may seem facetious to say those four symbols and eight parentheses constitute a 
Lisp interpreter. When we write that line, have we really accomplished anything? 
One answer to that question is to consider what we would have to do to write a Lisp 
(or Pascal) interpreter in Pascal. We would need a lexical analyzer and a symbol table 
manager. This is a considerable amount of work, but it is all handled by read. We 
would need a syntactic parser to assemble the lexical tokens into statements, read 
also handles this, but only because Lisp statements have trivial syntax: the syntax 
of lists and atoms. Thus read serves fine as a syntactic parser for Lisp, but would 
fail for Pascal. Next, we need the evaluation or interpretation part of the interpreter; 
eval does this nicely, and could handle Pascal just as well if we parsed Pascal syntax 
into Lisp expressions, print does much less work than read or eval, but is still 
quite handy. 

The important point is not whether one line of code can be considered an implementation 
of Lisp; it is to recognize common patterns of computation. Both el iza 
and lisp can be seen as interactive interpreters that read some input, transform or 
evaluate the input in some way, print the result, and then go back for more input. We 
can extract the following common pattern: 

(defun program () 

(loop 
(print prompt) 
(print (transform (read))))) 

There are two ways to make use of recurring patterns like this: formally and informally. 
The informal alternative is to treat the pattern as a cliche or idiom that will 
occur frequently in our writing of programs but will vary from use to use. When we 

<a id='page-177'></a>
want to write a new program, we remember writing or reading a similar one, go back 
and look at the first program, copy the relevant sections, and then modify them for 
the new program. If the borrowing is extensive, it would be good practice to insert 
a comment in the new program citing the original, but there would be no "official" 
connection between the original and the derived program. 

The formal alternative is to create an abstraction, in the form of functions and perhaps 
data structures, and refer explicitly to that abstraction in each new application— 
in other words, to capture the abstraction in the form of a useable software tool. The 
interpreter pattern could be abstracted into a function as follows: 

(defun interactive-interpreter (prompt transformer) 
"Read an expression, transform it, and print the result." 
(loop 

(print prompt) 

(print (funcall transformer (read))))) 

This function could then be used in writing each new interpreter: 

(defun lisp () 
(interactive-interpreter *> #'eval)) 

(defun eliza () 
(interactive-interpreter 'eliza> 
#'(lambda (x) (flatten (use-eliza-rules x))))) 

Or, with the help of the higher-order function compose: 

(defun compose (f g) 
"Return the function that computes (f (g x))." 
#'(lambda (x) (funcall f (funcall g x)))) 

(defun eliza () 
(i nteracti ve-i nterpreter 'el iza> 
(compose #'flatten #'use-eliza-rules))) 

There are two differences between the formal and informal approaches. First, they 
look different. If the abstraction is a simple one, as this one is, then it is probably 
easier to read an expression that has the loop explicitly written out than to read one 
that calls interact! ve-i nterpreter, since that requires finding the definition of 
i nteracti ve - i nterpreter and understanding it as well. 

The other difference shows up in what's called maintenance. Suppose we find a 
missing feature in the definition of the interactive interpreter. One such omission is 
that the 1 oop has no exit. I have been assuming that the user can terminate the loop by 
hitting some interrupt (or break, or abort) key. A cleaner implementation would allow 

<a id='page-178'></a>

the user to give the interpreter an explicit termination command. Another useful 
feature would be to handle errors within the interpreter. If we use the irrformal 
approach, then adding such a feature to one program would have no effect on the 
others. Butif we use the formal approach, then improving i nteracti ve- i nterpreter 
would automatically bring the new features to all the programs that use it. 

The following version of i nteracti ve- i nterpreter adds two new features. First, 
it uses the macro handler-case^ to handle errors. This macro evaluates its first 
argument, and normally just returns that value. However, if an error occurs, the 
subsequent arguments are checked for an error condition that matches the error that 
occurred. In this use, the case error matches all errors, and the action taken is to 
print the error condition and continue. 

This version also allows the prompt to be either a string or a function of no 
arguments that will be called to print the prompt. The function prompt-generator, 
for example, returns a function that will print prompts of the form [1], C2], and 
so forth. 

(defun interactive-interpreter (prompt transformer) 
"Read an expression, transform it, and print the result." 
(loop 

(handler-case 
(progn 

(if (stringp prompt) 
(print prompt) 
(funcall prompt)) 

(print (funcall transformer (read)))) 
In case of error, do this: 
(error (condition) 
(format t "'^&;; Error ~a ignored, back to top level. " 
condition))))) 

(defun prompt-generator (&optional (num 0) (ctl-string "C^d] ")) 
"Return a function that prints prompts like [1]. C2]. etc." 
#*(lambda () (format t ctl-string (incf num)))) 

6.2 A Pattern-Matching Tool 
The pat-match function was a pattern matcher defined specifically for the ELIZA 
program. Subsequent programs will need pattern matchers too, and rather than 
write specialized matchers for each new program, it is easier to define one general 

^The macro hand! er-case is only in ANSI Common Lisp. 

<a id='page-179'></a>
pattern matcher that can serve most needs, and is extensible in case novel needs 
come up. 

The problem in designing a "general" tool is deciding what features to provide. 
We can try to define features that might be useful, but it is also a good idea to make 
the list of features open-ended, so that new ones can be easily added when needed. 

Features can be added by generalizing or specializing existing ones. For example, 
we provide segment variables that match zero or more input elements. We can 
specialize this by providing for a kind of segment variable that matches one or more 
elements, or for an optional variable that matches zero or one element. Another 
possibility is to generalize segment variables to specify a match of m to . elements, for 
any specified m and n. These ideas come from experience with notations for writing 
regular expressions, as well as from very general heuristics for generalization, such 
as "consider important special cases" and "zero and one are likely to be important 
special cases." 

Another useful feature is to allow the user to specify an arbitrary predicate that 
a match must satisfy. The notation (?i s ?n numberp) could be used to match any 
expression that is a number and bind it to the variable ?n. This would look like: 

> (pat-match '(x = (?is ?n numberp)) '(x = 34)) => ((?n . 34)) 

> (pat-match '(x = (?is ?n numberp)) '(x = x)) => NIL 

Since patterns are like boolean expressions, it makes sense to allow boolean operators 
on them. Following the question-mark convention, we will use ?and, ?or and ?not 
for the operators.^ Here is a pattern to match a relational expression with one of three 
relations. It succeeds because the < matches one of the three possibiUties specified 
by(?or < = >). 

> (pat-match '(?x (?or < = >) ?y) *(3 < 4)) => ((?Y . 4) (?X . 3)) 

Here is an example of an ?and pattern that checks if an expression is both a number 
and odd: 

> (pat-match *(x = (?and (?is ?n numberp) (?is ?n oddp))) 
'(x = 3)) 
((?N . 3)) 

^An alternative would be to reserve the question mark for variables only and use another 
notation for these match operators. Keywords would be a good choice, such as : and,: or,: i s, 
etc. 

<a id='page-180'></a>

The next pattern uses ?not to insure that two parts are not equal: 

> (pat-match '(?x /= (?not ?x)) '(3 /= 4)) => ((?X . 3)) 

The segment matching notation we have seen before. It is augmented to allow for 
three possibilities: zero or more expressions; one or more expressions; and zero or 
one expressions. Finally, the notation (?if exp) can be used to test a relationship 
between several variables. It has to be Usted as a segment pattern rather than a single 
pattern because it does not consume any of the input at all: 

> (pat-match '(?x > ?y (?if (> ?x ?y))) '(4 > 3)) => 
((?Y . 3) (?X . 4)) 

When the description of a problem gets this complicated, it is a good idea to 
attempt a more formal specification. The following table describes a grammar of 
patterns, using the same grammar rule format described in chapter 2. 

pat => var match any one expression 

constant match just this atom 

segment-pat match something against a sequence 

single-pat match something against one expression 

(pat. pat) match the first and the rest 
single-pat =4^ (lis var predicate) test predicate on one expression 
(lor pat...) match any pattern on one expression 
(?andpai...) match every pattern on one expression 
(?not pat...) succeed if pattern(s) do not match 
segment-pat ((
l*var)
(d+var)
((V.var)
((?if 
...) 
...) 
...) 
exp)...) 
match zero or more expressions 
match one or more expressions 
match zero or one expression 
test if exp (which may contain 
variables) is true 

. chars a symbol starting with ? 

var-

atom any nonvariable atom 

constant 


Despite the added complexity, all patterns can still be classified into five cases. 
The pattern must be either a variable, constant, a (generalized) segment pattern, 
a (generalized) single-element pattern, or a cons of two patterns. The following 
definition of pat -match reflects the five cases (along with two checks for failure): 

<a id='page-181'></a>
(defun pat-match (pattern input &optional (bindings no-bindings)) 
"Match pattern against input in the context of the bindings" 
(cond ((eq bindings fail) fail) 

((variable-p pattern) 

(match-variable pattern input bindings)) 
((eql pattern input) bindings) 
((segment-pattern-p pattern) 

(segment-matcher pattern input bindings)) 
((single-pattern-p pattern) ; *** 
(single-matcher pattern input bindings)) ; *** 
((and (consp pattern) (consp input)) 
(pat-match (rest pattern) (rest input) 
(pat-match (first pattern) (first input) 
bindings))) 
(t fail))) 

For completeness, we repeat here the necessary constants and low-level functions 
from ELIZA: 

(defconstant fail nil "Indicates pat-match failure") 

(defconstant no-bindings '((t . t)) 
"Indicates pat-match success, with no variables.") 

(defun variable-p (x) 
"Is . a variable (a symbol beginning with '?*)?" 
(and (symbolp x) (equal (char (symbol-name x) 0) #\?))) 

(defun get-binding (var bindings) 
"Find a (variable . value) pair in a binding list. " 
(assoc var bindings)) 

(defun binding-var (binding) 
"Get the variable part of a single binding." 
(car binding)) 

(defun binding-val (binding) 
"Get the value part of a single binding." 
(cdr binding)) 

(defun make-binding (var val) (cons var val)) 

(defun lookup (var bindings) 
"Get the value part (for var) from a binding list. " 
(binding-val (get-binding var bindings))) 

<a id='page-182'></a>

(defun extend-bindings (var val bindings) 

"Add a (var . value) pair to a binding list." 

(cons (make-binding var val) 

Once we add a "real" binding, 

we can get rid of the dummy no-bindings 

(if (eq bindings no-bindings) 

nil 

bindings) 

(defun match-variable (var input bindings) 

"Does VAR match input? Uses (or updates) and returns bindings." 

(let ((binding (get-binding var bindings))) 

(cond ((not binding) (extend-bindings var input bindings)) 

((equal input (binding-val binding)) bindings) 

(t fail)))) 

The next step is to define the predicates that recognize generalized segment and 
single-element patterns, and the matching functions that operate on them. We could 
implementsegment-matcherandsingle-matcherwithcasestatementsthatconsider 
all possible cases. However, that would make it difficult to extend the matcher. A 
programmer who wanted to add a new kind of segment pattern would have to edit 
the definitions of both segment-pattern-p and segment-matcher to install the new 
feature. This by itself may not be too bad, but consider what happens when two 
programmers each add independent features. If you want to use both, then neither 
version of segment-matcher (or segment-pattern-p) will do. You'll have to edit the 
functions again, just to merge the two extensions. 

The solution to this dilemma is to write one version of segment-pattern-p and 
segment-matcher, once and for all, but to have these functions refer to a table of 
pattern/action pairs. The table would say "if you see ?* in the pattern, then use 
the function segment-match," and so on. Then programmers who want to extend 
the matcher just add entries to the table, and it is trivial to merge different extensions 
(unless of course two programmers have chosen the same symbol to mark 
different actions). 

This style of programming, where pattern/action pairs are stored in a table, is 
called data-driven programming. It is a very flexible style that is appropriate for writing 
extensible systems. 

There are many ways to implement tables in Conunon Lisp, as discussed in 
section 3.6, [page 73](chapter3.md#page-73). In this case, the keys to the table will be symbols (like ?*), 
and it is fine if the representation of the table is distributed across memory. Thus, 
property lists are an appropriate choice. We will have two tables, represented by 
the segment-match property and the si ngl e-match property of symbols like ?*. The 
value of each property will be the name of a function that implements the match. 
Here are the table entries to implement the granunar listed previously: 

<a id='page-183'></a>
(setf (get '?is 'single-match) 'match-is) 
(setf (get '?or 'single-match) 'match-or) 
(setf (get '?and 'single-match) 'match-and) 
(setf (get '?not 'single-match) 'match-not) 

(setf (get '? * 'segment-match) 'segment-match) 
(setf (get '?+ 'segment-match) 'segment-match+) 
(setf (get '?? 'segment-match) 'segment-match?) 
(setf (get '?if 'segment-match) 'match-if) 

With the table defined, we need to do two things. First, define the "glue" that holds 
the table together: the predicates and action-taking functions. A function that looks 
upadata-driven function and calls it (such as segment-matcher and single-matcher) 
is called a dispatch function. 

(defun segment-pattern-p (pattern) 
"Is this a segment-matching pattern like ((?* var) . pat)?" 
(and (consp pattern) (consp (first pattern)) 

(symbolp (first (first pattern))) 
(segment-match-fn (first (first pattern))))) 

(defun single-pattern-p (pattern) 
"Is this a single-matching pattern? 

E.g. (?is X predicate) (?and . patterns) (?or . patterns)." 
(and (consp pattern) 
(single-match-fn (first pattern)))) 

(defun segment-matcher (pattern input bindings) 
"Call the right function for this kind of segment pattern." 
(funcall (segment-match-fn (first (first pattern))) 

pattern input bindings)) 

(defun single-matcher (pattern input bindings) 
"Call the right function for this kind of single pattern." 
(funcall (single-match-fn (first pattern)) 

(rest pattern) input bindings)) 

(defun segment-match-fn (x) 
"Get the segment-match function for x. 
if it is a symbol that has one." 
(when (symbolp x) (get . 'segment-match))) 

(defun single-match-fn (x) 
"Get the single-match function for x, 
if it is a symbol that has one." 
(when (symbolp x) (get . 'single-match))) 

<a id='page-184'></a>

The last thing to do is define the individual matching functions. First, the single-
pattern matching functions: 

(defun match-is (var-and-pred input bindings) 
"Succeed and bind var if the input satisfies pred, 
where var-and-pred is the list (var pred)." 
(let* ((var (first var-and-pred)) 

(pred (second var-and-pred)) 
(new-bindings (pat-match var input bindings))) 
(if (or (eq new-bindings fail) 

(not (funcall pred input))) 
fail 
new-bindings))) 

(defun match-and (patterns input bindings) 
"Succeed if all the patterns match the input." 
(cond ((eq bindings fail) fail) 

((null patterns) bindings) 
(t (match-and (rest patterns) input 
(pat-match (first patterns) input 
bindings))))) 

(defun match-or (patterns input bindings) 
"Succeed if any one of the patterns match the input." 
(if (null patterns) 

fail 
(let ((new-bindings (pat-match (first patterns) 
input bindings))) 

(if (eq new-bindings fail) 
(match-or (rest patterns) input bindings) 
new-bindings)))) 

(defun match-not (patterns input bindings) 
"Succeed if none of the patterns match the input. 
This will never bind any variables." 
(if (match-or patterns input bindings) 

fail 
bindings)) 

Now the segment-pattern matching functions, segment-match is similar to the version 
presented as part of ELIZA. The difference is in how we determine pos, the 
position of the first element of the input that could match the next element of the 
pattern after the segment variable. In ELIZA, we assumed that the segment variable 
was either the last element of the pattern or was followed by a constant. In the 
following version, we allow nonconstant patterns to follow segment variables. The 
function first -match - pos is added to handle this. If the following element is in fact 
a constant, the same calculation is done using posi ti on. If it is not a constant, then 

<a id='page-185'></a>
we just return the first possible starting position—unless that would put us past the 
end of the input, in which case we return nil to indicate failure: 

(defun segment-match (pattern input bindings &optional (start 0)) 
"Match the segment pattern ((?* var) . pat) against input." 
(let ((var (second (first pattern))) 

(pat (rest pattern))) 

(if (null pat) 
(match-variable var input bindings) 
(let ((pos (first-match-pos (first pat) input start))) 

(if (null pos) 
fail 
(let ((b2 (pat-match 

pat (subseq input pos) 
(match-variable var (subseq input 0 pos) 
bindings)))) 
If this match failed, try another longer one 

(if (eq b2 fail) 
(segment-match pattern input bindings (+ pos 1)) 
b2))))))) 

(defun first-match-pos (patl input start) 
"Find the first position that patl could possibly match input, 
starting at position start. If patl is non-constant, then just 
return start." 
(cond ((and (atom patl) (not (variable-p patl))) 

(position patl input :start start :test #*equal)) 
((< start (length input)) start) 
(t nil))) 

In the first example below, the segment variable ?x matches the sequence (b c). In 
the second example, there are two segment variables in a row. The first successful 
match is achieved with the first variable, ?x, matching the empty sequence, and the 
second one, ?y, matching (be). 

> (pat-match '(a (?* ?x) d) '(a b c d)) => ((?X . .) 

> (pat-match '(a (?* ?x) (?* ?y) d) '(a b c d)) => ((?Y . C) (?X)) 

In the next example, ?x is first matched against nil and ?y against (be d), but that 
fails, so we try matching ?x against a segment of length one. That fails too, but 
finally the match succeeds with ?x matching the two-element segment (be), and ?y 
matching (d). 

<a id='page-186'></a>

> (pat-match '(a (?* ?x) (?* ?y) ?x ?y) 
'(a b c d (b c) (d))) ((?Y D) (?X . .) 

Given segment - match, it is easy to define the function to match one-or-more elements 
and the function to match zero-or-one element: 

(defun segment-match+ (pattern input bindings) 
"Match one or more elements of input." 
(segment-match pattern input bindings D) 

(defun segment-match? (pattern input bindings) 
"Match zero or one element of input." 
(let ((var (second (first pattern))) 

(pat (rest pattern))) 
(or (pat-match (cons var pat) input bindings) 
(pat-match pat input bindings)))) 

Finally, we supply the function to test an arbitrary piece of Lisp code. It does this 
by evaluating the code with the bindings implied by the binding list. This is one of 
the few cases where it is appropriate to call eval: when we want to give the user 
unrestricted access to the Lisp interpreter. 

(defun match-if (pattern input bindings) 
"Test an arbitrary expression involving variables. 
The pattern looks like ((?if code) . rest)." 
(and (progv (mapcar #*car bindings) 

(mapcar #'cdr bindings) 
(eval (second (first pattern)))) 
(pat-match (rest pattern) input bindings))) 

Here are two examples using ?i f. The first succeeds because (+ 3 4) is indeed 7, 
and the second fails because (> 3 4) is false. 

> (pat-match '(?x ?op ?y is ?z (?if (eql (?op ?x ?y) ?z))) 
'(3 + 4 is 7)) 
((?Z . 7) (?Y . 4) (?0P . +) (?X . 3)) 

> (pat-match '(?x ?op ?y (?if (?op ?x ?y))) 
'(3 > 4)) 
NIL 

The syntax we have defined for patterns has two virtues: first, the syntax is very 
general, so it is easy to extend. Second, the syntax can be easily manipulated by 
pat-match. However, there is one drawback: the syntax is a little verbose, and some 
may find it ugly. Compare the following two patterns: 

<a id='page-187'></a>
(a (?* ?x) (?* ?y) d) 

(a ?x* ?y* d) 

Many readers find the second pattern easier to understand at a glance. We could 
change pat-match to allow for patterns of the form ?x*, but that would mean 
pat-match would have a lot more work to do on every match. An alternative is 
to leave pat-match as is, but define another level of syntax for use by human readers 
only. That is, a programmer could type the second expression above, and have it 
translated into the first, which would then be processed by pat-match. 

In other words, we will define a facility to define a kind of pattern-matching 
macro that will be expanded the first time the pattern is seen. It is better to do this 
expansion once than to complicate pat-match and in effect do the expansion every 
time a pattern is used. (Of course, if a pattern is only used once, then there is no 
advantage. But in most programs, each pattern will be used again and again.) 

We need to define two functions: one to define pattern-matching macros, and 
another to expand patterns that may contain these macros. We will only allow 
symbols to be macros, so it is reasonable to store the expansions on each symbol's 
property list: 

(defun pat-match-abbrev (symbol expansion) 
"Define symbol as a macro standing for a pat-match pattern." 
(setf (get symbol *expand-pat-match-abbrev) 

(expand-pat-match-abbrev expansion)) 

(defun expand-pat-match-abbrev (pat) 
"Expand out all pattern matching abbreviations in pat." 
(cond ((and (symbolp pat) (get pat 'expand-pat-match-abbrev))) 

((atom pat) pat) 
(t (cons (expand-pat-match-abbrev (first pat)) 
(expand-pat-match-abbrev (rest pat)))))) 

We would use this facility as follows: 

> (pat-match-abbrev '?x* '(?* ?x)) => (?* ?X) 

> (pat-match-abbrev '?y* '(?* ?y)) (?* ?Y) 

> (setf axyd (expand-pat-match-abbrev '(a ?x* ?y* d))) 
(A (?* ?X) (?* ?Y) D) 

> (pat-match axyd '(a b c d)) ((?Y . C) (?X)) 

&#9635; Exercise 6.1 [m] Go back and change the ELIZA rules to use the abbreviation facility. 
Does this make the rules easier to read? 

<a id='page-188'></a>

&#9635; Exercise 6.2 [h] In the few prior examples, every time there was a binding of 
pattern variables that satisfied the input, that binding was found. Informally, show 
that pat-match will always find such a binding, or show a counterexample where it 
fails to find one. 

6.3 A Rule-Based Translator Tool 
As we have defined it, the pattern matcher matches one input against one pattern. In 
el i . a, we need to match each input against a number of patterns, and then return a 
result based on the rule that contains the first pattern that matches. To refresh your 
memory, here is the function use-el i za - rul es: 

(defun use-eliza-rules (input) 
"Find some rule with which to transform the input." 
(some #*(lambda (rule) 

(let ((result (pat-match (rule-pattern rule) input))) 
(if (not (eq result fail)) 
(sublis (switch-viewpoint result) 
(random-elt (rule-responses rule)))))) 
*eliza-rules*)) 

It turns out that this will be a quite common thing to do: search through a list of rules 
for one that matches, and take action according to that rule. To turn the structure of 
use-el i za-rules into a software tool, we will allow the user to specify each of the 
following: 

* What kind of rule to use. Every rule will be characterized by an if-part and a 
then-part, but the ways of getting at those two parts may vary. 
* What list of rules to use. In general, each appHcation will have its own list of 
rules. 
* How to see if a rule matches. By default, we will use pat-match, but it should 
be possible to use other matchers. 
* What to do when a rule matches. Once we have determined which rule to use, 
we have to determine what it means to use it. The default is just to substitute 
the bindings of the match into the then-part of the rule. 
<a id='page-189'></a>
The rule-based translator tool now looks like this: 

(defun rule-based-translator 
(input rules &key (matcher #'pat-match) 

(rule-if #*first) (rule-then #*rest) (action #*sublis)) 
"Find the first rule in rules that matches input, 
and apply the action to that rule." 
(some 

#'(lambda (rule) 
(let ((result (funcall matcher (funcall rule-if rule) 
input))) 
(if (not (eq result fail)) 
(funcall action result (funcall rule-then rule))))) 
rules)) 

(defun use-eliza-rules (input) 
"Find some rule with which to transform the input.' 
(rule-based-translator input *eliza-rules* 

laction #.(lambda (bindings responses) 
(sublis (switch-viewpoint bindings) 
(random-elt responses))))) 

6.4 A Set of Searching Tools 
The GPS program can be seen as a problem in search. In general, a search problem 
involves exploring from some starting state and investigating neighboring states 
until a solution is reached. As in GPS, state means a description of any situation or 
state of affairs. Each state may have several neighbors, so there will be a choice of 
how to search. We can travel down one path until we see it is a dead end, or we can 
consider lots of different paths at the same time, expanding each path step by step. 
Search problems are called nondeterministic because there is no way to determine 
what is the best step to take next. AI problems, by their very nature, tend to be 
nondeterministic. This can be a source of confusion for programmers who are used 
to deterministic problems. In this section we will try to clear up that confusion. 
This section also serves as an example of how higher-order functions can be used to 
implement general tools that can be specified by passing in specific functions. 

Abstractly, a search problem can be characterized by four features: 

* The siarf state. 
* The^Oiz/state (or states). 
<a id='page-190'></a>

* The successors, or states that can be reached from any other state. 
* The strategy that determines the order in which we search. 
The first three features are part of the problem, while the fourth is part of the 
solution. In GPS, the starting state was given, along with a description of the goal 
states. The successors of a state were determined by consulting the operators. The 
search strategy was means-ends analysis. This was never spelled out explicitly but 
was impUcit in the structure of the whole program. In this section we will formulate 
a general searching tool, show how it can be used to implement several different 
search strategies, and then show how GPS could be implemented with this tool. 

The first notion we have to define is the state space, or set of all possible states. 
We can view the states as nodes and the successor relation as links in a graph. Some 
state space graphs will have a small number of states, while others have an infinite 
number, but they can still be solved if we search cleverly. Some graphs will have 
a regular structure, while others will appear random. We will start by considering 
only trees—that is, graphs where a state can be reached by only one unique sequence 
of successor links. Here is a tree: 

Searching Trees 

We will call our first searching tool tree-search, because it is designed to search 
state spaces that are in the form of trees. It takes four arguments: (1) a list of valid 
starting states, (2) a predicate to decide if we have reached a goal state, (3) a function 
to generate the successors of a state, and (4) a function that decides in what order 

<a id='page-191'></a>
to search. The first argument is a hst rather than a single state so that tree-search 
can recursively call itself after it has explored several paths through the state space. 
Think of the first argument not as a starting state but as a list of possible states from 
which the goal may be reached. This lists represents the fringe of the tree that has 
been explored so far. tree-search has three cases: If there are no more states to 
consider, then give up and return f ai 1. If the first possible state is a goal state, 
then return the succesful state. Otherwise, generate the successors of the first state 
and combine them with the other states. Order this combined list according to the 
particular search strategy and continue searching. Note that tree - search itself does 
not specify any particular searching strategy. 

(defun tree-search (states goal-p successors combiner) 
"Find a state that satisfies goal-p. Start with states, 
and search according to successors and combiner." 
(dbg :search ""&;; Search: ~a" states) 
(cond ((null states) fail) 

((funcall goal-p (first states)) (first states)) 
(t (tree-search 

(funcall combiner 
(funcall successors (first states)) 
(rest states)) 

goal-p successors combiner)))) 

The first strategy we will consider is called depth-first search. In depth-first search, 
the longest paths are considered first. In other words, we generate the successors 
of a state, and then work on the first successor first. We only return to one of the 
subsequent successors if we arrive at a state that has no successors at all. This 
strategy can be implemented by simply appending the previous states to the end 
of the Ust of new successors on each iteration. The function depth-first-search 
takes a single starting state, a goal predicate, and a successor function. It packages 
the starting state into a Hst as expected by tree-search, and specifies append as the 
combining function: 

(defun depth-first-search (start goal-p successors) 
"Search new states first until goal is reached." 
(tree-search (list start) goal-p successors #'append)) 

Let's see how we can search through the binary tree defined previously. First, we 
define the successor function binary-tree. It returns a list of two states, the two 
numbers that are twice the input state and one more than twice the input state. So the 
successors of 1 will be 2 and 3, and the successors of 2 will be 4 and 5. The bi na ry - tree 
function generates an infinite tree of which the first 15 nodes are diagrammed in our 
example. 

<a id='page-192'></a>

(defun binary-tree (.) (list (* 2 .) (+ 1 (* 2 .)))) 

.. make it easier to specify a goal, we define the function i s as a function that returns 
a predicate that tests for a particular value. Note that 1 s does not do the test itself. 
Rather, it returns a function that can be called to perform tests: 

(defun is (value) #*(lambda (x) (eql . value))) 

Now we can turn on the debugging output and search through the binary tree, starting 
at 1, and looking for, say, 12, as the goal state. Each line of debugging output shows 
the list of states that have been generated as successors but not yet examined: 

> (debug :search) => (SEARCH) 

> (depth-first-search 1 (is 12) #*binary-tree) 

;; Search; (1) 

;: Search: (2 3) 

;; Search: (4 5 3) 

;; Search: (8 9 5 3) 

Search: (16 17 9 5 3) 

:; Search: (32 33 17 9 5 3) 

;; Search: (64 65 33 17 9 5 3) 

Search: (128 129 65 33 17 9 5 3) 

Search: (256 257 129 65 33 17 9 5 3) 

;: Search: (512 513 257 129 65 33 17 9 5 3) 

;; Search: (1024 1025 513 257 129 65 33 17 9 5 3) 

Search: (2048 2049 1025 513 257 129 65 33 17 9 5 3) 

[Abort] 

The problem is that we are searching an infinite tree, and the depth-first search 
strategy just dives down the left-hand branch at every step. The only way to stop the 
doomed search is to type an interrupt character. 

An alternative strategy isbreadth-first search, where the shortest path is extended 
first at each step. It can be implemented simply by appending the new successor 
states to the end of the existing states: 

(defun prepend (x y) "Prepend y to start of x" (append y .)) 

(defun breadth-first-search (start goal-p successors) 
"Search old states first until goal is reached." 
(tree-search (list start) goal-p successors #'prepend)) 

The only difference between depth-first and breadth-first search is the difference 
between append and prepend. Here we see breadth-first-search inaction: 

<a id='page-193'></a>

> (breadth-first-search 1 (is 12) *binary-tree) 
Search: (1) 
Search: (2 3) 
Search: (3 4 5) 
Search: (4 5 6 7) 
Search: (56789) 
Search: (6 7 8 9 10 11) 
Search: (7 8 9 10 11 12 13) 
Search: (8 9 10 11 12 13 14 15) 
Search: (9 10 11 12 13 14 15 16 17) 
Search: (10 11 12 13 14 15 16 17 18 19) 
Search: (11 12 13 14 15 16 17 18 19 20 21) 
Search: (12 13 14 15 16 17 18 19 20 21 22 23) 

12 

Breadth-first search ends up searching each node in numerical order, and so it will 
eventually find any goal. It is methodical, but therefore plodding. Depth-first search 
will be much faster—if it happens to find the goal at all. For example, if we were 
looking for 2048, depth-first search would find it in 12 steps, while breadth-first 
would take 2048 steps. Breadth-first search also requires more storage, because it 
saves more intermediate states. 

If the search tree is finite, then either breadth-first or depth-first will eventually 
find the goal. Both methods search the entire state space, but in a different order. We 
will now show a depth-first search of the 15-node binary tree diagrammed previously. 
It takes about the same amount of time to find the goal (12) as it did with breadth-first 
search. It would have taken more time to find 15; less to find 8. The big difference is 
in the number of states considered at one time. At most, depth-first search considers 
four at a time; in general it will need to store only log2 . states to search a n-node tree, 
while breadth-first search needs to store n/2 states. 

(defun finite-binary-tree (n) 
"Return a successor function that generates a binary tree 
with . nodes." 
#'(lambda (x) 

(remove-if #*(lambda (child) (> child n)) 
(binary-tree x)))) 

> (depth-first-search 1 (is 12) (finite-binary-tree 15)) 

;; Search: (1) 
Search: (2 3) 
Search: (4 5 3) 
Search: (8 9 5 3) 
Search: (9 5 3) 

:: Search: (5 3) 
:: Search: (10 11 3) 
;: Search: (11 3) 

<a id='page-194'></a>

Search: (3) 
Search: (6 7) 
Search: (12 13 7) 

12 

Guiding the Search 

While breadth-first search is more methodical, neither strategy is able to take advantage 
of any knowledge about the state space. They both search blindly. In most real 
applications we will have some estimate of how far a state is from the solution. In 
such cases, we can implement a best-first search. The name is not quite accurate; if 
we could really search best first, that would not be a search at all. The name refers to 
the fact that the state that appears to be best is searched first. 

To implement best-first search we need to add one more piece of information: a 
cost function that gives an estimate of how far a given state is from the goal. 

For the binary tree example, we will use as a cost estimate the numeric difference 
from the goal. So if we are looking for 12, then 12 has cost 0, 8 has cost 4 and 2048 
has cost 2036. The higher-order function d i ff, shown in the following, returns a cost 
function that computes the difference from a goal. The higher-order function sorter 
takes a cost function as an argument and returns a combiner function that takes the 
lists of old and new states, appends them together, and sorts the result based on the 
cost function, lowest cost first. (The built-in function sort sorts a list according to 
a comparison function. In this case the smaller numbers come first, sort takes an 
optional : key argument that says how to compute the score for each element. Be 
careful—sort is a destructive function.) 

(defun diff (num) 
"Return the function that finds the difference from num." 
#'(lambda (x) (abs (- . num)))) 

(defun sorter (cost-fn) 
"Return a combiner function that sorts according to cost-fn." 
#'(lambda (new old) 

(sort (append new old) #'< :key cost-fn))) 

(defun best-first-search (start goal-p successors cost-fn) 
"Search lowest cost states first until goal is reached." 
(tree-search (list start) goal-p successors (sorter cost-fn))) 

Now, using the difference from the goal as the cost function, we can search using 
best-first search: 

<a id='page-195'></a>
> (best-first-search 1 (is 12) #'binary-tree (diff 12)) 
Search: (1) 

;; Search: (3 2) 
Search: (7 6 2) 
Search: (14 15 6 2) 
Search: (15 6 2 28 29) 

;; Search: (6 2 28 29 30 31) 
Search: (12 13 2 28 29 30 31) 
12 

The more we know about the state space, the better we can search. For example, if we 
know that all successors are greater than the states they come from, then we can use 
a cost function that gives a very high cost for numbers above the goal. The function 
price-is-right is like diff, except that it gives a high penalty for going over the 
goal."^ Using this cost function leads to a near-optimal search on this example. It 
makes the "mistake" of searching 7 before 6 (because 7 is closer to 12), but does not 
waste time searching 14 and 15: 

(defun price-is-right (price) 
"Return a function that measures the difference from price, 
but gives a big penalty for going over price." 
#'(lambda (x) (if (> . price) 

most-positive-fixnum 
(- price x)))) 

> (best-first-search 1 (is 12) #'binary-tree (price-is-right 12)) 
Search: (1) 
Search: (3 2) 
Search: (7 6 2) 
Search: (6 2 14 15) 
Search: (12 2 13 14 15) 

12 

All the searching methods we have seen so far consider ever-increasing lists of states 
as they search. For problems where there is only one solution, or a small number of 
solutions, this is unavoidable. To find a needle in a haystack, you need to look at a 
lot of hay. But for problems with many solutions, it may be worthwhile to discard 
unpromising paths. This runs the risk of failing to find a solution at all, but it can 
save enough space and time to offset the risk. A best-first search that keeps only a 
fixed number of alternative states at any one time is known as a beam search. Think 
of searching as shining a light through the dark of the state space. In other search 

^The built-in constant most-positive-fixnum is a large integer, the largest that can be 
expressed without using bignums. Its value depends on the implementation, but in most 
Lisps it is over 16 million. 

<a id='page-196'></a>

strategies the light spreads out as we search deeper, but in beam search the light 
remains tightly focused. Beam search is a variant of best-first search, but it is also 
similar to depth-first search. The difference is that beam search looks down several 
paths at once, instead of just one, and chooses the best one to look at next. But 
it gives up the ability to backtrack indefinitely. The function beam-search is just 
like best-first-search, except that after we sort the states, we then take only the 
first beam-width states. This is done with subseq; (subseq list start end) returns the 
sublist that starts at position start and ends just before position end. 

(defun beam-search (start goal-p successors cost-fn beam-width) 
"Search highest scoring states first until goal is reached, 
but never consider more than beam-width states at a time." 
(tree-search (list start) goal-p successors 

#'(lambda (old new) 
(let ((sorted (funcall (sorter cost-fn) oldnew))) 
(if (> beam-width (length sorted)) 
sorted 

(subseq sorted0 beam-width)))))) 

We can successfully search for 12 in the binary tree using a beam width of only 2: 

> (beam-search 1 (is 12) #*binary-tree (price-is-right 12) 2) 
Search; (1) 
Search; (3 2) 

;; Search; (7 6) 
Search; (6 14) 
Search; (12 13) 

12 

However, if we go back to the scoring function that just takes the difference from 12, 
then beam search fails. When it generates 14 and 15, it throws away 6, and thus loses 
its only chance to find the goal: 

> (beam-search 1 (is 12) #'binary-tree (diff 12) 2) 
Search; (1) 
Search; (3 2) 
Search; (7 6) 
Search; (14 15) 
Search; (15 28) 
Search; (28 30) 
Search; (30 56) 
Search; (56 60) 
Search; (60 112) 
Search; (112 120) 
Search; (120 224) 

<a id='page-197'></a>

[Abort] 

This search would succeed if we gave a beam width of 3. This illustrates a general 
principle: we can find a goal either by looking at more states, or by being smarter 
about the states we look at. That means having a better ordering function. 

Notice that with a beam width of infinity we get best-first search. With a beam 
width of 1, we get depth-first search with no backup. This could be called "depth-only 
search," but it is more commonly known as hill-climbing. Think of a mountaineer 
trying to reach a peak in a heavy fog. One strategy would be for the moimtaineer to 
look at adjacent locations, climb to the highest one, and look again. This strategy 
may eventually hit the peak, but it may also get stuck at the top of a foothill, or local 
manmum. Another strategy would be for the mountaineer to turn back and try again 
when the fog lifts, but in AI, unfortunately, the fog rarely lifts.^ 

As a concrete example of a problem that can be solved by search, consider the 
task of planning a flight across the North American continent in a small airplane, one 
whose range is limited to 1000 kilometers. Suppose we have a list of selected cities 
with airports, along with their position in longitude and latitude: 

(defstruct (city (:type list)) name long lat) 

(defparameter *cities* 

'((Atlanta 84.23 33.45) (Los-Angeles 118.15 34.03) 
(Boston 71.05 42.21) (Memphis 90.03 35.09) 
(Chicago 87.37 41.50) (New-York 73.58 40.47) 
(Denver 105.00 39.45) (Oklahoma-City 97.28 35.26) 
(Eugene 123.05 44.03) (Pittsburgh 79.57 40.27) 
(Flagstaff 111.41 35.13) (Quebec 71.11 46.49) 
(Grand-Jet 108.37 39.05) (Reno 119.49 39.30) 
(Houston 105.00 34.00) (San-Francisco 122.26 37.47) 
(Indianapolis 86.10 39.46) (Tampa 82.27 27.57) 
(Jacksonville 81.40 30.22) (Victoria 123.21 48.25) 
(Kansas-City 94.35 39.06) (Wilmington 77.57 34.14))) 

This example introduces a new option to defstruct. Instead of just giving the name 
of the structure, it is also possible to use: 

(defstruct {structure-name {option value),,,) 'Optionaldoc'' slot,,,) 

For city, the option : type is specified as 1 i st. This means that cities will be implemented 
as lists of three elements, as they are in the initial value for *ci ti es*. 

^In chapter 8 we will see an example where the fog did lift: symbolic integration was once 
handled as a problem in search, but new mathematical results now make it possible to solve 
the same class of integration problems without search. 

<a id='page-198'></a>

Figure 6.1: A Map of Some Cities 

The cities are shown on the map in figure 6.1, which has cormections between 
all cities within the 1000 kilometer range of each other.^ This map was drawn with 
the help of ai r-di stance, a function that retiutis the distance in kilometers between 
two cities "as the crow flies." It will be defined later. Two other useful fimctions are 
nei ghbors, which finds all the cities within 1000 kilometers, and ci ty, which maps 
from a name to a city. The former uses f i nd - a 11 - i f, which was defined on [page 101](chapter3.md#page-101) 
as a synonym for remove- i f-not. 

(defun neighbors (city) 
"Find all cities within 1000 kilometers." 
(find-all-if #'(lambda (c) 

(and (not (eq c city)) 
(< (air-distance c city) 1000.0))) 
*cities*)) 

(defun city (name) 
"Find the city with this name." 
(assoc name *cities*)) 

We are now ready to plan a trip. The fimction trip takes the name of a starting and 
destination city and does a beam search of width one, considering all neighbors as 

^The astute reader will recognize that this graph is not a tree. The difference between trees 
and graphs and the implications for searching will be covered later. 

<a id='page-199'></a>
successors to a state. The cost for a state is the air distance to the destination city: 

(defun trip (start dest) 
"Search for a way from the start to dest." 
(beam-search start (is dest) #'neighbors 

#'(1ambda (c) (air-distance c dest)) 

D) 

Here we plan a trip from San Francisco to Boston. The result seems to be the best 
possible path: 

> (trip (city 'san-francisco) (city 'boston)) 
Search: ((SAN-FRANCISCO 122.26 37.47)) 
Search: ((RENO 119.49 39.3)) 
Search: ((GRAND-JCT 108.37 39.05)) 
Search: ((DENVER 105.0 39.45)) 
Search: ((KANSAS-CITY 94.35 39.06)) 
Search: ((INDIANAPOLIS 86.1 39.46)) 
Search: ((PITTSBURGH 79.57 40.27)) 

:; Search: ((BOSTON 71.05 42.21)) 
(BOSTON 71.05 42.21) 

But look what happens when we plan the return trip. There are two detours, to 
Chicago and Flagstaff: 

> (trip (city 'boston) (city 'san-francisco)) 
Search: ((BOSTON 71.05 42.21)) 
Search: ((PITTSBURGH 79.57 40.27)) 
Search: ((CHICAGO 87.37 41.5)) 
Search: ((KANSAS-CITY 94.35 39.06)) 
Search: ((DENVER 105.0 39.45)) 
Search: ((FLAGSTAFF 111.41 35.13)) 
Search: ((RENO 119.49 39.3)) 
Search: ((SAN-FRANCISCO 122.26 37.47)) 

(SAN-FRANCISCO 122.26 37.47) 

Why did tri . go from Denver to San Francisco via Flagstaff? Because Flagstaff is 
closer to the destination than Grand Junction. The problem is that we are minimizing 
the distance to the destination at each step, when we should be minimizing the sum 
of the distance to the destination plus the distance already traveled. 

<a id='page-200'></a>

Search Paths 

To minimize the total distance, we need some way to talk about the path that leads 
to the goal. But the functions we have defined so far only deal with individual states 
along the way. Representing paths would lead to another advantage: we could 
return the path as the solution, rather than just return the goal state. As it is, tri . 
only returns the goal state, not the path to it. So there is no way to determine what 
trip has done, except by reading the debugging output. 

The data structure path is designed to solve both these problems. A path has 
four fields: the current state, the previous partial path that this path is extending, 
the cost of the path so far, and an estimate of the total cost to reach the goal. Here is 
the structure definition for path. It uses the : pri nt-function option to say that all 
paths are to be printed with the function pr i nt - pa th, which will be defined below. 

(defstruct (path (:print-function print-path)) 
state (previous nil) (cost-so-far 0) (total-cost 0)) 

The next question is how to integrate paths into the searching routines with the 
least amount of disruption. Clearly, it would be better to make one change to 
tree-search rather than to change depth-first-search, breadth-first-search, 
and beam-search. However, looking back at the definition of tree-search, we see 
that it makes no assumptions about the structure of states, other than the fact that 
they can be manipulated by the goal predicate, successor, and combiner fimctions. 
This suggests that we can use tree-search unchanged if we pass it paths instead of 
states, and give it functions that can process paths. 

In the following redefinition of tri ., the beam- sea rch function is called with five 
arguments. Instead of passing it a city as the start state, we pass a path that has 
the city as its state field. The goal predicate should test whether its argument is a 
path whose state is the destination; we assume (and later define) a version of i s that 
accommodates this. The successor function is the most difficult. Instead of just 
generating a Ust of neighbors, we want to first generate the neighbors, then make 
each one into a path that extends the current path, but with an updated cost so far 
and total estimated cost. The function path - saver returns a function that will do just 
that. Finally, the cost function we are trying to minimize is path-total - cost, and 
we provide a beam width, which is now an optional argument to tri . that defaults 
to one: 

(defun trip (start dest &optional (beam-width 1)) 

"Search for the best path from the start to dest." 

(beam-search 

(make-path :state start) 

(is dest :key #*path-state) 

(path-saver #'neighbors #'air-distance 

<a id='page-201'></a>
#'(lambda (c) (air-distance c dest))) 
#*path-total-cost 
beam-width)) 

The calculation of ai r-di stance involves some complicated conversion of longitude 
and latitude to x-y-z coordinates. Since this is a problem in solid geometry, not AI, 
the code is presented without further comment: 

(defconstant earth-diameter 12765.0 
"Diameter of planet earth in kilometers.") 

(defun air-distance (cityl city2) 
"The great circle distance between two cities." 
(let ((d (distance (xyz-coords cityl) (xyz-coords city2)))) 

d is the straight-1ine chord between the two cities. 
;; The length of the subtending arc is given by: 
(* earth-diameter (asin (/ d 2))))) 

(defun xyz-coords (city) 
"Returns the x.y.z coordinates of a point on a sphere. 
The center is (0 0 0) and the north pole is (0 0 D." 
(let ((psi (deg->radians (city-lat city))) 

(phi (deg->radians (city-long city)))) 

(list (* (cos psi) (cos phi)) 
(* (cos psi) (sin phi)) 
(sin psi)))) 

(defun distance (pointl point2) 
"The Euclidean distance between two points. 
The points are coordinates in n-dimensional space." 
(sqrt (reduce #*+ (mapcar #'(lambda (a b) (expt (- a b) 2)) 

pointl point2)))) 

(defun deg->radians (deg) 
"Convert degrees and minutes to radians. " 
(* (+ (truncate deg) (* (rem deg 1) 100/60)) pi 1/180)) 

Before showing the auxiliary functions that implement this, here are some examples 
that show what it can do. With a beam width of 1, the detour to Flagstaff is eliminated, 
but the one to Chicago remains. With a beam width of 3, the correct optimal path is 
found. In the following examples, each call to the new version of t r i . returns a path, 
which is printed by s how- ci ty - pa th: 

> (show-city-path (trip (city 'san-francisco) (city 'boston) 1)) 
#<Path 4514.8 km: San-Francisco - Reno - Grand-Jet - Denver Kansas-
City - Indianapolis - Pittsburgh - Boston> 

<a id='page-202'></a>

> (show-city-path (trip (city 'boston) (city 'san-francisco) 1)) 
#<Path 4577.3 km: Boston - Pittsburgh - Chicago - Kansas-City Denver 
- Grand-Jet - Reno - San-Francisco> 

> (show-city-path (trip (city 'boston) (city 'san-francisco) 3)) 
#<Path 4514.8 km: Boston - Pittsburgh - Indianapolis Kansas-
City - Denver - Grand-Jet - Reno - San-Francisco> 

This example shows how search is susceptible to irregularities in the search space. It 
was easy to find the correct path from west to east, but the return trip required more 
search, because Flagstaff is a falsely promising step. In general, there may be even 
worse dead ends lurking in the search space. Look what happens when we limit the 
airplane's range to 700 kilometers. The map is shown in figure 6.2. 

Figure 6.2: A Map of Cities within 700km 

If we try to plan a trip from Tampa to Quebec, we can run into problems with 
the dead end at Wilmington, North Carolina. With a beam width of 1, the path to 
Jacksonville and then Wilmington will be tried first. From there, each step of the path 
alternates between Atlanta and Wilmington. The search never gets any closer to the 
goal. But with a beam width of 2, the path from Tampa to Atlanta is not discarded, 
and it is eventually continued on to Indianapolis and eventually to Quebec. So the 
capability to back up is essential in avoiding dead ends. 

Now for the implementation details. The function i s still returns a predicate that 
tests for a value, but now it accepts : key and : test keywords: 

<a id='page-203'></a>
(defun is (value &key (key #'identity) (test #'eql)) 
"Returns a predicate that tests for a given value." 
#'(lambda (path) (funcall test value (funcall key path)))) 

The path - saver function returns a function that will take a path as an argument and 
generate successors paths, path-saver takes as an argument a successor function 
that operates on bare states. It calls this function and, for each state returned, builds 
up a path that extends the existing path and stores the cost of the path so far as well 
as the estimated total cost: 

(defun path-saver (successors cost-fn cost-left-fn) 
#*(lambda (old-path) 
(let ((old-state (path-state old-path))) 
(mapcar 
#*(lambda (new-state) 
(let ((old-cost 
(+ (path-cost-so-far old-path) 
(funcall cost-fn old-state new-state)))) 

(make-path 
:state new-state 
rprevious old-path 
:cost-so-far old-cost 
:total-cost (+ old-cost (funcall cost-left-fn 

new-state))))) 
(funcall successors old-state))))) 

By default a path structure would be printed as #S (PATH ...). But because each path 
has a previ ous field that is filled by another path, this output would get quite verbose. 
That is why we installed pr i nt- pat h as the print function for paths when we defined 
the structure. It uses the notation #<...>, which is a Common Lisp convention for 
printing output that can not be reconstructed by read. The function show- ci ty - pa th 
prints a more complete representation of a path. We also define map-path to iterate 
over a path, collecting values: 

(defun print-path (path &optional (stream t) depth) 
(declare (ignore depth)) 
(format stream "#<Path to '"a cost ~.lf> " 

(path-state path) (path-total-cost path))) 

(defun show-city-path (path &optional (stream t)) 
"Show the length of a path, and the cities along it." 
(format stream "#<Path ~,lf km: "{^..^.^ -~}>" 

(path-total-cost path) 
(reverse (map-path #'city-name path))) 
(values)) 

<a id='page-204'></a>

(defun map-path (fn path) 

"Can fn on each state in the path, collecting results." 

(if (null path) 

nil 

(cons (funcall fn (path-state path)) 

(map-path fn (path-previous path))))) 

Guessing versus Guaranteeing a Good Solution 

Elementary AI textbooks place a great emphasis on search algorithms that are gusiranteed 
to find the best solution. However, in practice these algorithms are hardly 
ever used. The problem is that guaranteeing the best solution requires looking at a lot 
of other solutions in order to rule them out. For problems with large search spaces, 
this usually takes too much time. The alternative is to use an algorithm that will 
probably return a solution that is close to the best solution, but gives no guarantee. 
Such algorithms, traditionally known as non-admissible heuristic search algorithms, 
can be much faster. 

Of the algorithms we have seen so far, best-first search almost, but not quite, 
guarantees the best solution. The problem is that it terminates a little too early. 
Suppose it has calculated three paths, of cost 90, 95 and 110. It will expand the 90 
path next. Suppose this leads to a solution of total cost 100. Best-first search will 
then retimi that solution. But it is possible that the 95 path could lead to a solution 
with a total cost less than 100. Perhaps the 95 path is only one unit away from the 
goal, so it could result in a complete path of length 96. This means that an optimal 
search should examine the 95 path (but not the 110 path) before exiting. 

Depth-first seeu-ch and beam search, on the other hand, are definitely heuristic 
algorithms. Depth-first search finds a solution without any regard to its cost. With 
beam search, picking a good value for the beam width can lead to a good, quick 
solution, while picking the wrong value can lead to failure, or to a poor solution. 
One way out of this dilemma is to start with a narrow beam width, and if that does 
not lead to an acceptable solution, widen the beam and try again. We will call this 
iterative widening, although that is not a standard term. There are many variations on 
this theme, but here is a simple one: 

(defun iter-wide-search (start goal-p successors cost-fn 

&key (width 1) (max 100)) 

"Search, increasing beam width from width to max. 

Return the first solution found at any width." 

(dbg .-search "; Width: ~d" width) 

(unless (> width max) 

(or (beam-search start goal-p successors cost-fn width) 

(iter-wide-search start goal-p successors cost-fn 

<a id='page-205'></a>
:width (+ width 1) :max max)))) 

Here i ter-wide-search is used to search through a binary tree, failing with beam 
width 1 and 2, and eventually succeeding with beam width 3: 

> (iter-wide-search 1 (is 12) (finite-binary-tree 15) (diff 12)) 

Width: 1 

; Search: (1) 

; Search: (3) 

; Search: (7) 

: Search: (14) 

; Search: NIL 

Width: 2 

; Search: (1) 

; Search: (3 2) 

: Search: (7 6) 

: Search: (14 15) 

; Search: (15) 

: Search: NIL 

Width: 3 

; Search: (1) 

; Search: (3 2) 

; Search: (7 6 2) 

; Search: (14 15 6) 

; Search: (15 6) 

; Search: (6) 

; Search: (12 13) 

12 

The name iterative widening is derived from the established term iterative deepening. 
Iterative deepening is used to control depth-first search when we don't know the 
depth of the desired solution. The idea is first to limit the search to a depth of 1, 
then 2, and so on. That way we are guaranteed to find a solution at the minimum 
depth, just as in breadth-first search, but without wasting as much storage space. Of 
course, iterative deepening does waste some time because at each increasing depth 
it repeats all the work it did at the previous depth. But suppose that the average 
state has ten successors. That means that increasing the depth by one results in ten 
times more search, so only 10% of the time is wasted on repeated work. So iterative 
deepening uses only slightly more time and much less space. We will see it again in 
chapters 11 and 18. 

<a id='page-206'></a>

Searching Graphs 

So far, tree-search has been the workhorse behind all the searching routines. This 
is curious, when we consider that the city problem involves a graph that is not a tree 
at all. The reason tree - sea rch works is that any graph can be treated as a tree, if we 
ignore the fact that certain nodes are identical. For example, the graph in figure 6.3 
can be rendered as a tree. Figure 6.4 shows only the top four levels of the tree; each 
of the bottom nodes (except the 6s) needs to be expanded further. 

J L 

Figure 6.3: A Graph with Six Nodes 

In searching for paths through the graph of cities, we were implicitly turning the 
graph into a tree. That is, if tree - sea rch found two paths from Pittsburgh to Kansas 
City (via Chicago or Indianapolis), then it would treat them as two independent 
paths, just as if there were two distinct Kansas Cities. This made the algorithms 
simpler, but it also doubles the number of paths left to examine. If the destination is 
San Francisco, we will have to search for a path from Kansas City to San Francisco 
twice instead of once. In fact, even though the graph has only 22 cities, the tree is 
infinite, because we can go back and forth between adjacent cities any number of 
times. So, while it is possible to treat the graph as a tree, there are potential savings 
in treating it as a true graph. 

The function g raph- sea rch does just that. It is similar to tree - sea rch, but accepts 
two additional cirguments: a comparison function that tests if two states are equal, 
and a list of states that are no longer being considered, but were examined in the past. 
The difference between graph-search and tree -search is in the call to new-states, 
which generates successors but eliminates states that are in either the list of states 
currently being considered or the list of old states considered in the past. 

(defun graph-search (states goal-p successors combiner 
&optional (state= #'eql) old-states) 
"Find a state that satisfies goal-p. Start with states. 

<a id='page-207'></a>
Figure 6.4: The Corresponding Tree 

and search according to successors and combiner. 
Don't try the same state twice." 
(dbg :search ""&;; Search: '"a" states) 
(cond ((null states) fail) 

((funcall goal-p (first states)) (first states)) 
(t (graph-search 

(funcall 
combiner 
(new-states states successors state= old-states) 
(rest states)) 

goal-p successors combiner state= 
(adjoin (first states) old-states 
:test state=))))) 

(defun new-states (states successors state= old-states) 
"Generate successor states that have not been seen before." 
(remove-if 

#'(lambda (state) 
(or (member state states :test state=) 
(member state old-states :test state=^))) 
(funcall successors (first states)))) 

Using the successor function next2, we can search the graph shown here either as a 
tree or as a graph. If we search it as a graph, it takes fewer iterations and less storage 
space to find the goal. Of course, there is additional overhead to test for identical 

<a id='page-208'></a>

States, but on graphs Uke this one we get an exponential speed-up for a constant 
amount of overhead. 

(defun next2 (x) (list (+ . 1) (+ . 2))) 
> (tree-search '(1) (is 6) #'next2 #*prepend) 

Search: (1) 
Search: (2 3) 
Search: 4) 
Search: 4 5) 
Search: 5 4 5) 
Search: 4 5 5 6) 
Search: 5 5 6 5 6) 
Search: 5 6 5 6 6 7) 
Search: 6 5 6 6 7 5 6) 
Search: 5 6 6 7 5 6 6 7) 
Search: 6 6 7 5 6 6 7 6 7) 

> (graph-search '(1) (is 6) #'next2 #*prepend) 
Search: (1) 
Search: (2 3) 
Search: (3 4) 
Search: (4 5) 
Search: (5 6) 
Search: (6 7) 

The next step is to extend the graph-sea rch algorithm to handle paths. The complication 
is in deciding which path to keep when two paths reach the same state. If we 
have a cost function, then the answer is easy: keep the path with the cheaper cost. 
Best-first search of a graph removing duplicate states is called A * search. 

A* search is more complicated than graph-search because of the need both to 
add and to delete paths to the lists of current and old paths. For each new successor 
state, there are three possibilities. The new state may be in the list of current paths, in 
the Ust of old paths, or in neither. Within the first two cases, there are two subcases. 
If the new path is more expensive than the old one, then ignore the new path—it can 
not lead to a better solution. If the new path is cheaper than a corresponding path 
in the list of current paths, then replace it with the new path. If it is cheaper than a 
corresponding path in the list of the old paths, then remove that old path, and put 
the new path in the list of current paths. 

Also, rather than sort the paths by total cost on each iteration, they are kept sorted, 
and new paths are inserted into the proper place one at a time using i nsert-path. 
Two more functions, better-path and find-path, are used to compare paths and 
see if a state has already appeared. 

<a id='page-209'></a>
(defun a*-search (paths goal-p successors cost-fn cost-left-fn 

&optional (state= #'eql) old-paths) 
"Find a path whose state satisfies goal-p. Start with paths, 
and expand successors, exploring least cost first. 
When there are duplicate states, keep the one with the 
lower cost and discard the other." 
(dbg :search ";; Search: ~a" paths) 
(cond 

((null paths) fail) 
((funcall goal-p (path-state (first paths))) 
(values (first paths) paths)) 
(t (let* ((path (pop paths)) 
(state (path-state path))) 
;; Update PATHS and OLD-PATHS to reflect 

the new successors of STATE: 
(setf old-paths (insert-path path old-paths)) 
(dolist (state2 (funcall successors state)) 

(let* ((cost (+ (path-cost-so-far path) 

(funcall cost-fn state state2))) 
(cost2 (funcall cost-left-fn state2)) 
(path2 (make-path 

:state state2 :previous path 
:cost-so-far cost 
:total-cost (+ cost cost2))) 

(old nil) 
Place the new path, path2, in the right list: 
(cond 
((setf old (find-path state2 paths state=)) 
(when (better-path path2 old) 
(setf paths (insert-path 
path2 (delete old paths))))) 
((setf old (find-path state2 old-paths state=)) 

(when (better-path path2 old) 
(setf paths (insert-path path2 paths)) 
(setf old-paths (delete old old-paths)))) 

(t (setf paths (insert-path path2 paths)))))) 
Finally, call A* again with the updated path lists: 
(a*-search paths goal-p successors cost-fn cost-left-fn 
state= old-paths))))) 

<a id='page-210'></a>

Here are the three auxiliary functions: 

(defun find-path (state paths state=) 
"Find the path with this state among a list of paths." 
(find state paths :key #'path-state :test state=)) 

(defun better-path (pathl path2) 
"Is pathl cheaper than path2?" 
(< (path-total-cost pathl) (path-total-cost path2))) 

(defun insert-path (path paths) 
"Put path into the right position, sorted by total cost." 
MERGE is a built-in function 
(merge 'list (list path) paths #'< :key #'path-total-cost)) 

(defun path-states (path) 
"Collect the states along this path." 
(if (null path) 

nil 
(cons (path-state path) 
(path-states (path-previous path))))) 

Below we use a*-search to search for 6 in the graph previously shown in figure 6.3. 
The cost function is a constant 1 for each step. In other words, the total cost is the 
length of the path. The heuristic evaluation function is just the difference from the 
goal. The A* algorithm needs just three search steps to come up with the optimal 
solution. Contrast that to the graph search algorithm, which needed five steps, and 
the tree search algorithm, which needed ten steps—and neither of them found the 
optimal solution. 

> (path-states 
(a*-search (list (make-path :state D) (is 6) 

#'next2 #'(lambda (x y) 1) (diff 6))) 
Search: (#<Path to 1 cost 0.0>) 
Search: (#<Path to 3 cost 4.0> #<Path to 2 cost 5.0>) 
Search: (#<Path to 5 cost 3.0> #<Path to 4 cost 4.0> 

#<Path to 2 cost 5.0>) 
Search: (#<Path to 6 cost 3.0> #<Path to 7 cost 4.0> 
#<Path to 4 cost 4.0> #<Path to 2 cost 5.0>) 
(6 5 3 1) 

It may seem limiting that these search functions all return a single answer. In some 
applications, we may want to look at several solutions, or at all possible solutions. 
Other applications are more naturally seen as optimization problems, where we 
don't know ahead of time what counts as achieving the goal but are just trying to find 
some action with a low cost. 

<a id='page-211'></a>
It turns out that the functions we have defined are not Umiting at all in this respect. 
They can be used to serve both these new purposes—provided we carefully specify 
the goal predicate. To find all solutions to a problem, all we have to do is pass in a 
goal predicate that always fails, but saves all the solutions in a list. The goal predicate 
will see all possible solutions and save away just the ones that are real solutions. 
Of course, if the search space is infinite this will never terminate, so the user has 
to be careful in applying this technique. It would also be possible to write a goal 
predicate that stopped the search after finding a certain number of solutions, or after 
looking at a certain number of states. Here is a function that finds all solutions, using 
beam search: 

(defun search-all (start goal-p successors cost-fn beam-width) 
"Find all solutions to a search problem, using beam search." 

Be careful: this can lead to an infinite loop, 
(let ((solutions nil)) 
(beam-search 
start #'(lambda (x) 

(when (funcall goal-p x) (push . solutions)) 
nil) 
successors cost-fn beam-width) 
solutions)) 

6.5 GPS as Search 
The GPS program can be seen as a problem in search. For example, in the three-block 
blocks world, there are only 13 different states. They could be arranged in a graph and 
searched just as we searched for a route between cities. Figure 6.5 shows this graph. 

The function search-gps does just that. Like the gps function on [page 135](chapter4.md#page-135), it 
computes a final state and then picks out the actions that lead to that state. But 
it computes the state with a beam search. The goal predicate tests if the current 
state satisfies every condition in the goal, the successor function finds all applicable 
operators and applies them, and the cost function simply sums the number of actions 
taken so far, plus the number of conditions that are not yet satisfied: 

<a id='page-212'></a>

. 
I. 
. 
. 
.. . 
. C 
. 
Figure 6.5: The Blocks World as a Graph 
(defun search-gps (start goal &optional (beam-width 10)) 
"Search for a sequence of operators leading to goal." 
(find-all-i f 
#*action-p 
(beam-search 
(cons '(start) start) 
#'(lambda (state) (subsetp goal state :test #*equal)) 
#'gps-successors 
#'(lambda (state) 
(+ (count-if #'action-p state) 
(count-if #'(lambda (con) 
(not (member-equal con state))) 
goal))) 
beam-width))) 
Here is the successor function: 
(defun gps-successors (state) 
"Return a lis t of states reachable from this one using ops." 
(mapcar 
#.(lambda (op) 

<a id='page-213'></a>
(append 
(remove-if #'(lambda (x) 
(member-equal . (op-del-list op))) 
state) 
(op-add-list op))) 
(applicable-ops state))) 

(defun applicable-ops (state) 
"Return a list of all ops that are applicable now." 
(find-all-if 

#'(lambda (op) 
(subsetp (op-preconds op) state :test #'equal)) 
*ops*)) 

The search technique finds good solutions quickly for a variety of problems. Here 
we see the solution to the Sussman anomaly in the three-block blocks world: 

(setf start '((c on a) (a on table) (b on table) (space on c) 
(space on b) (space on table))) 

> (search-gps start '((a on b) (b on c))) 

((START) 
(EXECUTING (MOVE C FROM A TO TABLE)) 
(EXECUTING (MOVE . FROM TABLE TO O ) 
(EXECUTING (MOVE A FROM TABLE TO B))) 

> (search-gps start '((b on c) (a on b))) 

((START) 
(EXECUTING (MOVE C FROM A TO TABLE)) 
(EXECUTING (MOVE . FROM TABLE TO O ) 
(EXECUTING (MOVE A FROM TABLE TO B))) 

In these solutions we search forward from the start to the goal; this is quite different 
from the means-ends approach of searching backward from the goal for an appropriate 
operator. But we could formulate means-ends analysis as forward search simply 
by reversing start and goal: GPS's goal state is the search's start state, and the search's 
goal predicate tests to see if a state matches GPS's start state. This is left as an exercise. 

6.6 History and References 
Pattern matching is one of the most important tools for AI. As such, it is covered 
in most textbooks on Lisp. Good treatments include Abelson and Sussman 
(1984), Wilensky (1986), Winston and Horn (1988), and Kreutzer and McKenzie 
(1990). An overview is presented in the "pattern-matching" entry in Encyclopedia of 
.. (Shapiro 1990). 

<a id='page-214'></a>

Nilsson's Problem-Solving Methods in Artificial Intelligence (1971) was an early textbook 
that emphasized search as the most important defining characteristic of AI. 
More recent texts give less importance to search; Winston's Artificial Intelligence 
(1984) gives a balanced overview, and his Lisp (1988) provides implementations of 
some of the algorithms. They are at a lower level of abstraction than the ones in 
this chapter. Iterative deepening was first presented by Korf (1985), and iterative 
broadening by Ginsberg and Harvey (1990). 
6.7 Exercises 
&#9635; Exercise 6.3 [m] Write a version of i . te ra et i ve -i nterpreter that is more general 
than the one defined in this chapter. Decide what features can be specified, and 
provide defaults for them. 

&#9635; Exercise 6.4 [m] Define a version of compose that allows any number of arguments, 
not just two. Hint: You may want to use the function reduce. 

&#9635; Exercise 6.5 [m] Define a version of compose that allows any number of arguments 
but is more efficient than the answer to the previous exercise. Hint: try to make 
decisions when compose is called to build the resulting function, rather than making 
the same decisions over and over each time the resulting function is called. 

&#9635; Exercise 6.6 [m] One problem with pat-match is that it gives special significance 
to symbols starting with ?, which means that they can not be used to match a literal 
pattern. Define a pattern that matches the input literally, so that such symbols can 
be matched. 

&#9635; Exercise 6.7 [m] Discuss the pros and cons of data-driven programming compared 
to the conventional approach. 

&#9635; Exercise 6.8 [m]
recursion. 
Write a version of tree-searc h using an explicit loop rather than 

&#9635; Exercise 6.9 [m] The sorte r function is inefficient for two reasons: it calls append, 
which has to make a copy of the first argument, and it sorts the entire result, rather 
than just inserting the new states into the already sorted old states. Write a more 
efficient sorter . 

<a id='page-215'></a>

&#9635; Exercise 6.10 [m] Write versions of graph-search and a*-search that use hash 
tables rather than lists to test whether a state has been seen before. 

&#9635; Exercise 6.11 [m] Write a function that calls beam-searchtofindthefirstnsolution s 
to a problem and returns them in a list. 

&#9635; Exercise 6.12 [m] On personal computers without floating-point hardware, the 
ai r-di stanc e calculation will be rather slow. If this is a problem for you, arrange 
to compute the xyz-coords of each city only once and then store them, or store 
a complete table of air distances between cities. Also precompute and store the 
neighbors of each city. 

&#9635; Exercise 6.13 [d] Write a version of GPS that uses A* search instead of beam search. 
Compare the two versions in a variety of domains. 

&#9635; Exercise 6.14 [d] Write a version of GPS that allows costs for each operator. For 
example, driving the child to school might have a cost of 2, but calling a limousine 
to transport the child might have a cost of 100. Use these costs instead of a constant 
cost of 1 for each operation. 

&#9635; Exercise 6.15 [d] Write a version of GPS that uses the searching tools but does 
means-ends analysis. 
6.8 Answers 
Answer 6.2 Unfortunately, pat -match does not always find the answer. The problem 
is that it will only rebind a segment variable based on a failure to match the 
rest of the pattern after the segment variable. In all the examples above, the "rest of 
the pattern after the segment variable" was the whole pattern, so pat-match always 
worked properly. But if a segment variable appears nested inside a list, then the rest 
of the segment variable's sublist is only a part of the rest of the whole pattern, as the 
following example shows: 
> (pat-match '(((? * ?x) (?* ?y)) ?x ?y) 
'(( a b c d ) (a b) (c d))) ^ NIL 

The correct answer with ?x bound to (a b) and ?y bound to (c d) is not found 
because the inner segment match succeeds with ?x bound to () and ?y bound to (a 

<a id='page-216'></a>

bed), and once we leave the inner match and return to the top level, there is no 
going back for alternative bindings. 

Answer 6.3 The following version lets the user specify all four components of the 
prompt-read-eval-print loop, as well as the streams to use for input and output. 
Defaults are set up as for a Lisp interpreter. 

(defun interactive-interpreter 
(&key (read #'read) (eval #'eval) (print #*print) 

(prompt "> ") (input t) (output t)) 
"Read an expression, evaluate it, and print the result." 
(loop 

(fresh-line output) 
(princ prompt output) 
(funcall print (funcall eval (funcall read input)) 
output))) 

Here is another version that does all of the above and also handles multiple values 
and binds the various "history variables" that the Lisp top-level binds. 

(defun interactive-interpreter 
(&key (read #'read) (eval #'eval) (print #'print) 

(prompt "> ") (input t) (output t)) 
"Read an expression, evaluate it, and print the result(s). 
Does multiple values and binds: ***'''-++++++/// ///" 
(let ('**'*'-++++++/ // /// vals) 

The above variables are all special, except VALS 
The variable - holds the current input 

' ape the 3 most recent values 
+ ++ +++ are the 3 most recent inputs 
/ // /// are the 3 most recent lists of multiple-values 
(loop 
(fresh-line output) 
(princ prompt output) 

First read and evaluate an expression 
(setf - (funcall read input) 
vals (multiple-value-list (funcall eval -))) 
Now update the history variables 

(setf +++ ++ /// // *** (first ///) 
++ + // / (first //) 
+ -/ vals * (first /)) 

Finally print the computed value(s) 
(dolist (value vals) 
(funcall print value output))))) 

<a id='page-217'></a>

Answer 6.4 

(defun compose (&rest functions) 
"Return the function that is the composition of all the args. 

i.e. (compose f g h) = (lambda (x) (f (g (h x))))." 
#*(lambda (x) 
(reduce #'funcan functions :from-end t .-initial-value x))) 

Answer 6.5 

(defun compose (&rest functions) 
"Return the function that is the composition of all the args. 

i.e. (compose f g h) = (lambda (x) (f (g (h x))))." 
(case (length functions) 
(0 #'identity) 
(1 (first functions)) 
(2 (let ((f (first functions)) 

(g (second functions))) 
#'(lambda (x) (funcall f (funcall g x))))) 
(t #*(lambda (x) 
(reduce #'funcall functions :from-end t 
:initial-value x))))) 

Answer 6.8 

(defun tree-search (states goal-p successors combiner) 
"Find a state that satisfies goal-p. Start with states, 
and search according to successors and combiner." 
(loop 

(cond ((null states) (RETURN fail)) 
((funcall goal-p (first states)) 
(RETURN (first states)) 
(t (setf states 

(funcall combiner 
(funcall successors (first states)) 
(rest states)))))))) 

Answer 6.9 

(defun sorter (cost-fn) 
"Return a combiner function that sorts according to cost-fn." 
#'(lambda (new old) 

(merge 'list (sort new #'> :key cost-fn) 
old #'> :key cost-fn))) 

<a id='page-218'></a>

Answer 6.11 

(defun search-n (start . goal-p successors cost-fn beam-width) 
"Find . solutions to a search problem, using beam search." 
(let ((solutions nil)) 

(beam-search 
start #*(lambda (x) 

(cond ((not (funcall goal-p x)) nil) 
((= . 0) X) 
(t (decf n) 

(push X solutions) 
nil))) 
successors cost-fn beam-width) 
solutions)) 

