# Chapter 6
## Building Software Tools

> *Man is a tool-using animal…Without tools he is nothing with tools he is all.*

> –Thomas Carlyle (1795–1881)

In [chapters 4](B9780080571157500042.xhtml) and [5](B9780080571157500054.xhtml) we were concerned with building two particular programs, GPS !!!(span) {:.smallcaps} and ELIZA !!!(span) {:.smallcaps} In this chapter, we will reexamine those two programs to discover some common patterns.
Those patterns will be abstracted out to form reusable software tools that will prove helpful in subsequent chapters.

## [ ](#){:#st0010}6.1 An Interactive Interpreter Tool
{:#s0010}
{:.h1hd}

The structure of the function `eliza` is a common one.
It is repeated below:

[ ](#){:#l0010}`(defun eliza ()`
!!!(p) {:.unnumlist}

` "Respond to user input using pattern matching rules."`
!!!(p) {:.unnumlist}

` (loop`
!!!(p) {:.unnumlist}

`  (print 'eliza >)`
!!!(p) {:.unnumlist}

`  (print (flatten (use-eliza-rules (read))))))`
!!!(p) {:.unnumlist}

Many other applications use this pattern, including Lisp itself.
The top level of Lisp could be defined as:

[ ](#){:#l0015}`(defun lisp ()`
!!!(p) {:.unnumlist}

` (loop`
!!!(p) {:.unnumlist}

`  (print '>)`
!!!(p) {:.unnumlist}

`  (print (eval (read)))))`
!!!(p) {:.unnumlist}

The top level of a Lisp system has historically been called the “read-eval-print loop.” Most modern Lisps print a prompt before reading input, so it should really be called the “prompt-read-eval-print loop,” but there was no prompt in some early systems like MacLisp, so the shorter name stuck.
If we left out the prompt, we could write a complete Lisp interpreter using just four symbols:

[ ](#){:#l0020}`(loop (print (eval (read))))`
!!!(p) {:.unnumlist}

It may seem facetious to say those four symbols and eight parentheses constitute a Lisp interpreter.
When we write that line, have we really accomplished anything?
One answer to that question is to consider what we would have to do to write a Lisp (or Pascal) interpreter in Pascal.
We would need a lexical analyzer and a symbol table manager.
This is a considerable amount of work, but it is all handled by read.
We would need a syntactic parser to assemble the lexical tokens into statements.
read also handles this, but only because Lisp statements have trivial syntax: the syntax of lists and atoms.
Thus read serves fine as a syntactic parser for Lisp, but would fail for Pascal.
Next, we need the evaluation or interpretation part of the interpreter; eval does this nicely, and could handle Pascal just as well if we parsed Pascal syntax into Lisp expressions, `print` does much less work than `read` or `eval`, but is still quite handy.

The important point is not whether one line of code can be considered an implementation of Lisp; it is to recognize common patterns of computation.
Both `eliza` and `lisp` can be seen as interactive interpreters that read some input, transform or evaluate the input in some way, print the result, and then go back for more input.
We can extract the following common pattern:

[ ](#){:#l0025}`(defun *program* ()`
!!!(p) {:.unnumlist}

` (loop`
!!!(p) {:.unnumlist}

`  (print *prompt*)`
!!!(p) {:.unnumlist}

`  (print (*transform* (read)))))`
!!!(p) {:.unnumlist}

There are two ways to make use of recurring patterns like this: formally and informally.
The informal alternative is to treat the pattern as a cliche or idiom that will occur frequently in our writing of programs but will vary from use to use.
When we want to write a new program, we remember writing or reading a similar one, go back and look at the first program, copy the relevant sections, and then modify them for the new program.
If the borrowing is extensive, it would be good practice to insert a comment in the new program citing the original, but there would be no “official” connection between the original and the derived program.

The formal alternative is to create an abstraction, in the form of functions and perhaps data structures, and refer explicitly to that abstraction in each new application—in other words, to capture the abstraction in the form of a useable software tool.
The interpreter pattern could be abstracted into a function as follows:

[ ](#){:#l0030}`(defun interactive-interpreter (prompt transformer)`
!!!(p) {:.unnumlist}

` "Read an expression, transform it, and print the result."`
!!!(p) {:.unnumlist}

` (loop`
!!!(p) {:.unnumlist}

`  (print prompt)`
!!!(p) {:.unnumlist}

`  (print (funcall transformer (read)))))`
!!!(p) {:.unnumlist}

This function could then be used in writing each new interpreter:

[ ](#){:#l0035}`(defun lisp ()`
!!!(p) {:.unnumlist}

` (interactive-interpreter '> #'eval))`
!!!(p) {:.unnumlist}

`(defun eliza ()`
!!!(p) {:.unnumlist}

` (interactive-interpreter 'eliza >`
!!!(p) {:.unnumlist}

`  #'(lambda (x) (flatten (use-eliza-rules x)))))`
!!!(p) {:.unnumlist}

Or, with the help of the higher-order function compose:

[ ](#){:#l0040}`(defun compose (f g)`
!!!(p) {:.unnumlist}

` "Return the function that computes (f (g x))."`
!!!(p) {:.unnumlist}

` #'(lambda (x) (funcall f (funcall g x))))`
!!!(p) {:.unnumlist}

`(defun eliza ()`
!!!(p) {:.unnumlist}

` (interactive-interpreter 'eliza >`
!!!(p) {:.unnumlist}

`  (compose #'flatten #'use-eliza-rules)))`
!!!(p) {:.unnumlist}

There are two differences between the formal and informal approaches.
First, they look different.
If the abstraction is a simple one, as this one is, then it is probably easier to read an expression that has the loop explicitly written out than to read one that calls `interactive-interpreter`, since that requires finding the definition of `interactive-interpreter` and understanding it as well.

The other difference shows up in what's called *maintenance*.
Suppose we find a missing feature in the definition of the interactive interpreter.
One such omission is that the `loop` has no exit.
I have been assuming that the user can terminate the loop by hitting some interrupt (or break, or abort) key.
A cleaner implementation would allow the user to give the interpreter an explicit termination command.
Another useful feature would be to handle errors within the interpreter.
If we use the informal approach, then adding such a feature to one program would have no effect on the others.
But if we use the formal approach, then improving `interactive-interpreter` would automatically bring the new features to all the programs that use it.

The following version of `interactive-interpreter` adds two new features.
First, it uses the macro `handler-case`[1](#fn0015){:#xfn0015} to handle errors.
This macro evaluates its first argument, and normally just returns that value.
However, if an error occurs, the subsequent arguments are checked for an error condition that matches the error that occurred.
In this use, the case `error` matches all errors, and the action taken is to prints the error condition and continue.

This version also allows the prompt to be either a string or a function of no arguments that will be called to print the prompt.
The function `prompt-generator`, for example, returns a function that will print prompts of the form [1], [2], and so forth.

[ ](#){:#l0045}`(defun interactive-interpreter (prompt transformer)`
!!!(p) {:.unnumlist}

` "Read an expression, transform it, and print the result."`
!!!(p) {:.unnumlist}

` (loop`
!!!(p) {:.unnumlist}

`  (handler-case`
!!!(p) {:.unnumlist}

`   (progn`
!!!(p) {:.unnumlist}

`    (if (stringp prompt)`
!!!(p) {:.unnumlist}

`     (print prompt)`
!!!(p) {:.unnumlist}

`     (funcall prompt))`
!!!(p) {:.unnumlist}

`    (print (funcall transformer (read))))`
!!!(p) {:.unnumlist}

`   ;; In case of error.
do this:`
!!!(p) {:.unnumlist}

`   (error (condition)`
!!!(p) {:.unnumlist}

`    (format t "~&;; Error ~a ignored, back to top level."`
!!!(p) {:.unnumlist}

`     condition)))))`
!!!(p) {:.unnumlist}

`(defun prompt-generator (&optional (num 0) (ctl-string "[~d] "))`
!!!(p) {:.unnumlist}

` "Return a function that prints prompts like [l], [2], etc."`
!!!(p) {:.unnumlist}

` #'(lambda () (format t ctl-string (incf num))))`
!!!(p) {:.unnumlist}

## [ ](#){:#st0015}6.2 A Pattern-Matching Tool
{:#s0015}
{:.h1hd}

The `pat-match` function was a pattern matcher defined specifically for the ELIZA !!!(span) {:.smallcaps} program.
Subsequent programs will need pattern matchers too, and rather than write specialized matchers for each new program, it is easier to define one general pattern matcher that can serve most needs, and is extensible in case novel needs come up.

The problem in designing a “general” tool is deciding what features to provide.
We can try to define features that might be useful, but it is also a good idea to make the list of features open-ended, so that new ones can be easily added when needed.

Features can be added by generalizing or specializing existing ones.
For example, we provide segment variables that match zero or more input elements.
We can specialize this by providing for a kind of segment variable that matches one or more elements, or for an optional variable that matches zero or one element.
Another possibility is to generalize segment variables to specify a match of *m* to *n* elements, for any specified *m* and *n*.
These ideas come from experience with notations for writing regular expressions, as well as from very general heuristics for generalization, such as “consider important special cases” and “zero and one are likely to be important special cases.”

Another useful feature is to allow the user to specify an arbitrary predicate that a match must satisfy.
The notation `(?is ?n numberp)` could be used to match any expression that is a number and bind it to the variable `?n`.
This would look like:

[ ](#){:#l0050}`> (pat-match '(x = (?is ?n numberp)) '(x = 34)) ⇒ ((?n .
34))`
!!!(p) {:.unnumlist}

`> (pat-match '(x = (?is ?n numberp)) '(x = x)) ⇒ NIL`
!!!(p) {:.unnumlist}

Since patterns are like boolean expressions, it makes sense to allow boolean operators on them.
Following the question-mark convention, we will use `?and, ?or` and `?not` for the operators.[2](#fn0020){:#xfn0020} Here is a pattern to match a relational expression with one of three relations.
It succeeds because the < matches one of the three possibilities specified by `(?or < = >).`

[ ](#){:#l0055}`> (pat-match '(?x (?or < = >) ?y) '(3 < 4)) ⇒ ((?Y .
4) (?X .
3))`
!!!(p) {:.unnumlist}

Here is an example of an `?and` pattern that checks if an expression is both a number and odd:

[ ](#){:#t0010}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `> (pat-match` | `'(x = (?and (?is ?n numberp) (?is ?n oddp)))` |
| | `'(x = 3))` |
| `((?N . 3))` | |

The next pattern uses `?not` to insure that two parts are not equal:

[ ](#){:#l0060}`> (pat-match '(?x /= (?not ?x)) '(3 /= 4)) ⇒ ((?X .
3))`
!!!(p) {:.unnumlist}

The segment matching notation we have seen before.
It is augmented to allow for three possibilities: zero or more expressions; one or more expressions; and zero or one expressions.
Finally, the notation `(?if *exp*)` can be used to test a relationship between several variables.
It has to be listed as a segment pattern rather than a single pattern because it does not consume any of the input at all:

[ ](#){:#l0065}`> (pat-match '(?x > ?y (?if (> ?x ?y))) '(4 > 3)) ⇒`
!!!(p) {:.unnumlist}

`((?Y .
3) (?X .
4))`
!!!(p) {:.unnumlist}

When the description of a problem gets this complicated, it is a good idea to attempt a more formal specification.
The following table describes a grammar of patterns, using the same grammar rule format described in [chapter 2](B9780080571157500029.xhtml).

[ ](#){:#t0015}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| *pat*⇒ | *var* | match any one expression |
| *Constant* | match just this atom |
| *segment*-*pat* | match something against a sequence |
| *single*-*pat* | match something against one expression |
| (*pat*. *pat*) | match the first and the rest |
| *single*-*pat*⇒ | (?`is`*var predicate*) | test predicate on one expression |
| (?`or`*pat*…) | match any pattern on one expression |
| (?`and`*pat*…) | match every pattern on one expression |
| (?`not`*pat*…) | succeed if pattern(s) do not match |
| *segment*-*pat*⇒ | ( (?* *var*)…) | match zero or more expressions |
| ( (?+ *var*) … ) | match one or more expressions |
| ( ( ?? *var*) … ) | match zero or one expression |
| ( ( ?`if`*exp* )…) | test if exp (which may contain variables) is true |
| *Var*⇒ | ?*chars* | a symbol starting with ? |
| *constant*⇒ | *atom* | any nonvariable atom |

![t0015](images/B9780080571157500066/t0015.png)

Despite the added complexity, all patterns can still be classified into five cases.
The pattern must be either a variable, constant, a (generalized) segment pattern, a (generalized) single-element pattern, or a cons of two patterns.
The following definition of `pat-match` reflects the five cases (along with two checks for failure):

[ ](#){:#l0070}`(defun pat-match (pattern input &optional (bindings no-bindings))`
!!!(p) {:.unnumlist}

` "Match pattern against input in the context of the bindings"`
!!!(p) {:.unnumlist}

` (cond ((eq bindings fail) fail)`
!!!(p) {:.unnumlist}

`  ((variable-p pattern)`
!!!(p) {:.unnumlist}

`   (match-variable pattern input bindings))`
!!!(p) {:.unnumlist}

`  ((eq1 pattern input) bindings)`
!!!(p) {:.unnumlist}

`  ((segment-pattern-p pattern)`
!!!(p) {:.unnumlist}

`   (segment-matcher pattern input bindings))`
!!!(p) {:.unnumlist}

`  ((single-pattern-p pattern) ; ***`
!!!(p) {:.unnumlist}

`   (single-matcher pattern input bindings)) ; ***`
!!!(p) {:.unnumlist}

`  ((and (consp pattern) (consp input))`
!!!(p) {:.unnumlist}

`   (pat-match (rest pattern) (rest input)`
!!!(p) {:.unnumlist}

`      (pat-match (first pattern) (first input)`
!!!(p) {:.unnumlist}

`        bindings)))`
!!!(p) {:.unnumlist}

`  (t fail)))`
!!!(p) {:.unnumlist}

For completeness, we repeat here the necessary constants and low-level functions from ELIZA !!!(span) {:.smallcaps} :

[ ](#){:#l0075}`(defconstant fail nil "Indicates pat-match failure")`
!!!(p) {:.unnumlist}

`(defconstant no-bindings '((t .
t))`
!!!(p) {:.unnumlist}

` "Indicates pat-match success, with no variables.")`
!!!(p) {:.unnumlist}

`(defun variable-p (x)`
!!!(p) {:.unnumlist}

` "Is x a variable (a symbol beginning with '?')?"`
!!!(p) {:.unnumlist}

` (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))`
!!!(p) {:.unnumlist}

`(defun get-binding (var bindings)`
!!!(p) {:.unnumlist}

` "Find a (variable .
value) pair in a binding list."`
!!!(p) {:.unnumlist}

` (assoc var bindings))`
!!!(p) {:.unnumlist}

`(defun binding-var (binding)`
!!!(p) {:.unnumlist}

` "Get the variable part of a single binding."`
!!!(p) {:.unnumlist}

` (car binding))`
!!!(p) {:.unnumlist}

`(defun binding-val (binding)`
!!!(p) {:.unnumlist}

` "Get the value part of a single binding."`
!!!(p) {:.unnumlist}

` (cdr binding))`
!!!(p) {:.unnumlist}

`(defun make-binding (var val) (cons var val))`
!!!(p) {:.unnumlist}

`(defun lookup (var bindings)`
!!!(p) {:.unnumlist}

` "Get the value part (for var) from a binding list."`
!!!(p) {:.unnumlist}

` (binding-val (get-binding var bindings)))`
!!!(p) {:.unnumlist}

`(defun extend-bindings (var val bindings)`
!!!(p) {:.unnumlist}

` "Add a (var .
value) pair to a binding list."`
!!!(p) {:.unnumlist}

` (cons (make-binding var val)`
!!!(p) {:.unnumlist}

`  ;; Once we add a "real" binding,`
!!!(p) {:.unnumlist}

`  ;; we can get rid of the dumrny no-bindings`
!!!(p) {:.unnumlist}

`  (if (eq bindings no-bindings)`
!!!(p) {:.unnumlist}

`   nil`
!!!(p) {:.unnumlist}

`   bindings)`
!!!(p) {:.unnumlist}

`(defun match-variable (var input bindings)`
!!!(p) {:.unnumlist}

` "Does VAR match input?
Uses (or updates) and returns bindings."`
!!!(p) {:.unnumlist}

` (let ((binding (get-binding var bindings)))`
!!!(p) {:.unnumlist}

`  (cond ((not binding) (extend-bindings var input bindings))`
!!!(p) {:.unnumlist}

`   ((equal input (binding-val binding)) bindings)`
!!!(p) {:.unnumlist}

`   (t fail))))`
!!!(p) {:.unnumlist}

The next step is to define the predicates that recognize generalized segment and single-element patterns, and the matching functions that operate on them.
We could implement `segment-matcher` and `single-matcher` with case statements that consider all possible cases.
However, that would make it difficult to extend the matcher.
A programmer who wanted to add a new kind of segment pattern would have to edit the definitions of both `segment-pattern-p` and `segment-matcher` to install the new feature.
This by itself may not be too bad, but consider what happens when two programmers each add independent features.
If you want to use both, then neither version of `segment-matcher` (or `segment-pattern-p`) will do.
You'll have to edit the functions again, just to merge the two extensions.

The solution to this dilemma is to write one version of `segment-pattern-p` and `segment-matcher`, once and for all, but to have these functions refer to a table of pattern/action pairs.
The table would say “if you see ?* in the pattern, then use the function `segment-match`,” and so on.
Then programmers who want to extend the matcher just add entries to the table, and it is trivial to merge different extensions (unless of course two programmers have chosen the same symbol to mark different actions).

This style of programming, where pattern/action pairs are stored in a table, is called *data*-*driven programming*.
It is a very flexible style that is appropriate for writing extensible systems.

There are many ways to implement tables in Common Lisp, as discussed in [section 3.6](B9780080571157500030.xhtml#s0080), [page 73](B9780080571157500030.xhtml#p73).
In this case, the keys to the table will be symbols (like ?*), and it is fine if the representation of the table is distributed across memory.
Thus, property lists are an appropriate choice.
We will have two tables, represented by the `segment-match` property and the `single-match` property of symbols like ?*.
The value of each property will be the name of a function that implements the match.
Here are the table entries to implement the grammar listed previously:

[ ](#){:#l0080}`(setf (get '?is 'single-match) 'match-is)`
!!!(p) {:.unnumlist}

`(setf (get '?or 'single-match) 'match-or)`
!!!(p) {:.unnumlist}

`(setf (get '?and 'single-match) 'match-and)`
!!!(p) {:.unnumlist}

`(setf (get '?not 'single-match) 'match-not)`
!!!(p) {:.unnumlist}

`(setf (get '?* 'segment-match) 'segment-match)`
!!!(p) {:.unnumlist}

`(setf (get '?+ 'segment-match) 'segment-match +)`
!!!(p) {:.unnumlist}

`(setf (get '??
'segment-match) 'segment-match?)`
!!!(p) {:.unnumlist}

`(setf (get '?if 'segment-match) 'match-if)`
!!!(p) {:.unnumlist}

With the table defined, we need to do two things.
First, define the “glue” that holds the table together: the predicates and action-taking functions.
A function that looks up a data-driven function and calls it (such as `segment-matcher` and `single-matcher`) is called a *dispatch function*.

[ ](#){:#l0085}`(defun segment-pattern-p (pattern)`
!!!(p) {:.unnumlist}

` "Is this a segment-matching pattern like ((?* var) .
pat)?"`
!!!(p) {:.unnumlist}

` (and (consp pattern) (consp (first pattern))`
!!!(p) {:.unnumlist}

`  (symbolp (first (first pattern)))`
!!!(p) {:.unnumlist}

`  (segment-match-fn (first (first pattern)))))`
!!!(p) {:.unnumlist}

`(defun single-pattern-p (pattern)`
!!!(p) {:.unnumlist}

` "Is this a single-matching pattern?`
!!!(p) {:.unnumlist}

` E.g.
(?is x predicate) (?and .
patterns) (?or .
patterns)."`
!!!(p) {:.unnumlist}

` (and (consp pattern)`
!!!(p) {:.unnumlist}

`   (single-match-fn (first pattern))))`
!!!(p) {:.unnumlist}

`(defun segment-matcher (pattern input bindings)`
!!!(p) {:.unnumlist}

` "Call the right function for this kind of segment pattern."`
!!!(p) {:.unnumlist}

` (funcall (segment-match-fn (first (first pattern)))`
!!!(p) {:.unnumlist}

`    pattern input bindings))`
!!!(p) {:.unnumlist}

`(defun single-matcher (pattern input bindings)`
!!!(p) {:.unnumlist}

` "Call the right function for this kind of single pattern."`
!!!(p) {:.unnumlist}

` (funcall (single-match-fn (first pattern))`
!!!(p) {:.unnumlist}

`    (rest pattern) input bindings))`
!!!(p) {:.unnumlist}

`(defun segment-match-fn (x)`
!!!(p) {:.unnumlist}

` "Get the segment-match function for x,`
!!!(p) {:.unnumlist}

` if it is a symbol that has one."`
!!!(p) {:.unnumlist}

` (when (symbolp x) (get x 'segment-match)))`
!!!(p) {:.unnumlist}

`(defun single-match-fn (x)`
!!!(p) {:.unnumlist}

` "Get the single-match function for x,`
!!!(p) {:.unnumlist}

` if it is a symbol that has one."`
!!!(p) {:.unnumlist}

` (when (symbolp x) (get x 'single-match)))`
!!!(p) {:.unnumlist}

The last thing to do is define the individual matching functions.
First, the single-pattern matching functions:

[ ](#){:#l0090}`(defun match-is (var-and-pred input bindings)`
!!!(p) {:.unnumlist}

` "Succeed and bind var if the input satisfies pred,`
!!!(p) {:.unnumlist}

` where var-and-pred is the list (var pred)."`
!!!(p) {:.unnumlist}

` (let* ((var (first var-and-pred))`
!!!(p) {:.unnumlist}

`   (pred (second var-and-pred))`
!!!(p) {:.unnumlist}

`   (new-bindings (pat-match var input bindings)))`
!!!(p) {:.unnumlist}

`  (if (or (eq new-bindings fail)`
!!!(p) {:.unnumlist}

`    (not (funcall pred input)))`
!!!(p) {:.unnumlist}

`   fail`
!!!(p) {:.unnumlist}

`   new-bindings)))`
!!!(p) {:.unnumlist}

`(defun match-and (patterns input bindings)`
!!!(p) {:.unnumlist}

` "Succeed if all the patterns match the input."`
!!!(p) {:.unnumlist}

` (cond ((eq bindings fail) fail)`
!!!(p) {:.unnumlist}

`   ((null patterns) bindings)`
!!!(p) {:.unnumlist}

`   (t (match-and (rest patterns) input`
!!!(p) {:.unnumlist}

`       (pat-match (first patterns) input`
!!!(p) {:.unnumlist}

`         bindings)))))`
!!!(p) {:.unnumlist}

`(defun match-or (patterns input bindings)`
!!!(p) {:.unnumlist}

` "Succeed if any one of the patterns match the input."`
!!!(p) {:.unnumlist}

` (if (null patterns)`
!!!(p) {:.unnumlist}

`   fail`
!!!(p) {:.unnumlist}

`    (let ((new-bindings (pat-match (first patterns)`
!!!(p) {:.unnumlist}

`          input bindings)))`
!!!(p) {:.unnumlist}

`    (if (eq new-bindings fail)`
!!!(p) {:.unnumlist}

`     (match-or (rest patterns) input bindings)`
!!!(p) {:.unnumlist}

`     new-bindings))))`
!!!(p) {:.unnumlist}

`(defun match-not (patterns input bindings)`
!!!(p) {:.unnumlist}

` "Succeed if none of the patterns match the input`
!!!(p) {:.unnumlist}

` This will never bind any variables."`
!!!(p) {:.unnumlist}

` (if (match-or patterns input bindings)`
!!!(p) {:.unnumlist}

`   fail`
!!!(p) {:.unnumlist}

`   bindings))`
!!!(p) {:.unnumlist}

Now the segment-pattern matching functions.
`segment-match` is similar to the version presented as part of ELIZA !!!(span) {:.smallcaps} .
The difference is in how we determine pos, the position of the first element of the input that could match the next element of the pattern after the segment variable.
In ELIZA !!!(span) {:.smallcaps} , we assumed that the segment variable was either the last element of the pattern or was followed by a constant.
In the following version, we allow nonconstant patterns to follow segment variables.
The function `first -match - pos` is added to handle this.
If the following element is in fact a constant, the same calculation is done using `position`.
If it is not a constant, then we just return the first possible starting position—unless that would put us past the end of the input, in which case we return nil to indicate failure:

[ ](#){:#l0095}`(defun segment-match (pattern input bindings &optional (start 0))`
!!!(p) {:.unnumlist}

` "Match the segment pattern ((?* var) .
pat) against input."`
!!!(p) {:.unnumlist}

` (let ((var (second (first pattern)))`
!!!(p) {:.unnumlist}

`   (pat (rest pattern)))`
!!!(p) {:.unnumlist}

`  (if (null pat)`
!!!(p) {:.unnumlist}

`   (match-variable var input bindings)`
!!!(p) {:.unnumlist}

`   (let ((pos (first-match-pos (first pat) input start)))`
!!!(p) {:.unnumlist}

`    (if (null pos)`
!!!(p) {:.unnumlist}

`      fail`
!!!(p) {:.unnumlist}

`      (let ((b2 (pat-match`
!!!(p) {:.unnumlist}

`          pat (subseq input pos)`
!!!(p) {:.unnumlist}

`          (match-variable var (subseq input 0 pos)`
!!!(p) {:.unnumlist}

`            bindings))))`
!!!(p) {:.unnumlist}

`       ;; If this match failed, try another longer one`
!!!(p) {:.unnumlist}

`       (if (eq b2 fail)`
!!!(p) {:.unnumlist}

`        (segment-match pattern input bindings (+ pos 1))`
!!!(p) {:.unnumlist}

`        b2)))))))`
!!!(p) {:.unnumlist}

`(defun first-match-pos (patl input start)`
!!!(p) {:.unnumlist}

` "Find the first position that pat1 could possibly match input,`
!!!(p) {:.unnumlist}

` starting at position start.
If pat1 is non-constant, then just`
!!!(p) {:.unnumlist}

` return start."`
!!!(p) {:.unnumlist}

` (cond ((and (atom pat1) (not (variable-p pat1)))`
!!!(p) {:.unnumlist}

`    (position pat1 input :start start :test #'equal))`
!!!(p) {:.unnumlist}

`   ((< start (length input)) start)`
!!!(p) {:.unnumlist}

`   (t nil)))`
!!!(p) {:.unnumlist}

In the first example below, the segment variable ?`x` matches the sequence (`b c`).
In the second example, there are two segment variables in a row.
The first successful match is achieved with the first variable, ?`x`, matching the empty sequence, and the second one, ?`y`, matching (`b c`).

[ ](#){:#l0100}`> (pat-match '(a (?* ?x) d) '(a b c d)) ⇒ ((?X B C))`
!!!(p) {:.unnumlist}

`> (pat-match '(a (?* ?x) (?* ?y) d) '(a b c d))⇒ ((?Y B C) (?X))`
!!!(p) {:.unnumlist}

In the next example, ?`x` is first matched against nil and ?`y` against (`b c d` ), but that fails, so we try matching ?`x` against a segment of length one.
That fails too, but finally the match succeeds with ?`x` matching the two-element segment (`b c`), and ?`y` matching (`d`).

[ ](#){:#t0020}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `> (pat-match` | `'(a (?* ?x) (?* ?y) ?x ?y)` |
| | `'(a b c d (b c) (d))) ⇒ ((?Y D) (?X B C))` |

Given `segment-match`, it is easy to define the function to match one-or-more elements and the function to match zero-or-one element:

[ ](#){:#l0105}`(defun segment-match + (pattern input bindings)`
!!!(p) {:.unnumlist}

` "Match one or more elements of input."`
!!!(p) {:.unnumlist}

` (segment-match pattern input bindings 1))`
!!!(p) {:.unnumlist}

`(defun segment-match?
(pattern input bindings)`
!!!(p) {:.unnumlist}

` "Match zero or one element of input."`
!!!(p) {:.unnumlist}

` (let ((var (second (first pattern)))`
!!!(p) {:.unnumlist}

`   (pat (rest pattern)))`
!!!(p) {:.unnumlist}

`  (or (pat-match (cons var pat) input bindings)`
!!!(p) {:.unnumlist}

`   (pat-match pat input bindings))))`
!!!(p) {:.unnumlist}

Finally, we supply the function to test an arbitrary piece of Lisp code.
It does this by evaluating the code with the bindings implied by the binding list.
This is one of the few cases where it is appropriate to call `eval`: when we want to give the user unrestricted access to the Lisp interpreter.

[ ](#){:#l0110}`(defun match-if (pattern input bindings)`
!!!(p) {:.unnumlist}

` "Test an arbitrary expression involving variables`
!!!(p) {:.unnumlist}

` The pattern looks like ((?if code) .
rest)."`
!!!(p) {:.unnumlist}

` (and (progv (mapcar #'car bindings)`
!!!(p) {:.unnumlist}

`    (mapcar #'cdr bindings)`
!!!(p) {:.unnumlist}

`   (eval (second (first pattern))))`
!!!(p) {:.unnumlist}

`  (pat-match (rest pattern) input bindings)))`
!!!(p) {:.unnumlist}

Here are two examples using `?if`.
The first succeeds because `(+ 3 4)` is indeed `7`, and the second fails because `(> 3 4)` is false.

[ ](#){:#t0025}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `> (pat-match` | `'(?x ?op ?y is ?z (?if (eq1 (?op ?x ?y) ?z)))` |
| | `'(3 + 4 is 7))` |
| `((?Z . 7) (?Y . 4) (?0P . +) (?X . 3))` |
| `> (pat-match` | `'(?x ?op ?y (?if (?op ?x ?y)))` |
| | `'(3 > 4))` |
| `NIL` | |

![t0025](images/B9780080571157500066/t0025.png)

The syntax we have defined for patterns has two virtues: first, the syntax is very general, so it is easy to extend.
Second, the syntax can be easily manipulated by `pat-match`.
However, there is one drawback: the syntax is a little verbose, and some may find it ugly.
Compare the following two patterns:

[ ](#){:#l0115}`(a (?* ?x) (?* ?y) d)`
!!!(p) {:.unnumlist}

`(a ?x* ?y* d)`
!!!(p) {:.unnumlist}

Many readers find the second pattern easier to understand at a glance.
We could change `pat-match` to allow for patterns of the form ?`x*`, but that would mean `pat-match` would have a lot more work to do on every match.
An alternative is to leave `pat-match` as is, but define another level of syntax for use by human readers only.
That is, a programmer could type the second expression above, and have it translated into the first, which would then be processed by `pat-match.`

In other words, we will define a facility to define a kind of pattern-matching macro that will be expanded the first time the pattern is seen.
It is better to do this expansion once than to complicate `pat-match` and in effect do the expansion every time a pattern is used.
(Of course, if a pattern is only used once, then there is no advantage.
But in most programs, each pattern will be used again and again.)

We need to define two functions: one to define pattern-matching macros, and another to expand patterns that may contain these macros.
We will only allow symbols to be macros, so it is reasonable to store the expansions on each symbol's property list:

[ ](#){:#l0120}`(defun pat-match-abbrev (symbol expansion)`
!!!(p) {:.unnumlist}

` "Define symbol as a macro standing for a pat-match pattern."`
!!!(p) {:.unnumlist}

` (setf (get symbol 'expand-pat-match-abbrev)`
!!!(p) {:.unnumlist}

`  (expand-pat-match-abbrev expansion))`
!!!(p) {:.unnumlist}

`(defun expand-pat-match-abbrev (pat)`
!!!(p) {:.unnumlist}

` "Expand out all pattern matching abbreviations in pat."`
!!!(p) {:.unnumlist}

` (cond ((and (symbolp pat) (get pat 'expand-pat-match-abbrev)))`
!!!(p) {:.unnumlist}

`   ((atom pat) pat)`
!!!(p) {:.unnumlist}

`   (t (cons (expand-pat-match-abbrev (first pat))`
!!!(p) {:.unnumlist}

`     (expand-pat-match-abbrev (rest pat))))))`
!!!(p) {:.unnumlist}

We would use this facility as follows:

[ ](#){:#l0125}`> (pat-match-abbrev '?x* '(?* ?x)) ⇒ (?* ?X)`
!!!(p) {:.unnumlist}

`> (pat-match-abbrev '?y* '(?* ?y)) ⇒ (?* ?Y)`
!!!(p) {:.unnumlist}

`> (setf axyd (expand-pat-match-abbrev '(a ?x* ?y* d))) ⇒`
!!!(p) {:.unnumlist}

`(A (?* ?X) (?* ?Y) D)`
!!!(p) {:.unnumlist}

`> (pat-match axyd '(a b c d)) ⇒ ((?Y B C) (?X))`
!!!(p) {:.unnumlist}

**Exercise 6**.**1** [**m**] Go back and change the ELIZA !!!(span) {:.smallcaps} rules to use the abbreviation facility.
Does this make the rules easier to read?

**Exercise 6**.**2** [**h**] In the few prior examples, every time there was a binding of pattern variables that satisfied the input, that binding was found.
Informally, show that `pat-match` will always find such a binding, or show a counterexample where it fails to find one.

## [ ](#){:#st0020}6.3 A Rule-Based Translator Tool
{:#s0020}
{:.h1hd}

As we have defined it, the pattern matcher matches one input against one pattern.
In `eliza`, we need to match each input against a number of patterns, and then return a result based on the rule that contains the first pattern that matches.
To refresh your memory, here is the function `use-eliza-rules`:

[ ](#){:#l0130}`(defun use-eliza-rules (input)`
!!!(p) {:.unnumlist}

` "Find some rule with which to transform the input."`
!!!(p) {:.unnumlist}

` (some #'(lambda (rule)`
!!!(p) {:.unnumlist}

`   (let ((result (pat-match (rule-pattern rule) input)))`
!!!(p) {:.unnumlist}

`    (if (not (eq result fail))`
!!!(p) {:.unnumlist}

`     (sublis (switch-viewpoint result)`
!!!(p) {:.unnumlist}

`      (random-elt (rule-responses rule))))))`
!!!(p) {:.unnumlist}

`  *eliza-rules*))`
!!!(p) {:.unnumlist}

It turns out that this will be a quite common thing to do: search through a list of rules for one that matches, and take action according to that rule.
To turn the structure of `use-eliza-rules` into a software tool, we will allow the user to specify each of the following:

* [ ](#){:#l0135}• What kind of rule to use.
Every rule will be characterized by an if-part and a then-part, but the ways of getting at those two parts may vary.

* • What list of rules to use.
In general, each application will have its own list of rules.

* • How to see if a rule matches.
By default, we will use `pat-match`, but it should be possible to use other matchers.

* • What to do when a rule matches.
Once we have determined which rule to use, we have to determine what it means to use it.
The default is just to substitute the bindings of the match into the then-part of the rule.

The rule-based translater tool now looks like this:

[ ](#){:#l0140}`(defun rule-based-translator`
!!!(p) {:.unnumlist}

`   (input rules &key (matcher #'pat-match)`
!!!(p) {:.unnumlist}

`    (rule-if #'first) (rule-then #'rest) (action #'sublis))`
!!!(p) {:.unnumlist}

` "Find the first rule in rules that matches input,`
!!!(p) {:.unnumlist}

` and apply the action to that rule."`
!!!(p) {:.unnumlist}

` (some`
!!!(p) {:.unnumlist}

`  #'(lambda (rule)`
!!!(p) {:.unnumlist}

`    (let ((result (funcall matcher (funcall rule-if rule)`
!!!(p) {:.unnumlist}

`        input)))`
!!!(p) {:.unnumlist}

`    (if (not (eq result fail))`
!!!(p) {:.unnumlist}

`     (funcall action result (funcall rule-then rule)))))`
!!!(p) {:.unnumlist}

`  rules))`
!!!(p) {:.unnumlist}

`(defun use-eliza-rules (input)`
!!!(p) {:.unnumlist}

` "Find some rule with which to transform the input."`
!!!(p) {:.unnumlist}

` (rule-based-translator input *eliza-rules*`
!!!(p) {:.unnumlist}

`  :action #'(lambda (bindings responses)`
!!!(p) {:.unnumlist}

`     (sublis (switch-viewpoint bindings)`
!!!(p) {:.unnumlist}

`        (random-elt responses)))))`
!!!(p) {:.unnumlist}

## [ ](#){:#st0025}6.4 A Set of Searching Tools
{:#s0025}
{:.h1hd}

The GPS program can be seen as a problem in *search*.
In general, a search problem involves exploring from some starting state and investigating neighboring states until a solution is reached.
As in GPS, *state* means a description of any situation or state of affairs.
Each state may have several neighbors, so there will be a choice of how to search.
We can travel down one path until we see it is a dead end, or we can consider lots of different paths at the same time, expanding each path step by step.
Search problems are called *nondeterministic* because there is no way to determine what is the best step to take next.
AI problems, by their very nature, tend to be nondeterministic.
This can be a source of confusion for programmers who are used to deterministic problems.
In this section we will try to clear up that confusion.
This section also serves as an example of how higher-order functions can be used to implement general tools that can be specified by passing in specific functions.

Abstractly, a search problem can be characterized by four features:

* [ ](#){:#l0145}• The *start* state.

* • The *goal* state (or states).

* • The *successors*, or states that can be reached from any other state.

* • The *strategy* that determines the *order* in which we search.

The first three features are part of the problem, while the fourth is part of the solution.
In GPS, the starting state was given, along with a description of the goal states.
The successors of a state were determined by consulting the operators.
The search strategy was means-ends analysis.
This was never spelled out explicitly but was implicit in the structure of the whole program.
In this section we will formulate a general searching tool, show how it can be used to implement several different search strategies, and then show how GPS could be implemented with this tool.

The first notion we have to define is the *state space*, or set of all possible states.
We can view the states as nodes and the successor relation as links in a graph.
Some state space graphs will have a small number of states, while others have an infinite number, but they can still be solved if we search cleverly.
Some graphs will have a regular structure, while others will appear random.
We will start by considering only trees—that is, graphs where a state can be reached by only one unique sequence of successor links.
Here is a tree:

![u06-01-9780080571157](images/B9780080571157500066/u06-01-9780080571157.jpg)     

### [ ](#){:#st0030}Searching Trees
{:#s0035}
{:.h2hd}

We will call our first searching tool `tree-search`, because it is designed to search state spaces that are in the form of trees.
It takes four arguments: (1) a list of valid starting states, (2) a predicate to decide if we have reached a goal state, (3) a function to generate the successors of a state, and (4) a function that decides in what order to search.
The first argument is a list rather than a single state so that `tree-search` can recursively call itself after it has explored several paths through the state space.
Think of the first argument not as a starting state but as a list of possible states from which the goal may be reached.
This lists represents the fringe of the tree that has been explored so far.
`tree-search` has three cases: If there are no more states to consider, then give up and return `fai1`.
If the first possible state is a goal state, then return the succesful state.
Otherwise, generate the successors of the first state and combine them with the other states.
Order this combined list according to the particular search strategy and continue searching.
Note that `tree-search` itself does not specify any particular searching strategy.

[ ](#){:#l0150}`(defun tree-search (states goal-p successors combiner)`
!!!(p) {:.unnumlist}

` "Find a state that satisfies goal-p.
Start with states,`
!!!(p) {:.unnumlist}

` and search according to successors and combiner."`
!!!(p) {:.unnumlist}

` (dbg :search "~&; ; Search: ~ a" states)`
!!!(p) {:.unnumlist}

` (cond ((null states) fail)`
!!!(p) {:.unnumlist}

`   ((funcall goal-p (first states)) (first states))`
!!!(p) {:.unnumlist}

`   (t (tree-search`
!!!(p) {:.unnumlist}

`     (funcall combiner`
!!!(p) {:.unnumlist}

`        (funcall successors (first states))`
!!!(p) {:.unnumlist}

`        (rest states))`
!!!(p) {:.unnumlist}

`     goal-p successors combiner))))`
!!!(p) {:.unnumlist}

The first strategy we will consider is called *depth*-*first search*.
In depth-first search, the longest paths are considered first.
In other words, we generate the successors of a state, and then work on the first successor first.
We only return to one of the subsequent successors if we arrive at a state that has no successors at all.
This strategy can be implemented by simply appending the previous states to the end of the list of new successors on each iteration.
The function `depth-first-search` takes a single starting state, a goal predicate, and a successor function.
It packages the starting state into a list as expected by `tree-search`, and specifies append as the combining function:

[ ](#){:#l0155}`(defun depth-first-search (start goal-p successors)`
!!!(p) {:.unnumlist}

` "Search new states first until goal is reached."`
!!!(p) {:.unnumlist}

` (tree-search (list start) goal-p successors #'append))`
!!!(p) {:.unnumlist}

Let's see how we can search through the binary tree defined previously.
First, we define the successor function `binary-tree`.
It returns a list of two states, the two numbers that are twice the input state and one more than twice the input state.
So the successors of 1 will be 2 and 3, and the successors of 2 will be 4 and 5.
The `binary-tree` function generates an infinite tree of which the first 15 nodes are diagrammed in our example.

[ ](#){:#l0160}`(defun binary-tree (x) (list (* 2 x) (+ 1 (* 2 x))))`
!!!(p) {:.unnumlist}

To make it easier to specify a goal, we define the function is as a function that returns a predicate that tests for a particular value.
Note that is does not do the test itself.
Rather, it returns a function that can be called to perform tests:

[ ](#){:#l0165}`(defun is (value) #'(lambda (x) (eq1 x value)))`
!!!(p) {:.unnumlist}

Now we can turn on the debugging output and search through the binary tree, starting at 1, and looking for, say, 12, as the goal state.
Each line of debugging output shows the list of states that have been generated as successors but not yet examined:

[ ](#){:#l0170}`> (debug :search) ⇒ (SEARCH)`
!!!(p) {:.unnumlist}

`> (depth-first-search 1 (is 12) #'binary-tree)`
!!!(p) {:.unnumlist}

`;; Search: (1)`
!!!(p) {:.unnumlist}

`;; Search: (2 3)`
!!!(p) {:.unnumlist}

`;; Search: (4 5 3)`
!!!(p) {:.unnumlist}

`;; Search: (8 9 5 3)`
!!!(p) {:.unnumlist}

`;; Search: (16 17 9 5 3)`
!!!(p) {:.unnumlist}

`;; Search: (32 33 17 9 5 3)`
!!!(p) {:.unnumlist}

`;; Search: (64 65 33 17 9 5 3)`
!!!(p) {:.unnumlist}

`;; Search: (128 129 65 33 17 9 5 3)`
!!!(p) {:.unnumlist}

`;; Search: (256 257 129 65 33 17 9 5 3)`
!!!(p) {:.unnumlist}

`;; Search: (512 513 257 129 65 33 17 9 5 3)`
!!!(p) {:.unnumlist}

`;; Search: (1024 1025 513 257 129 65 33 17 9 5 3)`
!!!(p) {:.unnumlist}

`;; Search: (2048 2049 1025 513 257 129 65 33 17 9 5 3)`
!!!(p) {:.unnumlist}

`[Abort]`
!!!(p) {:.unnumlist}

The problem is that we are searching an infinite tree, and the depth-first search strategy just dives down the left-hand branch at every step.
The only way to stop the doomed search is to type an interrupt character.

An alternative strategy is *breadth*-*first search*, where the shortest path is extended first at each step.
It can be implemented simply by appending the new successor states to the end of the existing states:

[ ](#){:#l0175}`(defun prepend (x y) "Prepend y to start of x" (append y x))`
!!!(p) {:.unnumlist}

`(defun breadth-first-search (start goal-p successors)`
!!!(p) {:.unnumlist}

` "Search old states first until goal is reached."`
!!!(p) {:.unnumlist}

` (tree-search (list start) goal-p successors #'prepend))`
!!!(p) {:.unnumlist}

The only difference between depth-first and breadth-first search is the difference between `append` and `prepend`.
Here we see `breadth-first-search` in action:

[ ](#){:#l0180}`> (breadth-first-search 1 (is 12) 'binary-tree)`
!!!(p) {:.unnumlist}

`;; Search: (1)`
!!!(p) {:.unnumlist}

`;; Search: (2 3)`
!!!(p) {:.unnumlist}

`;; Search: (3 4 5)`
!!!(p) {:.unnumlist}

`;; Search: (4 5 6 7)`
!!!(p) {:.unnumlist}

`;; Search: (5 6 7 8 9)`
!!!(p) {:.unnumlist}

`;; Search: (6 7 8 9 10 11)`
!!!(p) {:.unnumlist}

`;; Search: (7 8 9 10 11 12 13)`
!!!(p) {:.unnumlist}

`;; Search: (8 9 10 11 12 13 14 15)`
!!!(p) {:.unnumlist}

`;; Search: (9 10 11 12 13 14 15 16 17)`
!!!(p) {:.unnumlist}

`Search: (10 11 12 13 14 15 16 17 18 19)`
!!!(p) {:.unnumlist}

`;; Search: (11 12 13 14 15 16 17 18 19 20 21)`
!!!(p) {:.unnumlist}

`;; Search: (12 13 14 15 16 17 18 19 20 21 22 23)`
!!!(p) {:.unnumlist}

`12`
!!!(p) {:.unnumlist}

Breadth-first search ends up searching each node in numerical order, and so it will eventually find any goal.
It is methodical, but therefore plodding.
Depth-first search will be much faster—if it happens to find the goal at all.
For example, if we were looking for 2048, depth-first search would find it in 12 steps, while breadth-first would take 2048 steps.
Breadth-first search also requires more storage, because it saves more intermediate states.

If the search tree is finite, then either breadth-first or depth-first will eventually find the goal.
Both methods search the entire state space, but in a different order.
We will now show a depth-first search of the 15-node binary tree diagrammed previously.
It takes about the same amount of time to find the goal (12) as it did with breadth-first search.
It would have taken more time to find 15; less to find 8.
The big difference is in the number of states considered at one time.
At most, depth-first search considers four at a time; in general it will need to store only log2*n* states to search a *n*-node tree, while breadth-first search needs to store *n*/2 states.

[ ](#){:#l0185}`(defun finite-binary-tree (n)`
!!!(p) {:.unnumlist}

` "Return a successor function that generates a binary tree`
!!!(p) {:.unnumlist}

` with n nodes."`
!!!(p) {:.unnumlist}

` #'(lambda (x)`
!!!(p) {:.unnumlist}

`     (remove-if #'(lambda (child) (> child n))`
!!!(p) {:.unnumlist}

`        (binary-tree x))))`
!!!(p) {:.unnumlist}

`(depth-first-search 1 (is 12) (finite-binary-tree 15))`
!!!(p) {:.unnumlist}

`;; Search: (1)`
!!!(p) {:.unnumlist}

`;; Search: (2 3)`
!!!(p) {:.unnumlist}

`;; Search: (4 5 3)`
!!!(p) {:.unnumlist}

`;; Search: (8 9 5 3)`
!!!(p) {:.unnumlist}

`;; Search: (9 5 3)`
!!!(p) {:.unnumlist}

`;; Search: (5 3)`
!!!(p) {:.unnumlist}

`;; Search: (10 11 3)`
!!!(p) {:.unnumlist}

`;; Search: (11 3)`
!!!(p) {:.unnumlist}

`;; Search: (3)`
!!!(p) {:.unnumlist}

`;; Search: (6 7)`
!!!(p) {:.unnumlist}

`;; Search: (12 13 7)`
!!!(p) {:.unnumlist}

`12`
!!!(p) {:.unnumlist}

### [ ](#){:#st0035}Guiding the Search
{:#s0040}
{:.h2hd}

While breadth-first search is more methodical, neither strategy is able to take advantage of any knowledge about the state space.
They both search blindly.
In most real applications we will have some estimate of how far a state is from the solution.
In such cases, we can implement a *best*-*first search*.
The name is not quite accurate; if we could really search best first, that would not be a search at all.
The name refers to the fact that the state that *appears* to be best is searched first.

To implement best-first search we need to add one more piece of information: a cost function that gives an estimate of how far a given state is from the goal.

For the binary tree example, we will use as a cost estimate the numeric difference from the goal.
So if we are looking for 12, then 12 has cost 0, 8 has cost 4 and 2048 has cost 2036.
The higher-order function `diff`, shown in the following, returns a cost function that computes the difference from a goal.
The higher-order function sorter takes a cost function as an argument and returns a combiner function that takes the lists of old and new states, appends them together, and sorts the result based on the cost function, lowest cost first.
(The built-in function `sort` sorts a list according to a comparison function.
In this case the smaller numbers come first.
`sort` takes an optional : `key` argument that says how to compute the score for each element.
Be careful—`sort` is a destructive function.)

[ ](#){:#l0190}`(defun diff (num)`
!!!(p) {:.unnumlist}

` "Return the function that finds the difference from num."`
!!!(p) {:.unnumlist}

` #'(lambda (x) (abs (- x num))))`
!!!(p) {:.unnumlist}

`(defun sorter (cost-fn)`
!!!(p) {:.unnumlist}

` "Return a combiner function that sorts according to cost-fn."`
!!!(p) {:.unnumlist}

` #'(lambda (new old)`
!!!(p) {:.unnumlist}

`   (sort (append new old) #'< :key cost-fn)))`
!!!(p) {:.unnumlist}

`(defun best-first-search (start goal-p successors cost-fn)`
!!!(p) {:.unnumlist}

` "Search lowest cost states first until goal is reached."`
!!!(p) {:.unnumlist}

` (tree-search (list start) goal-p successors (sorter cost-fn)))`
!!!(p) {:.unnumlist}

Now, using the difference from the goal as the cost function, we can search using best-first search:

[ ](#){:#l0195}`> (best-first-search 1 (is 12) #'binary-tree (diff 12))`
!!!(p) {:.unnumlist}

`;; Search: (1)`
!!!(p) {:.unnumlist}

`;; Search: (3 2)`
!!!(p) {:.unnumlist}

`;; Search: (7 6 2)`
!!!(p) {:.unnumlist}

`;; Search: (14 15 6 2)`
!!!(p) {:.unnumlist}

`;; Search: (15 6 2 28 29)`
!!!(p) {:.unnumlist}

`;; Search: (6 2 28 29 30 31)`
!!!(p) {:.unnumlist}

`;; Search: (12 13 2 28 29 30 31)`
!!!(p) {:.unnumlist}

`12`
!!!(p) {:.unnumlist}

The more we know about the state space, the better we can search.
For example, if we know that all successors are greater than the states they come from, then we can use a cost function that gives a very high cost for numbers above the goal.
The function `price- is - right` is like `diff`, except that it gives a high penalty for going over the goal.[3](#fn0025){:#xfn0025} Using this cost function leads to a near-optimal search on this example.
It makes the “mistake” of searching 7 before 6 (because 7 is closer to 12), but does not waste time searching 14 and 15:

[ ](#){:#l0200}`(defun price-is-right (price)`
!!!(p) {:.unnumlist}

` "Return a function that measures the difference from price,`
!!!(p) {:.unnumlist}

` but gives a big penalty for going over price."`
!!!(p) {:.unnumlist}

` #'(lambda (x) (if (> x price)`
!!!(p) {:.unnumlist}

`       most-positive-fixnum`
!!!(p) {:.unnumlist}

`       (− price x))))`
!!!(p) {:.unnumlist}

`> (best-first-search 1 (is 12) #'binary-tree (price-is-right 12)) ;; Search: (1)`
!!!(p) {:.unnumlist}

`;; Search: (3 2)`
!!!(p) {:.unnumlist}

`;; Search: (7 6 2)`
!!!(p) {:.unnumlist}

`;; Search: (6 2 14 15)`
!!!(p) {:.unnumlist}

`;; Search: (12 2 13 14 15)`
!!!(p) {:.unnumlist}

`12`
!!!(p) {:.unnumlist}

All the searching methods we have seen so far consider ever-increasing lists of states as they search.
For problems where there is only one solution, or a small number of solutions, this is unavoidable.
To find a needle in a haystack, you need to look at a lot of hay.
But for problems with many solutions, it may be worthwhile to discard unpromising paths.
This runs the risk of failing to find a solution at all, but it can save enough space and time to offset the risk.
A best-first search that keeps only a fixed number of alternative states at any one time is known as a *beam search*.
Think of searching as shining a light through the dark of the state space.
In other search strategies the light spreads out as we search deeper, but in beam search the light remains tightly focused.
Beam search is a variant of best-first search, but it is also similar to depth-first search.
The difference is that beam search looks down several paths at once, instead of just one, and chooses the best one to look at next.
But it gives up the ability to backtrack indefinitely.
The function `beam-search` is just like `best-first-search`, except that after we sort the states, we then take only the first `beam-width` states.
This is done with `subseq`; (`subseq`*list start end*) returns the sublist that starts at position *start* and ends just before position *end*.

[ ](#){:#l0205}`(defun beam-search (start goal-p successors cost-fn beam-width)`
!!!(p) {:.unnumlist}

` "Search highest scoring states first until goal is reached,`
!!!(p) {:.unnumlist}

` but never consider more than beam-width states at a time."`
!!!(p) {:.unnumlist}

` (tree-search (list start) goal-p successors`
!!!(p) {:.unnumlist}

`    #'(lambda (old new)`
!!!(p) {:.unnumlist}

`     (let ((sorted (funcall (sorter cost-fn) old new)))`
!!!(p) {:.unnumlist}

`      (if (> beam-width (length sorted))`
!!!(p) {:.unnumlist}

`       sorted`
!!!(p) {:.unnumlist}

`       (subseq sorted0 beam-width))))))`
!!!(p) {:.unnumlist}

We can successfully search for 12 in the binary tree using a beam width of only 2:

[ ](#){:#l0210}`> (beam-search 1 (is 12) #'binary-tree (price-is-right 12) 2)`
!!!(p) {:.unnumlist}

`;; Search: (1)`
!!!(p) {:.unnumlist}

`;; Search: (3 2)`
!!!(p) {:.unnumlist}

`;; Search: (7 6)`
!!!(p) {:.unnumlist}

`;; Search: (6 14)`
!!!(p) {:.unnumlist}

`;; Search: (12 13)`
!!!(p) {:.unnumlist}

`12`
!!!(p) {:.unnumlist}

However, if we go back to the scoring function that just takes the difference from 12, then beam search fails.
When it generates 14 and 15, it throws away 6, and thus loses its only chance to find the goal:

[ ](#){:#l0215}`> (beam-search 1 (is 12) #'binary-tree (diff 12) 2)`
!!!(p) {:.unnumlist}

`;; Search: (1)`
!!!(p) {:.unnumlist}

`;; Search: (3 2)`
!!!(p) {:.unnumlist}

`;; Search: (7 6)`
!!!(p) {:.unnumlist}

`;; Search: (14 15)`
!!!(p) {:.unnumlist}

`;; Search: (15 28)`
!!!(p) {:.unnumlist}

`;; Search: (28 30)`
!!!(p) {:.unnumlist}

`;; Search: (30 56)`
!!!(p) {:.unnumlist}

`;; Search: (56 60)`
!!!(p) {:.unnumlist}

`;; Search: (60 112)`
!!!(p) {:.unnumlist}

`;; Search: (112 120)`
!!!(p) {:.unnumlist}

`;; Search: (120 224)`
!!!(p) {:.unnumlist}

`[Abort]`
!!!(p) {:.unnumlist}

This search would succeed if we gave a beam width of 3.
This illustrâtes a general principle: we can find a goal either by looking at more states, or by being smarter about the states we look at.
That means having a better ordering function.

Notice that with a beam width of infinity we get best-first search.
With a beam width of 1, we get depth-first search with no backup.
This could be called “depth-only search,” but it is more commonly known as *hill*-*climbing*.
Think of a mountaineer trying to reach a peak in a heavy fog.
One strategy would be for the mountaineer to look at adjacent locations, climb to the highest one, and look again.
This strategy may eventually hit the peak, but it may also get stuck at the top of a foothill, or *local maximum*.
Another strategy would be for the mountaineer to turn back and try again when the fog lifts, but in AI, unfortunately, the fog rarely lifts.[4](#fn0030){:#xfn0030}

As a concrete example of a problem that can be solved by search, consider the task of planning a flight across the North American continent in a small airplane, one whose range is limited to 1000 kilometers.
Suppose we have a list of selected cities with airports, along with their position in longitude and latitude:

[ ](#){:#t0030}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `(defstruct (city (:type list)) name long lat)` |
| `(defparameter *cities*` |
| `'((Atlanta` | `84.23 33.45)` | `(Los-Angeles` | `118.15 34.03` |
| `(Boston` | `71.05 42.21)` | `(Memphis` | `90.03 35.09)` |
| `(Chicago` | `87.37 41.50)` | `(New-York` | `73.58 40.47)` |
| `(Denver` | `105.00 39.45)` | `(Oklahoma-City` | `97.28 35.26)` |
| `(Eugene` | `123.05 44.03)` | `(Pittsburgh` | `79.57 40.27)` |
| `(Flagstaff` | `111.41 35.13)` | `(Quebec` | `71.11 46.49)` |
| `(Grand-Jet` | `108.37 39.05)` | `(Reno` | `119.49 39.30)` |
| `(Houston` | `105.00 34.00)` | `(San-Francisco` | `122.26 37.47)` |
| `(Indianapolis` | `86.10 39.46)` | `(Tampa` | `82.27 27.57)` |
| `(Jacksonville` | `81.40 30.22)` | `(Victoria` | `123.21 48.25)` |
| `(Kansas-City` | `94.35 39.06)` | `(Wilmington` | `77.57 34.14)))` |

![t0030](images/B9780080571157500066/t0030.png)

This example introduces a new option to `defstruct`.
Instead of just giving the name of the structure, it is also possible to use:

[ ](#){:#l0220}`(defstruct *(structure*-*name (option value*)…) *"optional doc" slot*…)`
!!!(p) {:.unnumlist}

For city, the option : type is specified as `list`.
This means that cities will be implemented as lists of three elements, as they are in the initial value for `*cities*`.

The cities are shown on the map in [figure 6.1](#f0010), which has connections between all cities within the 1000 kilometer range of each other.[5](#fn0035){:#xfn0035} This map was drawn with the help of `air-distance`, a function that returns the distance in kilometers between two cities “as the crow flies.” It will be defined later.
Two other useful functions are `neighbors`, which finds all the cities within 1000 kilometers, and `city`, which maps from a name to a city.
The former uses `find-a11-if`, which was defined on [page 101](B9780080571157500030.xhtml#p101) as a synonym for `remove-if-not`.

![f06-01-9780080571157](images/B9780080571157500066/f06-01-9780080571157.jpg)     
Figure 6.1
!!!(span) {:.fignum}
A Map of Some Cities
[ ](#){:#l0225}`(defun neighbors (city)`
!!!(p) {:.unnumlist}

` "Find all cities within 1000 kilometers."`
!!!(p) {:.unnumlist}

` (find-all-if #'(lambda (c)`
!!!(p) {:.unnumlist}

`     (and (not (eq c city))`
!!!(p) {:.unnumlist}

`       (< (air-distance c city) 1000.0)))`
!!!(p) {:.unnumlist}

`    *cities*))`
!!!(p) {:.unnumlist}

`(defun city (name)`
!!!(p) {:.unnumlist}

` "Find the city with this name."`
!!!(p) {:.unnumlist}

` (assoc name *cities*))`
!!!(p) {:.unnumlist}

We are now ready to plan a trip.
The function `trip` takes the name of a starting and destination city and does a beam search of width one, considering all neighbors as successors to a state.
The cost for a state is the air distance to the destination city:

[ ](#){:#l0230}`(defun trip (start dest)`
!!!(p) {:.unnumlist}

` "Search for a way from the start to dest."`
!!!(p) {:.unnumlist}

` (beam-search start (is dest) #'ne1ghbors`
!!!(p) {:.unnumlist}

`     #'(lambda (c) (air-distance c dest))`
!!!(p) {:.unnumlist}

`     1))`
!!!(p) {:.unnumlist}

Here we plan a trip from San Francisco to Boston.
The result seems to be the best possible path:

[ ](#){:#l0235}`> (trip (city ' san-francisco) (city 'boston))`
!!!(p) {:.unnumlist}

`;; Search: ((SAN-FRANCISCO 122.26 37.47))`
!!!(p) {:.unnumlist}

`;; Search: ((RENO 119.49 39.3))`
!!!(p) {:.unnumlist}

`;; Search: ((GRAND-JCT 108.37 39.05))`
!!!(p) {:.unnumlist}

`;; Search: ((DENVER 105.0 39.45))`
!!!(p) {:.unnumlist}

`;; Search: ((KANSAS-CITY 94.35 39.06))`
!!!(p) {:.unnumlist}

`;; Search: ((INDIANAP0LIS 86.1 39.46))`
!!!(p) {:.unnumlist}

`;; Search: ((PITTSBURGH 79.57 40.27))`
!!!(p) {:.unnumlist}

`;; Search: ((BOSTON 71.05 42.21))`
!!!(p) {:.unnumlist}

`(BOSTON 71.05 42.21)`
!!!(p) {:.unnumlist}

But look what happens when we plan the return trip.
There are two detours, to Chicago and Flagstaff:

[ ](#){:#l0240}`> (trip (city 'boston) (city 'san-francisco))`
!!!(p) {:.unnumlist}

`;; Search: ((BOSTON 71.05 42.21))`
!!!(p) {:.unnumlist}

`;; Search: ((PITTSBURGH 79.57 40.27))`
!!!(p) {:.unnumlist}

`;; Search: ((CHICAGO 87.37 41.5))`
!!!(p) {:.unnumlist}

`;; Search: ((KANSAS-CITY 94.35 39.06))`
!!!(p) {:.unnumlist}

`;; Search: ((DENVER 105.0 39.45))`
!!!(p) {:.unnumlist}

`;; Search: ((FLAGSTAFF 111.41 35.13))`
!!!(p) {:.unnumlist}

`;; Search: ((RENO 119.49 39.3))`
!!!(p) {:.unnumlist}

`;; Search: ((SAN-FRANCISCO 122.26 37.47))`
!!!(p) {:.unnumlist}

`(SAN-FRANCISCO 122.26 37.47)`
!!!(p) {:.unnumlist}

Why did `trip` go from Denver to San Francisco via Flagstaff?
Because Flagstaff is closer to the destination than Grand Junction.
The problem is that we are minimizing the distance to the destination at each step, when we should be minimizing the sum of the distance to the destination plus the distance already traveled.

### [ ](#){:#st0040}Search Paths
{:#s0045}
{:.h2hd}

To minimize the total distance, we need some way to talk about the *path* that leads to the goal.
But the functions we have defined so far only deal with individual states along the way.
Representing paths would lead to another advantage: we could return the path as the solution, rather than just return the goal state.
As it is, `trip` only returns the goal state, not the path to it.
So there is no way to determine what `trip` has done, except by reading the debugging output.

The data structure path is designed to solve both these problems.
A path has four fields: the current state, the previous partial path that this path is extending, the cost of the path so far, and an estimate of the total cost to reach the goal.
Here is the structure definition for path.
It uses the : `print-function` option to say that all paths are to be printed with the function `print-path`, which will be defined below.

[ ](#){:#l0245}`(defstruct (path (:print-function print-path))`
!!!(p) {:.unnumlist}

`  state (previous nil) (cost-so-far 0) (total-cost 0))`
!!!(p) {:.unnumlist}

The next question is how to integrate paths into the searching routines with the least amount of disruption.
Clearly, it would be better to make one change to `tree-search` rather than to change `depth-first-search`, `breadth-first-search`, and `beam-search`.
However, looking back at the definition of `tree-search`, we see that it makes no assumptions about the structure of states, other than the fact that they can be manipulated by the goal predicate, successor, and combiner functions.
This suggests that we can use `tree-search` unchanged if we pass it paths instead of states, and give it functions that can process paths.

In the following redefinition of `trip`, the `beam-search` function is called with five arguments.
Instead of passing it a city as the start state, we pass a path that has the city as its state field.
The goal predicate should test whether its argument is a path whose state is the destination; we assume (and later define) a version of `is` that accommodates this.
The successor function is the most difficult.
Instead of just generating a list of neighbors, we want to first generate the neighbors, then make each one into a path that extends the current path, but with an updated cost so far and total estimated cost.
The function `path-saver` returns a function that will do just that.
Finally, the cost function we are trying to minimize is `path-total-cost`, and we provide a beam width, which is now an optional argument to `trip` that defaults to one:

[ ](#){:#l0250}`(defun trip (start dest &optional (beam-width 1))`
!!!(p) {:.unnumlist}

` "Search for the best path from the start to dest."`
!!!(p) {:.unnumlist}

` (beam-search`
!!!(p) {:.unnumlist}

`  (make-path :state start)`
!!!(p) {:.unnumlist}

`  (is dest :key #'path-state)`
!!!(p) {:.unnumlist}

`  (path-saver #'neighbors #'air-distance`
!!!(p) {:.unnumlist}

`     #,(lambda (c) (air-distance c dest)))`
!!!(p) {:.unnumlist}

`#'path-total-cost`
!!!(p) {:.unnumlist}

`beam-width))`
!!!(p) {:.unnumlist}

The calculation of `air-distance` involves some complicated conversion of longitude and latitude to `x-y-z` coordinates.
Since this is a problem in solid geometry, not AI, the code is presented without further comment:

[ ](#){:#l0255}`(defconstant earth-diameter 12765.0`
!!!(p) {:.unnumlist}

` "Diameter of planet earth in kilometers.")`
!!!(p) {:.unnumlist}

`(defun air-distance (city1 city2)`
!!!(p) {:.unnumlist}

` "The great circle distance between two cities."`
!!!(p) {:.unnumlist}

` (let ((d (distance (xyz-coords city1) (xyz-coords city2))))`
!!!(p) {:.unnumlist}

`  ;; d is the straight-1ine chord between the two cities,`
!!!(p) {:.unnumlist}

`  ;; The length of the subtending arc is given by:`
!!!(p) {:.unnumlist}

`  (* earth-diameter (asin (/ d 2)))))`
!!!(p) {:.unnumlist}

`(defun xyz-coords (city)`
!!!(p) {:.unnumlist}

` "Returns the x,y,z coordinates of a point on a sphere.`
!!!(p) {:.unnumlist}

` The center is (0 0 0) and the north pole is (0 0 1)."`
!!!(p) {:.unnumlist}

` (let ((psi (deg->radians (city-lat city)))`
!!!(p) {:.unnumlist}

`    (phi (deg->radians (city-long city))))`
!!!(p) {:.unnumlist}

`   (list (* (cos psi) (cos phi))`
!!!(p) {:.unnumlist}

`      (* (cos psi) (sin phi))`
!!!(p) {:.unnumlist}

`      (sin psi))))`
!!!(p) {:.unnumlist}

`(defun distance (point1 point2)`
!!!(p) {:.unnumlist}

` "The Euclidean distance between two points.`
!!!(p) {:.unnumlist}

` The points are coordinates in n-dimensional space."`
!!!(p) {:.unnumlist}

` (sqrt (reduce #'+ (mapcar #'(lambda (a b) (expt (− a b) 2))`
!!!(p) {:.unnumlist}

`        point1 point2))))`
!!!(p) {:.unnumlist}

`(defun deg->radians (deg)`
!!!(p) {:.unnumlist}

` "Convert degrees and minutes to radians."`
!!!(p) {:.unnumlist}

` (* (+ (truncate deg) (* (rem deg 1) 100/60)) pi 1/180))`
!!!(p) {:.unnumlist}

Before showing the auxiliary functions that implement this, here are some examples that show what it can do.
With a beam width of 1, the detour to Flagstaff is eliminated, but the one to Chicago remains.
With a beam width of 3, the correct optimal path is found.
In the following examples, each call to the new version of `trip` returns a path, which is printed by `show-city-path`:

[ ](#){:#l0260}`> (show-city-path (trip (city 'san-francisco) (city 'boston) 1))`
!!!(p) {:.unnumlist}

`#<Path 4514.8 km: San-Francisco - Reno - Grand-Jet - Denver -`
!!!(p) {:.unnumlist}

` Kansas-City - Indianapolis - Pittsburgh - Boston >`
!!!(p) {:.unnumlist}

`> (show-city-path (trip (city 'boston) (city 'san-francisco) 1))`
!!!(p) {:.unnumlist}

`#<Path 4577.3 km: Boston - Pittsburgh - Chicago - Kansas-City -`
!!!(p) {:.unnumlist}

` Denver - Grand-Jet - Reno - San-Francisco >`
!!!(p) {:.unnumlist}

`> (show-city-path (trip (city 'boston) (city 'san-francisco) 3))`
!!!(p) {:.unnumlist}

`#<Path 4514.8 km: Boston - Pittsburgh - Indianapolis -`
!!!(p) {:.unnumlist}

` Kansas-City - Denver - Grand-Jet - Reno - San-Francisco >`
!!!(p) {:.unnumlist}

This example shows how search is susceptible to irregularities in the search space.
It was easy to find the correct path from west to east, but the return trip required more search, because Flagstaff is a falsely promising step.
In general, there may be even worse dead ends lurking in the search space.
Look what happens when we limit the airplane's range to 700 kilometers.
The map is shown in [figure 6.2](#f0015).

![f06-02-9780080571157](images/B9780080571157500066/f06-02-9780080571157.jpg)     
Figure 6.2
!!!(span) {:.fignum}
A Map of Cities within 700 km
If we try to plan a trip from Tampa to Quebec, we can run into problems with the dead end at Wilmington, North Carolina.
With a beam width of 1, the path to Jacksonville and then Wilmington will be tried first.
From there, each step of the path alternates between Atlanta and Wilmington.
The search never gets any closer to the goal.
But with a beam width of 2, the path from Tampa to Atlanta is not discarded, and it is eventually continued on to Indianapolis and eventually to Quebec.
So the capability to back up is essential in avoiding dead ends.

Now for the implementation details.
The function `is` still returns a predicate that tests for a value, but now it accepts : `key` and : `test` keywords:

[ ](#){:#l0265}`(defun is (value &key (key #'identity) (test #'eq1))`
!!!(p) {:.unnumlist}

` "Returns a predicate that tests for a given value."`
!!!(p) {:.unnumlist}

` #'(lambda (path) (funcall test value (funcall key path))))`
!!!(p) {:.unnumlist}

The `path-saver` function returns a function that will take a path as an argument and generate successors paths.
`path-saver` takes as an argument a successor function that operates on bare states.
It calls this function and, for each state returned, builds up a path that extends the existing path and stores the cost of the path so far as well as the estimated total cost:

[ ](#){:#l0270}`(defun path-saver (successors cost-fn cost-left-fn)`
!!!(p) {:.unnumlist}

` #'(lambda (old-path)`
!!!(p) {:.unnumlist}

`   (let ((old-state (path-state old-path)))`
!!!(p) {:.unnumlist}

`    (mapcar`
!!!(p) {:.unnumlist}

`     #'(lambda (new-state)`
!!!(p) {:.unnumlist}

`      (let ((old-cost`
!!!(p) {:.unnumlist}

`         (+ (path-cost-so-far old-path)`
!!!(p) {:.unnumlist}

`           (funcall cost-fn old-state new-state))))`
!!!(p) {:.unnumlist}

`       (make-path`
!!!(p) {:.unnumlist}

`        :state new-state`
!!!(p) {:.unnumlist}

`        :previous old-path`
!!!(p) {:.unnumlist}

`        :cost-so-far old-cost`
!!!(p) {:.unnumlist}

`        :total-cost (+ old-cost (funcall cost-left-fn`
!!!(p) {:.unnumlist}

`            new-state)))))`
!!!(p) {:.unnumlist}

`     (funcall successors old-state)))))`
!!!(p) {:.unnumlist}

By default a path structure would be printed as `#S ( PATH … )`.
But because each path has a `previous` field that is filled by another path, this output would get quite verbose.
That is why we installed `print-path` as the print function for paths when we defined the structure.
It uses the notation `#<…>`, which is a Common Lisp convention for printing output that can not be reconstructed by `read`.
The function `show-city-path` prints a more complete representation of a path.
We also define `map-path` to iterate over a path, collecting values:

[ ](#){:#l0275}`(defun print-path (path &optional (stream t) depth)`
!!!(p) {:.unnumlist}

` (declare (ignore depth))`
!!!(p) {:.unnumlist}

` (format stream "#<Path to ~a cost ~,lf>"`
!!!(p) {:.unnumlist}

`    (path-state path) (path-total-cost path)))`
!!!(p) {:.unnumlist}

`(defun show-city-path (path &optional (stream t))`
!!!(p) {:.unnumlist}

` "Show the length of a path, and the cities along it."`
!!!(p) {:.unnumlist}

` (format stream "#<Path ~,lf km: ~{~:(~a~)~^− ~}>"`
!!!(p) {:.unnumlist}

`    (path-total-cost path)`
!!!(p) {:.unnumlist}

`    (reverse (map-path #'city-name path)))`
!!!(p) {:.unnumlist}

` (values))`
!!!(p) {:.unnumlist}

`(defun map-path (fn path)`
!!!(p) {:.unnumlist}

` "Call fn on each state in the path, collecting results."`
!!!(p) {:.unnumlist}

` (if (null path)`
!!!(p) {:.unnumlist}

`   nil`
!!!(p) {:.unnumlist}

`   (cons (funcall fn (path-state path))`
!!!(p) {:.unnumlist}

`     (map-path fn (path-previous path)))))`
!!!(p) {:.unnumlist}

### [ ](#){:#st0045}Guessing versus Guaranteeing a Good Solution
{:#s0050}
{:.h2hd}

Elementary AI textbooks place a great emphasis on search algorithms that are guaranteed to find the best solution.
However, in practice these algorithms are hardly ever used.
The problem is that guaranteeing the best solution requires looking at a lot of other solutions in order to rule them out.
For problems with large search spaces, this usually takes too much time.
The alternative is to use an algorithm that will probably return a solution that is close to the best solution, but gives no guarantee.
Such algorithms, traditionally known as *non*-*admissible heuristic search* algorithms, can be much faster.

Of the algorithms we have seen so far, best-first search almost, but not quite, guarantees the best solution.
The problem is that it terminâtes a little too early.
Suppose it has calculated three paths, of cost 90, 95 and 110.
It will expand the 90 path next.
Suppose this leads to a solution of total cost 100.
Best-first search will then return that solution.
But it is possible that the 95 path could lead to a solution with a total cost less than 100.
Perhaps the 95 path is only one unit away from the goal, so it could result in a complete path of length 96.
This means that an optimal search should examine the 95 path (but not the 110 path) before exiting.

Depth-first search and beam search, on the other hand, are definitely heuristic algorithms.
Depth-first search finds a solution without any regard to its cost.
With beam search, picking a good value for the beam width can lead to a good, quick solution, while picking the wrong value can lead to failure, or to a poor solution.
One way out of this dilemma is to start with a narrow beam width, and if that does not lead to an acceptable solution, widen the beam and try again.
We will call this *iterative widening*, although that is not a standard term.
There are many variations on this theme, but here is a simple one:

[ ](#){:#l0280}`(defun iter-wide-search (start goal-p successors cost-fn`
!!!(p) {:.unnumlist}

`        &key (width 1) (max 100))`
!!!(p) {:.unnumlist}

` "Search, increasing beam width from width to max.`
!!!(p) {:.unnumlist}

` Return the first solution found at any width."`
!!!(p) {:.unnumlist}

` (dbg :search "; Width: ~d" width)`
!!!(p) {:.unnumlist}

` (unless (> width max)`
!!!(p) {:.unnumlist}

`  (or (beam-search start goal-p successors cost-fn width)`
!!!(p) {:.unnumlist}

`   (iter-wide-search start goal-p successors cost-fn`
!!!(p) {:.unnumlist}

`            :width (+ width 1) :max max))))`
!!!(p) {:.unnumlist}

Here `iter-wide-search` is used to search through a binary tree, failing with beam width 1 and 2, and eventually succeeding with beam width 3:

[ ](#){:#l0285}`> (iter-wide-search 1 (is 12) (finite-binary-tree 15) (diff 12))`
!!!(p) {:.unnumlist}

`Width: 1`
!!!(p) {:.unnumlist}

`;; Search: (1)`
!!!(p) {:.unnumlist}

`;; Search: (3)`
!!!(p) {:.unnumlist}

`;; Search: (7)`
!!!(p) {:.unnumlist}

`;; Search: (14)`
!!!(p) {:.unnumlist}

`; Width: 2`
!!!(p) {:.unnumlist}

`;; Search: (1)`
!!!(p) {:.unnumlist}

`;; Search: (3 2)`
!!!(p) {:.unnumlist}

`;; Search: (7 6)`
!!!(p) {:.unnumlist}

`;; Search: (14 15)`
!!!(p) {:.unnumlist}

`;; Search: (15)`
!!!(p) {:.unnumlist}

`;; Search: NIL`
!!!(p) {:.unnumlist}

`; Width: 3`
!!!(p) {:.unnumlist}

`;; Search: (1)`
!!!(p) {:.unnumlist}

`;; Search: (3 2)`
!!!(p) {:.unnumlist}

`;; Search: (7 6 2)`
!!!(p) {:.unnumlist}

`;; Search: (14 15 6)`
!!!(p) {:.unnumlist}

`;; Search: (15 6)`
!!!(p) {:.unnumlist}

`;; Search: (6)`
!!!(p) {:.unnumlist}

`;; Search: (12 13)`
!!!(p) {:.unnumlist}

`12`
!!!(p) {:.unnumlist}

The name iterative widening is derived from the established term *iterative deepening*.
Iterative deepening is used to control depth-first search when we don't know the depth of the desired solution.
The idea is first to limit the search to a depth of 1, then 2, and so on.
That way we are guaranteed to find a solution at the minimum depth, just as in breadth-first search, but without wasting as much storage space.
Of course, iterative deepening does waste some time because at each increasing depth it repeats all the work it did at the previous depth.
But suppose that the average state has ten successors.
That means that increasing the depth by one results in ten times more search, so only 10% of the time is wasted on repeated work.
So iterative deepening uses only slightly more time and much less space.
We will see it again in [chapters 11](B978008057115750011X.xhtml) and [18](B9780080571157500182.xhtml).

### [ ](#){:#st0050}Searching Graphs
{:#s0055}
{:.h2hd}

So far, `tree-search` has been the workhorse behind all the searching routines.
This is curious, when we consider that the city problem involves a graph that is not a tree at all.
The reason `tree-search` works is that any graph can be treated as a tree, if we ignore the fact that certain nodes are identical.
For example, the graph in [figure 6.3](#f0020) can be rendered as a tree.
[Figure 6.4](#f0025) shows only the top four levels of the tree; each of the bottom nodes (except the 6 s) needs to be expanded further.

![f06-03-9780080571157](images/B9780080571157500066/f06-03-9780080571157.jpg)     
Figure 6.3
!!!(span) {:.fignum}
A Graph with Six Nodes
![f06-04-9780080571157](images/B9780080571157500066/f06-04-9780080571157.jpg)     
Figure 6.4
!!!(span) {:.fignum}
The Corresponding Tree
In searching for paths through the graph of cities, we were implicitly turning the graph into a tree.
That is, if `tree-search` found two paths from Pittsburgh to Kansas City (via Chicago or Indianapolis), then it would treat them as two independent paths, just as if there were two distinct Kansas Cities.
This made the algorithms simpler, but it also doubles the number of paths left to examine.
If the destination is San Francisco, we will have to search for a path from Kansas City to San Francisco twice instead of once.
In fact, even though the graph has only 22 cities, the tree is infinite, because we can go back and forth between adjacent cities any number of times.
So, while it is possible to treat the graph as a tree, there are potential savings in treating it as a true graph.

The function `graph-search` does just that.
It is similar to `tree-search`, but accepts two additional arguments: a comparison function that tests if two states are equal, and a list of states that are no longer being considered, but were examined in the past.
The difference between `graph-search` and `tree-search` is in the call to `new-states`, which generates successors but eliminates states that are in either the list of states currently being considered or the list of old states considered in the past.

[ ](#){:#l0290}`(defun graph-search (states goal-p successors combiner`
!!!(p) {:.unnumlist}

`        &optional (state = #'eq1) old-states)`
!!!(p) {:.unnumlist}

` "Find a state that satisfies goal-p.
Start with states,`
!!!(p) {:.unnumlist}

` and search according to successors and combiner.`
!!!(p) {:.unnumlist}

` Don't try the same state twice."`
!!!(p) {:.unnumlist}

` (dbg :search "~&;; Search: ~a" states)`
!!!(p) {:.unnumlist}

` (cond ((null states) fail)`
!!!(p) {:.unnumlist}

`    ((funcall goal-p (first states)) (first states))`
!!!(p) {:.unnumlist}

`    (t (graph-search`
!!!(p) {:.unnumlist}

`      (funcall`
!!!(p) {:.unnumlist}

`       combiner`
!!!(p) {:.unnumlist}

`       (new-states states successors state = old-states)`
!!!(p) {:.unnumlist}

`       (rest states))`
!!!(p) {:.unnumlist}

`      goal-p successors combiner state =`
!!!(p) {:.unnumlist}

`      (adjoin (first states) old-states`
!!!(p) {:.unnumlist}

`           :test state =)))))`
!!!(p) {:.unnumlist}

`(defun new-states (states successors state = old-states)`
!!!(p) {:.unnumlist}

` "Generate successor states that have not been seen before."`
!!!(p) {:.unnumlist}

` (remove-if`
!!!(p) {:.unnumlist}

`  #'(lambda (state)`
!!!(p) {:.unnumlist}

`   (or (member state states :test state =)`
!!!(p) {:.unnumlist}

`    (member state old-states :test state =)))`
!!!(p) {:.unnumlist}

`   (funcall successors (first states))))`
!!!(p) {:.unnumlist}

Using the successor function `next2`, we can search the graph shown here either as a tree or as a graph.
If we search it as a graph, it takes fewer iterations and less storage space to find the goal.
Of course, there is additional overhead to test for identical states, but on graphs like this one we get an exponential speed-up for a constant amount of overhead.

[ ](#){:#l0295}`(defun next2 (x) (list (+ x 1) (+ x 2)))`
!!!(p) {:.unnumlist}

`> (tree-search '(1) (is 6) #'next2 #'prepend)`
!!!(p) {:.unnumlist}

`;; Search: (1)`
!!!(p) {:.unnumlist}

`;; Search: (2 3)`
!!!(p) {:.unnumlist}

`;; Search: (3 3 4)`
!!!(p) {:.unnumlist}

`;; Search: (3 4 4 5)`
!!!(p) {:.unnumlist}

`;; Search:(4 4 5 4 5)`
!!!(p) {:.unnumlist}

`;; Search: (4 5 4 5 5 6)`
!!!(p) {:.unnumlist}

`;; Search: (5 4 5 5 6 5 6)`
!!!(p) {:.unnumlist}

`;; Search: (4 5 5 6 5 6 6 7)`
!!!(p) {:.unnumlist}

`;; Search: (5 5 6 5 6 6 7 5 6)`
!!!(p) {:.unnumlist}

`;; Search: (5 6 5 6 6 7 5 6 6 7)`
!!!(p) {:.unnumlist}

`;; Search: (6 5 6 6 7 5 6 6 7 6 7)`
!!!(p) {:.unnumlist}

`6`
!!!(p) {:.unnumlist}

`> (graph-search '(1) (is 6) #'next2 #'prepend)`
!!!(p) {:.unnumlist}

`;; Search: (1)`
!!!(p) {:.unnumlist}

`;; Search: (2 3)`
!!!(p) {:.unnumlist}

`;; Search: (3 4)`
!!!(p) {:.unnumlist}

`;; Search: (4 5)`
!!!(p) {:.unnumlist}

`;; Search: (5 6)`
!!!(p) {:.unnumlist}

`;; Search: (6 7)`
!!!(p) {:.unnumlist}

`6`
!!!(p) {:.unnumlist}

The next step is to extend the `graph-search` algorithm to handle paths.
The complication is in deciding which path to keep when two paths reach the same state.
If we have a cost function, then the answer is easy: keep the path with the cheaper cost.
Best-first search of a graph removing duplicate states is called *A* * *search*.

A* search is more complicated than `graph-search` because of the need both to add and to delete paths to the lists of current and old paths.
For each new successor state, there are three possibilites.
The new state may be in the list of current paths, in the list of old paths, or in neither.
Within the first two cases, there are two subcases.
If the new path is more expensive than the old one, then ignore the new path—it can not lead to a better solution.
If the new path is cheaper than a corresponding path in the list of current paths, then replace it with the new path.
If it is cheaper than a corresponding path in the list of the old paths, then remove that old path, and put the new path in the list of current paths.

Also, rather than sort the paths by total cost on each iteration, they are kept sorted, and new paths are inserted into the proper place one at a time using `insert-path`.
Two more functions, `better-path` and `find-path`, are used to compare paths and see if a state has already appeared.

[ ](#){:#l0300}`(defun a*-search (paths goal-p successors cost-fn cost-left-fn`
!!!(p) {:.unnumlist}

`      &optional (state = #'eq1) old-paths)`
!!!(p) {:.unnumlist}

` "Find a path whose state satisfies goal-p.
Start with paths,`
!!!(p) {:.unnumlist}

` and expand successors, exploring least cost first.`
!!!(p) {:.unnumlist}

` When there are duplicate states, keep the one with the`
!!!(p) {:.unnumlist}

` lower cost and discard the other."`
!!!(p) {:.unnumlist}

` (dbg :search ";; Search: ~a" paths)`
!!!(p) {:.unnumlist}

` (cond`
!!!(p) {:.unnumlist}

`  ((null paths) fail)`
!!!(p) {:.unnumlist}

`  ((funcall goal-p (path-state (first paths)))`
!!!(p) {:.unnumlist}

`   (values (first paths) paths))`
!!!(p) {:.unnumlist}

`  (t (let* ((path (pop paths))`
!!!(p) {:.unnumlist}

`     (state (path-state path)))`
!!!(p) {:.unnumlist}

`    ;; Update PATHS and OLD-PATHS to reflect`
!!!(p) {:.unnumlist}

`    ;; the new successors of STATE:`
!!!(p) {:.unnumlist}

`    (setf old-paths (insert-path path old-paths))`
!!!(p) {:.unnumlist}

`    (dolist (state2 (funcall successors state))`
!!!(p) {:.unnumlist}

`     (let* ((cost (+ (path-cost-so-far path)`
!!!(p) {:.unnumlist}

`            (funcall cost-fn state state2)))`
!!!(p) {:.unnumlist}

`       (cost2 (funcall cost-left-fn state2))`
!!!(p) {:.unnumlist}

`       (path2 (make-path`
!!!(p) {:.unnumlist}

`            :state state2 :previous path`
!!!(p) {:.unnumlist}

`            :cost-so-far cost`
!!!(p) {:.unnumlist}

`            :total-cost (+ cost cost2)))`
!!!(p) {:.unnumlist}

`       (old nil)`
!!!(p) {:.unnumlist}

`      ;; Place the new path, path2, in the right list:`
!!!(p) {:.unnumlist}

`      (cond`
!!!(p) {:.unnumlist}

`       ((setf old (find-path state2 paths state =))`
!!!(p) {:.unnumlist}

`       (when (better-path path2 old)`
!!!(p) {:.unnumlist}

`        (setf paths (insert-path`
!!!(p) {:.unnumlist}

`             path2 (delete old paths)))))`
!!!(p) {:.unnumlist}

`       ((setf old (find-path state2 old-paths state =))`
!!!(p) {:.unnumlist}

`       (when (better-path path2 old)`
!!!(p) {:.unnumlist}

`        (setf paths (insert-path path2 paths))`
!!!(p) {:.unnumlist}

`        (setf old-paths (delete old old-paths))))`
!!!(p) {:.unnumlist}

`       (t (setf paths (insert-path path2 paths))))))`
!!!(p) {:.unnumlist}

`     ;; Finally, call A* again with the updated path lists:`
!!!(p) {:.unnumlist}

`     (a*-search paths goal-p successors cost-fn cost-left-fn`
!!!(p) {:.unnumlist}

`     state = old-paths)))))`
!!!(p) {:.unnumlist}

Here are the three auxiliary functions:

[ ](#){:#l0305}`(defun find-path (state paths state =)`
!!!(p) {:.unnumlist}

` "Find the path with this state among a list of paths."`
!!!(p) {:.unnumlist}

` (find state paths :key #'path-state :test state =))`
!!!(p) {:.unnumlist}

`(defun better-path (pathl path2)`
!!!(p) {:.unnumlist}

` "Is path1 cheaper than path2?"`
!!!(p) {:.unnumlist}

` (< (path-total-cost path1) (path-total-cost path2)))`
!!!(p) {:.unnumlist}

`(defun insert-path (path paths)`
!!!(p) {:.unnumlist}

` "Put path into the right position, sorted by total cost."`
!!!(p) {:.unnumlist}

` ;; MERGE is a built-in function`
!!!(p) {:.unnumlist}

` (merge 'list (list path) paths #'< :key #'path-total-cost))`
!!!(p) {:.unnumlist}

`(defun path-states (path)`
!!!(p) {:.unnumlist}

` "Collect the states along this path."`
!!!(p) {:.unnumlist}

` (if (null path)`
!!!(p) {:.unnumlist}

`   nil`
!!!(p) {:.unnumlist}

`   (cons (path-state path)`
!!!(p) {:.unnumlist}

`      (path-states (path-previous path)))))`
!!!(p) {:.unnumlist}

Below we use `a*-search` to search for 6 in the graph previously shown in [figure 6.3](#f0020).
The cost function is a constant 1 for each step.
In other words, the total cost is the length of the path.
The heuristic evaluation function is just the difference from the goal.
The A* algorithm needs just three search steps to come up with the optimal solution.
Contrast that to the graph search algorithm, which needed five steps, and the tree search algorithm, which needed ten steps-and neither of them found the optimal solution.

[ ](#){:#l0310}`> (path-states`
!!!(p) {:.unnumlist}

`   (a*-search (list (make-path :state 1)) (is 6)`
!!!(p) {:.unnumlist}

`          #'next2 #'(lambda (x y) 1) (diff 6)))`
!!!(p) {:.unnumlist}

`;; Search: (#<Path to 1 cost 0.0 >)`
!!!(p) {:.unnumlist}

`;; Search: (#<Path to 3 cost 4.0 > #<Path to 2 cost 5.0 >)`
!!!(p) {:.unnumlist}

`;; Search: (#<Path to 5 cost 3.0 > #<Path to 4 cost 4.0 >`
!!!(p) {:.unnumlist}

`        #<Path to 2 cost 5.0 >)`
!!!(p) {:.unnumlist}

`;; Search: (#<Path to 6 cost 3.0 > #<Path to 7 cost 4.0 >`
!!!(p) {:.unnumlist}

`        #<Path to 4 cost 4.0 > #<Path to 2 cost 5.0 >)`
!!!(p) {:.unnumlist}

`(6 5 3 1)`
!!!(p) {:.unnumlist}

It may seem limiting that these search functions all return a single answer.
In some applications, we may want to look at several solutions, or at all possible solutions.
Other applications are more naturally seen as optimization problems, where we don't know ahead of time what counts as achieving the goal but are just trying to find some action with a low cost.

It turns out that the functions we have defined are not limiting at all in this respect.
They can be used to serve both these new purposes—provided we carefully specify the goal predicate.
To find all solutions to a problem, all we have to do is pass in a goal predicate that always fails, but saves all the solutions in a list.
The goal predicate will see all possible solutions and save away just the ones that are real solutions.
Of course, if the search space is infinite this will never terminate, so the user has to be careful in applying this technique.
It would also be possible to write a goal predicate that stopped the search after finding a certain number of solutions, or after looking at a certain number of states.
Here is a function that finds all solutions, using beam search:

[ ](#){:#l0315}`(defun search-all (start goal-p successors cost-fn beam-width)`
!!!(p) {:.unnumlist}

` "Find all solutions to a search problem, using beam search."`
!!!(p) {:.unnumlist}

` ;; Be careful: this can lead to an infinite loop.`
!!!(p) {:.unnumlist}

` (let ((solutions nil))`
!!!(p) {:.unnumlist}

`  (beam-search`
!!!(p) {:.unnumlist}

`   start #'(lambda (x)`
!!!(p) {:.unnumlist}

`       (when (funcall goal-p x) (push x solutions))`
!!!(p) {:.unnumlist}

`       nil)`
!!!(p) {:.unnumlist}

`   successors cost-fn beam-width)`
!!!(p) {:.unnumlist}

` solutions))`
!!!(p) {:.unnumlist}

## [ ](#){:#st0055}6.5 GPS as Search
{:#s0060}
{:.h1hd}

The GPS program can be seen as a problem in search.
For example, in the three-block blocks world, there are only 13 different states.
They could be arranged in a graph and searched just as we searched for a route between cities.
[Figure 6.5](#f0030) shows this graph.

![f06-05-9780080571157](images/B9780080571157500066/f06-05-9780080571157.jpg)     
Figure 6.5
!!!(span) {:.fignum}
The Blocks World as a Graph
The function `search-gps` does just that.
Like the gps function on [page 135](B9780080571157500042.xhtml#p135), it computes a final state and then picks out the actions that lead to that state.
But it computes the state with a beam search.
The goal predicate tests if the current state satisfies every condition in the goal, the successor function finds all applicable operators and applies them, and the cost function simply sums the number of actions taken so far, plus the number of conditions that are not yet satisfied:

[ ](#){:#l0320}`(defun search-gps (start goal &optional (beam-width 10))`
!!!(p) {:.unnumlist}

` "Search for a sequence of operators leading to goal."`
!!!(p) {:.unnumlist}

` (find-all-if`
!!!(p) {:.unnumlist}

`  #'action-p`
!!!(p) {:.unnumlist}

`  (beam-search`
!!!(p) {:.unnumlist}

`   (cons '(start) start)`
!!!(p) {:.unnumlist}

`   #'(lambda (state) (subsetp goal state :test #'equal))`
!!!(p) {:.unnumlist}

`   #'gps-successors`
!!!(p) {:.unnumlist}

`   #'(lambda (state)`
!!!(p) {:.unnumlist}

`    (+ (count-if #'action-p state)`
!!!(p) {:.unnumlist}

`     (count-if #'(lambda (con)`
!!!(p) {:.unnumlist}

`          (not (member-equal con state)))`
!!!(p) {:.unnumlist}

`        goal)))`
!!!(p) {:.unnumlist}

`   beam-width)))`
!!!(p) {:.unnumlist}

Here is the successor function:

[ ](#){:#l0325}`(defun gps-successors (state)`
!!!(p) {:.unnumlist}

` "Return a list of states reachable from this one using ops."`
!!!(p) {:.unnumlist}

` (mapcar`
!!!(p) {:.unnumlist}

`  #'(lambda (op)`
!!!(p) {:.unnumlist}

`  (append`
!!!(p) {:.unnumlist}

`   (remove-if #'(lambda (x)`
!!!(p) {:.unnumlist}

`          (member-equal x (op-del-list op)))`
!!!(p) {:.unnumlist}

`        state)`
!!!(p) {:.unnumlist}

`   (op-add-list op)))`
!!!(p) {:.unnumlist}

`  (applicable-ops state)))`
!!!(p) {:.unnumlist}

`(defun applicable-ops (state)`
!!!(p) {:.unnumlist}

` "Return a list of all ops that are applicable now."`
!!!(p) {:.unnumlist}

` (find-all-if`
!!!(p) {:.unnumlist}

`  #'(lambda (op)`
!!!(p) {:.unnumlist}

`    (subsetp (op-preconds op) state :test #'equal))`
!!!(p) {:.unnumlist}

`  *ops*))`
!!!(p) {:.unnumlist}

The search technique finds good solutions quickly for a variety of problems.
Here we see the solution to the Sussman anomaly in the three-block blocks world:

[ ](#){:#l0330}`(setf start '((c on a) (a on table) (b on table) (space on c)`
!!!(p) {:.unnumlist}

`      (space on b) (space on table)))`
!!!(p) {:.unnumlist}

`> (search-gps start '((a on b) (b on c)))`
!!!(p) {:.unnumlist}

`((START)`
!!!(p) {:.unnumlist}

` (EXECUTING (MOVE C FROM A TO TABLE))`
!!!(p) {:.unnumlist}

` (EXECUTING (MOVE B FROM TABLE TO C))`
!!!(p) {:.unnumlist}

` (EXECUTING (MOVE A FROM TABLE TO B)))`
!!!(p) {:.unnumlist}

`> (search-gps start '((b on c) (a on b)))`
!!!(p) {:.unnumlist}

`((START)`
!!!(p) {:.unnumlist}

` (EXECUTING (MOVE C FROM A TO TABLE))`
!!!(p) {:.unnumlist}

` (EXECUTING (MOVE B FROM TABLE TO C))`
!!!(p) {:.unnumlist}

` (EXECUTING (MOVE A FROM TABLE TO B)))`
!!!(p) {:.unnumlist}

In these solutions we search forward from the start to the goal; this is quite different from the means-ends approach of searching backward from the goal for an appropriate operator.
But we could formulate means-ends analysis as forward search simply by reversing start and goal: GPS's goal state is the search's start state, and the search's goal predicate tests to see if a state matches GPS's start state.
This is left as an exercise.

## [ ](#){:#st0060}6.6 History and References
{:#s0065}
{:.h1hd}

Pattern matching is one of the most important tools for AI.
As such, it is covered in most textbooks on Lisp.
Good treatments include Abelson and Sussman (1984), [Wilensky (1986)](B9780080571157500285.xhtml#bb1390), [Winston and Horn (1988)](B9780080571157500285.xhtml#bb1410), and [Kreutzer and McKenzie (1990)](B9780080571157500285.xhtml#bb0680).
An overview is presented in the “pattern-matching” entry in *Encyclopedia of AI* ([Shapiro 1990](B9780080571157500285.xhtml#bb1085)).

Nilsson's *Problem*-*Solving Methods in Artificial Intelligence* (1971) was an early text-book that emphasized search as the most important defining characteristic of AI.
More recent texts give less importance to search; Winston's *Artificial Intelligence* (1984) gives a balanced overview, and his *Lisp* (1988) provides implementations of some of the algorithms.
They are at a lower level of abstraction than the ones in this chapter.
Iterative deepening was first presented by [Korf (1985)](B9780080571157500285.xhtml#bb0640), and iterative broadening by [Ginsberg and Harvey (1990)](B9780080571157500285.xhtml#bb0470).

## [ ](#){:#st0065}6.7 Exercises
{:#s0070}
{:.h1hd}

**Exercise 6**.**3** [**m**] Write a version of `interaetive-interpreter` that is more general than the one defined in this chapter.
Decide what features can be specified, and provide defaults for them.

**Exercise 6**.**4** [**m**] Define a version of `compose` that allows any number of arguments, not just two.
Hint: You may want to use the function `reduce`.

**Exercise 6**.**5** [**m**] Define a version of `compose` that allows any number of arguments but is more efficient than the answer to the previous exercise.
Hint: try to make decisions when `compose` is called to build the resulting function, rather than making the same decisions over and over each time the resulting function is called.

**Exercise 6**.**6** [**m**] One problem with `pat-match` is that it gives special significance to symbols starting with ?, which means that they can not be used to match a literal pattern.
Define a pattern that matches the input literally, so that such symbols can be matched.

**Exercise 6**.**7** [**m**] Discuss the pros and cons of data-driven programming compared to the conventional approach.

**Exercise 6**.**8** [**m**] Write a version of `tree-search` using an explicit loop rather than recursion.

**Exercise 6**.**9** [**m**] The `sorter` function is inefficient for two reasons: it calls `append`, which has to make a copy of the first argument, and it sorts the entire result, rather than just inserting the new states into the already sorted *old* states.
Write a more efficient `sorter`.

**Exercise 6**.**10** [**m**] Write versions of `graph-search` and `a*-search` that use hash tables rather than lists to test whether a state has been seen before.

**Exercise 6**.**11** [**m**] Write a function that calls `beam-search` to find the first *n* solutions to a problem and returns them in a list.

**Exercise 6**.**12** [**m**] On personal computers without floating-point hardware, the `air-distance` calculation will be rather slow.
If this is a problem for you, arrange to compute the `xyz-coords` of each city only once and then store them, or store a complete table of air distances between cities.
Also precompute and store the neighbors of each city.

**Exercise 6**.**13** [**d**] Write a version of GPS that uses A* search instead of beam search.
Compare the two versions in a variety of domains.

**Exercise 6**.**14** [**d**] Write a version of GPS that allows costs for each operator.
For example, driving the child to school might have a cost of 2, but calling a limousine to transport the child might have a cost of 100.
Use these costs instead of a constant cost of 1 for each operation.

**Exercise 6**.**15** [**d**] Write a version of GPS that uses the searching tools but does means-ends analysis.

## [ ](#){:#st0070}6.8 Answers
{:#s0075}
{:.h1hd}

**Answer 6**.**2** Unfortunately, `pat-match` does not always find the answer.
The problem is that it will only rebind a segment variable based on a failure to match the rest of the pattern after the segment variable.
In all the examples above, the "rest of the pattern after the segment variable" was the whole pattern, so `pat-match` always worked properly.
But if a segment variable appears nested inside a list, then the rest of the segment variable's sublist is only a part of the rest of the whole pattern, as the following example shows:

[ ](#){:#t0035}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `> (pat-match` | `'(((?* ?x) (?* ?y)) ?x ?y)` |
| | `'((a b c d ) (a b) (c d)))`⇒ `NIL` |

The correct answer with `?x` bound to `(a b)` and `?y` bound to `(c d)` is not found because the inner segment match succeeds with `?x` bound to `( )` and `?y` bound to `(a b c d)`, and once we leave the inner match and return to the top level, there is no going back for alternative bindings.

**Answer 6**.**3** The following version lets the user specify all four components of the prompt-read-eval-print loop, as well as the streams to use for input and output.
Defaults are set up as for a Lisp interpreter.

[ ](#){:#l0335}`(defun interactive-interpreter`
!!!(p) {:.unnumlist}

`    (&key (read #'read) (eval #'eval) (print #'print)`
!!!(p) {:.unnumlist}

`     (prompt "> ") (input t) (output t))`
!!!(p) {:.unnumlist}

` "Read an expression, evaluate it, and print the result."`
!!!(p) {:.unnumlist}

` (loop`
!!!(p) {:.unnumlist}

`  (fresh-line output)`
!!!(p) {:.unnumlist}

`  (princ prompt output)`
!!!(p) {:.unnumlist}

`   (funcall print (funcall eval (funcall read input))`
!!!(p) {:.unnumlist}

`       output)))`
!!!(p) {:.unnumlist}

Here is another version that does all of the above and also handles multiple values and binds the various "history variables" that the Lisp top-level binds.

[ ](#){:#l0340}`(defun interactive-interpreter`
!!!(p) {:.unnumlist}

`   (&key (read #'read) (eval #'eval) (print #'print)`
!!!(p) {:.unnumlist}

`   (prompt "> ") (input t) (output t))`
!!!(p) {:.unnumlist}

` "Read an expression, evaluate it, and print the result(s).`
!!!(p) {:.unnumlist}

` Does multiple values and binds: * ** ***−+ ++ +++/ // ///"`
!!!(p) {:.unnumlist}

` (let (* ** ***−+ ++ +++/ // /// vais)`
!!!(p) {:.unnumlist}

`  ;; The above variables are all special, except VALS`
!!!(p) {:.unnumlist}

`  ;; The variable - holds the current input`
!!!(p) {:.unnumlist}

`  ;; * *** *** are the 3 most recent values`
!!!(p) {:.unnumlist}

`  ;; + ++ +++ are the 3 most recent inputs`
!!!(p) {:.unnumlist}

`  ;;/ // /// are the 3 most recent lists of multiple-values`
!!!(p) {:.unnumlist}

`  (loop`
!!!(p) {:.unnumlist}

`   (fresh-line output)`
!!!(p) {:.unnumlist}

`   (princ prompt output)`
!!!(p) {:.unnumlist}

`   ;; First read and evaluate an expression`
!!!(p) {:.unnumlist}

`   (setf - (funcall read input)`
!!!(p) {:.unnumlist}

`     vals (multiple-value-list (funcall eval -)))`
!!!(p) {:.unnumlist}

`   ;; Now update the history variables`
!!!(p) {:.unnumlist}

[ ](#){:#t0040}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `(setf +++ ++` | `/// //` | `*** (first ///)` |
| `++ +` | `// /` | `** (first //)` |
| `+ -` | `/ vais` | `* (first /))` |

[ ](#){:#l0345}`   ;; Finally print the computed value(s)`
!!!(p) {:.unnumlist}

`   (dolist (value vais)`
!!!(p) {:.unnumlist}

`    (funcall print value output)))))`
!!!(p) {:.unnumlist}

**Answer 6**.**4**

[ ](#){:#l0350}`(defun compose (&rest functions)`
!!!(p) {:.unnumlist}

` "Return the function that is the composition of all the args.`
!!!(p) {:.unnumlist}

` i.e.
(compose f g h) = (lambda (x) (f (g (h x)))).`
!!!(p) {:.unnumlist}

` " #'(lambda (x)`
!!!(p) {:.unnumlist}

`   (reduce #'funcall functions :from-end t .-initial-value x)))`
!!!(p) {:.unnumlist}

**Answer 6**.**5**

[ ](#){:#l0355}`(defun compose (&rest functions)`
!!!(p) {:.unnumlist}

` "Return the function that is the composition of all the args.`
!!!(p) {:.unnumlist}

` i.e.
(compose f g h) = (lambda (x) (f (g (h x))))."`
!!!(p) {:.unnumlist}

` (case (length functions)`
!!!(p) {:.unnumlist}

`  (0 #'identity)`
!!!(p) {:.unnumlist}

`  (1 (first functions))`
!!!(p) {:.unnumlist}

`  (2 (let ((f (first functions))`
!!!(p) {:.unnumlist}

`      (g (second functions)))`
!!!(p) {:.unnumlist}

`    #'(lambda (x) (funcall f (funcall g x)))))`
!!!(p) {:.unnumlist}

`  (t #'(lambda (x)`
!!!(p) {:.unnumlist}

`     (reduce #'funcall functions :from-end t`
!!!(p) {:.unnumlist}

`         :initia1-value x)))))`
!!!(p) {:.unnumlist}

**Answer 6**.**8**

[ ](#){:#l0360}`(defun tree-search (states goal-p successors combiner)`
!!!(p) {:.unnumlist}

` "Find a state that satisfies goal-p.
Start with states,`
!!!(p) {:.unnumlist}

` and search according to successors and combiner."`
!!!(p) {:.unnumlist}

` (loop`
!!!(p) {:.unnumlist}

`  (cond ((null states) (RETURN fail))`
!!!(p) {:.unnumlist}

`     ((funcall goal-p (first states))`
!!!(p) {:.unnumlist}

`     (RETURN (first states))`
!!!(p) {:.unnumlist}

`     (t (setf states`
!!!(p) {:.unnumlist}

`         (funcall combiner`
!!!(p) {:.unnumlist}

`             (funcall successors (first states))`
!!!(p) {:.unnumlist}

`             (rest states))))))))`
!!!(p) {:.unnumlist}

**Answer 6**.**9**

[ ](#){:#l0365}`(defun sorter (cost-fn)`
!!!(p) {:.unnumlist}

` "Return a combiner function that sorts according to cost-fn."`
!!!(p) {:.unnumlist}

` #'(lambda (new old)`
!!!(p) {:.unnumlist}

`   (merge 'list (sort new #'> :key cost-fn)`
!!!(p) {:.unnumlist}

`     old #'> :key cost-fn)))`
!!!(p) {:.unnumlist}

**Answer 6**.**11**

[ ](#){:#l0370}`(defun search-n (start n goal-p successors cost-fn beam-width)`
!!!(p) {:.unnumlist}

` "Find n solutions to a search problem, using beam search."`
!!!(p) {:.unnumlist}

` (let ((solutions nil))`
!!!(p) {:.unnumlist}

`  (beam-search`
!!!(p) {:.unnumlist}

`   start #'(lambda (x)`
!!!(p) {:.unnumlist}

`     (cond ((not (funcall goal-p x)) nil)`
!!!(p) {:.unnumlist}

`       ((= n 0) x)`
!!!(p) {:.unnumlist}

`       (t (decf n)`
!!!(p) {:.unnumlist}

`         (push x solutions)`
!!!(p) {:.unnumlist}

`         nil)))`
!!!(p) {:.unnumlist}

`   successors cost-fn beam-width)`
!!!(p) {:.unnumlist}

`  solutions))`
!!!(p) {:.unnumlist}

----------------------

[1](#xfn0015){:#np0015} The macro `handler-case` is only in ANSI Common Lisp.
!!!(p) {:.ftnote1}

[2](#xfn0020){:#np0020} An alternative would be to reserve the question mark for variables only and use another notation for these match operators.
Keywords would be a good choice, such as : `and, : or,``: is`, etc.
!!!(p) {:.ftnote1}

[3](#xfn0025){:#np0025} The built-in constant `most-positive-fixnum` is a large integer, the largest that can be expressed without using bignums.
Its value depends on the implementation, but in most Lisps it is over 16 million.
!!!(p) {:.ftnote1}

[4](#xfn0030){:#np0030} In [chapter 8](B978008057115750008X.xhtml) we will see an example where the fog did lift: symbolic integration was once handled as a problem in search, but new mathematical results now make it possible to solve the same class of integration problems without search.
!!!(p) {:.ftnote1}

[5](#xfn0035){:#np0035} The astute reader will recognize that this graph is not a tree.
The difference between trees and graphs and the implications for searching will be covered later.
!!!(p) {:.ftnote1}

