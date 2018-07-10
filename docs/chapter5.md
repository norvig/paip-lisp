# Chapter 5
## ELIZA: Dialog with a Machine

> *It is said that to explain is to explain away.*

> –Joseph Weizenbaum

> MIT computer scientist

This chapter and the rest of part I will examine three more well-known AI programs of the 1960s.
ELIZA !!!(span) {:.smallcaps} held a conversation with the user in which it simulated a psychotherapist.
STUDENT !!!(span) {:.smallcaps} solved word problems of the kind found in high school algebra books, and MACSYMA !!!(span) {:.smallcaps} solved a variety of symbolic mathematical problems, including differential and integral calculus.
We will develop versions of the first two programs that duplicate most of the essential features, but for the third we will implement only a tiny fraction of the original program’s capabilities.

All three programs make heavy use of a technique called pattern matching.
Part I serves to show the versatility–and also the limitations–of this technique.

Of the three programs, the first two process input in plain English, and the last two solve non-trivial problems in mathematics, so there is some basis for describing them as being “intelligent.” On the other hand, we shall see that this intelligence is largely an illusion, and that ELIZA !!!(span) {:.smallcaps} in particular was actually designed to demonstrate this illusion, not to be a “serious” AI program.

ELIZA !!!(span) {:.smallcaps} was one of the first programs to feature English output as well as input.
The program was named after the heroine of *Pygmalion,* who was taught to speak proper English by a dedicated teacher.
ELIZA’S !!!(span) {:.smallcaps} principal developer, MIT professor Joseph Weizenbaum, published a paper on ELIZA !!!(span) {:.smallcaps} in the January 1966 issue of the *Communications of the Association for Computing Machinery.* The introduction to that paper is reproduced in its entirety here:[ ](#){:#p0035}

> *It is said that to explain is to explain away.
This maxim is nowhere so well fulfilled as in the area of computer programming, especially in what is called heuristic programming and artificial intelligence.
For in those realms machines are made to behave in wondrous ways, often sufficient to dazzle even the most experienced observer.
But once a particular program is unmasked, once its inner workings are explained in language sufficiently plain to induce understanding, its magic crumbles away; it stands revealed as a mere collection of procedures, each quite comprehensible.
The observer says to himself, “I could have written that.” With that thought he moves the program in question from the shelf marked “intelligent,” to that reserved for curios, fit to be discussed only with people less enlightened than he.*

> *The object of this paper is to cause just such a re-evaluation of the program about to be “explained.” Few programs ever needed it more.*

Let me point out in passing that the main object of this book is to cause the reader to say to him or herself, “I could have written that.” It is hoped that in some cases this will be because the reader has gained something, and not solely because the programs in question have lost their ability to dazzle.

Let’s start our investigation of ELIZA !!!(span) {:.smallcaps} by looking at a sample dialog from the Weizenbaum article.
ELIZA !!!(span) {:.smallcaps} was designed to be instructed by a variety of scripts, or patterns of interaction.
In fact it had several scripts, including ones in German and Welsh.
But the only script that was used to any extent was in English, and it made ELIZA !!!(span) {:.smallcaps} emulate a Rogerian psychoanalyst.
Rogerians are “nondirective”–they try to let the patient reveal himself.
This is an easy script for ELIZA !!!(span) {:.smallcaps} to follow, because it involves being passive and reacting, rather than volunteering new information.
In the listing that follows, the user’s input is in mixed case, and ELIZA’S !!!(span) {:.smallcaps} response is in capitals:

[ ](#){:#l0010}`Men are all alike.`
!!!(p) {:.unnumlist}

`IN WHAT WAY`
!!!(p) {:.unnumlist}

`They’re always bugging us about something or another.`
!!!(p) {:.unnumlist}

`CAN YOU BE MORE SPECIFIC`
!!!(p) {:.unnumlist}

`Well, my boyfriend made me come here.`
!!!(p) {:.unnumlist}

`YOUR BOYFRIEND MADE YOU COME HERE`
!!!(p) {:.unnumlist}

`He says I’m depressed much of the time.`
!!!(p) {:.unnumlist}

`I AM SORRY TO HEAR YOU ARE DEPRESSED`
!!!(p) {:.unnumlist}

  `It’s true.
I am unhappy.`
!!!(p) {:.unnumlist}

  `DO YOU THINK COMING HERE WILL HELP YOU NOT TO BE UNHAPPY`
!!!(p) {:.unnumlist}

  `I need some help, that much seems certain.`
!!!(p) {:.unnumlist}

  `WHAT WOULD IT MEAN TO YOU IF YOU GOT SOME HELP`
!!!(p) {:.unnumlist}

  `Perhaps I could learn to get along with my mother.`
!!!(p) {:.unnumlist}

  `TELL ME MORE ABOUT YOUR FAMILY`
!!!(p) {:.unnumlist}

This seems quite impressive; the program is responding reasonably to a variety of inputs and appears to be exhibiting true “understanding.” But, as Weizenbaum explains, this appearance is an illusion, and the program has finessed the difficult task of understanding by carefully recognizing, transforming, and echoing pieces of the input.
The procedure is to look for specific patterns, based on a key word or words in the input.
For example, whenever the program sees the words “alike” or “same,” it prints a message asking about similarity, in this case, “`IN WHAT WAY.`” Other patterns are more complicated.
In the next to last exchange, ELIZA !!!(span) {:.smallcaps} first throws away everything after the comma, and then uses a pattern that looks for input of the form “I need X” and transforms it to the output “What would it mean to you if you got X,” where X is the phrase “some help” in this case.
The great advantage of this approach is that the response will make sense for a variety of inputs: “I need a vacation” will elicit “What would it mean to you if you got a vacation.” This will work automatically, without having to tell the program the difference between “some help” and “a vacation.” The disadvantage is that the program will sometimes make silly responses: “I need this like a hole in the head” will be answered with “What would it mean to you if you got this like a hole in the head.” In fact, the program only makes any sense at all if it is receiving input from someone who makes sense–and someone who interprets its answers charitably.
When given “I need gibble plix zzq,” ELIZA !!!(span) {:.smallcaps} would reply “ What would it mean to you if you got gibble plix zzq,” without any inkling that something is wrong.
Another example of the original ELIZA !!!(span) {:.smallcaps} program has it communicating with another program, PARRY, !!!(span) {:.smallcaps} which was written by Dr.
Kenneth Colby to mimic paranoid behavior.
Part of the conversation follows, with ELIZA !!!(span) {:.smallcaps} again in capitals and PARRY !!!(span) {:.smallcaps} in mixed case:

[ ](#){:#l0015}`I don’t understand your motives.`
!!!(p) {:.unnumlist}

`WHY DON’T YOU UNDERSTAND MY MOTIVES?`
!!!(p) {:.unnumlist}

`Shouldn’t I be?`
!!!(p) {:.unnumlist}

`WHY DO YOU SAY THAT JUST NOW?`
!!!(p) {:.unnumlist}

`That’s just the way it is.`
!!!(p) {:.unnumlist}

`SUPPOSE THAT’S JUST THE WAY IT WERE NOT.`
!!!(p) {:.unnumlist}

`So?`
!!!(p) {:.unnumlist}

`I AM NOT SURE I UNDERSTAND YOU FULLY.`
!!!(p) {:.unnumlist}

`You harp on the same questions too much.`
!!!(p) {:.unnumlist}

`WHAT DOES THAT SIMILARITY SUGGEST TO YOU?`
!!!(p) {:.unnumlist}

`I don’t get you.`
!!!(p) {:.unnumlist}

`DO YOU WISH THAT YOU DID GET I?`
!!!(p) {:.unnumlist}

This suggests that neither ELIZA !!!(span) {:.smallcaps} nor PARRY !!!(span) {:.smallcaps} had a deep understanding of the dialog.
When there is a human to take on most of the burden of carrying forth the conversation, these programs can be convincing, but when the human is taken out of the conversation, the weaknesses of the programs are revealed.

## [ ](#){:#st0010}5.1 Describing and Specifying ELIZA
!!!(span) {:.smallcaps}

{:#s0010}
{:.h1hd}

Now that we have an idea of what ELIZA !!!(span) {:.smallcaps} is like, we can begin the description and specification of the program, and eventually move to the implementation and debugging.

The ELIZA !!!(span) {:.smallcaps} algorithm can be described simply as: (1) read an input, (2) find a pattern that matches the input, (3) transform the input into a response, and (4) print the response.
These four steps are repeated for each input.

The specification and implementation of steps (1) and (4) are trivial: for (1), use the built-in `read` function to read a list of words, and for (4) use `print` to print the list of words in the response.

Of course, there are some drawbacks to this specification.
The user will have to type a real list–using parentheses–and the user can’t use characters that are special to `read`, like quotation marks, commas, and periods.
So our input won’t be as unconstrained as in the sample dialog, but that’s a small price to pay for the convenience of having half of the problem neatly solved.

## [ ](#){:#st0015}5.2 Pattern Matching
{:#s0015}
{:.h1hd}

The hard part comes with steps (2) and (3)–this notion of pattern matching and transformation.
There are four things to be concerned with: a general pattern and response, and a specific input and transformation of that input.
Since we have agreed to represent the input as a list, it makes sense for the other components to be lists too.
For example, we might have:

[ ](#){:#l0020}Pattern: `(i need a X)`
!!!(p) {:.unnumlist}

Response: `(what would it mean to you if you got a X ?)`
!!!(p) {:.unnumlist}

[ ](#){:#l0025}Input: `(i need a vacation)`
!!!(p) {:.unnumlist}

Transformation: `(what would it mean to you if you got a vacation ?)`
!!!(p) {:.unnumlist}

The pattern matcher must match the literals `i` with `i`, `need` with `need`, and `a` with `a`, as well as match the variable `X` with `vacation`.
This presupposes that there is some way of deciding that `X` is a variable and that `need` is not.
We must then arrange to substitute `vacation` for `X` within the response, in order to get the final transformation.

Ignoring for a moment the problem of transforming the pattern into the response, we can see that this notion of pattern matching is just a generalization of the Lisp function `equal`.
Below we show the function `simple-equal`, which is like the built-in function `equal`,[1](#fn0010){:#xfn0010} and the function `pat-match`, which is extended to handle pattern-matching variables:

[ ](#){:#l0030}`(defun simple-equal (x y)`
!!!(p) {:.unnumlist}

` “Are x and y equal?
(Don’t check inside strings.)”`
!!!(p) {:.unnumlist}

` (if (or (atom x) (atom y))`
!!!(p) {:.unnumlist}

`  (eql x y)`
!!!(p) {:.unnumlist}

`  (and (simple-equal (first x) (first y))`
!!!(p) {:.unnumlist}

`   (simple-equal (rest x) (rest y)))))`
!!!(p) {:.unnumlist}

`(defun pat-match (pattern input)`
!!!(p) {:.unnumlist}

` “Does pattern match input?
Any variable can match anything.”`
!!!(p) {:.unnumlist}

` (if (variable-p pattern)`
!!!(p) {:.unnumlist}

`  t`
!!!(p) {:.unnumlist}

`  (if (or (atom pattern) (atom input))`
!!!(p) {:.unnumlist}

`   (eql pattern input)`
!!!(p) {:.unnumlist}

`   (and (pat-match (first pattern) (first input))`
!!!(p) {:.unnumlist}

`    (pat-match (rest pattern) (rest input))))))`
!!!(p) {:.unnumlist}

**Exercise 5.1 [s]** Would it be a good idea to replace the complex and form in `pat-match` with the simpler `(every #’pat-match pattern input)?`

Before we can go on, we need to decide on an implementation for pattern-matching variables.
We could, for instance, say that only a certain set of symbols, such as {X,Y,Z}, are variables.
Alternately, we could define a structure of type `variable`, but then we’d have to type something verbose like `(make-variable :name’ X )` every time we wanted one.
Another choice would be to use symbols, but to distinguish variables from constants by the name of the symbol.
For example, in Prolog, variables start with capital letters and constants with lowercase.
But Common Lisp is case-insensitive, so that won’t work.
Instead, there is a tradition in Lisp-based AI programs to have variables be symbols that start with the question mark character.

So far we have dealt with symbols as atoms–objects with no internal structure.
But things are always more complicated than they first appear and, as in Lisp as in physics, it turns out that even atoms have components.
In particular, symbols have names, which are strings and are accessible through the `symbol-name` function.
Strings in turn have elements that are characters, accessible through the function `char`.
The character ‘?’ is denoted by the self-evaluating escape sequence #\?.
So the predicate `variable-p` can be defined as follows, and we now have a complete pattern matcher:

[ ](#){:#l0035}`(defun variable-p (x)`
!!!(p) {:.unnumlist}

` “Is x a variable (a symbol beginning with ‘?’)?”`
!!!(p) {:.unnumlist}

` (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))`
!!!(p) {:.unnumlist}

`> (pat-match ‘(I need a ?X) ‘(I need a vacation))`
!!!(p) {:.unnumlist}

`T`
!!!(p) {:.unnumlist}

`> (pat-match ‘(I need a ?X) ‘(I really need a vacation))`
!!!(p) {:.unnumlist}

`NIL`
!!!(p) {:.unnumlist}

In each case we get the right answer, but we don’t get any indication of what `?X` is, so we couldn’t substitute it into the response.
We need to modify `pat-match` to return some kind of table of variables and corresponding values.
In making this choice, the experienced Common Lisp programmer can save some time by being opportunistic: recognizing when there is an existing function that will do a large part of the task at hand.
What we want is to substitute values for variables throughout the response.
The alert programmer could refer to the index of this book or the Common Lisp reference manual and find the functions `substitute, subst`, and `sublis`.
All of these substitute some new expression for an old one within an expression.
It turns out that `sublis` is most appropriate because it is the only one that allows us to make several substitutions all at once.
`sublis` takes two arguments, the first a list of old-new pairs, and the second an expression in which to make the substitutions.
For each one of the pairs, the `car` is replaced by the `cdr`.
In other words, we would form each pair with something like `(cons old new)`.
(Such a list of pairs is known as an *association list*, or *a-list,* because it associates keys with values.
See [section 3.6.](B9780080571157500030.xhtml#s0035)) In terms of the example above, we would use:

[ ](#){:#l0040}`(sublis ’((?X .
vacation))`
!!!(p) {:.unnumlist}

`   ’(what would it mean to you if you got a ?X ?))`
!!!(p) {:.unnumlist}

`(WHAT WOULD IT MEAN TO YOU IF YOU GOT A VACATION ?)`
!!!(p) {:.unnumlist}

Now we need to arrange for `pat-match` to return an a-list, rather than just T for success.
Here’s a first attempt:

[ ](#){:#l0045}`(defun pat-match (pattern input)`
!!!(p) {:.unnumlist}

` “Does pattern match input?
WARNING: buggy version.”`
!!!(p) {:.unnumlist}

` (if (variable-p pattern)`
!!!(p) {:.unnumlist}

`  (list (cons pattern input))`
!!!(p) {:.unnumlist}

`  (if (or (atom pattern) (atom input))`
!!!(p) {:.unnumlist}

`   (eql pattern input)`
!!!(p) {:.unnumlist}

`   (append (pat-match (first pattern) (first input))`
!!!(p) {:.unnumlist}

`    (pat-match (rest pattern) (rest input))))))`
!!!(p) {:.unnumlist}

This implementation looks reasonable: it returns an a-list of one element if the pattern is a variable, and it appends alists if the pattern and input are both lists.
However, there are several problems.
First, the test `(eql pattern input)` may return `T`, which is not a list, so `append` will complain.
Second, the same test might return nil, which should indicate failure, but it will just be treated as a list, and will be appended to the rest of the answer.
Third, we haven’t distinguished between the case where the match fails–and returns nil–versus the case where everything matches, but there are no variables, so it returns the null a-list.
(This is the semipredicate problem discussed on [page 127](B9780080571157500042.xhtml#p127).) Fourth, we want the bindings of variables to agree—if `?X` is used twice in the pattern, we don’t want it to match two different values in the input.
Finally, it is inefficient for `pat-match` to check both the `first` and `rest` of lists, even when the corresponding `first` parts fail to match.
(Isn’t it amazing that there could be five bugs in a seven-line function?)

We can resolve these problems by agreeing on two major conventions.
First, it is very convenient to make `pat-match` a true predicate, so we will agree that it returns `ni1` only to indicate failure.
That means that we will need a non-nil value to represent the empty binding list.
Second, if we are going to be consistent about the values of variables, then the `first` will have to know what the `rest` is doing.
We can accomplish this by passing the binding list as a third argument to `pat-match`.
We make it an optional argument, because we want to be able to say simply `(pat-match *a b*)`.

To abstract away from these implementation decisions, we define the constants `fai1` and `no-bindings` to represent the two problematic return values.
The special form `defconstant` is used to indicate that these values will not change.
(It is customary to give special variables names beginning and ending with asterisks, but this convention usually is not followed for constants.
The reasoning is that asterisks shout out, “Careful!
I may be changed by something outside of this lexical scope.” Constants, of course, will not be changed.)

[ ](#){:#l0050}`(defconstant fail nil “Indicates pat-match failure”)`
!!!(p) {:.unnumlist}

`(defconstant no-bindings ’((t .
t))`
!!!(p) {:.unnumlist}

` “Indicates pat-match success, with no variables.”)`
!!!(p) {:.unnumlist}

Next, we abstract away from assoc by introducing the following four functions:

[ ](#){:#l0055}`(defun get-binding (var bindings)`
!!!(p) {:.unnumlist}

` “Find a (variable .
value) pair in a binding list.”`
!!!(p) {:.unnumlist}

` (assoc var bindings))`
!!!(p) {:.unnumlist}

`(defun binding-val (binding)`
!!!(p) {:.unnumlist}

` “Get the value part of a single binding.”`
!!!(p) {:.unnumlist}

` (cdr binding))`
!!!(p) {:.unnumlist}

`(defun lookup (var bindings)`
!!!(p) {:.unnumlist}

` “Get the value part (for var) from a binding list.”`
!!!(p) {:.unnumlist}

` (binding-val (get-binding var bindings)))`
!!!(p) {:.unnumlist}

`(defun extend-bindings (var val bindings)`
!!!(p) {:.unnumlist}

` “Add a (var .
value) pair to a binding list.”`
!!!(p) {:.unnumlist}

` (cons (cons var val) bindings))`
!!!(p) {:.unnumlist}

Now that variables and bindings are defined, `pat-match` is easy.
It consists of five cases.
First, if the binding list is `fai1`, then the match fails (because some previous match must have failed).
If the pattern is a single variable, then the match returns whatever `match-variable` returns; either the existing binding list, an extended one, or `fai1`.
Next, if both pattern and input are lists, we first call `pat-match` recursively on the first element of each list.
This returns a binding list (or `fai1`), which we use to match the rest of the lists.
This is the only case that invokes a nontrivial function, so it is a good idea to informally prove that the function will terminate: each of the two recursive calls reduces the size of both pattern and input, and `pat-match` checks the case of atomic patterns and inputs, so the function as a whole must eventually return an answer (unless both pattern and input are of infinite size).
If none of these four cases succeeds, then the match fails.

[ ](#){:#l0060}`(defun pat-match (pattern input &optional (bindings no-bindings))`
!!!(p) {:.unnumlist}

` “Match pattern against input in the context of the bindings”`
!!!(p) {:.unnumlist}

` (cond ((eq bindings fail) fail)`
!!!(p) {:.unnumlist}

`   ((variable-p pattern)`
!!!(p) {:.unnumlist}

`    (match-variable pattern input bindings))`
!!!(p) {:.unnumlist}

`   ((eql pattern input) bindings)`
!!!(p) {:.unnumlist}

`   ((and (consp pattern) (consp input))`
!!!(p) {:.unnumlist}

`    (pat-match (rest pattern) (rest input)`
!!!(p) {:.unnumlist}

`     (pat-match (first pattern) (first input)`
!!!(p) {:.unnumlist}

`      bindings)))`
!!!(p) {:.unnumlist}

`   (t fail)))`
!!!(p) {:.unnumlist}

`(defun match-variable (var input bindings)`
!!!(p) {:.unnumlist}

` “Does VAR match input?
Uses (or updates) and returns bindings.”`
!!!(p) {:.unnumlist}

` (let ((binding (get-binding var bindings)))`
!!!(p) {:.unnumlist}

`  (cond ((not binding) (extend-bindings var input bindings))`
!!!(p) {:.unnumlist}

`    ((equal input (binding-val binding)) bindings)`
!!!(p) {:.unnumlist}

`    (t fail))))`
!!!(p) {:.unnumlist}

We can now test `pat-match` and see how it works:

[ ](#){:#l0065}`> (pat-match ’(i need a ?X) ’(i need a vacation))`
!!!(p) {:.unnumlist}

`((?X .
VACATION) (T .
T))`
!!!(p) {:.unnumlist}

The answer is a list of variable bindings in dotted pair notation; each element of the list is a (`*variable .
value*`) pair.
The `(T .
T)` is a remnant from `no-bindings`.
It does no real harm, but we can eliminate it by making `extend-bindings` a little more complicated:

[ ](#){:#l0070}`(defun extend-bindings (var val bindings)`
!!!(p) {:.unnumlist}

` “Add a (var .
value) pair to a binding list.”`
!!!(p) {:.unnumlist}

` (cons (cons var val)`
!!!(p) {:.unnumlist}

`   ;; Once we add a “real” binding,`
!!!(p) {:.unnumlist}

`   ;; we can get rid of the dummy no-bindings`
!!!(p) {:.unnumlist}

`    (if (eq bindings no-bindings)`
!!!(p) {:.unnumlist}

`     nil`
!!!(p) {:.unnumlist}

`     bindings)`
!!!(p) {:.unnumlist}

`> (sublis (pat-match ’(i need a ?X) ’(i need a vacation))`
!!!(p) {:.unnumlist}

`     ’(what would it mean to you if you got a ?X ?))`
!!!(p) {:.unnumlist}

`(WHAT WOULD IT MEAN TO YOU IF YOU GOT A VACATION ?)`
!!!(p) {:.unnumlist}

`> (pat-match ’(i need a ?X) ’(i really need a vacation))`
!!!(p) {:.unnumlist}

`NIL`
!!!(p) {:.unnumlist}

`> (pat-match ’(this is easy) ’(this is easy))`
!!!(p) {:.unnumlist}

`((T .
T))`
!!!(p) {:.unnumlist}

`> (pat-match ’(?X is ?X) ’((2 + 2) is 4))`
!!!(p) {:.unnumlist}

`NIL`
!!!(p) {:.unnumlist}

`> (pat-match ’(?X is ?X) ’((2 + 2) is (2 + 2)))`
!!!(p) {:.unnumlist}

`((?X 2 + 2))`
!!!(p) {:.unnumlist}

`> (pat-match ’(?P need .
?X) ’(i need a long vacation))`
!!!(p) {:.unnumlist}

`((?X A LONG VACATION) (?P .
I))`
!!!(p) {:.unnumlist}

Notice the distinction between `NIL` and `((T .
T))`.
The latter means that the match succeeded, but there were no bindings to return.
Also, remember that `(?X 2 + 2)` means the same as `(?X .
(2 + 2))`.

A more powerful implementation of `pat-match` is given in [chapter 6](B9780080571157500066.xhtml).
Yet another implementation is given in [section 10.4](B9780080571157500108.xhtml#s0025).
It is more efficient but more cumbersome to use.

## [ ](#){:#st0020}5.3 Segment Pattern Matching
{:#s0020}
{:.h1hd}

In the pattern `(?P need .
?X)`, the variable `?X` matches the rest of the input list, regardless of its length.
This is in contrast to `?P`, which can only match a single element, namely, the first element of the input.
For many applications of pattern matching, this is fine; we only want to match corresponding elements.
However, ELIZA !!!(span) {:.smallcaps} is somewhat different in that we need to account for variables in any position that match a sequence of items in the input.
We will call such variables *segment variables.* We will need a notation to differentiate segment variables from normal variables.
The possibilities fail into two classes: either we use atoms to represent segment variables and distinguish them by some spelling convention (as we did to distinguish variables from constants) or we use a nonatomic construct.
We will choose the latter, using a list of the form (`?*`*variable*) to denote segment variables.
The symbol `?*` is chosen because it combines the notion of variable with the Kleenestar notation.
So, the behavior we want from `pat-match` is now:

[ ](#){:#l0075}`> (pat-match ’((?* ?p) need (?* ?x))`
!!!(p) {:.unnumlist}

`    ’(Mr Hulot and I need a vacation))`
!!!(p) {:.unnumlist}

`((?P MR HULOT AND I) (?X A VACATION))`
!!!(p) {:.unnumlist}

In other words, when both pattern and input are lists and the first element of the pattern is a segment variable, then the variable will match some initial part of the input, and the rest of the pattern will attempt to match the rest.
We can update `pat-match` to account for this by adding a single cond-clause.
Defining the predicate to test for segment variables is also easy:

[ ](#){:#l0080}`(defun pat-match (pattern input &optional (bindings no-bindings))`
!!!(p) {:.unnumlist}

` “Match pattern against input in the context of the bindings”`
!!!(p) {:.unnumlist}

` (cond ((eq bindings fail) fail)`
!!!(p) {:.unnumlist}

`   ((variable-p pattern)`
!!!(p) {:.unnumlist}

`    (match-variable pattern input bindings))`
!!!(p) {:.unnumlist}

`   ((eql pattern input) bindings)`
!!!(p) {:.unnumlist}

`   ((segment-pattern-p pattern); ***`
!!!(p) {:.unnumlist}

`    (segment-match pattern input bindings)); ***`
!!!(p) {:.unnumlist}

`   ((and (consp pattern) (consp input))`
!!!(p) {:.unnumlist}

`    (pat-match (rest pattern) (rest input)`
!!!(p) {:.unnumlist}

`     (pat-match (first pattern) (first input)`
!!!(p) {:.unnumlist}

`      bindings)))`
!!!(p) {:.unnumlist}

`   (t fail)))`
!!!(p) {:.unnumlist}

`(defun segment-pattern-p (pattern)`
!!!(p) {:.unnumlist}

` “Is this a segment matching pattern: ((?* var) .
pat)”`
!!!(p) {:.unnumlist}

` (and (consp pattern)`
!!!(p) {:.unnumlist}

`  (starts-with (first pattern) ‘?*)))`
!!!(p) {:.unnumlist}

In writing `segment-match`, the important question is how much of the input the segment variable should match.
One answer is to look at the next element of the pattern (the one after the segment variable) and see at what position it occurs in the input.
If it doesn’t occur, the total pattern can never match, and we should `fail`.
If it does occur, call its position `pos`.
We will want to match the variable against the initial part of the input, up to `pos`.
But first we have to see if the rest of the pattern matches the rest of the input.
This is done by a recursive call to `pat-match`.
Let the result of this recursive call be named `b2`.
If `b2` succeeds, then we go ahead and match the segment variable against the initial subsequence.

The tricky part is when `b2` fails.
We don’t want to give up completely, because it may be that if the segment variable matched a longer subsequence of the input, then the rest of the pattern would match the rest of the input.
So what we want is to try `segment-match` again, but forcing it to consider a longer match for the variable.
This is done by introducing an optional parameter, `start`, which is initially 0 and is increased with each failure.
Notice that this policy rules out the possibility of any kind of variable following a segment variable.
(Later we will remove this constraint.)

[ ](#){:#l0085}`(defun segment-match (pattern input bindings &optiona1 (start 0))`
!!!(p) {:.unnumlist}

` “Match the segment pattern ((?* var) .
pat) against input.”`
!!!(p) {:.unnumlist}

` (let ((var (second (first pattern)))`
!!!(p) {:.unnumlist}

`   (pat (rest pattern))) (if (null pat)`
!!!(p) {:.unnumlist}

`  (if (null pat)`
!!!(p) {:.unnumlist}

`   (match-variable var input bindings)`
!!!(p) {:.unnumlist}

`   ;; We assume that pat starts with a constant`
!!!(p) {:.unnumlist}

`   ;; In other words, a pattern can’t have 2 consecutive vars`
!!!(p) {:.unnumlist}

`   (let ((pos (position (first pat) input`
!!!(p) {:.unnumlist}

`        :start start :test #’equal)))`
!!!(p) {:.unnumlist}

`    (if (null pos)`
!!!(p) {:.unnumlist}

`     fail`
!!!(p) {:.unnumlist}

`     (let ((b2 (pat-match pat (subseq input pos) bindings)))`
!!!(p) {:.unnumlist}

`      ;; If this match failed, try another longer one`
!!!(p) {:.unnumlist}

`      ;; If it worked, check that the variables match`
!!!(p) {:.unnumlist}

`      (if (eq b2 fail)`
!!!(p) {:.unnumlist}

`       (segment-match pattern input bindings (+ pos 1))`
!!!(p) {:.unnumlist}

`       (match-variable var (subseq input 0 pos) b2))))))))`
!!!(p) {:.unnumlist}

Some examples of segment matching follow:

[ ](#){:#l0090}`> (pat-match ‘((?* ?p) need (?* ?x))`
!!!(p) {:.unnumlist}

`      ‘(Mr Hulot and I need a vacation))`
!!!(p) {:.unnumlist}

`((?P MR HULOT AND I) (?X A VACATION))`
!!!(p) {:.unnumlist}

`> (pat-match ‘((?* ?x) is a (?* ?y)) ‘(what he is is a fool))`
!!!(p) {:.unnumlist}

`((?X WHAT HE IS) (?Y FOOL))`
!!!(p) {:.unnumlist}

The first of these examples shows a fairly simple case: `?p` matches everything up to need, and `?x` matches the rest.
The next example involves the more complicated backup case.
First `?x` matches everything up to the first `is` (this is position 2, since counting starts at 0 in Common Lisp).
But then the pattern a fails to match the input `is`, so `segment-match` tries again with starting position 3.
This time everything works; `is` matches `is`, `a` matches a, and `(?* ?y)` matches `fool`.

Unfortunately, this version of `segment-match` does not match as much as it should.
Consider the following example:

[ ](#){:#l0095}`> (pat-match ’((?* ?x) a b (?* ?x)) ’(1 2 a b a b 1 2 a b)) => NIL`
!!!(p) {:.unnumlist}

This fails because `?x` is matched against the subsequence `(1 2)`, and then the remaining pattern succesfully matches the remaining input, but the final call to `match-variable` fails, because `?x` has two different values.
The fix is to call `match-variable` before testing whether the `b2` fails, so that we will be sure to try `segment-match` again with a longer match no matter what the cause of the failure.

[ ](#){:#l0100}`(defun segment-match (pattern input bindings &optional (start 0))`
!!!(p) {:.unnumlist}

` “Match the segment pattern ((?* var) .
pat) against input.”`
!!!(p) {:.unnumlist}

` (let ((var (second (first pattern)))`
!!!(p) {:.unnumlist}

`   (pat (rest pattern)))`
!!!(p) {:.unnumlist}

`  (if (null pat)`
!!!(p) {:.unnumlist}

`   (match-variable var input bindings)`
!!!(p) {:.unnumlist}

`   ;; We assume that pat starts with a constant`
!!!(p) {:.unnumlist}

`   ;; In other words, a pattern can’t have 2 consecutive vars`
!!!(p) {:.unnumlist}

`   (let ((pos (position (first pat) input`
!!!(p) {:.unnumlist}

`        :start start :test #’equal)))`
!!!(p) {:.unnumlist}

`   (if (null pos)`
!!!(p) {:.unnumlist}

`    fail`
!!!(p) {:.unnumlist}

`    (let ((b2 (pat-match`
!!!(p) {:.unnumlist}

`     pat (subseq input pos)`
!!!(p) {:.unnumlist}

`     (match-variable var (subseq input 0 pos)`
!!!(p) {:.unnumlist}

`         bindings))))`
!!!(p) {:.unnumlist}

`     ;; If this match failed, try another longer one`
!!!(p) {:.unnumlist}

`     (if (eq b2 fail)`
!!!(p) {:.unnumlist}

`      (segment-match pattern input bindings (+ pos 1))`
!!!(p) {:.unnumlist}

`      b2)))))))`
!!!(p) {:.unnumlist}

Now we see that the match goes through:

[ ](#){:#l0105}`> (pat-match ’((?* ?x) a b (?* ?x)) ’(1 2 a b a b 1 2 a b)) ((?X 1 2 A B))`
!!!(p) {:.unnumlist}

Note that this version of `segment-match` tries the shortest possible match first.
It would also be possible to try the longest match first.

## [ ](#){:#st0025}5.4 The ELIZA
!!!(span) {:.smallcaps}
 Program: A Rule-Based Translator
{:#s0025}
{:.h1hd}

Now that we have a working pattern matcher, we need some patterns to match.
What’s more, we want the patterns to be associated with responses.
We can do this by inventing a data structure called a `rule`, which consists of a pattern and one or more associated responses.
These are rules in the sense that they assert, “If you see A, then respond with B or C, chosen at random.” We will choose the simplest possible implementation for rules: as lists, where the first element is the pattern and the rest is a list of responses:

[ ](#){:#l0110}`(defun rule-pattern (rule) (first rule))`
!!!(p) {:.unnumlist}

`(defun rule-responses (rule) (rest rule))`
!!!(p) {:.unnumlist}

Here’s an example of a rule:

[ ](#){:#l0115}`(((?* ?x) I want (?* ?y))`
!!!(p) {:.unnumlist}

` (What would it mean if you got ?y)`
!!!(p) {:.unnumlist}

` (Why do you want ?y)`
!!!(p) {:.unnumlist}

` (Suppose you got ?y soon))`
!!!(p) {:.unnumlist}

When applied to the input `(I want to test this program)`, this rule (when interpreted by the ELIZA !!!(span) {:.smallcaps} program) would pick a response at random, substitute in the value of `?y`, and respond with, say, `(why do you want to test this program)`.

Now that we know what an individual rule will do, we need to decide how to handle a set of rules.
If ELIZA !!!(span) {:.smallcaps} is to be of any interest, it will have to have a variety of responses.
So several rules may all be applicable to the same input.
One possibility would be to choose a rule at random from among the rules having patterns that match the input.

Another possibility is just to accept the first rule that matches.
This implies that the rules form an ordered list, rather than an unordered set.
The clever ELIZA !!!(span) {:.smallcaps} rule writer can take advantage of this ordering and arrange for the most specific rules to come first, while more vague rules are near the end of the list.

The original ELIZA !!!(span) {:.smallcaps} had a system where each rule had a priority number associated with it.
The matching rule with the highest priority was chosen.
Note that putting the rules in order achieves the same effect as having a priority number on each rule: the first rule implicitly has the highest priority, the second rule is next highest, and so on.

Here is a short list of rules, selected from Weizenbaum’s original article, but with the form of the rules updated to the form we are using.
The answer to exercise 5.19 contains a longer list of rules.

[ ](#){:#l0120}`(defparameter *eliza-rules*`
!!!(p) {:.unnumlist}

` ’((((?* ?x) hello (?* ?y))`
!!!(p) {:.unnumlist}

`   (How do you do.
Please state your problem.))`
!!!(p) {:.unnumlist}

`  (((?* ?x) I want (?* ?y))`
!!!(p) {:.unnumlist}

`   (What would it mean if you got ?y)`
!!!(p) {:.unnumlist}

`   (Why do you want ?y) (Suppose you got ?y soon))`
!!!(p) {:.unnumlist}

`  (((?* ?x) if (?* ?y))`
!!!(p) {:.unnumlist}

`   (Do you really think its likely that ?y) (Do you wish that ?y)`
!!!(p) {:.unnumlist}

`   (What do you think about ?y) (Really-- if ?y))`
!!!(p) {:.unnumlist}

`  (((?* ?x) no (?* ?y))`
!!!(p) {:.unnumlist}

`   (Why not?) (You are being a bit negative)`
!!!(p) {:.unnumlist}

`   (Are you saying “NO” just to be negative?))`
!!!(p) {:.unnumlist}

`  (((?* ?x) I was (?* ?y))`
!!!(p) {:.unnumlist}

`   (Were you really?) (Perhaps I already knew you were ?y)`
!!!(p) {:.unnumlist}

`   (Why do you tell me you were ?y now?))`
!!!(p) {:.unnumlist}

`  (((?* ?x) I feel (?* ?y))`
!!!(p) {:.unnumlist}

`   (Do you often feel ?y ?))`
!!!(p) {:.unnumlist}

`  (((?* ?x) I felt (?* ?y))`
!!!(p) {:.unnumlist}

`   (What other feelings do you have?))))`
!!!(p) {:.unnumlist}

Finally we are ready to define ELIZA !!!(span) {:.smallcaps} proper.
As we said earlier, the main program should be a loop that reads input, transforms it, and prints the result.
Transformation is done primarily by finding some rule such that its pattern matches the input, and then substituting the variables into the rule’s response.
The program is summarized in [figure 5.1](#f0010).

![f05-01-9780080571157](images/B9780080571157500054/f05-01-9780080571157.jpg)     
Figure 5.1
!!!(span) {:.fignum}
Glossary for the ELIZA
!!!(span) {:.smallcaps}
Program
There are a few minor complications.
We print a prompt to tell the user to input something.
We use the function `flatten` to insure that the output won’t have imbedded lists after variable substitution.
An important trick is to alter the input by swapping “you” for “me” and so on, since these terms are relative to the speaker.
Here is the complete program:

[ ](#){:#l0125}`(defun eliza ()`
!!!(p) {:.unnumlist}

` “Respond to user input using pattern matching rules.”`
!!!(p) {:.unnumlist}

` (loop`
!!!(p) {:.unnumlist}

`  (print ’eliza >)`
!!!(p) {:.unnumlist}

`  (write (flatten (use-eliza-rules (read))) :pretty t)))`
!!!(p) {:.unnumlist}

`(defun use-eliza-rules (input)`
!!!(p) {:.unnumlist}

` “Find some rule with which to transform the input.”`
!!!(p) {:.unnumlist}

` (some #’(lambda (rule)`
!!!(p) {:.unnumlist}

`    (let ((result (pat-match (rule-pattern rule) input)))`
!!!(p) {:.unnumlist}

`     (if (not (eq result fail))`
!!!(p) {:.unnumlist}

`      (sublis (switch-viewpoint result)`
!!!(p) {:.unnumlist}

`          (random-elt (rule-responses rule))))))`
!!!(p) {:.unnumlist}

`   *eliza-rules*))`
!!!(p) {:.unnumlist}

`(defun switch-viewpoint (words)`
!!!(p) {:.unnumlist}

` “Change I to you and vice versa, and so on.”`
!!!(p) {:.unnumlist}

` (sublis ‘((I .
you) (you .
I) (me .
you) (am .
are))`
!!!(p) {:.unnumlist}

`    words))`
!!!(p) {:.unnumlist}

Note the use of `write` with the : `pretty` keyword true.
This will give better formatted output in some cases.
The program makes use of the previously defined `random-elt`, and `flatten`, which is defined here using `mappend` and `mklist`, a function that is defined in the InterLisp dialect but not in Common Lisp.

[ ](#){:#l0130}`(defun flatten (the-list)`
!!!(p) {:.unnumlist}

` “Append together elements (or lists) in the list.”`
!!!(p) {:.unnumlist}

` (mappend #’mklist the-list))`
!!!(p) {:.unnumlist}

`(defun mklist (x)`
!!!(p) {:.unnumlist}

` “Return x if it is a list, otherwise (x).”`
!!!(p) {:.unnumlist}

` (if (listp x)`
!!!(p) {:.unnumlist}

`  x`
!!!(p) {:.unnumlist}

`  (list x)))`
!!!(p) {:.unnumlist}

`(defun mappend (fn the-list)`
!!!(p) {:.unnumlist}

` “Apply fn to each element of list and append the results.”`
!!!(p) {:.unnumlist}

` (apply #’append (mapcar fn the-list)))`
!!!(p) {:.unnumlist}

`(defun random-elt (choices)`
!!!(p) {:.unnumlist}

` “Choose an element from a list at random.”`
!!!(p) {:.unnumlist}

` (elt choices (random (length choices))))`
!!!(p) {:.unnumlist}

The actual ELIZA !!!(span) {:.smallcaps} program had a few complications that don’t show up in this version.
First, there was an alias method for associating several words with the same pattern; both “mother” and “father” could be associated with the “family” pattern.
There was also a synonym mechanism that would treat “don’t” and “do not” or “everybody” and “everyone” the same.
In the case of input with several comma-separated phrases, each phrase was processed separately, and the response with the highest priority was selected.
In addition, the original ELIZA !!!(span) {:.smallcaps} had a “memory” mechanism.
When no pattern matched the input, it said something like “Tell me more about X,” where X is some input given earlier.
Finally, our version has fewer rules.
Despite the shortcomings of our version, it can occasionally hold up its end of a conversation, as shown below:

[ ](#){:#l0135}`> (eliza)`
!!!(p) {:.unnumlist}

`ELIZA > (hello there)`
!!!(p) {:.unnumlist}

`(HOW DO YOU DO.
PLEASE STATE YOUR PROBLEM.)`
!!!(p) {:.unnumlist}

`ELIZA > (i want to test this program)`
!!!(p) {:.unnumlist}

`(WHAT WOULD IT MEAN IF YOU GOT TO TEST THIS PROGRAM)`
!!!(p) {:.unnumlist}

`ELIZA > (i could see if it works)`
!!!(p) {:.unnumlist}

`(DO YOU REALLY THINK ITS LIKELY THAT IT WORKS)`
!!!(p) {:.unnumlist}

`ELIZA > (no not really)`
!!!(p) {:.unnumlist}

`(ARE YOU SAYING “NO” JUST TO BE NEGATIVE?)`
!!!(p) {:.unnumlist}

`ELIZA > (no)`
!!!(p) {:.unnumlist}

`(ARE YOU SAYING “NO” JUST TO BE NEGATIVE?)`
!!!(p) {:.unnumlist}

`ELIZA > (forget it-- i was wondering how general the program is)`
!!!(p) {:.unnumlist}

`(WHY DO YOU TELL ME YOU WERE WONDERING HOW GENERAL THE PROGRAM IS NOW?)`
!!!(p) {:.unnumlist}

`ELIZA > (i felt like it)`
!!!(p) {:.unnumlist}

`(WHAT OTHER FEELINGS DO YOU HAVE?)`
!!!(p) {:.unnumlist}

`ELIZA > (i feel this is enough)`
!!!(p) {:.unnumlist}

`(DO YOU OFTEN FEEL THIS IS ENOUGH ?)`
!!!(p) {:.unnumlist}

`ELIZA > [Abort]`
!!!(p) {:.unnumlist}

`>`
!!!(p) {:.unnumlist}

In the end, it is the technique that is important–not the program.
ELIZA !!!(span) {:.smallcaps} has been “explained away” and should rightfully be moved to the curio shelf.
Pattern matching in general remains important technique, and we will see it again in subsequent chapters.
The notion of a rule-based translator is also important.
The problem of understanding English (and other languages) remains an important part of AI.
Clearly, the problem of understanding English is not solved by ELIZA.
!!!(span) {:.smallcaps} In part V, we will address the problem again, using more sophisticated techniques.

## [ ](#){:#st0030}5.5 History and References
{:#s0030}
{:.h1hd}

As mentioned above, the original article describing ELIZA !!!(span) {:.smallcaps} is Weizenbaum 1966.
Another dialog system using similar pattern-matching techniques is [Kenneth Colby’s (1975)](B9780080571157500285.xhtml#bb0235)PARRY.
!!!(span) {:.smallcaps} This program simulated the conversation of a paranoid person well enough to fool several professional psychologists.
Although the pattern matching techniques were simple, the model of belief maintained by the system was much more sophisticated than ELIZA !!!(span) {:.smallcaps} .
Colby has suggested that dialog programs like ELIZA, !!!(span) {:.smallcaps} augmented with some sort of belief model like PARRY !!!(span) {:.smallcaps} , could be useful tools in treating mentally disturbed people.
According to Colby, it would be inexpensive and effective to have patients converse with a specially designed program, one that could handle simple cases and alert doctors to patients that needed more help.
Weizenbaum’s book *Computer Power and Human Reason* (1976) discusses ELIZA !!!(span) {:.smallcaps} and PARRY !!!(span) {:.smallcaps} and takes a very critical view toward Colby’s suggestion.
Other interesting early work on dialog systems that model belief is reported by[Allan Collins (1978)](B9780080571157500285.xhtml#bb0240) and [Jamie Carbonell (1981)](B9780080571157500285.xhtml#bb0160).

## [ ](#){:#st0035}5.6 Exercises[ ](#){:#p1575}
{:#s0035}
{:.h1hd}

[ ](#){:#l0140}**Exercise 5.2 [m]** Experiment with this version of ELIZA !!!(span) {:.smallcaps} .
Show some exchanges where it performs well, and some where it fails.
Try to characterize the difference.
Which failures could be fixed by changing the rule set, which by changing the `pat-match` function (and the pattern language it defines), and which require a change to the `eliza` program itself?
!!!(p) {:.unnumlist}

**Exercise 5.3 [h]** Define a new set of rules that make ELIZA !!!(span) {:.smallcaps} give stereotypical responses to some situation other than the doctor–patient relationship.
Or, write a set of rules in a language other than English.
Test and debug your new rule set.
!!!(p) {:.unnumlist}

**Exercise 5.4 [s]** We mentioned that our version of ELIZA !!!(span) {:.smallcaps} cannot handle commas or double quote marks in the input.
However, it seems to handle the apostrophe in both input and patterns.
Explain.
!!!(p) {:.unnumlist}

**Exercise 5.5 [h]** Alter the input mechanism to handle commas and other punctuation characters.
Also arrange so that the user doesn’t have to type parentheses around the whole input expression.
(Hint: this can only be done using some Lisp functions we have not seen yet.
Lookat `read-lineand read-from-string`.)
!!!(p) {:.unnumlist}

**Exercise 5.6 [m]** Modify ELIZA !!!(span) {:.smallcaps} to have an explicit exit.
Also arrange so that the output is not printed in parentheses either.
!!!(p) {:.unnumlist}

**Exercise 5.7 [m]** Add the “memory mechanism” discussed previously to ELIZA.
!!!(span) {:.smallcaps} Also add some way of definining synonyms like “everyone” and “everybody.”
!!!(p) {:.unnumlist}

**Exercise 5.8 [h]** It turns out that none of the rules in the given script uses a variable more than once–there is no rule of the form `(?x… ?x)`.
Write a pattern matcher that only adds bindings, never checks variables against previous bindings.
Use the `time` special form to compare your function against the current version.
!!!(p) {:.unnumlist}

**Exercise 5.9 [h]** Winston and Horn’s book *Lisp* presents a good pattern-matching program.
Compare their implementation with this one.
One difference is that they handle the case where the first element of the pattern is a segment variable with the following code (translated into our notation):
!!!(p) {:.unnumlist}

[ ](#){:#l9000}`(or (pat-match (rest pattern) (rest input) bindings)`
!!!(p) {:.unnumlist1}

`   (pat-match pattern (rest input) bindings))`
!!!(p) {:.unnumlist1}

This says that a segment variable matches either by matching the first element of the input, or by matching more than the first element.
It is much simpler than our approach using `position`, partly because they don’t update the binding list.
Can you change their code to handle bindings, and incorporate it into our version of `pat-match?` Is it still simpler?
Is it more or less efficient?

**Exercise 5.10** What is wrong with the following definition of `simple-equal?`
!!!(p) {:.unnumlist}

[ ](#){:#l9005}`(defun simple-equal (x y)`
!!!(p) {:.unnumlist1}

` “Test if two lists or atoms are equal.”`
!!!(p) {:.unnumlist1}

` ;; Warning - incorrect`
!!!(p) {:.unnumlist1}

` (or (eql x y)`
!!!(p) {:.unnumlist1}

`   (and (listp x) (listp y)`
!!!(p) {:.unnumlist1}

`    (simple-equal (first x) (first y))`
!!!(p) {:.unnumlist1}

`    (simple-equal (rest x) (rest y)))))`
!!!(p) {:.unnumlist1}

**Exercise 5.11 [m]** Weigh the advantages of changing `no-bindings` to `nil`, and `fail` to something else.
!!!(p) {:.unnumlist}

**Exercise 5.12 [m]** Weigh the advantagesof making `pat-match` return multiple values: the first would be true for a match and false for failure, and the second would be the binding list.
!!!(p) {:.unnumlist}

**Exercise 5.13 [m]** Suppose that there is a call to `segment-match` where the variable already has a binding.
The current definition will keep making recursive calls to `segment-match`, one for each possible matching position.
But this is silly–if the variable is already bound, there is only one sequence that it can possibly match against.
Change the definition so that it looks only for this one sequence.
!!!(p) {:.unnumlist}

**Exercise 5.14 [m]** Define a version of `mappend` that, like `mapcar`, accepts any number of argument lists.
!!!(p) {:.unnumlist}

**Exercise 5.15 [m]** Give an informal proof that `segment-match` always terminates.
!!!(p) {:.unnumlist}

**Exercise 5.16 [s]** Trick question: There is an object in Lisp which, when passed to `variable-p`, results in an error.
What is that object?
!!!(p) {:.unnumlist}

**Exercise 5.17 [m]** The current version of ELIZA !!!(span) {:.smallcaps} takes an input, transforms it according to the first applicable rule, and outputs the result.
One can also imagine a system where the input might be transformed several times before the final output is printed.
Would such a system be more powerful?
If so, in what way?
!!!(p) {:.unnumlist}

**Exercise 5.18 [h]** Read Weizenbaum’s original article on ELIZA !!!(span) {:.smallcaps} and transpose his list of rules into the notation used in this chapter.
!!!(p) {:.unnumlist}

## [ ](#){:#st0040}5.7 Answers
{:#s0040}
{:.h1hd}

**Answer 5.1** No.
If either the pattern or the input were shorter, but matched every existing element, the every expression would incorrectly return true.

[ ](#){:#l0150}`(every #’pat-match ’(a b c) ’(a))`⇒ `T`
!!!(p) {:.unnumlist}

Furthermore, if either the pattern or the input were a dotted list, then the result of the every would be undefined–some implementations might signal an error, and others might just ignore the expression after the dot.

[ ](#){:#l0155}`(every #’pat-match ’(a b .
c) ’(a b .
d))`⇒ `T, NIL.or error.`
!!!(p) {:.unnumlist}

**Answer 5.4** The expression `don’t` may look like a single word, but to the Lisp reader it is composed of the two elements `don` and `’t`, or `(quote t )`.
If these elements are used consistently, they will match correctly, but they won’t print quite right–there will be a space before the quote mark.
In fact the `:pretty t` argument to `write` is specified primarily to make `(quote t)` print as `’t` (See [page 559](B9780080571157500169.xhtml#p559) of Steele’s *Common Lisp the Language*, 2d edition.)

**Answer 5.5** One way to do this is to read a whole line of text with `read-line` rather than `read`.
Then, substitute spaces for any punctuation character in that string.
Finally, wrap the string in parentheses, and read it back in as a list:

[ ](#){:#l0160}`(defun read-line-no-punct ()`
!!!(p) {:.unnumlist}

` “Read an input line, ignoring punctuation.”`
!!!(p) {:.unnumlist}

` (read-from-string`
!!!(p) {:.unnumlist}

`  (concatenate ‘string “(“ (substitute-if #\space#’punctuation-p`
!!!(p) {:.unnumlist}

`          (read-line))`
!!!(p) {:.unnumlist}

`     “)”)))`
!!!(p) {:.unnumlist}

`(defun punctuation-p (char) (find char “.,;:‘!?#-()\\\”“))`
!!!(p) {:.unnumlist}

This could also be done by altering the readtable, as in [section 23.5](B9780080571157500236.xhtml#s0030), [page 821](B9780080571157500236.xhtml#p821).

**Answer 5.6**

[ ](#){:#l0165}`(defun eliza ()`
!!!(p) {:.unnumlist}

` “Respond to user input using pattern matching rules.”`
!!!(p) {:.unnumlist}

` (loop`
!!!(p) {:.unnumlist}

`  (print ’eliza >)`
!!!(p) {:.unnumlist}

`  (let* ((input (read-line-no-punct))`
!!!(p) {:.unnumlist}

`    (response (flatten (use-eliza-rules input))))`
!!!(p) {:.unnumlist}

`   (print-with-spaces response)`
!!!(p) {:.unnumlist}

`   (if (equal response ’(good bye)) (RETURN)))))`
!!!(p) {:.unnumlist}

`(defun print-with-spaces (list)`
!!!(p) {:.unnumlist}

` (mapc #’(lambda (x) (prinl x) (princ “ ”)) list))`
!!!(p) {:.unnumlist}

***`or`***
!!!(p) {:.unnumlist}

`(defun print-with-spaces (list)`
!!!(p) {:.unnumlist}

` (format t “~{~a ~}H list))`
!!!(p) {:.unnumlist}

**Answer 5.10** Hint: consider `(simple-equal ’() ’(nil .
nil))`.

**Answer 5.14**

[ ](#){:#l0170}`(defun mappend (fn &rest list)`
!!!(p) {:.unnumlist}

` “Apply fn to each element of lists and append the results.”`
!!!(p) {:.unnumlist}

` (apply #’append (apply #’mapcar fn lists)))`
!!!(p) {:.unnumlist}

**Answer 5.16** It must be a symbol, because for nonsymbols, `variable-p` justreturns nil.
Getting the `symbol-name` of a symbol is just accessing a slot, so that can’t cause an error.
The only thing left is `elt;` if the symbol name is the empty string, then accessing element zero of the empty string is an error.
Indeed, there is a symbol whose name is the empty string: the symbol.

**Answer 5.17** Among other things, a recursive transformation system could be used to handle abbreviations.
That is, a form like “don’t” could be transformed into “do not” and then processed again.
That way, the other rules need only work on inputs matching “do not.”

**Answer 5.19** The following includes most of Weizenbaum’s rules:

[ ](#){:#l0175}`(defparameter *eliza-rules*`
!!!(p) {:.unnumlist}

` ’((((?* ?x) hello (?* ?y))`
!!!(p) {:.unnumlist}

`   (How do you do.
Please state your problem.))`
!!!(p) {:.unnumlist}

`  (((?* ?x) computer (?* ?y))`
!!!(p) {:.unnumlist}

`   (Do computers worry you?) (What do you think about machines?)`
!!!(p) {:.unnumlist}

`   (Why do you mention computers?)`
!!!(p) {:.unnumlist}

`   (What do you think machines have to do with your problem?))`
!!!(p) {:.unnumlist}

`  (((?* ?x) name (?* ?y))`
!!!(p) {:.unnumlist}

`   (I am not interested in names))`
!!!(p) {:.unnumlist}

`  (((?* ?x) sorry (?* ?y))`
!!!(p) {:.unnumlist}

`   (Please don’t apologize) (Apologies are not necessary)`
!!!(p) {:.unnumlist}

`   (What feelings do you have when you apologize))`
!!!(p) {:.unnumlist}

`  (((?* ?x) I remember (?* ?y))`
!!!(p) {:.unnumlist}

`   (Do you often think of ?y)`
!!!(p) {:.unnumlist}

`   (Does thinking of ?y bring anything else to mind?)`
!!!(p) {:.unnumlist}

`   (What else do you remember) (Why do you recall ?y right now?)`
!!!(p) {:.unnumlist}

`   (What in the present situation reminds you of ?y)`
!!!(p) {:.unnumlist}

`   (What is the connection between me and ?y))`
!!!(p) {:.unnumlist}

`  (((?* ?x) do you remember (?* ?y))`
!!!(p) {:.unnumlist}

`   (Did you think I would forget ?y ?)`
!!!(p) {:.unnumlist}

`   (Why do you think I should recall ?y now)`
!!!(p) {:.unnumlist}

`   (What about ?y) (You mentioned ?y))`
!!!(p) {:.unnumlist}

`  (((?* ?x) if (?* ?y))`
!!!(p) {:.unnumlist}

`   (Do you really think its likely that ?y) (Do you wish that ?y)`
!!!(p) {:.unnumlist}

`   (What do you think about ?y) (Really-- if ?y))`
!!!(p) {:.unnumlist}

`  (((?* ?x) I dreamt (?* ?y))`
!!!(p) {:.unnumlist}

`   (Really-- ?y) (Have you ever fantasized ?y while you were awake?)`
!!!(p) {:.unnumlist}

`   (Have you dreamt ?y before?))`
!!!(p) {:.unnumlist}

`  (((?* ?x) dream about (?* ?y))`
!!!(p) {:.unnumlist}

`   (How do you feel about ?y in reality?))`
!!!(p) {:.unnumlist}

`  (((?* ?x) dream (?* ?y))`
!!!(p) {:.unnumlist}

`   (What does this dream suggest to you?) (Do you dream often?)`
!!!(p) {:.unnumlist}

`   (What persons appear in your dreams?)`
!!!(p) {:.unnumlist}

`   (Don’t you believe that dream has to do with your problem?))`
!!!(p) {:.unnumlist}

`  (((?* ?x) my mother (?* ?y))`
!!!(p) {:.unnumlist}

`   (Who else in your family ?y) (Tell me more about your family))`
!!!(p) {:.unnumlist}

`  (((?* ?x) my father (?* ?y))`
!!!(p) {:.unnumlist}

`   (Your father) (Does he influence you strongly?)`
!!!(p) {:.unnumlist}

`   (What else comes to mind when you think of your father?))`
!!!(p) {:.unnumlist}

`  (((?* ?x) I want (?* ?y))`
!!!(p) {:.unnumlist}

`   (What would it mean if you got ?y)`
!!!(p) {:.unnumlist}

`   (Why do you want ?y) (Suppose you got ?y soon))`
!!!(p) {:.unnumlist}

`  (((?* ?x) I am glad (?* ?y))`
!!!(p) {:.unnumlist}

`   (How have I helped you to be ?y) (What makes you happy just now)`
!!!(p) {:.unnumlist}

`   (Can you explain why you are suddenly ?y))`
!!!(p) {:.unnumlist}

`  (((?* ?x) I am sad (?* ?y))`
!!!(p) {:.unnumlist}

`   (I am sorry to hear you are depressed)`
!!!(p) {:.unnumlist}

`   (I’m sure it’s not pleasant to be sad))`
!!!(p) {:.unnumlist}

`  (((?* ?x) are like (?* ?y))`
!!!(p) {:.unnumlist}

`   (What resemblance do you see between ?x and ?y))`
!!!(p) {:.unnumlist}

`  (((?* ?x) is like (?* ?y))`
!!!(p) {:.unnumlist}

`   (In what way is it that ?x is like ?y)`
!!!(p) {:.unnumlist}

`   (What resemblance do you see?)`
!!!(p) {:.unnumlist}

`   (Could there really be some connection?) (How?))`
!!!(p) {:.unnumlist}

`  (((?* ?x) alike (?* ?y))`
!!!(p) {:.unnumlist}

`   (In what way?) (What similarities are there?))`
!!!(p) {:.unnumlist}

`  (((?* ?x) same (?* ?y))`
!!!(p) {:.unnumlist}

`   (What other connections do you see?))`
!!!(p) {:.unnumlist}

`  (((?* ?x) I was (?* ?y))`
!!!(p) {:.unnumlist}

`   (Were you really?) (Perhaps I already knew you were ?y)`
!!!(p) {:.unnumlist}

`   (Why do you tell me you were ?y now?))`
!!!(p) {:.unnumlist}

`  (((?* ?x) was I (?* ?y))`
!!!(p) {:.unnumlist}

`   (What if you were ?y ?) (Do you think you were ?y)`
!!!(p) {:.unnumlist}

`   (What would it mean if you were ?y))`
!!!(p) {:.unnumlist}

`  (((?* ?x) I am (?* ?y))`
!!!(p) {:.unnumlist}

`   (In what way are you ?y) (Do you want to be ?y ?))`
!!!(p) {:.unnumlist}

`  (((?* ?x) am I (?* ?y))`
!!!(p) {:.unnumlist}

`   (Do you believe you are ?y) (Would you want to be ?y)`
!!!(p) {:.unnumlist}

`   (You wish I would tell you you are ?y)`
!!!(p) {:.unnumlist}

`   (What would it mean if you were ?y))`
!!!(p) {:.unnumlist}

`  (((?* ?x) am (?* ?y))`
!!!(p) {:.unnumlist}

`   (Why do you say “AM?”) (I don’t understand that))`
!!!(p) {:.unnumlist}

`  (((?* ?x) are you (?* ?y))`
!!!(p) {:.unnumlist}

`   (Why are you interested in whether I am ?y or not?)`
!!!(p) {:.unnumlist}

`   (Would you prefer if I weren’t ?y)`
!!!(p) {:.unnumlist}

`   (Perhaps I am ?y in your fantasies))`
!!!(p) {:.unnumlist}

`  (((?* ?x) you are (?* ?y))`
!!!(p) {:.unnumlist}

`   (What makes you think I am ?y ?))`
!!!(p) {:.unnumlist}

`  (((?* ?x) because (?* ?y))`
!!!(p) {:.unnumlist}

`   (Is that the real reason?) (What other reasons might there be?)`
!!!(p) {:.unnumlist}

`   (Does that reason seem to explain anything else?))`
!!!(p) {:.unnumlist}

`  (((?* ?x) were you (?* ?y))`
!!!(p) {:.unnumlist}

`   (Perhaps I was ?y) (What do you think?) (What if I had been ?y))`
!!!(p) {:.unnumlist}

`  (((?* ?x) I can’t (?* ?y))`
!!!(p) {:.unnumlist}

`   (Maybe you could ?y now) (What if you could ?y ?))`
!!!(p) {:.unnumlist}

`  (((?* ?x) I feel (?* ?y))`
!!!(p) {:.unnumlist}

`   (Do you often feel ?y ?))`
!!!(p) {:.unnumlist}

`  (((?* ?x) I felt (?* ?y))`
!!!(p) {:.unnumlist}

`   (What other feelings do you have?))`
!!!(p) {:.unnumlist}

`  (((?* ?x) I (?* ?y) you (?* ?z)`
!!!(p) {:.unnumlist}

`   (Perhaps in your fantasy we ?y each other))`
!!!(p) {:.unnumlist}

`  (((?* ?x) why don’t you (?* ?y))`
!!!(p) {:.unnumlist}

`   (Should you ?y yourself?)`
!!!(p) {:.unnumlist}

`   (Do you believe I don’t ?y) (Perhaps I will ?y in good time))`
!!!(p) {:.unnumlist}

`  (((?* ?x) yes (?* ?y))`
!!!(p) {:.unnumlist}

`   (You seem quite positive) (You are sure) (I understand))`
!!!(p) {:.unnumlist}

`  (((?* ?x) no (?* ?y))`
!!!(p) {:.unnumlist}

`   (Why not?) (You are being a bit negative)`
!!!(p) {:.unnumlist}

`   (Are you saying “NO” just to be negative?))`
!!!(p) {:.unnumlist}

`  (((?* ?x) someone (?* ?y))`
!!!(p) {:.unnumlist}

`   (Can you be more specific?))`
!!!(p) {:.unnumlist}

`  (((?* ?x) everyone (?* ?y))`
!!!(p) {:.unnumlist}

`   (surely not everyone) (Can you think of anyone in particular?)`
!!!(p) {:.unnumlist}

`   (Who for example?) (You are thinking of a special person))`
!!!(p) {:.unnumlist}

`  (((?* ?x) always (?* ?y))`
!!!(p) {:.unnumlist}

`   (Can you think of a specific example) (When?)`
!!!(p) {:.unnumlist}

`   (What incident are you thinking of?)`
!!!(p) {:.unnumlist}

`   (Really-- always))`
!!!(p) {:.unnumlist}

`  (((?* ?x) what (?* ?y))`
!!!(p) {:.unnumlist}

`   (Why do you ask?) (Does that question interest you?)`
!!!(p) {:.unnumlist}

`   (What is it you really want to know?) (What do you think?)`
!!!(p) {:.unnumlist}

`   (What cornes to your mind when you ask that?))`
!!!(p) {:.unnumlist}

`  (((?* ?x) perhaps (?* ?y))`
!!!(p) {:.unnumlist}

`   (You do not seem quite certain))`
!!!(p) {:.unnumlist}

`  (((?* ?x) are (?* ?y))`
!!!(p) {:.unnumlist}

`   (Did you think they might not be ?y)`
!!!(p) {:.unnumlist}

`   (Possibly they are ?y))`
!!!(p) {:.unnumlist}

`  (((?* ?x))`
!!!(p) {:.unnumlist}

`   (Very interesting) (I am not sure I understand you fully)`
!!!(p) {:.unnumlist}

`   (What does that suggest to you?) (Please continue) (Go on)`
!!!(p) {:.unnumlist}

`   (Do you feel strongly about discussing such things?))))`
!!!(p) {:.unnumlist}

----------------------

[1](#xfn0010){:#np0010} The difference is that `simple-equal` does not handle strings.
!!!(p) {:.ftnote1}

