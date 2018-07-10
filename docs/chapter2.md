# Chapter 2
## A Simple Lisp Program

> *Certum quod factum.*

> (One is certain of only what one builds.)

> – Giovanni Battista Vico (1668–1744)

> Italian royal historiographer

You will never become proficient in a foreign language by studying vocabulary lists.
Rather, you must hear and speak (or read and write) the language to gain proficiency.
The same is true for learning computer languages.

This chapter shows how to combine the basic functions and special forms of Lisp into a complete program.
If you can learn how to do that, then acquiring the remaining vocabulary of Lisp (as outlined in [chapter 3](B9780080571157500030.xhtml)) will be easy.

## [ ](#){:#st0010}2.1 A Grammar for a Subset of English
{:#s0010}
{:.h1hd}

The program we will develop in this chapter generates random English sentences.
Here is a simple grammar for a tiny portion of English:

  [ ](#){:#l0010}*Sentence*⇒ *Noun-Phrase + Verb-Phrase*
!!!(p) {:.unnumlist}

  *Noun-Phrase*⇒ *Article + Noun*
!!!(p) {:.unnumlist}

  *Verb-Phrase*⇒ *Verb + Noun-Phrase*
!!!(p) {:.unnumlist}

  *Article*⇒ *the, a,…*
!!!(p) {:.unnumlist}

  *Noun*⇒ *man, ball, woman, table…*
!!!(p) {:.unnumlist}

  *Verb*⇒ *hit, took, saw, liked…*
!!!(p) {:.unnumlist}

To be technical, this description is called a *context-free phrase-structure grammar*, and the underlying paradigm is called *generative syntax*.
The idea is that anywhere we want a sentence, we can generate a noun phrase followed by a verb phrase.
Anywhere a noun phrase has been specified, we generate instead an article followed by a noun.
Anywhere an article has been specified, we generate either “the,” “a,” or some other article.
The formalism is “context-free” because the rules apply anywhere regardless of the surrounding words, and the approach is “generative” because the rules as a whole define the complete set of sentences in a language (and by contrast the set of nonsentences as well).
In the following we show the derivation of a single sentence using the rules:

  [ ](#){:#l0015}To get a *Sentence*, append a *Noun-Phrase* and a *Verb-Phrase*
!!!(p) {:.unnumlist}

     To get a *Noun-Phrase*, append an *Article* and a *Noun*
!!!(p) {:.unnumlist}

          Choose “*the*” for the *Article*
!!!(p) {:.unnumlist}

          Choose “*man*” for the *Noun*
!!!(p) {:.unnumlist}

     The resulting *Noun-Phrase* is “*the man*”
!!!(p) {:.unnumlist}

     To get a *Verb-Phrase*, append a *Verb* and a *Noun-Phrase*
!!!(p) {:.unnumlist}

          Choose “*hit*” for the *Verb*
!!!(p) {:.unnumlist}

        To get a *Noun-Phrase*, append an *Article* and a *Noun*
!!!(p) {:.unnumlist}

          Choose “*the*” for the *Article*
!!!(p) {:.unnumlist}

          Choose “*ball*” for the *Noun*
!!!(p) {:.unnumlist}

       The resulting *Noun-Phrase* is “*the ball*”
!!!(p) {:.unnumlist}

     The resulting *Verb-Phrase* is “*hit the ball*”
!!!(p) {:.unnumlist}

  The resulting *Sentence* is “*The man hit the ball*”
!!!(p) {:.unnumlist}

## [ ](#){:#st0015}2.2 A Straightforward Solution
{:#s0015}
{:.h1hd}

We will develop a program that generates random sentences from a phrase-structure grammar.
The most straightforward approach is to represent each grammar rule by a separate Lisp function:

[ ](#){:#t0010}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `(defun sentence ()` | `(append (noun-phrase) (verb-phrase)))` |
| `(defun noun-phrase ()` | `(append (Article) (Noun)))` |
| `(defun verb-phrase ()` | `(append (Verb) (noun-phrase)))` |
| `(defun Article ()` | `(one-of '(the a)))` |
| `(defun Noun ()` | `(one-of '(man ball woman table)))` |
| `(defun Verb ()` | `(one-of '(hit took saw liked)))` |

Each of these function definitions has an empty parameter list, `()`.
That means the functions take no arguments.
This is unusual because, strictly speaking, a function with no arguments would always return the same thing, so we would use a constant instead.
However, these functions make use of the `random` function (as we will see shortly), and thus can return different results even with no arguments.
Thus, they are not functions in the mathematical sense, but they are still called functions in Lisp, because they return a value.

All that remains now is to define the function `one-of`.
It takes a list of possible choices as an argument, chooses one of these at random, and returns a one-element list of the element chosen.
This last part is so that all functions in the grammar will return a list of words.
That way, we can freely apply `append` to any category.

  [ ](#){:#l0020}`(defun one-of (set)`
!!!(p) {:.unnumlist}

    ` "Pick one element of set, and make a list of it."`
!!!(p) {:.unnumlist}

    ` (list (random-elt set)))`
!!!(p) {:.unnumlist}

  `(defun random-elt (choices)`
!!!(p) {:.unnumlist}

    ` "Choose an element from a list at random."`
!!!(p) {:.unnumlist}

    ` (elt choices (random (length choices))))`
!!!(p) {:.unnumlist}

There are two new functions here, `elt` and `random.``elt` picks an element out of a list.
The first argument is the list, and the second is the position in the list.
The confusing part is that the positions start at 0, so `(elt choices 0)` is the first element of the list, and `(elt choices 1)` is the second.
Think of the position numbers as telling you how far away you are from the front.
The expression `(random n)` returns an integer from 0 to n-1, so that `(random 4)` would return either 0,1,2, or 3.

Now we can test the program by generating a few random sentences, along with a noun phrase and a verb phrase:

  [ ](#){:#l0025}`> (sentence)`⇒ `(THE WOMAN HIT THE BALL)`
!!!(p) {:.unnumlist}

  `> (sentence)`⇒ `(THE WOMAN HIT THE MAN)`
!!!(p) {:.unnumlist}

  `> (sentence)`⇒ `(THE BALL SAW THE WOMAN)`
!!!(p) {:.unnumlist}

  `> (sentence)`⇒ `(THE BALL SAW THE TABLE)`
!!!(p) {:.unnumlist}

  `> (noun-phrase)`⇒ `(THE MAN)`
!!!(p) {:.unnumlist}

  `> (verb-phrase)`⇒ `(LIKED THE WOMAN)`
!!!(p) {:.unnumlist}

  `> (trace sentence noun-phrase verb-phrase article noun verb) ⇒` `(SENTENCE NOUN-PHRASE VERB-PHRASE ARTICLE NOUN VERB)`
!!!(p) {:.unnumlist}

  `> (sentence)`⇒
!!!(p) {:.unnumlist}

` (1 ENTER SENTENCE)`
!!!(p) {:.unnumlist}

  ` (1 ENTER NOUN-PHRASE)`
!!!(p) {:.unnumlist}

    ` (1 ENTER ARTICLE)`
!!!(p) {:.unnumlist}

    ` (1 EXIT ARTICLE: (THE))`
!!!(p) {:.unnumlist}

    ` (1 ENTER NOUN)`
!!!(p) {:.unnumlist}

    ` (1 EXIT NOUN: (MAN))`
!!!(p) {:.unnumlist}

  ` (1 EXIT NOUN-PHRASE: (THE MAN))`
!!!(p) {:.unnumlist}

  ` (1 ENTER VERB-PHRASE)`
!!!(p) {:.unnumlist}

     ` (1 ENTER VERB)`
!!!(p) {:.unnumlist}

     ` (1 EXIT VERB: (HIT))`
!!!(p) {:.unnumlist}

     ` (1 ENTER NOUN-PHRASE)`
!!!(p) {:.unnumlist}

       ` (1 ENTER ARTICLE)`
!!!(p) {:.unnumlist}

       ` (1 EXIT ARTICLE: (THE))`
!!!(p) {:.unnumlist}

       ` (1 ENTER NOUN)`
!!!(p) {:.unnumlist}

       ` (1 EXIT NOUN: (BALL))`
!!!(p) {:.unnumlist}

     ` (1 EXIT NOUN-PHRASE: (THE BALL))`
!!!(p) {:.unnumlist}

   ` (1 EXIT VERB-PHRASE: (HIT THE BALL))`
!!!(p) {:.unnumlist}

  `(1 EXIT SENTENCE: (THE MAN HIT THE BALL))`
!!!(p) {:.unnumlist}

  `(THE MAN HIT THE BALL)`
!!!(p) {:.unnumlist}

The program works fine, and the trace looks just like the sample derivation above, but the Lisp definitions are a bit harder to read than the original grammar rules.
This problem will be compounded as we consider more complex rules.
Suppose we wanted to allow noun phrases to be modified by an indefinite number of adjectives and an indefinite number of prepositional phrases.
In grammatical notation, we might have the following rules:

  [ ](#){:#l0030}*Noun-Phrase*⇒ *Article + Adj* + Noun + PP**
!!!(p) {:.unnumlist}

  *Adj**⇒ *Ø, Adj + Adj**
!!!(p) {:.unnumlist}

  *PP**⇒ *Ø, PP + PP**
!!!(p) {:.unnumlist}

  *PP*⇒ *Prep + Noun-Phrase*
!!!(p) {:.unnumlist}

  *Adj*⇒ *big, little, blue, green*,…
!!!(p) {:.unnumlist}

  *Prep*⇒ *to, in, by, with*,…
!!!(p) {:.unnumlist}

In this notation, Ø indicates a choice of nothing at all, a comma indicates a choice of several alternatives, and the asterisk is nothing special–as in Lisp, it’s just part of the name of a symbol.
However, the convention used here is that names ending in an asterisk denote zero or more repetitions of the underlying name.
That is, *PP** denotes zero or more repetitions of *PP*.
This is known as “Kleene star” notation (pronounced "clean-E") after the mathematician Stephen Cole Kleene.[1](#fn0010){:#xfn0010}

The problem is that the rules for *Adj** and *PP** contain choices that we would have to represent as some kind of conditional in Lisp.
For example:

  [ ](#){:#l0035}`(defun Adj* ()`
!!!(p) {:.unnumlist}

    ` (if (= (random 2) 0)`
!!!(p) {:.unnumlist}

            ` nil`
!!!(p) {:.unnumlist}

            ` (append (Adj) (Adj*))))`
!!!(p) {:.unnumlist}

  `(defun PP* ()`
!!!(p) {:.unnumlist}

    ` (if (random-elt '(t nil))`
!!!(p) {:.unnumlist}

            ` (append (PP) (PP*))`
!!!(p) {:.unnumlist}

            ` nil))`
!!!(p) {:.unnumlist}

  `(defun noun-phrase () (append (Article) (Adj*) (Noun) (PP*)))`
!!!(p) {:.unnumlist}

  `(defun PP () (append (Prep) (noun-phrase)))`
!!!(p) {:.unnumlist}

  `(defun Adj () (one-of '(big little blue green adiabatic)))`
!!!(p) {:.unnumlist}

  `(defun Prep () (one-of '(to in by with on)))`
!!!(p) {:.unnumlist}

I’ve chosen two different implementations for `Adj*` and `PP*`; either approach would work in either function.
We have to be careful, though; here are two approaches that would not work:

  [ ](#){:#l0040}`(defun Adj* ()`
!!!(p) {:.unnumlist}

    ` "Warning - incorrect definition of Adjectives."`
!!!(p) {:.unnumlist}

    ` (one-of '(nil (append (Adj) (Adj*)))))`
!!!(p) {:.unnumlist}

  `(defun Adj* ()`
!!!(p) {:.unnumlist}

    ` "Warning - incorrect definition of Adjectives."`
!!!(p) {:.unnumlist}

    ` (one-of (list nil (append (Adj) (Adj*)))))`
!!!(p) {:.unnumlist}

The first definition is wrong because it could return the literal expression `((append (Adj) (Adj*)))` rather than a list of words as expected.
The second definition would cause infinite recursion, because Computing the value of `(Adj*)` always involves a recursive call to `(Adj*)`.
The point is that what started out as simple functions are now becoming quite complex.
To understand them, we need to know many Lisp conventions–`defun, (), case, if`, `quote`, and the rules for order of evaluation–when ideally the implementation of a grammar rule should use only *linguistic* conventions.
If we wanted to develop a larger grammar, the problem could get worse, because the rule-writer might have to depend more and more on Lisp.

## [ ](#){:#st0020}2.3 A Rule-Based Solution
{:#s0020}
{:.h1hd}

An alternative implementation of this program would concentrate on making it easy to write grammar rules and would worry later about how they will be processed.
Let’s look again at the original grammar rules:

  [ ](#){:#l0045}*Sentence*⇒ *Noun-Phrase + Verb-Phrase*
!!!(p) {:.unnumlist}

  *Noun-Phrase*⇒ *Article + Noun*
!!!(p) {:.unnumlist}

  *Verb-Phrase*⇒ *Verb + Noun-Phrase*
!!!(p) {:.unnumlist}

  *Article*⇒ *the, a,…*
!!!(p) {:.unnumlist}

  *Noun*⇒ *man, ball, woman, table …*
!!!(p) {:.unnumlist}

  *Verb*⇒ *hit, took, saw, liked…*
!!!(p) {:.unnumlist}

Each rule consists of an arrow with a symbol on the left-hand side and something on the right-hand side.
The complication is that there can be two kinds of right-hand sides: a concatenated list of symbols, as in “*Noun-Phrase ⇒ Article+Noun*,” or a list of alternate words, as in “*Noun ⇒ man, ball,…*” We can account for these possibilities by deciding that every rule will have a list of possibilities on the right-hand side, and that a concatenated list, *for example “Article+Noun,”* will be represented as a Lisp list, *for example*“(`Article Noun`)”.
The list of rules can then be represented as follows:

  [ ](#){:#l0050}`(defparameter *simple-grammar*`
!!!(p) {:.unnumlist}

    ` '((sentence -> (noun-phrase verb-phrase))`
!!!(p) {:.unnumlist}

      ` (noun-phrase -> (Article Noun))`
!!!(p) {:.unnumlist}

      ` (verb-phrase -> (Verb noun-phrase))`
!!!(p) {:.unnumlist}

      ` (Article -> the a)`
!!!(p) {:.unnumlist}

      ` (Noun -> man ball woman table)`
!!!(p) {:.unnumlist}

      ` (Verb -> hit took saw liked))`
!!!(p) {:.unnumlist}

    ` "A grammar for a trivial subset of English.")`
!!!(p) {:.unnumlist}

  `(defvar *grammar* *simple-grammar*`
!!!(p) {:.unnumlist}

    ` "The grammar used by generate.
Initially, this is`
!!!(p) {:.unnumlist}

    ` *simple-grammar*, but we can switch to other grammars.")`
!!!(p) {:.unnumlist}

Note that the Lisp version of the rules closely mimics the original version.
In particular, I include the symbol “->”, even though it serves no real purpose; it is purely decorative.

The special forms `defvar` and `defparameter` both introduce special variables and assign a value to them; the difference is that a *variable*, like `*grammar*,` is routinely changed during the course of running the program.
A *parameter*, like `*simple-grammar*`, on the other hand, will normally stay constant.
A change to a parameter is considered a change *to* the program, not a change *by* the program.

Once the list of rules has been defined, it can be used to find the possible rewrites of a given category symbol.
The function `assoc` is designed for just this sort of task.
It takes two arguments, a “key” and a list of lists, and returns the first element of the list of lists that starts with the key.
If there is none, it returns `nil`.
Here is an example:

  [ ](#){:#l0055}`> (assoc 'noun *grammar*)`⇒ `(NOUN -> MAN BALL WOMAN TABLE)`
!!!(p) {:.unnumlist}

Although rules are quite simply implemented as lists, it is a good idea to impose a layer of abstraction by defining functions to operate on the rules.
We will need three functions: one to get the right-hand side of a rule, one for the left-hand side, and one to look up all the possible rewrites (right-hand sides) for a category.

  [ ](#){:#l0060}`(defun rule-lhs (rule)`
!!!(p) {:.unnumlist}

    ` "The left-hand side of a rule."`
!!!(p) {:.unnumlist}

    ` (first rule))`
!!!(p) {:.unnumlist}

  `(defun rule-rhs (rule)`
!!!(p) {:.unnumlist}

    ` "The right-hand side of a rule."`
!!!(p) {:.unnumlist}

    ` (rest (rest rule)))`
!!!(p) {:.unnumlist}

  `(defun rewrites (category)`
!!!(p) {:.unnumlist}

    ` "Return a list of the possible rewrites for this category."`
!!!(p) {:.unnumlist}

    ` (rule-rhs (assoc category *grammar*)))`
!!!(p) {:.unnumlist}

Defining these functions will make it easier to read the programs that use them, and it also makes changing the representation of rules easier, should we ever decide to do so.

We are now ready to address the main problem: defining a function that will generate sentences (or noun phrases, or any other category).
We will call this function `generate.` It will have to contend with three cases: (1) In the simplest case, `generate` is passed a symbol that has a set of rewrite rules associated with it.
We choose one of those at random, and then generate from that.
(2) If the symbol has no possible rewrite rules, it must be a terminal symbol–a word, rather than a grammatical category–and we want to leave it alone.
Actually, we return the list of the input word, because, as in the previous program, we want all results to be lists of words.
(3) In some cases, when the symbol has rewrites, we will pick one that is a list of symbols, and try to generate from that.
Thus, `generate` must also accept a list as input, in which case it should generate each element of the list, and then append them all together.
In the following, the first clause in `generate` handles this case, while the second clause handles (1) and the third handles (2).
Note that we used the `mappend` function from [section 1.7](B9780080571157500017.xhtml#s0040) ([page 18](B9780080571157500017.xhtml#p18)).

  [ ](#){:#l0065}`(defun generate (phrase)`
!!!(p) {:.unnumlist}

    ` "Generate a random sentence or phrase"`
!!!(p) {:.unnumlist}

    ` (cond ((listp phrase)`
!!!(p) {:.unnumlist}

                ` (mappend #'generate phrase))`
!!!(p) {:.unnumlist}

                ` ((rewrites phrase)`
!!!(p) {:.unnumlist}

                 ` (generate (random-elt (rewrites phrase))))`
!!!(p) {:.unnumlist}

                ` (t (list phrase))))`
!!!(p) {:.unnumlist}

Like many of the programs in this book, this function is short, but dense with information: the craft of programming includes knowing what *not* to write, as well as what to write.

This style of programming is called *data-driven* programming, because the data (the list of rewrites associated with a category) drives what the program does next.
It is a natural and easy-to-use style in Lisp, leading to concise and extensible programs, because it is always possible to add a new piece of data with a new association without having to modify the original program.

Here are some examples of generate in use:

  [ ](#){:#l0070}`> (generate 'sentence)`⇒ `(THE TABLE SAW THE BALL)`
!!!(p) {:.unnumlist}

  `> (generate 'sentence)`⇒ `(THE WOMAN HIT A TABLE)`
!!!(p) {:.unnumlist}

  `> (generate 'noun-phrase)`⇒ `(THE MAN)`
!!!(p) {:.unnumlist}

  `> (generate 'verb-phrase)`⇒ `(TOOK A TABLE)`
!!!(p) {:.unnumlist}

There are many possible ways to write `generate`.
The following version uses `if` instead of `cond`:

  [ ](#){:#l0075}`(defun generate (phrase)`
!!!(p) {:.unnumlist}

    ` "Generate a random sentence or phrase"`
!!!(p) {:.unnumlist}

    ` (if (listp phrase)`
!!!(p) {:.unnumlist}

            ` (mappend #'generate phrase)`
!!!(p) {:.unnumlist}

            ` (let ((choices (rewrites phrase)))`
!!!(p) {:.unnumlist}

              ` (if (null choices)`
!!!(p) {:.unnumlist}

                      ` (list phrase)`
!!!(p) {:.unnumlist}

                      ` (generate (random-elt choices))))))`
!!!(p) {:.unnumlist}

This version uses the special form `let`, which introduces a new variable (in this case, `choices`) and also binds the variable to a value.
In this case, introducing the variable saves us from calling the function `rewrites twice`, as was done in the `cond` version of `generate`.
The general form of a `let` form is:

  [ ](#){:#l0080}`(let` ((*var value*)…)
!!!(p) {:.unnumlist}

    * body-containing-vars*)
!!!(p) {:.unnumlist}

`let` is the most common way of introducing variables that are not parameters of functions.
One must resist the temptation to use a variable without introducing it:

  [ ](#){:#l0085}`(defun generate (phrase)`
!!!(p) {:.unnumlist}

  ` (setf choices …) ;; wrong!`
!!!(p) {:.unnumlist}

  ` … choices …)`
!!!(p) {:.unnumlist}

This is wrong because the symbol `choices` now refers to a special or global variable, one that may be shared or changed by other functions.
Thus, the function generate is not reliable, because there is no guarantee that `choices` will retain the same value from the time it is set to the time it is referenced again.
With `let` we introduce a brand new variable that nobody else can access; therefore it is guaranteed to maintain the proper value.

**Exercise 2.1 [m]** Write a version of `generate` that uses `cond` but avoids calling `rewrites twice`.

**Exercise 2.2 [m]** Write a version of `generate` that explicitly differentiates between terminal symbols (those with no rewrite rules) and nonterminal symbols.

## [ ](#){:#st0025}2.4 Two Paths to Follow
{:#s0025}
{:.h1hd}

The two versions of the preceding program represent two alternate approaches that come up time and time again in developing programs: (1) Use the most straightforward mapping of the problem description directly into Lisp code.
(2) Use the most natural notation available to solve the problem, and then worry about writing an interpreter for that notation.

Approach (2) involves an extra step, and thus is more work for small problems.
However, programs that use this approach are often easier to modify and expand.
This is especially true in a domain where there is a lot of data to account for.
The grammar of natural language is one such domain–in fact, most AI problems fit this description.
The idea behind approach (2) is to work with the problem as much as possible in its own terms, and to minimize the part of the solution that is written directly in Lisp.

Fortunately, it is very easy in Lisp to design new notations–in effect, new programming languages.
Thus, Lisp encourages the construction of more robust programs.
Throughout this book, we will be aware of the two approaches.
The reader may notice that in most cases, we choose the second.

## [ ](#){:#st0030}2.5 Changing the Grammar without Changing the Program
{:#s0030}
{:.h1hd}

We show the utility of approach (2) by defining a new grammar that includes adjectives, prepositional phrases, proper names, and pronouns.
We can then apply the `generate` function without modification to this new grammar.

  [ ](#){:#l0090}`(defparameter *bigger-grammar*`
!!!(p) {:.unnumlist}

    ` '((sentence -> (noun-phrase verb-phrase))`
!!!(p) {:.unnumlist}

        ` (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))`
!!!(p) {:.unnumlist}

        ` (verb-phrase -> (Verb noun-phrase PP*))`
!!!(p) {:.unnumlist}

        ` (PP* -> () (PP PP*))`
!!!(p) {:.unnumlist}

        ` (Adj* -> () (Adj Adj*))`
!!!(p) {:.unnumlist}

        ` (PP -> (Prep noun-phrase))`
!!!(p) {:.unnumlist}

        ` (Prep -> to in by with on)`
!!!(p) {:.unnumlist}

        ` (Adj -> big little blue green adiabatic)`
!!!(p) {:.unnumlist}

        ` (Article -> the a)`
!!!(p) {:.unnumlist}

        ` (Name -> Pat Kim Lee Terry Robin)`
!!!(p) {:.unnumlist}

        ` (Noun -> man ball woman table)`
!!!(p) {:.unnumlist}

        ` (Verb -> hit took saw liked)`
!!!(p) {:.unnumlist}

        ` (Pronoun -> he she it these those that)))`
!!!(p) {:.unnumlist}

  `(setf *grammar* *bigger-grammar*)`
!!!(p) {:.unnumlist}

  `> (generate 'sentence)`
!!!(p) {:.unnumlist}

  `(A TABLE ON A TABLE IN THE BLUE ADIABATIC MAN SAW ROBIN`
!!!(p) {:.unnumlist}

  `WITH A LITTLE WOMAN)`
!!!(p) {:.unnumlist}

  `> (generate 'sentence)`
!!!(p) {:.unnumlist}

  `(TERRY SAW A ADIABATIC TABLE ON THE GREEN BALL BY THAT WITH KIM IN THESE BY A GREEN WOMAN BY A LITTLE ADIABATIC TABLE IN ROBIN ON LEE)`
!!!(p) {:.unnumlist}

  `> (generate 'sentence)`
!!!(p) {:.unnumlist}

  `(THE GREEN TABLE HIT IT WITH HE)`
!!!(p) {:.unnumlist}

Notice the problem with case agreement for pronouns: the program generated “with he,” although “with him” is the proper grammatical form.
Also, it is clear that the program does not distinguish sensible from silly output.

## [ ](#){:#st0035}2.6 Using the Same Data for Several Programs
{:#s0035}
{:.h1hd}

Another advantage of representing information in a declarative form–as rules or facts rather than as Lisp functions–is that it can be easier to use the information for multiple purposes.
Suppose we wanted a function that would generate not just the list of words in a sentence but a representation of the complete syntax of a sentence.
For example, instead of the list `(a woman took a ball)`, we want to get the nested list:

  [ ](#){:#l0095}`(SENTENCE (NOUN-PHRASE (ARTICLE A) (NOUN WOMAN))`
!!!(p) {:.unnumlist}

                    ` (VERB-PHRASE (VERB TOOK)`
!!!(p) {:.unnumlist}

                                              ` (NOUN-PHRASE (ARTICLE A) (NOUN BALL))))`
!!!(p) {:.unnumlist}

This corresponds to the tree that linguists draw as in [figure 2.1](#f0010).

![f02-01-9780080571157](images/B9780080571157500029/f02-01-9780080571157.jpg)     
Figure 2.1:
!!!(span) {:.fignum}
Sentence Parse Tree
Using the “straightforward functions” approach we would be stuck; we’d have to rewrite every function to generate the additional structure.
With the “new notation” approach we could keep the grammar as it is and just write one new function: a version of `generate` that produces nested lists.
The two changes are to `cons` the category onto the front of each rewrite, and then not to `append` together the results but rather just list them with `mapcar`:

  [ ](#){:#l0100}`(defun generate-tree (phrase)`
!!!(p) {:.unnumlist}

    ` "Generate a random sentence or phrase,`
!!!(p) {:.unnumlist}

    ` with a complete parse tree."`
!!!(p) {:.unnumlist}

    ` (cond ((listp phrase)`
!!!(p) {:.unnumlist}

                 ` (mapcar #'generate-tree phrase))`
!!!(p) {:.unnumlist}

                ` ((rewrites phrase)`
!!!(p) {:.unnumlist}

                 ` (cons phrase`
!!!(p) {:.unnumlist}

                            ` (generate-tree (random-elt (rewrites phrase)))))`
!!!(p) {:.unnumlist}

                ` (t (list phrase))))`
!!!(p) {:.unnumlist}

Here are some examples:

  [ ](#){:#l0105}`> (generate-tree 'Sentence)`
!!!(p) {:.unnumlist}

  `(SENTENCE (NOUN-PHRASE (ARTICLE A)`
!!!(p) {:.unnumlist}

                                              ` (ADJ*)`
!!!(p) {:.unnumlist}

                                              ` (NOUN WOMAN)`
!!!(p) {:.unnumlist}

                                              ` (PP*))`
!!!(p) {:.unnumlist}

      ` (VERB-PHRASE (VERB HIT)`
!!!(p) {:.unnumlist}

                                              ` (NOUN-PHRASE (PRONOUN HE))`
!!!(p) {:.unnumlist}

                                              ` (PP*)))`
!!!(p) {:.unnumlist}

  `> (generate-tree 'Sentence)`
!!!(p) {:.unnumlist}

  `(SENTENCE (NOUN-PHRASE (ARTICLE A)`
!!!(p) {:.unnumlist}

                                              ` (NOUN WOMAN))`
!!!(p) {:.unnumlist}

                   ` (VERB-PHRASE (VERB TOOK)`
!!!(p) {:.unnumlist}

                                              ` (NOUN-PHRASE (ARTICLE A) (NOUN BALL))))`
!!!(p) {:.unnumlist}

As another example of the one-data/multiple-program approach, we can develop a function to generate all possible rewrites of a phrase.
The function `generate-all` returns a list of phrases rather than just one, and we define an auxiliary function, `combine-all`, to manage the combination of results.
Also, there are four cases instead of three, because we have to check for nil explicitly.
Still, the complete program is quite simple:

  [ ](#){:#l0110}`(defun generate-all (phrase)`
!!!(p) {:.unnumlist}

    ` "Generate a list of all possible expansions of this phrase."`
!!!(p) {:.unnumlist}

    ` (cond ((null phrase) (list nil))`
!!!(p) {:.unnumlist}

                ` ((listp phrase)`
!!!(p) {:.unnumlist}

                 ` (combine-all (generate-all (first phrase))`
!!!(p) {:.unnumlist}

                                           ` (generate-all (rest phrase))))`
!!!(p) {:.unnumlist}

                ` ((rewrites phrase)`
!!!(p) {:.unnumlist}

                 ` (mappend #'generate-all (rewrites phrase)))`
!!!(p) {:.unnumlist}

                ` (t (list (list phrase)))))`
!!!(p) {:.unnumlist}

  `(defun combine-all (xlist ylist)`
!!!(p) {:.unnumlist}

    ` "Return a list of lists formed by appending a y to an x.`
!!!(p) {:.unnumlist}

    ` E.g., (combine-all '((a) (b)) '((1) (2)))`
!!!(p) {:.unnumlist}

      `-> ((A 1) (B 1) (A 2) (B 2))."`
!!!(p) {:.unnumlist}

    ` (mappend #'(lambda (y)`
!!!(p) {:.unnumlist}

                            ` (mapcar #'(lambda (x) (append x y)) xlist))`
!!!(p) {:.unnumlist}

                      ` ylist))`
!!!(p) {:.unnumlist}

We can now use `generate-all` to test our original little grammar.
Note that a serious drawback of `generate-all` is that it can’t deal with recursive grammar rules like 'Adj* ⇒ Adj + Adj*' that appear in `*bigger-grammar*,` since these lead to an infinite number of outputs.
But it works fine for finite languages, like the language generated by *si`mple-grammar*:`

  [ ](#){:#l0115}`> (generate-all 'Article)`
!!!(p) {:.unnumlist}

  `((THE) (A))`
!!!(p) {:.unnumlist}

  `> (generate-all 'Noun)`
!!!(p) {:.unnumlist}

  `((MAN) (BALL) (WOMAN) (TABLE))`
!!!(p) {:.unnumlist}

  `> (generate-all 'noun-phrase)`
!!!(p) {:.unnumlist}

  `((A MAN) (A BALL) (A WOMAN) (A TABLE)`
!!!(p) {:.unnumlist}

  `(THE MAN) (THE BALL) (THE WOMAN) (THE TABLE))`
!!!(p) {:.unnumlist}

  `> (length (generate-all 'sentence))`
!!!(p) {:.unnumlist}

  `256`
!!!(p) {:.unnumlist}

There are 256 sentences because every sentence in this language has the form Article- Noun-Verb-Article-Noun, and there are two articles, four nouns and four verbs (2 × 4 × 4 × 2 × 4 = 256).

## [ ](#){:#st0040}2.7 Exercises
{:#s0040}
{:.h1hd}

**Exercise 2.3 [h]** Write a trivial grammar for some other language.
This can be a natural language other than English, or perhaps a subset of a computer language.

**Exercise 2.4 [m]** One way of describing `combine-all` is that it calculates the cross-product of the function `append` on the argument lists.
Write the higher-order function `cross-product`, and define `combine-all` in terms of it.

The moral is to make your code as general as possible, because you never know what you may want to do with it next.

## [ ](#){:#st0045}2.8 Answers
{:#s0045}
{:.h1hd}

**Answer 2.1**

  [ ](#){:#l0120}`(defun generate (phrase)`
!!!(p) {:.unnumlist}

    ` "Generate a random sentence or phrase"`
!!!(p) {:.unnumlist}

    ` (let ((choices nil))`
!!!(p) {:.unnumlist}

      ` (cond ((listp phrase)`
!!!(p) {:.unnumlist}

                   ` (mappend #'generate phrase))`
!!!(p) {:.unnumlist}

                  ` ((setf choices (rewrites phrase))`
!!!(p) {:.unnumlist}

                   ` (generate (random-elt choices)))`
!!!(p) {:.unnumlist}

                  ` (t (list phrase)))))`
!!!(p) {:.unnumlist}

**Answer 2.2**

  [ ](#){:#l0125}`(defun generate (phrase)`
!!!(p) {:.unnumlist}

    ` "Generate a random sentence or phrase"`
!!!(p) {:.unnumlist}

    ` (cond ((listp phrase)`
!!!(p) {:.unnumlist}

                   ` (mappend #'generate phrase))`
!!!(p) {:.unnumlist}

                  ` ((non-terminal-p phrase)`
!!!(p) {:.unnumlist}

                    ` (generate (random-elt (rewrites phrase))))`
!!!(p) {:.unnumlist}

                  ` (t (list phrase))))`
!!!(p) {:.unnumlist}

  `(defun non-terminal-p (category)`
!!!(p) {:.unnumlist}

    ` "True if this is a category in the grammar."`
!!!(p) {:.unnumlist}

    ` (not (null (rewrites category))))`
!!!(p) {:.unnumlist}

**Answer 2.4**

  [ ](#){:#l0130}`(defun cross-product (fn xlist ylist)`
!!!(p) {:.unnumlist}

    ` "Return a list of all (fn x y) values."`
!!!(p) {:.unnumlist}

    ` (mappend #'(lambda (y)`
!!!(p) {:.unnumlist}

                              ` (mapcar #'(lambda (x) (funcall fn x y))`
!!!(p) {:.unnumlist}

                                      ` xlist))`
!!!(p) {:.unnumlist}

                    ` ylist))`
!!!(p) {:.unnumlist}

  `(defun combine-all (xlist ylist)`
!!!(p) {:.unnumlist}

    ` "Return a list of lists formed by appending a y to an x"`
!!!(p) {:.unnumlist}

    ` (cross-product #'append xlist ylist))`
!!!(p) {:.unnumlist}

Now we can use the `cross-product` in other ways as well:

  [ ](#){:#l0135}`> (cross-product #'+ '(1 2 3) '(10 20 30))`
!!!(p) {:.unnumlist}

  `(11 12 13`
!!!(p) {:.unnumlist}

  ` 21 22 23`
!!!(p) {:.unnumlist}

  ` 31 32 33)`
!!!(p) {:.unnumlist}

  `> (cross-product #'list '(a b c d e f g h)`
!!!(p) {:.unnumlist}

  `                        '(1 2 3 4 5 6 7 8))`
!!!(p) {:.unnumlist}

  `((A 1) (B 1) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)`
!!!(p) {:.unnumlist}

  ` (A 2) (B 2) (C 2) (D 2) (E 2) (F 2) (G 2) (H 2)`
!!!(p) {:.unnumlist}

  ` (A 3) (B 3) (C 3) (D 3) (E 3) (F 3) (G 3) (H 3)`
!!!(p) {:.unnumlist}

  ` (A 4) (B 4) (C 4) (D 4) (E 4) (F 4) (G 4) (H 4)`
!!!(p) {:.unnumlist}

  ` (A 5) (B 5) (C 5) (D 5) (E 5) (F 5) (G 5) (H 5)`
!!!(p) {:.unnumlist}

  ` (A 6) (B 6) (C 6) (D 6) (E 6) (F 6) (G 6) (H 6)`
!!!(p) {:.unnumlist}

  ` (A 7) (B 7) (C 7) (D 7) (E 7) (F 7) (G 7) (H 7)`
!!!(p) {:.unnumlist}

  ` (A 8) (B 8) (C 8) (D 8) (E 8) (F 8) (G 8) (H 8))`
!!!(p) {:.unnumlist}

----------------------

[1](#xfn0010){:#np0010} We will soon see “Kleene plus” notation, wherein *PP+* denotes one or more repetition of *PP*.
!!!(p) {:.ftnote1}

