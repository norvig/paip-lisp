# Chapter 20
## Unification Grammars

Prolog was invented because Alain Colmerauer wanted a formalism to describe the grammar of French.
His intuition was that the combination of Horn clauses and unification resulted in a language that was just powerful enough to express the kinds of constraints that show up in natural languages, while not as powerful as, for example, full predicate calculus.
This lack of power is important, because it enables efficient implementation of Prolog, and hence of the language-analysis programs built on top of it.

Of course, Prolog has evolved and is now used for many applications besides natural language, but Colmerauer’s underlying intuition remains a good one.
This chapter shows how to view a grammar as a set of logic programming clauses.
The clauses define what is a legal sentence and what isn’t, without any explicit reference to the process of parsing or generation.
The amazing thing is that the clauses can be defined in a way that leads to a very efficient parser.
Furthermore, the same grammar can be used for both parsing and generation (at least in some cases).

## [ ](#){:#st0010}20.1 Parsing as Deduction
{:#s0010}
{:.h1hd}

Here’s how we could express the grammar rule “A sentence can be composed of a noun phrase followed by a verb phrase” in Prolog:

[ ](#){:#l0010}`(<− (S ?s)`
!!!(p) {:.unnumlist}

   `(NP ?np)`
!!!(p) {:.unnumlist}

   `(VP ?vp)`
!!!(p) {:.unnumlist}

   `(concat ?np ?vp ?s))`
!!!(p) {:.unnumlist}

The variables represent strings of words.
As usual, they will be implemented as lists of symbols.
The rule says that a given string of words `?s` is a sentence if there is a string that is noun phrase and one that is a verb phrase, and if they can be concatenated to form `?s`.
Logically, this is fine, and it would work as a program to generate random sentences.
However, it is a very inefficient program for parsing sentences.
It will consider all possible noun phrases and verb phrases, without regard to the input words.
Only when it gets to the concat goal (defined on [page 411](B9780080571157500121.xhtml#p411)) will it test to see if the two constituents can be concatenated together to make up the input string.
Thus, a better order of evaluation for parsing is:

[ ](#){:#l0015}`(<− (S ?s)`
!!!(p) {:.unnumlist}

   `(concat ?np ?vp ?s)`
!!!(p) {:.unnumlist}

   `(NP ?np)`
!!!(p) {:.unnumlist}

   `(VP ?vp))`
!!!(p) {:.unnumlist}

The first version had `NP` and `VP` guessing strings to be verified by `concat`.
In most grammars, there will be a very large or infinite number of `NPs` and `VPs`.
This second version has `concat` guessing strings to be verified by `NP` and `VP`.
If there are *n* words in the sentence, then concat can only make *n* + 1 guesses, quite an improvement.
However, it would be better still if we could in effect have `concat` and `NP` work together to make a more constrained guess, which would then be verified by `VP`.

We have seen this type of problem before.
In Lisp, the answer is to return multiple values.
`NP` would be a function that takes a string as input and returns two values: an indication of success or failure, and a remainder string of words that have not yet been parsed.
When the first value indicates success, then `VP` would be called with the remaining string as input.
In Prolog, return values are just extra arguments.
So each predicate will have two parameters: an input string and a remainder string.
Following the usual Prolog convention, the output parameter comes after the input.
In this approach, no calls to concat are necessary, no wild guesses are made, and Prolog’s backtracking takes care of the necessary guessing:

[ ](#){:#l0020}`(<− (S ?s0 ?s2)`
!!!(p) {:.unnumlist}

       `(NP ?s0 ?sl)`
!!!(p) {:.unnumlist}

       `(VP ?sl ?s2))`
!!!(p) {:.unnumlist}

This rule can be read as “The string from `*s*0` to `*s*2` is a sentence if there is an `*s*1` such that the string from `s0` to `*s*1` is a noun phrase and the string from `*s*1` to `*s*2` is a verb phrase.”

A sample query would be `(?- (S (The boy ate the apple) ())).` With suitable definitions of `NP` and `VP`, this would succeed, with the following bindings holding within `S`:

[ ](#){:#l0025}`?s0 = (The boy ate the apple)`
!!!(p) {:.unnumlist}

`?sl =         (ate the apple)`
!!!(p) {:.unnumlist}

`?s2 =                      ()`
!!!(p) {:.unnumlist}

Another way of reading the goal `(NP ?s0 ?sl)`, for example, is as “`IS` the list `?s0` minus the list `?sl` a noun phrase?” In this case, `?s0` minus `?sl` is the list `(The boy)`.
The combination of two arguments, an input list and an output list, is often called a *difference list*, to emphasize this interpretation.
More generally, the combination of an input parameter and output parameter is called an *accumulator.* Accumulators, particularly difference lists, are an important technique throughout logic programming and are also used in functional programming, as we saw on [page 63](B9780080571157500030.xhtml#p63).

In our rule for `S`, the concatenation of difference lists was implicit.
If we prefer, we could define a version of `concat` for difference lists and call it explicitly:

[ ](#){:#l0030}`(<− (S ?s-in ?s-rem)`
!!!(p) {:.unnumlist}

       `(NP ?np-in ?np-rem)`
!!!(p) {:.unnumlist}

       `(VP ?vp-in ?vp-rem)`
!!!(p) {:.unnumlist}

       `(concat ?np-in ?np-rem ?vp-in ?vp-rem ?s-in ?s-rem))`
!!!(p) {:.unnumlist}

`(<− (concat ?a ?b ?b ?c ?a ?c))`
!!!(p) {:.unnumlist}

Because this version of `concat` has a different arity than the old version, they can safely coexist.
It states the difference list equation *(a − b) + (b − c) = (a − c)*.

In the last chapter we stated that context-free phrase-structure grammar is inconvenient for expressing things like agreement between the subject and predicate of a sentence.
With the Horn-clause-based grammar formalism we are developing here, we can add an argument to the predicates NP and VP to represent agreement.
In English, the agreement rule does not have a big impact.
For all verbs except *be,* the difference only shows up in the third-person singular of the present tense:

[ ](#){:#t0010}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| | Singular | Plural |
| first person second person third person | I you he/she | sleep sleep sleeps | we you they | sleep sleep sleep |

![t0010](images/B9780080571157500200/t0010.png)

Thus, the agreement argument will take on one of the two values `3sg` or `˜3sg` to indicate third-person-singular or not-third-person-singular.
We could write:

[ ](#){:#l0035}`(<− (S ?s0 ?s2)`
!!!(p) {:.unnumlist}

       `(NP ?agr ?s0 ?sl)`
!!!(p) {:.unnumlist}

       `(VP ?agr ?sl ?s2))`
!!!(p) {:.unnumlist}

`(<− (NP 3sg (he .
?s) ?s))`
!!!(p) {:.unnumlist}

`(<− (NP`˜`3sg (they .
?s) ?s))`
!!!(p) {:.unnumlist}

`(<− (VP 3sg (sleeps .
?s) ?s))`
!!!(p) {:.unnumlist}

`(<− (VP`˜`3sg (sleep .
?s) ?s))`
!!!(p) {:.unnumlist}

This grammar parses just the right sentences:

[ ](#){:#l0040}`> (?- (S (He sleeps) ()))`
!!!(p) {:.unnumlist}

`Yes.`
!!!(p) {:.unnumlist}

`> (?- (S (He sleep) ()))`
!!!(p) {:.unnumlist}

`No.`
!!!(p) {:.unnumlist}

Let’s extend the grammar to allow common nouns as well as pronouns:

[ ](#){:#l0045}`(<− (NP ?agr ?s0 ?s2)`
!!!(p) {:.unnumlist}

       `(Det ?agr ?s0 ?sl)`
!!!(p) {:.unnumlist}

       `(N ?agr ?sl ?s2))`
!!!(p) {:.unnumlist}

`(<− (Det ?any (the .
?s) ?s))`
!!!(p) {:.unnumlist}

`(<− (N 3sg (boy .
?s) ?s))`
!!!(p) {:.unnumlist}

`(<− (N 3sg (girl .
?s) ?s))`
!!!(p) {:.unnumlist}

The same grammar rules can be used to generate sentences as well as parse.
Here are all possible sentences in this trivial grammar:

[ ](#){:#l0050}`> (?- (S ?words ()))`
!!!(p) {:.unnumlist}

`?WORDS = (HE SLEEPS);`
!!!(p) {:.unnumlist}

`?W0RDS = (THEY SLEEP);`
!!!(p) {:.unnumlist}

`?WORDS = (THE BOY SLEEPS);`
!!!(p) {:.unnumlist}

`?WORDS = (THE GIRL SLEEPS);`
!!!(p) {:.unnumlist}

`No.`
!!!(p) {:.unnumlist}

So far all we have is a recognizer: a predicate that can separate sentences from nonsentences.
But we can add another argument to each predicate to build up the semantics.
The result is not just a recognizer but a true parser:

[ ](#){:#l0055}`(<− (S (?pred ?subj) ?s0 ?s2)`
!!!(p) {:.unnumlist}

       `(NP ?agr ?subj ?s0 ?sl)`
!!!(p) {:.unnumlist}

        `(VP ?agr ?pred ?sl ?s2))`
!!!(p) {:.unnumlist}

`(<− (NP 3sg (the male) (he .
?s) ?s))`
!!!(p) {:.unnumlist}

`(<− (NP`˜`3sg (some objects) (they .
?s) ?s))`
!!!(p) {:.unnumlist}

`(<− (NP ?agr (?det ?n) ?s0 ?s2)`
!!!(p) {:.unnumlist}

        `(Det ?agr ?det ?s0 ?sl)`
!!!(p) {:.unnumlist}

        `(N ?agr ?n ?sl ?s2))`
!!!(p) {:.unnumlist}

`(<− (VP 3sg sleep (sleeps .
?s) ?s))`
!!!(p) {:.unnumlist}

`(<− (VP`˜`3sg sleep (sleep .
?s) ?s))`
!!!(p) {:.unnumlist}

`(<− (Det ?any the (the .
?s) ?s))`
!!!(p) {:.unnumlist}

`(<− (N 3sg (young male human) (boy .
?s) ?s))`
!!!(p) {:.unnumlist}

`(<− (N 3sg (young female human) (girl .
?s) ?s))`
!!!(p) {:.unnumlist}

The semantic translations of individual words is a bit capricious.
In fact, it is not too important at this point if the translation of `boy` is `(young male human)` or just `boy`.
There are two properties of a semantic representation that are important.
First, it should be unambiguous.
The representation of *orange* the fruit should be different from *orange* the color (although the representation of the fruit might well refer to the color, or vice versa).
Second, it should express generalities, or allow them to be expressed elsewhere.
So either *sleep* and *sleeps* should have the same or similar representation, or there should be an inference rule relating them.
Similarly, if the representation of *boy* does not say so explicitly, there should be some other rule saying that a boy is a male and a human.

Once the semantics of individual words is decided, the semantics of higher-level categories (sentences and noun phrases) is easy.
In this grammar, the semantics of a sentence is the application of the predicate (the verb phrase) to the subject (the noun phrase).
The semantics of a compound noun phrase is the application of the determiner to the noun.

This grammar returns the semantic interpretation but does not build a syntactic tree.
The syntactic structure is implicit in the sequence of goals: `S` calls `NP` and `VP`, and `NP` can call `Det` and `N`.
If we want to make this explicit, we can provide yet another argument to each nonterminal:

[ ](#){:#l0060}`(<− (S (?pred ?subj) (s ?np ?vp) ?s0 ?s2)`
!!!(p) {:.unnumlist}

       `(NP ?agr ?subj ?np ?s0 ?sl)`
!!!(p) {:.unnumlist}

        `(VP ?agr ?pred ?vp ?sl ?s2))`
!!!(p) {:.unnumlist}

`(<− (NP 3sg (the male) (np he) (he .
?s) ?s))`
!!!(p) {:.unnumlist}

`(<− (NP`˜`3sg (some objects) (np they) (they .
?s) ?s))`
!!!(p) {:.unnumlist}

`(<− (NP ?agr (?det ?n) (np ?det-syn ?n-syn)?s0 ?s2)`
!!!(p) {:.unnumlist}

        `(Det ?agr ?det ?det-syn ?s0 ?sl)`
!!!(p) {:.unnumlist}

        `(N ?agr ?n ?n-syn ?sl ?s2))`
!!!(p) {:.unnumlist}

`(<− (VP 3sg sleep (vp sleeps)(sleeps .
?s) ?s))`
!!!(p) {:.unnumlist}

`(<− (VP`˜`3sg sleep (vp sleep) (sleep .
?s) ?s))`
!!!(p) {:.unnumlist}

`(<− (Det ?any the (det the) (the .
?s) ?s))`
!!!(p) {:.unnumlist}

`(<− (N 3sg (young male human) (n boy) (boy .
?s) ?s))`
!!!(p) {:.unnumlist}

`(<− (N 3sg (young female human) (n girl) (girl .
?s) ?s))`
!!!(p) {:.unnumlist}

This grammar can still be used to parse or generate sentences, or even to enumerate all syntax/semantics/sentence triplets:

[ ](#){:#l0065}`;; Parsing:`
!!!(p) {:.unnumlist}

`> (?- (S ?sem ?syn (He sleeps) ()))`
!!!(p) {:.unnumlist}

`?SEM = (SLEEP (THE MALE))`
!!!(p) {:.unnumlist}

`?SYN = (S (NP HE) (VP SLEEPS)).`
!!!(p) {:.unnumlist}

`;; Generating:`
!!!(p) {:.unnumlist}

`> (?- (S (sleep (the male)) ?
?words ()))`
!!!(p) {:.unnumlist}

`?WORDS = (HE SLEEPS)`
!!!(p) {:.unnumlist}

`;; Enumerating:`
!!!(p) {:.unnumlist}

`> (?- (S ?sem ?syn ?words ()))`
!!!(p) {:.unnumlist}

`?SEM = (SLEEP (THE MALE))`
!!!(p) {:.unnumlist}

`?SYN = (S (NP HE) (VP SLEEPS))`
!!!(p) {:.unnumlist}

`?WORDS = (HE SLEEPS);`
!!!(p) {:.unnumlist}

`?SEM = (SLEEP (SOME OBJECTS))`
!!!(p) {:.unnumlist}

`?SYN = (S (NP THEY) (VP SLEEP))`
!!!(p) {:.unnumlist}

`?WORDS = (THEY SLEEP);`
!!!(p) {:.unnumlist}

`?SEM = (SLEEP (THE (YOUNG MALE HUMAN)))`
!!!(p) {:.unnumlist}

`?SYN = (S (NP (DET THE) (N BOY)) (VP SLEEPS))`
!!!(p) {:.unnumlist}

`?WORDS = (THE BOY SLEEPS);`
!!!(p) {:.unnumlist}

`?SEM = (SLEEP (THE (YOUNG FEMALE HUMAN)))`
!!!(p) {:.unnumlist}

`?SYN = (S (NP (DET THE) (N GIRL)) (VP SLEEPS))`
!!!(p) {:.unnumlist}

`?WORDS = (THE GIRL SLEEPS);`
!!!(p) {:.unnumlist}

`No.`
!!!(p) {:.unnumlist}

## [ ](#){:#st0015}20.2 Definite Clause Grammars
{:#s0015}
{:.h1hd}

We now have a powerful and efficient tool for parsing sentences.
However, it is getting to be a very messy tool–there are too many arguments to each goal, and it is hard to tell which arguments represent syntax, which represent semantics, which represent in/out strings, and which represent other features, like agreement.
So, we will take the usual step when our bare programming language becomes messy: define a new language.

Edinburgh Prolog recognizes assertions called *definite clause grammar* (DCG) rules.
The term *definite clause* is just another name for a Prolog clause, so DCGs are also called “logic grammars.” They could have been called “Horn clause grammars” or “Prolog grammars” as well.

DCG rules are clauses whose main functor is an arrow, usually written -->.
They compile into regular Prolog clauses with extra arguments.
In normal DCG rules, only the string arguments are automatically added.
But we will see later how this can be extended to add other arguments automatically as well.

We will implement DCG rules with the macro `rule` and an infix arrow.
Thus, we want the expression:

[ ](#){:#l0070}`(rule (S) --> (NP) (VP))`
!!!(p) {:.unnumlist}

to expand into the clause:

[ ](#){:#l0075}`(<− (S ?s0 ?s2)`
!!!(p) {:.unnumlist}

       `(NP ?s0 ?sl)`
!!!(p) {:.unnumlist}

       `(VP ?sl ?s2))`
!!!(p) {:.unnumlist}

While we’re at it, we may as well give `rule` the ability to deal with different types of rules, each one represented by a different type of arrow.
Here’s the `rule` macro:

[ ](#){:#l0080}`(defmacro rule (head &optional (arrow ’:-) &body body)`
!!!(p) {:.unnumlist}

   `“Expand one of several types of logic rules into pure Prolog.”`
!!!(p) {:.unnumlist}

   `;; This is data-driven, dispatching on the arrow`
!!!(p) {:.unnumlist}

   `(funcall (get arrow ’rule-function) head body))`
!!!(p) {:.unnumlist}

As an example of a rule function, the arrow : - will be used to represent normal Prolog clauses.
That is, the form (`rule`*head : − body*) will be equivalent to (<− *head body).*

[ ](#){:#l0085}`(setf (get ’:- ’rule-function)`
!!!(p) {:.unnumlist}

          `#’(lambda (head body) ‘(<− ,head .,body)))`
!!!(p) {:.unnumlist}

Before writing the rule function for DCG rules, there are two further features of the DCG formalism to consider.
First, some goals in the body of a rule may be normal Prolog goals, and thus do not require the extra pair of arguments.
In Edinburgh Prolog, such goals are surrounded in braces.
One would write:

[ ](#){:#l0090}`s(Sem) --> np(Subj), vp(Pred),`
!!!(p) {:.unnumlist}

                  `{combi ne(Subj,Pred,Sem)}.`
!!!(p) {:.unnumlist}

where the idea is that `combine` is not a grammatical constituent, but rather a Prolog predicate that could do some calculations on `Subj` and `Pred` to arrive at the proper semantics, `Sem`.
We will mark such a test predicate not by brackets but by a list headed by the keyword `:test`, as in:

[ ](#){:#l0095}`(rule (S ?sem) --> (NP ?subj) (VP ?pred)`
!!!(p) {:.unnumlist}

   `(:test (combine ?subj ?pred ?sem)))`
!!!(p) {:.unnumlist}

Second, we need some way of introducing individual words on the right-hand side, as opposed to categories of words.
In Prolog, brackets are used to represent a word or list of words on the right-hand side:

[ ](#){:#l0100}`verb --> [sleeps].`
!!!(p) {:.unnumlist}

We will use a list headed by the keyword `:word:`

[ ](#){:#l0105}`(rule (NP (the male) 3sg) --> (:word he))`
!!!(p) {:.unnumlist}

`(rule (VP sleeps 3sg) --> (:word sleeps))`
!!!(p) {:.unnumlist}

The following predicates test for these two special cases.
Note that the cut is also allowed as a normal goal.

[ ](#){:#l0110}`(defun dcg-normal-goal-p (x) (or (starts-with x :test) (eq x ’!)))`
!!!(p) {:.unnumlist}

`(defun dcg-word-list-p (x) (starts-with x ’:word))`
!!!(p) {:.unnumlist}

At last we are in a position to present the rule function for DCG rules.
The function `make-dcg` inserts variables to keep track of the strings that are being parsed.

[ ](#){:#l0115}`(setf (get ’-->’rule-function) ’make-dcg)`
!!!(p) {:.unnumlist}

`(defun make-dcg (head body)`
!!!(p) {:.unnumlist}

  `(let ((n (count-if (complement #’dcg-normal-goal-p) body)))`
!!!(p) {:.unnumlist}

    `‘(<− (,@head ?sO .(symbol ’?s n))`
!!!(p) {:.unnumlist}

            `.,(make-deg-body body 0))))`
!!!(p) {:.unnumlist}

`(defun make-dcg-body (body n)`
!!!(p) {:.unnumlist}

  `“Make the body of a Definite Clause Grammar (DCG) clause.`
!!!(p) {:.unnumlist}

  `Add ?string-in and -out variables to each constituent.`
!!!(p) {:.unnumlist}

  `Goals like (:test goal) are ordinary Prolog goals,`
!!!(p) {:.unnumlist}

  `and goals like (:word hello) are literal words to be parsed.”`
!!!(p) {:.unnumlist}

  `(if (null body)`
!!!(p) {:.unnumlist}

          `nil`
!!!(p) {:.unnumlist}

          `(let ((goal (first body)))`
!!!(p) {:.unnumlist}

            `(cond`
!!!(p) {:.unnumlist}

               `((eq goal ’!) (cons ’!
(make-dcg-body (rest body) n)))`
!!!(p) {:.unnumlist}

               `((dcg-normal-goal-p goal)`
!!!(p) {:.unnumlist}

                 `(append (rest goal)`
!!!(p) {:.unnumlist}

                               `(make-dcg-body (rest body) n)))`
!!!(p) {:.unnumlist}

               `((dcg-word-list-p goal)`
!!!(p) {:.unnumlist}

                 `(cons`
!!!(p) {:.unnumlist}

                    `‘(= ,(symbol ’?s n)`
!!!(p) {:.unnumlist}

                          `(,@(rest goal) .,(symbol ’?s (+ n 1))))`
!!!(p) {:.unnumlist}

                   `(make-dcg-body (rest body) (+ n 1))))`
!!!(p) {:.unnumlist}

         `(t (cons`
!!!(p) {:.unnumlist}

               `(append goal`
!!!(p) {:.unnumlist}

                            `(list (symbol ’?s n)`
!!!(p) {:.unnumlist}

                                      `(symbol ’?s (+ n 1))))`
!!!(p) {:.unnumlist}

                `(make-dcg-body (rest body) (+ n 1))))))))`
!!!(p) {:.unnumlist}

**Exercise 20.1 [m]**`make-dcg` violates one of the cardinal rules of macros.
What does it do wrong?
How would you fix it?

## [ ](#){:#st0020}20.3 A Simple Grammar in DCG Format
{:#s0020}
{:.h1hd}

Here is the trivial grammar from [page 688](B9780080571157500200.xhtml#p688) in DCG format.

[ ](#){:#l0120}`(rule (S (?pred ?subj)) -->`
!!!(p) {:.unnumlist}

   `(NP ?agr ?subj)`
!!!(p) {:.unnumlist}

   `(VP ?agr ?pred))`
!!!(p) {:.unnumlist}

`(rule (NP ?agr (?det ?n)) -->`
!!!(p) {:.unnumlist}

   `(Det ?agr ?det)`
!!!(p) {:.unnumlist}

   `(N ?agr ?n))`
!!!(p) {:.unnumlist}

`(rule (NP 3sg (the male))          --> (:word he))`
!!!(p) {:.unnumlist}

`(rule (NP`˜`3sg (some objects))      --> (:word they))`
!!!(p) {:.unnumlist}

`(rule (VP 3sg sleep)               --> (:word sleeps))`
!!!(p) {:.unnumlist}

`(rule (VP`˜`3sg sleep)               --> (:word sleep))`
!!!(p) {:.unnumlist}

`(rule (Det ?any the)               --> (:word the))`
!!!(p) {:.unnumlist}

`(rule (N 3sg (young male human))   --> (:word boy))`
!!!(p) {:.unnumlist}

`(rule (N 3sg (young female human)) --> (:word girl))`
!!!(p) {:.unnumlist}

This grammar is quite limited, generating only four sentences.
The first way we will extend it is to allow verbs with objects: in addition to “The boy sleeps,” we will allow “The boy meets the girl.” To avoid generating ungrammatical sentences like “* The boy meets,”[1](#fn0015){:#xfn0015} we will separate the category of verb into two *subcategories*: transitive verbs, which take an object, and intransitive verbs, which don’t.

Transitive verbs complicate the semantic interpretation of sentences.
We would like the interpretation of “Terry kisses Jean” to be `(kiss Terry Jean)`.
The interpretation of the noun phrase “Terry” is just `Terry`, but then what should the interpretation of the verb phrase “kisses Jean” be?
To fit our predicate application model, it must be something equivalent to `(lambda (x) (kiss x Jean))`.
When applied to the subject, we want to get the simplification:

[ ](#){:#l0125}`((lambda (x) (kiss x Jean)) Terry)`⇒ `(kiss Terry Jean)`
!!!(p) {:.unnumlist}

Such simplification is not done automatically by Prolog, but we can write a predicate to do it.
We will call it `funcall`, because it is similar to the Lisp function of that name, although it only handles replacement of the argument, not full evaluation of the body.
(Technically, this is the lambda-calculus operation known as *beta-reduction.)* The predicate `funcall` is normally used with two input arguments, a function and its argument, and one output argument, the resulting reduction:

[ ](#){:#l0130}`(<− (funcall (lambda (?x) ?body) ?x ?body))`
!!!(p) {:.unnumlist}

With this we could write our rule for sentences as:

[ ](#){:#l0135}`(rule (S ?sem) -->`
!!!(p) {:.unnumlist}

   `(NP ?agr ?subj)`
!!!(p) {:.unnumlist}

   `(VP ?agr ?pred)`
!!!(p) {:.unnumlist}

   `(:test (funcall ?pred ?subj ?sem)))`
!!!(p) {:.unnumlist}

An alternative is to, in effect, compile away the call to `funcall`.
Instead of having the semantic representation of `VP` be a single lambda expression, we can represent it as two arguments: an input argument, `?subj`, which acts as a parameter to the output argument, `?pred`, which takes the place of the body of the lambda expression.
By explicitly manipulating the parameter and body, we can eliminate the call to `funcall`.
The trick is to make the parameter and the subject one and the same:

[ ](#){:#l0140}`(rule (S ?pred) -->`
!!!(p) {:.unnumlist}

   `(NP ?agr ?subj)`
!!!(p) {:.unnumlist}

   `(VP ?agr ?subj ?pred))`
!!!(p) {:.unnumlist}

One way of reading this rule is “To parse a sentence, parse a noun phrase followed by a verb phrase.
If they have different agreement features then fail, but otherwise insert the interpretation of the noun phrase, `?subj`, into the proper spot in the interpretation of the verb phrase, `?pred`, and return `?pred` as the final interpretation of the sentence.”

The next step is to write rules for verb phrases and verbs.
Transitive verbs are listed under the predicate `Verb/tr`, and intransitive verbs are listed as `Verb/intr`.
The semantics of tenses (past and present) has been ignored.

[ ](#){:#l0145}`(rule (VP ?agr ?subj ?pred) -->`
!!!(p) {:.unnumlist}

   `(Verb/tr ?agr ?subj ?pred ?obj)`
!!!(p) {:.unnumlist}

   `(NP ?any-agr ?obj))`
!!!(p) {:.unnumlist}

`(rule (VP ?agr ?subj ?pred) -->`
!!!(p) {:.unnumlist}

   `(Verb/intr ?agr ?subj ?pred))`
!!!(p) {:.unnumlist}

`(rule (Verb/tr`˜`3sg ?x (kiss ?x ?y) ?y) --> (:word kiss))`
!!!(p) {:.unnumlist}

`(rule (Verb/tr 3sg ?x (kiss ?x ?y) ?y) --> (:word kisses))`
!!!(p) {:.unnumlist}

`(rule (Verb/tr ?any ?x (kiss ?x ?y) ?y) --> (:word kissed))`
!!!(p) {:.unnumlist}

`(rule (Verb/intr`˜`3sg ?x (sleep ?x)) --> (:word sleep))`
!!!(p) {:.unnumlist}

`(rule (Verb/intr 3sg ?x (sleep ?x)) --> (:word sleeps))`
!!!(p) {:.unnumlist}

`(rule (Verb/intr ?any ?x (sleep ?x)) --> (:word slept))`
!!!(p) {:.unnumlist}

Here are the rules for noun phrases and nouns:

[ ](#){:#l0150}`(rule (NP ?agr ?sem) -->`
!!!(p) {:.unnumlist}

   `(Name ?agr ?sem))`
!!!(p) {:.unnumlist}

`(rule (NP ?agr (?det-sem ?noun-sem)) -->`
!!!(p) {:.unnumlist}

   `(Det ?agr ?det-sem)`
!!!(p) {:.unnumlist}

   `(Noun ?agr ?noun-sem))`
!!!(p) {:.unnumlist}

`(rule (Name 3sg Terry) --> (:word Terry))`
!!!(p) {:.unnumlist}

`(rule (Name 3sg Jean) --> (:word Jean))`
!!!(p) {:.unnumlist}

`(rule (Noun 3sg (young male human)) --> (:word boy))`
!!!(p) {:.unnumlist}

`(rule (Noun 3sg (young female human)) --> (:word girl))`
!!!(p) {:.unnumlist}

`(rule (Noun`˜`3sg (group (young male human))) --> (:word boys))`
!!!(p) {:.unnumlist}

`(rule (Noun`˜`3sg (group (young female human))) --> (:word girls))`
!!!(p) {:.unnumlist}

`(rule (Det ?any the) --> (:word the))`
!!!(p) {:.unnumlist}

`(rule (Det 3sg a) --> (:word a))`
!!!(p) {:.unnumlist}

This grammar and lexicon generates more sentences, although it is still rather limited.
Here are some examples:

[ ](#){:#l0155}`> (?- (S ?sem (The boys kiss a girl) ()))`
!!!(p) {:.unnumlist}

`?SEM = (KISS (THE (GROUP (YOUNG MALE HUMAN)))`
!!!(p) {:.unnumlist}

                       `(A (YOUNG FEMALE HUMAN))).`
!!!(p) {:.unnumlist}

`> (?- (S ?sem (The girls kissed the girls) ()))`
!!!(p) {:.unnumlist}

`?SEM = (KISS (THE (GROUP (YOUNG FEMALE HUMAN)))`
!!!(p) {:.unnumlist}

                       `(THE (GROUP (YOUNG FEMALE HUMAN)))).`
!!!(p) {:.unnumlist}

`> (?- (S ?sem (Terry kissed the girl) ()))`
!!!(p) {:.unnumlist}

`?SEM = (KISS TERRY (THE (YOUNG FEMALE HUMAN))).`
!!!(p) {:.unnumlist}

`> (?- (S ?sem (The girls kisses the boys) ()))`
!!!(p) {:.unnumlist}

`No.`
!!!(p) {:.unnumlist}

`> (?- (S ?sem (Terry kissed a girls) ()))`
!!!(p) {:.unnumlist}

`No.`
!!!(p) {:.unnumlist}

`> (?- (S ?sem (Terry sleeps Jean) ()))`
!!!(p) {:.unnumlist}

`No.`
!!!(p) {:.unnumlist}

The first three examples are parsed correctly, while the final three are correctly rejected.
The inquisitive reader may wonder just what is going on in the interpretation of a sentence like “The girls kissed the girls.” Do the subject and object represent the same group of girls, or different groups?
Does everyone kiss everyone, or are there fewer kissings going on?
Until we define our representation more carefully, there is no way to tell.
Indeed, it seems that there is a potential problem in the representation, in that the predicate `kiss` sometimes has individuals as its arguments, and sometimes groups.
More careful representations of “The girls kissed the girls” include the following candidates, using predicate calculus:

[ ](#){:#l0160}`∀x∀y x![f0005](images/B9780080571157500200/f0005.jpg) girls ∧ y![f0005](images/B9780080571157500200/f0005.jpg) girls`⇒ `kiss(x,y)`
!!!(p) {:.unnumlist}

`∀x∀y x![f0005](images/B9780080571157500200/f0005.jpg) girls ∧ yεgirls ∧ x≠y`⇒ `kiss(x,y)`
!!!(p) {:.unnumlist}

`∀x∃y,z x![f0005](images/B9780080571157500200/f0005.jpg) girls ∧ y![f0005](images/B9780080571157500200/f0005.jpg) girls ∧ z![f0005](images/B9780080571157500200/f0005.jpg) girls`⇒ `kiss(x,y) ∧ kiss(z,x)`
!!!(p) {:.unnumlist}

`∀x∃y x![f0005](images/B9780080571157500200/f0005.jpg) girls ∧ y![f0005](images/B9780080571157500200/f0005.jpg) girls`⇒ `kiss(x,y)`∨ `kiss(y,x)`
!!!(p) {:.unnumlist}

The first of these says that every girl kisses every other girl.
The second says the same thing, except that a girl need not kiss herself.
The third says that every girl kisses and is kissed by at least one other girl, but not necessarily all of them, and the fourth says that everbody is in on at least one kissing.
None of these interpretations says anything about who “the girls” are.

Clearly, the predicate calculus representations are less ambiguous than the representation produced by the current system.
On the other hand, it would be wrong to choose one of the representations arbitrarily, since in different contexts, “The girls kissed the girls” can mean different things.
Maintaining ambiguity in a concise form is useful, as long as there is some way eventually to recover the proper meaning.

## [ ](#){:#st0025}20.4 A DCG Grammar with Quantifiers
{:#s0025}
{:.h1hd}

The problem in the representation we have been using becomes more acute when we consider other determiners, such as “every.” Consider the sentence “Every picture paints a story.” The preceding DCG, if given the right vocabulary, would produce the interpretation:

[ ](#){:#l0165}`(paints (every picture) (a story))`
!!!(p) {:.unnumlist}

This can be considered ambiguous between the following two meanings, in predicate calculus form:

[ ](#){:#l0170}`∀ x picture(x)`⇒ `∃ y story(y) ∧ paint(x,y)`
!!!(p) {:.unnumlist}

`∃ y story (y) ∧ ∀ x picture(x)`⇒ `paint(x,y)`
!!!(p) {:.unnumlist}

The first says that for each picture, there is a story that it paints.
The second says that there is a certain special story that every picture paints.
The second is an unusual interpretation for this sentence, but for “Every U.S.
citizen has a president,” the second interpretation is perhaps the preferred one.
In the next section, we will see how to produce representations that can be transformed into either interpretation.
For now, it is a useful exercise to see how we could produce just the first representation above, the interpretation that is usually correct.
First, we need to transcribe it into Lisp:

[ ](#){:#l0175}`(all ?x (−> (picture ?x) (exists ?y (and (story ?y) (paint ?x ?y)))))`
!!!(p) {:.unnumlist}

The first question is how the `all` and `exists` forms get in there.
They must come from the determiners, “every” and “a.” Also, it seems that `all` is followed by an implication arrow, `->`, while `exists` is followed by a conjunction, `and`.
So the determiners will have translations looking like this:

[ ](#){:#l0180}`(rule (Det ?any ?x ?p ?q (the ?x (and ?p ?q)))   --> (:word the))`
!!!(p) {:.unnumlist}

`(rule (Det 3sg ?x ?p ?q (exists ?x (and ?p ?q))) --> (:word a))`
!!!(p) {:.unnumlist}

`(rule (Det 3sg ?x ?p ?q (all ?x (−> ?p ?q)))     --> (:word every))`
!!!(p) {:.unnumlist}

Once we have accepted these translations of the determiners, everything else follows.
The formulas representing the determiners have two holes in them, `?p` and `?q`.
The first will be filled by a predicate representing the noun, and the latter will be filled by the predicate that is being applied to the noun phrase as a whole.
Notice that a curious thing is happening.
Previously, translation to logical form was guided by the sentence’s verb.
Linguisticly, the verb expresses the main predicate, so it makes sense that the verb’s logical translation should be the main part of the sentence’s translation.
In linguistic terms, we say that the verb is the *head* of the sentence.

With the new translations for determiners, we are in effect turning the whole process upside down.
Now the subject’s determiner carries the weight of the whole sentence.
The determiner’s interpretation is a function of two arguments; it is applied to the noun first, yielding a function of one argument, which is in turn applied to the verb phrase’s interpretation.
This primacy of the determiner goes against intuition, but it leads directly to the right interpretation.

The variables `?p` and `?q` can be considered holes to be filled in the final interpretation, but the variable `?x` fills a quite different role.
At the end of the parse, `?x` will not be filled by anything; it will still be a variable.
But it will be referred to by the expressions filling `?p` and `?q`.
We say that `?x` is a *metavariable,* because it is a variable in the representation, not a variable in the Prolog implementation.
It just happens that Prolog variables can be used to implement these metavariables.

Here are the interpretations for each word in our target sentence and for each intermediate constituent:

[ ](#){:#l0185}`Every          = (all ?x (−> ?pl ?ql))`
!!!(p) {:.unnumlist}

`picture        = (picture ?x)`
!!!(p) {:.unnumlist}

`paints         = (paint ?x ?y)`
!!!(p) {:.unnumlist}

`a              = (exists ?y (and ?p2 ?q2))`
!!!(p) {:.unnumlist}

`story          = (story ?y)`
!!!(p) {:.unnumlist}

`Every picture  = (all ?x (−> (picture ?x) ?ql))`
!!!(p) {:.unnumlist}

`a story        = (exists ?y (and (story ?y) ?q2))`
!!!(p) {:.unnumlist}

`paints a story = (exists ?y (and (story ?y) (paint ?x ?y)))`
!!!(p) {:.unnumlist}

The semantics of a noun has to fill the `?p` hole of a determiner, possibly using the metavariable `?x`.
The three arguments to the Noun predicate are the agreement, the metavariable `?x`, and the assertion that the noun phrase makes about `?x`:

[ ](#){:#l0190}`(rule (Noun 3sg ?x (picture ?x)) --> (:word picture))`
!!!(p) {:.unnumlist}

`(rule (Noun 3sg ?x (story ?x)) --> (:word story))`
!!!(p) {:.unnumlist}

`(rule (Noun 3sg ?x (and (young ?x) (male ?x) (human ?x))) -->`
!!!(p) {:.unnumlist}

   `(:word boy))`
!!!(p) {:.unnumlist}

The NP predicate is changed to take four arguments.
First is the agreement, then the metavariable `?x`.
Third is a predicate that will be supplied externally, by the verb phrase.
The final argument returns the interpretation of the NP as a whole.
As we have stated, this comes from the determiner:

[ ](#){:#l0195}`(rule (NP ?agr ?x ?pred ?pred) -->`
!!!(p) {:.unnumlist}

   `(Name ?agr ?name))`
!!!(p) {:.unnumlist}

`;(rule (NP ?agr ?x ?pred ?np) -->`
!!!(p) {:.unnumlist}

`; (Det ?agr ?x ?noun ?pred ?np)`
!!!(p) {:.unnumlist}

`; (Noun ?agr ?x ?noun))`
!!!(p) {:.unnumlist}

The rule for an NP with determiner is commented out because it is convenient to introduce an extended rule to replace it at this point.
The new rule accounts for certain relative clauses, such as “the boy that paints a picture”:

[ ](#){:#l0200}`(rule (NP ?agr ?x ?pred ?np) -->`
!!!(p) {:.unnumlist}

   `(Det ?agr ?x ?noun&rel ?pred ?np)`
!!!(p) {:.unnumlist}

   `(Noun ?agr ?x ?noun)`
!!!(p) {:.unnumlist}

   `(rel-clause ?agr ?x ?noun ?noun&rel))`
!!!(p) {:.unnumlist}

`(rule (rel-clause ?agr ?x ?np ?np) --> )`
!!!(p) {:.unnumlist}

`(rule (rel-clause ?agr ?x ?np (and ?np ?rel)) -->`
!!!(p) {:.unnumlist}

   `(:word that)`
!!!(p) {:.unnumlist}

   `(VP ?agr ?x ?rel))`
!!!(p) {:.unnumlist}

The new rule does not account for relative clauses where the object is missing, such as “the picture that the boy paints.” Nevertheless, the addition of relative clauses means we can now generate an infinite language, since we can always introduce a relative clause, which introduces a new noun phrase, which in turn can introduce yet another relative clause.

The rules for relative clauses are not complicated, but they can be difficult to understand.
Of the four arguments to `rel-clause,` the first two hold the agreement features of the head noun and the metavariable representing the head noun.
The last two arguments are used together as an accumulator for predications about the metavariable: the third argument holds the predications made so far, and the fourth will hold the predications including the relative clause.
So, the first rule for `rel-clause` says that if there is no relative clause, then what goes in to the accumulator is the same as what goes out.
The second rule says that what goes out is the conjunction of what comes in and what is predicated in the relative clause itself.

Verbs apply to either one or two metavariables, just as they did before.
So we can use the definitions of `Verb/tr` and `Verb/intr` unchanged.
For variety, I’ve added a few more verbs:

[ ](#){:#l0205}`(rule (Verb/tr`˜`3sg ?x ?y (paint ?x ?y)) --> (:word paint))`
!!!(p) {:.unnumlist}

`(rule (Verb/tr 3sg ?x ?y (paint ?x ?y)) --> (:word paints))`
!!!(p) {:.unnumlist}

`(rule (Verb/tr ?any ?x ?y (paint ?x ?y)) --> (:word painted))`
!!!(p) {:.unnumlist}

`(rule (Verb/intr`˜`3sg ?x (sleep ?x)) --> (:word sleep))`
!!!(p) {:.unnumlist}

`(rule (Verb/intr 3sg ?x (sleep ?x)) --> (:word sleeps))`
!!!(p) {:.unnumlist}

`(rule (Verb/intr ?any ?x (sleep ?x)) --> (:word slept))`
!!!(p) {:.unnumlist}

`(rule (Verb/intr 3sg ?x (sells ?x)) --> (:word sells))`
!!!(p) {:.unnumlist}

`(rule (Verb/intr 3sg ?x (stinks ?x)) --> (:word stinks))`
!!!(p) {:.unnumlist}

Verb phrases and sentences are almost as before.
The only difference is in the call to `NP`, which now has extra arguments:

[ ](#){:#l0210}`(rule (VP ?agr ?x ?vp) -->`
!!!(p) {:.unnumlist}

   `(Verb/tr ?agr ?x ?obj ?verb)`
!!!(p) {:.unnumlist}

   `(NP ?any-agr ?obj ?verb ?vp))`
!!!(p) {:.unnumlist}

`(rule (VP ?agr ?x ?vp) -->`
!!!(p) {:.unnumlist}

   `(Verb/intr ?agr ?x ?vp))`
!!!(p) {:.unnumlist}

`(rule (S ?np) -->`
!!!(p) {:.unnumlist}

   `(NP ?agr ?x ?vp ?np)`
!!!(p) {:.unnumlist}

   `(VP ?agr ?x ?vp))`
!!!(p) {:.unnumlist}

With this grammar, we get the following correspondence between sentences and logical forms:

[ ](#){:#l0215}`Every picture paints a story.`
!!!(p) {:.unnumlist}

`(ALL ?3 (−> (PICTURE ?3)`
!!!(p) {:.unnumlist}

                     `(EXISTS ?14 (AND (STORY ?14) (PAINT ?3 ?14)))))`
!!!(p) {:.unnumlist}

`Every boy that paints a picture sleeps.`
!!!(p) {:.unnumlist}

`(ALL ?3 (−> (AND (AND (YOUNG ?3) (MALE ?3) (HUMAN ?3))`
!!!(p) {:.unnumlist}

                              `(EXISTS ?19 (AND (PICTURE ?19)`
!!!(p) {:.unnumlist}

                                                            `(PAINT ?3 ?19))))`
!!!(p) {:.unnumlist}

                  `(SLEEP ?3)))`
!!!(p) {:.unnumlist}

`Every boy that sleeps paints a picture.`
!!!(p) {:.unnumlist}

`(ALL ?3 (−> (AND (AND (YOUNG ?3) (MALE ?3) (HUMAN ?3))`
!!!(p) {:.unnumlist}

                                `(SLEEP ?3))`
!!!(p) {:.unnumlist}

                    `(EXISTS ?22 (AND (PICTURE ?22) (PAINT ?3 ?22)))))`
!!!(p) {:.unnumlist}

`Every boy that paints a picture that sells`
!!!(p) {:.unnumlist}

`paints a picture that stinks.`
!!!(p) {:.unnumlist}

`(ALL ?3 (−> (AND (AND (YOUNG ?3) (MALE ?3) (HUMAN ?3))`
!!!(p) {:.unnumlist}

                              `(EXISTS ?19 (AND (AND (PICTURE ?19) (SELLS ?19))`
!!!(p) {:.unnumlist}

                                                    `(PAINT ?3 ?19))))`
!!!(p) {:.unnumlist}

                    `(EXISTS ?39 (AND (AND (PICTURE ?39) (STINKS ?39))`
!!!(p) {:.unnumlist}

                                                  `(PAINT ?3 ?39)))))`
!!!(p) {:.unnumlist}

## [ ](#){:#st0030}20.5 Preserving Quantifier Scope Ambiguity
{:#s0030}
{:.h1hd}

Consider the simple sentence “Every man loves a woman.” This sentence is ambiguous between the following two interpretations:

[ ](#){:#l0220}`∀m∃w man(m) ∧ woman(w) ∧ loves(m,w)`
!!!(p) {:.unnumlist}

`∃w∀m man(m) ∧ woman(w) ∧ loves(m,w)`
!!!(p) {:.unnumlist}

The first interpretation is that every man loves some woman–his wife, perhaps.
The second interpretation is that there is a certain woman whom every man loves–Natassja Kinski, perhaps.
The meaning of the sentence is ambiguous, but the structure is not; there is only one syntactic parse.

In the last section, we presented a parser that would construct one of the two interpretations.
In this section, we show how to construct a single interpretation that preserves the ambiguity, but can be disambiguated by a postsyntactic process.
The basic idea is to construct an intermediate logical form that leaves the scope of quantifiers unspecified.
This intermediate form can then be rearranged to recover the final interpretation.

To recap, here is the interpretation we would get for “Every man loves a woman,” given the grammar in the previous section:

[ ](#){:#l0225}`(all ?m (−> (man ?m) (exists ?w) (and (woman ?w) (loves ?m ?w))))`
!!!(p) {:.unnumlist}

We will change the grammar to produce instead the intermediate form:

[ ](#){:#l0230}`(and (all ?m (man ?m))`
!!!(p) {:.unnumlist}

         `(exists ?w (wowan ?w))`
!!!(p) {:.unnumlist}

         `(loves ?m ?w))`
!!!(p) {:.unnumlist}

The difference is that logical components are produced in smaller chunks, with unscoped quantifiers.
The typical grammar rule will build up an interpretation by conjoining constituents with `and`, rather than by fitting pieces into holes in other pieces.
Here is the complete grammar and a just-large-enough lexicon in the new format:

[ ](#){:#l0235}`(rule (S (and ?np ?vp)) -->`
!!!(p) {:.unnumlist}

   `(NP ?agr ?x ?np)`
!!!(p) {:.unnumlist}

   `(VP ?agr ?x ?vp))`
!!!(p) {:.unnumlist}

`(rule (VP ?agr ?x (and ?verb ?obj)) -->`
!!!(p) {:.unnumlist}

   `(Verb/tr ?agr ?x ?o ?verb)`
!!!(p) {:.unnumlist}

   `(NP ?any-agr ?o ?obj))`
!!!(p) {:.unnumlist}

`(rule (VP ?agr ?x ?verb) -->`
!!!(p) {:.unnumlist}

   `(Verb/intr ?agr ?x ?verb))`
!!!(p) {:.unnumlist}

`(rule (NP ?agr ?name t) -->`
!!!(p) {:.unnumlist}

   `(Name ?agr ?name))`
!!!(p) {:.unnumlist}

`(rule (NP ?agr ?x ?det) -->`
!!!(p) {:.unnumlist}

   `(Det ?agr ?x (and ?noun ?rel) ?det)`
!!!(p) {:.unnumlist}

   `(Noun ?agr ?x ?noun)`
!!!(p) {:.unnumlist}

   `(rel-clause ?agr ?x ?rel))`
!!!(p) {:.unnumlist}

`(rule (rel-clause ?agr ?x t) --> )`
!!!(p) {:.unnumlist}

`(rule (rel-clause ?agr ?x ?rel) -->`
!!!(p) {:.unnumlist}

   `(:word that)`
!!!(p) {:.unnumlist}

   `(VP ?agr ?x ?rel))`
!!!(p) {:.unnumlist}

`(rule (Name 3sg Terry)                    --> (:word Terry))`
!!!(p) {:.unnumlist}

`(rule (Name 3sg Jean)                     --> (:word Jean))`
!!!(p) {:.unnumlist}

`(rule (Det 3sg ?x ?restr (all ?x ?restr)) --> (:word every))`
!!!(p) {:.unnumlist}

`(rule (Noun 3sg ?x (man ?x))              --> (:word man))`
!!!(p) {:.unnumlist}

`(rule (Verb/tr 3sg ?x ?y (love ?x ?y))    --> (:word loves))`
!!!(p) {:.unnumlist}

`(rule (Verb/intr 3sg ?x (lives ?x))       --> (:word lives))`
!!!(p) {:.unnumlist}

`(rule (Det 3sg ?x ?res (exists ?x ?res))  --> (:word a))`
!!!(p) {:.unnumlist}

`(rule (Noun 3sg ?x (woman ?x))            --> (:word woman))`
!!!(p) {:.unnumlist}

This gives us the following parse for “Every man loves a woman”:

[ ](#){:#l0240}`(and (all ?4 (and (man ?4) t))`
!!!(p) {:.unnumlist}

        `(and (love ?4 ?12) (exists ?12 (and (woman ?12) t))))`
!!!(p) {:.unnumlist}

If we simplified this, eliminating the ts and joining ands, we would get the desired representation:

[ ](#){:#l0245}`(and (all ?m (man ?m))`
!!!(p) {:.unnumlist}

        `(exists ?w (wowan ?w))`
!!!(p) {:.unnumlist}

        `(loves ?m ?w))`
!!!(p) {:.unnumlist}

From there, we could use what we know about syntax, in addition to what we know about men, woman, and loving, to determine the most likely final interpretation.
This will be covered in the next chapter.

## [ ](#){:#st0035}20.6 Long-Distance Dependencies
{:#s0035}
{:.h1hd}

So far, every syntactic phenomena we have considered has been expressible in a rule that imposes constraints only at a single level.
For example, we had to impose the constraint that a subject agree with its verb, but this constraint involved two immediate constituents of a sentence, the noun phrase and verb phrase.
We didn’t need to express a constraint between, say, the subject and a modifier of the verb’s object.
However, there are linguistic phenomena that require just these kinds of constraints.

Our rule for relative clauses was a very simple one: a relative clause consists of the word “that” followed by a sentence that is missing its subject, as in “every man that loves a woman.” Not all relative clauses follow this pattern.
It is also possible to form a relative clause by omitting the object of the embedded sentence: “every man that a woman loves ![f0010](images/B9780080571157500200/f0010.jpg) .
“ In this sentence, the symbol ![f0010](images/B9780080571157500200/f0010.jpg) indicates a gap, which is understood as being filled by the head of the complete noun phrase, the man.
This has been called a *filler-gap dependency.* It is also known as a *long-distance dependency,* because the gap can occur arbitrarily far from the filler.
For example, all of the following are valid noun phrases:

[ ](#){:#l9250}The person that Lee likes ![f0010](images/B9780080571157500200/f0010.jpg)
!!!(p) {:.unnumlist}

The person that Kim thinks Lee likes ![f0010](images/B9780080571157500200/f0010.jpg)
!!!(p) {:.unnumlist}

The person that Jan says Kim thinks Lee likes ![f0010](images/B9780080571157500200/f0010.jpg)
!!!(p) {:.unnumlist}

In each case, the gap is filled by the head noun, the person.
But any number of relative clauses can intervene between the head noun and the gap.

The same kind of filler-gap dependency takes place in questions that begin with “who,” “what,” “where,” and other interrogative pronouns.
For example, we can ask a question about the subject of a sentence, as in “Who likes Lee?”, or about the object, as in “Who does Kim like ![f0010](images/B9780080571157500200/f0010.jpg) ?”

Here is a grammar that covers relative clauses with gapped subjects or objects.
The rules for `S, VP,` and `NP` are augmented with a pair of arguments representing an accumulator for gaps.
Like a difference list, the first argument minus the second represents the presence or absence of a gap.
For example, in the first two rules for noun phrases, the two arguments are the same, `?g0` and `?g0`.
This means that the rule as a whole has no gap, since there can be no difference between the two arguments.
In the third rule for NP, the first argument is of the form `(gap …),` and the second is `nogap.` This means that the right-hand side of the rule, an empty constituent, can be parsed as a gap.
(Note that if we had been using true difference lists, the two arguments would be `((gap …) ?g0)` and `?g0`.
But since we are only dealing with one gap per rule, we don’t need true difference lists.)

The rule for `S` says that a noun phrase with gap `?g0` minus `?gl` followed by a verb phrase with gap `?gl` minus `?g2` comprise a sentence with gap `?g0` minus `?g2`.
The rule for relative clauses finds a sentence with a gap anywhere; either in the subject position or embedded somewhere in the verb phrase.
Here’s the complete grammar:

[ ](#){:#l0250}`(rule (S ?g0 ?g2 (and ?np ?vp)) -->`
!!!(p) {:.unnumlist}

   `(NP ?g0 ?gl ?agr ?x ?np)`
!!!(p) {:.unnumlist}

   `(VP ?gl ?g2 ?agr ?x ?vp))`
!!!(p) {:.unnumlist}

`(rule (VP ?g0 ?gl ?agr ?x (and ?obj ?verb)) -->`
!!!(p) {:.unnumlist}

   `(Verb/tr ?agr ?x ?o ?verb)`
!!!(p) {:.unnumlist}

   `(NP ?g0 ?gl ?any-agr ?o ?obj))`
!!!(p) {:.unnumlist}

`(rule (VP ?g0 ?g0 ?agr ?x ?verb) -->`
!!!(p) {:.unnumlist}

   `(Verb/intr ?agr ?x ?verb))`
!!!(p) {:.unnumlist}

`(rule (NP ?g0 ?g0 ?agr ?name t) -->`
!!!(p) {:.unnumlist}

   `(Name ?agr ?name))`
!!!(p) {:.unnumlist}

`(rule (NP ?g0 ?g0 ?agr ?x ?det) -->`
!!!(p) {:.unnumlist}

   `(Det ?agr ?x (and ?noun ?rel) ?det)`
!!!(p) {:.unnumlist}

   `(Noun ?agr ?x ?noun)`
!!!(p) {:.unnumlist}

   `(rel-clause ?agr ?x ?rel))`
!!!(p) {:.unnumlist}

`(rule (NP (gap NP ?agr ?x) nogap ?agr ?x t) --> )`
!!!(p) {:.unnumlist}

`(rule (rel-clause ?agr ?x t) --> )`
!!!(p) {:.unnumlist}

`(rule (rel-clause ?agr ?x ?rel) -->`
!!!(p) {:.unnumlist}

   `(:word that)`
!!!(p) {:.unnumlist}

   `(S (gap NP ?agr ?x) nogap ?rel))`
!!!(p) {:.unnumlist}

Here are some sentence/parse pairs covered by this grammar:

[ ](#){:#l0255}`Every man that`![f0010](images/B9780080571157500200/f0010.jpg) `loves a woman likes a person.`
!!!(p) {:.unnumlist}

`(AND (ALL ?28 (AND (MAN ?28)`
!!!(p) {:.unnumlist}

      `(AND T (AND (LOVE ?28 ?30)`
!!!(p) {:.unnumlist}

         `(EXISTS ?30 (AND (WOMAN ?30)`
!!!(p) {:.unnumlist}

               `T))))))`
!!!(p) {:.unnumlist}

   `(AND (EXISTS ?39 (AND (PERSON ?39) T)) (LIKE ?28 ?39)))`
!!!(p) {:.unnumlist}

`Every man that a woman loves`![f0010](images/B9780080571157500200/f0010.jpg) `likes a person.`
!!!(p) {:.unnumlist}

`(AND (ALL ?37 (AND (MAN ?37)`
!!!(p) {:.unnumlist}

      `(AND (EXISTS ?20 (AND (WOMAN ?20) T))`
!!!(p) {:.unnumlist}

        `(AND T (LOVE ?20 ?37)))))`
!!!(p) {:.unnumlist}

   `(AND (EXISTS ?39 (AND (PERSON ?39) T)) (LIKE ?37 ?39)))`
!!!(p) {:.unnumlist}

`Every man that loves a bird that`![f0010](images/B9780080571157500200/f0010.jpg) `flies likes a person.`
!!!(p) {:.unnumlist}

`(AND (ALL ?28 (AND (MAN ?28)`
!!!(p) {:.unnumlist}

      `(AND T (AND (EXISTS ?54`
!!!(p) {:.unnumlist}

         `(AND (BIRD ?54)`
!!!(p) {:.unnumlist}

             `(AND T (FLY ?54))))`
!!!(p) {:.unnumlist}

        `(LOVE ?28 ?54)))))`
!!!(p) {:.unnumlist}

   `(AND (EXISTS ?60 (AND (PERSON ?60) T)) (LIKE ?28 ?60)))`
!!!(p) {:.unnumlist}

Actually, there are limitations on the situations in which gaps can appear.
In particular, it is rare to have a gap in the subject of a sentence, except in the case of a relative clause.
In the next chapter, we will see how to impose additional constraints on gaps.

## [ ](#){:#st0040}20.7 Augmenting DCG Rules
{:#s0040}
{:.h1hd}

In the previous section, we saw how to build up a semantic representation of a sentence by conjoining the semantics of the components.
One problem with this approach is that the semantic interpretation is often something of the form `(and (and t *a) b),*` when we would prefer `(and *a b)*`.
There are two ways to correct this problem: either we add a step that takes the final semantic interpretation and simplifies it, or we complicate each individual rule, making it generate the simplified form.
The second choice would be slightly more efficient, but would be very ugly and error prone.
We should be doing all we can to make the rules simpler, not more complicated; that is the whole point of the DCG formalism.
This suggests a third approach: change the rule interpreter so that it automatically generates the semantic interpretation as a conjunction of the constituents, unless the rule explicitly says otherwise.
This section shows how to augment the DCG rules to handle common cases like this automatically.

Consider again a rule from [section 20.4](#s0025):

[ ](#){:#l0260}`(rule (S (and ?np ?vp)) -->`
!!!(p) {:.unnumlist}

   `(NP ?agr ?x ?np)`
!!!(p) {:.unnumlist}

   `(VP ?agr ?x ?vp))`
!!!(p) {:.unnumlist}

If we were to alter this rule to produce a simplified semantic interpretation, it would look like the following, where the predicate `and*` simplifies a list of conjunctions into a single conjunction:

[ ](#){:#l0265}`(rule (S ?sem) -->`
!!!(p) {:.unnumlist}

   `(np ?agr ?x ?np)`
!!!(p) {:.unnumlist}

   `(vp ?agr ?x ?vp)`
!!!(p) {:.unnumlist}

   `(:test (and*(?np ?vp) ?sem)))`
!!!(p) {:.unnumlist}

Many rules will have this form, so we adopt a simple convention: if the last argument of the constituent on the left-hand side of a rule is the keyword `:sem`, then we will build the semantics by replacing `:sem` with a conjunction formed by combining all the last arguments of the constituents on the right-hand side of the rule.
`A==>` arrow will be used for rules that follow this convention, so the following rule is equivalent to the one above:

[ ](#){:#l0270}`(rule (S :sem) ==>`
!!!(p) {:.unnumlist}

   `(NP ?agr ?x ?np)`
!!!(p) {:.unnumlist}

   `(VP ?agr ?x ?vp))`
!!!(p) {:.unnumlist}

It is sometimes useful to introduce additional semantics that does not come from one of the constituents.
This can be indicated with an element of the right-hand side that is a list starting with `:sem`.
For example, the following rule adds to the semantics the fact that `?x` is the topic of the sentence:

[ ](#){:#l0275}`(rule (S :sem) ==>`
!!!(p) {:.unnumlist}

   `(NP ?agr ?x ?np)`
!!!(p) {:.unnumlist}

   `(VP ?agr ?x ?vp)`
!!!(p) {:.unnumlist}

   `(:sem (topic ?x)))`
!!!(p) {:.unnumlist}

Before implementing the rule function for the `==>` arrow, it is worth considering if there are other ways we could make things easier for the rule writer.
One possibility is to provide a notation for describing examples.
Examples make it easier to understand what a rule is designed for.
For the `S` rule, we could add examples like this:

[ ](#){:#l0280}`(rule (S :sem) ==>`
!!!(p) {:.unnumlist}

   `(:ex “John likes Mary” “He sleeps”)`
!!!(p) {:.unnumlist}

   `(NP ?agr ?x ?np)`
!!!(p) {:.unnumlist}

   `(VP ?agr ?x ?vp))`
!!!(p) {:.unnumlist}

These examples not only serve as documentation for the rule but also can be stored under `S` and subsequently run when we want to test if `S` is in fact implemented properly.

Another area where the rule writer could use help is in handling left-recursive rules.
Consider the rule that says that a sentence can consist of two sentences joined by a conjunction:

[ ](#){:#l0285}`(rule (S (?conj ?sl ?s2)) ==>`
!!!(p) {:.unnumlist}

   `(:ex “John likes Mary and Mary likes John”)`
!!!(p) {:.unnumlist}

   `(S ?sl)`
!!!(p) {:.unnumlist}

   `(Conj ?conj)`
!!!(p) {:.unnumlist}

   `(S ?s2))`
!!!(p) {:.unnumlist}

While this rule is correct as a declarative statement, it will run into difficulty when run by the standard top-down depth-first DCG interpretation process.
The top-level goal of parsing an `S` will lead immediately to the subgoal of parsing an `S`, and the resuit will be an infinite loop.

Fortunately, we know how to avoid this kind of infinite loop: split the offending predicate, `S`, into two predicates: one that supports the recursion, and one that is at a lower level.
We will call the lower-level predicate `S-`.
Thus, the following rule says that a sentence can consist of two sentences, where the first one is not conjoined and the second is possibly conjoined:

[ ](#){:#l0290}`(rule (S (?conj ?sl ?s2)) ==>`
!!!(p) {:.unnumlist}

   `(S_ ?sl)`
!!!(p) {:.unnumlist}

   `(Conj ?conj)`
!!!(p) {:.unnumlist}

   `(S ?s2))`
!!!(p) {:.unnumlist}

We also need a rule that says that a possibly conjoined sentence can consist of a nonconjoined sentence:

[ ](#){:#l0295}`(rule (S ?sem) ==> (S- ?sem))`
!!!(p) {:.unnumlist}

To make this work, we need to replace any mention of `S` in the left-hand side of a rule with `S-`.
References to `S` in the right-hand side of rules remain unchanged.

[ ](#){:#l0300}`(rule (S- ?sem) ==>…)`
!!!(p) {:.unnumlist}

To make this all automatic, we will provide a macro, `conj-rule`, that declares a category to be one that can be conjoined.
Such a declaration will automatically generate the recursive and nonrecursive rules for the category, and will insure that future references to the category on the left-hand side of a rule will be replaced with the corresponding lower-level predicate.

One problem with this approach is that it imposes a right-branching parse on multiple conjoined phrases.
That is, we will get parses like “spaghetti and (meatballs and salad)” not “(spaghetti and meatballs) and salad.” Clearly, that is the wrong interpretation for this sentence.
Still, it can be argued that it is best to produce a single canonical parse, and then let the semantic interpretation functions worry about rearranging the parse in the right order.
We will not attempt to resolve this debate but will provide the automatic conjunction mechanism as a tool that can be convenient but has no cost for the user who prefers a different solution.

We are now ready to implement the extended DCG rule formalism that handles `:sem, :ex,` and automatie conjunctions.
The function `make-augmented-dcg,` stored under the arrow `==>`, will be used to implement the formalism:

[ ](#){:#l0305}`(setf (get ‘==>‘rule-function) ‘make-augmented-dcg)`
!!!(p) {:.unnumlist}

`(defun make-augmented-dcg (head body)`
!!!(p) {:.unnumlist}

   `“Build an augmented DCG rule that handles :sem, :ex,`
!!!(p) {:.unnumlist}

   `and automatie conjunctiontive constituents.”`
!!!(p) {:.unnumlist}

   `(if (eq (lastl head) :sem)`
!!!(p) {:.unnumlist}

           `;; Handle :sem`
!!!(p) {:.unnumlist}

           `(let* ((?sem (gensym “?SEM”)))`
!!!(p) {:.unnumlist}

             `(make-augmented-dcg`
!!!(p) {:.unnumlist}

               `‘(.@(butlast head) ,?sem)`
!!!(p) {:.unnumlist}

               `‘(,@(remove :sem body :key #’first-or-nil)`
!!!(p) {:.unnumlist}

                  `(:test ,(collect-sems body ?sem)))))`
!!!(p) {:.unnumlist}

`      ;; Separate out examples from body`
!!!(p) {:.unnumlist}

         `(multiple-value-bind (exs new-body)`
!!!(p) {:.unnumlist}

`         (partition-if #’(lambda (x) (starts-with x :ex)) body)`
!!!(p) {:.unnumlist}

        `;; Handle conjunctions`
!!!(p) {:.unnumlist}

`    (let ((rule ‘(rule ,(handle-conj head) --> ,@new-body)))`
!!!(p) {:.unnumlist}

         `(if (null exs) rule`
!!!(p) {:.unnumlist}

                `rule`
!!!(p) {:.unnumlist}

              `‘(progn (:ex ,head ..(mappend #’rest exs))`
!!!(p) {:.unnumlist}

                            `,rule))))))`
!!!(p) {:.unnumlist}

First we show the code that collects together the semantics of each constituent and conjoins them when `:sem` is specified.
The function `collect-sems` picks out the semantics and handles the trivial cases where there are zero or one constituents on the right-hand side.
If there are more than one, it inserts a call to the predicate `and*`.

[ ](#){:#l0310}`(defun collect-sems (body ?sem)`
!!!(p) {:.unnumlist}

   `“Get the semantics out of each constituent in body,`
!!!(p) {:.unnumlist}

   `and combine them together into ?sem.”`
!!!(p) {:.unnumlist}

   `(let ((sems (loop for goal in body`
!!!(p) {:.unnumlist}

     `unless (or (dcg-normal-goal-p goal)`
!!!(p) {:.unnumlist}

        `(dcg-word-list-p goal)`
!!!(p) {:.unnumlist}

        `(starts-with goal :ex)`
!!!(p) {:.unnumlist}

        `(atom goal))`
!!!(p) {:.unnumlist}

     `collect (lastl goal))))`
!!!(p) {:.unnumlist}

   `(case (length sems)`
!!!(p) {:.unnumlist}

    `(0 ‘(= ,?sem t))`
!!!(p) {:.unnumlist}

    `(1 ‘(= ,?sem .(first sems)))`
!!!(p) {:.unnumlist}

    `(t ‘(and* ,sems ,?sem)))))`
!!!(p) {:.unnumlist}

We could have implemented `and*` with Prolog clauses, but it is slightly more efficient to do it directly in Lisp.
A call to `conjuncts` collects all the conjuncts, and we then add an and if necessary:

[ ](#){:#l0315}`(defun and*/2 (in out cont)`
!!!(p) {:.unnumlist}

   `“IN is a list of conjuncts that are conjoined into OUT.”`
!!!(p) {:.unnumlist}

   `;; E.g.: (and* (t (and a b) t (and c d) t) ?x) ==>`
!!!(p) {:.unnumlist}

   `;; ?x = (and a b c d)`
!!!(p) {:.unnumlist}

   `(if (unify!
out (maybe-add ’and (conjuncts (cons ’and in)) t))`
!!!(p) {:.unnumlist}

          `(funcall cont)))`
!!!(p) {:.unnumlist}

`(defun conjuncts (exp)`
!!!(p) {:.unnumlist}

   `“Get all the conjuncts from an expression.”`
!!!(p) {:.unnumlist}

   `(deref exp)`
!!!(p) {:.unnumlist}

   `(cond ((eq exp t) nil)`
!!!(p) {:.unnumlist}

             `((atom exp) (list exp))`
!!!(p) {:.unnumlist}

              `((eq (deref (first exp)) ’nil) nil)`
!!!(p) {:.unnumlist}

              `((eq (first exp) ’and)`
!!!(p) {:.unnumlist}

               `(mappend #’conjuncts (rest exp)))`
!!!(p) {:.unnumlist}

              `(t (list exp))))`
!!!(p) {:.unnumlist}

The next step is handling example phrases.
The code in `make-augmented-dcg` turns examples into expressions of the form:

[ ](#){:#l0320}`(:ex (S ?sem) “John likes Mary” “He sleeps”)`
!!!(p) {:.unnumlist}

To make this work, :ex will have to be a macro:

[ ](#){:#l0325}`(defmacro :ex ((category .
args) &body examples)`
!!!(p) {:.unnumlist}

   `“Add some example phrases, indexed under the category.”`
!!!(p) {:.unnumlist}

   `’(add-examples ’.category ’.args ’.examples))`
!!!(p) {:.unnumlist}

`:ex calls add-examples` to do all the work.
Each example is stored in a hash table indexed under the the category.
Each example is transformed into a two-element list: the example phrase string itself and a call to the proper predicate with all arguments supplied.
The function `add-examples` does this transformation and indexing, and `run-examples` retrieves the examples stored under a category, prints each phrase, and calls each goal.
The auxiliary functions `get-examples` and `clear-exampl` es are provided to manipulate the example table, and `remove-punction, punctuation-p` and `string->list` are used to map from a string to a list of words.

[ ](#){:#l0330}`(defvar *examples* (make-hash-table :test #’eq))`
!!!(p) {:.unnumlist}

`(defun get-exampl es (category) (gethash category *examples*))`
!!!(p) {:.unnumlist}

`(defun clear-examples () (clrhash *examples*))`
!!!(p) {:.unnumlist}

`(defun add-examples (category args examples)`
!!!(p) {:.unnumlist}

   `“Add these example strings to this category,`
!!!(p) {:.unnumlist}

   `and when it cornes time to run them, use the args.”`
!!!(p) {:.unnumlist}

   `(dolist (example examples)`
!!!(p) {:.unnumlist}

     `(when (stringp example)`
!!!(p) {:.unnumlist}

        `(let ((ex ‘(,example`
!!!(p) {:.unnumlist}

                       `(,category ,@args`
!!!(p) {:.unnumlist}

                        `,(string->list`
!!!(p) {:.unnumlist}

                           `(remove-punctuation example)) ()))))`
!!!(p) {:.unnumlist}

      `(unless (member ex (get-examples category)`
!!!(p) {:.unnumlist}

                              `:test #’equal )`
!!!(p) {:.unnumlist}

       `(setf (gethash category *examples*)`
!!!(p) {:.unnumlist}

            `(nconc (get-examples category) (1ist ex))))))))`
!!!(p) {:.unnumlist}

`(defun run-examples (&optional category)`
!!!(p) {:.unnumlist}

   `“Run all the example phrases stored under a category.`
!!!(p) {:.unnumlist}

   `With no category, run ALL the examples.”`
!!!(p) {:.unnumlist}

   `(prolog-compi1e-symbols)`
!!!(p) {:.unnumlist}

   `(if (null category)`
!!!(p) {:.unnumlist}

      `(maphash #’(lambda (cat val)`
!!!(p) {:.unnumlist}

            `(declare (ignore val ))`
!!!(p) {:.unnumlist}

            `(format t “˜2&Examples of ˜a:˜&” cat)`
!!!(p) {:.unnumlist}

            `(run-examples cat))`
!!!(p) {:.unnumlist}

       `*examples*)`
!!!(p) {:.unnumlist}

   `(dolist (example (get-examples category))`
!!!(p) {:.unnumlist}

       `(format t “˜2&EXAMPLE: ˜{˜a˜&˜9T˜a˜}” example)`
!!!(p) {:.unnumlist}

       `(top-level-prove (cdr example)))))`
!!!(p) {:.unnumlist}

`(defun remove-punctuation (string)`
!!!(p) {:.unnumlist}

   `“Replace punctuation with spaces in string.”`
!!!(p) {:.unnumlist}

   `(substitute-if #\space #’punctuation-p string))`
!!!(p) {:.unnumlist}

`(defun string->list (string)`
!!!(p) {:.unnumlist}

   `“Convert a string to a list of words.”`
!!!(p) {:.unnumlist}

   `(read-from-string(concatenate ’string “(“string “)”)))`
!!!(p) {:.unnumlist}

`(defun punctuation-p (char) (find char “*_..,;:’!?#-()\\\”“))`
!!!(p) {:.unnumlist}

The final part of our augmented DCG formalism is handling conjunctive constituents automatically.
We already arranged to translate category symbols on the left-hand side of rules into the corresponding conjunctive category, as specified by the function `handle-conj`.
We also want to generate automatically (or as easily as possible) rules of the following form:

[ ](#){:#l0335}`(rule (S (?conj ?sl ?s2)) ==>`
!!!(p) {:.unnumlist}

   `(S_ ?sl)`
!!!(p) {:.unnumlist}

   `(Conj ?conj)`
!!!(p) {:.unnumlist}

   `(S ?s2))`
!!!(p) {:.unnumlist}

`(rule (S ?sem) ==> (S_ ?sem))`
!!!(p) {:.unnumlist}

But before we generate these rules, let’s make sure they are exactly what we want.
Consider parsing a nonconjoined sentence with these two rules in place.
The first rule would parse the entire sentence as a `S_`, and would then fail to see a `Conj`, and thus fail.
The second rule would then duplicate the entire parsing process, thus doubling the amount of time taken.
If we changed the order of the two rules we would be able to parse nonconjoined sentences quickly, but would have to backtrack on conjoined sentences.

The following shows a better approach.
A single rule for `S` parses a sentence with `S_`, and then calls `Conj_S`, which can be read as “either a conjunction followed by a sentence, or nothing.” If the first sentence is followed by nothing, then we just use the semantics of the first sentence; if there is a conjunction, we have to form a combined semantics.
I have added … to show where arguments to the predicate other than the semantic argument fit in.

[ ](#){:#l0340}`(rule (S … ?s-combined) ==>`
!!!(p) {:.unnumlist}

   `(S_ … ?seml)`
!!!(p) {:.unnumlist}

   `(Conj_S ?seml ?s-combined))`
!!!(p) {:.unnumlist}

`(rule (Conj_S ?seml (?conj ?seml ?sem2)) ==>`
!!!(p) {:.unnumlist}

   `(Conj ?conj)`
!!!(p) {:.unnumlist}

   `(S … ?sem2))`
!!!(p) {:.unnumlist}

`(rule (Conj_S ?seml ?seml) ==>)`
!!!(p) {:.unnumlist}

Now all we need is a way for the user to specify that these three rules are desired.
Since the exact method of building up the combined semantics and perhaps even the call to `Conj` may vary depending on the specifics of the grammar being defined, the rules cannot be generated entirely automatically.
We will settle for a macro, `conj-rule`, that looks very much like the second of the three rules above but expands into all three, plus code to relate `S_` to `S`.
So the user will type:

[ ](#){:#l0345}`(conj-rule (Conj_S ?seml (?conj ?seml ?sem2)) ==>`
!!!(p) {:.unnumlist}

   `(Conj ?conj)`
!!!(p) {:.unnumlist}

   `(S ?a ?b ?c ?sem2))`
!!!(p) {:.unnumlist}

Here is the macro definition:

[ ](#){:#l0350}`(defmacro conj-rule ((conj-cat sem1 combined-sem) ==>`
!!!(p) {:.unnumlist}

          `conj (cat .
args))`
!!!(p) {:.unnumlist}

   `“Define this category as an automatic conjunction.”`
!!!(p) {:.unnumlist}

   `‘(progn`
!!!(p) {:.unnumlist}

   `(setf (get ’,cat ’conj-cat) ’,(symbol cat ’_))`
!!!(p) {:.unnumlist}

   `(rule (.cat ,@(butlast args) ?combined-sem) ==>`
!!!(p) {:.unnumlist}

      `(,(symbol cat ’_) ,@(butlast args) .sem1)`
!!!(p) {:.unnumlist}

      `(,conj-cat ,seml ?combined-sem))`
!!!(p) {:.unnumlist}

   `(rule (,conj-cat ,sem1 ,combined-sem) ==>`
!!!(p) {:.unnumlist}

      `,conj`
!!!(p) {:.unnumlist}

     `(,cat .@args))`
!!!(p) {:.unnumlist}

   `(rule (,conj-cat ?seml ?seml) ==>)))`
!!!(p) {:.unnumlist}

and here we define `handle-conj` to substitute `S-` for `S` in the left-hand side of rules:

[ ](#){:#l0355}`(defun handle-conj (head)`
!!!(p) {:.unnumlist}

   `“Replace (Cat …) with (Cat-…) if Cat is declared`
!!!(p) {:.unnumlist}

   `as a conjunctive category.”`
!!!(p) {:.unnumlist}

   `(if (and (listp head) (conj-category (predicate head)))`
!!!(p) {:.unnumlist}

   `(cons (conj-category (predicate head)) (args head))`
!!!(p) {:.unnumlist}

   `head))`
!!!(p) {:.unnumlist}

`(defun conj-category (predicate)`
!!!(p) {:.unnumlist}

   `“If this is a conjunctive predicate, return the Cat- symbol.”`
!!!(p) {:.unnumlist}

   `(get predicate ‘conj-category))`
!!!(p) {:.unnumlist}

## [ ](#){:#st0045}20.8 History and References
{:#s0045}
{:.h1hd}

As we have mentioned, Alain Colmerauer invented Prolog to use in his grammar of French (1973).
His *metamorphosis grammar* formalism was more expressive but much less efficient than the standard DCG formalism.

The grammar in [section 20.4](#s0025) is essentially the same as the one presented in Fernando Pereira and David H.
D.
Warren’s 1980 paper, which introduced the Definite Clause Grammar formalism as it is known today.
The two developed a much more substantial grammar and used it in a very influential question-answering system called Chat-80 ([Warren and Pereira, 1982](B9780080571157500285.xhtml#bb1340)).
Pereira later teamed with Stuart Shieber on an excellent book covering logic grammars in more depth: *Prolog and Natural-Language Analysis* (1987).
The book has many strong points, but unfortunately it does not present a grammar anywhere near as complete as the Chat-80 grammar.

The idea of a compositional semantics based on mathematical logic owes much to the work of the late linguist Richard Montague.
The introduction by [Dowty, Wall, and Peters (1981)](B9780080571157500285.xhtml#bb0335) and the collection by [Rich Thomason (1974)](B9780080571157500285.xhtml#bb1235) cover Montague’s approach.

The grammar in [section 20.5](#s0030) is based loosely on Michael McCord’s modular logic grammar, as presented in [Walker et al.
1990](B9780080571157500285.xhtml#bb1295).

It should be noted that logic grammars are by no means the only approach to natural language processing.
[Woods (1970)](B9780080571157500285.xhtml#bb1425) presents an approach based on the *augmented transition network*, or ATN.
A transition network is like a context-free grammar.
The *augmentation* is a way of manipulating features and semantic values.
This is just like the extra arguments in DCGs, except that the basic operations are setting and testing variables rather than unification.
So the choice between ATNs and DCGs is largely a matter of what programming approach you are most comfortable with: procedural for ATNs and declarative for DCGs.
My feeling is that unification is a more suitable primitive than assignment, so I chose to present DCGs, even though this required bringing in Prolog’s backtracking and unification mechanisms.

In either approach, the same linguistic problems must be addressed–agreement, long-distance dependencies, topicalization, quantifier-scope ambiguity, and so on.
Comparing [Woods’s (1970)](B9780080571157500285.xhtml#bb1425) ATN grammar to [Pereira and Warren’s (1980)](B9780080571157500285.xhtml#bb0950) DCG grammar, the careful reader will see that the solutions have much in common.
The analysis is more important than the notation, as it should be.

## [ ](#){:#st0050}20.9 Exercises
{:#s0050}
{:.h1hd}

**Exercise 20.2 [m]** Modify the grammar (from [section 20.4](#s0025), [20.5](#s0030), [or 20.6](#s0035)) to allow for adjectives before a noun.

**Exercise 20.3 [m]** Modify the grammar to allow for prepositional phrase modifiers on verb and noun phrases.

**Exercise 20.4 [m]** Modify the grammar to allow for ditransitive verbs–verbs that take two objects, as in “give the dog a bone.”

**Exercise 20.5** Suppose we wanted to adopt the Prolog convention of writing DCG tests and words in brackets and braces, respectively.
Write a function that will alter the readtable to work this way.

**Exercise 20.6 [m]** Define a rule function for a new type of DCG rule that automatically builds up a syntactic parse of the input.
For example, the two rules:

[ ](#){:#l0360}`(rule (s) => (np) (vp))`
!!!(p) {:.unnumlist}

`(rule (np) => (:word he))`
!!!(p) {:.unnumlist}

should be equivalent to:

[ ](#){:#l0365}`(rule (s (s ?1 ?2)) --> (np ?1) (vp ?2))`
!!!(p) {:.unnumlist}

`(rule (np (np he)) --> (:word he))`
!!!(p) {:.unnumlist}

**Exercise 20.7 [m]** There are advantages and disadvantages to the approach that Prolog takes in dividing predicates into clauses.
The advantage is that it is easy to add a new clause.
The disadvantage is that it is hard to alter an existing clause.
If you edit a clause and then evaluate it, the new clause will be added to the end of the clause list, when what you really wanted was for the new clause to take the place of the old one.
To achieve that effect, you have to call `clear-predicate`, and then reload all the clauses, not just the one that has been changed.

Write a macro `named-rule` that is just like `rule`, except that it attaches names to clauses.
When a named rule is reloaded, it replaces the old clause rather than adding a new one.

**Exercise 20.8 [h]** Extend the DCG rule function to allow or goals in the right-hand side.
To make this more useful, also allow `and` goals.
For example:

[ ](#){:#l0370}`(rule (A) --> (B) (or (C) (and (D) (E))) (F))`
!!!(p) {:.unnumlist}

should compile into the equivalent of :

[ ](#){:#l0375}`(<− (A ?S0 ?S4)`
!!!(p) {:.unnumlist}

   `(B ?S0 ?S1)`
!!!(p) {:.unnumlist}

   `(OR (AND (C ?S1 ?S2) (= ?S2 ?S3))`
!!!(p) {:.unnumlist}

  `(AND (D ?S1 ?S2) (E ?S2 ?S3)))`
!!!(p) {:.unnumlist}

   `(F ?S3 ?S4))`
!!!(p) {:.unnumlist}

## [ ](#){:#st0055}20.10 Answers
{:#s0055}
{:.h1hd}

**Answer 20.1** It uses local variables `(?s0, ?sl …)` that are not guaranteed to be unique.
This is a problem if the grammar writer wants to use these symbols anywhere in his or her rules.
The fix is to `gensym` symbols that are guaranteed to be unique.

### [ ](#){:#st0060}Answer 20.5[ ](#){:#p3145}
{:#s0060}
{:.h2hd}

[ ](#){:#l0380}`(defun setup-braces Uoptional (on?
t) (readtable *readtable*))`
!!!(p) {:.unnumlist}

   `“Make [a b] read as (:word a b) and {a b} as (:test a b c) if ON?
is true; otherwise revert {[]} to normal.”`
!!!(p) {:.unnumlist}

   `if ON?
is true; otherwise revert {[]} to normal.”`
!!!(p) {:.unnumlist}

   `(if (not on?)`
!!!(p) {:.unnumlist}

   `(map nil #’(lambda (c)`
!!!(p) {:.unnumlist}

       `(set-macro-character c (get-macro-character #\a)`
!!!(p) {:.unnumlist}

            `t readtable))`
!!!(p) {:.unnumlist}

    `“{[]}”)`
!!!(p) {:.unnumlist}

   `(progn`
!!!(p) {:.unnumlist}

    `(set-macro-character`
!!!(p) {:.unnumlist}

     `#\] (get-macro-character #\)) nil readtable)`
!!!(p) {:.unnumlist}

    `(set-macro-character`
!!!(p) {:.unnumlist}

     `#\} (get-macro-character #\)) nil readtable)`
!!!(p) {:.unnumlist}

    `(set-macro-character`
!!!(p) {:.unnumlist}

     `#\[ #’(lambda (s ignore)`
!!!(p) {:.unnumlist}

         `(cons :word (read-delimited-1ist #\] s t)))`
!!!(p) {:.unnumlist}

     `nil readtable)`
!!!(p) {:.unnumlist}

    `(set-macro-character`
!!!(p) {:.unnumlist}

     `#\{ #’(lambda (s ignore)`
!!!(p) {:.unnumlist}

         `(cons :test (read-delimited-1ist #\} s t)))`
!!!(p) {:.unnumlist}

     `nil readtable))))`
!!!(p) {:.unnumlist}

----------------------

[1](#xfn0015){:#np0015} The asterisk at the start of a sentence is the standard linguistic notation for an utterance that is ungrammatical or otherwise ill-formed.
!!!(p) {:.ftnote1}

