# Chapter 19 {docsify-ignore}
<a id='page-655'></a>

Introduction to 
Natural Language 

Language is everywhere. It permeates our thoughts, 
mediates our relations with others, and even creeps 
into our dreams. The overwhelming hulk of human 
knowledge is stored and transmitted in language. 
Language is so ubiquitous that we take itfor granted, 
but without it, society as we know it would 
be impossible. 

- Ronand Langacker 

Language and its Structure (1967) 

A
A
natural language is a language spoken by people, such as English, German, or Tagalog. 
This is in opposition to artificial languages like Lisp, FORTRAN, or Morse code. 
Natural language processing is an important part of AI because language is intimately 
connected to thought. One measure of this is the number of important books that mention 
language and thought in the title: in AI, Schank and Colby's Computer Models of Thought 
and Language; in linguistics, Whorf's Language, Thought, and Reality (and Chomsky's Language 
and Mind;) in philosophy, Fodor's The Language of Thought; and in psychology, Vygotsky's 
Thought and Language and John Anderson's Language, Memory, and Thought. Indeed, language is 

<a id='page-656'></a>

the trait many think of as being the most characteristic of humans. Much controversy 
has been generated over the question of whether animals, especially primates and 
dolphins, can use and "understand" language. Similar controversy surrounds the 
same question asked of computers. 

The study of language has been traditionally separated into two broad classes: 
syntax, or grammar, and semantics, or meaning. Historically, syntax has achieved 
the most attention, largely because on the surface it is more amenable to formal and 
semiformal methods. Although there is evidence that the boundary between the two 
is at best fuzzy, we still maintain the distinction for the purposes of these notes. We 
will cover the "easier" part, syntax, first, and then move on to semantics. 

A good artificial language, like Lisp or C, is unambiguous. There is only one 
interpretation for a valid Lisp expression. Of course, the interpretation may depend 
on the state of the current state of the Lisp world, such as the value of global variables. 
But these dependencies can be explicitly enumerated, and once they are spelled out, 
then there can only be one meaning for the expression,^ 

Natural language does not work like this. Natural expressions are inherently 
ambiguous, depending on any number of factors that can never be quite spelled out 
completely. It is perfectly reasonable for two people to disagree on what some other 
person meant by a natural language expression. (Lawyers and judges make their 
living largely by interpreting natural language expressions - laws - that are meant to 
be unambiguous but are not.) 

This chapter is a brief introduction to natural language processing. The next 
chapter gives a more thorough treatment from the point of view of logic grammars, 
and the chapter after that puts it all together into a full-fledged system. 

19-1 Parsing with a Phrase-Structure Grammar 

To parse a sentence means to recover the constituent structure of the sentence - to 
discover what sequence of generation rules could have been applied to come up with 
the sentence. In general, there may be several possible derivations, in which case 
we say the sentence is grammatically ambiguous. In certain circles, the term "parse" 
means to arrive at an understanding of a sentence's meaning, not just its grammatical 
form. We will attack that more difficult question later. 

^Some erroneous expressions are underspecified and may return different results in different 
implementations, but we will ignore that problem. 

<a id='page-657'></a>

We start with the grammar defined on [page 39](chapter2.md#page-39) for the generate program: 

(defvar ^grammar* "The grammar used by GENERATE.") 

(defparameter *grammarl* 

'((Sentence -> (NP VP)) 
(NP -> (Art Noun)) 
(VP -> (Verb NP)) 
(Art -> the a) 
(Noun -> man ball woman table) 
(Verb -> hit took saw liked))) 
Our parser takes as input a list of words and returns a structure containing the parse 
tree and the unparsed words, if any. That way, we can parse the remaining words 
under the next category to get compound rules. For example, in parsing "the man 
saw the table," we would first parse "the man," returning a structure representing 
the noun phrase, with the remaining words "saw the table." This remainder would 
then be parsed as a verb phrase, returning no remainder, and the two phrases could 
then be joined to form a parse that is a complete sentence with no remainder. 

Before proceeding, I want to make a change in the representation of grammar 
rules. Currently, rules have a left-hand side and a list of alternative right-hand sides. 
But each of these alternatives is really a separate rule, so it would be more modular 
to write them separately. For the generate program it was fine to have them all together, 
because that made processing choices easier, but now I want a more flexible 
representation. Later on we will want to add more information to each rule, like the 
semantics of the assembled left-hand side, and constraints between constituents on 
the right-hand side, so the rules would become quite large indeed if we didn't split up 
the alternatives. I also take this opportunity to clear up the confusion between words 
and category symbols. The convention is that a right-hand side can be either an 
atom, in which case it is a word, or a list of symbols, which are then all interpreted as 
categories. To emphasize this, I include "noun" and "verb" as nouns in the grammar 
*grammar3*, which is otherwise equivalent to the previous *grammarl*. 

(defparameter *grammar3* 

'((Sentence -> (NP VP)) 

(NP -> (Art Noun)) 

(VP -> (Verb NP)) 

(Art -> the) (Art -> a) 

(Noun -> man) (Noun -> ball) (Noun -> woman) (Noun -> table) 

(Noun -> noun) (Noun -> verb) 

(Verb -> hit) (Verb -> took) (Verb -> saw) (Verb -> liked))) 

(setf *grammar* *grammar3*) 

I also define the data types rul e, parse, and tree, and some functions for getting 

<a id='page-658'></a>

at the rules. Rules are defined as structures of type list with three slots: the left-
hand side, the arrow (which should always be represented as the literal ->) and the 
right-hand side. Compare this to the treatment on [page 40](chapter2.md#page-40). 

(defstruct (rule (:type list)) Ihs -> rhs) 

(defstruct (parse) "A parse tree and a remainder." tree rem) 

;; Trees are of the form: (Ihs . rhs) 
(defun new-tree (cat rhs) (cons cat rhs)) 
(defun tree-lhs (tree) (first tree)) 
(defun tree-rhs (tree) (rest tree)) 

(defun parse-lhs (parse) (tree-lhs (parse-tree parse))) 

(defun lexical-rules (word) 
"Return a list of rules with word on the right-hand side." 
(find-all word ^grammar* :key #'rule-rhs :test #'equal)) 

(defun rules-starting-with (cat) 
"Return a list of rules where cat starts the rhs." 
(find-all cat *grammar* 

:key #'(lambda (rule) (first-or-nil (rule-rhs rule))))) 

(defun first-or-nil (x) 
"The first element of . if it is a list; else nil." 
(if (consp X) (first x) nil)) 

Now we're ready to define the parser. The main function parser takes a list of 
words to parse. It calls parse, which returns a Ust of all parses that parse some 
subsequence of the words, starting at the beginning, parser keeps only the parses 
with no remainder - that is, the parses that span all the words. 

(defun parser (words) 
"Return all complete parses of a list of words." 
(mapcar #'parse-tree (complete-parses (parse words)))) 

(defun complete-parses (parses) 
"Those parses that are complete (have no remainder)." 
(find-all-if #*null parses :key #*parse-rem)) 

The function parse looks at the first word and considers each category it could be. It 
makes a parse of the first word under each category, and calls extend - pa rse to try to 
continue to a complete parse, pa rse uses mapcan to append together all the resulting 
parses. As an example, suppose we are trying to parse "the man took the ball." pa rse 
would find the single lexical rule for "the" and call extend-pa rse with a parse with 
tree (Art t he) and remainder "man took the ball," with no more categories needed. 

<a id='page-659'></a>
extend-parse has two cases. If the partial parse needs no more categories to be 
complete, then it returns the parse itself, along with any parses that can be formed 
by extending parses starting with the partial parse. In our example, there is one rule 
startingwith Art, namely (NP -> (Art Noun)), so the function would try to extend 
theparse tree (NP (Art the)) with remainder "man took the ball," with the category 
Noun needed. That call to extend-parse represents the second case. We first parse 
"man took the ball," and for every parse that is of category Noun (there will be only 
one), we combine with the partial parse. In this case we get (NP (Art the) (Noun 
man)). This gets extended as a sentence with a VP needed, and eventually we get a 
parse of the complete hst of words. 

(defun parse (words) 
"Bottom-up parse, returning all parses of any prefix of words." 
(unless (null words) 

(mapcan #'(lambda (rule) 
(extend-parse (rule-lhs rule) (list (first words)) 
(rest words) nil)) 
(lexical-rules (first words))))) 

(defun extend-parse (Ihs rhs rem needed) 
"Look for the categories needed to complete the parse." 
(if (null needed) 

If nothing needed, return parse and upward extensions 
(let ((parse (make-parse :tree (new-tree Ihs rhs) :rem rem))) 
(cons parse 
(mapcan 
#.(lambda (rule) 

(extend-parse (rule-lhs rule) 
(list (parse-tree parse)) 
rem (rest (rule-rhs rule)))) 

(rules-starting-with Ihs)))) 
otherwise try to extend rightward 
(mapcan 
#'(lambda (p) 
(if (eq (parse-lhs p) (first needed)) 
(extend-parse Ihs (appendl rhs (parse-tree p)) 
(parse-rem p) (rest needed)))) 

(parse rem)))) 

This makes use of the auxiliary function appendl: 
(defun appendl (items item) 
"Add item to end of list of items." 
(append items (list item))) 

<a id='page-660'></a>

Some examples of the parser in action are shown here: 

> (parser '(the table)) 
((NP (ART THE) (NOUN TABLE))) 

> (parser '(the ball hit the table)) 

((SENTENCE (NP (ART THE) (NOUN BALD) 

(VP (VERB HIT) 

(NP (ARTTHE) (NOUN TABLE))))) 

> (parser '(the noun took the verb)) 
((SENTENCE (NP (ART THE) (NOUN NOUN)) 
(VP (VERB TOOK) 
(NP (ARTTHE) (NOUN VERB))))) 

19.2 Extending the Grammar and 
Recognizing Ambiguity 
Overall, the parser seems to work fine, but the range of sentences we can parse is 
quite limited with the current grammar. The following grammar includes a wider 
variety of linguistic phenomena: adjectives, prepositional phrases, pronouns, and 
proper names. It also uses the usual linguistic conventions for category names, 
summarized in the table below: 

Category Examples 
Sentence John likes Mary

s 

NP Noun Phrase John; a blue table 
VP Verb Phrase likes Mary; hit the ball 
PP Prepositional Phrase to Mary; with the man 

A Adjective little; blue 

A+ A list of one or more adjectives little blue 
D Determiner the; a 
. Noun ball; table 
Name Proper Name John; Mary 
. Preposition to; with 
Pro Pronoun you; me 
V Verb liked; hit 

<a id='page-661'></a>

Here is the grammar: 

(defparameter *grammar4* 

'((S -> (NP VP)) 
(NP -> (D N)) 
(NP -> (D A+ N)) 
(NP -> (NP PP)) 
(NP -> (Pro)) 
(NP -> (Name)) 
(VP -> (V NP)) 
(VP -> (V)) 
(VP -> (VP PP)) 
(PP -> (P NP)) 
(A+ -> (A)) 
(A+ -> (A A+)) 
(Pro -> I) (Pro -> you) (Pro -> he) (Pro -> she) 
(Pro -> it) (Pro -> me) (Pro -> him) (Pro -> her) 
(Name -> John) (Name -> Mary) 
(A -> big) (A -> little) (A -> old) (A -> young) 
(A -> blue) (A -> green) (A -> orange) (A -> perspicuous) 
(D -> the) (D -> a) (D -> an) 
(N -> man) (N -> ball) (N -> woman) (N -> table) (N -> orange) 
(N -> saw) (N -> saws) (N -> noun) (N -> verb) 
(P -> with) (P -> for) (P -> at) (P -> on) (P -> by) (P -> of) (P -> in) 
(V -> hit) (V -> took) (V -> saw) (V -> liked) (V -> saws))) 
(setf ^grammar* *grammar4*) 

Now we can parse more interesting sentences, and we can see a phenomenon that 
was not present in the previous examples: ambiguous sentences. The sentence "The 
man hit the table with the ball" has two parses, one where the ball is the thing that 
hits the table, and the other where the ball is on or near the table, parser finds both 
of these parses (although of course it assigns no meaning to either parse): 

> (parser '(The man hit the table with the ball)) 
((S (NP (D THE) (N MAN)) 
(VP (VP (V HIT) (NP (D THE) (N TABLE))) 
(PP (P WITH) (NP (DTHE) (N BALL))))) 
(S (NP (D THE) (N MAN)) 
(VP (V HIT) 
(NP (NP (D THE) (N TABLE)) 
(PP (P WITH) (NP (DTHE) (N BALL))))))) 

Sentences are not the only category that can be ambiguous, and not all ambiguities 
have to be between parses in the same category. Here we see a phrase that is 
ambiguous between a sentence and a noun phrase: 

<a id='page-662'></a>

> (parser '(the orange saw)) 
((S (NP (D THE) (N ORANGE)) (VP (V SAW))) 
(NP (D THE) (A+ (A ORANGE)) (N SAW))) 

19.3 More Efficient Parsing 
With more complex grammars and longer sentences, the parser starts to slow down. 
The main problem is that it keeps repeating work. For example, in parsing "The 
man hit the table with the ball," it has to reparse "with the ball" for both of the 
resulting parses, even though in both cases it receives the same analysis, a PP. We 
have seen this problem before and have already produced an answer: memoization 
(see section 9.6). To see how much memoization will help, we need a benchmark: 

> (setf s (generate 's)) 
(THE PERSPICUOUS BIG GREEN BALL BY A BLUE WOMAN WITH A BIG MAN 
HIT A TABLE BY THE SAW BY THE GREEN ORANGE) 

> (time (length (parser s))) 
Evaluation of (LENGTH (PARSER S)) took 33.11 Seconds of elapsed time. 
10 

The sentence S has 10 parses, since there are two ways to parse the subject NP and 
five ways to parse the VP. It took 33 seconds to discover these 10 parses with the 
pa rse function as it was written. 

We can improve this dramatically by memoizing parse (along with the table-
lookup functions). Besides memoizing, the only change is to clear the memoization 
table within parser. 

(memoize 'lexical-rules) 
(memoize *rules-starting-with) 
(memoize 'parse -.test #*eq) 

(defun parser (words) 
"Return all complete parses of a list of words." 
(clear-memoize 'parse) 
(mapcar #'parse-tree (complete-parses (parse words)))) 

In normal human language use, memoization would not work very well, since the 
interpretation of a phrase depends on the context in which the phrase was uttered. 
But with context-free grammars we have a guarantee that the context cannot affect the 
interpretation. The call (parse words) must return all possible parses for the words. 
We are free to choose between the possibilities based on contextual information, but 

<a id='page-663'></a>

context can never supply a new interpretation that is not in the context-free list of 

parses. 

The function use is introduced to tell the table-lookup functions that they are out 
of date whenever the grammar changes: 

(defun use (grammar) 

"Switch to a new grammar." 

(clear-memoize 'rules-starting-with) 

(clear-memoize 'lexical-rules) 

(length (setf *grammar* grammar))) 

Now we run the benchmark again with the memoized version of pa rse: 

> (time (length (parser s))) 

Evaluation of (LENGTH (PARSER S 'S)) took .13 Seconds of elapsed time. 

10 

By memoizing pa rs e we reduce the parse time from 33 to .13 seconds, a 250-fold speedup. 
We can get a more systematic comparison by looking at a range of examples. 
For example, consider sentences of the form "The man hit the table [with the ball]*" 
for zero or more repetitions of the PP "with the ball." In the following table we 
record N, the number of repetitions of the PP, along with the number of resulting 
parses^, and for both memoized and unmemoized versions of parse, the number 
of seconds to produce the parse, the number of parses per second (PPS), and the 
number of recursive calls to parse. The performance of the memoized version is 
quite acceptable; for N=5, a 20-word sentence is parsed into 132 possibilities in .68 
seconds, as opposed to the 20 seconds it takes in the unmemoized version. 

^The number of parses of sentences of this kind is the same as the number of bracketings 
of a arithmetic expression, or the number of binary trees with a given number of leaves. The 
resulting sequence (1,2,5,14,42,...) is known as the Catalan Numbers. This kind of ambiguity 
is discussed by Church and Patil (1982) in their articleCoping with Syntactic Ambiguity, or How 
to Put the Block in the Box on the Table. 

<a id='page-664'></a>

Memoized Unmemoized 
. Parses Sees PPS CaUs Sees PPS CaUs 
0 1 0.02 60 4 0.02 60 17 
1 2 0.02 120 11 0.07 30 96 
2 5 0.05 100 21 0.23 21 381 
3 14 0.10 140 34 0.85 16 1388 
4 42 0.23 180 50 3.17 13 4999 
5 132 0.68 193 69 20.77 6 18174 
6 429 1.92 224 91 -
7 1430 5.80 247 116 -
8 4862 20.47 238 144 -

&#9635; Exercise 19.1 Pi] It seems that we could be more efficient still by memoizing with 
a table consisting of a vector whose length is the number of words in the input (plus 
one). Implement this approach and see if it entails less overhead than the more 
general hash table approach. 

19.4 The Unknown-Word Problem 
As it stands, the parser cannot deal with unknown words. Any sentence containing 
a word that is not in the grammar will be rejected, even if the program can parse all 
the rest of the words perfectly. One way of treating unknown words is to allow them 
to be any of the "open-class" categories - nouns, verbs, adjectives, and names, in our 
grammar. An unknown word will not be considered as one of the "closed-class" 
categories - prepositions, determiners, or pronouns. This can be programmed very 
simply by having 1 exi ca 1 - rul es return a list of these open-class rules for every word 
that is not already known. 

(defparameter *open-categories* '(NVA Name) 
"Categories to consider for unknown words") 

(defun lexical-rules (word) 
"Return a list of rules with word on the right-hand side." 
(or (find-all word *grammar* :key #'rule-rhs :test #'equal) 

(mapcar #'(lambda (cat) '(.cat -> .word)) *open-categories*))) 

With memoization of 1 exi cal - rul es, this means that the lexicon is expanded every 
time an unknown word is encountered. Let's try this out: 

> (parser '(John liked Mary)) 
((S (NP (NAME JOHN)) 
(VP (V LIKED) (NP (NAME MARY))))) 

<a id='page-665'></a>

> (parser '(Dana liked Dale)) 
((S (NP (NAME DANA)) 
(VP (V LIKED) (NP (NAME DALE))))) 

> (parser '(the rab zaggled the woogly quax)) 
((S (NP (D THE) (N RAB)) 
(VP (V ZAGGLED) (NP (D THE) (A+ (A WOOGLY)) (N QUAX))))) 

We see the parser works as well with words it knows (John and Mary) as with new 
words (Dana and Dale), which it can recognize as names because of their position 
in the sentence. In the last sentence in the example, it recognizes each unknown 
word unambiguously. Things are not always so straightforward, unfortunately, as 
the following examples show: 

> (parser '(the slithy toves gymbled)) 

((S (NP (D THE) (N SLITHY)) (VP (V TOVES) (NP (NAME GYMBLED)))) 
(S (NP (D THE) (A+ (A SLITHY)) (N TOVES)) (VP (V GYMBLED))) 
(NP (D THE) (A+ (A SLITHY) (A+ (A TOVES))) (N GYMBLED))) 

> (parser '(the slithy toves gymbled on the wabe)) 
((S (NP (D THE) (N SLITHY)) 
(VP (VP (V TOVES) (NP (NAME GYMBLED))) 
(PP (P ON) (NP (D THE) (N WABE))))) 
(S (NP (D THE) (N SLITHY)) 
(VP (V TOVES) (NP (NP (NAME GYMBLED)) 
(PP (P ON) (NP (D THE) (N WABE)))))) 
(S (NP (D THE) (A+ (A SLITHY)) (N TOVES)) 
(VP (VP (V GYMBLED)) (PP (P ON) (NP (D THE) (N WABE))))) 
(NP (NP (D THE) (A+ (A SLITHY) (A+ (A TOVES))) (N GYMBLED)) 
(PP (P ON) (NP (D THE) (N WABE))))) 

If the program knew morphology - that a y at the end of a word often signals an 
adjective, an s a plural noun, and an ed a past-tense verb - then it could do much 
better. 

19.5 Parsing into a Semantic Representation 
Syntactic parse trees of a sentence may be interesting, but by themselves they're not 
very useful. We use sentences to communicate ideas, not to display grammatical 
structures. To explore the idea of the semantics, or meaning, of a phrase, we need 
a domain to talk about. Imagine the scenario of a compact disc player capable of 
playing back selected songs based on their track number. Imagine further that this 
machine has buttons on the front panel indicating numbers, as well as words such as 
"play," "to," "and," and "without." If you then punch in the sequence of buttons "play 

<a id='page-666'></a>

1 to 5 without 3/' you could reasonably expect the machine to respond by playing 
tracks 1,2,4, and 5. After a few such successful interactions, you might say that the 
machine "understands" a limited language. The important point is that the utility of 
this machine would not be enhanced much if it happened to display a parse tree of 
the input. On the other hand, you would be justifiably annoyed if it responded to 
"play 1 to 5 without 3" by playing 3 or skipping 4. 

Now let's stretch the imagination one more time by assuming that this CD player 
comes equipped with a full Common Lisp compiler, and that we are now in charge 
of writing the parser for its input language. Let's first consider the relevant data 
structures. We need to add a component for the semantics to both the rule and tree 
structures. Once we've done that, it is clear that trees are nothing more than instances 
of rules, so their definitions should reflect that. Thus, I use an : 1nc1 ude defstruct 
to define trees, and I specify no copier function, because copy-tree is already a 
Common Lisp function, and I don't want to redefine it. To maintain consistency 
with the old new-tree function (and to avoid having to put in all those keywords) I 
definetheconstructor new-tree. Thisoptiontodefstructmakes (new-tree a b c) 
equivalent to (make-tree :lhs a :sem b :rhsc). 

(defstruct (rule (itype list)) 
Ihs -> rhs sem) 

(defstruct (tree (:type list) (:include rule) (rcopiernil) 
(:constructor new-tree (Ihs sem rhs)))) 

We will adopt the convention that the semantics of a word can be any Lisp object. For 
example, the semantics of the word "1" could be the object 1, and the semantics of 
"without" could be the function set-di ff erence. The semantics of a tree is formed 
by taking the semantics of the rule that generated the tree and applying it (as a 
function) to the semantics of the constituents of the tree. Thus, the grammar writer 
must insure that the semantic component of rules are functions that expect the right 
number of arguments. For example, given the rule 

(NP -> (NP CONJ NP) infix-funcall) 

then the semantics of the phrase "1 to 5 without 3" could be determined by first deter-
miningthesemanticsof"lto5"tobe(l 2 3 4 5),of"without"tobeset -difference, 
and of "3" to be (3). After these sub-constituents are determined, the rule is applied 
by calling the function infix-funcall with the three arguments (1 2 3 4 5), 
set-difference, and (3). Assuming that infix-funcall is defined to apply its 
second argument to the other two arguments, the result will be (1 2 4 5). 

This may make more sense if we look at a complete grammar for the CD player 
problem: 

<a id='page-667'></a>

(use 

'((NP -> (NP CONJ NP) infix-funcall) 
(NP -> (N) list) 
(NP -> (N . .) infix-funcall) 
(. -> (DIGIT) identity) 
(P -> to integers) 
(CONJ -> and union) 
(CONJ -> without set-difference) 
(N -> 1 1) (N -> 2 2) (N -> 3 3) (N -> 4 4) (N -> 5 5) 
(N -> 6 6) (N -> 7 7) (N -> 8 8) (N -> 9 9) (N -> 0 0))) 

(defun integers (start end) 
"A list of all the integers in the range [start...end] inclusive." 
(if (> start end) nil 

(cons start (integers (+ start 1) end)))) 

(defun infix-funcall (argl function arg2) 
"Apply the function to the two arguments" 
(funcall function argl arg2)) 

Consider the first three grammar rules, which are the only nonlexical rules. The first 
says that when two NPs are joined by a conjunction, we assume the translation of 
the conjunction will be a function, and the translation of the phrase as a whole is 
derived by calling that function with the translations of the two NPs as arguments. 
The second rule says that a single noun (whose translation should be a number) 
translates into the singleton list consisting of that number. The third rule is similar 
to the first, but concerns joining Ns rather than NPs. The overall intent is that the 
translation of an NP will always be a list of integers, representing the songs to play. 

As for the lexical rules, the conjunction "and" translates to the union function, 
"without" translates to the function that subtracts one set from another, and "to" 
translates to the function that generates a list of integers between two end points. 
The numbers "0" to "9" translate to themselves. Note that both lexical rules like 
"CONJ -> and" and nonlexical rules like "NP -> (N . .)" can have functions as 
their semantic translations; in the first case, the function will just be returned as the 
semantic translation, whereas in the second case the function will be applied to the 
list of constituents. 

Only minor changes are needed to par s e to support this kind of semantic processing. 
As we see in the following, we add a sem argument to extend - pa r se and arrange 
to pass the semantic components around properly. When we have gathered all the 
right-hand-side components, we actually do the function application. All changes 
are marked with We adopt the convention that the semantic value .i 1 indicates 
failure, and we discard all such parses. 

<a id='page-668'></a>

(defun parse (words) 
"Bottom-up parse, returning all parses of any prefix of words. 
This version has semantics." 
(unless (null words) 

(mapcan #'(lambda (rule) 
(extend-parse (rule-lhs rule) (rule-sem rule) 
(list (first words)) (rest words) nil)) 
(lexical-rules (first words))))) 

(defun extend-parse (Ihs sem rhs rem needed) 
"Look for the categories needed to complete the parse. 
This version has semantics." 
(if (null needed) 

If nothing is needed, return this parse and upward extensions, 
:; unless the semantics fails 
(let ((parse (make-parse rtree (new-tree Ihs sem rhs) :rem rem))) 

(unless (null (apply-semantics (parse-tree parse))) 
(cons parse 
(mapcan 
#'(lambda (rule) 

(extend-parse (rule-lhs rule) (rule-semrule) 
(list (parse-tree parse)) rem 
(rest (rule-rhs rule)))) 

(rules-starting-with Ihs))))) 
;; otherwise try to extend rightward 
(mapcan 

#*(lambda (p) 
(if (eq (parse-lhs p) (first needed)) 
(extend-parse Ihs sem (appendl rhs (parse-tree p)) 
(parse-rem p) (rest needed)))) 

(parse rem)))) 

We need to add some new functions to support this: 
(defun apply-semantics (tree) 
"For terminal nodes, just fetch the semantics. 
Otherwise, apply the sem function to its constituents." 
(if (terminal-tree-p tree) 
(tree-sem tree) 
(setf (tree-sem tree) 
(apply (tree-sem tree) 
(mapcar #'tree-sem (tree-rhs tree)))))) 

(defun terminal-tree-p (tree) 
"Does this tree have a single word on the rhs?" 
(and (length=1 (tree-rhs tree)) 

(atom (first (tree-rhs tree))))) 

<a id='page-669'></a>

(defun meanings (words) 
"Return all possible meanings of a phrase. Throw away the syntactic part." 
(remove-duplicates (mapcar #'tree-sem (parser words)) :test #'equal)) 

Here are some examples of the meanings that the parser can extract: 

> (meanings '(1 to 5 without 3)) 
((1 2 4 5)) 

> (meanings '(1 to 4 and 7 to 9)) 
((123478 9)) 

> (meanings '(1 to 6 without 3 and 4)) 
((12 4 5 6) 
(1 2 5 6)) 

The example "(1 to 6 without 3 and 4)" is ambiguous. The first reading corresponds 
to "((1 to 6) without 3) and 4/' while the second corresponds to "(1 to 6) 
without (3 and 4)." The syntactic ambiguity leads to a semantic ambiguity - the two 
meanings have different lists of numbers in them. However, it seems that the second 
reading is somehow better, in that it doesn't make a lot of sense to talk of adding 4 to 
a set that already includes it, which is what the first translation does. 

We can upgrade the lexicon to account for this. The following lexicon insists 
that "and" conjoins disjoint sets and that "without" removes only elements that were 
already in the first argument. If these conditions do not hold, then the translation 
will return nil, and the parse will fail. Note that this also means that an empty list, 
such as "3 to 2," will also fail. 

The previous grammar only allowed for the numbers 0 to 9. We can allow larger 
numbers by stringing together digits. So now we have two rules for numbers: a 
number is either a single digit, in which case the value is the digit itself (the i dent i ty 
function), or it is a number followed by another digit, in which case the value is 10 
times the number plus the digit. We could alternately have specified a number to be 
a digit followed by a number, or even a number followed by a number, but either of 
those formulations would require a more complex semantic interpretation. 

(use 

'((NP -> (NP CONJ NP) infix-funcall) 
(NP -> (N) list) 
(NP -> (N . .) infix-funcall) 
(. -> (DIGIT) identity) 
(N -> (N DIGIT) 10*N+D) 
(P -> to integers) 
(CONJ -> and union*) 
(CONJ -> without set-diff) 
(DIGIT -> 1 1) (DIGIT -> 2 2) (DIGIT -> 3 3) 

<a id='page-670'></a>

(DIGIT -> 4 4) (DIGIT -> 5 5) (DIGIT -> 6 6) 
(DIGIT -> 7 7) (DIGIT -> 8 8) (DIGIT -> 9 9) 
(DIGIT -> 0 0))) 

(defun union* (x y) (if (null (intersection . y)) (append . y))) 
(defun set-diff (. y) (if (subsetp y .) (set-difference . y))) 
(defun 10*N-^D (N D) (+ (* 10 N) D)) 

With this new grammar, we can get single interpretations out of most reasonable 
inputs: 

> (meanings '(1 to 6 without 3 and 4)) 
((1 2 5 6)) 

> (meanings '(1 and 3 to 7 and 9 without 5 and 6)) 
((13 4 7 9)) 

> (meanings '(1 and 3 to 7 and 9 without 5 and 2)) 
((134679 2)) 

> (meanings '(1 9 8 to 2 0 D) 
((198 199 200 201)) 

> (meanings '(1 2 3)) 
(123 (123)) 

The example "1 2 3" shows an ambiguity between the number 123 and the list (123), 
but all the others are unambiguous. 

19.6 Parsing with Preferences 
One reason we have unambiguous interpretations is that we have a very limited 
domain of interpretation: we are dealing with sets of numbers, not lists. This is 
perhaps typical of the requests faced by a CD player, but it does not account for 
all desired input. For example, if you had a favorite song, you couldn't hear it 
three times with the request "1 and 1 and 1" under this grammar. We need some 
compromise between the permissive grammar, which generated all possible parses, 
and the restrictive grammar, which eliminates too many parses. To get the "best" 
interpretation out of an arbitrary input, we will not only need a new grammar, we 
will also need to modify the program to compare the relative worth of candidate 
interpretations. In other words, we will assign each interpretation a numeric score, 
and then pick the interpretation with the highest score. 

We start by once again modifying the rule and tree data types to include a score 
component. As with the sem component, this will be used to hold first a function to 
compute a score and then eventually the score itself. 

<a id='page-671'></a>

(defstruct (rule (:type list) 
(:constructor 
rule (Ihs -> rhs &optional sem score))) 
Ihs -> rhs sem score) 

(defstruct (tree (itype list) (rinclude rule) (:copiernil) 
(:constructor new-tree (Ihs sem score rhs)))) 

Note that we have added the constructor function rul e. The intent is that the sem 
and score component of grammar rules should be optional. The user does not have 
to supply them, but the function use will make sure that the function rul e is called 
to fill in the missing sem and score values with ni 1. 

(defun use (grammar) 
"Switch to a new grammar." 
(clear-memoize 'rules-starting-with) 
(clear-memoize 'lexical-rules) 
(length (setf *grammar* 

(mapcar #'(lambda (r) (apply #'rule r)) 
grammar)))) 

Now we modify the parser to keep track of the score. The changes are again minor, 
and mirror the changes needed to add semantics. There are two places where we 
put the score into trees as we create them, and one place where we apply the scoring 
function to its arguments. 

(defun parse (words) 
"Bottom-up parse, returning all parses of any prefix of words. 
This version has semantics and preference scores." 
(unless (null words) 

(mapcan #'(lambda (rule) 

(extend-parse 
(rule-lhs rule) (rule-sem rule) 
(rule-score rule) (list (first words)) 
(rest words) nil)) 

(lexical-rules (first words))))) 

(defun extend-parse (Ihs sem score rhs rem needed) 
"Look for the categories needed to complete the parse. 
This version has semantics and preference scores." 
(if (null needed) 

If nothing is needed, return this parse and upward extensions, 
;; unless the semantics fails 
(let ((parse (make-parse :tree (new-tree Ihs sem score rhs) 

:rem rem))) 
(unless (null (apply-semantics (parse-tree parse))) 

<a id='page-672'></a>

(apply-scorer (parse-tree parse)) 
(cons parse 
(mapcan 
#'(lambda (rule) 

(extend-parse 
(rule-lhs rule) (rule-sem rule) 
(rule-score rule) (list (parse-tree parse)) 
rem (rest (rule-rhs rule)))) 

(rules-starting-with Ihs))))) 
otherwise try to extend rightward 
(mapcan 
#*(lambda (p) 
(if (eq (parse-lhs p) (first needed)) 

(extend-parse Ihs sem score 
(appendl rhs (parse-tree p)) 
(parse-rem p) (rest needed)))) 

(parse rem)))) 

Again we need some new functions to support this. Most important is appl y - scorer, 
which computes the score for a tree. If the tree is a terminal (a word), then the function 
just looks up the score associated with that word. In this grammar all words have 
a score of 0, but in a grammar with ambiguous words it would be a good idea to 
give lower scores for infrequently used senses of ambiguous words. If the tree is 
a nonterminal, then the score is computed in two steps. First, all the scores of the 
constituents of the tree are added up. Then, this is added to a measure for the tree 
as a whole. The rule associated with each tree will have either a number attached to 
it, which is added to the sum, or a function. In the latter case, the function is applied 
to the tree, and the result is added to obtain the final score. Asa final special case, if 
the function returns nil, then we assume it meant to return zero. This will simplify 
the definition of some of the scoring functions. 

(defun apply-scorer (tree) 
"Compute the score for this tree." 
(let ((score (or (tree-score tree) 0))) 

(setf (tree-score tree) 
(if (terminal-tree-p tree) 
score 

Add up the constituent's scores, 
;; along with the tree's score 
(+ (sum (tree-rhs tree) #'tree-score-or-0) 

(if (numberp score) 
score 

(or (apply score (tree-rhs tree)) 0))))))) 

Here is an accessor function to pick out the score from a tree: 

<a id='page-673'></a>

(defun tree-score-or-O (tree) 

(if (numberp (tree-score tree)) 
(tree-score tree) 
0)) 

Here is the updated grammar. First, I couldn't resist the chance to add more features 
to the grammar. I added the postnominal adjectives "shuffled," which randomly 
permutes the list of songs, and "reversed," which reverses the order of play. I also 
added the operator "repeat," as in "1 to 3 repeat 5," which repeats a list a certain 
number of times. 1 also added brackets to allow input that says explicitly how it 
should be parsed. 

(use 

'((NP -> (NP CONJ NP) infix-funcall infix-scorer) 
(NP -> (N . .) infix-funcall infix-scorer) 
(NP -> (.) list) 
(NP ([ NP ]) arg2) 
(NP (NP ADJ) rev-funcal1 rev-scorer) 
(NP -> (NP OP N) infix-funcall) 
(N -> (D) identity) 
(N (N D) 10*N+D) 
(P -> to integers prefer<) 

([ -> [ [) 
(] -> ] ]) 
(OP -> repeat repeat) 
(CONJ -> and append prefer-disjoint) 
(CONJ -> without set-difference prefer-subset) 
(ADJ -> reversed reverse inv-span) 
(ADJ -> shuffled permute prefer-not-singleton) 
(D -> 1 1) (D -> 2 2) (D -> 3 3) (D -> 4 4) (D -> 5 5) 
(D -> 6 6) (D -> 7 7) (D -> 8 8) (D -> 9 9) (D -> 0 0))) 

The following scoring functions take trees as inputs and compute bonuses or penalties 
for those trees. The scoring function pref er<, used for the word "to," gives a 
one-point penalty for reversed ranges: "5 to 1" gets a score of -1, while "1 to 5" gets 
a score of 0. The scorer for "and," prefer-di s joi nt, gives a one-point penalty for 
intersecting lists: "1 to 3 and 7 to 9" gets a score of 0, while "1 to 4 and 2 to 5" gets -1. 
The "x without y" scorer, prefer-subset, gives a three-point penalty when the y list 
has elements that aren't in the . list. It also awards points in inverse proportion to the 
length (in words) of the . phrase. The idea is that we should prefer to bind "without" 
tightly to some small expression on the left. If the final scores come out as positive 
or as nonintegers, then this scoring component is responsible, since all the other 
components are negative intgers. The "x shuffled" scorer, prefer-not-singleton, 
is similar, except that there the penalty is for shuffling a list of less than two songs. 

<a id='page-674'></a>

(defun prefer< (x y) 
(if (>= (sem X) (sem y)) -1)) 

(defun prefer-disjoint (x y) 
(if (intersection (sem x) (sem y)) -1)) 

(defun prefer-subset (x y) 
(+ (inv-span x) (if (subsetp (sem y) (sem x)) 0 -3))) 

(defun prefer-not-singleton (x) 
(+ (inv-span x) (if (< (length (sem x)) 2) -4 0))) 

The inf i x-scorer and rev-scorer functionsdon'taddanythingnew,theyjustassure 
that the previously mentioned scoring functions will get applied in the right place. 

(defun infix-scorer (argl scorer arg2) 
(funcall (tree-score scorer) argl arg2)) 

(defun rev-scorer (arg scorer) (funcall (tree-score scorer) arg)) 

Here are the functions mentioned in the grammar, along with some useful utilities: 

(defun arg2 (al a2 &rest a-n) (declare (ignore al a-n)) a2) 

(defun rev-funcall (arg function) (funcall function arg)) 

(defun repeat (list n) 
"Append list . times." 
(if (= . 0) 

nil 
(append list (repeat list (- . 1))))) 

(defun span-length (tree) 
"How many words are in tree?" 
(if (terminal-tree-p tree) 1 

(sum (tree-rhs tree) #'span-length))) 

(defun inv-span (tree) (/ 1 (span-length tree))) 

(defun sem (tree) (tree-sem tree)) 

(defun integers (start end) 
"A list of all the integers in the range [start...end]inclusive. 
This version allows start > end." 
(cond ((< start end) (cons start (integers (+ start 1) end))) 

((> start end) (cons start (integers (- start 1) end))) 
(t (list start)))) 

(defun sum (numbers &optional fn) 
"Sum the numbers, or sum (mapcar fn numbers)." 
(if fn 

(loop for X in numbers sum (funcall fn x)) 
(loop for X in numbers sum x))) 

<a id='page-675'></a>

(defun permute (bag) 
"Return a random permutation of the given input list. " 
(if (null bag) 

nil 
(let ((e (random-elt bag))) 
(cons e (permute (remove e bag rcount 1 :test #*eq)))))) 

We will need a way to show off the preference rankings: 

(defun all-parses (words) 
(format t "~%Score Semantics^ZBT^a" words) 
(format t "~% = --251 -~%") 
(loop for tree in (sort (parser words) #*> :key#'tree-score) 

do (format t "~5,lf ~9a~25T''a~%" (tree-score tree) (tree-sem tree) 
(bracketing tree))) 
(values)) 

(defun bracketing (tree) 
"Extract the terminals, bracketed with parens." 
(cond ((atom tree) tree) 

((length=1 (tree-rhs tree)) 
(bracketing (first (tree-rhs tree)))) 
(t (mapcar #'bracketing (tree-rhs tree))))) 

Now we can try some examples: 

> (all-parses '(1 to 6 without 3 and 4)) 
Score Semantics (1 TO 6 WITHOUT 3 AND 4) 

0.3 (1 2 5 6) ((1 TO 6) WITHOUT (3 AND 4)) 
-0.7 (1 2 4 5 6 4) (((1 TO 6) WITHOUT 3) AND 4) 
> (all -parses '(1 and 3 to 7 and 9 without 5 and 6)) 
Score Semantics (1 AND 3 TO 7 AND 9 WITHOUT 5 AND 6) 

0.2 (1 3 4 7 9) (1 AND (((3 TO 7) AND 9) WITHOUT (5 AND 6))) 
0.1 (1 3 4 7 9) (((1 AND (3 TO 7)) AND 9) WITHOUT (5 AND 6)) 
0.1 (1 3 4 7 9) ((1 AND ((3 TO 7) AND 9)) WITHOUT (5 AND 6)) 
-0.8 (1 3 4 6 7 9 6) ((1 AND (((3 TO 7) AND 9) WITHOUT 5)) AND 6) 
-0.8 (1 3 4 6 7 9 6) (1 AND ((((3 TO 7) AND 9) WITHOUT 5) AND 6)) 
-0.9 (1 3 4 6 7 9 6) ((((1 AND (3 TO 7)) AND 9) WITHOUT 5) AND 6) 
-0.9 (1 3 4 6 7 9 6) (((1 AND ((3 TO 7) AND 9)) WITHOUT 5) AND 6) 
-2.0 (1 3 4 5 6 7 9) ((1 AND (3 TO 7)) AND (9 WITHOUT (5 AND 6))) 
-2.0 (1 3 4 5 6 7 9) (1 AND ((3 TO 7) AND (9 WITHOUT (5 AND 6)))) 
-3.0 (1 3 4 5 6 7 9 6) (((1 AND (3 TO 7)) AND (9 WITHOUT 5)) AND 6) 
-3.0 (1 3 4 5 6 7 9 6) ((1 AND (3 TO 7)) AND ((9 WITHOUT 5) AND 6)) 
-3.0 (1 3 4 5 6 7 9 6) ((1 AND ((3 TO 7) AND (9 WITHOUT 5))) AND 6) 
<a id='page-676'></a>

-3.0 (1 3 4 5 6 7 9 6) (1 AND (((3 TO 7) AND (9 WITHOUT 5)) AND 6)) 
-3.0 (13 4 5 6 7 9 6) (1 AND ((3 TO 7) AND ((9 WITHOUT 5) AND 6))) 

> (all -parses '(1 and 3 :o 7 and 9 without 5 and 2)) 
Score Semantics (1 AND 3 TO 7 AND 9 WITHOUT 5 AND 2) 

0.2 (1 3 4 6 7 9 2) ((1 AND (((3 TO 7) AND 9) WITHOUT 5)) AND 2) 
0.2 (1 3 4 6 7 9 2) (1 AND ((((3 TO 7) AND 9) WITHOUT 5) AND 2)) 
0.1 (1 3 4 6 7 9 2) ((((1 AND (3 TO 7)) AND 9) WITHOUT 5) AND 2) 
0.1 (1 3 4 6 7 9 2) (((1 AND ((3 TO 7) AND 9)) WITHOUT 5) AND 2) 
-2.0 (1 3 4 5 6 7 9 2) (((1 AND (3 TO 7)) AND (9 WITHOUT 5)) AND 2) 
-2.0 (1 3 4 5 6 7 9 2) ((1 AND (3 TO 7)) AND ((9 WITHOUT 5) AND 2)) 
-2.0 (1 3 4 5 6 7 9) ((1 AND (3 TO 7)) AND (9 WITHOUT (5 AND 2))) 
-2.0 (1 3 4 5 6 7 9 2) ((1 AND ((3 TO 7) AND (9 WITHOUT 5))) AND 2) 
-2.0 (1 3 4 5 6 7 9 2) (1 AND (((3 TO 7) AND (9 WITHOUT 5)) AND 2)) 
-2.0 (1 3 4 5 6 7 9 2) (1 AND ((3 TO 7) AND ((9 WITHOUT 5) AND 2))) 
-2.0 (1 3 4 5 6 7 9) (1 AND ((3 TO 7) AND (9 WITHOUT (5 AND 2)))) 
-2.8 (1 3 4 6 7 9) (1 AND (((3 TO 7) AND 9) WITHOUT (5 AND 2))) 
-2.9 (1 3 4 6 7 9) (((1 AND (3 TO 7)) AND 9) WITHOUT (5 AND 2)) 
-2.9 (1 3 4 6 7 9) ((1 AND ((3 TO 7) AND 9)) WITHOUT (5 AND 2)) 
In each case, the preference rules are able to assign higher scores to more reasonable 
interpretations. It turns out that, in each case, all the interpretations with positive 
scores represent the same set of numbers, while interpretations with negative scores 
seem worse. Seeing all the scores in gory detail may be of academic interest, but what 
we really want is something to pick out the best interpretation. The following code 
is appropriate for many situations. It picks the top scorer, if there is a unique one, 
or queries the user if several interpretations tie for the best score, and it complains 
if there are no valid parses at all. The query-user function may be useful in many 
applications, but note that meani ng uses it only as a default; a program that had some 
automatic way of deciding could supply another ti e-breaker function to meani ng. 

(defun meaning (words &optional (tie-breaker #'query-user)) 
"Choose the single top-ranking meaning for the words." 
(let* ((trees (sort (parser words) #*> :key #'tree-score)) 

(best-score (if trees (tree-score (first trees)) 0)) 
(best-trees (delete best-score trees 
:key #*tree-score :test-not #'eql)) 
(best-sems (delete-duplicates (mapcar #'tree-sem best-trees) 
.-test #'equal))) 

(case (length best-sems) 
(0 (format t "~&Sorry. I didn't understand that.") nil) 
(1 (first best-sems)) 
(t (funcall tie-breaker best-sems))))) 

<a id='page-677'></a>

(defun query-user (choices &optiona1 
(header-str "~&Please pick one:") 
(footer-str "~&Your choice? ")) 

"Ask user to make a choice." 
(format *query-io* header-str) 
(loop for choice in choices for i from 1 do 

(format *query-io* "~&~3d: ~a" i choice)) 
(format *query-io* footer-str) 
(nth (- (read) 1) choices)) 

Here we see some final examples: 

> (meaning '(1 to 5 without 3 and 4)) 
(1 2 5) 

> (meaning '(1 to 5 without 3 and 6)) 
(12 4 5 6) 

> (meaning '(1 to 5 without 3 and 6 shuffled)) 
(64125) 

> (meaning '([ 1 to 5 without C 3 and 6 ] ] reversed)) 
(5 4 2 1) 

> (meaning '(1 to 5 to 9)) 

Sorry. I didn't understand that. 
NIL 

> (meaning '(1 to 5 without 3 and 7 repeat 2)) 
Please pick one: 

1: (12 4 5 7 12 4 5 7) 
2: (12 4 5 7 7) 
Your choice? 1 
(12 4 5 7 12 4 5 7) 

> (all-parses '(1 to 5 without 3 and 7 repeat 2)) 
Score Semantics (1 TO 5 WITHOUT 3 AND 7 REPEAT 2) 

0.3 (12 4 5 7 12 4 5 7) ((((1 TO 5) WITHOUT 3) AND 7) REPEAT 2) 
0.3 (12 4 5 7 7) (((1 TO 5) WITHOUT 3) AND (7 REPEAT 2)) 
-2.7 (12 4 5 12 4 5) (((1 TO 5) WITHOUT (3 AND 7)) REPEAT 2) 
-2.7 (12 4 5) ((1 TO 5) WITHOUT ((3 AND 7) REPEAT 2)) 
-2.7 (12 4 5) ((1 TO 5) WITHOUT (3 AND (7 REPEAT 2))) 
This last example points out a potential problem: I wasn't sure what was a good 
scoring function for "repeat," so I left it blank, it defaulted to 0, and we end up 
with two parses with the same score. This example suggests that "repeat" should 
probably involve inv-span like the other modifiers, but perhaps other factors should 
be involved as well. There can be a complicated interplay between phrases, and it 

<a id='page-678'></a>

is not always clear where to assign the score. For example, it doesn't make much 
sense to repeat a "without" phrase; that is, the bracketing (. without (y repeat 
.)) is probably a bad one. But the scorer for "without" nearly handles that already. 
It assigns a penalty if its right argument is not a subset of its left. Unfortunately, 
repeated elements are not counted in sets, so for example, the list (1 2 3 1 2 3) is a 
subset of (1 2 3 4). However, we could change the scorer for "without" to test for 
sub-bag-. (not a built-in Common Lisp function) instead, and then "repeat" would 
not have to be concerned with that case. 

19.7 The Problem with Context-Free 
Phrase-Structure Rules 
The fragment of English grammar we specified in section 19.2 admits a variety of 
ungrammatical phrases. For example, it is equally happy with both "I liked her" and 
"me liked she." Only the first of these should be accepted; the second should be 
ruled out. Similarly, our grammar does not state that verbs have to agree with their 
subjects in person and number. And, since the grammar has no notion of meaning, 
it will accept sentences that are semantically anomalous (or at least unusual), such 
as "the table liked the man." 

There are also some technical problems with context-free grammars. For example, 
it can be shown that no context-free grammar can be written to account for the 
language consisting of just the strings ABC, AABBCC, AAABBBCCC, and so forth, 
where each string has an equal number of As, Bs, and Cs. Yet sentences roughly of 
that form show up (admittedly rarely) in natural languages. An example is "Robin 
and Sandy loved and hated Pat and Kim, respectively." While there is still disagreement 
over whether it is possible to generate natural languages with a context-free 
grammar, clearly it is much easier to use a more powerful grammatical formalism. 
For example, consider solving the subject-predicate agreement problem. It is possible 
to do this with a context-free language including categories like singular-NP, 
plural-NP, singular-VP, and plural-VP, but it is far easier to augment the grammatical 
formahsm to allow passing features between constituents. 

It should be noted that context-free phrase-structure rules turned out to be very 
useful for describing programming languages. Starting with Algol 60, the formalism 
has been used under the name Bflcfcus-Nflwr Form (BNF) by computer scientists. In this 
book we are more interested in natural languages, so in the next chapter we will see a 
more powerful formalism known as unification grammar that can handle the problem 
of agreement, as well as other difficulties. Furthermore, unification grammars allow a 
natural way of attaching semantics to a parse. 

<a id='page-679'></a>

19.8 History and References 
There is a class of parsing algorithms known as chart parsers that explicitly cache 
partial parses and reuse them in constructing larger parses. Barley's algorithm (1970) 
is the first example, and Martin Kay (1980) gives a good overview of the field and 
introduces a data structure, the chart, for storing substrings of a parse. Winograd 
(1983) gives a complex (five-page) specification of a chart parser. None of these 
authors have noticed that one can achieve the same results by augmenting a simple 
(one-page) parser with memoization. In fact, it is possible to write a top-down parser 
that is even more succinct. (See exercise 19.3 below.) 

For a general overview of natural language processing, my preferences (in order) 
are Allen 1987, Winograd 1983 or Gazdar and Mellish 1989. 

19.9 Exercises 
&#9635; Exercise 19.2 [m-h] Experiment with the grammar and the parser. Find sentences 
it cannot parse correctly, and try to add new syntactic rules to account for them. 

&#9635; Exercise 19.3 [m-h] The parser works in a bottom-up fashion. Write a top-down 
parser, and compare it to the bottom-up version. Can both parsers work with the 
same grammar? If not, what constraints on the grammar does each parsing strategy 
impose? 

&#9635; Exercise 19.4 [h] Imagine an interface to a dual cassette deck. Whereas the CD 
player had one assumed verb, "play," this unit has three explicit verb forms: "record," 
"play," and "erase." There should also be modifiers "from" and "to," where the object 
of a "to" is either 1 or 2, indicating which cassette to use, and the object of a "from" 
is either 1 or 2, or one of the symbols PHONO, CD, or AUX. It's up to you to design 
the grammar, but you should allow input something like the following, where I have 
chosen to generate actual Lisp code as the meaning: 

> (meaning '(play 1 to 5 from CD shuffled and 
record 1 to 5 from CD and 1 and 3 and 7 from 1)) 

(PROGN (PLAY '(15 2 3 4) :FROM 'CD) 
(RECORD '(12345) :FROM 'CD) 
(RECORD '(1 3 7) :FROM .)) 

This assumes that the functions play and record take keyword arguments (with 
defaults) for : from and : to. You could also extend the grammar to accommodate an 
automatic timer, with phrases like "at 3:00." 

<a id='page-680'></a>

&#9635; Exercise 19.5 [m] In the definition of permute, repeated here, why is the :test 
#'eq needed? 

(defun permute (bag) 
"Return a random permutation of the given input list. " 
(if (null bag) 

nil 
(let ((e (random-elt bag))) 
(cons e (permute (remove e bag :count 1 :test #'eq)))))) 

&#9635; Exercise 19.6 [m] The definition of permute takes 0{n^). Replace it by an 0 (n) 
algorithm. 

19.10 Answers 
Answer 19.1 

(defun parser (words) 
"Return all complete parses of a list of words." 
(let* ((table (make-array (+ (length words) 1) :initial-element 0)) 

(parses (parse words (length words) table))) 
(mapcar #'parse-tree (complete-parses parses)))) 

(defun parse (words num-words table) 
"Bottom-up parse, returning all parses of any prefix of words." 
(unless (null words) 

(let ((ans (aref table num-words))) 

(if (not (eq ans 0)) 
ans 
(setf (aref table num-words) 

(mapcan #*(lambda (rule) 

(extend-parse (rule-lhs rule) 
(list (firstwords)) 
(rest words) nil 
(- num-words 1) table)) 

(lexical-rules (first words)))))))) 

<a id='page-681'></a>

(defun extend-parse (Ihs rhs rem needed num-words table) 
"Look for the categories needed to complete the parse." 
(if (null needed) 

If nothing is needed, return this parse and upward extensions 
(let ((parse (make-parse :tree (new-tree Ihs rhs) :rem rem))) 
(cons parse 
(mapcan 
#*(lambda (rule) 

(extend-parse (rule-lhs rule) 
(list (parse-tree parse)) 
rem (rest (rule-rhs rule)) 
num-words table)) 

(rules-starting-with Ihs)))) 
otherwise try to extend rightward 
(mapcan 
#'(lambda (p) 
(if (eq (parse-lhs p) (first needed)) 

(extend-parse Ihs (appendl rhs (parse-tree p)) 
(parse-rem p) (rest needed) 
(length (parse-rem p)) table))) 

(parse rem num-words table)))) 

It turns out that, for the Lisp system used in the timings above, this version is no 
faster than normal memoization. 

Answer 19.3 Actually, the top-down parser is a little easier (shorter) than the 

bottom-up version. The problem is that the most straightforward way of imple


menting a top-down parser does not handle so-called left recursive rules - rules of the 

form(X -> (X ...)). This includes rules we've used, like (NP -> (NP and NP)). 

The problem is that the parser will postulate an NP, and then postulate that it is of 

the form (NP and NP), and that the first NP of that expression is of the form (NP and 

NP), and so on. An infinite structure of NPs is explored before even the first word is 

considered. 

Bottom-up parsers are stymied by rules with null right-hand sides: (X -> ()) . 
Note that I was careful to exclude such rules in my grammars earlier. 

(defun parser (words &optional (cat *S)) 
"Parse a list of words; return only parses with no remainder." 
(mapcar #*parse-tree (complete-parses (parse words cat)))) 

(defun parse (tokens start-symbol) 
"Parse a list of tokens, return parse trees and remainders." 
(if (eq (first tokens) start-symbol) 

(list (make-parse rtree (first tokens) :rem (rest tokens))) 
(mapcan #*(lambda (rule) 
(extend-parse (Ihs rule) nil tokens (rhs rule))) 
(rules-for start-symbol)))) 

<a id='page-682'></a>

(defun extend-parse (Ihs rhs rem needed) 
"Parse the remaining needed symbols." 
(if (null needed) 

(list (make-parse :tree (cons Ihs rhs) :rem rem)) 
(mapcan 
#'(lambda (p) 
(extend-parse Ihs (append rhs (list (parse-tree p))) 
(parse-rem p) (rest needed))) 
(parse rem (first needed))))) 

(defun rules-for (cat) 
"Return all the rules with category on Ihs" 
(find-all cat ^grammar* :key #'rule-lhs)) 

Answer 19.5 If it were omitted, then : tes t would default to #'eql, and it would be 
possible to remove the "wrong" element from the list. Consider the list (1.0 1.0) in 
an implementation where floating-point numbers are eql but not eq. if random-el t 
chooses the first 1.0 first, then everything is satisfactory - the result Ust is the same 
as the input list. However, if random-el t chooses the second 1.0, then the second 

1.0will be the first element of the answer, but remove will remove the wrong 1.0! It 
will remove the first 1.0, and the final answer will be a list with two pointers to the 
second 1.0 and none to the first. In other words, we could have: 
> (member (first x) (permute x) .-test #'eq) 
NIL 

Answer 19.6 

(defun permute (bag) 

"Return a random permutation of the bag." 
It is done by converting the bag to a vector, but the 
result is always the same type as the input bag. 

(let ((bag-copy (replace (make-array (length bag)) bag)) 
(bag-type (if (listp bag) 'list (type-of bag)))) 
(coerce (permute-vector! bag-copy) bag-type))) 

(defun permute-vector! (vector) 
"Destructively permute (shuffle) the vector." 
(loop for i from (length vector) downto 2 do 

(rotatef (aref vector (-i D) 
(aref vector (random i)))) 
vector) 

The answer uses rotatef, a relative of setf that swaps 2 or more values. That is, 
(rotatef a b) is like: 

<a id='page-683'></a>
(let ((temp a)) 
(setf a b) 
(setf b temp) 
nil) 
Rarely, rotatef is used with more than two arguments, (rotatef a b c) is like: 
(let ((temp a)) 
(setf a b) 
(setf b c) 
(setf c temp) 
nil) 

