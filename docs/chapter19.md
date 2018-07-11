# Chapter 19
## Introduction to Natural Language

> Language is everywhere.
It permeates our thoughts mediates our relations with others, and even creeps into our dreams.
The overwhelming bulk of human knowledge is stored and transmitted in language.
Language is so ubiquitous that we take it for granted but without it, society as we know it would be impossible.

> –Ronand Langacker

> Language and its Structure (1967)

Anatural language is a language spoken by people, such as English, German, or Tagalog.
This is in opposition to artificial languages like Lisp, FORTRAN, or Morse code.
Natural language processing is an important part of AI because language is intimately connected to thought.
One measure of this is the number of important books that mention language and thought in the title: in AI, Schank and Colby’s *Computer Models of Thought and Language;* in linguistics, Whorf’s *Language, Thought, and Reality* (and Chomsky’s *Language and Mind;)* in philosophy, Fodor’s *The Language of Thought;* and in psychology, Vygotsky’s *Thought and Language* and John Anderson’s *Language, Memory, and Thought.* Indeed, language is the trait many think of as being the most characteristic of humans.
Much controversy has been generated over the question of whether animais, especially primates and dolphins, can use and “understand” language.
Similar controversy surrounds the same question asked of computers.

The study of language has been traditionally separated into two broad classes: syntax, or grammar, and semantics, or meaning.
Historically, syntax has achieved the most attention, largely because on the surface it is more amenable to formai and semiformal methods.
Although there is evidence that the boundary between the two is at best fuzzy, we still maintain the distinction for the purposes of these notes.
We will cover the “easier” part, syntax, first, and then move on to semantics.

A good artificial language, like Lisp or C, is unambiguous.
There is only one interpretation for a valid Lisp expression.
Of course, the interpretation may depend on the state of the current state of the Lisp world, such as the value of global variables.
But these dependencies can be explicitly enumerated, and once they are spelled out, then there can only be one meaning for the expression.[1](#fn0015){:#xfn0015}

Natural language does not work like this.
Natural expressions are inherently ambiguous, depending on any number of factors that can never be quite spelled out completely.
It is perfectly reasonable for two people to disagree on what some other person meant by a natural language expression.
(Lawyers and judges make their living largely by interpreting natural language expressions–laws–that are meant to be unambiguous but are not.)

This chapter is a brief introduction to natural language processing.
The next chapter gives a more thorough treatment from the point of view of logic grammars, and the chapter after that puts it all together into a full-fledged system.

## [ ](#){:#st0010}19.1 Parsing with a Phrase-Structure Grammar
{:#s0010}
{:.h1hd}

To parse a sentence means to recover the constituent structure of the sentence–to discover what sequence of generation rules could have been applied to come up with the sentence.
In general, there may be several possible derivations, in which case we say the sentence is grammatically ambiguous.
In certain circles, the term “parse” means to arrive at an understanding of a sentence’s meaning, not just its grammatical form.
We will attack that more difficult question later.

We start with the grammar defined on [page 39](B9780080571157500029.xhtml#p39) for the generate program:

[ ](#){:#l0010}`(defvar *grammar* “The grammar used by GENERATE.”)`
!!!(p) {:.unnumlist}

`(defparameter *grammarl*`
!!!(p) {:.unnumlist}

`   ’((Sentence -> (NP VP))`
!!!(p) {:.unnumlist}

`     (NP -> (Art Noun))`
!!!(p) {:.unnumlist}

`     (VP -> (Verb NP))`
!!!(p) {:.unnumlist}

`     (Art -> the a)`
!!!(p) {:.unnumlist}

`     (Noun -> man ball woman table)`
!!!(p) {:.unnumlist}

`     (Verb -> hit took saw liked)))`
!!!(p) {:.unnumlist}

Our parser takes as input a list of words and returns a structure containing the parse tree and the unparsed words, if any.
That way, we can parse the remaining words under the next category to get compound rules.
For example, in parsing “the man saw the table,” we would first parse “the man,” returning a structure representing the noun phrase, with the remaining words “saw the table.” This remainder would then be parsed as a verb phrase, returning no remainder, and the two phrases could then be joined to form a parse that is a complete sentence with no remainder.

Before proceeding, I want to make a change in the representation of grammar rules.
Currently, rules have a left-hand side and a list of alternative right-hand sides.
But each of these alternatives is really a separate rule, so it would be more modular to write them separately.
For the `generate` program it was fine to have them all together, because that made processing choices easier, but now I want a more flexible representation.
Later on we will want to add more information to each rule, like the semantics of the assembled left-hand side, and constraints between constituents on the right-hand side, so the rules would become quite large indeed if we didn’t split up the alternatives.
I also take this opportunity to clear up the confusion between words and category symbols.
The convention is that a right-hand side can be either an atom, in which case it is a word, or a list of symbols, which are then all interpreted as categories.
To emphasize this, I include "noun" and “verb” as nouns in the grammar `*grammar3*`, which is otherwise equivalent to the previous `*grammarl*`.

[ ](#){:#l0015}`(defparameter *grammar3*`
!!!(p) {:.unnumlist}

`   '((Sentence -> (NP VP))`
!!!(p) {:.unnumlist}

`     (NP -> (Art Noun))`
!!!(p) {:.unnumlist}

`     (VP -> (Verb NP))`
!!!(p) {:.unnumlist}

`     (Art -> the) (Art -> a)`
!!!(p) {:.unnumlist}

`     (Noun -> man) (Noun -> ball) (Noun -> woman) (Noun -> table)`
!!!(p) {:.unnumlist}

`     (Noun -> noun) (Noun -> verb)`
!!!(p) {:.unnumlist}

`     (Verb -> hit) (Verb -> took) (Verb -> saw) (Verb -> liked)))`
!!!(p) {:.unnumlist}

`(setf *grammar* *grammar3*)`
!!!(p) {:.unnumlist}

I also define the data types `rule, parse`, and `tree`, and some functions for getting at the rules.
Rules are defined as structures of type list with three slots: the left-hand side, the arrow (which should always be represented as the literal ->) and the right-hand side.
Compare this to the treatment on [page 40](B9780080571157500029.xhtml#p40).

[ ](#){:#l0020}`(defstruct (rule (:type list)) lhs -> rhs)`
!!!(p) {:.unnumlist}

`(defstruct (parse) "A parse tree and a remainder." tree rem)`
!!!(p) {:.unnumlist}

`;; Trees are of the form: (lhs .
rhs)`
!!!(p) {:.unnumlist}

`(defun new-tree (cat rhs) (cons cat rhs))`
!!!(p) {:.unnumlist}

`(defun tree-lhs (tree) (first tree))`
!!!(p) {:.unnumlist}

`(defun tree-rhs (tree) (rest tree))`
!!!(p) {:.unnumlist}

`(defun parse-lhs (parse) (tree-lhs (parse-tree parse)))`
!!!(p) {:.unnumlist}

`(defun lexical-rules (word)`
!!!(p) {:.unnumlist}

`   "Return a list of rules with word on the right-hand side."`
!!!(p) {:.unnumlist}

`   (find-all word *grammar* :key #’rule-rhs :test #’equal))`
!!!(p) {:.unnumlist}

`(defun rules-starting-with (cat)`
!!!(p) {:.unnumlist}

`   "Return a list of rules where cat starts the rhs."`
!!!(p) {:.unnumlist}

`   (find-all cat *grammar*`
!!!(p) {:.unnumlist}

`                :key #’(lambda (rule) (first-or-nil (rule-rhs rule)))))`
!!!(p) {:.unnumlist}

`(defun first-or-nil (x)`
!!!(p) {:.unnumlist}

`   "The first element of x if it is a list; else nil."`
!!!(p) {:.unnumlist}

`   (if (consp x) (first x) nil))`
!!!(p) {:.unnumlist}

Now we’re ready to define the parser.
The main function parser takes a list of words to parse.
It calls parse, which returns a list of all parses that parse some subsequence of the words, starting at the beginning.
parser keeps only the parses with no remainder–that is, the parses that span all the words.

[ ](#){:#l0025}`(defun parser (words)`
!!!(p) {:.unnumlist}

`   "Return all complete parses of a list of words."`
!!!(p) {:.unnumlist}

`   (mapcar #’parse-tree (compiete-parses (parse words))))`
!!!(p) {:.unnumlist}

`(defun compiete-parses (parses)`
!!!(p) {:.unnumlist}

`   "Those parses that are complete (have no remainder)."`
!!!(p) {:.unnumlist}

`   (find-all-if #’null parses :key #’parse-rem))`
!!!(p) {:.unnumlist}

The function parse looks at the first word and considers each category it could be.
It makes a parse of the first word under each category, and calls extend - parse to try to continue to a complete parse.
parse uses mapcan to append together all the resulting parses.
As an example, suppose we are trying to parse “the man took the ball.” pa rse would find the single lexical rule for “the” and call extend-parse with a parse with tree (Art the) and remainder “man took the ball,” with no more categories needed.

`extend-parse` has two cases.
If the partial parse needs no more categories to be complete, then it returns the parse itself, along with any parses that can be formed by extending parses starting with the partial parse.
In our example, there is one rule starting with `Art`, namely `(NP -> (Art Noun))`, so the function would try to extend the parse tree (`NP (Art the))` with remainder “man took the ball,” with the category `Noun` needed.
That call to `extend-parse` represents the second case.
We first parse “man took the ball,” and for every parse that is of category `Noun` (there will be only one), we combine with the partial parse.
In this case we get `(NP (Art the) (Noun man))`.
This gets extended as a sentence with a VP needed, and eventually we get a parse of the complete list of words.

[ ](#){:#l0030}`(defun parse (words)`
!!!(p) {:.unnumlist}

`   "Bottom-up parse, returning all parses of any prefix of words."`
!!!(p) {:.unnumlist}

`   (unless (null words)`
!!!(p) {:.unnumlist}

`      (mapcan #’(lambda (rule)`
!!!(p) {:.unnumlist}

`                         (extend-parse (rule-lhs rule) (list (first words))`
!!!(p) {:.unnumlist}

`                                               (rest words) nil))`
!!!(p) {:.unnumlist}

`                    (lexical-rules (first words)))))`
!!!(p) {:.unnumlist}

`(defun extend-parse (lhs rhs rem needed)`
!!!(p) {:.unnumlist}

`   "Look for the categories needed to complete the parse."`
!!!(p) {:.unnumlist}

`   (if (null needed)`
!!!(p) {:.unnumlist}

`       ;; If nothing needed.
return parse and upward extensions`
!!!(p) {:.unnumlist}

`       (let ((parse (make-parse :tree (new-tree lhs rhs) :rem rem)))`
!!!(p) {:.unnumlist}

`         (cons parse`
!!!(p) {:.unnumlist}

`                   (mapcan`
!!!(p) {:.unnumlist}

`                     # '(lambda (rule)`
!!!(p) {:.unnumlist}

`                             (extend-parse (rule-lhs rule)`
!!!(p) {:.unnumlist}

`                                                   (list (parse-tree parse))`
!!!(p) {:.unnumlist}

`                                                   rem (rest (rule-rhs rule))))`
!!!(p) {:.unnumlist}

`                     (rules-starting-with lhs))))`
!!!(p) {:.unnumlist}

`       ;; otherwise try to extend rightward`
!!!(p) {:.unnumlist}

`       (mapcan`
!!!(p) {:.unnumlist}

`         #’(lambda (p)`
!!!(p) {:.unnumlist}

`             (if (eq (parse-lhs p) (first needed))`
!!!(p) {:.unnumlist}

`                       (extend-parse lhs (appendl rhs (parse-tree p))`
!!!(p) {:.unnumlist}

`                                              (parse-rem p) (rest needed))))`
!!!(p) {:.unnumlist}

`          (parse rem))))`
!!!(p) {:.unnumlist}

This makes use of the auxiliary function append1:

[ ](#){:#l0035}`(defun appendl (items item)`
!!!(p) {:.unnumlist}

`   "Add item to end of list of items."`
!!!(p) {:.unnumlist}

`   (append items (list item)))`
!!!(p) {:.unnumlist}

Some examples of the parser in action are shown here:

[ ](#){:#l0040}`> (parser ’(the table))`
!!!(p) {:.unnumlist}

`((NP (ART THE) (NOUN TABLE)))`
!!!(p) {:.unnumlist}

`> (parser ’(the ball hit the table))`
!!!(p) {:.unnumlist}

`((SENTENCE (NP (ART THE) (NOUN BALL))`
!!!(p) {:.unnumlist}

`              (VP (VERB HIT)`
!!!(p) {:.unnumlist}

`                   (NP (ARTTHE) (NOUN TABLE)))))`
!!!(p) {:.unnumlist}

`> (parser '(the noun took the verb))`
!!!(p) {:.unnumlist}

`((SENTENCE (NP (ART THE) (NOUN NOUN))`
!!!(p) {:.unnumlist}

`              (VP (VERB TOOK)`
!!!(p) {:.unnumlist}

`                   (NP (ARTTHE) (NOUN VERB)))))`
!!!(p) {:.unnumlist}

## [ ](#){:#st0015}19.2 Extending the Grammar and Recognizing Ambiguity
{:#s0015}
{:.h1hd}

Overall, the parser seems to work fine, but the range of sentences we can parse is quite limited with the current grammar.
The following grammar includes a wider variety of linguistic phenomena: adjectives, prepositional phrases, pronouns, and proper names.
It also uses the usual linguistic conventions for category names, summarized in the table below:

[ ](#){:#t0010}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| | Category | Examples |
| S | Sentence | *John likes Mary* |
| NP | Noun Phrase | *John; a blue table* |
| VP | Verb Phrase | *likes Mary; hit the ball* |
| PP | Prepositional Phrase | *to Mary; with the man* |
| A | Adjective | *little; blue* |
| A + | A list of one or more adjectives | *little blue* |
| D | Determiner | *the; a* |
| N | Noun | *ball; table* |
| Name | Proper Name | *John; Mary* |
| P | Preposition | *to; with* |
| Pro | Pronoun | *you; me* |
| V | Verb | *liked; hit* |

Here is the grammar:

[ ](#){:#l0045}`(defparameter *grammar4*`
!!!(p) {:.unnumlist}

`   ’((S -> (NP VP))`
!!!(p) {:.unnumlist}

`     (NP -> (D N))`
!!!(p) {:.unnumlist}

`     (NP -> (D A + N))`
!!!(p) {:.unnumlist}

`     (NP -> (NP PP))`
!!!(p) {:.unnumlist}

`     (NP -> (Pro))`
!!!(p) {:.unnumlist}

`     (NP -> (Name))`
!!!(p) {:.unnumlist}

`     (VP -> (V NP))`
!!!(p) {:.unnumlist}

`     (VP -> (V))`
!!!(p) {:.unnumlist}

`     (VP -> (VP PP))`
!!!(p) {:.unnumlist}

`     (PP -> (P NP))`
!!!(p) {:.unnumlist}

`     (A + -> (A))`
!!!(p) {:.unnumlist}

`     (A + -> (A A +))`
!!!(p) {:.unnumlist}

`     (Pro -> I) (Pro -> you) (Pro -> he) (Pro -> she)`
!!!(p) {:.unnumlist}

`     (Pro -> it) (Pro -> me) (Pro -> him) (Pro -> her)`
!!!(p) {:.unnumlist}

`     (Name -> John) (Name -> Mary)`
!!!(p) {:.unnumlist}

`     (A -> big) (A -> little) (A -> old) (A -> young)`
!!!(p) {:.unnumlist}

`     (A -> blue) (A -> green) (A -> orange) (A -> perspicuous)`
!!!(p) {:.unnumlist}

`     (D -> the) (D -> a) (D -> an)`
!!!(p) {:.unnumlist}

`     (N -> man) (N -> ball) (N -> woman) (N -> table) (N -> orange)`
!!!(p) {:.unnumlist}

`     (N -> saw) (N -> saws) (N -> noun) (N -> verb)`
!!!(p) {:.unnumlist}

`     (P -> with) (P -> for) (P -> at) (P -> on) (P -> by) (P -> of) (P -> in)`
!!!(p) {:.unnumlist}

`     (V -> hit) (V -> took) (V -> saw) (V -> liked) (V -> saws)))`
!!!(p) {:.unnumlist}

`(setf *grammar* *grammar4*)`
!!!(p) {:.unnumlist}

Now we can parse more interesting sentences, and we can see a phenomenon that was not present in the previous examples: ambiguous sentences.
The sentence “The man hit the table with the ball” has two parses, one where the ball is the thing that hits the table, and the other where the ball is on or near the table, parser finds both of these parses (although of course it assigns no meaning to either parse):

[ ](#){:#l0050}`> (parser '(The man hit the table with the ball))`
!!!(p) {:.unnumlist}

`((S (NP (D THE) (N MAN))`
!!!(p) {:.unnumlist}

`   (VP (VP (V HIT) (NP (D THE) (N TABLE)))`
!!!(p) {:.unnumlist}

`     (PP (P WITH) (NP (DTHE) (N BALL)))))`
!!!(p) {:.unnumlist}

`(S (NP (D THE) (N MAN))`
!!!(p) {:.unnumlist}

`   (VP (V HIT)`
!!!(p) {:.unnumlist}

`     (NP (NP (D THE) (N TABLE))`
!!!(p) {:.unnumlist}

`            (PP (P WITH) (NP (DTHE) (N BALL)))))))`
!!!(p) {:.unnumlist}

Sentences are not the only category that can be ambiguous, and not all ambiguities have to be between parses in the same category.
Here we see a phrase that is ambiguous between a sentence and a noun phrase:

[ ](#){:#l0055}`> (parser ’(the orange saw))`
!!!(p) {:.unnumlist}

`((S (NP (D THE) (N ORANGE)) (VP (V SAW)))`
!!!(p) {:.unnumlist}

` (NP (D THE) (A + (A ORANGE)) (N SAW)))`
!!!(p) {:.unnumlist}

## [ ](#){:#st0020}19.3 More Efficient Parsing
{:#s0020}
{:.h1hd}

With more complex grammars and longer sentences, the parser starts to slow down.
The main problem is that it keeps repeating work.
For example, in parsing “The man hit the table with the ball,” it has to reparse “with the ball” for both of the resulting parses, even though in both cases it receives the same analysis, a PP.
We have seen this problem before and have already produced an answer: memoization (see [section 9.6](#s0035)).
To see how much memoization will help, we need a benchmark:

[ ](#){:#l0060}`> (setf s (generate ’s))`
!!!(p) {:.unnumlist}

`(THE PERSPICUOUS BIG GREEN BALL BY A BLUE WOMAN WITH A BIG MAN`
!!!(p) {:.unnumlist}

`  HIT A TABLE BY THE SAW BY THE GREEN ORANGE)`
!!!(p) {:.unnumlist}

`> (time (length (parser s)))`
!!!(p) {:.unnumlist}

`Evaluation of (LENGTH (PARSER S)) took 33.11 Seconds of elapsed time.`
!!!(p) {:.unnumlist}

`10`
!!!(p) {:.unnumlist}

The sentence S has 10 parses, since there are two ways to parse the subject NP and five ways to parse the VP.
It took 33 seconds to discover these 10 parses with the parse function as it was written.

We can improve this dramatically by memoizing parse (along with the table- lookup functions).
Besides memoizing, the only change is to clear the memoization table within parser.

[ ](#){:#l0065}`(memoize ’lexical-rules)`
!!!(p) {:.unnumlist}

`(memoize ’rules-starting-with)`
!!!(p) {:.unnumlist}

`(memoize ’parse :test #’eq)`
!!!(p) {:.unnumlist}

`(defun parser (words)`
!!!(p) {:.unnumlist}

`   "Return all complete parses of a list of words."`
!!!(p) {:.unnumlist}

`   (clear-memoize ’parse) ;***`
!!!(p) {:.unnumlist}

`   (mapcar #’parse-tree (complete-parses (parse words))))`
!!!(p) {:.unnumlist}

In normal human language use, memoization would not work very well, since the interpretation of a phrase depends on the context in which the phrase was uttered.
But with context-f ree grammars we have a guarantee that the context cannot af f ect the interpretation.
The call `(parse words)` must return all possible parses for the words.
We are free to choose between the possibilities based on contextual information, but context can never supply a new interpretation that is not in the context-free list of parses.

The function use is introduced to tell the table-lookup functions that they are out of date whenever the grammar changes:

[ ](#){:#l0070}`(defun use (grammar)`
!!!(p) {:.unnumlist}

`   "Switch to a new grammar."`
!!!(p) {:.unnumlist}

`   (clear-memoize ߣrules-starting-with)`
!!!(p) {:.unnumlist}

`   (clear-memoize ߣlexical-rules)`
!!!(p) {:.unnumlist}

`   (length (setf *grammar* grammar)))`
!!!(p) {:.unnumlist}

Now we run the benchmark again with the memoized version of pa rse:

[ ](#){:#l0075}`> (time (length (parser s)))`
!!!(p) {:.unnumlist}

`Evaluation of (LENGTH (PARSER S ’s)) took .13 Seconds of elapsed time.`
!!!(p) {:.unnumlist}

`10`
!!!(p) {:.unnumlist}

By memoizing p a r s e we reduce the parse time f rom 33 to.
13 seconds, a 250-f old speed- up.
We can get a more systematic comparison by looking at a range of examples.
For example, consider sentences of the form “The man hit the table [with the ball]*” for zero or more repetitions of the PP “with the ball.” In the following table we record N, the number of repetitions of the PP, along with the number of resulting parses,[2](#fn0020){:#xfn0020} and for both memoized and unmemoized versions of parse, the number of seconds to produce the parse, the number of parses per second (PPS), and the number of recursive calls to `parse`.
The performance of the memoized version is quite acceptable; for N=5, a 20-word sentence is parsed into 132 possibilities in .68 seconds, as opposed to the 20 seconds it takes in the unmemoized version.

[ ](#){:#t0015}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| | Memoized | Unmemoized |
| N | Parses | Secs | PPS | Calls | Secs | PPS | Calls |
| 0 | 1 | 0.02 | 60 | 4 | 0.02 | 60 | 17 |
| 1 | 2 | 0.02 | 120 | 11 | 0.07 | 30 | 96 |
| 2 | 5 | 0.05 | 100 | 21 | 0.23 | 21 | 381 |
| 3 | 14 | 0.10 | 140 | 34 | 0.85 | 16 | 1388 |
| 4 | 42 | 0.23 | 180 | 50 | 3.17 | 13 | 4999 |
| 5 | 132 | 0.68 | 193 | 69 | 20.77 | 6 | 18174 |
| 6 | 429 | 1.92 | 224 | 91 | – | | |
| 7 | 1430 | 5.80 | 247 | 116 | – | | |
| 8 | 4862 | 20.47 | 238 | 144 | – | | |

![t0015](images/B9780080571157500194/t0015.png)

**Exercise 19.1 [h]** It seems that we could be more efficient still by memoizing with a table consisting of a vector whose length is the number of words in the input (plus one).
Implement this approach and see if it entails less overhead than the more general hash table approach.

## [ ](#){:#st0025}19.4 The Unknown-Word Problem
{:#s0025}
{:.h1hd}

As it stands, the parser cannot deal with unknown words.
Any sentence containing a word that is not in the grammar will be rejected, even if the program can parse all the rest of the words perfectly.
One way of treating unknown words is to allow them to be any of the “open-class” categories–nouns, verbs, adjectives, and names, in our grammar.
An unknown word will not be considered as one of the “closed-class” categories–prepositions, determiners, or pronouns.
This can be programmed very simply by having `lexical-rules` return a list of these open-class rules for every word that is not already known.

[ ](#){:#l0080}`(defparameter *open-categories* ’(N V A Name)`
!!!(p) {:.unnumlist}

`   "Categories to consider for unknown words")`
!!!(p) {:.unnumlist}

`(defun lexical-rules (word)`
!!!(p) {:.unnumlist}

`   "Return a list of rules with word on the right-hand side."`
!!!(p) {:.unnumlist}

`   (or (find-all word *grammar* :key #’rule-rhs :test #’equal)`
!!!(p) {:.unnumlist}

`         (mapcar #’(lambda (cat) ‘(,cat -> ,word)) *open-categories*)))`
!!!(p) {:.unnumlist}

With memoization of lexical - rules, this means that the lexicon is expanded every time an unknown word is encountered.
Let’s try this out:

[ ](#){:#l0085}`> (parser ’(John liked Mary))`
!!!(p) {:.unnumlist}

`((S (NP (NAME JOHN))`
!!!(p) {:.unnumlist}

`      (VP (V LIKED) (NP (NAME MARY)))))`
!!!(p) {:.unnumlist}

`> (parser ’(Dana liked Dale))`
!!!(p) {:.unnumlist}

`((S (NP (NAME DANA))`
!!!(p) {:.unnumlist}

`      (VP (V LIKED) (NP (NAME DALE)))))`
!!!(p) {:.unnumlist}

`> (parser ’(the rab zaggled the woogly quax))`
!!!(p) {:.unnumlist}

`((S (NP (D THE) (N RAB))`
!!!(p) {:.unnumlist}

`      (VP (V ZAGGLED) (NP (D THE) (A + (A WOOGLY)) (N QUAX)))))`
!!!(p) {:.unnumlist}

We see the parser works as well with words it knows (John and Mary) as with new words (Dana and Dale), which it can recognize as names because of their position in the sentence.
In the last sentence in the example, it recognizes each unknown word unambiguously.
Things are not always so straightforward, unfortunately, as the following examples show:

[ ](#){:#l0090}`> (parser ’(the slithy toves gymbled))`
!!!(p) {:.unnumlist}

`((S (NP (D THE) (N SLITHY)) (VP (V TOVES) (NP (NAME GYMBLED))))`
!!!(p) {:.unnumlist}

` (S (NP (D THE) (A + (A SLITHY)) (N TOVES)) (VP (V GYMBLED)))`
!!!(p) {:.unnumlist}

` (NP (D THE) (A + (A SLITHY) (A + (A TOVES))) (N GYMBLED)))`
!!!(p) {:.unnumlist}

`> (parser ’(the slithy toves gymbled on the wabe))`
!!!(p) {:.unnumlist}

`((S (NP (D THE) (N SLITHY))`
!!!(p) {:.unnumlist}

`   (VP (VP (V TOVES) (NP (NAME GYMBLED)))`
!!!(p) {:.unnumlist}

`      PP (P ON) (NP (D THE) (N WABE)))))`
!!!(p) {:.unnumlist}

`(S (NP (D THE) (N SLITHY))`
!!!(p) {:.unnumlist}

`   (VP (V TOVES) (NP (NP (NAME GYMBLED))`
!!!(p) {:.unnumlist}

`      (PP (P ON) (NP (D THE) (N WABE))))))`
!!!(p) {:.unnumlist}

`(S (NP (D THE) (A + (A SLITHY)) (N TOVES))`
!!!(p) {:.unnumlist}

`    (VP (VP (V GYMBLED)) (PP (P ON) (NP (D THE) (N WABE)))))`
!!!(p) {:.unnumlist}

`(NP (NP (D THE) (A + (A SLITHY) (A + (A TOVES))) (N GYMBLED))`
!!!(p) {:.unnumlist}

`    (PP (P ON) (NP (D THE) (N WABE)))))`
!!!(p) {:.unnumlist}

If the program knew morphology–that a *y* at the end of a word often signais an adjective, an *s* a plural noun, and an *ed* a past-tense verb–then it could do much better.

## [ ](#){:#st0030}19.5 Parsing into a Semantic Representation
{:#s0030}
{:.h1hd}

Syntactic parse trees of a sentence may be interesting, but by themselves they're not very useful.
We use sentences to communicate ideas, not to display grammatical structures.
To explore the idea of the semantics, or meaning, of a phrase, we need a domain to talk about.
Imagine the scenario of a compact dise player capable of playing back selected songs based on their track number.
Imagine further that this machine has buttons on the front panel indicating numbers, as well as words such as “play,” “to,” “and,” and “without.” If you then punch in the sequence of buttons “play 1 to 5 without 3,” you could reasonably expect the machine to respond by playing tracks 1,2,4, and 5.
After a few such successful interactions, you might say that the machine “understands” a limited language.
The important point is that the utility of this machine would not be enhanced much if it happened to display a parse tree of the input.
On the other hand, you would be justifiably annoyed if it responded to “play 1 to 5 without 3” by playing 3 or skipping 4.

Now let’s stretch the imagination one more time by assuming that this CD player cornes equipped with a full Common Lisp compiler, and that we are now in charge of writing the parser for its input language.
Let’s first consider the relevant data structures.
We need to add a component for the semantics to both the rule and tree structures.
Once we've done that, it is clear that trees are nothing more than instances of rules, so their definitions should reflect that.
Thus, I use an : incl ude defstruct to define trees, and I specify no copier function, because copy-tree is already a Common Lisp function, and I don’t want to redefine it.
To maintain consistency with the old new-tree function (and to avoid having to put in all those keywords) I define the constructor new-tree.
This option to `defstruct makes (new-tree a b c)` equivalent to `(make-tree :lhs a :sem b :rhs c)`.

[ ](#){:#l0095}`(defstruct (rule (:type list))`
!!!(p) {:.unnumlist}

`   lhs -> rhs sem)`
!!!(p) {:.unnumlist}

`(defstruct (tree (:type list) (:include rule) (:copiernil)`
!!!(p) {:.unnumlist}

`                 (:constructor new-tree (lhs sem rhs))))`
!!!(p) {:.unnumlist}

We will adopt the convention that the semantics of a word can be any Lisp object.
For example, the semantics of the word “1” could be the object 1, and the semantics of “without” could be the function set-di fference.
The semantics of a tree is formed by taking the semantics of the rule that generated the tree and applying it (as a function) to the semantics of the constituents of the tree.
Thus, the grammar writer must insure that the semantic component of rules are functions that expect the right number of arguments.
For example, given the rule

[ ](#){:#l0100}`   (NP -> (NP CONJ NP) infix-funcall)`
!!!(p) {:.unnumlist}

then the semantics of the phrase “1 to 5 without 3” could be determined by first determining the semantics of"1 to 5" tobe(l 2 3 4 5),of"without"tobe set-`difference`, and of “3” to be (3).
After these sub-constituents are determined, the rule is applied by calling the function `infix-funcall` with the three arguments (1 2 3 4 5), `set-difference`, and (3).
Assuming that `infix-funcall` is defined to apply its second argument to the other two arguments, the resuit will be (1 2 4 5).

This may make more sense if we look at a complete grammar for the CD player problem:

[ ](#){:#l0105}`(use`
!!!(p) {:.unnumlist}

`   '((NP -> (NP CONJ NP)    infix-funcall)`
!!!(p) {:.unnumlist}

`    (NP -> (N)              list)`
!!!(p) {:.unnumlist}

`    (NP -> (N P N)          infix-funcall)`
!!!(p) {:.unnumlist}

`    (N -> (DIGIT)           identity)`
!!!(p) {:.unnumlist}

`    (P -> to                integers)`
!!!(p) {:.unnumlist}

`    (CONJ -> and            union)`
!!!(p) {:.unnumlist}

`    (CONJ -> without        set-difference)`
!!!(p) {:.unnumlist}

`    (N -> 1 1) (N -> 2 2) (N -> 3 3) (N -> 4 4) (N -> 5 5)`
!!!(p) {:.unnumlist}

`    (N -> 6 6) (N -> 7 7) (N -> 8 8) (N -> 9 9) (N -> 0 0)))`
!!!(p) {:.unnumlist}

`(defun integers (start end)`
!!!(p) {:.unnumlist}

`   "A list of all the integers in the range [start…end] inclusive."`
!!!(p) {:.unnumlist}

`   (if (> start end) nil`
!!!(p) {:.unnumlist}

`       (cons start (integers (+ start 1) end))))`
!!!(p) {:.unnumlist}

`(defun infix-funcal1 (argl function arg2)`
!!!(p) {:.unnumlist}

`   "Apply the function to the two arguments"`
!!!(p) {:.unnumlist}

`   (funcall function argl arg2))`
!!!(p) {:.unnumlist}

Consider the first three grammar rules, which are the only nonlexical rules.
The first says that when two NPs are joined by a conjunction, we assume the translation of the conjunction will be a function, and the translation of the phrase as a whole is derived by calling that function with the translations of the two NPs as arguments.
The second rule says that a single noun (whose translation should be a number) translates into the singleton list consisting of that number.
The third rule is similar to the first, but concerns joining Ns rather than NPs.
The overall intent is that the translation of an NP will always be a list of integers, representing the songs to play.

As for the lexical rules, the conjunction “and” translates to the union function, “without” translates to the function that subtracts one set from another, and “to” translates to the function that generates a list of integers between two end points.
The numbers “0” to “9” translate to themselves.
Note that both lexical rules like “`CONJ ->` and” and nonlexical rules like “`NP -> (N P N)`” can have functions as their semantic translations; in the first case, the function will just be returned as the semantic translation, whereas in the second case the function will be applied to the list of constituents.

Only minor changes are needed to pa rse to support this kind of semantic processing.
As we see in the following, we add a sem argument to extend - parse and arrange to pass the semantic components around properly.
When we have gathered all the right-hand-side components, we actually do the function application.
All changes are marked with ***.
We adopt the convention that the semantic value `nil` indicates failure, and we discard all such parses.

[ ](#){:#l0110}`(defun parse (words)`
!!!(p) {:.unnumlist}

`   "Bottom-up parse, returning all parses of any prefix of words.`
!!!(p) {:.unnumlist}

`   This version has semantics."`
!!!(p) {:.unnumlist}

`   (unless (null words)`
!!!(p) {:.unnumlist}

`      (mapcan #’(lambda (rule)`
!!!(p) {:.unnumlist}

`                  (extend-parse (rule-lhs rule) (rule-sem rule) ;***`
!!!(p) {:.unnumlist}

`                                (list (first words)) (rest words) nil))`
!!!(p) {:.unnumlist}

`              (lexical-rules (first words)))))`
!!!(p) {:.unnumlist}

`(defun extend-parse (lhs sem rhs rem needed) ;***`
!!!(p) {:.unnumlist}

`   "Look for the categories needed to complete the parse.`
!!!(p) {:.unnumlist}

`   This version has semantics."`
!!!(p) {:.unnumlist}

`   (if (null needed)`
!!!(p) {:.unnumlist}

`       ;; If nothing is needed, return this parse and upward extensions.`
!!!(p) {:.unnumlist}

`       ;; unless the semantics fails`
!!!(p) {:.unnumlist}

`       (let ((parse (make-parse :tree (new-tree lhs sem rhs) :rem rem)))`
!!!(p) {:.unnumlist}

`         (unless (null (apply-semantics (parse-tree parse))) ;***`
!!!(p) {:.unnumlist}

`           (cons parse`
!!!(p) {:.unnumlist}

`                 (mapcan`
!!!(p) {:.unnumlist}

`                    #’(lambda (rule)`
!!!(p) {:.unnumlist}

`                         (extend-parse (rule-lhs rule) (rule-semrule) ;***`
!!!(p) {:.unnumlist}

`                                       (list (parse-tree parse)) rem`
!!!(p) {:.unnumlist}

`                                       (rest (rule-rhs rule))))`
!!!(p) {:.unnumlist}

`                          (rules-starting-with lhs)))))`
!!!(p) {:.unnumlist}

`       ;; otherwise try to extend rightward`
!!!(p) {:.unnumlist}

`       (mapcan`
!!!(p) {:.unnumlist}

`         #’(lambda (p)`
!!!(p) {:.unnumlist}

`             (if (eq (parse-lhs p) (first needed))`
!!!(p) {:.unnumlist}

`                 (extend-parse lhs sem (appendl rhs (parse-tree p)) ;***`
!!!(p) {:.unnumlist}

`                              (parse-rem p) (rest needed))))`
!!!(p) {:.unnumlist}

`         (parse rem))))`
!!!(p) {:.unnumlist}

We need to add some new functions to support this:

[ ](#){:#l0115}`(defun apply-semantics (tree)`
!!!(p) {:.unnumlist}

`   "For terminal nodes, just fetch the semantics.`
!!!(p) {:.unnumlist}

`   Otherwise, apply the sem function to its constituents."`
!!!(p) {:.unnumlist}

`   (if (terminal-tree-p tree)`
!!!(p) {:.unnumlist}

`       (tree-sem tree)`
!!!(p) {:.unnumlist}

`       (setf (tree-sem tree)`
!!!(p) {:.unnumlist}

`              (apply (tree-sem tree)`
!!!(p) {:.unnumlist}

`                    (mapcar #’tree-sem (tree-rhs tree))))))`
!!!(p) {:.unnumlist}

`(defun terminal-tree-p (tree)`
!!!(p) {:.unnumlist}

`   "Does this tree have a single word on the rhs?"`
!!!(p) {:.unnumlist}

`   (and (length=l (tree-rhs tree))`
!!!(p) {:.unnumlist}

`           (atom (first (tree-rhs tree)))))`
!!!(p) {:.unnumlist}

`(defun meanings (words)`
!!!(p) {:.unnumlist}

`   "Return all possible meanings of a phrase.
Throw away the syntactic part."`
!!!(p) {:.unnumlist}

`   (remove-duplicates (mapcar #’tree-sem (parser words)) :test #’equal))`
!!!(p) {:.unnumlist}

Here are some examples of the meanings that the parser can extract:

[ ](#){:#l0120}(meanings ’(1 to 5 without 3))
!!!(p) {:.unnumlist}

((1 2 4 5))
!!!(p) {:.unnumlist}

(meanings ’(1 to 4 and 7 to 9))
!!!(p) {:.unnumlist}

((1 2 3 4 7 8 9))
!!!(p) {:.unnumlist}

(meanings ’(1 to 6 without 3 and 4))
!!!(p) {:.unnumlist}

((12 4 5 6)
!!!(p) {:.unnumlist}

(1 2 5 6))
!!!(p) {:.unnumlist}

The example “(1 to 6 without 3 and 4)” is ambiguous.
The first reading corresponds to “((1 to 6) without 3) and 4,” while the second corresponds to “(1 to 6) without (3 and 4).” The syntactic ambiguity leads to a semantic ambiguity–the two meanings have different lists of numbers in them.
However, it seems that the second reading is somehow better, in that it doesn’t make a lot of sense to talk of adding 4 to a set that already includes it, which is what the first translation does.

We can upgrade the lexicon to account for this.
The following lexicon insists that “and” conjoins disjoint sets and that “without” removes only elements that were already in the first argument.
If these conditions do not hold, then the translation will return nil, and the parse will fail.
Note that this also means that an empty list, such as “3 to 2,” will also fail.

The previous grammar only allowed for the numbers 0 to 9.
We can allow larger numbers by stringing together digits.
So now we have two rules for numbers: a number is either a single digit, in which case the value is the digit itself (the i denti ty function), or it is a number followed by another digit, in which case the value is 10 times the number plus the digit.
We could alternately have specified a number to be a digit followed by a number, or even a number followed by a number, but either of those formulations would require a more complex semantic interpretation.

[ ](#){:#l0125}`(use`
!!!(p) {:.unnumlist}

`   '((NP -> (NP CONJ NP)   infix-funcall)`
!!!(p) {:.unnumlist}

`   (NP -> (N)              list)`
!!!(p) {:.unnumlist}

`   (NP -> (N P N)          infix-funcall)`
!!!(p) {:.unnumlist}

`   (N -> (DIGIT)           identity)`
!!!(p) {:.unnumlist}

`   (N -> (N DIGIT)         10*N+D)`
!!!(p) {:.unnumlist}

`   (P -> to                integers)`
!!!(p) {:.unnumlist}

`   (CONJ -> and            union*)`
!!!(p) {:.unnumlist}

`   (CONJ -> without        set-diff)`
!!!(p) {:.unnumlist}

`   (DIGIT -> 1 1) (DIGIT -> 2 2)   (DIGIT -> 3 3)`
!!!(p) {:.unnumlist}

`   (DIGIT -> 4 4) (DIGIT -> 5 5)   (DIGIT -> 6 6)`
!!!(p) {:.unnumlist}

`   (DIGIT -> 7 7) (DIGIT -> 8 8)   (DIGIT -> 9 9)`
!!!(p) {:.unnumlist}

`   (DIGIT -> 0 0)))`
!!!(p) {:.unnumlist}

`(defun union* (x y) (if (null (intersection x y)) (append x y)))`
!!!(p) {:.unnumlist}

`(defun set-diff (x y) (if (subsetp y x) (set-difference x y)))`
!!!(p) {:.unnumlist}

`(defun 10*N+D (N D) (+ (* 10 N) D))`
!!!(p) {:.unnumlist}

With this new grammar, we can get single interpretations out of most reasonable inputs:

[ ](#){:#l0130}`> (meanings ’(1 to 6 without 3 and 4))`
!!!(p) {:.unnumlist}

`((1 2 5 6))`
!!!(p) {:.unnumlist}

`> (meanings ’(1 and 3 to 7 and 9 without 5 and 6))`
!!!(p) {:.unnumlist}

`((13 4 7 9))`
!!!(p) {:.unnumlist}

`> (meanings ’(1 and 3 to 7 and 9 without 5 and 2))`
!!!(p) {:.unnumlist}

`((1 3 4 6 7 9 2))`
!!!(p) {:.unnumlist}

`> (meanings ’(1 9 8 to 2 0 1))`
!!!(p) {:.unnumlist}

`((198 199 200 201))`
!!!(p) {:.unnumlist}

`> (meanings ’(1 2 3))`
!!!(p) {:.unnumlist}

`(123 (123))`
!!!(p) {:.unnumlist}

The example “1 2 3” shows an ambiguity between the number 123 and the list (123), but all the others are unambiguous.

## [ ](#){:#st0035}19.6 Parsing with Preferences
{:#s0035}
{:.h1hd}

One reason we have unambiguous interpretations is that we have a very limited domain of interpretation: we are dealing with sets of numbers, not lists.
This is perhaps typical of the requests faced by a CD player, but it does not account for all desired input.
For example, if you had a favorite song, you couldn’t hear it three times with the request “1 and 1 and 1” under this grammar.
We need some compromise between the permissive grammar, which generated all possible parses, and the restrictive grammar, which eliminates too many parses.
To get the “best” interpretation out of an arbitrary input, we will not only need a new grammar, we will also need to modify the program to compare the relative worth of candidate interpretations.
In other words, we will assign each interpretation a numeric score, and then pick the interpretation with the highest score.

We start by once again modifying the rule and tree data types to include a score component.
As with the sem component, this will be used to hold first a function to compute a score and then eventually the score itself.

[ ](#){:#l0135}`(defstruct (rule (:type list)`
!!!(p) {:.unnumlist}

`                 (:constructor`
!!!(p) {:.unnumlist}

`                 rule (lhs -> rhs &optional sem score)))`
!!!(p) {:.unnumlist}

`    lhs -> rhs sem score)`
!!!(p) {:.unnumlist}

`(defstruct (tree (:type list) (:include rule) (:copiernil)`
!!!(p) {:.unnumlist}

`                 (:constructor new-tree (lhs sem score rhs))))`
!!!(p) {:.unnumlist}

Note that we have added the constructor function rul e.
The intent is that the sem and score component of grammar rules should be optional.
The user does not have to supply them, but the function use will make sure that the function rul e is called to fill in the missing sem and score values with nil.

[ ](#){:#l0140}`(defun use (grammar)`
!!!(p) {:.unnumlist}

`    "Switch to a new grammar."`
!!!(p) {:.unnumlist}

`    (clear-memoize 'rules-starting-with)`
!!!(p) {:.unnumlist}

`    (clear-memoize 'lexical-rules)`
!!!(p) {:.unnumlist}

`    (length (setf *grammar*`
!!!(p) {:.unnumlist}

`                  (mapcar #’(lambda (r) (apply #’rule r))`
!!!(p) {:.unnumlist}

`                          grammar))))`
!!!(p) {:.unnumlist}

Now we modify the parser to keep track of the score.
The changes are again minor, and mirror the changes needed to add semantics.
There are two places where we put the score into trees as we create them, and one place where we apply the scoring function to its arguments.

[ ](#){:#l0145}`(defun parse (words)`
!!!(p) {:.unnumlist}

`    "Bottom-up parse, returning all parses of any prefix of words.`
!!!(p) {:.unnumlist}

`    This version has semantics and preference scores."`
!!!(p) {:.unnumlist}

`    (unless (null words)`
!!!(p) {:.unnumlist}

`      (mapcan #’(lambda (rule)`
!!!(p) {:.unnumlist}

`                  (extend-parse`
!!!(p) {:.unnumlist}

`                     (rule-lhs rule) (rule-sem rule)`
!!!(p) {:.unnumlist}

`                     (rule-score rule) (list (first words)) ;***`
!!!(p) {:.unnumlist}

`                     (rest words) nil))`
!!!(p) {:.unnumlist}

`              (lexical-rules (first words)))))`
!!!(p) {:.unnumlist}

`(defun extend-parse (lhs sem score rhs rem needed) ;***`
!!!(p) {:.unnumlist}

`    "Look for the categories needed to complete the parse.`
!!!(p) {:.unnumlist}

`    This version has semantics and preference scores."`
!!!(p) {:.unnumlist}

`    (if (null needed)`
!!!(p) {:.unnumlist}

`          ;; If nothing is needed, return this parse and upward extensions,`
!!!(p) {:.unnumlist}

`          ;; unless the semantics fails`
!!!(p) {:.unnumlist}

`          (let ((parse (make-parse :tree (new-tree lhs sem score rhs) ;***`
!!!(p) {:.unnumlist}

`                                   :rem rem)))`
!!!(p) {:.unnumlist}

`             (unless (null (apply-semantics (parse-tree parse)))`
!!!(p) {:.unnumlist}

`    (apply-scorer (parse-tree parse)) ;***`
!!!(p) {:.unnumlist}

`    (cons parse`
!!!(p) {:.unnumlist}

`          (mapcan`
!!!(p) {:.unnumlist}

`            #’(lambda (rule)`
!!!(p) {:.unnumlist}

`                (extend-parse`
!!!(p) {:.unnumlist}

`                  (rule-lhs rule) (rule-sem rule)`
!!!(p) {:.unnumlist}

`                  (rule-score rule) (list (parse-tree parse)) ;***`
!!!(p) {:.unnumlist}

`                  rem (rest (rule-rhs rule))))`
!!!(p) {:.unnumlist}

`              (rules-starting-with lhs)))))`
!!!(p) {:.unnumlist}

`    ;; otherwise try to extend rightward`
!!!(p) {:.unnumlist}

`    (mapcan`
!!!(p) {:.unnumlist}

`      #’(lambda (p)`
!!!(p) {:.unnumlist}

`          (if (eq (parse-lhs p) (first needed))`
!!!(p) {:.unnumlist}

`              (extend-parse lhs sem score`
!!!(p) {:.unnumlist}

`                            (appendl rhs (parse-tree p)) ;***`
!!!(p) {:.unnumlist}

`                            (parse-rem p) (rest needed))))`
!!!(p) {:.unnumlist}

`           (parse rem))))`
!!!(p) {:.unnumlist}

Again we need some new functions to support this.
Most important is appl y - scorer, which computes the score for a tree.
If the tree is a terminal (a word), then the function just looks up the score associated with that word.
In this grammar all words have a score of 0, but in a grammar with ambiguous words it would be a good idea to give lower scores for infrequently used senses of ambiguous words.
If the tree is a nonterminal, then the score is computed in two steps.
First, all the scores of the constituents of the tree are added up.
Then, this is added to a measure for the tree as a whole.
The rule associated with each tree will have either a number attached to it, which is added to the sum, or a function.
In the latter case, the function is applied to the tree, and the resuit is added to obtain the final score.
As a final special case, if the function returns nil, then we assume it meant to return zero.
This will simplify the definition of some of the scoring functions.

[ ](#){:#l0150}`(defun apply-scorer (tree)`
!!!(p) {:.unnumlist}

`    "Compute the score for this tree."`
!!!(p) {:.unnumlist}

`    (let ((score (or (tree-score tree) 0)))`
!!!(p) {:.unnumlist}

`       (setf (tree-score tree)`
!!!(p) {:.unnumlist}

`              (if (terminal-tree-p tree)`
!!!(p) {:.unnumlist}

`                   score`
!!!(p) {:.unnumlist}

`                   ;; Add up the constituent’s scores,`
!!!(p) {:.unnumlist}

`                   ;; along with the tree’s score`
!!!(p) {:.unnumlist}

`                   (+ (sum (tree-rhs tree) #’tree-score-or-0)`
!!!(p) {:.unnumlist}

`                       (if (numberp score)`
!!!(p) {:.unnumlist}

`                           score`
!!!(p) {:.unnumlist}

`                           (or (apply score (tree-rhs tree)) 0)))))))`
!!!(p) {:.unnumlist}

Here is an accessor function to pick out the score from a tree:

[ ](#){:#l0155}`(defun tree-score-or-0 (tree)`
!!!(p) {:.unnumlist}

`    (if (numberp (tree-score tree))`
!!!(p) {:.unnumlist}

`        (tree-score tree)`
!!!(p) {:.unnumlist}

`        0))`
!!!(p) {:.unnumlist}

Here is the updated grammar.
First, I couldn’t resist the chance to add more features to the grammar.
I added the postnominal adjectives “shuffled,” which randomly permutes the list of songs, and “reversed,” which reverses the order of play.
I also added the operator “repeat,” as in “1 to 3 repeat 5,” which repeats a list a certain number of times.
I also added brackets to allow input that says explicitly how it should be parsed.

[ ](#){:#t0020}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `(use` |
| `    ’((NP` | `->` | `(NP CONJ NP)` | `infix-funcal1` | `infix-scorer)` |
| `    (NP` | `->` | `(N P N)` | `infix-funcal1` | `infix-scorer)` |
| `    (NP` | `->` | `(N)` | `list)` | |
| `    (NP` | `->` | `([ NP ])` | `arg2)` | |
| `    (NP` | `->` | `(NP ADJ)` | `rev-funcall` | `rev-scorer)` |
| `    (NP` | `->` | `(NP OP N)` | `infix-funcall)` | |
| `    (N` | `->` | `(D)` | `identity)` | |
| `    (N` | `->` | `(N D)` | `10*N+D)` | |
| `    (P` | `->` | `to` | `integers` | `prefer <)` |
| `    ([` | `->` | `[` | `[)` | |
| `    (]` | `->` | `]` | `])` | |
| `    (OP` | `->` | `repeat` | `repeat)` | |
| `    (CONJ` | `-> and` | `append` | `prefer-disjoint)` |
| `    (CONJ` | `-> without` | `set-difference` | `prefer-subset)` |
| `    (ADJ` | | `-> reversed` | `reverse` | `inv-span)` |
| `    (ADJ` | | `-> shuffled` | `permute` | `prefer-not-singleton)` |
| `    (D -> 1 1) (D -> 2 2) (D -> 3 3) (D -> 4 4) (D -> 5 5)` |
| `    (D -> 6 6) (D -> 7 7) (D -> 8 8) (D -> 9 9) (D -> 0 0)))` |

![t0020](images/B9780080571157500194/t0020.png)

The following scoring functions take trees as inputs and compute bonuses or penalties for those trees.
The scoring function `prefer <`, used for the word “to,” gives a one-point penalty for reversed ranges: “5 to 1” gets a score of -1, while “1 to 5” gets a score of 0.
The scorer for “and,” `prefer-disjoint`, gives a one-point penalty for intersecting lists: “1 to 3 and 7 to 9” gets a score of 0, while “1 to 4 and 2 to 5” gets -1.
The “x without y” scorer, `prefer-subset`, gives a three-point penalty when the y list has elements that aren’t in the x list.
It also awards points in inverse proportion to the length (in words) of the x phrase.
The idea is that we should prefer to bind “without” tightly to some small expression on the left.
If the final scores corne out as positive or as nonintegers, then this scoring component is responsible, since all the other components are negative intgers.
The “x shuffled” scorer, `prefer-not-singleton`, is similar, except that there the penalty is for shuffling a list of less than two songs.

[ ](#){:#l0160}`(defun prefer < (x y)`
!!!(p) {:.unnumlist}

`   (if (>= (sem x) (sem y)) -1))`
!!!(p) {:.unnumlist}

`(defun prefer-disjoint (x y)`
!!!(p) {:.unnumlist}

`   (if (intersection (sem x) (sem y)) -1))`
!!!(p) {:.unnumlist}

`(defun prefer-subset (x y)`
!!!(p) {:.unnumlist}

`   (+ (inv-span x) (if (subsetp (sem y) (sem x)) 0 -3)))`
!!!(p) {:.unnumlist}

`(defun prefer-not-singleton (x)`
!!!(p) {:.unnumlist}

`   (+ (inv-span x) (if (< (length (sem x)) 2) -4 0)))`
!!!(p) {:.unnumlist}

The `infix-scorer` and `rev-scorer` functionsdon’taddanythingnew, they justassure that the previously mentioned scoring functions will get applied in the right place.

[ ](#){:#l0165}`(defun infix-scorer (argl scorer arg2)`
!!!(p) {:.unnumlist}

`   (funcall (tree-score scorer) argl arg2))`
!!!(p) {:.unnumlist}

`(defun rev-scorer (arg scorer) (funcall (tree-score scorer) arg))`
!!!(p) {:.unnumlist}

Here are the functions mentioned in the grammar, along with some useful utilities:

[ ](#){:#l0170}`(defun arg2 (al a2 &rest a-n) (declare (ignore al a-n)) a2)`
!!!(p) {:.unnumlist}

`(defun rev-funcall (arg function) (funcall function arg))`
!!!(p) {:.unnumlist}

`(defun repeat (list n)`
!!!(p) {:.unnumlist}

`   "Append list n times."`
!!!(p) {:.unnumlist}

`   (if (= n 0)`
!!!(p) {:.unnumlist}

`        nil`
!!!(p) {:.unnumlist}

`        (append list (repeat list (- n 1)))))`
!!!(p) {:.unnumlist}

`(defun span-length (tree)`
!!!(p) {:.unnumlist}

`   "How many words are in tree?"`
!!!(p) {:.unnumlist}

`   (if (terminal-tree-p tree) 1`
!!!(p) {:.unnumlist}

`        (sum (tree-rhs tree) #’span-length)))`
!!!(p) {:.unnumlist}

`(defun inv-span (tree) (/ 1 (span-length tree)))`
!!!(p) {:.unnumlist}

`(defun sem (tree) (tree-sem tree))`
!!!(p) {:.unnumlist}

`(defun integers (start end)`
!!!(p) {:.unnumlist}

`   "A list of all the integers in the range [start…end] inclusive.`
!!!(p) {:.unnumlist}

`   This version allows start > end."`
!!!(p) {:.unnumlist}

`   (cond ((< start end) (cons start (integers (+ start 1) end)))`
!!!(p) {:.unnumlist}

`         ((> start end) (cons start (integers (- start 1) end)))`
!!!(p) {:.unnumlist}

`         (t (list start))))`
!!!(p) {:.unnumlist}

`(defun sum (numbers &optional fn)`
!!!(p) {:.unnumlist}

`   "Sum the numbers, or sum (mapcar fn numbers)."`
!!!(p) {:.unnumlist}

`   (if fn`
!!!(p) {:.unnumlist}

`       (loop for x in numbers sum (funcall fn x))`
!!!(p) {:.unnumlist}

`       (loop for x in numbers sum x)))`
!!!(p) {:.unnumlist}

`(defun permute (bag)`
!!!(p) {:.unnumlist}

`   "Return a random permutation of the given input list."`
!!!(p) {:.unnumlist}

`   (if (null bag)`
!!!(p) {:.unnumlist}

`        nil`
!!!(p) {:.unnumlist}

`        (let ((e (random-elt bag)))`
!!!(p) {:.unnumlist}

`          (cons e (permute (remove e bag :count 1 :test #’eq))))))`
!!!(p) {:.unnumlist}

We will need a way to show off the preference rankings:

[ ](#){:#l0175}`(defun all-parses (words)`
!!!(p) {:.unnumlist}

`   (format t “~%Score Semantics~25T~a” words)`
!!!(p) {:.unnumlist}

`   (format t “~%======= ========== ~ 25T =============== ~% ”)`
!!!(p) {:.unnumlist}

`   (loop for tree in (sort (parser words) #’> :key #’tree-score)`
!!!(p) {:.unnumlist}

`     do (format t “~5.1f ~ 9a~25T~a~%” (tree-score tree) (tree-sem tree)`
!!!(p) {:.unnumlist}

`                    (bracketing tree)))`
!!!(p) {:.unnumlist}

`   (values))`
!!!(p) {:.unnumlist}

`(defun bracketing (tree)`
!!!(p) {:.unnumlist}

`   "Extract the terminais, bracketed with parens."`
!!!(p) {:.unnumlist}

`   (cond ((atom tree) tree)`
!!!(p) {:.unnumlist}

`         ((length=l (tree-rhs tree))`
!!!(p) {:.unnumlist}

`          (bracketing (first (tree-rhs tree))))`
!!!(p) {:.unnumlist}

`         (t (mapcar #’bracketing (tree-rhs tree)))))`
!!!(p) {:.unnumlist}

Now we can try some examples:

[ ](#){:#t0025}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `> (all-parses '(1 to 6 without 3 and 4))` |
| `Score` | `Semantics` | `(1 TO 6 WITHOUT 3 AND 4)` |
| `=======` | `===========` | `========================` |
| `0.3` | `(12 5 6)` | `((1 TO 6) WITHOUT (3 AND 4))` |
| `-0.7` | `(12 4 5 6 4)` | `(((1 TO 6) WITHOUT 3) AND 4)` |
| `> (all-parses ’(1 and 3 to 7 and 9 without 5 and 6))` |
| `Score` | `Semantics` | `(1 AND 3 T0 7 AND 9 WITHOUT 5 AND 6)` |
| `=======` | `===========` | `=================================` |
| `0.2` | `(1 3 4 7 9)` | `(1 AND (((3 T0 7) AND 9) WITHOUT (5 AND 6)))` |
| `0.1` | `(1 3 4 7 9)` | `(((1 AND (3 T0 7)) AND 9) WITHOUT (5 AND 6))` |
| `0.1` | `(1 3 4 7 9)` | `((1 AND ((3 T0 7) AND 9)) WITHOUT (5 AND 6))` |
| `-0.8` | `(1 3 4 6 7 9 6)` | `((1 AND (((3 T0 7) AND 9) WITHOUT 5)) AND 6)` |
| `-0.8` | `(1 3 4 6 7 9 6)` | `(1 AND ((((3 T0 7) AND 9) WITHOUT 5) AND 6))` |
| `-0.9` | `(1 3 4 6 7 9 6)` | `((((1 AND (3 T0 7)) AND 9) WITHOUT 5) AND 6)` |
| `-0.9` | `(1 3 4 6 7 9 6)` | `(((1 AND ((3 T0 7) AND 9)) WITHOUT 5) AND 6)` |
| `-2.0` | `(1 3 4 5 6 7 9)` | `((1 AND (3 TO 7)) AND (9 WITHOUT (5 AND 6)))` |
| `-2.0` | `(1 3 4 5 6 7 9)` | `(1 AND ((3 TO 7) AND (9 WITHOUT (5 AND 6))))` |
| `-3.0` | `(1 3 4 5 6 7 9 6)` | `(((1 AND (3 TO 7)) AND (9 WITHOUT 5)) AND 6)` |
| `-3.0` | `(1 3 4 5 6 7 9 6)` | `((1 AND (3 TO 7)) AND ((9 WITHOUT 5) AND 6))` |
| `-3.0` | `(1 3 4 5 6 7 9 6)` | `((1 AND ((3 TO 7) AND (9 WITHOUT 5))) AND 6)` |
| `-3.0` | `(1 3 4 5 6 7 9 6)` | `(1 AND (((3 T0 7) AND (9 WITHOUT 5)) AND 6))` |
| `-3.0` | `(1 3 4 5 6 7 9 6)` | `(1 AND ((3 T0 7) AND ((9 WITHOUT 5) AND 6)))` |
| `> (all -parses '(1 and 3 to 7 and 9 without 5 and 2))` |
| `Score` | `Semantics` | `(1 AND 3 T0 7 AND 9 WITHOUT 5 AND 2)` |
| `======` | `================` | `===================================` |
| `0.2` | `(1 3 4 6 7 9 2)` | `((1 AND (((3 T0 7) AND 9) WITHOUT 5)) AND 2)` |
| `0.2` | `(1 3 4 6 7 9 2)` | `(1 AND ((((3 T0 7) AND 9) WITHOUT 5) AND 2))` |
| `0.1` | `(1 3 4 6 7 9 2)` | `((((1 AND (3 T0 7)) AND 9) WITHOUT 5) AND 2)` |
| `0.1` | `(1 3 4 6 7 9 2)` | `(((1 AND ((3 T0 7) AND 9)) WITHOUT 5) AND 2)` |
| `-2.0` | `(1 3 4 5 6 7 9 2)` | `(((1 AND (3 T0 7)) AND (9 WITHOUT 5)) AND 2)` |
| `-2.0` | `(1 3 4 5 6 7 9 2)` | `((1 AND (3 T0 7)) AND ((9 WITHOUT 5) AND 2))` |
| `-2.0` | `(1 3 4 5 6 7 9)` | `((1 AND (3 T0 7)) AND (9 WITHOUT (5 AND 2)))` |
| `-2.0` | `(1 3 4 5 6 7 9 2)` | `((1 AND ((3 T0 7) AND (9 WITHOUT 5))) AND 2)` |
| `-2.0` | `(1 3 4 5 6 7 9 2)` | `(1 AND (((3 T0 7) AND (9 WITHOUT 5)) AND 2))` |
| `-2.0` | `(1 3 4 5 6 7 9 2)` | `(1 AND ((3 T0 7) AND ((9 WITHOUT 5) AND 2)))` |
| `-2.0` | `(1 3 4 5 6 7 9)` | `(1 AND ((3 T0 7) AND (9 WITHOUT (5 AND 2))))` |
| `-2.8` | `(1 3 4 6 7 9)` | `(1 AND (((3 T0 7) AND 9) WITHOUT (5 AND 2)))` |
| `-2.9` | `(1 3 4 6 7 9)` | `(((1 AND (3 T0 7)) AND 9) WITHOUT (5 AND 2))` |
| `-2.9` | `(1 3 4 6 7 9)` | `((1 AND ((3 T0 7) AND 9)) WITHOUT (5 AND 2))` |

![t0025](images/B9780080571157500194/t0025.png)

In each case, the preference rules are able to assign higher scores to more reasonable interpretations.
It turns out that, in each case, all the interpretations with positive scores represent the same set of numbers, while interpretations with negative scores seem worse.
Seeing all the scores in gory detail may be of academic interest, but what we really want is something to pick out the best interpretation.
The following code is appropriate for many situations.
It picks the top scorer, if there is a unique one, or queries the user if several interpretations tie for the best score, and it complains if there are no valid parses at all.
The query-user function may be useful in many applications, but note that meani ng uses it only as a default; a program that had some automatic way of deciding could supply another `tie-breaker` function to meani ng.

[ ](#){:#l0180}`(defun meaning (words &optional (tie-breaker #’query-user))`
!!!(p) {:.unnumlist}

`    "Choose the single top-ranking meaning for the words."`
!!!(p) {:.unnumlist}

`    (let* ((trees (sort (parser words) #’> :key #’tree-score))`
!!!(p) {:.unnumlist}

`           (best-score (if trees (tree-score (first trees)) 0))`
!!!(p) {:.unnumlist}

`           (best-trees (delete best-score trees`
!!!(p) {:.unnumlist}

`                               :key #’tree-score :test-not #’eql))`
!!!(p) {:.unnumlist}

`           (best-sems (delete-duplicates (mapcar #’tree-sem best-trees)`
!!!(p) {:.unnumlist}

`                                         :test #’equal)))`
!!!(p) {:.unnumlist}

`(case (length best-sems)`
!!!(p) {:.unnumlist}

`    (0 (format t “~&Sorry.
I didn’t understand that.”) nil)`
!!!(p) {:.unnumlist}

`    (1 (first best-sems))`
!!!(p) {:.unnumlist}

`    (t (funcall tie-breaker best-sems)))))`
!!!(p) {:.unnumlist}

`(defun query-user (choices &optional`
!!!(p) {:.unnumlist}

`                           (header-str “~&Please pick one:”)`
!!!(p) {:.unnumlist}

`                           (footer-str “~&Your choice?
”))`
!!!(p) {:.unnumlist}

`    "Ask user to make a choice."`
!!!(p) {:.unnumlist}

`    (format *query-io* header-str)`
!!!(p) {:.unnumlist}

`    (loop for choice in choices for i from 1 do`
!!!(p) {:.unnumlist}

`           (format *query-io* “~&~3d: ~ a” i choice))`
!!!(p) {:.unnumlist}

`    (format *query-io* footer-str)`
!!!(p) {:.unnumlist}

`    (nth (- (read) 1) choices))`
!!!(p) {:.unnumlist}

Here we see some final examples:

[ ](#){:#l0185}`> (meaning ’(1 to 5 without 3 and 4))`
!!!(p) {:.unnumlist}

`(1 2 5)`
!!!(p) {:.unnumlist}

`> (meaning ’(1 to 5 without 3 and 6))`
!!!(p) {:.unnumlist}

`(1 2 4 5 6)`
!!!(p) {:.unnumlist}

`> (meaning ’(1 to 5 without 3 and 6 shuffled))`
!!!(p) {:.unnumlist}

`(6 4 1 2 5)`
!!!(p) {:.unnumlist}

`> (meaning ’([ 1 to 5 without [ 3 and 6 ] ] reversed))`
!!!(p) {:.unnumlist}

`(5 4 2 1)`
!!!(p) {:.unnumlist}

`> (meaning ’(1 to 5 to 9))`
!!!(p) {:.unnumlist}

`Sorry.
I didn’t understand that.`
!!!(p) {:.unnumlist}

`NIL`
!!!(p) {:.unnumlist}

`> (meaning ’(1 to 5 without 3 and 7 repeat 2))`
!!!(p) {:.unnumlist}

`Please pick one:`
!!!(p) {:.unnumlist}

`   1: (12 4 5 7 12 4 5 7)`
!!!(p) {:.unnumlist}

`   2: (12 4 5 7 7)`
!!!(p) {:.unnumlist}

`Your choice?
1`
!!!(p) {:.unnumlist}

`(1 2 4 5 7 1 2 4 5 7)`
!!!(p) {:.unnumlist}

[ ](#){:#t0030}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `> (all-parses ’(1 to 5 without 3 and 7 repeat 2))` |
| `Score` | `Semantics` | `(1 TO 5 WITHOUT 3 AND 7 REPEAT 2)` |
| `==========` | `=========` | `===========================` |
| `0.3` | `(1 2 4 5 7 1 2 4 5 7)` | `((((1 TO 5) WITHOUT 3) AND 7) REPEAT 2)` |
| `0.3` | `(1 2 4 5 7 7)` | `(((1 TO 5) WITHOUT 3) AND (7 REPEAT 2))` |
| `-2.7` | `(1 2 4 5 1 2 4 5)` | `(((1 TO 5) WITHOUT (3 AND 7)) REPEAT 2)` |
| `-2.7` | `(1 2 4 5)` | `((1 TO 5) WITHOUT ((3 AND 7) REPEAT 2))` |
| `-2.7` | `(1 2 4 5)` | `((1 TO 5) WITHOUT (3 AND (7 REPEAT 2)))` |

![t0030](images/B9780080571157500194/t0030.png)

This last example points out a potential problem: I wasn’t sure what was a good scoring function for “repeat”, so I left it blank, it defaulted to 0, and we end up with two parses with the same score.
This example suggests that “repeat” should probably involve `inv-span` like the other modifiers, but perhaps other factors should be involved as well.
There can be a complicated interplay between phrases, and it is not always clear where to assign the score.
For example, it doesn’t make much sense to repeat a “without” phrase; that is, the bracketing `(x without (y repeat n))` is probably a bad one.
But the scorer for “without” nearly handles that already.
It assigns a penalty if its right argument is not a subset of its left.
Unfortunately, repeated elements are not counted in sets, so for example, the list (1 2 3 1 2 3) is a subset of (1 2 3 4).
However, we could change the scorer for “without” to test for `sub-bag-p` (not a built-in Common Lisp function) instead, and then “repeat” would not have to be concerned with that case.

## [ ](#){:#st0040}19.7 The Problem with Context-Free Phrase-Structure Rules
{:#s0040}
{:.h1hd}

The fragment of English grammar we specified in [section 19.2](#s0015) admits a variety of ungrammatical phrases.
For example, it is equally happy with both “I liked her” and “me liked she.” Only the first of these should be accepted; the second should be ruled out.
Similarly, our grammar does not state that verbs have to agree with their subjects in person and number.
And, since the grammar has no notion of meaning, it will accept sentences that are semantically anomalous (or at least unusual), such as “the table liked the man.”

There are also some technical problems with context-free grammars.
For example, it can be shown that no context-free grammar can be written to account for the language consisting of just the strings ABC, AABBCC, AAABBBCCC, and so forth, where each string has an equal number of As, Bs, and Cs.
Yet sentences roughly of that form show up (admittedly rarely) in natural languages.
An example is “Robin and Sandy loved and hated Pat and Kim, respectively.” While there is still disagreement over whether it is possible to generate natural languages with a context-free grammar, clearly it is much easier to use a more powerful grammatical formalism.
For example, consider solving the subject-predicate agreement problem.
It is possible to do this with a context-free language including categories like singular-NP, plural-NP, singular-VP, and plural-VP, but it is far easier to augment the grammatical formalism to allow passing features between constituents.

It should be noted that context-free phrase-structure rules turned out to be very useful for describing programming languages.
Starting with Algol 60, the formalism has been used under the name *Backus-NaurForm* (BNF) by computer scientists.
In this book we are more interested in natural languages, so in the next chapter we will see a more powerful formalism known as *unification grammar* that can handle the problem of agreement, as well as other difficulties.
Furthermore, *unification grammars* allow a natural way of attaching semantics to a parse.

## [ ](#){:#st0045}19.8 History and References
{:#s0045}
{:.h1hd}

There is a class of parsing algorithms known as *chart parsers* that explicitly cache partial parses and reuse them in constructing larger parses.
Earley’s algorithm (1970) is the first example, and Martin [Kay (1980)](B9780080571157500285.xhtml#bb0605) gives a good overview of the field and introduces a data structure, the *chart*, for storing substrings of a parse.
[Winograd (1983)](B9780080571157500285.xhtml#bb1395) gives a complex (five-page) specification of a chart parser.
None of these authors have noticed that one can achieve the same results by augmenting a simple (one-page) parser with memoization.
In fact, it is possible to write a top-down parser that is even more succinct.
(See [exercise 19.3](#p2455) below.)

For a general overview of natural language processing, my preferences (in order) are [Allen 1987](B9780080571157500285.xhtml#bb0030), [Winograd 1983](B9780080571157500285.xhtml#bb1395) or [Gazdar and Mellish 1989](B9780080571157500285.xhtml#bb0445).

## [ ](#){:#st0050}19.9 Exercises
{:#s0050}
{:.h1hd}

**Exercise 19.2 [m-h]** Experiment with the grammar and the parser.
Find sentences it cannot parse correctly, and try to add new syntactic rules to account for them.

**Exercise 19.3 [m-h]** The parser works in a bottom-up fashion.
Write a top-down parser, and compare it to the bottom-up version.
Can both parsers work with the same grammar?
If not, what constraints on the grammar does each parsing strategy impose?

**Exercise 19.4 [h]** Imagine an interface to a dual cassette deck.
Whereas the CD player had one assumed verb, “play,” this unit has three explicit verb forms: “record,” “play,” and “erase.” There should also be modifiers “from” and “to,” where the object of a “to” is either 1 or 2, indicating which cassette to use, and the object of a “from” is either 1 or 2, or one of the symbols PHONO, CD, or AUX.
It’s up to you to design the grammar, but you should allow input something like the following, where I have chosen to generate actual Lisp code as the meaning:

[ ](#){:#l0190}`> (meaning ’(play 1 to 5 from CD shuffled and`
!!!(p) {:.unnumlist}

`             record 1 to 5 from CD and 1 and 3 and 7 from 1))`
!!!(p) {:.unnumlist}

`(PROGN (PLAY ’(15 2 3 4) :FROM ’CD)`
!!!(p) {:.unnumlist}

`       (RECORD '(12345) :FROM 'CD)`
!!!(p) {:.unnumlist}

`       (RECORD '(1 3 7) :FROM '1))`
!!!(p) {:.unnumlist}

This assumes that the functions play and record take keyword arguments (with defaults) for : `from` and : `to`.
You could also extend the grammar to accommodate an automatic timer, with phrases like “at 3:00.”

**Exercise 19.5 [m]** In the definition of `permute`, repeated here, why is the :`test # ' eq needed?`

[ ](#){:#l0195}`(defun permute (bag)`
!!!(p) {:.unnumlist}

`   "Return a random permutation of the given input list."`
!!!(p) {:.unnumlist}

`   (if (null bag)`
!!!(p) {:.unnumlist}

`       nil`
!!!(p) {:.unnumlist}

`       (let ((e (random-elt bag)))`
!!!(p) {:.unnumlist}

`         (cons e (permute (remove e bag :count 1 :test #’eq))))))`
!!!(p) {:.unnumlist}

**Exercise 19.6 [m]** The definition of `permute` takes *O*(*n*2).
Replace it by an *O*(*n*) algorithm.

## [ ](#){:#st0055}19.10 Answers
{:#s0055}
{:.h1hd}

**Answer 19.1**

[ ](#){:#l0200}`(defun parser (words)`
!!!(p) {:.unnumlist}

`   "Return all complete parses of a list of words."`
!!!(p) {:.unnumlist}

`   (let* ((table (make-array (+ (length words) 1) :initial-element 0))`
!!!(p) {:.unnumlist}

`            (parses (parse words (length words) table)))`
!!!(p) {:.unnumlist}

`     (mapcar #’parse-tree (complete-parses parses))))`
!!!(p) {:.unnumlist}

`(defun parse (words num-words table)`
!!!(p) {:.unnumlist}

`   "Bottom-up parse.
returning all parses of any prefix of words."`
!!!(p) {:.unnumlist}

`   (unless (null words)`
!!!(p) {:.unnumlist}

`     (let ((ans (aref table num-words)))`
!!!(p) {:.unnumlist}

`       (if (not (eq ans 0))`
!!!(p) {:.unnumlist}

`           ans`
!!!(p) {:.unnumlist}

`           (setf (aref table num-words)`
!!!(p) {:.unnumlist}

`                  (mapcan #’(lambda (rule)`
!!!(p) {:.unnumlist}

`                              (extend-parse (rule-lhs rule)`
!!!(p) {:.unnumlist}

`                                            (list (firstwords))`
!!!(p) {:.unnumlist}

`                                            (rest words) nil`
!!!(p) {:.unnumlist}

`                                            (- num-words 1) table))`
!!!(p) {:.unnumlist}

`                            (lexical-rules (first words))))))))`
!!!(p) {:.unnumlist}

`(defun extend-parse (lhs rhs rem needed num-words table)`
!!!(p) {:.unnumlist}

`   "Look for the categories needed to complete the parse."`
!!!(p) {:.unnumlist}

`   (if (null needed)`
!!!(p) {:.unnumlist}

`      ;; If nothing is needed, return this parse and upward extensions`
!!!(p) {:.unnumlist}

`      (let ((parse (make-parse :tree (new-tree lhs rhs) :rem rem)))`
!!!(p) {:.unnumlist}

`        (cons parse`
!!!(p) {:.unnumlist}

`              (mapcan`
!!!(p) {:.unnumlist}

`             #’(lambda (rule)`
!!!(p) {:.unnumlist}

`                     (extend-parse (rule-lhs rule)`
!!!(p) {:.unnumlist}

`                                   (list (parse-tree parse))`
!!!(p) {:.unnumlist}

`                                    rem (rest (rule-rhs rule))`
!!!(p) {:.unnumlist}

`                                    num-words table))`
!!!(p) {:.unnumlist}

`                 (rules-starting-with lhs))))`
!!!(p) {:.unnumlist}

`       ;; otherwise try to extend rightward`
!!!(p) {:.unnumlist}

`       (mapcan`
!!!(p) {:.unnumlist}

`         #’(lambda (p)`
!!!(p) {:.unnumlist}

`             (if (eq (parse-lhs p) (first needed))`
!!!(p) {:.unnumlist}

`                  (extend-parse lhs (appendl rhs (parse-tree p))`
!!!(p) {:.unnumlist}

`                                (parse-rem p) (rest needed)`
!!!(p) {:.unnumlist}

`                                (length (parse-rem p)) table)))`
!!!(p) {:.unnumlist}

`         (parse rem num-words table))))`
!!!(p) {:.unnumlist}

It turns out that, for the Lisp system used in the timings above, this version is no faster than normal memoization.

**Answer 19.3** Actually, the top-down parser is a little easier (shorter) than the bottom-up version.
The problem is that the most straightforward way of implementing a top-down parser does not handle so-called *left recursive* rules–rules of the form `(X -> (X …))`.
This includes rules we've used, like `(NP -> (NP and NP))`.
The problem is that the parser will postulate an NP, and then postulate that it is of the form `(NP and NP)`, and that the first NPof that expression is ofthe form `(NP and NP)`, and so on.
An infinite structure of NPs is explored before even the first word is considered.

Bottom-up parsers are stymied by rules with null right-hand sides: `(X -> O)`.
Note that I was careful to exclude such rules in my grammars earlier.

[ ](#){:#l0205}`(defun parser (words &optional (cat ’s))`
!!!(p) {:.unnumlist}

`   "Parse a list of words; return only parses with no remainder."`
!!!(p) {:.unnumlist}

`   (mapcar #’parse-tree (compiete-parses (parse words cat))))`
!!!(p) {:.unnumlist}

`(defun parse (tokens start-symbol)`
!!!(p) {:.unnumlist}

`   "Parse a list of tokens, return parse trees and remainders."`
!!!(p) {:.unnumlist}

`   (if (eq (first tokens) start-symbol)`
!!!(p) {:.unnumlist}

`       (list (make-parse :tree (first tokens) :rem (rest tokens)))`
!!!(p) {:.unnumlist}

`       (mapcan #’(lambda (rule)`
!!!(p) {:.unnumlist}

`                   (extend-parse (lhs rule) nil tokens (rhs rule)))`
!!!(p) {:.unnumlist}

`                 (rules-for start-symbol))))`
!!!(p) {:.unnumlist}

`(defun extend-parse (lhs rhs rem needed)`
!!!(p) {:.unnumlist}

`   "Parse the remaining needed symbols."`
!!!(p) {:.unnumlist}

`   (if (null needed)`
!!!(p) {:.unnumlist}

`       (list (make-parse :tree (cons lhs rhs) :rem rem))`
!!!(p) {:.unnumlist}

`       (mapcan`
!!!(p) {:.unnumlist}

`         #’(lambda (p)`
!!!(p) {:.unnumlist}

`              (extend-parse lhs (append rhs (list (parse-tree p)))`
!!!(p) {:.unnumlist}

`                              (parse-rem p) (rest needed)))`
!!!(p) {:.unnumlist}

`         (parse rem (first needed)))))`
!!!(p) {:.unnumlist}

`(defun rules-for (cat)`
!!!(p) {:.unnumlist}

`   "Return all the rules with category on lhs"`
!!!(p) {:.unnumlist}

`   (find-all cat *grammar* :key #’rule-lhs))`
!!!(p) {:.unnumlist}

**Answer 19.5** If it were omitted, then : test would default `to #’eql`, and it would be possible to remove the “wrong” element from the list.
Consider the list (1.0 1.0) in an implementation where floating-point numbers are `eql` but not `eq`.
if `random-elt` chooses the first 1.0 first, then everything is satisfactory–the resuit list is the same as the input list.
However, if `random-elt` chooses the second 1.0, then the second 1.0 will be the first element of the answer, but `remove` will remove the wrong 1.0!
It will remove the first 1.0, and the final answer will be a list with two pointers to the second 1.0 and none to the first.
In other words, we could have:

[ ](#){:#l0210}` > (member (first x) (permute x) :test #’eq)`
!!!(p) {:.unnumlist}

` NIL`
!!!(p) {:.unnumlist}

**Answer 19.6**

[ ](#){:#l0215}`(defun permute (bag)`
!!!(p) {:.unnumlist}

`   "Return a random permutation of the bag."`
!!!(p) {:.unnumlist}

`   ;; It is done by converting the bag to a vector, but the`
!!!(p) {:.unnumlist}

`   ;; resuit is always the same type as the input bag.`
!!!(p) {:.unnumlist}

`   (let ((bag-copy (replace (make-array (length bag)) bag))`
!!!(p) {:.unnumlist}

`         (bag-type (if (listp bag) 'list (type-of bag))))`
!!!(p) {:.unnumlist}

`      (coerce (permute-vector!
bag-copy) bag-type)))`
!!!(p) {:.unnumlist}

`(defun permute-vector!
(vector)`
!!!(p) {:.unnumlist}

`   "Destructively permute (shuffle) the vector."`
!!!(p) {:.unnumlist}

`   (loop for i from (length vector) downto 2 do`
!!!(p) {:.unnumlist}

`         (rotatef (aref vector (- i 1))`
!!!(p) {:.unnumlist}

`                  (aref vector (random i))))`
!!!(p) {:.unnumlist}

`vector)`
!!!(p) {:.unnumlist}

The answer uses `rotatef`, a relative of `setf` that swaps 2 or more values.
That is, `(rotatef a b)` is like:

[ ](#){:#l0220}`(let ((temp a))`
!!!(p) {:.unnumlist}

`   (setf a b)`
!!!(p) {:.unnumlist}

`   (setf b temp)`
!!!(p) {:.unnumlist}

`   nil)`
!!!(p) {:.unnumlist}

Rarely, `rotatef` is used with more than two arguments, `(rotatef a b c)` is like:

[ ](#){:#l0225}`(let ((temp a))`
!!!(p) {:.unnumlist}

`   (setf a b)`
!!!(p) {:.unnumlist}

`   (setf b c)`
!!!(p) {:.unnumlist}

`   (setf c temp)`
!!!(p) {:.unnumlist}

`   nil)`
!!!(p) {:.unnumlist}

----------------------

[1](#xfn0015){:#np0015} Some erroneous expressions are underspecified and may return different results in different implementations, but we will ignore that problem.
!!!(p) {:.ftnote1}

[2](#xfn0020){:#np0020} The number of parses of sentences of this kind is the same as the number of bracketings of a arithmetic expression, or the number of binary trees with a given number of leaves.
The resulting sequence (1,2,5,14,42,…) is known as the Catalan Numbers.
This kind of ambiguity is discussed by [Church and Patil (1982)](B9780080571157500285.xhtml#bb0200) in their article *Coping with Syntactic Ambiguity, or How to Put the Block in the Box on the Table.*
!!!(p) {:.ftnote1}

