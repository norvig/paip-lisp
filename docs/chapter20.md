# Chapter 20 {docsify-ignore}
<a id='page-684'></a>

Unification Grammars 

P
P
rolog was invented because Alain Colmerauer wanted a formalism to describe the grammar 
of French. His intuition was that the combination of Horn clauses and unification 
resulted in a language that was just powerful enough to express the kinds of constraints 
that show up in natural languages, while not as powerful as, for example, full predicate calculus. 
This lack of power is important, because it enables efficient implementation of Prolog, and 
hence of the language-analysis programs built on top of it. 

Of course, Prolog has evolved and is now used for many applications besides natural language, 
but Colmerauer's underlying intuition remains a good one. This chapter shows how 
to view a grammar as a set of logic programming clauses. The clauses define what is a legal 
sentence and what isn't, without any explicit reference to the process of parsing or generation. 
The amazing thing is that the clauses can be defined in a way that leads to a very efficient 
parser. Furthermore, the same grammar can be used for both parsing and generation (at least 
in some cases). 

<a id='page-685'></a>
20. 1 Parsing as Deduction 
Here's how we could express the grammar rule "A sentence can be composed of a 
noun phrase followed by a verb phrase" in Prolog: 

(<- (S ?s) 
(NP ?np) 
(VP ?vp) 
(concat ?np ?vp ?s)) 

The variables represent strings of words. As usual, they will be implemented as lists 
of symbols. The rule says that a given string of words ? s is a sentence if there is a string 
that is noun phrase and one that is a verb phrase, and if they can be concatenated to 
form ?s. Logically, this is fine, and it would work as a program to generate random 
sentences. However, it is a very inefficient program for parsing sentences. It will 
consider all possible noun phrases and verb phrases, without regard to the input 
words. Only when it gets to the concat goal (defined on [page 411](chapter12.md#page-411)) will it test to see if 
the two constituents can be concatenated together to make up the input string. Thus, 
a better order of evaluation for parsing is: 

(<- (S ?s) 
(concat ?np ?vp ?s) 
(NP ?np) 
(VP ?vp)) 

The first version had NP and VP guessing strings to be verified by concat. In most 
grammars, there will be a very large or infinite number of NPs and VPs. This second 
version has concat guessing strings to be verified by NP and VP. If there are . words 
in the sentence, then concat can only make . -h 1 guesses, quite an improvement. 
However, it would be better still if we could in effect have concat and .. work together 
to make a more constrained guess, which would then be verified by VP. 

We have seen this type of problem before. In Lisp, the answer is to return multiple 
values. NP would be a function that takes a string as input and returns two values: 
an indication of success or failure, and a remainder string of words that have not yet 
been parsed. When the first value indicates success, then VP would be called with 
the remaining string as input. In Prolog, return values are just extra arguments. So 
each predicate will have two parameters: an input string and a remainder string. 
Following the usual Prolog convention, the output parameter comes after the input. 
In this approach, no calls to concat are necessary, no wild guesses are made, and 
Prolog's backtracking takes care of the necessary guessing: 

<a id='page-686'></a>

(<- (S ?sO ?s2) 
(NP ?sO ?sl) 
(VP ?sl ?s2)) 

This rule can be read as "The string from s0 to s2 is a sentence if there is an si such 
that the string from sq to si is a noun phrase and the string from 5i to S2 is a verb 
phrase." 

A sample query would be (? - (S (The boy ate the apple) ())). With 
suitable definitions of . . and VP, this would succeed, with the following bindings 
holding within S: 

?sO = (The boy ate the apple) 

?sl = (ate the apple) 

?s2 = () 

Another way of reading the goal (NP ?sO ?sl), for example, is as "IS the Hst ?sO 
minus the Ust ?sl a noun phrase?" In this case, ?sO minus ?sl is the Ust (The boy). 
The combination of two arguments, an input list and an output list, is often called a 
difference list, to emphasize this interpretation. More generally, the combination of an 
input parameter and output parameter is caUed an accumulator. Accumulators, particularly 
difference lists, are an important technique throughout logic programming 
and are also used in functional programming, as we saw on [page 63](chapter3.md#page-63). 

In our rule for S, the concatenation of difference lists was implicit. If we prefer, 
we could define a version of concat for difference lists and call it explicitly: 

(<- (S ?s-in ?s-rem) 
(NP ?np-in ?np-rem) 
(VP ?vp-in ?vp-rem) 

(concat ?np-in ?np-rem ?vp-in ?vp-rem ?s-in ?s-rem)) 

(<- (concat ?a ?b ?b ?c ?a ?c)) 

Because this version of concat has a different arity than the old version, they can 
safely coexist. It states the difference list equation {a -b) -\- {b -c) = {a - c). 

In the last chapter we stated that context-free phrase-structure grammar is inconvenient 
for expressing things like agreement between the subject and predicate of a 
sentence. With the Horn-clause-based grammar formalism we are developing here, 
we can add an argument to the predicates NP and VP to represent agreement. In 
English, the agreement rule does not have a big impact. For all verbs except be, the 
difference only shows up in the third-person singular of the present tense: 

<a id='page-687'></a>
Singular Plural 

first person I sleep we sleep 

second person you sleep you sleep 

third person he/she sleeps they sleep 

Thus, the agreement argument will take on one of the two values 3sg or ~3sg to 
indicate third-person-singular or not-third-person-singular. We could write: 

(<- (S ?sO ?s2) 
(NP ?agr ?sO ?sl) 
(VP ?agr ?sl ?s2)) 

(<- (NP 3sg (he . ?s) ?s)) 
(<- (NP ~3sg (they . ?s) ?$)) 

(<- (VP 3sg (sleeps . ?s) ?s)) 
(<- (VP ~3sg (sleep . Is) Is)) 

This grammar parses just the right sentences: 

> (?- (S (He sleeps) ())) 
Yes. 

> (?- (S (He sleep) ())) 
No. 

Let's extend the grammar to allow common nouns as well as pronouns: 

(<- (NP ?agr ?sO ?s2) 
(Det ?agr ?sO ?sl) 
(N ?agr ?sl ?s2)) 

(<- (Det ?any (the . ?s) ?s)) 
(<- (N 3sg (boy . Is) Is)) 
(<- (N 3sg (girl . ?s) ?s)) 

The same grammar rules can be used to generate sentences as well as parse. Here 
are all possible sentences in this trivial grammar: 

> (?- (S ?words ())) 
7W0RDS = (HE SLEEPS); 
7W0RDS = (THEY SLEEP); 
?WORDS = (THE BOY SLEEPS); 
7W0RDS = (THE GIRL SLEEPS); 
No. 

So far all we have is a recognizer: a predicate that can separate sentences from 

<a id='page-688'></a>

nonsentences. But we can add another argument to each predicate to build up the 
semantics. The result is not just a recognizer but a true parser: 

(<- (S (?pred ?subj) ?sO ?s2) 

(NP ?agr ?subj ?sO ?sl) 

(VP ?agr ?pred ?sl ?s2)) 

(<- (NP 3sg (the male) (he . ?s) ?s)) 
(<- (NP ~3sg (some objects) (they . ?s) ?s)) 

(<- (NP ?agr (?det ?n) ?sO ?s2) 

(Det ?agr ?det ?sO ?sl) 

(N ?agr ?n ?sl ?s2)) 

(<- (VP 3sg sleep (sleeps . ?s) ?s)) 
(<- (VP ~3sg sleep (sleep . ?s) ?s)) 

(<- (Det ?any the (the . ?s) ?s)) 
(<- (N 3sg (young male human) (boy . ?s) ?s)) 
(<- (N 3sg (young female human) (girl . ?s) ?s)) 

The semantic translations of individual words is a bit capricious. In fact, it is not too 
important at this point if the translation of boy is (young mal e human) or just boy. 
There are two properties of a semantic representation that are important. First, it 
should be unambiguous. The representation of orange the fruit should be different 
from orange the color (although the representation of the fruit might well refer to 
the color, or vice versa). Second, it should express generalities, or allow them to 
be expressed elsewhere. So either sleep and sleeps should have the same or similar 
representation, or there should be an inference rule relating them. Similarly, if the 
representation of boy does not say so explicitly, there should be some other rule 
saying that a boy is a male and a human. 

Once the semantics of individual words is decided, the semantics of higher-level 
categories (sentences and noun phrases) is easy. In this grammar, the semantics of 
a sentence is the application of the predicate (the verb phrase) to the subject (the 
noun phrase). The semantics of a compound noun phrase is the application of the 
determiner to the noun. 

This grammar returns the semantic interpretation but does not build a syntactic 
tree. The syntactic structure is implicit in the sequence of goals: S calls NP and VP, 
and . . can call Det and N. If we want to make this explicit, we can provide yet another 
argument to each nonterminal: 

(<- (S (?pred ?subj) (s ?np ?vp)?sO ?s2) 
(NP ?agr ?subj ?np ?sO ?sl) 
(VP ?agr ?pred ?vp ?sl ?s2)) 

(<- (NP 3sg (the male) (np he) (he . Is) ?s)) 
(<- (NP ~3sg (some objects) (np they) (they . ?s) ?s)) 

<a id='page-689'></a>
(<- (NP ?agr (?det ?n) (np ?det-syn ?n-syn) ?sO ?s2) 
(Det ?agr ?det ?det-syn ?sO ?sl) 
(N ?agr ?n ?n-syn ?sl ?s2)) 

(<- (VP 3sg sleep (vp sleeps) (sleeps . ?s) ?s)) 
(<- (VP ""Ssg sleep (vp sleep) (sleep . ?s) ?s)) 

(<- (Det ?any the (det the) (the . ?s) ?s)) 
(<- (N 3sg (young male human) (n boy) (boy . ?s) ?s)) 
(<- (N 3sg (young female human) (n girl) (girl . ?s) ?s)) 

This grammar can still be used to parse or generate sentences, or even to enumerate 
all syntax/semantics/sentence triplets: 

Parsing: 
> (?- (S ?sem ?syn (He sleeps) ())) 
?SEM = (SLEEP (THE MALE)) 
?SYN = (S (NP HE) (VP SLEEPS)). 

Generating: 
> (?- (S (sleep (the male)) ? ?words ())) 
7W0RDS = (HE SLEEPS) 

Enumerating: 
> (?- (S ?sem ?syn ?words ())) 
?SEM = (SLEEP (THE MALE)) 
?SYN = (S (NP HE) (VP SLEEPS)) 
?WORDS = (HE SLEEPS); 

?SEM = (SLEEP (SOME OBJECTS)) 

?SYN = (S (NP THEY) (VP SLEEP)) 

7W0RDS = (THEY SLEEP); 

?SEM = (SLEEP (THE (YOUNG MALE HUMAN))) 
?SYN = (S (NP (DET THE) (N BOY)) (VP SLEEPS)) 
7W0RDS = (THE BOY SLEEPS); 

?SEM = (SLEEP (THE (YOUNG FEMALE HUMAN))) 
?SYN = (S (NP (DET THE) (N GIRD) (VP SLEEPS)) 
7W0RDS = (THE GIRL SLEEPS); 

No. 

20.2 Definite Clause Grammars 
We now have a powerful and efficient tool for parsing sentences. However, it is 
getting to be a very messy tool - there are too many arguments to each goal, and it 

<a id='page-690'></a>

is hard to tell which arguments represent syntax, which represent semantics, which 
represent in/out strings, and which represent other features, like agreement. So, 
we will take the usual step when our bare programming language becomes messy: 
define a new language. 

Edinburgh Prolog recognizes assertions called definite clause grammar (DCG) rules. 
The term definite clause is just another name for a Prolog clause, so DCGs are also 
called "logic grammars." They could have been called "Horn clause grammars" or 
"Prologgrammars" as well. 

DCG rules are clauses whose main functor is an arrow, usually written - ->. They 
compile into regular Prolog clauses with extra arguments. In normal DCG rules, only 
the string arguments are automatically added. But we will see later how this can be 
extended to add other arguments automatically as well. 

We will implement DCG rules with the macro rule and an infix arrow. Thus, we 
want the expression: 

(rule (S) --> (NP) (VP)) 

to expand into the clause: 

(<- (S ?sO ?s2) 

(NP ?sO ?sl) 

(VP ?sl ?s2)) 

While we're at it, we may as well give rul e the ability to deal with different types of 
rules, each one represented by a different type of arrow. Here's the rul e macro: 

(defmacro rule (head &optional (arrow *:-) &body body) 
"Expand one of several types of logic rules into pure Prolog." 
This is data-driven, dispatching on the arrow 
(funcall (get arrow 'rule-function) head body)) 

As an example of a rule function, the arrow: - will be used to represent normal Prolog 
clauses. That is, the form (rul e head : -body) will be equivalent to (<-head body). 

(setf (get *:- 'rule-function) 
#'(lambda (head body) .(<- .head .,body))) 

Before writing the rule function for DCG rules, there are two further features of the 
DCG formalism to consider. First, some goals in the body of a rule may be normal 
Prolog goals, and thus do not require the extra pair of arguments. In Edinburgh 
Prolog, such goals are surrounded in braces. One would write: 

<a id='page-691'></a>
s(Sem) --> np(Subj), vp(Pred). 
{combi ne(Subj,Pred. Sem)}. 

where the idea is that combi ne is riot a grammatical constituent, but rather a Prolog 
predicate that could do some calculations on Subj and Pred to arrive at the proper 
semantics, Sem. We will mark such a test predicate not by brackets but by a list 
headed by the keyword : test, as in: 

(rule (S ?sem) --> (NP ?subj) (VP ?pred) 
(:test (combine ?subj ?pred ?sem))) 

Second, we need some way of introducing individual words on the right-hand side, 
as opposed to categories of words. In Prolog, brackets are used to represent a word 
or Ust of words on the right-hand side: 

verb --> [sleeps]. 

We will use a list headed by the keyword : word: 

(rule (NP (the male) 3sg) --> (:word he)) 
(rule (VP sleeps 3sg) --> (:word sleeps)) 

The following predicates test for these two special cases. Note that the cut is also 
allowed as a normal goal. 

(defun dcg-normal-goal-p (x) (or (starts-with . :test) (eq . '!))) 
(defun dcg-word-list-p (x) (starts-with . 'iword)) 

At last we are in a position to present the rule function for DCG rules. The function 
make-deg inserts variables to keep track of the strings that are being parsed. 

(setf (get '--> 'rule-function) 'make-dcg) 

(defun make-dcg (head body) 
(let ((n (count-if (complement #'dcg-normal-goal-p) body))) 
.(<- (,@head ?sO .(symbol *?s n)) 
.,(make-dcg-body body 0)))) 

<a id='page-692'></a>

(defun make-dcg-body (body n) 
"Make the body of a Definite Clause Grammar (DCG) clause. 
Add ?string-in and -out variables to each constituent. 
Goals like (:test goal) are ordinary Prolog goals, 
and goals like (:word hello) are literal words to be parsed." 
(if (null body) 

nil 
(let ((goal (first body))) 

(cond 
((eq goal '!) (cons . (make-dcg-body (rest body) n))) 
((dcg-normal-goal-p goal) 

(append (rest goal) 
(make-dcg-body (rest body) n))) 
((dcg-word-list-p goal) 
(cons 
'(= .(symbol 'Is n) 
(.(rest goal) ..(symbol '?s (+ . 1)))) 
(make-dcg-body (rest body) (+ . 1)))) 
(t (cons 
(append goal 
(list (symbol '?s n) 
(symbol '?s (+ . 1)))) 
(make-dcg-body (rest body) (+ . 1)))))))) 

&#9635; Exercise 20.1 [m] make - dcg violates one of the cardinal rules of macros. What does 
it do wrong? How would you fix it? 

20.3 A Simple Grammar in DCG Format 
Here is the trivial grammar from [page 688](chapter20.md#page-688) in DCG format. 

(rule (S (?pred ?subj)) --> 
(NP ?agr ?subj) 
(VP ?agr ?pred)) 

(rule (NP ?agr (?det ?n)) --> 
(Det ?agr ?det) 
(N ?agr ?n)) 

<a id='page-693'></a>

(rule (NP 3sg (the male)) --> (:word he)) 

(rule (NP ~3sg (some objects)) --> (:word they)) 

(rule (VP 3sg sleep) --> (:word sleeps)) 

(rule (VP ~3sg sleep) --> (:word sleep)) 

(rule (Det ?any the) --> (:word the)) 

(rule (N 3sg (young male human)) --> (:word boy)) 

(rule (N 3sg (young female human)) --> (:word girl)) 

This grammar is quite limited, generating only four sentences. The first way we will 
extend it is to allow verbs with objects: in addition to "The boy sleeps," we will allow 
"The boy meets the girl." To avoid generating ungrammatical sentences like "* The 
boy meets,"^ we will separate the category of verb into two subcategories: transitive 
verbs, which take an object, and intransitive verbs, which don't. 

Transitive verbs complicate the semantic interpretation of sentences. We would 
liketheinterpretationof "Terry kisses Jean" tobe (kiss Terry Jean). The interpretation 
of the noun phrase "Terry" is just Te r ry, but then what should the interpretation 
of the verb phrase "kisses Jean" be? To fit our predicate application model, it must 
be something equivalent to (lambda (x) (kiss . Jean)). When applied to the 
subject, we want to get the simplification: 

((lambda (x) (kiss . Jean)) Terry) => (kiss Terry Jean) 

Such simplification is not done automatically by Prolog, but we can write a predicate 
to do it. We will call it funcall, because it is similar to the Lisp function of that name, 
although it only handles replacement of the argument, not full evaluation of the 
body. (Technically, this is the lambda-calculus operation known as beta-reduction.) 
The predicate funcall is normally used with two input arguments, a function and its 
argument, and one output argument, the resulting reduction: 

(<- (funcall (lambda (?x) ?body) ?x ?body)) 

With this we could write our rule for sentences as: 

(rule (S ?sem) --> 
(NP ?agr ?subj) 
(VP ?agr ?pred) 
(:test (funcall ?pred ?subj ?sem))) 

An alternative is to, in effect, compile away the call to funcall. Instead of having the 
semantic representation of VP be a single lambda expression, we can represent it as 

^The asterisk at the start of a sentence is the standard linguistic notation for an utterance 
that is ungrammatical or otherwise ill-formed. 

<a id='page-694'></a>

two arguments: an input argument, ?subj, which acts as a parameter to the output 
argument, ?pred, which takes the place of the body of the lambda expression. By 
explicitly manipulating the parameter and body, we can eliminate the call to funcall. 
The trick is to make the parameter and the subject one and the same: 

(rule (S ?pred) --> 
(NP ?agr ?subj) 
(VP ?agr ?subj ?pred)) 

One way of reading this rule is "To parse a sentence, parse a noun phrase followed 
bya verb phrase. If they have different agreement features then fail, but otherwise 
insert the interpretation of the noun phrase, ?subj, into the proper spot in the 
interpretation of the verb phrase, ?pred, and return ?pred as the final interpretation 
of the sentence." 

The next step is to write rules for verb phrases and verbs. Transitive verbs are 
Usted under the predicate Verb/tr, and intransitive verbs are Usted as Verb/intr. 
The semantics of tenses (past and present) has been ignored. 

(rule (VP ?agr ?subj ?pred) --> 
(Verb/tr ?agr ?subj ?pred ?obj) 
(NP ?any-agr ?obj)) 

(rule (VP ?agr ?subj ?pred) --> 
(Verb/intr ?agr ?subj ?pred)) 

(rule (Verb/tr ~3sg ?x (kiss ?x ?y) ?y) --> (iword kiss)) 
(rule (Verb/tr 3sg ?x (kiss ?x ?y) ?y) --> (:word kisses)) 
(rule (Verb/tr ?any ?x (kiss ?x ?y) ?y) --> (:word kissed)) 

(rule (Verb/intr ~3sg ?x (sleep ?x)) --> (iword sleep)) 
(rule (Verb/intr 3sg ?x (sleep ?x)) --> (iword sleeps)) 
(rule (Verb/intr ?any ?x (sleep ?x)) --> (:word slept)) 

Here are the rules for noun phrases and nouns: 

(rule (NP ?agr ?sem) --> 
(Name ?agr ?sem)) 

(rule (NP ?agr (?det-sem ?noun-sem)) --> 
(Det ?agr ?det-sem) 
(Noun ?agr ?noun-sem)) 

(rule (Name 3sg Terry) --> (iword Terry)) 
(rule (Name 3sg Jean) --> (iword Jean)) 

<a id='page-695'></a>
(rule (Noun 3sg (young male human)) --> (:word boy)) 
(rule (Noun 3sg (young female human)) --> (rword girl)) 
(rule (Noun ~3sg (group (young male human))) --> (:word boys)) 
(rule (Noun ~3sg (group (young female human))) --> (:word girls)) 

(rule (Det ?any the) --> (:word the)) 
(rule (Det 3sg a) --> (rword a)) 

This grammar and lexicon generates more sentences, although it is still rather limited. 
Here are some examples: 

> (?- (S ?sem (The boys kiss a girl) ())) 
?SEM = (KISS (THE (GROUP (YOUNG MALE HUMAN))) 
(A (YOUNG FEMALE HUMAN))). 

> (?- (S ?sem (The girls kissed the girls) ())) 
?SEM = (KISS (THE (GROUP (YOUNG FEMALE HUMAN))) 
(THE (GROUP (YOUNG FEMALE HUMAN)))). 

> (?- (S ?sem (Terry kissed the girl) ())) 
?SEM = (KISS TERRY (THE (YOUNG FEMALE HUMAN))). 

> (?- (S ?sem (The girls kisses the boys) ())) 
No. 

> (?- (S ?sem (Terry kissed a girls) ())) 
No. 

> (?- (S ?sem (Terry sleeps Jean) ())) 
No. 

The first three examples are parsed correctly, while the final three are correctly 
rejected. The inquisitive reader may wonder just what is going on in the interpretation 
of a sentence like "The girls kissed the girls." Do the subject and object represent the 
same group of girls, or different groups? Does everyone kiss everyone, or are there 
fewer kissings going on? Until we define our representation more carefully, there is no 
way to tell. Indeed, it seems that there is a potential problem in the representation, in 
that the predicate ki ss sometimes has individuals as its arguments, and sometimes 
groups. More careful representations of "The girls kissed the girls" include the 
following candidates, using predicate calculus: 

VxVy xegirls . yegirls => kiss(x,y) 
VxVy xegirls . yegirls . x^^y => kiss(x,y) 
Vx3y,z xegirls . yegirls . zegirls => kiss(x,y) . kiss(z,x) 
Vx3y xegirls . yegirls => kiss(x,y) V kiss(y,x) 

The first of these says that every girl kisses every other girl. The second says the same 
thing, except that a girl need not kiss herself. The third says that every girl kisses 

<a id='page-696'></a>

and is kissed by at least one other girl, but not necessarily all of them, and the fourth 
says that everbody is in on at least one kissing. None of these interpretations says 
anything about who "the girls" are. 

Clearly, the predicate calculus representations are less ambiguous than the representation 
produced by the current system. On the other hand, it would be wrong 
to choose one of the representations arbitrarily, since in different contexts, "The girls 
kissed the girls" can mean different things. Maintaining ambiguity in a concise form 
is useful, as long as there is some way eventually to recover the proper meaning. 

20.4 A DCG Grammar with Quantifiers 
The problem in the representation we have been using becomes more acute when we 
consider other determiners, such as "every." Consider the sentence "Every picture 
paints a story." The preceding DCG, if given the right vocabulary, would produce 
the interpretation: 

(paints (every picture) (a story)) 

This can be considered ambiguous between the following two meanings, in predicate 
calculus form: 

VX picture(x) 3 y story(y) . paint(x,y) 
3 y story(y) . V . picture(x) => paint(x,y) 

The first says that for each picture, there is a story that it paints. The second says that 
there is a certain special story that every picture paints. The second is an unusual 
interpretation for this sentence, but for "Every U.S. citizen has a president," the 
second interpretation is perhaps the preferred one. In the next section, we will see 
how to produce representations that can be transformed into either interpretation. 
For now, it is a useful exercise to see how we could produce just the first representation 
above, the interpretation that is usually correct. First, we need to transcribe it into 
Lisp: 

(all ?x (-> (picture ?x) (exists ?y (and (story ?y) (paint ?x ?y))))) 

The first question is how the a 11 and exi sts forms get in there. They must come from 
the determiners, "every" and "a." Also, it seems that a 11 is followed by an implication 
arrow, ->, while exi sts is followed by a conjunction, and. So the determiners will 
have translations looking like this: 

<a id='page-697'></a>
(rule (Det ?any ?x ?p ?q (the ?x (and ?p ?q))) --> (:word the)) 
(rule (Det 3sg ?x ?p ?q (exists ?x (and ?p ?q))) --> (:word a)) 
(rule (Det 3sg ?x ?p ?q (all ?x (-> ?p ?q))) --> (:word every)) 

Once we have accepted these translations of the determiners, everything else follows. 
The formulas representing the determiners have two holes in them, ?p and ?q. The 
first will be filled by a predicate representing the noun, and the latter will be filled 
by the predicate that is being applied to the noun phrase as a whole. Notice that a 
curious thing is happening. Previously, translation to logical form was guided by 
the sentence's verb. Linguisticly, the verb expresses the main predicate, so it makes 
sense that the verb's logical translation should be the main part of the sentence's 
translation. In linguistic terms, we say that the verb is the head of the sentence. 

With the new translations for determiners, we are in effect turning the whole 
process upside down. Now the subject's determiner carries the weight of the whole 
sentence. The determiner's interpretation is a function of two arguments; it is applied 
to the noun first, yielding a function of one argument, which is in turn applied to the 
verb phrase's interpretation. This primacy of the determiner goes against intuition, 
but it leads directly to the right interpretation. 

The variables ?p and ?q can be considered holes to be filled in the final interpretation, 
but the variable ?x fills a quite different role. At the end of the parse, ?x will 
not be filled by anything; it will still be a variable. But it will be referred to by the 
expressions filling ?p and ?q. We say that ?x is a metavariable, because it is a variable 
in the representation, not a variable in the Prolog implementation. It just happens 
that Prolog variables can be used to implement these metavariables. 

Here are the interpretations for each word in our target sentence and for each 
intermediate constituent: 

Every = (all ?x (-> ?pl ?ql)) 
picture = (picture ?x) 
paints = (paint ?x ?y) 
a = (exists ?y (and ?p2 ?q2)) 
story = (story ?y) 

Every picture = (all ?x (-> (picture ?x) ?ql)) 
a story = (exists ?y (and (story ?y) ?q2)) 
paints a story = (exists ?y (and (story ?y) (paint ?x ?y))) 

The semantics of a noun has to fill the ?p hole of a determiner, possibly using the 
metavariable ?x. The three arguments to the Noun predicate are the agreement, the 
metavariable ?x, and the assertion that the noun phrase makes about ?x: 

<a id='page-698'></a>

(rule (Noun 3sg ?x (picture ?x)) --> (:word picture)) 
(rule (Noun 3sg ?x (story ?x)) --> (:word story)) 
(rule (Noun 3sg ?x (and (young ?x) (male ?x) (human ?x))) --> 

(iword boy)) 

The NP predicate is changed to take four arguments. First is the agreement, then 
the metavariable ?x. Third is a predicate that will be supplied externally, by the verb 
phrase. The final argument returns the interpretation of the NP as a whole. As we 
have stated, this comes from the determiner: 

(rule (NP ?agr ?x ?pred ?pred) --> 
(Name ?agr ?name)) 

(rule (NP ?agr ?x ?pred ?np) --> 
(Det ?agr ?x ?noun ?pred ?np) 
(Noun ?agr ?x ?noun)) 

The rule for an NP with determiner is commented out because it is convenient to 
introduce an extended rule to replace it at this point. The new rule accounts for 
certain relative clauses, such as "the boy that paints a picture": 

(rule (NP ?agr ?x ?pred ?np) --> 
(Det ?agr ?x ?noun&rel ?pred ?np) 
(Noun ?agr ?x ?noun) 
(rel-clause ?agr ?x ?noun ?noun&rel)) 

(rule (rel-clause ?agr ?x ?np ?np) --> ) 

(rule (rel-clause ?agr ?x ?np (and ?np ?rel)) --> 
(iword that) 
(VP ?agr ?x ?rel)) 

The new rule does not account for relative clauses where the object is missing, such 
as "the picture that the boy paints." Nevertheless, the addition of relative clauses 
means we can now generate an infinite language, since we can always introduce a 
relative clause, which introduces a new noun phrase, which in turn can introduce 
yet another relative clause. 

The rules for relative clauses are not complicated, but they can be difficult to 
understand. Of the four arguments to rel -clause, the first two hold the agreement 
features of the head noun and the metavariable representing the head noun. 
The last two arguments are used together as an accumulator for predications about 
the metavariable: the third argument holds the predications made so far, and the 
fourth will hold the predications including the relative clause. So, the first rule for 
rel -cl ause says that if there is no relative clause, then what goes in to the accumulator 
is the same as what goes out. The second rule says that what goes out is the 
conjunction of what comes in and what is predicated in the relative clause itself. 

<a id='page-699'></a>
Verbs apply to either one or two metavariables, just as they did before. So we can 
use the definitions of Verb/tr and Verb/i ntr unchanged. For variety, I've added a 
few more verbs: 

(rule (Verb/tr ~3sg ?x ?y (paint ?x ?y)) --> (rword paint)) 
(rule (Verb/tr 3sg ?x ?y (paint ?x ?y)) --> (iword paints)) 
(rule (Verb/tr ?any ?x ?y (paint ?x ?y)) --> (.-word painted)) 

(rule (Verb/intr ''3sg ?x (sleep ?x)) --> (:word sleep)) 
(rule (Verb/intr 3sg ?x (sleep ?x)) --> (:word sleeps)) 
(rule (Verb/intr ?any ?x (sleep ?x)) --> (:word slept)) 

(rule (Verb/intr 3sg ?x (sells ?x)) --> (:word sells)) 

(rule (Verb/intr 3sg ?x (stinks ?x)) --> (:word stinks)) 

Verb phrases and sentences are almost as before. The only difference is in the call to 
NP, which now has extra arguments: 

(rule (VP ?agr ?x ?vp) --> 
(Verb/tr ?agr ?x ?obj ?verb) 
(NP ?any-agr ?obj ?verb ?vp)) 

(rule (VP ?agr ?x ?vp) --> 
(Verb/intr ?agr ?x ?vp)) 

(rule (S ?np) --> 
(NP ?agr ?x ?vp ?np) 
(VP ?agr ?x ?vp)) 

With this grammar, we get the following correspondence between sentences and 
logical forms: 

Every picture paints a story. 
(ALL ?3 (-> (PICTURE ?3) 
(EXISTS ?14 (AND (STORY ?14) (PAINT ?3 ?14))))) 

Every boy that paints a picture sleeps. 
(ALL ?3 (-> (AND (AND (YOUNG ?3) (MALE ?3) (HUMAN ?3)) 
(EXISTS ?19 (AND (PICTURE ?19) 
(PAINT ?3 ?19)))) 
(SLEEP ?3))) 

Every boy that sleeps paints a picture. 
(ALL ?3 (-> (AND (AND (YOUNG ?3) (MALE ?3) (HUMAN ?3)) 
(SLEEP ?3)) 
(EXISTS ?22 (AND (PICTURE ?22) (PAINT ?3 ?22))))) 

<a id='page-700'></a>

Every boy that paints a picture that sells 
paints a picture that stinks. 
(ALL ?3 (-> (AND (AND (YOUNG ?3) (MALE ?3) (HUMAN ?3)) 

(EXISTS ?19 (AND (AND (PICTURE ?19) (SELLS ?19)) 
(PAINT ?3 ?19)))) 
(EXISTS ?39 (AND (AND (PICTURE ?39) (STINKS ?39)) 
(PAINT ?3 ?39))))) 

20.5 Preserving Quantifier Scope Ambiguity 
Consider the simple sentence "Every man loves a woman." This sentence is ambiguous 
between the following two interpretations: 

Vm3w man(m) . woman(w) . loves(m,w) 
3wVm man(m) . woman(w) . Ioves(m,w) 

The first interpretation is that every man loves some woman - his wife, perhaps. 
The second interpretation is that there is a certain woman whom every man loves - 
Natassja Kinski, perhaps. The meaning of the sentence is ambiguous, but the structure 
is not; there is only one syntactic parse. 

In the last section, we presented a parser that would construct one of the two 
interpretations. In this section, we show how to construct a single interpretation 
that preserves the ambiguity, but can be disambiguated by a postsyntactic process. 
The basic idea is to construct an intermediate logical form that leaves the scope of 
quantifiers unspecified. This intermediate form can then be rearranged to recover 
the final interpretation. 

To recap, here is the interpretation we would get for "Every man loves a woman," 
given the grammar in the previous section: 

(all ?m (-> (man ?m) (exists ?w) (and (woman ?w) (loves ?m ?w)))) 

We will change the grammar to produce instead the intermediate form: 

(and (all ?m (man ?m)) 
(exists ?w (wowan ?w)) 
(loves ?m ?w)) 

The difference is that logical components are produced in smaller chunks, with 
unscoped quantifiers. The typical grammar rule will build up an interpretation by 
conjoining constituents with and, rather than by fitting pieces into holes in other 

<a id='page-701'></a>
pieces. Here is the complete grammar and a just-large-enough lexicon in the new 
format: 

(rule (S (and ?np ?vp)) --> 
(NP ?agr ?x ?np) 
(VP ?agr ?x ?vp)) 

(rule (VP ?agr ?x (and ?verb ?obj)) --> 
(Verb/tr ?agr ?x ?o ?verb) 
(NP ?any-agr ?o ?obj)) 

(rule (VP ?agr ?x ?verb) --> 
(Verb/intr ?agr ?x ?verb)) 

(rule (NP ?agr ?name t) --> 
(Name ?agr ?name)) 

(rule (NP ?agr ?x ?det) --> 
(Det ?agr ?x (and ?noun ?rel) ?det) 
(Noun ?agr ?x ?noun) 
(rel-clause ?agr ?x ?rel)) 

(rule (rel-clause ?agr ?x t) --> ) 

(rule (rel-clause ?agr ?x ?rel) --> 
(:word that) 
(VP ?agr ?x ?rel)) 

(rule (Name 3sg Terry) --> (:word Terry)) 
(rule (Name 3sg Jean) --> (:word Jean)) 
(rule (Det 3sg ?x ?restr (all ?x ?restr)) --> (:word every)) 
(rule (Noun 3sg ?x (man ?x)) --> (:word man)) 
(rule (Verb/tr 3sg ?x ?y (love ?x ?y)) --> (iword loves)) 
(rule (Verb/intr 3sg ?x (lives ?x)) --> (iword lives)) 
(rule (Det 3sg ?x ?res (exists ?x ?res)) --> (iword a)) 
(rule (Noun 3sg ?x (woman ?x)) --> (iword woman)) 

This gives us the following parse for "Every man loves a woman": 

(and (all ?4 (and (man ?4) t)) 
(and (love ?4 ?12) (exists ?12 (and (woman ?12) t)))) 

If we simplified this, eliminating the ts and joining ands, we would get the desired 
representation: 

(and (all ?m (man ?m)) 
(exists ?w (wowan ?w)) 
(loves ?m ?w)) 

From there, we could use what we know about syntax, in addition to what we know 

<a id='page-702'></a>

about men, woman, and loving, to determine the most likely final interpretation. 
This will be covered in the next chapter. 

20.6 Long-Distance Dependencies 
So far, every syntactic phenomena we have considered has been expressible in a 
rule that imposes constraints only at a single level. For example, we had to impose 
the constraint that a subject agree with its verb, but this constraint involved two 
immediate constituents of a sentence, the noun phrase and verb phrase. We didn't 
need to express a constraint between, say, the subject and a modifier of the verb's 
object. However, there are linguistic phenomena that require just these kinds of 
constraints. 

Our rule for relative clauses was a very simple one: a relative clause consists of the 
word "that" followed by a sentence that is missing its subject, as in "every man that 
loves a woman." Not all relative clauses follow this pattern. It is also possible to form 
a relative clause by omitting the object of the embedded sentence: "every man that a 
woman loves In this sentence, the symbol u indicates a gap, which is understood 
as being filled by the head of the complete noun phrase, the man. This has been 
called a filler-gap dependency. It is also known as a long-distance dependency, because 
the gap can occur arbitrarily far from the filler. For example, all of the following are 
valid noun phrases: 

The person that Lee likes u 

The person that Kim thinks Lee likes ' 

The person that Jan says Kim thinks Lee likes u 

In each case, the gap is filled by the head noun, the person. But any number of relative 
clauses can intervene between the head noun and the gap. 

The same kind of filler-gap dependency takes place in questions that begin with 
"who," "what," "where," and other interrogative pronouns. For example, we can ask 
a question about the subject of a sentence, as in "Who likes Lee?", or about the object, 
as in "Who does Kim like '?" 

Here is a grammar that covers relative clauses with gapped subjects or objects. 
The rules for S, VP, and .. are augmented with a pair of arguments representing 
an accumulator for gaps. Like a difference list, the first argument minus the second 
represents the presence or absence of a gap. For example, in the first two rules for 
noun phrases, the two arguments are the same, ?gO and ?gO. This means that the rule 
as a whole has no gap, since there can be no difference between the two arguments. 
In the third rule for NP, the first argument is of the form (gap ...), and the second 
is nogap. This means that the right-hand side of the rule, an empty constituent, can 
be parsed as a gap. (Note that if we had been using true difference lists, the two 

<a id='page-703'></a>

arguments would be ((gap ...) ?gO) and ?gO. But since we are only dealing with 
one gap per rule, we don't need true difference lists.) 

The rule for S says that a noun phrase with gap ?gO minus ?gl followed by a verb 
phrase with gap ?gl minus ?g2 comprise a sentence with gap ?gO minus ?g2. The 
rule for relative clauses finds a sentence with a gap anywhere; either in the subject 
position or embedded somewhere in the verb phrase. Here's the complete grammar: 

(rule (S ?gO ?g2 (and ?np ?vp)) --> 
(NP ?gO ?gl ?agr ?x ?np) 
(VP ?gl ?g2 ?agr ?x ?vp)) 

(rule (VP ?gO ?gl ?agr ?x (and ?obj ?verb)) --> 
(Verb/tr ?agr ?x ?o ?verb) 
(NP ?gO ?gl ?any-agr ?o ?obj)) 

(rule (VP ?gO ?gO ?agr ?x ?verb) --> 
(Verb/intr ?agr ?x ?verb)) 

(rule (NP ?gO ?gO ?agr ?name t) --> 
(Name ?agr ?name)) 

(rule (NP ?gO ?gO ?agr ?x ?det) --> 
(Det ?agr ?x (and ?noun ?rel) ?det) 
(Noun ?agr ?x ?noun) 
(rel-clause ?agr ?x ?rel)) 

(rule (NP (gap NP ?agr ?x) nogap ?agr ?x t) --> ) 

(rule (rel-clause ?agr ?x t) --> ) 

(rule (rel-clause ?agr ?x ?rel) --> 
(:word that) 

(S (gap NP ?agr ?x) nogap ?rel)) 

Here are some sentence/parse pairs covered by this grammar: 
Every man that ' loves a woman likes a person. 
(AND (ALL ?28 (AND (MAN ?28) 
(AND . (AND (LOVE ?28 ?30) 
(EXISTS ?30 (AND (WOMAN ?30) 
T)))))) 
(AND (EXISTS ?39 (AND (PERSON ?39) T)) (LIKE ?28 ?39))) 

Every man that a woman loves yUkes a person. 
(AND (ALL ?37 (AND (MAN ?37) 
(AND (EXISTS ?20 (AND (WOMAN ?20) T)) 
(AND . (LOVE ?20 137))))) 
(AND (EXISTS ?39 (AND (PERSON ?39) T)) (LIKE ?37 ?39))) 

<a id='page-704'></a>

Every man that loves a bird that u^Hes likes a person. 
(AND (ALL ?28 (AND (MAN ?28) 
(AND . (AND (EXISTS ?54 
(AND (BIRD ?54) 
(AND . (FLY ?54)))) 
(LOVE ?28 ?54))))) 
(AND (EXISTS ?60 (AND (PERSON ?60) T)) (LIKE ?28 ?60))) 

Actually, there are limitations on the situations in which gaps can appear. In particular, 
it is rare to have a gap in the subject of a sentence, except in the case of a relative 
clause. In the next chapter, we will see how to impose additional constraints on gaps. 

20.7 Augmenting DCG Rules 
In the previous section, we saw how to build up a semantic representation of a 
sentence by conjoining the semantics of the components. One problem with this 
approach is that the semantic interpretation is often something of the form (and 
(and t a) when we would prefer (and ab). There are two ways to correct 
this problem: either we add a step that takes the final semantic interpretation and 
simplifies it, or we complicate each individual rule, making it generate the simplified 
form. The second choice would be slightly more efficient, but would be very ugly 
and error prone. We should be doing all we can to make the rules simpler, not more 
complicated; that is the whole point of the DCG formalism. This suggests a third 
approach: change the rule interpreter so that it automatically generates the semantic 
interpretation as a conjunction of the constituents, unless the rule explicitly says 
otherwise. This section shows how to augment the DCG rules to handle common 
cases like this automatically. 

Consider again a rule from section 20.4: 

(rule (S (and ?np ?vp))--> 
(NP ?agr ?x ?np) 
(VP ?agr ?x ?vp)) 

If we were to alter this rule to produce a simplified semantic interpretation, it would 
look like the following, where the predicate and* simplifies a list of conjunctions into 
a single conjunction: 

<a id='page-705'></a>
(rule (S ?sem) --> 
(np ?agr ?x ?np) 
(vp ?agr ?x ?vp) 
(:test (ancl*(?np ?vp) ?sem))) 

Many rules will have this form, so we adopt a simple convention: if the last argument 
of the constituent on the left-hand side of a rule is the keyword : sem, then we will 
build the semantics by replacing : sem with a conjunction formed by combining all 
the last arguments of the constituents on the right-hand side of the rule. A==> arrow 
will be used for rules that follow this convention, so the following rule is equivalent 
to the one above: 

(rule (S :sem) ==> 
(NP ?agr ?x ?np) 
(VP ?agr ?x ?vp)) 

It is sometimes useful to introduce additional semantics that does not come from one 
of the constituents. This can be indicated with an element of the right-hand side that 
is a list starting with : sem. For example, the following rule adds to the semantics the 
fact that ?x is the topic of the sentence: 

(rule (S ;sem) ==> 
(NP ?agr ?x ?np) 
(VP ?agr ?x ?vp) 
(:sem (topic ?x))) 

Before implementing the rule function for the ==> arrow, it is worth considering if 
there are other ways we could make things easier for the rule writer. One possibility is 
to provide a notation for describing examples. Examples make it easier to understand 
what a rule is designed for. For the S rule, we could add examples like this: 

(rule (S :sem) ==> 
(:ex "John likes Mary" "He sleeps") 
(NP ?agr ?x ?np) 
(VP ?agr ?x ?vp)) 

These examples not only serve as documentation for the rule but also can be stored 
under S and subsequently run when we want to test if S is in fact implemented 
properly. 

Another area where the rule writer could use help is in handling left-recursive 

rules. Consider the rule that says that a sentence can consist of two sentences joined 

by a conjunction: 

<a id='page-706'></a>

(rule (S (?conj ?sl ?s2)) ==> 
(:ex "John likes Mary and Mary likes John") 
(S ?sl) 
(Conj ?conj) 
(S ?s2)) 

While this rule is correct as a declarative statement, it will run into difficulty when 
run by the standard top-down depth-first DCG interpretation process. The top-level 
goal of parsing an S will lead immediately to the subgoal of parsing an S, and the 
result will be an infinite loop. 

Fortunately, we know how to avoid this kind of infinite loop: split the offending 
predicate, S, into two predicates: one that supports the recursion, and one that is at 
a lower level. We will call the lower-level predicate S_. Thus, the following rule says 
that a sentence can consist of two sentences, where the first one is not conjoined and 
the second is possibly conjoined: 

(rule (S (?conj ?sl ?s2)) ==> 

(S- ?sl) 

(Conj ?conj) 

(S ?s2)) 

We also need a rule that says that a possibly conjoined sentence can consist of a 
nonconjoined sentence: 

(rule (S ?sem) ==> (S_ ?sem)) 

To make this work, we need to replace any mention of S in the left-hand side of a rule 
with S_. References to S in the right-hand side of rules remain unchanged. 

(rule (S_ ?sem) ==> ...) 

To make this all automatic, we will provide a macro, conj-rule, that declares a 
category to be one that can be conjoined. Such a declaration will automatically 
generate the recursive and nonrecursive rules for the category, and will insure that 
future references to the category on the left-hand side of a rule will be replaced with 
the corresponding lower-level predicate. 

One problem with this approach is that it imposes a right-branching parse on 
multiple conjoined phrases. That is, we will get parses like "spaghetti and (meatballs 
and salad)" not "(spaghetti and meatballs) and salad." Clearly, that is the wrong 
interpretation for this sentence. Still, it can be argued that it is best to produce 
a single canonical parse, and then let the semantic interpretation functions worry 
about rearranging the parse in the right order. We will not attempt to resolve this 

<a id='page-707'></a>
debate but will provide the automatic conjunction mechanism as a tool that can be 
convenient but has no cost for the user who prefers a different solution. 

We are now ready to implement the extended DCG rule formalism that handles 
:sem, :ex, and automatic conjunctions. The function make-augmented-dcg, stored 
under the arrow = =>, will be used to implement the formalism: 

(setf (get '==> 'rule-function) 'make-augmented-dcg) 

(defun make-augmented-dcg (head body) 
"Build an augmented DCG rule that handles :sem. :ex, 
and automatic conjunctiontive constituents." 
(if (eq (lastl head) :sem) 

;; Handle :sem 

(let* ((?sem (gensym "?SEM"))) 

(make-augmented-dcg 
'(.(butlast head) .?sem) 
'(.(remove :sem body :key #'first-or-nil) 

(:test .(collect-sems body ?sem))))) 
Separate out examples from body 
(multiple-value-bind (exs new-body) 
(partition-if #'(lambda (x) (starts-with . :ex)) body) 
Handle conjunctions 
(let ((rule '(rule .(handle-conj head) --> .new-body))) 

(if (null exs) 
rule 
'(progn (:ex .head ..(mappend #'rest exs)) 

.rule)))))) 

First we show the code that collects together the semantics of each constituent and 
conjoins them when :sem is specified. The function collect-sems picks out the 
semantics and handles the trivial cases where there are zero or one constituents on 
the right-hand side. If there are more than one, it inserts a call to the predicate and*. 

(defun collect-sems (body ?sem) 
"Get the semantics out of each constituent in body, 
and combine them together into ?sem." 
(let ((sems (loop for goal in body 

unless (or (dcg-normal-goal-p goal) 
(dcg-word-list-p goal) 
(starts-with goal :ex) 
(atom goal)) 

collect (lastl goal)))) 

(case (length sems) 
(0 '(= .?sem t)) 
(1 '(= .?sem .(first sems))) 
(t '(and* .sems .?sem))))) 

<a id='page-708'></a>

We could have implemented and* with Prolog clauses, but it is slightly more efficient 
to do it directly in Lisp. A call to conjuncts collects all the conjuncts, and we then 
add an and if necessary: 

(defun and*/2 (in out cont) 
"IN is a list of conjuncts that are conjoined into OUT." 
E.g.: (and* (t (and a b) t (and c d) t) ?x) ==> 
;; ?x= (and abed) 
(if (unify! out (maybe-add 'and (conjuncts (cons 'and in)) t)) 
(funcall cont))) 

(defun conjuncts (exp) 
"Get all the conjuncts from an expression." 
(deref exp) 
(cond ((eq exp t) nil) 

((atom exp) (list exp)) 
((eq (deref (first exp)) 'nil) nil) 
((eq (first exp) 'and) 

(mappend #'conjuncts (rest exp))) 
(t (list exp)))) 

The next step is handling example phrases. The code in make-augmented-dcg turns 
examples into expressions of the form: 

(:ex (S ?sem) "John likes Mary" "He sleeps") 

To make this work, : ex will have to be a macro: 

(defmacro :ex ((category . args) &body examples) 
"Add some example phrases, indexed under the category." 
'(add-examples ',category ',args ',examples)) 

: ex calls add-exampl es to do all the work. Each example is stored in a hash table 
indexed under the the category. Each example is transformed into a two-element list: 
the example phrase string itself and a call to the proper predicate with all arguments 
supplied. The function add-exampl es does this transformation and indexing, and 
run-examples retrieves the examples stored under a category, prints each phrase, 
and calls each goal. The auxiliary functions get-exampl es and cl ear-exampl es are 
provided to manipulate the example table, and remove-punction, punctuation-p 
and stri ng ->1 i st are used to map from a string to a Hst of words. 

(defvar *examples* (make-hash-table :test #'eq)) 

(defun get-examples (category) (gethash category *examples*)) 

(defun clear-examples () (clrhash *examples*)) 

<a id='page-709'></a>
(defun add-examples (category args examples) 
"Add these example strings to this category, 
and when it comes time to run them, use the args." 
(dolist (example examples) 

(when (stringp example) 
(let ((ex '(.example 
(.category .@args 
.(string->list 
(remove-punctuation example)) ())))) 
(unless (member ex (get-examples category) 
:test #'equal) 
(setf (gethash category ^examples*) 
(nconc (get-examples category) (1 ist ex)))))))) 

(defun run-examples (&optional category) 
"Run all the example phrases stored under a category. 
With no category, run ALL the examples." 
(prolog-compi1e-symbols) 
(if (null category) 

(maphash #'(lambda (cat val) 
(declare (ignore val)) 
(format t "~2&Examples of ~a:~&" cat) 
(run-examples cat)) 

^examples*) 

(dolist (example (get-examples category)) 
(format t "~2&EXAMPLE: ~{~a~r9T~a~}" example) 
(top-level-prove (cdr example))))) 

(defun remove-punctuation (string) 
"Replace punctuation with spaces in string. " 
(substitute-if #\space #'punctuation-p string)) 

(defun string->list (string) 
"Convert a string to a list of words." 
(read-from-string(concatenate 'string "("string ")"))) 

(defun punctuation-p (char) (find char "*...;:'!?#-()\\\"")) 

The final part of our augmented DCG formalism is handling conjunctive constituents 
automatically. We already arranged to translate category symbols on the left-hand 
side of rules into the corresponding conjunctive category, as specified by the function 
handl e-con j. We also want to generate automatically (or as easily as possible) rules 
of the following form: 

(rule (S (?conj ?sl ?s2)) ==> 
(S_ ?sl) 
(Conj ?conj) 
(S ?s2)) 

<a id='page-710'></a>

(rule (S ?sem) ==> (S_ ?sem)) 

But before we generate these rules, let's make sure they are exactly what we want. 
Consider parsing a nonconjoined sentence with these two rules in place. The first 
rule would parse the entire sentence as a S_, and would then fail to see a Con j, and thus 
fail. The second rule would then duplicate the entire parsing process, thus doubling 
the amount of time taken. If we changed the order of the two rules we would be able 
to parse nonconjoined sentences quickly, but would have to backtrack on conjoined 
sentences. 

The following shows a better approach. A single rule for S parses a sentence 
with S_, and then calls Conj.S, which can be read as "either a conjunction followed 
by a sentence, or nothing." If the first sentence is followed by nothing, then we just 
use the semantics of the first sentence; if there is a conjunction, we have to form a 
combined semantics. I have added ... to show where arguments to the predicate 
other than the semantic argument fit in. 

(rule (S ... ?s-combi ned) ==> 
(S_ ... ?seml) 
(Conj_S ?seml ?s-combined)) 

(rule (Conj.S ?seml (?conj ?seml ?sem2)) ==> 
(Conj ?conj) 
(S ... ?sem2)) 

(rule (Conj_S ?seml ?seml) ==>) 

Now all we need is a way for the user to specify that these three rules are desired. 
Since the exact method of building up the combined semantics and perhaps even 
the call to Conj may vary depending on the specifics of the grammar being defined, 
the rules cannot be generated entirely automatically. We will settle for a macro, 
conj - rule, that looks very much like the second of the three rules above but expands 
into all three, plus code to relate S_ to S. So the user will type: 

(conj-rule (Conj.S ?seml (?conj ?seml ?sem2)) ==> 
(Conj ?conj) 
(S ?a ?b ?c ?sem2)) 

Here is the macro definition: 

(defmacro conj-rule ((conj-cat semi combined-sem) ==> 

conj (cat . args)) 
"Define this category as an automatic conjunction." 
'(progn 

(setf (get ',cat 'conj-cat) '.(symbol cat '_)) 

<a id='page-711'></a>
(rule (.cat ,@(butlast args) ?combined-sem) ==> 
(.(symbol cat '_) .(butlast args) .semi) 
(.conj-cat ,seml ?combined-sem)) 

(rule (,conj-cat .semi .combined-sem) ==> 
.conj 
(.cat .args)) 

(rule (.conj-cat ?seml ?seml) ==>))) 

and here we define handl e-conj to substitute S_for S in the left-hand side of rules: 

(defun handle-conj (head) 
"Replace (Cat ...) with (Cat. ...) if Cat is declared 
as a conjunctive category." 
(if (and (listp head) (conj-category (predicate head))) 

(cons (conj-category (predicate head)) (args head)) 
head)) 

(defun conj-category (predicate) 
"If this is a conjunctive predicate, return the Cat. symbol." 
(get predicate 'conj-category)) 

20.8 History and References 
As we have mentioned, Alain Colmerauer invented Prolog to use in his grammar of 
French (1973). His metamorphosis grammar formalismwas more expressive but much 
less efficient than the standard DCG formalism. 

The grammar in section 20.4 is essentially the same as the one presented in Fernando 
Pereira and David H. D. Warren's 1980 paper, which introduced the Definite 
Clause Grammar formalism as it is known today. The two developed a much more 
substantial grammar and used it in a very influential question-answering system 
called Chat-80 (Warren and Pereira, 1982). Pereira later teamed with Stuart Shieber 
on an excellent book covering logic grammars in more depth: Prolog and Natural-
Language Analysis (1987). The book has many strong points, but unfortunately it does 
not present a grammar anywhere near as complete as the Chat-80 grammar. 

The idea of a compositional semantics based on mathematical logic owes much 
to the work of the late linguist Richard Montague. The introduction by Dowty, Wall, 
and Peters (1981) and the collection by Rich Thomason (1974) cover Montague's 
approach. 

The grammar in section 20.5 is based loosely on Michael McCord's modular logic 
grammar, as presented in Walker et al. 1990. 
It should be noted that logic grammars are by no means the only approach to 
natural language processing. Woods (1970) presents an approach based on the 

<a id='page-712'></a>

augmented transition network, or ATN. A transition network is like a context-free 
grammar. The augmentation is a way of manipulating features and semantic values. 
This is just like the extra arguments in DCGs, except that the basic operations are 
setting and testing variables rather than unification. So the choice between ATNs and 
DCGs is largely a matter of what programming approach you are most comfortable 
with: procedural for ATNs and declarative for DCGs. My feeling is that unification is 
a more suitable primitive than assignment, so I chose to present DCGs, even though 
this required bringing in Prolog's backtracking and unification mechanisms. 
In either approach, the same linguistic problems must be addressed - agreement, 
long-distance dependencies, topicalization, quantifier-scope ambiguity, and so on. 
Comparing Woods's (1970) ATN grammar to Pereira and Warren's (1980) DCG grammar, 
the careful reader will see that the solutions have much in common. The analysis 
is more important than the notation, as it should be. 
20.9 Exercises 
&#9635; Exercise 20.2 [m] Modify the grammar (from section 20.4, 20.5,
for adjectives before a noun. 
or 20.6) to allow 

&#9635; Exercise 20.3 [m] Modify the grammar to allow for prepositional phrase modifiers 
on verb and noun phrases. 

&#9635; Exercise 20.4 [m] Modify the grammar to allow for ditransitive verbs?erbs that 
take two objects, as in "give the dog a bone." 

&#9635; Exercise 20.5 Suppose we wanted to adopt the Prolog convention of writing DCG 
tests and words in brackets and braces, respectively. Write a function that will alter 
the readtable to work this way. 

&#9635; Exercise 20.6 [m] Define a rule function for a new type of DCG rule that automatically 
builds up a syntactic parse of the input. For example, the two rules: 
(rule is) => (np) (vp)) 
(rule (np) => (iword he)) 
should be equivalent to: 

<a id='page-713'></a>
(rule (s (s ?1 ?2)) --> (np ?1) (vp 12)) 
(rule (np (np he)) --> (:word he)) 

&#9635; Exercise 20.7 [m] There are advantages and disadvantages to the approach that 
Prolog takes in dividing predicates into clauses. The advantage is that it is easy to 
add a new clause. The disadvantage is that it is hard to alter an existing clause. If 
you edit a clause and then evaluate it, the new clause will be added to the end of the 
clause list, when what you really wanted was for the new clause to take the place 
of the old one. To achieve that effect, you have to call cl ear-predicate, and then 
reload all the clauses, not just the one that has been changed. 

Write a macro named - rul e that is just like rul e, except that it attaches names to 
clauses. When a named rule is reloaded, it replaces the old clause rather than adding 
a new one. 

&#9635; Exercise 20.8 [h] Extend the DCG rule function to allow or goals in the right-hand 
side. To make this more useful, also allow and goals. For example: 

(rule (A) --> (B) (or (C) (and (D) (E))) (F)) 

should compile into the equivalent of: 

(<- (A ?S0 ?S4) 
(B ?S0 ?S1) 
(OR (AND (C ?S1 ?S2) (= ?S2 ?S3)) 

(AND (D ?S1 ?S2) (E ?S2 ?S3))) 
(F ?S3 ?S4)) 

20.10 Answers 
Answer 20.1 It uses local variables (?s0, ?sl ...) that are not guaranteed to be 
unique. This is a problem if the grammar writer wants to use these symbols anywhere 
in his or her rules. The fix is to gensym symbols that are guaranteed to be unique. 

<a id='page-714'></a>

Answer 20.5 

(defun setup-braces (&optional (on? t) (readtable *readtable*)) 
"Make Ca b] read as (:word a b) and {a b} as (rtest a b c) 
if ON? is true; otherwise revert {[]} to normal." 
(if (not on?) 

(map nil #'(lambda (c) 
(set-macro-character c (get-macro-character #\a) 
t readtable)) 
"{[]}") 
(progn 
(set-macro-character 
#\] (get-macro-character #\)) nil readtable) 
(set-macro-character 
#\} (get-macro-character #\)) nil readtable) 
(set-macro-character 
#\[ #'(lambda (s ignore) 
(cons :word (read-delimited-1ist #\] s t))) 
nil readtable) 
(set-macro-character 
#\{ #'(lambda (s ignore) 
(cons rtest (read-delimited-1ist #\} s t))) 
nil readtable)))) 

