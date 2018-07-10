# Chapter 14
## Knowledge Representation and Reasoning

> Knowledge itself is power.

> —Francis Bacon (1561–1626)

> The power resides in the knowledge.

> —Edward Feigenbaum

> Stanford University Heuristic Programming Project

> Knowledge is Knowledge, and vice versa.

> —Tee shirt

> Stanford University Heuristic Programming Project

In the 1960s, much of AI concentrated on search techniques.
In particular, a lot of work was concerned with *theorem proving:* stating a problem as a small set of axioms and searching for a proof of the problem.
The implicit assumption was that the power resided in the inference mechanism–if we could just find the right search technique, then all our problems would be solved, and all our theorems would be proved.

Starting in the 1970s, this began to change.
The theorem-proving approach falled to live up to its promise.
AI workers slowly began to realize that they were not going to solve NP-hard problems by coming up with a clever inference algorithm.
The general inferencing mechanisms that worked on toy examples just did not scale up when the problem size went into the thousands (or sometimes even into the dozens).

The *expert-system* approach offered an alternative.
The key to solving hard problems was seen to be the acquisition of special-case rules to break the problem into easier problems.
According to Feigenbaum, the lesson learned from expert systems like MYCIN !!!(span) {:.smallcaps} (which we will see in [chapter 16](B9780080571157500169.xhtml)) is that the choice of inferencing mechanism is not as important as having the right knowledge.
In this view it doesn't matter very much if MYCIN !!!(span) {:.smallcaps} uses forward- or backward-chaining, or if it uses certainty factors, probabilities, or fuzzy set theory.
What matters crucially is that we know pseudomonas is a gram-negative, rod-shaped organism that can infect patients with compromised immune systems.
In other words, the key problem is acquiring and representing knowledge.

While the expert system approach had some successes, it also had fallures, and researchers were interested in learning the limits of this new technology and understanding exactly how it works.
Many found it troublesome that the meaning of the knowledge used in some systems was never clearly defined.
For example, does the assertion `(color apple red)` mean that a particular apple is red, that all apples are red, or that some/most apples are red?
The field of *knowledge representation* concentrated on providing clear semantics for such representations, as well as providing algorithms for manipulating the knowledge.
Much of the emphasis was on finding a good trade-off between *expressiveness* and *efficiency.* An efficient language is one for which all queries (or at least the average query) can be answered quickly.
If we want to guarantee that queries will be answered quickly, then we have to limit what can be expressed in the language.

In the late 1980s, a series of results shed doubt on the hopes of finding an efficient language with any reasonable degree of expressiveness at all.
Using mathematical techniques based on worst-case analysis, it was shown that even seemingly trivial languages were *intractable—*in the worst case, it would take an exponential amount of time to answer a simple query.

Thus, in the 1990s the emphasis has shifted to *knowledge representation and reasoning,* a field that encompasses both the expressiveness and efficiency of languages but recognizes that the average case is more important than the worst case.
No amount of knowledge can help solve an intractable problem in the worse case, but in practice the worst case rarely occurs.

## [ ](#){:#st0010}14.1 A Taxonomy of Representation Languages
{:#s0010}
{:.h1hd}

AI researchers have investigated hundreds of knowledge representation languages, trying to find languages that are convenient, expressive, and efficient.
The languages can be classified into four groups, depending on what the basic unit of representation is.
Here are the four categories, with some examples:

* [ ](#){:#l0010}• *Logical Formulae* (Prolog)

* • *Networks* (semantic nets, conceptual graphs)

* • *Objects* (scripts, frames)

* • *Procedures* (Lisp, production systems)

We have already dealt with *logic-based* languages like Prolog.

*Network-based* languages can be seen as a syntactic variation on logical languages.
A link *L* between nodes *A* and *B* is just another way of expressing the logical relation *L(A, B).* The difference is that network-based languages take their links more seriously: they are intended to be implemented directly by pointers in the computer, and inference is done by traversing these pointers.
So placing a link *L* between *A* and *B* not only asserts that *L(A, B)* is true, but it also says something about how the knowledge base is to be searched.

*Object-oriented* languages can also be seen as syntactic variants of predicate calculus.
Here is a statement in a typical slot-filler frame language:

[ ](#){:#l0015}`(a person`
!!!(p) {:.unnumlist}

` (name = Jan)`
!!!(p) {:.unnumlist}

` (age = 32))`
!!!(p) {:.unnumlist}

This is equivalent to the logical formula:

[ ](#){:#l0020}∃p: person(p) ![images](images/B9780080571157500145/cap.png) name(p,Jan) ![images](images/B9780080571157500145/cap.png) age(p,32)
!!!(p) {:.unnumlist}

The frame notation has the advantage of being easier to read, in some people's opinion.
However, the frame notation is less expressive.
There is no way to say that the person's name is either Jan or John, or that the person's age is not 34.
In predicate calculus, of course, such statements can be easily made.

Finally, *procedural* languages are to be contrasted with representation languages: procedural languages compute answers without explicit representation of knowledge.

There are also hybrid representation languages that use different methods to encode different kinds of knowledge.
The KL-ONE family of languages uses both logical formulae and objects arranged into a network, for example.
Many frame languages allow *procedural attachment,* a technique that uses arbitrary procedures to compute values for expressions that are inconvenient or impossible to express in the frame language itself.

## [ ](#){:#st0015}14.2 Predicate Calculus and its Problems
{:#s0015}
{:.h1hd}

So far, many of our representations have been based on predicate calculus, a notation with a distinguished position in AI: it serves as the universal standard by which other representations are defined and evaluated.
The previous section gave an example expression from a frame language.
The frame language may have many merits in terms of the ease of use of its syntax or the efficiency of its internal representation of data.
However, to understand what expressions in the language mean, there must be a clear definition.
More often than not, that definition is given in terms of predicate calculus.

A predicate calculus representation assumes a universe of individuals, with relations and functions on those individuals, and sentences formed by combining relations with the logical connectives `and`, `or`, and `not`.
Philosophers and psychologists will argue the question of how appropriate predicate calculus is as a model of human thought, but one point stands clear: predicate calculus is sufficient to represent anything that can be represented in a digital computer.
This is easy to show: assuming the computer's memory has *n* bits, and the equation *bi* = 1 means that bit *i* is on, then the entire state of the computer is represented by a conjunction such as:

b0=0∧b1=0∧b2=1...∧bn=0

![si1_e](images/B9780080571157500145/si1_e.gif)

Once we can represent a state of the computer, it becomes possible to represent any computer program in predicate calculus as a set of axioms that map one state onto another.
Thus, predicate calculus is shown to be a *sufficient* langaage for representing anything that goes on inside a computer—it can be used as a tool for analyzing any program from the outside.

This does not prove that predicate calculus is an *appropriate* tool for all applications.
There are good reasons why we may want to represent knowledge in a form that is quite different from predicate calculus, and manipulate the knowledge with procedures that are quite different from logical inference.
But we should still be able to describe our system in terms of predicate calculus axioms, and prove theorems about it.
To do any less is to be sloppy.
For example, we may want to manipulate numbers inside the computer by using the arithmetic instructions that are built into the CPU rather than by manipulating predicate calculus axioms, but when we write a square-root routine, it had better satisfy the axiom:

x=y⇒y×y=x

![si2_e](images/B9780080571157500145/si2_e.gif)

Predicate calculus also serves another purpose: as a tool that can be used *by* a program rather than *on* a program.
All programs need to manipulate data, and some programs will manipulate data that is considered to be in predicate calculus notation.
It is this use that we will be concerned with.

Predicate calculus makes it easy to start writing down facts about a domain.
But the most straightforward version of predicate calculus suffers from a number of serious limitations:

* [ ](#){:#l0025}• *Decidability—*given a set of axioms and a goal, it may be that neither the goal nor its negation can be derived from the axioms.

* • *Tractability—even* when a goal is provable, it may take too long to find the proof using the avallable inferencing mechanisms.

* • *Uncertainty—*it can be inconvenient to deal with relations that are probable to a degree but not known to be definitely true or false.

* • *Monotonicity—*in pure predicate calculus, once a theorem is proved, it is true forever.
But we would like a way to derive tentative theorems that rely on assumptions, and be able to retract them when the assumptions prove false.

* • *Consistency—*pure predicate calculus admits no contradictions.
If by accident both *P* and ¬*P* are derived, then *any* theorem can be proved.
In effect, a single contradiction corrupts the entire data base.

* • *Omniscience—*it can be difficult to distinguish what is provable from what should be proved.
This can lead to the unfounded assumption that an agent believes all the consequences of the facts it knows.

* • *Expressiveness—*the first-order predicate calculus makes it awkward to talk about certain things, such as the relations and propositions of the language itself.

The view held predominantly today is that it is best to approach these problems with a dual attack that is both within and outside of predicate calculus.
It is considered a good idea to invent new notations to address the problems—both for convenience and to facilitate special-purpose reasoners that are more efficient than a general-purpose theorem prover.
However, it is also important to define scrupulously the meaning of the new notation in terms of familiar predicate-calculus notation.
As Drew McDermott put it, "No notation without denotation!" (1978).

In this chapter we show how new notations (and their corresponding meanings) can be used to extend an existing representation and reasoning system.
Prolog is chosen as the language to extend.
This is not meant as an endorsement for Prolog as the ultimate knowledge representation language.
Rather, it is meant solely to give us a clear and familiar foundation from which to build.

## [ ](#){:#st0020}14.3 A Logical Language: Prolog
{:#s0020}
{:.h1hd}

Prolog has been proposed as the answer to the problem of programming in logic.
Why isn't it accepted as the universal representation language?
Probably because Prolog is a compromise between a representation language and a programming language.
Given two specifications that are logically equivalent, one can be an efficient Prolog program, while the other is not.
Kowalski's famous equation “*algorithm = logic + control”* expresses the limits of logic alone: *logic = algorithm - control.* Many problems (especially in AI) have large or infinite search spaces, and if Prolog is not given some advice on how to search that space, it will not come up with the answer in any reasonable length of time.

Prolog's problems fall into three classes.
First, in order to make the language efficient, its expressiveness was restricted.
It is not possible to assert that a person's name is either Jan or John in Prolog (although it is possible to *ask* if the person's name is one of those).
Similarly, it is not possible to assert that a fact is false; Prolog does not distinguish between false and unknown.
Second, Prolog's inference mechanism is neither sound nor complete.
Because it does not check for circular unification, it can give incorrect answers, and because it searches depth-first it can miss correct answers.
Third, Prolog has no good way of adding control information to the underlying logic, making it inefficient on certain problems.

## [ ](#){:#st0025}14.4 Problems with Prolog's Expressiveness
{:#s0025}
{:.h1hd}

If Prolog is programming in logic, it is not the full predicate logic we are familiar with.
The main problem is that Prolog can't express certain kinds of indefinite facts.
It can represent definite facts: the capital of Rhode Island is Providence.
It can represent conjunctions of facts: the capital of Rhode Island is Providence and the capital of California is Sacramento.
But it can not represent disjunctions or negations: that the capital of California is *not* Los Angeles, or that the capital of New York is *either* New York City *or* Albany.
We could try this:

[ ](#){:#l0030}`(<− (not (capital LA CA)))`
!!!(p) {:.unnumlist}

`(<− (or (capital Albany NY) (capital NYC NY)))`
!!!(p) {:.unnumlist}

but note that these last two facts concern the relation `not` and `or`, not the relation `capital`.
Thus, they will not be considered when we ask a query about `capital`.
Fortunately, the assertion "Either NYC or Albany is the capital of NY" can be rephrased as two assertions: "Albany is the capital of NY if NYC is not" and "NYC is the capital of NY if Albany is not:"

[ ](#){:#l0035}`(<− (capital Albany NY) (not (capital NYC NY)))`
!!!(p) {:.unnumlist}

`(<− (capital NYC NY) (not (capital Albany NY)))`
!!!(p) {:.unnumlist}

Unfortunately, Prolog's not is different from logic's `not`.
When Prolog answers "no" to a query, it means the query cannot be proven from the known facts.
If everything is known, then the query must be false, but if there are facts that are not known, the query may in fact be true.
This is hardly surprising; we can't expect a program to come up with answers using knowledge it doesn't have.
But in this case, it causes problems.
Given the previous two clauses and the query `(capital ?c NY)`, Prolog will go into an infinite loop.
If we remove the first clause, Prolog would fall to prove that Albany is the capital, and hence conclude that NYC is.
If we remove the second clause, the opposite conclusion would be drawn.

The problem is that Prolog equates "not proven" with "false." Prolog makes what is called the *closed world assumption*—it assumes that it knows everything that is true.
The closed world assumption is reasonable for most programs, because the programmer does know all the relevant information.
But for knowledge representation in general, we would like a system that does not make the closed world assumption and has three ways to answer a query: "yes," "no," or "unknown." In this example, we would not be able to conclude that the capital of NY is or is not NYC, hence we would not be able to conclude anything about Albany.

As another example, consider the clauses:

[ ](#){:#l0040}`(<− (damned) (do))`
!!!(p) {:.unnumlist}

`(<− (damned) (not (do)))`
!!!(p) {:.unnumlist}

With these rules, the query `(?
(damned))` should logically be answered "yes." Furthermore, it should be possible to conclude `(damned)` without even investigating if `(do)` is provable or not.
What Prolog does is first try to prove `(do)`.
If this succeeds, then `(damned)` is proved.
Either way, Prolog then tries again to prove `(do)`, and this time if the proof falls, then `(damned)` is proved.
So Prolog is doing the same proof twice, when it is unnecessary to do the proof at all.
Introducing negation wrecks havoc on the simple Prolog evaluation scheme.
It is no longer sufficient to consider a single clause at a time.
Rather, multiple clauses must be considered together if we want to derive all the right answers.

Robert [Moore 1982](B9780080571157500285.xhtml#bb0865) gives a good example of the power of disjunctive reasoning.
His problem concerned three colored blocks, but we will update it to deal with three countries.
Suppose that a certain Eastern European country, *E*, has just decided if it will remain under communist rule or become a democracy, but we do not know the outcome of the decision.
*E* is situated between the democracy *D* and the communist country *C*:

![u14-02-9780080571157](images/B9780080571157500145/u14-02-9780080571157.jpg)     

The question is: Is there a communist country next to a democracy?
Moore points out that the answer is "yes," but discovering this requires reasoning by cases.
If *E* is a democracy then it is next to *C* and the answer is yes.
But if *E* is communist then it is next to *D* and the answer is still yes.
Since those are the only two possibilities, the answer must be yes in any case.
Logical reasoning gives us the right answer, but Prolog can not.
We can describe the problem with the following seven assertions and one query, but Prolog can not deal with the or in the final assertion.

[ ](#){:#l0045}`(<− (next-to D E))  (<− (next-to E D))`
!!!(p) {:.unnumlist}

`(<− (next-to E C))  (<− (next-to C E))`
!!!(p) {:.unnumlist}

`(<− (democracy D))  (<− (communist C))`
!!!(p) {:.unnumlist}

`(<− (or (democracy E) (communist E)))`
!!!(p) {:.unnumlist}

`(?- (next-to ?A ?B) (democracy ?A) (communist ?B))`
!!!(p) {:.unnumlist}

We have seen that Prolog is not very good at representing disjunctions and negations.
It also has difficulty representing existentials.
Consider the following statement in English, logic, and Prolog:

[ ](#){:#l0050}Jan likes everyone.
!!!(p) {:.unnumlist}

∀ *x* person(*x*) ⇒ likes(Jan,*x*)
!!!(p) {:.unnumlist}

`(<− (likes Jan ?x) (person ?x))`
!!!(p) {:.unnumlist}

The Prolog translation is faithful.
But there is no good translation for "Jan likes someone." The closest we can get is:

[ ](#){:#l0055}Jan likes someone.
!!!(p) {:.unnumlist}

∃ *x* person(x) ⇒ likes(Jan,x)
!!!(p) {:.unnumlist}

`(<- (likes Jan pl))`
!!!(p) {:.unnumlist}

`(<- (person pl))`
!!!(p) {:.unnumlist}

Here we have invented a new symbol, `p1`, to represent the unknown person that Jan likes, and have asserted that `p1` is a person.
Notice that `p1` is a constant, not a variable.
This use of a constant to represent a specific but unknown entity is called a *Skolem constant,* after the logician Thoralf Skolem (1887–1963).
The intent is that `p1` may be equal to some other person that we know about.
If we find out that Adrian is the person Jan likes, then in logic we can just add the assertion p1 = Adrian.
But that does not work in Prolog, because Prolog implicitly uses the *unique name assumption—*all atoms represent distinct individuals.

A Skolem constant is really just a special case of a *Skolem function—*an unknown entity that depends on one or more variable.
For example, to represent "Everyone likes someone" we could use:

[ ](#){:#l0060}Everyone likes someone.
!!!(p) {:.unnumlist}

∀*y*∃ *x* person (*x*) ⇒ likes (*y, x*)
!!!(p) {:.unnumlist}

`(<- (likes ?y (p2 ?y)))`
!!!(p) {:.unnumlist}

`(<- (person (p2 ?y)))`
!!!(p) {:.unnumlist}

Here `p2` is a Skolem function that depends on the variable `?y`.
In other words, everyone likes some person, but not necessarily the same person.

## [ ](#){:#st0030}14.5 Problems with Predicate Calculus's Expressiveness
{:#s0030}
{:.h1hd}

In the previous section we saw that Prolog has traded some expressiveness for efficiency.
This section explores the limits of predicate calculus's expressiveness.

Suppose we want to assert that lions, tigers, and bears are kinds of animais.
In predicate calculus or in Prolog we could write an implication for each case:

[ ](#){:#l0065}`(<- (animal ?x) (lion ?x))`
!!!(p) {:.unnumlist}

`(<- (animal ?x) (tiger ?x))`
!!!(p) {:.unnumlist}

`(<- (animal ?x) (bear ?x))`
!!!(p) {:.unnumlist}

These implications allow us to prove that any known lion, tiger, or bear is in fact an animal.
However, they do not allow us to answer the question "What kinds of animais are there?" It is not hard to imagine extending Prolog so that the query

[ ](#){:#l0070}`(?- (<- (animal ?x) ?proposition))`
!!!(p) {:.unnumlist}

would be legal.
However, this happens not to be valid Prolog, and it is not even valid first-order predicate calculus (or FOPC).
In FOPC the variables must range over constants in the language, not over relations or propositions.
Higher-order predicate calculus removes this limitation, but it has a more complicated proof theory.

It is not even clear what the values of `?proposition` should be in the query above.
Surely `(lion ?x)` would be a valid answer, but so would `(animal ?x), (or (tiger ?x) (bear ?x))`, and an infinite number of other propositions.
Perhaps we should have two types of queries, one that asks about "kinds," and another that asks about propositions.

There are other questions that we might want to ask about relations.
Just as it is useful to declare the types of parameters to a Lisp function, it can be useful to declare the types of the parameters of a relation, and later query those types.
For example, we might say that the `likes` relation holds between a person and an object.

In general, a sentence in the predicate calculus that uses a relation or sentence as a term is called a higher-order sentence.
There are some quite subtle problems that come into play when we start to allow higher-order expressions.
Allowing sentences in the calculus to talk about the truth of other sentences can lead to a paradox: is the sentence "This sentence is false" true or false?

Predicate calculus is defined in terms of a universe of individuals and their properties and relations.
Thus it is well suited for a model of the world that picks out individuals and categorizes them–a person here, a building there, a sidewalk between them.
But how well does predicate calculus fare in a world of continuous substances?
Consider a body of water consisting of an indefinite number of subconstituents that are all water, with some of the water evaporating into the air and rising to form clouds.
It is not at all obvious how to define the individuals here.
However, Patrick Hayes has shown that when the proper choices are made, predicate calculus can describe this kind of situation quite well.
The detalls are in Hayes 1985.

The need to define categories is a more difficult problem.
Predicate calculus works very well for crisp, mathematical categories: a; is a triangle if and only if *x* is a polygon with three sides.
Unfortunately, most categories that humans deal with in everyday life are not defined so rigorously.
The category *friend* refers to someone you have mostly positive feelings for, whom you can usually trust, and so on.
This "definition" is not a set of necessary and sufficient conditions but rather is an open-ended list of ill-defined qualities that are highly correlated with the category *friend.* We have a prototype for what an ideal friend should be, but no clear-cut boundaries that separate *friend* from, say, *acquaintance.* Furthermore, the boundaries seem to vary from one situation to another: a person you describe as a good friend in your work place might be only an acquaintance in the context of your home life.

There are versions of predicate calculus that admit quantifiers like "most" in addition to "for all" and "there exists," and there have been attempts to define prototypes and measure distances from them.
However, there is no consensus on the way to approach this problem.

## [ ](#){:#st0035}14.6 Problems with Completeness
{:#s0035}
{:.h1hd}

Because Prolog searches depth-first, it can get caught in one branch of the search space and never examine the other branches.
This problem can show up, for example, in trying to define a commutative relation, like `sibling`:

[ ](#){:#l0075}`(<- (sibling lee kim))`
!!!(p) {:.unnumlist}

`(<- (sibling ?x ?y) (sibling ?y ?x))`
!!!(p) {:.unnumlist}

With these clauses, we expect to be able to conclude that Lee is Kim's sibling, and Kim is Lee's.
Let's see what happens:

[ ](#){:#l0080}`> (?- (sibling ?x ?y))`
!!!(p) {:.unnumlist}

`?X = LEE`
!!!(p) {:.unnumlist}

`?Y = KIM;`
!!!(p) {:.unnumlist}

`?X = KIM`
!!!(p) {:.unnumlist}

`?Y = LEE;`
!!!(p) {:.unnumlist}

`?X = LEE`
!!!(p) {:.unnumlist}

`?Y = KIM;`
!!!(p) {:.unnumlist}

`?X = KIM`
!!!(p) {:.unnumlist}

`?Y = LEE.`
!!!(p) {:.unnumlist}

`No.`
!!!(p) {:.unnumlist}

We get the expected conclusions, but they are deduced repeatedly, because the commutative clause for siblings is applied over and over again.
This is annoying, but not critical.
Far worseis when we ask `(?- (sibling fred ?x))`.
This query loops forever.
Happily, this particular type of example has an easy fix: just introduce two predicates, one for data-base level facts, and one at the level of axioms and queries:

[ ](#){:#l0085}`(<- (sibling-fact lee kim))`
!!!(p) {:.unnumlist}

`(<- (sibling ?x ?y) (sibling-fact ?x ?y))`
!!!(p) {:.unnumlist}

`(<- (sibling ?x ?y) (sibling-fact ?y ?x))`
!!!(p) {:.unnumlist}

Another fix would be to change the interpreter to fall when a repeated goal was detected.
This was the approach taken in GPS.
However, even if we eliminated repeated goals, Prolog can still get stuck in one branch of a depth-first search.
Consider the example:

[ ](#){:#l0090}`(<- (natural 0))`
!!!(p) {:.unnumlist}

`(<- (natural (1 + ?n)) (natural ?n))`
!!!(p) {:.unnumlist}

These rules define the natural numbers (the non-negative integers).
We can use the rules either to confirm queries like `(natural (1 + (1 + (1 + 0))))` or to generate the natural numbers, as in the query `(natural ?n)`.
So far, everything is fine.
But suppose we wanted to define all the integers.
One approach would be this:

[ ](#){:#l0095}`(<- (integer 0))`
!!!(p) {:.unnumlist}

`(<- (integer ?n) (integer (1 + ?n)))`
!!!(p) {:.unnumlist}

`(<- (integer (1 + ?n)) (integer ?n))`
!!!(p) {:.unnumlist}

These rules say that 0 is an integer, and any *n* is an integer if *n* + 1 is, and *n* + 1 is if *n* is.
While these rules are correct in a logical sense, they don't work as a Prolog program.
Asking `(integer *x*)` will resuit in an endless series of ever-increasing queries: `(integer (1 + *x*)), (integer (1 + (1 + *x*)))`, and so on.
Each goal is different, so no check can stop the recursion.

The occurs check may or may not introduce problems into Prolog, depending on your interpretation of infinite trees.
Most Prolog systems do not do the occurs check.
The reasoning is that unifying a variable with some value is the Prolog equivalent of assigning a value to a variable, and programmers expect such a basic operation to be fast.
With the occurs check turned off, it will in fact be fast.
With checking on, it takes time proportional to the size of the value, which is deemed unacceptable.

With occurs checking off, the programmer gets the benefit of fast unification but can run into problems with circular structures.
Consider the following clauses:

[ ](#){:#l0100}`(<- (parent ?x (mother-of ?x)))`
!!!(p) {:.unnumlist}

`(<- (parent ?x (father-of ?x)))`
!!!(p) {:.unnumlist}

These clauses say that, for any person, the mother of that person and the father of that person are parents of that person.
Now let us ask if there is a person who is his or her own parent:

[ ](#){:#l0105}`> (?
(parent ?y ?y))`
!!!(p) {:.unnumlist}

`?Y = [Abort]`
!!!(p) {:.unnumlist}

The system has found an answer, where `?y = (mother-of ?y).` The answer can't be printed, though, because `deref` (or `subst-bindings` in the interpreter) goes into an infinite loop trying to figure out what `?y` is.
Without the printing, there would be no infinite loop:

[ ](#){:#l0110}`(<- (self-parent) (parent ?y ?y))`
!!!(p) {:.unnumlist}

`> (?
(self-parent))`
!!!(p) {:.unnumlist}

`Yes;`
!!!(p) {:.unnumlist}

`Yes;`
!!!(p) {:.unnumlist}

`No.`
!!!(p) {:.unnumlist}

The `self-parent` query succeeds twice, once with the mother clause and once with the father clause.
Has Prolog done the right thing here?
It depends on your interpretation of infinite circular trees.
If you accept them as valid objects, then the answer is consistent.
If you don't, then leaving out the occurs check makes Prolog *unsound:* it can come up with incorrect answers.

The same problem cornes up if we ask if there are any sets that include themselves as members.
The query `(member ?set ?set)` will succeed, but we will not be able to print the value of `?set`.

## [ ](#){:#st0040}14.7 Problems with Efficiency: Indexing
{:#s0040}
{:.h1hd}

Our Prolog compiler is designed to handle "programlike" predicates—predicates with a small number of rules, perhaps with complex bodies.
The compiler does much worse on "tablelike" predicates—predicates with a large number of simple facts.
Consider the predicate `pb`, which encodes phone-book facts in the form:

[ ](#){:#l0115}`(pb (name Jan Doe) (num 415 555 1212))`
!!!(p) {:.unnumlist}

Suppose we have a few thousand entries of this kind.
A typical query for this data base would be:

[ ](#){:#l0120}`(pb (name Jan Doe) ?num)`
!!!(p) {:.unnumlist}

It would be inefficient to search through the facts linearly, matching each one against the query.
It would also be inefficient to recompile the whole `pb/2` predicate every time a new entry is added.
But that is just what our compiler does.

The solutions to the three problems–expressiveness, completeness, and indexing–will be considered in reverse order, so that the most difficult one, expressiveness, will come last.

## [ ](#){:#st0045}14.8 A Solution to the Indexing Problem
{:#s0045}
{:.h1hd}

A better solution to the phone-book problem is to index each phone-book entry in some kind of table that makes it easy to add, delete, and retrieve entries.
That is what we will do in this section.
We will develop an extension of the trie or discrimination tree data structure built in [section 10.5](B9780080571157500108.xhtml#s0030) ([page 344](B9780080571157500108.xhtml#p344)).

Making a discrimination tree for Prolog facts is complicated by the presence of variables in both the facts and the query.
Either facts with variables in them will have to be indexed in several places, or queries with variables will have to look in several places, or both.
We also have to decide if the discrimination tree itself will handle variable binding, or if it will just return candidate matches which are then checked by some other process.
It is not clear what to store in the discrimination tree: copies of the fact, functions that can be passed continuations, or something else.
More design choices will come up as we proceed.

It is difficult to make design choices when we don't know exactly how the system will be used.
We don't know what typical facts will look like, nor typical queries.
Therefore, we will design a fairly abstract tool, forgetting for the moment that it will be used to index Prolog facts.

We will address the problem of a discrimination tree where both the keys and queries are predicate structures with wild cards.
A wild card is a variable, but with the understanding thatthere is no variable binding; each instance of a variable can match anything.
A predicate structure is a list whose first element is a nonvariable symbol.
The discrimination tree supports three operations:

* [ ](#){:#l0125}• `index`–add a key/value pair to the tree

* • `fetch`–find all values that potentially match a given key

* • `unindex`–remove all key/value pairs that match a given key

To appreciate the problems, we need an example.
Suppose we have the following six keys to index.
For simplicity, the value of each key will be the key itself:

[ ](#){:#l0130}`1 (p a b)`
!!!(p) {:.unnumlist}

`2 (p a c)`
!!!(p) {:.unnumlist}

`3 (p a ?x)`
!!!(p) {:.unnumlist}

`4 (p b c)`
!!!(p) {:.unnumlist}

`5 (p b (f c))`
!!!(p) {:.unnumlist}

`6 (p a (f .
?x))`
!!!(p) {:.unnumlist}

Now assume the query `(p ?y c)`.
This should match keys 2, 3, and 4.
How could we efficiently arrive at this set?
One idea is to list the key/value pairs under every atom that they contain.
Thus, all six would be listed under the atom `p`, while 2, 4, and 5 would be listed under the atom c.
A unification check could elimina te 5, but we still would be missing 3.
Key 3 (and every key with a variable in it) could potentially contain the atom `c`.
So to get the right answers under this approach, we will need to index every key that contains a variable under every atom–not an appealing situation.

An alternative is to create indices based on both atoms and their position.
So now we would be retrieving all the keys that have a c in the second argument position: 2 and 4, plus the keys that have a variable as the second argument: 3.
This approach seems to work much better, at least for the example shown.
To create the index, we essentially superimpose the list structure of all the keys on top of each other, to arrive at one big discrimination tree.
At each position in the tree, we create an index of the keys that have either an atom or a variable at that position.
[Figure 14.1](#f0010) shows the discrimination tree for the six keys.

![f14-01-9780080571157](images/B9780080571157500145/f14-01-9780080571157.jpg)     
Figure 14.1
!!!(span) {:.fignum}
Discrimination Tree with Six Keys
Consider the query `(p ?y c)`.
Either the `p` or the `c` could be used as an index.
The `p` in the predicate position retrieves all six keys.
But the c in the second argument position retrieves only three keys: 2 and 4, which are indexed under c itself, and 3, which is indexed under the variable in that position.

Now consider the query `(p ?y (f ?z))`.
Again, the `p` serves as an index to all six keys.
The `f` serves as an index to only three keys: the 5 and 6, which are indexed directly under f in that position, and 3, which is indexed under the variable in a position along the path that lead to f.
In general, all the keys indexed under variables along the path must be considered.

The retrieval mechanism can overretrieve.
Given the query `(p a (f ?x))`, the atom `p` will again retrieve all six keys, the atom a retrieves 1, 2, 3, and 6, and f again retrieves 5, 6, and 3.
So `f` retrieves the shortest list, and hence it will be used to determine the final resuit.
But key 5 is `(p b (f c))`, which does not match the query `(pa (f?x))`.

We could eliminate this problem by intersecting all the lists instead of just taking the shortest list.
It is perhaps feasible to do the intersection using bit vectors, but probably too slow and wasteful of space to do it using lists.
Even if we did intersect keys, we would still overretrieve, for two reasons.
First, we don't use ni1 as an index, so we are ignoring the difference between `(f ?x)` and `(f .
?x)`.
Second, we are using wild-card semantics, so the query `(p ?x ?x)` would retrieve all six keys, when it should only retrieve three.
Because of these problems, we make a design choice: we will first build a data base retrieval function that retrieves potential matches, and later worry about the unification process that will eliminate mismatches.

We are ready for a more complete specification of the indexing strategy:

* [ ](#){:#l0135}• The value will be indexed under each non-nil nonvariable atom in the key, with a separate index for each position.
For example, given the preceding data base, the atom a in the first argument position would index values 1,2,3, and 6, while the atom b in the second argument position would index value 4 and 5.
The atom p in the predicate position would index all six values.

* • In addition, we will maintain a separate index for variables at each position.
For example, value 3 would be stored under the index "variable in second argument position."

* • "Position" does not refer solely to the linear position in the top-level list.
For example, value 5 would be indexed under atom f in the caaddr position.

* • It follows that a key with *n* atoms will be indexed in *n* different ways.

For retrieval, the strategy is:

* [ ](#){:#l0140}• For each non-nil nonvariable atom in the retrieval key, generate a list of possible matches.
Choose the shortest such list.

* • Each list of possible matches will have to be augmented with the values indexed under a variable at every position "above." For example, `f` in the `caaddr` position retrieves value 5, but it also must retrieve value 3, because the third key has a variable in the `caddr` position, and `caddr` is "above" `caaddr.`

* • The discrimination tree may return values that are not valid matches.
The purpose of the discrimination tree is to reduce the number of values we will have to unify against, not to determine the exact set of matches.

It is important that the retrieval function execute quickly.
If it is slow, we might just as well match against every key in the table linearly.
Therefore, we will take care to implement each part efficiently.
Note that we will have to compare the length of lists to choose the shortest possibility.
Of course, it is trivial to compare lengths using `length,` but `length` requires traversing the whole list.
We can do better if we store the length of the list explicitly.
A list with its length will be called an `nlist`.
It will be implemented as a cons cell containing the number of elements and a list of the elements themselves.
An alternative would be to use extensible vectors with fill pointers.

[ ](#){:#l0145}`;; An nlist is implemented as a (count .
elements) pair:`
!!!(p) {:.unnumlist}

`(defun make-empty-nlist ()`
!!!(p) {:.unnumlist}

` "Create a new, empty nlist."`
!!!(p) {:.unnumlist}

` (cons 0 nil))`
!!!(p) {:.unnumlist}

`(defun nlist-n (x) "The number of elements in an nlist." (carx))`
!!!(p) {:.unnumlist}

`(defun nlist-list (x) "The elements in an nlist." (cdr x))`
!!!(p) {:.unnumlist}

`(defun nlist-push (item nlist)`
!!!(p) {:.unnumlist}

` "Add a new element to an nlist."`
!!!(p) {:.unnumlist}

` (incf (car nlist))`
!!!(p) {:.unnumlist}

` (push item (cdr nlist))`
!!!(p) {:.unnumlist}

` nlist)`
!!!(p) {:.unnumlist}

Now we need a place to store these nlists.
We will build the data base out of discrimination tree nodes called dtree nodes.
Each dtree node has a field to hold the variable index, the atom indices, and pointers to two subnodes, one for the `first` and one for the `rest`.
We implement dtrees as vectors for efficiency, and because we will never need a `dtree-p` predicate.

[ ](#){:#l0150}`(defstruct (dtree (:type vector))`
!!!(p) {:.unnumlist}

` (first nil) (rest nil) (atoms nil) (var (make-empty-nlist)))`
!!!(p) {:.unnumlist}

A separate dtree will be stored for each predicate.
Since the predicates must be symbols, it is possible to store the dtrees on the predicate's property list.
In most implementations, this will be faster than alternatives such as hash tables.

[ ](#){:#l0155}`(let ((predicates nil))`
!!!(p) {:.unnumlist}

` (defun get-dtree (predicate)`
!!!(p) {:.unnumlist}

`  "Fetch (or make) the dtree for this predicate."`
!!!(p) {:.unnumlist}

`  (cond ((get predicate 'dtree))`
!!!(p) {:.unnumlist}

`   (t (push predicate predicates)`
!!!(p) {:.unnumlist}

`    (setf (get predicate 'dtree) (make-dtree)))))`
!!!(p) {:.unnumlist}

` (defun clear-dtrees ()`
!!!(p) {:.unnumlist}

`  "Remove all the dtrees for all the predicates."`
!!!(p) {:.unnumlist}

`  (dolist (predicate predicates)`
!!!(p) {:.unnumlist}

`   (setf (get predicate 'dtree) nil))`
!!!(p) {:.unnumlist}

`  (setf predicates nil)))`
!!!(p) {:.unnumlist}

The function `index` takes a relation as key and stores it in the dtree for the predicate of the relation.
It calls `dtree-index` to do all the work of storing a value under the proper indices for the key in the proper dtree node.

The atom indices are stored in an association list.
Property lists would not work, because they are searched using eq and atoms can be numbers, which are not necessarily `eq`.
Association lists are searched using `eql` by default.
An alternative would be to use hash tables for the index, or even to use a scheme that starts with association lists and switches to a hash table when the number of entries gets large.
I use `lookup` to look up the value of a key in a property list.
This function, and its `setf` method, are defined on [page 896](B978008057115750025X.xhtml#p896).

[ ](#){:#l0160}`(defun index (key)`
!!!(p) {:.unnumlist}

` "Store key in a dtree node.
Key must be (predicate .
args);`
!!!(p) {:.unnumlist}

` it is stored in the predicate's dtree."`
!!!(p) {:.unnumlist}

` (dtree-index key key (get-dtree (predicate key))))`
!!!(p) {:.unnumlist}

`(defun dtree-index (key value dtree)`
!!!(p) {:.unnumlist}

` "Index value under all atoms of key in dtree."`
!!!(p) {:.unnumlist}

` (cond`
!!!(p) {:.unnumlist}

` ((consp key)   ; index on both first and rest`
!!!(p) {:.unnumlist}

`  (dtree-index (first key) value`
!!!(p) {:.unnumlist}

`    (or (dtree-first dtree)`
!!!(p) {:.unnumlist}

`     (setf (dtree-first dtree) (make-dtree))))`
!!!(p) {:.unnumlist}

`  (dtree-index (rest key) value`
!!!(p) {:.unnumlist}

`    (or (dtree-rest dtree)`
!!!(p) {:.unnumlist}

`     (setf (dtree-rest dtree) (make-dtree)))))`
!!!(p) {:.unnumlist}

` ((null key)) ; don't index on nil`
!!!(p) {:.unnumlist}

` ((variable-p key) ; index a variable`
!!!(p) {:.unnumlist}

`  (nlist-push value (dtree-var dtree)))`
!!!(p) {:.unnumlist}

` (t ;; Make sure there is an nlist for this atom, and add to it`
!!!(p) {:.unnumlist}

`  (nlist-push value (lookup-atom key dtree)))))`
!!!(p) {:.unnumlist}

`(defun lookup-atom (atom dtree)`
!!!(p) {:.unnumlist}

` "Return (or create) the nlist for this atom in dtree."`
!!!(p) {:.unnumlist}

` (or (lookup atom (dtree-atoms dtree))`
!!!(p) {:.unnumlist}

`  (let ((new (make-empty-nlist)))`
!!!(p) {:.unnumlist}

`   (push (cons atom new) (dtree-atoms dtree))`
!!!(p) {:.unnumlist}

`   new)))`
!!!(p) {:.unnumlist}

Now we define a function to test the indexing routine.
Compare the output with [figure 14.1](#f0010).

[ ](#){:#l0165}`(defun test-index ()`
!!!(p) {:.unnumlist}

` (let ((props '((p a b) (p a c) (p a ?x) (p b c)`
!!!(p) {:.unnumlist}

`         (p b (f c)) (p a (f .
?x)))))`
!!!(p) {:.unnumlist}

` (clear-dtrees)`
!!!(p) {:.unnumlist}

` (mapc #'index props)`
!!!(p) {:.unnumlist}

` (write (list props (get-dtree 'p))`
!!!(p) {:.unnumlist}

`  :circle t :array t :pretty t)`
!!!(p) {:.unnumlist}

` (values)))`
!!!(p) {:.unnumlist}

`> (test-index)`
!!!(p) {:.unnumlist}

`((#1=(P A B)`
!!!(p) {:.unnumlist}

` #2=(P A C)`
!!!(p) {:.unnumlist}

` #3=(P A ?X)`
!!!(p) {:.unnumlist}

` #4=(P B C)`
!!!(p) {:.unnumlist}

` #5=(P B (F C))`
!!!(p) {:.unnumlist}

` #6=(P A (F .
?X)))`
!!!(p) {:.unnumlist}

` #(#(NIL NIL (P (6 #6# #5# #4# #3# #2# #1#)) (0))`
!!!(p) {:.unnumlist}

` #(#(NIL NIL (B (2 #5# #4#) A (4 #6# #3# #2# #1#)) (0))`
!!!(p) {:.unnumlist}

`  #(#(#(NIL NIL (F (2 #6# #5#)) (0))`
!!!(p) {:.unnumlist}

`   #(#(NIL NIL (C (1 #5#)) (0))`
!!!(p) {:.unnumlist}

`    #(NIL NIL NIL (0)) NIL (1 #6#))`
!!!(p) {:.unnumlist}

`   (C (2 #4# #2#) B (1 #1#))`
!!!(p) {:.unnumlist}

`   (1 #3#))`
!!!(p) {:.unnumlist}

`  #(NIL NIL NIL (0))`
!!!(p) {:.unnumlist}

`  NIL (0))`
!!!(p) {:.unnumlist}

` NIL (0))`
!!!(p) {:.unnumlist}

` NIL (0)))`
!!!(p) {:.unnumlist}

The next step is to fetch matches from the dtree data base.
The function `fetch` takes a query, which must be a valid relation, as its argument, and returns a list of possible matches.
It calls `dtree-fetch` to do the work:

[ ](#){:#l0170}`(defun fetch (query)`
!!!(p) {:.unnumlist}

` "Return a list of buckets potentially matching the query,`
!!!(p) {:.unnumlist}

` which must be a relation of form (predicate .
args)."`
!!!(p) {:.unnumlist}

` (dtree-fetch query (get-dtree (predicate query))`
!!!(p) {:.unnumlist}

`   nil 0 nil most-positive-fixnum))`
!!!(p) {:.unnumlist}

`dtree-fetch` must be passed the query and the dtree, of course, but it is also passed four additional arguments.
First, we have to accumulate matches indexed under variables as we are searching through the dtree.
So two arguments are used to pass the actual matches and a count of their total number.
Second, we want `dtree-fetch` to return the shortest possible index, so we pass it the shortest answer found so far, and the size of the shortest answer.
That way, as it is making its way down the tree, accumulating values indexed under variables, it can be continually comparing the size of the evolving answer with the best answer found so far.

We could use nlists to pass around count/values pairs, but nlists only support a push operation, where one new item is added.
We need to append together lists of values coming from the variable indices with values indexed under an atom.
Append is expensive, so instead we make a list-of-lists and keep the count in a separate variable.
When we are done, `dtree-fetch` and hence `fetch` does a multiple-value return, yielding the list-of-lists and the total count.

There are four cases to consider in `dtree-fetch.` If the dtree is null or the query pattern is either null or a variable, then nothing will be indexed, so we should just return the best answer found so far.
Otherwise, we bind `var-n` and `var-list` to the count and list-of-lists of variable matches found so far, including at the current node.
If the count `var-n` is greater than the best count so far, then there is no sense continuing, and we return the best answer found.
Otherwise we look at the query pattern.
If it is an atom, we use `dtree-atom-fetch` to return either the current index (along with the accumulated variable index) or the accumulated best answer, whichever is shorter.
If the query is a cons, then we use `dtree-fetch` on the first part of the cons, yielding a new best answer, which is passed along to the call of `dtree-fetch` on the rest of the cons.

[ ](#){:#l0175}`(defun dtree-fetch (pat dtree var-list-in var-n-in best-list best-n)`
!!!(p) {:.unnumlist}

` "Return two values: a list-of-lists of possible matches to pat.`
!!!(p) {:.unnumlist}

` and the number of elements in the list-of-lists."`
!!!(p) {:.unnumlist}

` (if (or (null dtree) (null pat) (variable-p pat))`
!!!(p) {:.unnumlist}

`  (values best-list best-n)`
!!!(p) {:.unnumlist}

`  (let* ((var-nlist (dtree-var dtree))`
!!!(p) {:.unnumlist}

`    (var-n (+ var-n-in (nlist-n var-nlist)))`
!!!(p) {:.unnumlist}

`    (var-list (if (null (nlist-list var-nlist))`
!!!(p) {:.unnumlist}

`       var-list-in`
!!!(p) {:.unnumlist}

`       (cons (nlist-list var-nlist)`
!!!(p) {:.unnumlist}

`        var-list-in))))`
!!!(p) {:.unnumlist}

`   (cond`
!!!(p) {:.unnumlist}

`   ((>= var-n best-n) (values best-list best-n))`
!!!(p) {:.unnumlist}

`   ((atom pat) (dtree-atom-fetch pat dtree var-list var-n`
!!!(p) {:.unnumlist}

`        best-list best-n))`
!!!(p) {:.unnumlist}

`   (t (multiple-value-bind (listl n1)`
!!!(p) {:.unnumlist}

`     (dtree-fetch (first pat) (dtree-first dtree)`
!!!(p) {:.unnumlist}

`         var-list var-n best-list best-n)`
!!!(p) {:.unnumlist}

`        (dtree-fetch (rest pat) (dtree-rest dtree)`
!!!(p) {:.unnumlist}

`           var-list var-n listl ni)))))))`
!!!(p) {:.unnumlist}

`(defun dtree-atom-fetch (atom dtree var-list var-n best-list best-n)`
!!!(p) {:.unnumlist}

` "Return the answers indexed at this atom (along with the vars),`
!!!(p) {:.unnumlist}

` or return the previous best answer, if it is better."`
!!!(p) {:.unnumlist}

` (let ((atom-nlist (lookup atom (dtree-atoms dtree))))`
!!!(p) {:.unnumlist}

`  (cond`
!!!(p) {:.unnumlist}

`   ((or (null atom-nlist) (null (nlist-list atom-nlist)))`
!!!(p) {:.unnumlist}

`    (values var-list var-n))`
!!!(p) {:.unnumlist}

`   ((and atom-nlist (< (incf var-n (nlist-n atom-nlist)) best-n))`
!!!(p) {:.unnumlist}

`    (values (cons (nlist-list atom-nlist) var-list) var-n))`
!!!(p) {:.unnumlist}

`   (t (values best-list best-n)))))`
!!!(p) {:.unnumlist}

Here we see a call to `fetch` on the data base created by `test-index`.
It returns two values: a list-of-lists of facts, and the total number of facts, three.

[ ](#){:#l0180}`(fetch '(p ?
c))`
!!!(p) {:.unnumlist}

`(((P B C) (P A C))`
!!!(p) {:.unnumlist}

` ((P A ?X)))`
!!!(p) {:.unnumlist}

`3`
!!!(p) {:.unnumlist}

Now let's stop and see what we have accomplished.
The functions `fetch and dtree-fetch` fulfill their contract of returning potential matches.
However, we still need to integrate the dtree facility with Prolog.
We need to go through the potential matches and determine which candidates are actual matches.
For simplicity we will use the version of `unify` with binding lists defined in [section 11.2](B978008057115750011X.xhtml#s0020).
(It is also possible to construct a more efficient version that uses the compiler and the destructive function `unify!`.)

The function `mapc-retrieve` calls `fetch` to get a list-of-lists of potential matches and then calls `unify` to see if the match is a true one.
If the match is true, it calls the supplied function with the binding list that represents the unification as the argument, `mapc-retrieve` is proclaimed `inline` so that functions passed to it can also be compiled in place.

[ ](#){:#l0185}`(proclaim '(inline mapc-retrieve))`
!!!(p) {:.unnumlist}

`(defun mapc-retrieve (fn query)`
!!!(p) {:.unnumlist}

` "For every fact that matches the query.`
!!!(p) {:.unnumlist}

` apply the function to the binding list."`
!!!(p) {:.unnumlist}

` (dolist (bucket (fetch query))`
!!!(p) {:.unnumlist}

` (dolist (answer bucket)`
!!!(p) {:.unnumlist}

`  (let ((bindings (unify query answer)))`
!!!(p) {:.unnumlist}

`   (unless (eq bindings fall)`
!!!(p) {:.unnumlist}

`   (funcall fn bindings))))))`
!!!(p) {:.unnumlist}

There are many ways to use this retriever.
The function `retrieve` returns a list of the matching binding lists, and `retrieve-matches` substitutes each binding list into the original query so that the resuit is a list of expressions that unify with the query.

[ ](#){:#l0190}`(defun retrieve (query)`
!!!(p) {:.unnumlist}

` "Find all facts that match query.
Return a list of bindings."`
!!!(p) {:.unnumlist}

` (let ((answers nil))`
!!!(p) {:.unnumlist}

` (mapc-retrieve #'(lambda (bindings) (push bindings answers))`
!!!(p) {:.unnumlist}

`     query)`
!!!(p) {:.unnumlist}

` answers))`
!!!(p) {:.unnumlist}

`(defun retrieve-matches (query)`
!!!(p) {:.unnumlist}

` "Find all facts that match query.`
!!!(p) {:.unnumlist}

` Return a list of expressions that match the query."`
!!!(p) {:.unnumlist}

` (mapcar #'(lambda (bindings) (subst-bindings bindings query))`
!!!(p) {:.unnumlist}

`   (retrieve query)))`
!!!(p) {:.unnumlist}

There is one further complication to consider.
Recall that in our original Prolog interpreter, the function prove had to rename the variables in each clause as it retrieved it from the data base.
This was to insure that there was no conflict between the variables in the query and the variables in the clause.
We could do that in `retrieve`.
However, if we assume that the expressions indexed in discrimination trees are tablelike rather than rulelike and thus are not recursive, then we can get away with renaming the variables only once, when they are entered into the data base.
This is done by changing `index`:

[ ](#){:#l0195}`(defun index (key)`
!!!(p) {:.unnumlist}

` "Store key in a dtree node.
Key must be (predicate .
args);`
!!!(p) {:.unnumlist}

` it is stored in the predicate's dtree."`
!!!(p) {:.unnumlist}

` (dtree-index key (rename-variables key) ; store unique vars`
!!!(p) {:.unnumlist}

`     (get-dtree (predicate key))))`
!!!(p) {:.unnumlist}

With the new `index` in place, and after calling `test-index` to rebuild the data base, we are now ready to test the retrieval mechanism:

[ ](#){:#l0200}`> (fetch '(p ?x c))`
!!!(p) {:.unnumlist}

`(((P B C) (P A C))`
!!!(p) {:.unnumlist}

` ((P A ?X3408)))`
!!!(p) {:.unnumlist}

`3`
!!!(p) {:.unnumlist}

`> (retrieve '(p ?x c))`
!!!(p) {:.unnumlist}

`(((?X3408 .
C) (?X .
A))`
!!!(p) {:.unnumlist}

` ((?X .
A))`
!!!(p) {:.unnumlist}

` ((?X .
B)))`
!!!(p) {:.unnumlist}

`> (retrieve-matches '(p ?x c))`
!!!(p) {:.unnumlist}

`((P A C) (P A C) (P B C))`
!!!(p) {:.unnumlist}

`> (retrieve-matches '(p ?x (?fn c)))`
!!!(p) {:.unnumlist}

`((P A (?FN C)) (P A (F C)) (P B (F C)))`
!!!(p) {:.unnumlist}

Actually, it is better to use `mapc-retrieve` when possible, since it doesn't cons up answers the way `retrieve` and `retrieve-matches` do.
The macro `query-bind` is provided as a nice interface to `mapc-retrieve`.
The macro takes as arguments a list of variables to bind, a query, and one or more forms to apply to each retrieved answer.
Within this list of forms, the variables will be bound to the values that satisfy the query.
The syntax was chosen to be the same as `multiple-va1ue-bind`.
Here we see a typical use of `query-bind`, its resuit, and its macro-expansion:

[ ](#){:#l0205}`> (query-bind (?x ?fn) '(p ?x (?fn c))`
!!!(p) {:.unnumlist}

` (format t "~&P holds between ~a and ~a of c." ?x ?fn))`⇒
!!!(p) {:.unnumlist}

`P holds between B and F of c.`
!!!(p) {:.unnumlist}

`P holds between A and F of c.`
!!!(p) {:.unnumlist}

`P holds between A and ?FN of c.`
!!!(p) {:.unnumlist}

`NIL`
!!!(p) {:.unnumlist}

`≡ (mapc-retrieve`
!!!(p) {:.unnumlist}

` #'(lambda (#:bindings6369)`
!!!(p) {:.unnumlist}

`  (let ((?x (subst-bindings #:bindings6369 *?x))`
!!!(p) {:.unnumlist}

`      (?fn (subst-bindings #:bindings6369 *?fn)))`
!!!(p) {:.unnumlist}

`   (format t "~&P holds between ~a and ~a of c." ?x ?fn)))`
!!!(p) {:.unnumlist}

` '(p ?x (?fn c)))`
!!!(p) {:.unnumlist}

Here is the implementation:

[ ](#){:#l0210}`(defmacro query-bind (variables query &body body)`
!!!(p) {:.unnumlist}

` "Execute the body for each match to the query.`
!!!(p) {:.unnumlist}

` Within the body, bind each variable."`
!!!(p) {:.unnumlist}

` (let* ((bindings (gensym "BINDINGS"))`
!!!(p) {:.unnumlist}

`  (vars-and-vals`
!!!(p) {:.unnumlist}

`   (mapcar`
!!!(p) {:.unnumlist}

`    #'(lambda (var)`
!!!(p) {:.unnumlist}

`     (list var '(subst-bindings ,bindings * ,var)))`
!!!(p) {:.unnumlist}

`    variables)))`
!!!(p) {:.unnumlist}

` '(mapc-retrieve`
!!!(p) {:.unnumlist}

`  #'(lambda (,bindings)`
!!!(p) {:.unnumlist}

`   (let ,vars-and-vals`
!!!(p) {:.unnumlist}

`    ,©body))`
!!!(p) {:.unnumlist}

`  ,query)))`
!!!(p) {:.unnumlist}

## [ ](#){:#st0050}14.9 A Solution to the Completeness Problem
{:#s0050}
{:.h1hd}

We saw in [chapter 6](B9780080571157500066.xhtml) that iterative deepening is an efficient way to cover a search space without falling into an infinite loop.
Iterative deepening can also be used to guide the search in Prolog.
It will insure that all valid answers are found eventually, but it won't turn an infinite search space into a finite one.

In the interpreter, iterative deepening is implemented by passing an extra argument to `prove` and `prove-all` to indicate the depth remaining to be searched.
When that argument is zero, the search is eut off, and the proof falls.
On the next iteration the bounds will be increased and the proof may succeed.
If the search is never eut off by a depth bound, then there is no reason to go on to the next iteration, because all proofs have already been found.
The special variable `*search-cut-off*` keeps track of this.

[ ](#){:#l0215}`(defvar *search-cut-off* nil "Has the search been stopped?")`
!!!(p) {:.unnumlist}

`(defun prove-all (goals bindings depth)`
!!!(p) {:.unnumlist}

` "Find a solution to the conjunction of goals."`
!!!(p) {:.unnumlist}

` ;; This version just passes the depth on to PROVE.`
!!!(p) {:.unnumlist}

` (cond ((eq bindings fail) fail)`
!!!(p) {:.unnumlist}

`   ((null goals) bindings)`
!!!(p) {:.unnumlist}

`   (t (prove (first goals) bindings (rest goals) depth))))`
!!!(p) {:.unnumlist}

`(defun prove (goal bindings other-goals depth)`
!!!(p) {:.unnumlist}

` "Return a list of possible solutions to goal."`
!!!(p) {:.unnumlist}

` ;; Check if the depth bound has been exceeded`
!!!(p) {:.unnumlist}

` (if (= depth 0) ;***`
!!!(p) {:.unnumlist}

`  (progn (setf *search-cut-off* t) ;***`
!!!(p) {:.unnumlist}

`    fall) ;***`
!!!(p) {:.unnumlist}

`  (let ((clauses (get-clauses (predicate goal))))`
!!!(p) {:.unnumlist}

`   (if (listp clauses)`
!!!(p) {:.unnumlist}

`    (some`
!!!(p) {:.unnumlist}

`     #'(lambda (clause)`
!!!(p) {:.unnumlist}

`      (let ((new-clause (rename-variables clause)))`
!!!(p) {:.unnumlist}

`       (prove-all`
!!!(p) {:.unnumlist}

`        (append (clause-body new-clause) other-goals)`
!!!(p) {:.unnumlist}

`        (unify goal (clause-head new-clause) bindings)`
!!!(p) {:.unnumlist}

`        (– depth 1)))) ;***`
!!!(p) {:.unnumlist}

`     clauses)`
!!!(p) {:.unnumlist}

`    ;; The predicate's "clauses" can be an atom:`
!!!(p) {:.unnumlist}

`    ;; a primitive function to call`
!!!(p) {:.unnumlist}

`    (funcall clauses (rest goal) bindings`
!!!(p) {:.unnumlist}

`        other-goals depth))))) ;***`
!!!(p) {:.unnumlist}

`prove` and `prove-all` now implement search cutoff, but we need something to control the iterative deepening of the search.
First we define parameters to control the iteration: one for the initial depth, one for the maximum depth, and one for the increment between iterations.
Setting the initial and increment values to one will make the results come out in strict breadth-first order, but will duplicate more effort than a slightly larger value.

[ ](#){:#l0220}`(defparameter *depth-start* 5`
!!!(p) {:.unnumlist}

` "The depth of the first round of iterative search.")`
!!!(p) {:.unnumlist}

`(defparameter *depth-incr* 5`
!!!(p) {:.unnumlist}

` "Increase each iteration of the search by this amount.")`
!!!(p) {:.unnumlist}

`(defparameter *depth-max* most-positive-fixnum`
!!!(p) {:.unnumlist}

` "The deepest we will ever search.")`
!!!(p) {:.unnumlist}

A new version of `top-level-prove` will be used to control the iteration.
It calls `prove-all` for all depths from the starting depth to the maximum depth, increasing by the increment.
However, it only proceeds to the next iteration if the search was eut off at some point in the previous iteration.

[ ](#){:#l0225}`(defun top-level-prove (goals)`
!!!(p) {:.unnumlist}

` (let ((all-goals`
!!!(p) {:.unnumlist}

`   '(,@goals (show-prolog-vars ,@(variables-in goals)))))`
!!!(p) {:.unnumlist}

`  (loop for depth from *depth-start* to *depth-max* by *depth-incr*`
!!!(p) {:.unnumlist}

`   while (let ((*search-cut-off* nil))`
!!!(p) {:.unnumlist}

`    (prove-all all-goals no-bindings depth)`
!!!(p) {:.unnumlist}

`    *search-cut-off*)))`
!!!(p) {:.unnumlist}

` (format t "~&No.")`
!!!(p) {:.unnumlist}

` (values))`
!!!(p) {:.unnumlist}

There is one final complication.
When we increase the depth of search, we may find some new proof s, but we will also find all the old proof s that were found on the previous iteration.
We can modify `show-prolog-vars` to only print proofs that are found with a depth less than the increment–that is, those that were not found on the previous iteration.

[ ](#){:#l0230}`(defun show-prolog-vars (vars bindings other-goals depth)`
!!!(p) {:.unnumlist}

` "Print each variable with its binding.`
!!!(p) {:.unnumlist}

` Then ask the user if more solutions are desired."`
!!!(p) {:.unnumlist}

` (if (> depth *depth-incr*)`
!!!(p) {:.unnumlist}

`  fall`
!!!(p) {:.unnumlist}

`  (progn`
!!!(p) {:.unnumlist}

`   (if (null vars)`
!!!(p) {:.unnumlist}

`    (format t "~&Yes")`
!!!(p) {:.unnumlist}

`    (dolist (var vars)`
!!!(p) {:.unnumlist}

`     (format t "~&~a = ~a" var`
!!!(p) {:.unnumlist}

`      (subst-bindings bindings var))))`
!!!(p) {:.unnumlist}

`   (if (continue-p)`
!!!(p) {:.unnumlist}

`    fall`
!!!(p) {:.unnumlist}

`    (prove-all other-goals bindings depth)))))`
!!!(p) {:.unnumlist}

To test that this works, try setting `*depth-max*` to 5 and running the following assertions and query.
The infinite loop is avoided, and the first four solutions are found.

[ ](#){:#l0235}`(<- (natural 0))`
!!!(p) {:.unnumlist}

`(<- (natural (1 + ?n)) (natural ?n))`
!!!(p) {:.unnumlist}

`> (?- (natural ?n))`
!!!(p) {:.unnumlist}

`?N = 0;`
!!!(p) {:.unnumlist}

`?N = (1 + 0);`
!!!(p) {:.unnumlist}

`?N = (1 + (1 + 0));`
!!!(p) {:.unnumlist}

`?N = (1 + (1 + (1 + 0)));`
!!!(p) {:.unnumlist}

`No.`
!!!(p) {:.unnumlist}

## [ ](#){:#st0055}14.10 Solutions to the Expressiveness Problems
{:#s0055}
{:.h1hd}

In this section we present solutions to three of the limitations described above:

* [ ](#){:#l0240}• Treatment of (limited) higher-order predications.

* • Introduction of a frame-based syntax.

* • Support for possible worlds, negation, and disjunction.

We also introduce a way to attach functions to predicates to do forward-chaining and error detection, and we discuss ways to extend unification to handle Skolem constants and other problems.

### [ ](#){:#st9055}Higher-Order Predications
{:#s9055}
{:.h2hd}

First we will tackle the problem of answering questions like "What kinds of animais are there?" Paradoxically, the key to allowing more expressiveness in this case is to invent a new, more limited language and insist that all assertions and queries are made in that language.
That way, queries that would have been higher-order in the original language become first-order in the restricted language.

The language admits three types of objects: *categories, relations*, and *individuals.* A category corresponds to a one-place predicate, a relation to a two-place predicate, and an individual to constant, or zero-place predicate.
Statements in the language must have one of five primitive operators: `sub, rel, ind, val`, and `and.` They have the following form:

[ ](#){:#l0245}`(sub`*subcategory super category*)
!!!(p) {:.unnumlist}

`(rel`*relation domain-category range-category*)
!!!(p) {:.unnumlist}

`(ind`*individual category*)
!!!(p) {:.unnumlist}

`(val`*relation individual value*)
!!!(p) {:.unnumlist}

`(and`*assertion…*)
!!!(p) {:.unnumlist}

The following table gives some examples, along with English translations:

[ ](#){:#t0015}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `(sub dog animal)` | Dog is a kind of animal. |
| `(rel birthday animal date)` | The birthday relation holds between each animal and some date. |
| `(ind fido dog)` | The individual Fido is categorized as a dog. |
| `(val birthday fido july-1)` | The birthday of Fido is July-1. |
| `(and *AB*)` | Both *A* and *B* are true. |

For those who feel more comfortable with predicate calculus, the following table gives the formal definition of each primitive.
The most complicated definition is for rel.
The form (rel *R A B*) means that every *R* holds between an individual of *A* and an individual of *B,* and furthermore that every individual of *A* participates in at least one *R* relation.

[ ](#){:#t0020}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `(sub`*AB*) | ∀*x:A*(*x*) ⊃ *B*(*x*) |
| `(rel`*RAB*) | ∀*x,y* : *R*(*x,y*) ⊃ *A*(*x*) A *B*(*y*) *^*∀*xA*(*x*) ⊃ ∃*y* : *R*(*x, y*) |
| `(ind`*IC)* | *C*(*I*) |
| `(val`*RIV*) | *R*(*I, V*) |
| `(and`*P Q…*) | *P ^ Q…* |

Queries in the language, not surprisingly, have the same form as assertions, except that they may contain variables as well as constants.
Thus, to find out what kinds of animais there are, use the query `(sub ?kind animal)`.
To find out what individual animais there are, use the query `(ind ?x animal)`.
To find out what individual animais of what kinds there are, use:

[ ](#){:#l0260}`(and (sub ?kind animal) (ind ?x ?kind))`
!!!(p) {:.unnumlist}

The implemention of this new language can be based directly on the previous implementation of dtrees.
Each assertion is stored as a fact in a dtree, except that the components of an and assertion are stored separately.
The function `add-fact` does this:

[ ](#){:#l0265}`(defun add-fact (fact)`
!!!(p) {:.unnumlist}

` "Add the fact to the data base."`
!!!(p) {:.unnumlist}

` (if (eq (predicate fact) 'and)`
!!!(p) {:.unnumlist}

`  (mapc #'add-fact (args fact))`
!!!(p) {:.unnumlist}

`  (index fact)))`
!!!(p) {:.unnumlist}

Querying this new data base consists of querying the dtree just as before, but with a special case for conjunctive (and) queries.
Conceptually, the function to do this, `retrieve-fact`, should be as simple as the following:

[ ](#){:#l0270}`(defun retrieve-fact (query)`
!!!(p) {:.unnumlist}

` "Find all facts that match query.
Return a list of bindings.`
!!!(p) {:.unnumlist}

` Warning!!
this version is incomplete."`
!!!(p) {:.unnumlist}

` (if (eq (predicate query) 'and)`
!!!(p) {:.unnumlist}

`  (retrieve-conjunction (args query))`
!!!(p) {:.unnumlist}

`  (retrieve query bindings)))`
!!!(p) {:.unnumlist}

Unfortunately, there are some complications.
Think about what must be done in `retrieve-conjunction`.
It is passed a list of conjuncts and must return a list of binding lists, where each binding list satisfies the query.
For example, to find out what people were born on July Ist, we could use the query:

[ ](#){:#l0275}`(and (val birthday ?p july-1) (ind ?p person))`
!!!(p) {:.unnumlist}

`retrieve-conjunction` could solve this problem by first calling `retrieve-fact` on `(val birthday ?p july-1)`.
Once that is done, there is only one conjunct remaining, but in general there could be several, so we need to call `retrieve-conjunction` recursively with two arguments: the remainingconjuncts, and the resultthat `retrieve-fact` gave for the first solution.
Since `retrieve-fact` returns a list of binding lists, it will be easiest if `retrieve-conjunction` accepts such a list as its second argument.
Furthermore, when it cornes time to call `retrieve-fact` on the second conjunct, we will want to respect the bindings set up by the first conjunct.
So `retrieve-fact` must accept a binding list as its second argument.
Thus we have:

[ ](#){:#l0280}`(defun retrieve-fact (query &optional (bindings no-bindings))`
!!!(p) {:.unnumlist}

` "Find all facts that match query.
Return a list of bindings."`
!!!(p) {:.unnumlist}

` (if (eq (predicate query) 'and)`
!!!(p) {:.unnumlist}

`  (retrieve-conjunction (args query) (list bindings))`
!!!(p) {:.unnumlist}

`  (retrieve query bindings)))`
!!!(p) {:.unnumlist}

`(defun retrieve-conjunction (conjuncts bindings-1ists)`
!!!(p) {:.unnumlist}

` "Return a list of binding lists satisfying the conjuncts."`
!!!(p) {:.unnumlist}

` (mapcan`
!!!(p) {:.unnumlist}

`  #'(lambda (bindings)`
!!!(p) {:.unnumlist}

`   (cond ((eq bindings fall) nil)`
!!!(p) {:.unnumlist}

`    ((null conjuncts) (list bindings))`
!!!(p) {:.unnumlist}

`    (t (retrieve-conjunction`
!!!(p) {:.unnumlist}

`     (rest conjuncts)`
!!!(p) {:.unnumlist}

`     (retrieve-fact`
!!!(p) {:.unnumlist}

`      (subst-bindings bindings (first conjuncts))`
!!!(p) {:.unnumlist}

`      bindings)))))`
!!!(p) {:.unnumlist}

`  bindings-1ists))`
!!!(p) {:.unnumlist}

Notice that `retrieve` and therefore `mapc-retrieve` now also must accept a binding list.
The changes to them are shown in the following.
In each case the extra argument is made optional so that previously written functions that call these functions without passing in the extra argument will still work.

[ ](#){:#l0285}`(defun mapc-retrieve (fn query &optional (bindings no-bindings))`
!!!(p) {:.unnumlist}

` "For every fact that matches the query,`
!!!(p) {:.unnumlist}

` apply the function to the binding list."`
!!!(p) {:.unnumlist}

` (dolist (bucket (fetch query))`
!!!(p) {:.unnumlist}

`  (dolist (answer bucket)`
!!!(p) {:.unnumlist}

`   (let ((new-bindings (unify query answer bindings)))`
!!!(p) {:.unnumlist}

`    (unless (eq new-bindings fall)`
!!!(p) {:.unnumlist}

`     (funcall fn new-bindings))))))`
!!!(p) {:.unnumlist}

`(defun retrieve (query &optional (bindings no-bindings))`
!!!(p) {:.unnumlist}

` "Find all facts that match query.
Return a list of bindings."`
!!!(p) {:.unnumlist}

` (let ((answers nil))`
!!!(p) {:.unnumlist}

`  (mapc-retrieve #'(lambda (bindings) (push bindings answers))`
!!!(p) {:.unnumlist}

`       query bindings)`
!!!(p) {:.unnumlist}

`  answers))`
!!!(p) {:.unnumlist}

Now `add-fact` and `retrieve-fact` comprise all we need to implement the language.
Here is a short example where `add-fact` is used to add facts about bears and dogs, both as individuals and as species:

[ ](#){:#l0290}`> (add-fact '(sub dog animal))`⇒ `T`
!!!(p) {:.unnumlist}

`> (add-fact '(sub bear animal))`⇒ `T`
!!!(p) {:.unnumlist}

`> (add-fact '(ind Fido dog))`⇒ `T`
!!!(p) {:.unnumlist}

`> (add-fact '(ind Yogi bear))`⇒ `T`
!!!(p) {:.unnumlist}

`> (add-fact '(val color Yogi brown))`⇒ `T`
!!!(p) {:.unnumlist}

`> (add-fact '(val color Fido golden))`⇒ `T`
!!!(p) {:.unnumlist}

`> (add-fact '(val latin-name bear ursidae))`⇒ `T`
!!!(p) {:.unnumlist}

`> (add-fact '(val latin-name dog canis-familiaris))`⇒ `T`
!!!(p) {:.unnumlist}

Now `retrieve-fact` is used to answer three questions: What kinds of animais are there?
What are the Latin names of each kind of animal?
and What are the colors of each individual bear?

[ ](#){:#l0295}`> (retrieve-fact '(sub ?kind animal))`
!!!(p) {:.unnumlist}

`(((?KIND .
DOG))`
!!!(p) {:.unnumlist}

`((?KIND .
BEAR)))`
!!!(p) {:.unnumlist}

`> (retrieve-fact '(and (sub ?kind animal)`
!!!(p) {:.unnumlist}

`     (val latin-name ?kind ?latin)))`
!!!(p) {:.unnumlist}

`(((?LATIN .
CANIS-FAMILIARIS) (?KIND .
DOG))`
!!!(p) {:.unnumlist}

` ((?LATIN .
URSIDAE) (?KIND .
BEAR)))`
!!!(p) {:.unnumlist}

`> (retrieve-fact '(and (ind ?x bear) (val color ?x ?c)))`
!!!(p) {:.unnumlist}

`(((?C .
BROWN) (?X .
YOGI)))`
!!!(p) {:.unnumlist}

### [ ](#){:#st9010}Improvements
{:#s9010}
{:.h2hd}

There are quite a few improvements that can be made to this system.
One direction is to provide different kinds of answers to queries.
The following two functions are similar to `retrieve-matches` in that they return lists of solutions that match the query, rather than lists of possible bindings:

[ ](#){:#l0300}`(defun retrieve-bagof (query)`
!!!(p) {:.unnumlist}

` "Find all facts that match query.`
!!!(p) {:.unnumlist}

` Return a list of queries with bindings filled in."`
!!!(p) {:.unnumlist}

` (mapcar #'(lambda (bindings) (subst-bindings bindings query))`
!!!(p) {:.unnumlist}

`   (retrieve-fact query)))`
!!!(p) {:.unnumlist}

`(defun retrieve-setof (query)`
!!!(p) {:.unnumlist}

` "Find all facts that match query.`
!!!(p) {:.unnumlist}

` Return a list of unique queries with bindings filled in."`
!!!(p) {:.unnumlist}

` (remove-duplicates (retrieve-bagof query) :test #'equal))`
!!!(p) {:.unnumlist}

Another direction to take is to provide better error checking.
The current system does not complain if a fact or query is ill-formed.
It also relies on the user to input all facts, even those that could be derived automatically from the semantics of existing facts.
For example, the semantics of `sub` imply that if `(sub bear animal)` and `(sub polar-bear bear)` are true, then `(sub polar-bear animal)` must also be true.
This kind of implication can be handled in two way s.
The typical Prolog approach would be to write rules that derive the additional `sub` facts by backward-chaining.
Then every query would have to check if there were rules to run.
The alternative is to use a *forward-chaining* approach, which caches each new `sub` fact by adding it to the data base.
This latter alternative takes more storage, but because it avoids rederiving the same facts over and over again, it tends to be faster.

The following version of `add-fact` does error checking, and it automatically caches facts that can be derived from existing facts.
Both of these things are done by a set of functions that are attached to the primitive operators.
It is done in a data-driven style to make it easier to add new primitives, should that become necessary.

The function `add-fact` checks that each argument to a primitive relation is a nonvariable atom, and it also calls `fact-present-p` to check if the fact is already present in the data base.
If not, it indexes the fact and calls `run-attached-fn` to do additional checking and caching:

[ ](#){:#l0305}`(defparameter *primitives* '(and sub ind rel val))`
!!!(p) {:.unnumlist}

`(defun add-fact (fact)`
!!!(p) {:.unnumlist}

` "Add the fact to the data base."`
!!!(p) {:.unnumlist}

` (cond ((eq (predicate fact) 'and)`
!!!(p) {:.unnumlist}

`   (mapc #'add-fact (args fact)))`
!!!(p) {:.unnumlist}

`  ((or (not (every #'atom (args fact)))`
!!!(p) {:.unnumlist}

`    (some #'variable-p (args fact))`
!!!(p) {:.unnumlist}

`    (not (member (predicate fact) *primitives*)))`
!!!(p) {:.unnumlist}

`   (error "Ill-formed fact: ~a" fact))`
!!!(p) {:.unnumlist}

`  ((not (fact-present-p fact))`
!!!(p) {:.unnumlist}

`   (index fact)`
!!!(p) {:.unnumlist}

`   (run-attached-fn fact)))`
!!!(p) {:.unnumlist}

` t)`
!!!(p) {:.unnumlist}

`(defun fact-present-p (fact)`
!!!(p) {:.unnumlist}

` "Is this fact present in the data base?"`
!!!(p) {:.unnumlist}

` (retrieve fact))`
!!!(p) {:.unnumlist}

The attached functions are stored on the operator's property list under the indicator `attached-fn:`

[ ](#){:#l0310}`(defun run-attached-fn (fact)`
!!!(p) {:.unnumlist}

` "Run the function associated with the predicate of this fact."`
!!!(p) {:.unnumlist}

` (apply (get (predicate fact) 'attached-fn) (args fact)))`
!!!(p) {:.unnumlist}

`(defmacro def-attached-fn (pred args &body body)`
!!!(p) {:.unnumlist}

` "Define the attached function for a primitive."`
!!!(p) {:.unnumlist}

` '(setf (get '.pred 'attached-fn)`
!!!(p) {:.unnumlist}

`   #'(lambda .args ..body)))`
!!!(p) {:.unnumlist}

The attached functions for `ind` and `val` are fairly simple.
If we know `(sub bear animal)`, then when `(ind Yogi bear)` is asserted, we have to also assert `(ind Yogi animal)`.
Similarly, the values in a `val` assertion must be individuals of the categories in the relation's `rel` assertion.
That is, if `(rel birthday animal date)` is a fact and `(val birthday Lee july-1)` is added, then we can conclude `(ind Lee animal)` and `(ind july-1 date).` The following functions add the appropriate facts:

[ ](#){:#l0315}`(def-attached-fn ind (individual category)`
!!!(p) {:.unnumlist}

` ;; Cache facts about inherited categories`
!!!(p) {:.unnumlist}

` (query-bind (?super) '(sub .category ?super)`
!!!(p) {:.unnumlist}

`  (add-fact '(ind .individual .?super))))`
!!!(p) {:.unnumlist}

`(def-attached-fn val (relation indl ind2)`
!!!(p) {:.unnumlist}

` ;; Make sure the individuals are the right kinds`
!!!(p) {:.unnumlist}

` (query-bind (?cat1 ?cat2) '(rel .relation ?cat1 ?cat2)`
!!!(p) {:.unnumlist}

`  (add-fact '(ind .ind1 .?cat1))`
!!!(p) {:.unnumlist}

`  (add-fact '(ind .ind2 .?cat2))))`
!!!(p) {:.unnumlist}

The attached function for rel simply runs the attached function for any individual of the given relation.
Normally one would make all `rel` assertions before `ind` assertions, so this will have no effect at all.
But we want to be sure the data base stays consistent even if facts are asserted in an unusual order.

[ ](#){:#l0320}`(def-attached-fn rel (relation cat1 cat2)`
!!!(p) {:.unnumlist}

` ;; Run attached function for any IND's of this relation`
!!!(p) {:.unnumlist}

` (query-bind (?a ?b) '(ind .relation ?a ?b)`
!!!(p) {:.unnumlist}

`  (run-attached-fn '(ind .relation .?a .?b))))`
!!!(p) {:.unnumlist}

The most complicated attached function is for `sub`.
Adding a fact such as `(sub bear animal)` causes the following to happen:

* [ ](#){:#l0325}• All of `animal`'s supercategories (such as `living-thing)` become supercategories of all of `bear`'s subcategories (such as `polar-bear)`.

* • `animal` itself becomes a supercategory all of `bear`'s subcategories.

* • bear itself becomes a subcategory of all of `animal`'s supercategories.

* • All of the individuals of bear become individuals of `animal` and its supercategories.

The following accomplishes these four tasks.
It does it with four calls to `index-new-fact`, which is used instead of `add-fact` because we don't need to run the attached function on the new facts.
We do, however, need to make sure that we aren't indexing the same fact twice.

[ ](#){:#l0330}`(def-attached-fn sub (subcat supercat)`
!!!(p) {:.unnumlist}

` ;; Cache SUB facts`
!!!(p) {:.unnumlist}

` (query-bind (?super-super) '(sub ,supercat ?super-super)`
!!!(p) {:.unnumlist}

`  (index-new-fact '(sub ,subcat ,?super-super))`
!!!(p) {:.unnumlist}

`  (query-bind (?sub-sub) '(sub ?sub-sub ,subcat)`
!!!(p) {:.unnumlist}

`   (index-new-fact '(sub ,?sub-sub ,?super-super))))`
!!!(p) {:.unnumlist}

` (query-bind (?sub-sub) '(sub ?sub-sub ,subcat)`
!!!(p) {:.unnumlist}

`  (index-new-fact '(sub ,?sub-sub ,supercat)))`
!!!(p) {:.unnumlist}

` ;; Cache IND facts`
!!!(p) {:.unnumlist}

` (query-bind (?super-super) '(sub ,subcat ?super-super)`
!!!(p) {:.unnumlist}

`  (query-bind (?sub-sub) '(sub ?sub-sub ,supercat)`
!!!(p) {:.unnumlist}

`   (query-bind (?ind) '(ind ?ind ,?sub-sub)`
!!!(p) {:.unnumlist}

`    (index-new-fact '(ind ,?ind ,?super-super))))))`
!!!(p) {:.unnumlist}

`(defun index-new-fact (fact)`
!!!(p) {:.unnumlist}

` "Index the fact in the data base unless it is already there."`
!!!(p) {:.unnumlist}

` (unless (fact-present-p fact)`
!!!(p) {:.unnumlist}

`  (index fact)))`
!!!(p) {:.unnumlist}

The following function tests the attached functions.
It shows that adding the single fact `(sub bear animal)` to the given data base causes 18 new facts to be added.

[ ](#){:#l0335}`(defun test-bears ()`
!!!(p) {:.unnumlist}

` (clear-dtrees)`
!!!(p) {:.unnumlist}

` (mapc #'add-fact`
!!!(p) {:.unnumlist}

`   '((sub animal living-thing)`
!!!(p) {:.unnumlist}

`    (sub living-thing thing) (sub polar-bear bear)`
!!!(p) {:.unnumlist}

`    (sub grizzly bear) (ind Yogi bear) (ind Lars polar-bear)`
!!!(p) {:.unnumlist}

`    (ind Helga grizzly)))`
!!!(p) {:.unnumlist}

` (trace index)`
!!!(p) {:.unnumlist}

` (add-fact '(sub bear animal))`
!!!(p) {:.unnumlist}

` (untrace index))`
!!!(p) {:.unnumlist}

`>(test-bears)`
!!!(p) {:.unnumlist}

`(1 ENTER INDEX: (SUB BEAR ANIMAL))`
!!!(p) {:.unnumlist}

`(1 EXIT INDEX: T)`
!!!(p) {:.unnumlist}

`(1 ENTER INDEX: (SUB BEAR THING))`
!!!(p) {:.unnumlist}

`(1 EXIT INDEX: T)`
!!!(p) {:.unnumlist}

`(1 ENTER INDEX: (SUB GRIZZLY THING))`
!!!(p) {:.unnumlist}

`(1 EXIT INDEX: T)`
!!!(p) {:.unnumlist}

`(1 ENTER INDEX: (SUB POLAR-BEAR THING))`
!!!(p) {:.unnumlist}

`(1 EXIT INDEX: T)`
!!!(p) {:.unnumlist}

`(1 ENTER INDEX: (SUB BEAR LIVING-THING))`
!!!(p) {:.unnumlist}

`(1 EXIT INDEX: T)`
!!!(p) {:.unnumlist}

`(1 ENTER INDEX: (SUB GRIZZLY LIVING-THING))`
!!!(p) {:.unnumlist}

`(1 EXIT INDEX: T)`
!!!(p) {:.unnumlist}

`(1 ENTER INDEX: (SUB POLAR-BEAR LIVING-THING))`
!!!(p) {:.unnumlist}

`(1 EXIT INDEX: T)`
!!!(p) {:.unnumlist}

`(1 ENTER INDEX: (SUB GRIZZLY ANIMAL))`
!!!(p) {:.unnumlist}

`(1 EXIT INDEX: T)`
!!!(p) {:.unnumlist}

`(1 ENTER INDEX: (SUB POLAR-BEAR ANIMAL))`
!!!(p) {:.unnumlist}

`(1 EXIT INDEX: T)`
!!!(p) {:.unnumlist}

`(1 ENTER INDEX: (IND LARS LIVING-THING))`
!!!(p) {:.unnumlist}

`(1 EXIT INDEX: T)`
!!!(p) {:.unnumlist}

`(1 ENTER INDEX: (IND HELGA LIVING-THING)`
!!!(p) {:.unnumlist}

`(1 EXIT INDEX: T)`
!!!(p) {:.unnumlist}

`(1 ENTER INDEX: (IND YOGI LIVING-THING))`
!!!(p) {:.unnumlist}

`(1 EXIT INDEX: T)`
!!!(p) {:.unnumlist}

`(1 ENTER INDEX: (IND LARS THING))`
!!!(p) {:.unnumlist}

`(1 EXIT INDEX: T)`
!!!(p) {:.unnumlist}

`(1 ENTER INDEX: (IND HELGA THING))`
!!!(p) {:.unnumlist}

`(1 EXIT INDEX: T)`
!!!(p) {:.unnumlist}

`(1 ENTER INDEX: (IND YOGI THING))`
!!!(p) {:.unnumlist}

`(1 EXIT INDEX: T)`
!!!(p) {:.unnumlist}

`(1 ENTER INDEX: (IND LARS ANIMAL))`
!!!(p) {:.unnumlist}

`(1 EXIT INDEX: T)`
!!!(p) {:.unnumlist}

`(1 ENTER INDEX: (IND HELGA ANIMAL))`
!!!(p) {:.unnumlist}

`(1 EXIT INDEX: T)`
!!!(p) {:.unnumlist}

`(1 ENTER INDEX: (IND YOGI ANIMAL))`
!!!(p) {:.unnumlist}

`(1 EXIT INDEX: T)`
!!!(p) {:.unnumlist}

`(INDEX)`
!!!(p) {:.unnumlist}

### [ ](#){:#st9015}A Frame Language
{:#s9015}
{:.h2hd}

Another direction we can take is to provide an alternative syntax that will be easier to read and write.
Many representation languages are based on the idea of *frames,* and their syntax reflects this.
A frame is an object with slots.
We will continue to use the same data base in the same format, but we will provide an alternative syntax that consider s the individuals and categories as frames, and the relations as slots.

Here is an example of the frame syntax for individuals, which uses the operator a.
Note that it is more compact than the equivalent notation using the primitives.

[ ](#){:#l0340}`(a person (name Joe) (age 27)) ≡`
!!!(p) {:.unnumlist}

`(and (ind person1 person)`
!!!(p) {:.unnumlist}

` (val name person1 Joe)`
!!!(p) {:.unnumlist}

` (val age person1 27))`
!!!(p) {:.unnumlist}

The syntax also allows for nested expressions to appear as the values of slots.
Notice that the Skolem constant `person1` was generated automatically; an alternative is to supply a constant for the individual after the category name.
For example, the following says that Joe is a person of age 27 whose best friend is a person named Fran who is 28 and whose best friend is Joe:

[ ](#){:#l0345}`(a person p1 (name Joe) (age 27)`
!!!(p) {:.unnumlist}

` (best-friend (a person (name Fran) (age 28)`
!!!(p) {:.unnumlist}

`     (best-friend pl)))) ≡`
!!!(p) {:.unnumlist}

`(and (ind p1 person) (val name p1 joe) (val age p1 27)`
!!!(p) {:.unnumlist}

` (ind person2 person) (val name person2 fran)`
!!!(p) {:.unnumlist}

` (val age person2 28) (val best-friend person2 pl)`
!!!(p) {:.unnumlist}

` (val best-friend p1 person2))`
!!!(p) {:.unnumlist}

The frame syntax for categories uses the operator `each`.
For example:

[ ](#){:#l0350}`(each person (isa animal) (name person-name) (age integer)) ≡`
!!!(p) {:.unnumlist}

`(and (sub person animal)`
!!!(p) {:.unnumlist}

` (rel name person person-name)`
!!!(p) {:.unnumlist}

` (rel age person integer))`
!!!(p) {:.unnumlist}

The syntax for queries is the same as for assertions, except that variables are used instead of the Skolem constants.
This is true even when the Skolem constants are automatically generated, as in the following query:

[ ](#){:#l0355}`(a person (age 27)) ≡ (AND (IND ?3 PERSON) (VAL AGE ?3 27))`
!!!(p) {:.unnumlist}

To support the frame notation, we define the macros `a` and `each` to make assertions and `??` to make queries.

[ ](#){:#l0360}`(defmacro a (&rest args)`
!!!(p) {:.unnumlist}

` "Define a new individual and assert facts about it in the data base."`
!!!(p) {:.unnumlist}

` '(add-fact ',(translate-exp (cons 'a args))))`
!!!(p) {:.unnumlist}

`(defmacro each (&rest args)`
!!!(p) {:.unnumlist}

` "Define a new category and assert facts about it in the data base."`
!!!(p) {:.unnumlist}

` '(add-fact ',(transiate-exp (cons 'each args))))`
!!!(p) {:.unnumlist}

`(defmacro ??
(&rest queries)`
!!!(p) {:.unnumlist}

` "Return a list of answers satisfying the query or queries."`
!!!(p) {:.unnumlist}

` '(retrieve-setof`
!!!(p) {:.unnumlist}

`  '.(translate-exp (maybe-add 'and (replace-?-vars queries))`
!!!(p) {:.unnumlist}

`     :query)))`
!!!(p) {:.unnumlist}

All three of these macros call on `translate-exp` to translate from the frame syntax to the primitive syntax.
Note that an `a` or `each` expression is Computing a conjunction of primitive relations, but it is also Computing a *term* when it is used as the nested value of a slot.
It would be possible to do this by returning multiple values, but it is easier to build `transiate-exp` as a set of local functions that construct facts and push them on the local variable `conjuncts`.
At the end, the list of `conjuncts` is returned as the value of the translation.
The local functions `transiate-a` and `transiate-each` return the atom that represents the term they are translating.
The local function `translate` translates any kind of expression, `transiate-siot` handles a slot, and `collect-fact` is responsible for pushing a fact onto the list of conjuncts.
The optional argument `query-mode-p` tells what to do if the individual is not provided in an `a` expression.
If `query-mode-p` is true, the individual will be represented by a variable; otherwise it will be a Skolem constant.

[ ](#){:#l0365}`(defun translate-exp (exp &optional query-mode-p)`
!!!(p) {:.unnumlist}

` "Translate exp into a conjunction of the four primitives."`
!!!(p) {:.unnumlist}

` (let ((conjuncts nil))`
!!!(p) {:.unnumlist}

`  (labels`
!!!(p) {:.unnumlist}

`   ((collect-fact (&rest terms) (push terms conjuncts))`
!!!(p) {:.unnumlist}

`    (translate (exp)`
!!!(p) {:.unnumlist}

`     ;; Figure out what kind of expression this is`
!!!(p) {:.unnumlist}

`     (cond`
!!!(p) {:.unnumlist}

`      ((atom exp) exp)`
!!!(p) {:.unnumlist}

`      ((eq (first exp) 'a) (translate-a (rest exp)))`
!!!(p) {:.unnumlist}

`      ((eq (first exp) 'each) (translate-each (rest exp)))`
!!!(p) {:.unnumlist}

`      (t (apply #'collect-fact exp) exp)))`
!!!(p) {:.unnumlist}

`    (translate-a (args)`
!!!(p) {:.unnumlist}

`     ;; translate (A category [ind] (rel filler)*)`
!!!(p) {:.unnumlist}

`     (let* ((category (pop args))`
!!!(p) {:.unnumlist}

`       (self (cond ((and args (atom (first args)))`
!!!(p) {:.unnumlist}

`         (pop args))`
!!!(p) {:.unnumlist}

`        (query-mode-p (gentemp "?"))`
!!!(p) {:.unnumlist}

`        (t (gentemp (string category))))))`
!!!(p) {:.unnumlist}

`      (collect-fact 'ind self category)`
!!!(p) {:.unnumlist}

`      (dolist (slot args)`
!!!(p) {:.unnumlist}

`       (translate-slot 'val self slot))`
!!!(p) {:.unnumlist}

`      self))`
!!!(p) {:.unnumlist}

`    (translate-each (args)`
!!!(p) {:.unnumlist}

`     ;; translate (EACH category [(isa cat*)] (slot cat)*)`
!!!(p) {:.unnumlist}

`     (let* ((category (pop args)))`
!!!(p) {:.unnumlist}

`      (when (eq (predicate (first args)) 'isa)`
!!!(p) {:.unnumlist}

`       (dolist (super (rest (pop args)))`
!!!(p) {:.unnumlist}

`        (collect-fact 'sub category super)))`
!!!(p) {:.unnumlist}

`      (dolist (slot args)`
!!!(p) {:.unnumlist}

`       (translate-slot 'rel category slot))`
!!!(p) {:.unnumlist}

`      category))`
!!!(p) {:.unnumlist}

`    (translate-slot (primitive self slot)`
!!!(p) {:.unnumlist}

`     ;; translate (relation value) into a REL or SUB`
!!!(p) {:.unnumlist}

`     (assert (= (length slot) 2))`
!!!(p) {:.unnumlist}

`     (collect-fact primitive (first slot) self`
!!!(p) {:.unnumlist}

`         (translate (second slot)))))`
!!!(p) {:.unnumlist}

`   ;; Body of translate-exp:`
!!!(p) {:.unnumlist}

`   (translate exp) ;; Build up the list of conjuncts`
!!!(p) {:.unnumlist}

`   (maybe-add 'and (nreverse conjuncts)))))`
!!!(p) {:.unnumlist}

The auxiliary functions `maybe-add` and `replace-?-vars` are shown in the following:

[ ](#){:#l0370}`(defun maybe-add (op exps &optional if-nil)`
!!!(p) {:.unnumlist}

` "For example, (maybe-add 'and exps t) returns`
!!!(p) {:.unnumlist}

` t if exps is nil, (first exps) if there is only one,`
!!!(p) {:.unnumlist}

` and (and expl exp2…) if there are several exps."`
!!!(p) {:.unnumlist}

` (cond ((null exps) if-nil)`
!!!(p) {:.unnumlist}

`  ((length=1 exps) (first exps))`
!!!(p) {:.unnumlist}

`  (t (cons op exps))))`
!!!(p) {:.unnumlist}

`(defun length=1 (x)`
!!!(p) {:.unnumlist}

` "Is x a list of length 1?"`
!!!(p) {:.unnumlist}

` (and (consp x) (null (cdr x))))`
!!!(p) {:.unnumlist}

`(defun replace-?-vars (exp)`
!!!(p) {:.unnumlist}

` "Replace each ?
in exp with a temporary var: ?123"`
!!!(p) {:.unnumlist}

` (cond ((eq exp '?) (gentemp "?"))`
!!!(p) {:.unnumlist}

`  ((atom exp) exp)`
!!!(p) {:.unnumlist}

`  (t (reuse-cons (replace-?-vars (first exp))`
!!!(p) {:.unnumlist}

`     (replace-?-vars (rest exp))`
!!!(p) {:.unnumlist}

`     exp))))`
!!!(p) {:.unnumlist}

### [ ](#){:#st19055}Possible Worlds: Truth, Negation, and Disjunction
{:#s19055}
{:.h2hd}

In this section we address four problems: distinguishing `unknown` from `false`, representing negations, representing disjunctions, and representing multiple possible states of affairs.
It turns out that all four problems can be solved by introducing two new techniques: possible worlds and negated predicates.
The solution is not completely general, but it is practical in a wide variety of applications.

There are two basic ways to distinguish unknown from false.
The first possibility is to store a truth value–`true` or `false`–along with each proposition.
The second possibility is to include the truth value as part of the proposition.
There are several syntactic variations on this theme.
The following table shows the possibilities for the propositions "Jan likes Dean is true" and "Jan likes Ian is false:"

[ ](#){:#t0025}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| Approach | True Prop. | False Prop. |
| 1) | `(likes Jan Dean) -- true` | `(likes Jan Ian) -- false` |
| (2a) | `(likes true Jan Dean)` | `(likes false Jan Ian)` |
| (2b) | `(likes Jan Dean)` | `(not (likes Jan Dean))` |
| (2c) | `(likes Jan Dean)` | `(~ likes Jan Dean)` |

The difference between (1) and (2) shows up when we want to make a query.
With (1), we make the single query `(likes Jan Dean)` (or perhaps `(likes Jan ?x))`, and the answers will tell us who Jan does and does not like.
With (2), we make one query to find out what liking relationships are true, and another to find out which ones are false.
In either approach, if there are no responses then the answer is truly unknown.

Approach (1) is better for applications where most queries are of the form "Is this sentence true or false?" But applications that include backward-chaining rules are not like this.
The typical backward-chaining rule says "Conclude X is true if Y is true." Thus, most queries will be of the type "Is Y true?" Therefore, some version of approach (2) is preferred.

Representing true and false opens the door to a host of possible extensions.
First, we could add multiple truth values beyond the simple "true" and "false." These could be symbolic values like "probably-true" or "false-by-default" or they could be numeric values representing probabilities or certainty factors.

Second, we could introduce the idea of *possible worlds.* That is, the truth of a proposition could be unknown in the current world, but true if we assume *p*, and false if we assume *q.* In the possible world approach, this is handled by calling the current world *W*, and then creating a new world *W*1, which is just like *W* except that *p* is true, and *W*2, which is just like *W* except that *q* is true.
By doing reasoning in different worlds we can make predictions about the future, resolve ambiguitites about the current state, and do reasoning by cases.

For example, possible worlds allow us to solve Moore's communism/democracy problem ([page 466](#p466)).
We create two new possible worlds, one where *E* is a democracy and one where it is communist.
In each world it is easy to derive that there is a democracy next to a communist country.
The trick is to realize then that the two worlds form a partition, and that therefore the assertion holds in the original "real" world as well.
This requires an interaction between the Prolog-based tactical reasoning going on within a world and the planning-based strategie reasoning that decides which worlds to consider.

We could also add a *truth maintenance system* (or TMS) to keep track of the assumptions or justifications that lead to each fact being considered true.
A truth maintenance system can lessen the need to backtrack in a search for a global solution.
Although truth maintenance systems are an important part of AI programming, they will not be covered in this book.

In this section we extend the dtree facility ([section 14.8](#s0045)) to handle truth values and possible worlds.
With so many options, it is difficult to make design choices.
We will choose a fairly simple system, one that remains close to the simplicity and speed of Prolog but offers additional functionality when needed.
We will adopt approach (2c) to truth values, using negated predicates.
For example, the negated predicate of `likes` is `~likes`, which is pronounced "not likes."

We will also provide minimal support for possible worlds.
Assume that there is always a current world, *W,* and that there is a way to create alternative worlds and change the current world to an alternative one.
Assertions and queries will always be made with respect to the current world.
Each fact is indexed by the atoms it contains, just as before.
The difference is that the facts are also indexed by the current world.
To support this, we need to modify the notion of the numbered list, or `nlist`, to include a numbered association list, or `nalist`.
The following is an `nalist` showing six facts indexed under three different worlds: `W0, Wl`, and `W2`:

[ ](#){:#l0375}`(6 (W0 #1# #2# #3#) (Wl #4#) (W2 #5# #6#))`
!!!(p) {:.unnumlist}

The fetching routine will remain unchanged, but the postfetch processing will have to sort through the nalists to find only the facts in the current world.
It would also be possible for `fetch` to do this work, but the reasoning is that most facts will be indexed under the "real world," and only a few facts will exist in alternative, hypothetical worlds.
Therefore, we should delay the effort of sorting through the answers to elimina te those answers in the wrong world–it may be that the first answer fetched will suffice, and then it would have been a waste to go through and eliminate other answers.
The following changes to `index` and `dtree-index` add support for worlds:

[ ](#){:#l0380}`(defvar *world* 'W0 "The current world used by index and fetch.")`
!!!(p) {:.unnumlist}

`(defun index (key &optional (world *world*))`
!!!(p) {:.unnumlist}

` "Store key in a dtree node.
Key must be (predicate .
args);`
!!!(p) {:.unnumlist}

` it is stored in the dtree, indexed by the world."`
!!!(p) {:.unnumlist}

` (dtree-index key key world (get-dtree (predicate key))))`
!!!(p) {:.unnumlist}

`(defun dtree-index (key value world dtree)`
!!!(p) {:.unnumlist}

` "Index value under all atoms of key in dtree."`
!!!(p) {:.unnumlist}

` (cond`
!!!(p) {:.unnumlist}

`  ((consp key)  ; index on both first and rest`
!!!(p) {:.unnumlist}

`   (dtree-index (first key) value world`
!!!(p) {:.unnumlist}

`      (or (dtree-first dtree)`
!!!(p) {:.unnumlist}

`       (setf (dtree-first dtree) (make-dtree))))`
!!!(p) {:.unnumlist}

`   (dtree-index (rest key) value world`
!!!(p) {:.unnumlist}

`      (or (dtree-rest dtree)`
!!!(p) {:.unnumlist}

`       (setf (dtree-rest dtree) (make-dtree)))))`
!!!(p) {:.unnumlist}

`  ((null key))  ; don't index on nil`
!!!(p) {:.unnumlist}

`  ((variable-p key)  ; index a variable`
!!!(p) {:.unnumlist}

`   (nalist-push world value (dtree-var dtree)))`
!!!(p) {:.unnumlist}

`  (t ;; Make sure there is an nlist for this atom.
and add to it`
!!!(p) {:.unnumlist}

`   (nalist-push world value (lookup-atom key dtree)))))`
!!!(p) {:.unnumlist}

The new function `nalist-push` adds a value to an nalist, either by inserting the value in an existing key's list or by adding a new key/value list:

[ ](#){:#l0385}`(defun nalist-push (key val nalist)`
!!!(p) {:.unnumlist}

` "Index val under key in a numbered al ist."`
!!!(p) {:.unnumlist}

` ;; An nalist is of the form (count (key val*)*)`
!!!(p) {:.unnumlist}

` ;; Ex: (6 (nums 12 3) (letters a b c))`
!!!(p) {:.unnumlist}

` (incf (car nalist))`
!!!(p) {:.unnumlist}

` (let ((pair (assoc key (cdr nalist))))`
!!!(p) {:.unnumlist}

`  (if pair`
!!!(p) {:.unnumlist}

`   (push val (cdr pair))`
!!!(p) {:.unnumlist}

`   (push (list key val) (cdr nalist)))))`
!!!(p) {:.unnumlist}

In the following, `fetch` is used on the same data base created by `test-index`, indexed under the world `W0`.
This time the resuit is a list-of-lists of world/values a-lists.
The count, 3, is the same as before.

[ ](#){:#l0390}`>(fetch '(p ?x c))`
!!!(p) {:.unnumlist}

`(((W0 (P B C) (P A C)))`
!!!(p) {:.unnumlist}

` ((W0 (P A ?X))))`
!!!(p) {:.unnumlist}

`3`
!!!(p) {:.unnumlist}

So far, worlds have been represented as symbols, with the implication that different symbols represent completely distinct worlds.
That doesn't make worlds very easy to use.
We would like to be able to use worlds to explore alternatives—create a new hypothetical world, make some assumptions (by asserting them as facts in the hypothetical world), and see what can be derived in that world.
It would be tedious to have to copy all the facts from the real world into each hypothetical world.

An alternative is to establish an inheritance hierarchy among worlds.
Then a fact is considered true if it is indexed in the current world or in any world that the current world inherits from.

To support inheritance, we will implement worlds as structures with a name field and a field for the list of parents the world inherits from.
Searching through the inheritance lattice could become costly, so we will do it only once each time the user changes worlds, and mark all the current worlds by setting the `current` field on or off.
Here is the definition for the world structure:

[ ](#){:#l0395}`(defstruct (world (:print-function print-world))`
!!!(p) {:.unnumlist}

` name parents current)`
!!!(p) {:.unnumlist}

We will need a way to get from the name of a world to the world structure.
Assuming names are symbols, we can store the structure on the name's property list.
The function `get-world` gets the structure for a name, or builds a new one and stores it.
`get-world` can also be passed a world instead of a name, in which case it just returns the world.
We also include a definition of the default initial world.

[ ](#){:#l0400}`(defun get-world (name &optional current (parents (list *world*)))`
!!!(p) {:.unnumlist}

` "Look up or create the world with this name.`
!!!(p) {:.unnumlist}

` If the world is new, give it the list of parents."`
!!!(p) {:.unnumlist}

` (cond ((world-p name) name) ; ok if it already is a world`
!!!(p) {:.unnumlist}

`   ((get name 'world))`
!!!(p) {:.unnumlist}

`   (t (setf (get name 'world)`
!!!(p) {:.unnumlist}

`     (make-world :name name :parents parents`
!!!(p) {:.unnumlist}

`      :current current)))))`
!!!(p) {:.unnumlist}

`(defvar *world* (get-world 'W0 nil nil)`
!!!(p) {:.unnumlist}

` "The current world used by index and fetch.")`
!!!(p) {:.unnumlist}

The function `use-world` is used to switch to a new world.
It first makes the current world and all its parents no longer current, and then makes the new chosen world and all its parents current.
The function `use-new-world` is more efficient in the common case where you want to create a new world that inherits from the current world.
It doesn't have to turn any worlds off; it j ust crea tes the new world and makes it current.

[ ](#){:#l0405}`(defun use-world (world)`
!!!(p) {:.unnumlist}

` "Make this world current."`
!!!(p) {:.unnumlist}

` ;; If passed a name, look up the world it names`
!!!(p) {:.unnumlist}

` (setf world (get-world world))`
!!!(p) {:.unnumlist}

` (unless (eq world *world*)`
!!!(p) {:.unnumlist}

`  ;; Turn the old world(s) off and the new one(s) on,`
!!!(p) {:.unnumlist}

`  ;; unless we are already using the new world`
!!!(p) {:.unnumlist}

`  (set-world-current *world* nil)`
!!!(p) {:.unnumlist}

`  (set-world-current world t)`
!!!(p) {:.unnumlist}

`  (setf *world* world)))`
!!!(p) {:.unnumlist}

`(defun use-new-world ()`
!!!(p) {:.unnumlist}

` "Make up a new world and use it.`
!!!(p) {:.unnumlist}

` The world inherits from the current world."`
!!!(p) {:.unnumlist}

` (setf *wor1d* (get-world (gensym "W")))`
!!!(p) {:.unnumlist}

` (setf (world-current *world*) t)`
!!!(p) {:.unnumlist}

` *world*)`
!!!(p) {:.unnumlist}

`(defun set-world-current (world on/off)`
!!!(p) {:.unnumlist}

` "Set the current field of world and its parents on or off."`
!!!(p) {:.unnumlist}

` ;; nil is off, anything else is on.`
!!!(p) {:.unnumlist}

` (setf (world-current world) on/off)`
!!!(p) {:.unnumlist}

` (dolist (parent (world-parents world))`
!!!(p) {:.unnumlist}

`  (set-world-current parent on/off)))`
!!!(p) {:.unnumlist}

We also add a print function for worlds, which just prints the world's name.

[ ](#){:#l0410}`(defun print-world (world &optional (stream t) depth)`
!!!(p) {:.unnumlist}

` (declare (ignore depth))`
!!!(p) {:.unnumlist}

` (prin1 (world-name world) stream))`
!!!(p) {:.unnumlist}

The format of the dtree data base has changed to include worlds, so we need new retrieval functions to search through this new format.
Here the functions `mapc-retrieve, retrieve`, and `retrieve-bagof` are modified to give new versions that treat worlds.
To reflect this change, the new functions all have names ending in -`in-world`:

[ ](#){:#l0415}`(defun mapc-retrieve-in-world (fn query)`
!!!(p) {:.unnumlist}

` "For every fact in the current world that matches the query,`
!!!(p) {:.unnumlist}

` apply the function to the binding list."`
!!!(p) {:.unnumlist}

` (dolist (bucket (fetch query))`
!!!(p) {:.unnumlist}

`  (dolist (world/entries bucket)`
!!!(p) {:.unnumlist}

`   (when (world-current (first world/entries))`
!!!(p) {:.unnumlist}

`    (dolist (answer (rest world/entries))`
!!!(p) {:.unnumlist}

`     (let ((bindings (unify query answer)))`
!!!(p) {:.unnumlist}

`      (unless (eq bindings fall)`
!!!(p) {:.unnumlist}

`       (funcall fn bindings))))))))`
!!!(p) {:.unnumlist}

`(defun retrieve-in-world (query)`
!!!(p) {:.unnumlist}

` "Find all facts that match query.
Return a list of bindings."`
!!!(p) {:.unnumlist}

` (let ((answers nil))`
!!!(p) {:.unnumlist}

`  (mapc-retrieve-in-world`
!!!(p) {:.unnumlist}

`   #'(lambda (bindings) (push bindings answers))`
!!!(p) {:.unnumlist}

`   query)`
!!!(p) {:.unnumlist}

`  answers))`
!!!(p) {:.unnumlist}

`(defun retrieve-bagof-in-world (query)`
!!!(p) {:.unnumlist}

` "Find all facts in the current world that match query.`
!!!(p) {:.unnumlist}

` Return a list of queries with bindings filled in."`
!!!(p) {:.unnumlist}

` (mapcar #'(lambda (bindings) (subst-bindings bindings query))`
!!!(p) {:.unnumlist}

`     (retrieve-in-world query)))`
!!!(p) {:.unnumlist}

Now let's see how these worlds work.
First, in `W0` we see that the facts from `test-index` are still in the data base:

[ ](#){:#l0420}`> *world* ⇒ W0`
!!!(p) {:.unnumlist}

`> (retrieve-bagof-in-world '(p ?z c))`⇒
!!!(p) {:.unnumlist}

`((P A C) (P A C) (P B C))`
!!!(p) {:.unnumlist}

Now we create and use a new world that inherits from `W0`.
Two new facts are added to this new world:

[ ](#){:#l0425}`> (use-new-world)`⇒ `W7031`
!!!(p) {:.unnumlist}

`> (index '(p new c))`⇒ `T`
!!!(p) {:.unnumlist}

`> (index '(~p b b))`⇒ `T`
!!!(p) {:.unnumlist}

We see that the two new facts are accessible in this world:

[ ](#){:#l0430}`> (retrieve-bagof-in-world '(p ?z c))`⇒
!!!(p) {:.unnumlist}

`((P A C) (P A C) (P B C) (P NEW C))`
!!!(p) {:.unnumlist}

`> (retrieve-bagof-in-world '(~p ?x ?y))`⇒
!!!(p) {:.unnumlist}

`((~P B B))`
!!!(p) {:.unnumlist}

Now we create another world as an alternative to the current one by first switching back to the original `W0`, then creating the new world, and then adding some facts:

[ ](#){:#l0435}`> (use-world 'W0)`⇒ `W0`
!!!(p) {:.unnumlist}

`> (use-new-world)`⇒ `W7173`
!!!(p) {:.unnumlist}

`> (index '(p newest c))`⇒ `T`
!!!(p) {:.unnumlist}

`> (index '(~p c newest))`⇒ `T`
!!!(p) {:.unnumlist}

Here we see that the facts entered in `W7031` are not accessible, but the facts in the new world and in `W0` are:

[ ](#){:#l0440}`> (retrieve-bagof-in-world '(p ?z c))`⇒
!!!(p) {:.unnumlist}

`((P A C) (P A C) (P B C) (P NEWEST C))`
!!!(p) {:.unnumlist}

`> (retrieve-bagof-in-world '(~p ?x ?y))`⇒
!!!(p) {:.unnumlist}

`((~P C NEWEST))`
!!!(p) {:.unnumlist}

### [ ](#){:#st9155}Unification, Equality, Types, and Skolem Constants
{:#s9155}
{:.h2hd}

The lesson of the zebra puzzle in [section 11.4](B978008057115750011X.xhtml#s0040) was that unification can be used to lessen the need for backtracking, because an uninstantiated logic variable or partially instantiated term can stand for a whole range of possible solutions.
However, this advantage can quickly disappear when the representation forces the problem solver to enumerate possible solutions rather than treating a whole range of solutions as one.
For example, consider the following query in the frame language and its expansion into primitives:

[ ](#){:#l0445}`(a person (name Fran))`
!!!(p) {:.unnumlist}

`≡ (and (ind ?p person) (val name ?p fran))`
!!!(p) {:.unnumlist}

The way to answer this query is to enumerate all individuals `?p` of type `person` and then check the `name` slot of each such person.
It would be more efficient if `(ind ?p person)` did not act as an enumeration, but rather as a constraint on the possible values of `?p`.
This would be possible if we changed the definition of variables (and of the unification function) so that each variable had a type associated with it.
In fact, there are at least three sources of information that have been implemented as constraints on variables terms:

* [ ](#){:#l0450}• The type or category of the term.

* • The members or size of a term considered as a set or list.

* • Other terms this term is equal or not equal to.

Note that with a good solution to the problem of equality, we can solve the problem of Skolem constants.
The idea is that a regular constant unifies with itself but no other regular constant.
On the other hand, a Skolem constant can potentially unify with any other constant (regular or Skolem).
The equality mechanism is used to keep track of each Skolem variable's possible bindings.

## [ ](#){:#st0085}14.11 History and References
{:#s0085}
{:.h1hd}

[Brachman and Levesque (1985)](B9780080571157500285.xhtml#bb0115) collect thirty of the key papers in knowledge representation.
Included are some early approaches to semantic network based ([Quillian 1967](B9780080571157500285.xhtml#bb0965)) and logic-based ([McCarthy 1968](B9780080571157500285.xhtml#bb0805)) representation.
Two thoughtful critiques of the ad hoc use of representations without defining their meaning are by [Woods (1975)](B9780080571157500285.xhtml#bb1430) and [McDermott (1978)](B9780080571157500285.xhtml#bb0820).
It is interesting to contrast the latter with [McDermott 1987](B9780080571157500285.xhtml#bb0825), which argues that logic by itself is not sufficient to solve the problems of AI.
This argument should not be surprising to those who remember the slogan *logic = algorithm - control.*

[Genesereth and Nilsson's textbook (1987)](B9780080571157500285.xhtml#bb0455) cover the predicate-calculus-based approach to knowledge representation and AI in general.
[Ernest Davis (1990)](B9780080571157500285.xhtml#bb0275) presents a good overview of the field that includes specialized representations for time, space, qualitative physics, propositional attitudes, and the interaction between agents.

Many representation languages focus on the problem of defining descriptions for categories of objects.
These have come to be known as *term-subsumption languages.* Examples include KL-ONE ([Schmolze and Lipkis 1983](B9780080571157500285.xhtml#bb1060)) and KRYPTON ([Brachman, Fikes, and Levesque 1983](B9780080571157500285.xhtml#bb0120)).
See [Lakoff 1987](B9780080571157500285.xhtml#bb0685) for much more on the problem of categories and prototypes.

Hector [Levesque (1986)](B9780080571157500285.xhtml#bb0720) points out that the areas Prolog has difficulty with–disjunction, negation, and existentials–all involve a degree of vagueness.
In his term, they lack *vividness.* A vivid proposition is one that could be represented directly in a picture: the car is blue; she has a martini in her left hand; Albany is the capital of New York.
Nonvivid propositions cannot be so represented: the car is not blue; she has a martini in one hand; either Albany or New York City is the capital of New York.
There is interest in separating vivid from nonvivid reasoning, but no current systems are actually built this way.

The possible world approach of [section 14.10](#s0055) was used in the MRS system ([Russell 1985](B9780080571157500285.xhtml#bb1020)).
More recent knowledge representation systems tend to use truth maintenance systems instead of possible worlds.
This approach was pioneered by [Doyle (1979)](B9780080571157500285.xhtml#bb0340) and [McAllester (1982)](B9780080571157500285.xhtml#bb0785).
Doyle tried to change the name to "reason maintenance,' in (1983), but it was too late.
The version in widest used today is the assumption-based truth maintenance system, or ATMS, developed by de Kleer (1986a,b,c).
[Charniak et al.
(1987)](B9780080571157500285.xhtml#bb0180) present a complete Common Lisp implementation of a McAllester-styleTMS.

There is little communication between the logic programming and knowledge representation communities, even though they cover overlapping territory.
[Colmerauer (1990)](B9780080571157500285.xhtml#bb0250) and [Cohen (1990)](B9780080571157500285.xhtml#bb0230) describe Logic Programming languages that address some of the issues covered in this chapter.
Key papers in equality reasoning include Galler and Fisher 1974, [Kornfeld 1983](B9780080571157500285.xhtml#bb0645),[1](#fn0015){:#xfn0015} Jaffar, Lassez, and Maher 1984, and [van Emden and Yukawa 1987](B9780080571157500285.xhtml#bb1265).
[Hölldobler's book (1987)](B9780080571157500285.xhtml#bb0550) includes an overview of the area.
Papers on extending unification in ways other than equality include [Aït-Kaci et al.
1987](B9780080571157500285.xhtml#bb0025) and [Staples and Robinson 1988](B9780080571157500285.xhtml#bb1125).
Finally, papers on extending Prolog to cover disjunction and negation (i.e., non-Horn clauses) include [Loveland 1987](B9780080571157500285.xhtml#bb0755), [Plaisted 1988](B9780080571157500285.xhtml#bb0960), and [Stickel l988](B9780080571157500285.xhtml#bb1200).

## [ ](#){:#st0090}14.12 Exercises[ ](#){:#p9450}
{:#s0090}
{:.h1hd}

[ ](#){:#l0250}**Exercise 14.1 [m]** Arrange to store dtrees in a hash table rather than on the property list of predicates.
!!!(p) {:.unnumlist}

**Exercise 14.2 [m]** Arrange to store the `dtree-atoms` in a hash table rather than in an association list.
!!!(p) {:.unnumlist}

**Exercise 14.3 [m]** Change the `dtree` code so that `nil` is used as an atom index.
Time the performance on an application and see if the change helps or hurts.
!!!(p) {:.unnumlist}

**Exercise 14.4 [m]** Consider the query `(p a b c d e f g)`.
If the index under a returns only one or two keys, then it is probably a waste of time for `dtree-fetch` to consider the other keys in the hope of finding a smaller bucket.
It is certainly a waste if there are no keys at all indexed under `a`.
Make appropriate changes to `dtree-fetch`.
!!!(p) {:.unnumlist}

**Exercise 14.5 [h]** Arrange to delete elements from a `dtree`.
!!!(p) {:.unnumlist}

**Exercise 14.6 [h]** Implement iterative-deepening search in the Prolog compiler.
You will have to change each function to accept the depth as an extra argument, and compile in checks for reaching the maximum depth.
!!!(p) {:.unnumlist}

**Exercise 14.7 [d]** Integrate the Prolog compiler with the dtree data base.
Use the dtrees for predicates with a large number of clauses, and make sure that each predicate that is implemented as a dtree has a Prolog primitive accessing the dtree.
!!!(p) {:.unnumlist}

**Exercise 14.8 [d]** Add support for possible worlds to the Prolog compiler with dtrees.
This support has already been provided for dtrees, but you will have to provide it for ordinary Prolog rules.
!!!(p) {:.unnumlist}

**Exercise 14.9 [h]** Integrate the language described in [section 14.10](#s0055) and the frame syntax from [section 14.10](#s0055) with the extended Prolog compiler from the previous exercise.
!!!(p) {:.unnumlist}

**Exercise 14.10 [d]** Build a strategie reasoner that decides when to create a possible world and does reasoning by cases over these worlds.
Use it to solve Moore 's problem ([page 466](#p466)).
!!!(p) {:.unnumlist}

## [ ](#){:#st0095}14.13 Answers
{:#s0095}
{:.h1hd}

**Answer 14.1**

[ ](#){:#l0455}`(let ((dtrees (make-hash-table :test #'eq)))`
!!!(p) {:.unnumlist}

` (defun get-dtree (predicate)`
!!!(p) {:.unnumlist}

`  "Fetch (or make) the dtree for this predicate."`
!!!(p) {:.unnumlist}

`  (setf (gethash predicate dtrees)`
!!!(p) {:.unnumlist}

`    (or (gethash predicate dtrees)`
!!!(p) {:.unnumlist}

`     (make-dtree))))`
!!!(p) {:.unnumlist}

` (defun clear-dtrees ()`
!!!(p) {:.unnumlist}

` "Remove all the dtrees for all the predicates."`
!!!(p) {:.unnumlist}

` (clrhash dtrees)))`
!!!(p) {:.unnumlist}

**Answer 14.5** Hint: here is the code for `nlist-delete`.
Now figure out how to find all the nlists that an item is indexed under.

[ ](#){:#l0460}`(defun nlist-delete (item nlist)`
!!!(p) {:.unnumlist}

` "Remove an element from an nlist.`
!!!(p) {:.unnumlist}

` Assumes that item is present exactly once."`
!!!(p) {:.unnumlist}

` (decf (car nlist))`
!!!(p) {:.unnumlist}

` (setf (cdr nlist) (delete item (cdr nlist) :count 1))`
!!!(p) {:.unnumlist}

` nlist)`
!!!(p) {:.unnumlist}

----------------------

[1](#xfn0015){:#np0015} A commentary on this paper appears in [Elcock and Hoddinott 1986](B9780080571157500285.xhtml#bb0360).
!!!(p) {:.ftnote1}

Part IV
Advanced AI Programs
!!!(p) {:.parttitle}

