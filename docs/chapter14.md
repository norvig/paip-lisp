# Chapter 14 {docsify-ignore}
<a id='page-460'></a>

Knowledge Representation 
and Reasoning 

Knowledge itself is power. 

-Francis Bacon (. 561-1626) 

The power resides in the knowledge. 

—Edward Feigenbaum 
Stanford University Heuristic Programming Project 

Knowledge is Knowledge, and vice versa. 

—Tee shirt 
Stanford University Heuristic Programming Project 

I
I
n the 1960s, much of AI concentrated on search techniques. In particular, a lot of w^ork v^as 
concerned with theorem proving: stating a problem as a small set of axioms and searching for 
a proof of the problem. The implicit assumption was that the power resided in the inference 
mechanism-if we could just find the right search technique, then all our problems would be 
solved, and all our theorems would be proved. 

<a id='page-461'></a>

Starting in the 1970s, this began to change. The theorem-proving approach failed 
to live up to its promise. AI workers slowly began to realize that they were not going 
to solve NP-hard problems by conung up with a clever inference algorithm. The 
general inferencing mechanisms that worked on toy examples just did not scale up 
when the problem size went into the thousands (or sometimes even into the dozens). 

The expert-system approach offered an alternative. The key to solving hard problems 
was seen to be the acquisition of special-case rules to break the problem into 
easier problems. According to Feigenbaum, the lesson learned from expert systems 
like MYCIN (which we will see in chapter 16) is that the choice of inferencing mechanism 
is not as important as having the right knowledge. In this view it doesn't 
matter very much if MYCIN uses forward- or backward-chaining, or if it uses certainty 
factors, probabilities, or fuzzy set theory. What matters crucially is that we know 
Pseudomonas is a gram-negative, rod-shaped organism that can infect patients with 
compromised immune systems. In other words, the key problem is acquiring and 
representing knowledge. 

While the expert system approach had some successes, it also had failiu-es, and 
researchers were interested in learning the limits of this new technology and understanding 
exactly how it works. Many found it troublesome that the meaning of the 
knowledge used in some systems was never clearly defined. For example, does the 
assertion (color appl e red) mean that a particular apple is red, that all apples are 
red, or that some/most apples are red? The field of knowledge representation concentrated 
on providing clear semantics for such representations, as well as providing 
algorithms for manipulating the knowledge. Much of the emphasis was on finding a 
good trade-off between expressiveness and efficiency. An efficient language is one for 
which all queries (or at least the average query) can be answered quickly. If we want 
to guarantee that queries will be answered quickly, then we have to limit what can 
be expressed in the language. 

In the late 1980s, a series of results shed doubt on the hopes of finding an efficient 
language with any reasonable degree of expressiveness at all. Using mathematical 
techniques based on worst-case analysis, it was shown that even seemingly trivial 
languages were intractable—in the worst case, it would take an exponential amount of 
time to answer a simple query. 

Thus, in the 1990s the emphasis has shifted to knowledge representation and reasoning, 
a field that encompasses both the expressiveness and efficiency of languages but 
recognizes that the average case is more important than the worst case. No amount 
of knowledge can help solve an intractable problem in the worse case, but in practice 
the worst case rarely occurs. 

<a id='page-462'></a>

14.1 A Taxonomy of Representation Languages 
AI researchers have investigated hundreds of knowledge representation languages, 
trying to find languages that are convenient, expressive, and efficient. The languages 
can be classified into four groups, depending on what the basic unit of representation 
is. Here are the four categories, with some examples: 

* Logical Formulae (Prolog) 
* Networks (semantic nets, conceptual graphs) 
* Objects (scripts, frames) 
* Procedures (Lisp, production systems) 
We have already dealt with logic-based languages like Prolog. 

Network-based languages can be seen as a syntactic variation on logical languages. 
A link L between nodes A and . is just another way of expressing the logical relation 
B), The difference is that network-based languages take their links more 
seriously: they are intended to be implemented directly by pointers in the computer, 
and inference is done by traversing these pointers. So placing a link L between A 
and . not only asserts that L(A, B) is true, but it also says something about how the 
knowledge base is to be searched. 

Object-oriented languages can also be seen as syntactic variants of predicate calculus. 
Here is a statement in a typical slot-filler frame language: 

(a person 

(name = Jan) 

(age = 32)) 

This is equivalent to the logical formula: 

3 p: person(p) . name(p,Jan) . age(p,32) 

The frame notation has the advantage of being easier to read, in some people's 
opinion. However, the frame notation is less expressive. There is no way to say that 
the person's name is either Jan or John, or that the person's age is not 34. In predicate 
calculus, of course, such statements can be easily made. 

Finally, procedural languages are to be contrasted with representation languages: 
procedural languages compute answers without explicit representation of knowledge. 


There are also hybrid representation languages that use different methods to 
encode different kinds of knowledge. The KL-ONE family of languages uses both 
logical formulae and objects arranged into a network, for example. Many frame 

<a id='page-463'></a>
languages allow procedural attachment, a technique that uses arbitrary procedures to 
compute values for expressions that are inconvenient or impossible to express in the 
frame language itself. 

14.2 Predicate Calculus and its Problems 
So far, many of our representations have been based on predicate calculus, a notation 
with a distinguished position in AI: it serves as the universal standard by which other 
representations are defined and evaluated. The previous section gave an example 
expression from a frame language. The frame language may have many merits in 
terms of the ease of use of its syntax or the efficiency of its internal representation of 
data. However, to understand what expressions in the language mean, there must be 
a clear definition. More often than not, that definition is given in terms of predicate 
calculus. 

A predicate calculus representation assumes a universe of individuals, with relations 
and functions on those individuals, and sentences formed by combining 
relations with the logical connectives and, or, and not. Philosophers and psychologists 
will argue the question of how appropriate predicate calculus is as a model of 
human thought, but one point stands clear: predicate calculus is sufficient to represent 
anything that can be represented in a digital computer. This is easy to show: 
assuming the computer's memory has . bits, and the equation hi = 1 means that bit 
i is on, then the entire state of the computer is represented by a conjunction such as: 

(6o = 0) . (6i = 0) . (62 = 1) . ... . {bn = 0) 

Once we can represent a state of the computer, it becomes possible to represent 
any computer program in predicate calculus as a set of axioms that map one state onto 
another. Thus, predicate calculus is shown to be asufficientlangaage for representing 
anything that goes on inside a computer—it can be used as a tool for analyzing any 
program from the outside. 

This does not prove that predicate calculus is an appropriate tool for all applications. 
There are good reasons why we may want to represent knowledge in a form 
that is quite different from predicate calculus, and manipulate the knowledge with 
procedures that are quite different from logical inference. But we should still be able 
to describe our system in terms of predicate calculus axioms, and prove theorems 
about it. To do any less is to be sloppy. For example, we may want to manipulate 
numbers inside the computer by using the arithmetic instructions that are built into 
the CPU rather than by manipulating predicate calculus axioms, but when we write 
a square-root routine, it had better satisfy the axiom: 

y/x = y=^yxy = x 

<a id='page-464'></a>

Predicate calculus also serves another purpose: as a tool that can be used by a 
program rather than on a program. All programs need to manipulate data, and some 
programs will manipulate data that is considered to be in predicate calculus notation. 
It is this use that we will be concerned with. 

Predicate calculus makes it easy to start writing down facts about a domain. But 
the most straightforward version of predicate calculus suffers from a number of 
serious limitations: 

* Decidability—^ven a set of axioms and a goal, it may be that neither the goal nor 
its negation can be derived from the axioms. 
* Tractability—even when a goal is provable, it may take too long to find the proof 
using the available inferencing mechanisms. 
* Uncertainty—it can be inconvenient to deal with relations that are probable to a 
degree but not known to be definitely true or false. 
* Monotonicity—in pure predicate calculus, once a theorem is proved, it is true 
forever. But we would like a way to derive tentative theorems that rely on 
assumptions, and be able to retract them when the assumptions prove false. 
* Consistency—pure predicate calculus admits no contradictions. If by accident 
both P and &not;P are derived, then any theorem can be proved. In effect, a single 
contradiction corrupts the entire data base. 
* Omniscience—it can be difficult to distinguish what is provable from what should 
be proved. This can lead to the unfounded assumption that an agent believes 
all the consequences of the facts it knows. 
* Expressiveness—the first-order predicate calculus makes it awkward to talk 
about certain things, such as the relations and propositions of the language 
itself. 
The view held predominantly today is that it is best to approach these problems 
with a dual attack that is both within and outside of predicate calculus. It is considered 
a good idea to invent new notations to address the problems—both for convenience 
and to facilitate special-purpose reasoners that are more efficient than a general-
purpose theorem prover. However, it is also important to define scrupulously the 
meaning of the new notation in terms of familiar predicate-calculus notation. As 
Drew McDermott put it, "No notation without denotation!" (1978). 

In this chapter we show how new notations (and their corresponding meanings) 
can be used to extend an existing representation and reasoning system. Prolog is 
chosen as the language to extend. This is not meant as an endorsement for Prolog as 
the ultimate knowledge representation language. Rather, it is meant solely to give us 
a clear and familiar foundation from which to build. 

<a id='page-465'></a>
14.3 A Logical Language: Prolog 
Prolog has been proposed as the answer to the problem of programming in logic. Why 
isn't it accepted as the universal representation language? Probably because Prolog 
is a compromise between a representation language and a programming language. 
Given two specifications that are logically equivalent, one can be an efficient Prolog 
program, while the other is not. Kowalski's famous equation "algonthm = logic + 
control" expresses the limits of logic alone: logic = algorithm -control Many problems 
(especially in AI) have large or infinite search spaces, and if Prolog is not given some 
advice on how to search that space, it will not come up with the answer in any 
reasonable length of time. 

Prolog's problems fall into three classes. First, in order to make the language 
efficient, its expressiveness was restricted. It is not possible to assert that a person's 
name is either Jan or John in Prolog (although it is possible to ask if the person's 
name is one of those). Similarly, it is not possible to assert that a fact is false; 
Prolog does not distinguish between false and unknown. Second, Prolog's inference 
mechanism is neither sound nor complete. Because it does not check for circular 
unification, it can give incorrect answers, and because it searches depth-first it can 
miss correct answers. Third, Prolog has no good way of adding control information 
to the underlying logic, making it inefficient on certain problems. 

14.4 Problems with Prolog's Expressiveness 
If Prolog is programming in logic, it is not the full predicate logic we are familiar with. 
The main problem is that Prolog can't express certain kinds of indefinite facts. It can 
represent definite facts: the capital of Rhode Island is Providence. It can represent 
conjunctions of facts: the capital of Rhode Island is Providence and the capital of 
California is Sacramento. But it can not represent disjunctions or negations: that the 
capital of California is not Los Angeles, or that the capital of New York is either New 
York City or Albany. We could try this: 

(<- (not (capital LA CA))) 
(<- (or (capital Albany NY) (capital NYC NY))) 

but note that these last two facts concern the relation not and or, not the relation 
capital. Thus, they will not be considered when we ask a query about capital. Fortunately, 
the assertion "Either NYC or Albany is the capital of NY" can be rephrased 
as two assertions: "Albany is the capital of NY if NYC is not" and "NYC is the capital 
of NY if Albany is not:" 

<a id='page-466'></a>

(<- (capital Albany NY) (not (capital NYC NY))) 

(<- (capital NYC NY) (not (capital Albany NY))) 

Unfortunately, Prolog's not is different from logic's not. When Prolog answers "no" 
to a query, it means the query cannot be proven from the known facts. If everything 
is known, then the query must be false, but if there are facts that are not known, the 
query may in fact be true. This is hardly surprising; we can't expect a program to 
come up with answers using knowledge it doesn't have. But in this case, it causes 
problems. Given the previous two clauses and the query (capi tal ?c NY), Prolog 
will go into an infinite loop. If we remove the first clause, Prolog would fail to prove 
that Albany is the capital, and hence conclude that NYC is. If we remove the second 
clause, the opposite conclusion would be drawn. 

The problem is that Prolog equates "not proven" with "false." Prolog makes what 
is called the closed world assumption—it assumes that it knows everything that is true. 
The closed world assumption is reasonable for most programs, because the programmer 
does know all the relevant information. But for knowledge representation in 
general, we would like a system that does not make the closed world assumption 
and has three ways to answer a query: "yes," "no," or "unknown." In this example, 
we would not be able to conclude that the capital of NY is or is not NYC, hence we 
would not be able to conclude anything about Albany. 

As another example, consider the clauses: 

(<- (damned) (do)) 

(<- (damned) (not (do))) 

With these rules, the query (? (damned)) should logically be answered "yes." 
Furthermore, it should be possible to conclude (damned) without even investigating 
if (do) is provable or not. What Prolog does is first try to prove (do). If this succeeds, 
then (damned) is proved. Either way, Prolog then tries again to prove (do), and this 
time if the proof fails, then (damned) is proved. So Prolog is doing the same proof 
twice, when it is unnecessary to do the proof at all. Introducing negation wrecks 
havoc on the simple Prolog evaluation scheme. It is no longer sufficient to consider 
a single clause at a time. Rather, multiple clauses must be considered together if we 
want to derive all the right answers. 

Robert Moore 1982 gives a good example of the power of disjunctive reasoning. 
His problem concerned three colored blocks, but we will update it to deal with three 
countries. Suppose that a certain Eastern European country, E, has just decided if it 
will remain under communist rule or become a democracy, but we do not know the 
outcome of the decision. . is situated between the democracy D and the communist 
country C: 

D E C 

<a id='page-467'></a>

The question is: Is there a communist country next to a democracy? Moore points 
out that the answer is "yes," but discovering this requires reasoning by cases. If . is 
a democracy then it is next to C and the answer is yes. But if . is communist then 
it is next toD and the answer is still yes. Since those are the only two possibilities, 
the answer must be yes in any case. Logical reasoning gives us the right answer, but 
Prolog can not. We can describe the problem with the following seven assertions 
and one query, but Prolog can not deal with the or in the final assertion. 

(<- (next-to D E)) (<- (next-to . D)) 
(<- (next-to . .) (<- (next-to C E)) 
(<- (democracy D)) (<- (communist O) 
(<- (or (democracy E) (communist E))) 

(?- (next-to ?A ?B) (democracy ?A) (communist ?B)) 

We have seen that Prolog is not very good at representing disjunctions and negations. 
It also has difficulty representing existentials. Consider the following statement in 
English, logic, and Prolog: 

Jan likes everyone. 

VX person(x) => likesQan,x) 

(<- (likes Jan ?x) (person ?x)) 

The Prolog translation is faithful. But there is no good translation for "Jan likes 
someone." The closest we can get is: 

Jan likes someone. 

3 X person(x) => likesQan,x) 

(<- (likes Jan pD) 
(<- (person pD) 

Here we have invented a new symbol, pi, to represent the unknown person that Jan 
likes, and have asserted that pi is a person. Notice that pi is a constant, not a variable. 
This use of a constant to represent a specific but unknown entity is called a Skolem 
constant, after the logician Thoralf Skolem (1887-1963). The intent is that pi may be 
equal to some other person that we know about. If we find out that Adrian is the 
person Jan likes, then in logic we can just add the assertion pi = Adrian. But that does 
not work in Prolog, because Prolog implicitly uses the unique name assumption—d\\ 
atoms represent distinct individuals. 

A Skolem constant is really just a special case of a Skolem function - an unknown 

entity that depends on one or more variable. For example, to represent "Everyone 

likes someone" we could use: 

<a id='page-468'></a>

Everyone likes someone. 

V 2/ 3 X person(3:) => likes (y, x) 

(<- (likes ?y (p2 ?y))) 
(<- (person (p2 ?y))) 

Here .2 is a Skolem function that depends on the variable ?y. In other words, 
everyone likes some person, but not necessarily the same person. 

14.5 Problems with Predicate Calculus's 
Expressiveness 
In the previous section we saw that Prolog has traded some expressiveness for 
efficiency. This section explores the limits of predicate calculus's expressiveness. 
Suppose we want to assert that lions, tigers, and bears are kinds of animals. In 
predicate calculus or in Prolog we could write an impHcation for each case: 

(<- (animal ?x) (lion ?x)) 
(<- (animal ?x) (tiger ?x)) 
(<- (animal ?x) (bear ?x)) 

These implications allow us to prove that any known lion, tiger, or bear is in fact 
an animal. However, they do not allow us to answer the question "What kinds of 
animals are there?" It is not hard to imagine extending Prolog so that the query 

(?- (<- (animal ?x) ?proposition)) 

would be legal. However, this happens not to be valid Prolog, and it is not even 
valid first-order predicate calculus (or FOPC). In FOPC the variables must range over 
constants in the language, not over relations or propositions. Higher-order predicate 
calculus removes this limitation, but it has a more complicated proof theory. 

It is not even clear what the values of ?propos i ti on should be in the query above. 
Surely (1 ion ?x) would be a valid answer, but so would (animal ?x), (or (tiger 
?x) (bea r ?x)), and an infinite number of other propositions. Perhaps we should 
have two types of queries, one that asks about "kinds," and another that asks about 
propositions. 

There are other questions that we might want to ask about relations. Just as it is 
useful to declare the types of parameters to a Lisp function, it can be useful to declare 
the types of the parameters of a relation, and later query those types. For example, 
we might say that the 1 i kes relation holds between a person and an object. 

In general, a sentence in the predicate calculus that uses a relation or sentence as 
a term is called a higher-order sentence. There are some quite subtle problems that 

<a id='page-469'></a>
come into play when we start to allow higher-order expressions. Allowing sentences 
in the calculus to talk about the truth of other sentences can lead to a paradox: is the 
sentence "This sentence is false" true or false? 

Predicate calculus is defined in terms of a universe of individuals and their 
properties and relations. Thus it is well suited for a model of the world that picks out 
individuals and categorizes them - a person here, a building there, a sidewalk between 
them. But how well does predicate calculus fare in a world of continuous substances? 
Consider a body of water consisting of an indefinite number of subconstituents that 
are all water, with some of the water evaporating into the air and rising to form clouds. 
It is not at all obvious how to define the individuals here. However, Patrick Hayes 
has shown that when the proper choices are made, predicate calculus can describe 
this kind of situation quite well. The details are in Hayes 1985. 

The need to define categories is a more difficult problem. Predicate calculus 
works very well for crisp, mathematical categories: . is a triangle if and only if . is 
a polygon with three sides. Unfortunately, most categories that humans deal with 
in everyday life are not defined so rigorously. The category friend refers to someone 
you have mostly positive feelings for, whom you can usually trust, and so on. This 
"definition" is not a set of necessary and sufficient conditions but rather is an open-
ended list of ill-defined qualities that are highly correlated with the category friend. 
We have a prototype for what an ideal friend should be, but no clear-cut boundaries 
that separate friend from, say, acquaintance. Furthermore, the boundaries seem to 
vary from one situation to another: a person you describe as a good friend in your 
work place might be only an acquaintance in the context of your home life. 

There are versions of predicate calculus that admit quantifiers like "most" in 
addition to "for all" and "there exists," and there have been attempts to define 
prototypes and measure distances from them. However, there is no consensus on 
the way to approach this problem. 

14.6 Problems with Completeness 
Because Prolog searches depth-first, it can get caught in one branch of the search 
space and never examine the other branches. This problem can show up, for example, 
in trying to define a commutative relation, like si bl i ng: 

(<- (sibling lee kim)) 
(<- (sibling ?x ?y) (sibling ?y ?x)) 

With these clauses, we expect to be able to conclude that Lee is Kim's sibling, and 
Kim is Lee's. Let's see what happens: 

<a id='page-470'></a>

> (?- (sibling ?x ?y)) 
?X = LEE 
?Y = KIM; 
?X = KIM 
?Y = LEE; 
?X = LEE 
?Y = KIM; 
?X = KIM 
?Y = LEE. 
No. 

We get the expected conclusions, but they are deduced repeatedly, because the 
commutative clause for siblings is applied over and over again. This is annoying, but 
not critical. Far worse is when we ask (? - (sibling fred ?x)). This query loops 
forever. Happily, this particular type of example has an easy fix: just introduce two 
predicates, one for data-base level facts, and one at the level of axioms and queries: 

(<- (sibling-fact lee kim)) 
(<- (sibling ?x ?y) (sibling-fact ?x ?y)) 
(<- (sibling ?x ?y) (sibling-fact ?y ?x)) 

Another fix would be to change the interpreter to fail when a repeated goal was detected. 
This was the approach taken in GPS. However, even if we eliminated repeated 
goals, Prolog can still get stuck in one branch of a depth-first search. Consider the 
example: 

(<- (natural 0)) 
(<- (natural (1-.- ?n)) (natural ?n)) 

These rules define the natural numbers (the non-negative integers). We can use 
the rules either to confirm queries like (natural (1+ (1->- (1-.- 0)))) or to generate 
the natural numbers, as in the query (natural ?n). So far, everything is fine. But 
suppose we wanted to define all the integers. One approach would be this: 

(<- (integer 0)) 
(<- (integer ?n) (integer (1+ ?n))) 
(<- (integer a+ ?n)) (integer ?n)) 

These rules say that 0 is an integer, and any . is an integer if . -f 1 is, and . -h 1 is 
if . is. While these rules are correct in a logical sense, they don't work as a Prolog 
program. Asking (integer x) will result in an endless series of ever-increasing 
queries: (integer (1+ x)), (integer (1+ (1+ and so on. Each goal is 
different, so no check can stop the recursion. 

<a id='page-471'></a>

The occurs check may or may not introduce problems into Prolog, depending on 
your interpretation of infinite trees. Most Prolog systems do not do the occurs check. 
The reasoning is that unifying a variable with some value is the Prolog equivalent of 
assigning a value to a variable, and programmers expect such a basic operation to be 
fast. With the occurs check turned off, it will in fact be fast. With checking on, it 
takes time proportional to the size of the value, which is deemed unacceptable. 

With occurs checking off, the programmer gets the benefit of fast unification but 
can run into problems with circular structures. Consider the following clauses: 

(<- (parent ?x (mother-of ?x))) 

(<- (parent ?x (father-of ?x))) 

These clauses say that, for any person, the mother of that person and the father of 
that person are parents of that person. Now let us ask if there is a person who is his 
or her own parent: 

> (? (parent ?y ?y)) 
?Y = [Abort] 

The system has found an answer, where ?y = (mother-of ?y). The answer can't be 
printed, though, because deref (or subst-bindings in the interpreter) goes into an 
infinite loop trying to figure out what ?y is. Without the printing, there would be no 
infinite loop: 

(<- (self-parent) (parent ?y ?y)) 

> (? (self-parent)) 

Yes; 

Yes; 

No. 

The sel f-parent query succeeds twice, once with the mother clause and once with 
the father clause. Has Prolog done the right thing here? It depends on your interpretation 
of infinite circular trees. If you accept them as valid objects, then the answer 
is consistent. If you don't, then leaving out the occurs check makes Prolog unsound: 
it can come up with incorrect answers. 

The same problem comes up if we ask if there are any sets that include themselves 

as members. The query (member ?set ?set) will succeed, but we will not be able to 

print the value of ?set. 

<a id='page-472'></a>

14.7 Problems with Efficiency: Indexing 
Our Prolog compiler is designed to handle "programlike" predicates - predicates 
with a small number of rules, perhaps with complex bodies. The compiler does 
much worse on "tablelike" predicates-predicates with a large number of simple 
facts. Consider the predicate pb, which encodes phone-book facts in the form: 

(pb (name Jan Doe) (num 415 555 1212)) 

Suppose we have a few thousand entries of this kind. A typical query for this data 
base would be: 

(pb (name Jan Doe) ?num) 

It would be inefficient to search through the facts linearly, matching each one against 
the query. It would also be inefficient to recompile the whole pb/2 predicate every 
time a new entry is added. But that is just what our compiler does. 

The solutions to the three problems - expressiveness, completeness, and index-
ing-will be considered in reverse order, so that the most difficult one, expressiveness, 
will come last. 

14.8 A Solution to the Indexing Problem 
A better solution to the phone-book problem is to index each phone-book entry in 
some kind of table that makes it easy to add, delete, and retrieve entries. That is what 
we will do in this section. We will develop an extension of the trie or discrimination 
tree data structure built in section 10.5 ([page 344](chapter10.md#page-344)). 

Making a discrimination tree for Prolog facts is complicated by the presence of 
variables in both the facts and the query. Either facts with variables in them will have 
to be indexed in several places, or queries with variables will have to look in several 
places, or both. We also have to decide if the discrimination tree itself will handle 
variable binding, or if it will just return candidate matches which are then checked by 
some other process. It is not clear what to store in the discrimination tree: copies of 
the fact, functions that can be passed continuations, or something else. More design 
choices will come up as we proceed. 

It is difficult to make design choices when we don't know exactly how the system 
will be used. We don't know what typical facts will look like, nor typical queries. 
Therefore, we will design a fairly abstract tool, forgetting for the moment that it will 
be used to index Prolog facts. 

<a id='page-473'></a>

We will address the problem of a discrimination tree where both the keys and 
queries are predicate structures with wild cards. A wild card is a variable, but with 
the understanding thatjhere is no variable binding; each instance of a variable can 
match anything. A predicate structure is a list whose first element is a nonvariable 
symbol. The discrimination tree supports three operations: 

* index &ndash; add a key/value pair to the tree 
* fetch &ndash; find all values that potentially match a given key 
* unindex &ndash; remove all key/value pairs that match a given key 
To appreciate the problems, we need an example. Suppose we have the following 
six keys to index. For simplicity, the value of each key will be the key itself: 

1 (p a b) 

2 (p a c) 

3 (p a ?x) 

4 (p b c) 

5 (p b (f c)) 

6 (p a (f . ?x)) 

Now assume the query (. ?y c). This should match keys 2, 3, and 4. How could 
we efficiently arrive at this set? One idea is to list the key/value pairs under every 
atom that they contain. Thus, all six would be listed under the atom p, while 2, 
4, and 5 would be listed under the atom c. A unification check could eliminate 5, 
but we still would be missing 3. Key 3 (and every key with a variable in it) could 
potentially contain the atom c. So to get the right answers under this approach, 
we will need to index every key that contains a variable under every atom - not an 
appealing situation. 

An alternative is to create indices based on both atoms and their position. So now 
we would be retrieving all the keys that have a c in the second argument position: 2 
and 4, plus the keys that have a variable as the second argument: 3. This approach 
seems to work much better, at least for the example shown. To create the index, we 
essentially superimpose the list structure of all the keys on top of each other, to arrive 
at one big discrimination tree. At each position in the tree, we create an index of the 
keys that have either an atom or a variable at that position. Figure 14.1 shows the 
discrimination tree for the six keys. 

Consider the query (. ?y c). Either the . or the c could be used as an index. 
The . in the predicate position retrieves all six keys. But the c in the second argument 
position retrieves only three keys: 2 and 4, which are indexed under c itself, and 3, 
which is indexed under the variable in that position. 

Now consider the query (. ?y (f ?z)). Again, the . serves as an index to all 
six keys. The f serves as an index to only three keys: the 5 and 6, which are indexed 

<a id='page-474'></a>

. A 
(.A .) (.A .) 
(PAC) (PAC) 
(PA?) (PA?) 
(PBC) (. A (F.?)) . 
(.8 (FC)) 
(. A (F.?)) 
. 
(PBC) 
(. A (F.?)) (. . (FC)) 
(. . (FC)) (. . (F C)) 
(PA(F.?) ) 
. 
(. . .) 
C 
(PAC) 
(PBC) 
? 
(PA?) 

Figure 14.1: Discrimination Tree with Six Keys 

directly under f in that position, and 3, which is indexed under the variable in a 
position along the path that lead to f. In general, all the keys indexed under variables 
along the path must be considered. 

The retrieval mechanism can overretrieve. Given the query (. a (f ?x)),the 
atom . will again retrieve all six keys, the atom a retrieves 1,2,3, and 6, and f again 
retrieves 5, 6, and 3. So f retrieves the shortest list, and hence it will be used to 
determine the final result. But key 5 is (. b (f c)), which does not match the query 
(pa (f?x)). 

We could eliminate this problem by intersecting all the lists instead of just taking 
the shortest list. It is perhaps feasible to do the intersection using bit vectors, but 
probably too slow and wasteful of space to do it using lists. Even if we did intersect 
keys, we would still overretrieve, for two reasons. First, we don't use . i 1 as an index, 
so we are ignoring the difference between (f ?x) and (f . ?x). Second, we are 
using wild-card semantics, so the query (. ?x ?x) would retrieve all six keys, when 

<a id='page-475'></a>
it should only retrieve three. Because of these problems, we make a design choice: 
we will first build a data base retrieval function that retrieves potential matches, and 
later worry about the unification process that will eliminate mismatches. 

We are ready for a more complete specification of the indexing strategy: 

* The value will be indexed under each non-nil nonvariable atom in the key, with 
a separate index for each position. For example, given the preceding data base, 
the atom a in the first argument position would index values 1,2,3, and 6, while 
the atom b in the second argument position would index value 4 and 5. The 
atom . in the predicate position would index all six values. 
In addition, we will maintain a separate index for variables at each position. For 
example, value 3 would be stored under the index "variable in second argument 
position." 

* "Position" does not refer solely to the linear position in the top-level list. For 
example, value 5 would be indexed under atom f in the caaddr position. 
* It follows that a key with . atoms will be indexed in. different ways. 
For retrieval, the strategy is: 

* For each non-nil nonvariable atom in the retrieval key, generate a list of possible 
matches. Choose the shortest such list. 
* Each list of possible matches will have to be augmented with the values indexed 
under a variable at every position "above." For example, f in the ca add r position 
retrieves value 5, but it also must retrieve value 3, because the third key has a 
variable in the caddr position, and caddr is "above" caaddr. 
* The discrimination tree may return values that are not valid matches. The 
purpose of the discrimination tree is to reduce the number of values we will 
have to unify against, not to determine the exact set of matches. 
It is important that the retrieval function execute quickly. If it is slow, we might 
just as well match against every key in the table linearly. Therefore, we will take 
care to implement each part efficiently. Note that we will have to compare the length 
of lists to choose the shortest possibility. Of course, it is trivial to compare lengths 
using length, but length requires traversing the whole list. We can do better if we 
store the length of the list explicitly. A list with its length will be called an nl1 st. 
It will be implemented as a cons cell containing the number of elements and a list 
of the elements themselves. An alternative would be to use extensible vectors with 
fill pointers. 

<a id='page-476'></a>

An nlist is implemented as a (count . elements) pair: 

(defun make-empty-nlist () 
"Create a new, empty nlist." 
(cons 0 nil)) 

(defun nlist-n (x) "The number of elements in an nlist." (carx)) 
(defun nlist-list (x) "The elements in an nlist." (cdr x)) 

(defun nlist-push (item nlist) 
"Add a new element to an nlist." 
(incf (car nlist)) 
(push item (cdr nlist)) 
nlist) 

Now we need a place to store these nlists. We will build the data base out of 
discrimination tree nodes called dtree nodes. Each dtree node has a field to hold 
the variable index, the atom indices, and pointers to two subnodes, one for the first 
and one for the rest. We implement dtrees as vectors for efficiency, and because we 
will never need a dtree-. predicate. 

(defstruct (dtree (:type vector)) 
(first nil) (rest nil) (atoms nil) (var (make-empty-nlist))) 

A separate dtree will be stored for each predicate. Since the predicates must be 
symbols, it is possible to store the dtrees on the predicate's property list. In most 
implementations, this will be faster than alternatives such as hash tables. 

(let ((predicates nil)) 

(defun get-dtree (predicate) 
"Fetch (or make) the dtree for this predicate." 
(cond ((get predicate 'dtree)) 

(t (push predicate predicates) 
(setf (get predicate 'dtree) (make-dtree))))) 

(defun clear-dtrees () 
"Remove all the dtrees for all the predicates." 
(dolist (predicate predicates) 

(setf (get predicate 'dtree) nil)) 
(setf predicates nil))) 

The function i ndex takes a relation as key and stores it in the dtree for the predicate 
of the relation. It calls dtree - i ndex to do all the work of storing a value under the 
proper indices for the key in the proper dtree node. 

The atom indices are stored in an association Ust. Property lists would not 
work, because they are searched using eq and atoms can be numbers, which are not 

<a id='page-477'></a>
necessarily eq. Association lists are searched using eql by default. An alternative 

would be to use hash tables for the index, or even to use a scheme that starts with 

association lists and switches to a hash table when the number of entries gets large. I 

use 1 ookup to look up the value of a key in a property list. This function, and its setf 

method, are defined on [page 896](chapter25.md#page-896). 

(defun index (key) 
"Store key in a dtree node. Key must be (predicate . args); 
it is stored in the predicate's dtree." 
(dtree-index key key (get-dtree (predicate key)))) 

(defun dtree-index (key value dtree) 
"Index value under all atoms of key in dtree." 
(cond 

((consp key) ; index on both first and rest 
(dtree-index (first key) value 
(or (dtree-first dtree) 
(setf (dtree-first dtree) (make-dtree)))) 
(dtree-index (rest key) value 
(or (dtree-rest dtree) 

(setf (dtree-rest dtree) (make-dtree))))) 
((null key)) ; don't index on nil 
((variable-p key) ; index a variable 

(nlist-push value (dtree-var dtree))) 
(t Make sure there is an nlist for this atom, and add to it 
(nlist-push value (lookup-atom key dtree))))) 

(defun lookup-atom (atom dtree) 
"Return (or create) the nlist for this atom in dtree." 
(or (lookup atom (dtree-atoms dtree)) 

(let ((new (make-empty-nlist))) 
(push (cons atom new) (dtree-atoms dtree)) 
new))) 

Now we define a function to test the indexing routine. Compare the output with 
figure 14.1. 

(defun test-index () 
(let ((props '((p a b) (p a c) (p a ?x) (p b c) 

(p b (f c)) (p a (f . ?x))))) 
(clear-dtrees) 
(mapc #*index props) 
(write (list props (get-dtree '.)) 

icircle t rarray t :pretty t) 
(values))) 

<a id='page-478'></a>

> (test-index) 

((#1=(P A B) 
#2=(P A C) 
#3=(P A ?X) 
#4=(P . C) 
#5=(P . (F O) 
#6=(P A (F . ?X))) 

#(#(NIL NIL (P (6 #6# #5# #4# #3# #2# #!#)) (0)) 
#(#(NIL NIL (B (2 #5# #4#) A (4 #6# #3# #2# #!#)) (0)) 
#(#(#(NIL NIL (F (2 #6# #5#)) (0)) 
#(#(NIL NIL (C (1 #5#)) (0)) 

#(NIL NIL NIL (0)) NIL (1 #6#)) 
(C (2 #4# #2#) . (1 #!#)) 
(1 #3#)) 

#(NIL NIL NIL (0)) 
NIL (0)) 
NIL (0)) 
NIL (0))) 

The next step is to fetch matches from the dtree data base. The function fetch takes 
a query, which must be a valid relation, as its argument, and returns a list of possible 
matches. It calls dtree-fetch to do the work: 

(defun fetch (query) 
"Return a list of buckets potentially matching the query, 
which must be a relation of form (predicate . args)." 
(dtree-fetch query (get-dtree (predicate query)) 

nil 0 nil most-positive-fixnum)) 

dtree-fetch must be passed the query and the dtree, of course, but it is also passed 
four additional arguments. First, we have to accumulate matches indexed under 
variables as we are searching through the dtree. So two arguments are used to pass 
the actual matches and a count of their total number. Second, we want dtree - fetch 
to return the shortest possible index, so we pass it the shortest answer found so far, 
and the size of the shortest answer. That way, as it is making its way down the tree, 
accumulating values indexed under variables, it can be continually comparing the 
size of the evolving answer with the best answer found so far. 

We could use nlists to pass around count/values pairs, but nlists only support a 
push operation, where one new item is added. We need to append together lists of 
values coming from the variable indices with values indexed under an atom. Append 
is expensive, so instead we make a list-of-lists and keep the count in a separate 
variable. When we are done, dtree-fetch and hence fetch does a multiple-value 
return, yielding the list-of-lists and the total count. 

<a id='page-479'></a>
There are four cases to consider in dtree-fetch. If the dtree is null or the query 
pattern is either null or a variable, then nothing will be indexed, so we should just 
return the best answer found so far. Otherwise, we bind var-. and var-1 ist to 
the count and list-of-lists of variable matches found so far, including at the current 
node. If the count var-. is greater than the best count so far, then there is no 
sense continuing, and we return the best answer found. Otherwise we look at the 
query pattern. If it is an atom, we use dtree-atom-f etch to return either the current 
index (along with the accumulated variable index) or the accumulated best answer, 
whichever is shorter. If the query is a cons, then we use dtree-fetch on the first 
part of the cons, yielding a new best answer, which is passed along to the call of 
dtree-fetch on the rest of the cons. 

(defun dtree-fetch (pat dtree var-list-in var-n-in best-list best-n) 
"Return two values: a list-of-lists of possible matches to pat. 
and the number of elements in the list-of-lists." 
(if (or (null dtree) (null pat) (variable-p pat)) 

(values best-list best-n) 

(let* ((var-nlist (dtree-var dtree)) 
(var-n (+ var-n-in (nlist-n var-nlist))) 
(var-list (if (null (nlist-list var-nlist)) 

var-1 ist-i . 
(cons (nlist-list var-nlist) 
var-list-in)))) 

(cond 
((>= var-n best-n) (values best-list best-n)) 
((atom pat) (dtree-atom-fetch pat dtree var-list var-n 

best-list best-n)) 
(t (multiple-value-bind (listl nl) 
(dtree-fetch (first pat) (dtree-first dtree) 
var-list var-n best-list best-n) 
(dtree-fetch (rest pat) (dtree-rest dtree) 
var-list var-n listl nl))))))) 

(defun dtree-atom-fetch (atom dtree var-list var-n best-list best-n) 
"Return the answers indexed at this atom (along with the vars), 
or return the previous best answer, if it is better." 
(let ((atom-nlist (lookup atom (dtree-atoms dtree)))) 

(cond 
((or (null atom-nlist) (null (nlist-list atom-nlist))) 
(values var-list var-n)) 
((and atom-nlist (< (incf var-n (nlist-n atom-nlist)) best-n)) 
(values (cons (nlist-list atom-nlist) var-list) var-n)) 
(t (values best-list best-n))))) 

Here we see a call to fetch on the data base created by test - i ndex. It returns two 
values: a list-of-lists of facts, and the total number of facts, three. 

<a id='page-480'></a>

> (fetch '(. ? c)) 
(((. . . (. A .) 
((. . ?.))) 
3 

Now let's stop and see what we have accomplished. The functions fetch and 
dtree-fetch fulfill their contract of returning potential matches. However, we still 
need to integrate the dtree facility with Prolog. We need to go through the potential 
matches and determine which candidates are actual matches. For simplicity we will 
use the version of u.i f y with binding lists defined in section 11.2. (It is also possible to 
construct a more efficient version that uses the compiler and the destructive function 
unifyl.) 

The function mapc- retri eve calls fetch to get a Ust-of-Usts of potential matches 
and then calls uni fy to see if the match is a true one. If the match is true, it calls 
the supplied function with the binding list that represents the unification as the 
argument, mapc-retri eve is proclaimed inl ine so that functions passed to it can 
also be compiled in place. 

(proclaim '(inline mapc-retrieve)) 

(defun mapc-retrieve (fn query) 
"For every fact that matches the query, 
apply the function to the binding list. " 
(dolist (bucket (fetch query)) 

(dolist (answer bucket) 
(let ((bindings (unify query answer))) 
(unless (eq bindings fail) 
(funcall fn bindings)))))) 

There are many ways to use this retriever. The function retri eve returns a list of the 
matching binding hsts, and retri eve-matches substitutes each binding hst into the 
original query so that the result is a list of expressions that unify with the query. 

(defun retrieve (query) 
"Find all facts that match query. Return a list of bindings." 
(let ((answers nil)) 

(mapc-retrieve #'(lambda (bindings) (push bindings answers)) 
query) 
answers)) 

(defun retrieve-matches (query) 
"Find all facts that match query. 
Return a list of expressions that match the query." 
(mapcar #'(lambda (bindings) (subst-bindings bindings query)) 

(retrieve query))) 

<a id='page-481'></a>
There is one further complication to consider. Recall that in our original Prolog 
interpreter, the function prove had to rename the variables in each clause as it 
retrieved it from the data base. This was to insure that there was no conflict between 
the variables in the query and the variables in the clause. We could do that in 
retrieve. However, if we assume that the expressions indexed in discrimination 
trees are tablelike rather than rulelike and thus are not recursive, then we can get 
away with renaming the variables only once, when they are entered into the data 
base. This is done by changing i ndex: 

(defun index (key) 
"Store key in a dtree node. Key must be (predicate . args); 
it is stored in the predicate's dtree." 
(dtree-index key (rename-variables key) ; store unique vars 

(get-dtree (predicate key)))) 

With the new i ndex in place, and after calling test - i ndex to rebuild the data base, 
we are now ready to test the retrieval mechanism: 

> (fetch '(p ?x c)) 
(((P . C) (P A O) 
((PA 7X3408))) 
3 

> (retrieve '(p ?x c)) 

(((7X3408 . C) (7X . A)) 
((7X . A)) 
((7X . B))) 

> (retrieve-matches '(p 7x c)) 

((P A C) (P A C) (P . .) 

> (retrieve-matches *(p 7x (7fn c))) 

((P A (7FN O) (P A (F O) (P . (F C))) 

Actually, it is better to use mapc-retrieve when possible, since it doesn't cons up 

answers the way retrieve and retrieve-matches do. The macro query-bind is 

provided as a nice interface to mapc - ret r i eve. The macro takes as arguments a list of 

variables to bind, a query, and one or more forms to apply to each retrieved answer. 

Within this list of forms, the variables will be bound to the values that satisfy the 

query. The syntax was chosen to be the same as mul ti pi e - va 1 ue - bi nd. Here we see 

a typical use of query - bi nd, its result, and its macro-expansion: 

<a id='page-482'></a>

> (query-bind (?x ?fn) '(p ?x (?fn c)) 

(format t "~&P holds between ~a and ~a of c." ?x ?fn)) =. 
. holds between . and F of c. 
. holds between A and F of c. 
. holds between A and ?FN of c. 
NIL 

= (mapc-retrieve 
#'(lambda (#:bindings6369) 
(let ((?x (subst-bindings #:bindings6369 '?.)) 
(?fn (subst-bindings #:bindings6369 '?fn))) 
(format t "~&P holds between ~a and ~a of c." ?x ?fn))) 
'(p ?x (?fn c))) 

Here is the implementation: 

(defmacro query-bind (variables query &body body) 
"Execute the body for each match to the query. 
Within the body, bind each variable." 
(let* ((bindings (gensym "BINDINGS")) 

(vars-and-vals 
(mapcar 
#'(lambda (var) 
(list var '(subst-bindings .bindings ',var))) 
variables))) 
'(mapc-retrieve 
#'(lambda (.bindings) 
(let ,vars-and-vals 
.body)) 
.query))) 

14.9 A Solution to the Completeness Problem 
We saw in chapter 6 that iterative deepening is an efficient way to cover a search 
space without falling into an infinite loop. Iterative deepening can also be used to 
guide the search in Prolog. It will insiu-e that all valid answers are found eventually, 
but it won't turn an infinite search space into a finite one. 

In the interpreter, iterative deepening is implemented by passing an extra argument 
to prove and prove-a 11 to indicate the depth remaining to be searched. When 
that argument is zero, the search is cut off, and the proof fails. On the next iteration 
the bounds will be increased and the proof may succeed. If the search is never cut off 
by a depth bound, then there is no reason to go on to the next iteration, because all 

<a id='page-483'></a>
proofs have already been found. The special variable *sea r ch - cut - off* keeps track 
of this. 

(defvar *search-cut-off* nil "Has the search been stopped?") 

(defun prove-all (goals bindings depth) 
"Find a solution to the conjunction of goals." 
This version just passes the depth on to PROVE, 

(cond ((eq bindings fail) fail) 
((null goals) bindings) 
(t (prove (first goals) bindings (rest goals) depth)))) 

(defun prove (goal bindings other-goals depth) 
"Return a list of possible solutions to goal." 
:; Check if the depth bound has been exceeded 
(if (= depth 0) 

(progn (setf *search-cut-off* t) 
fail) 
(let ((clauses (get-clauses (predicate goal)))) 
(if (listp clauses) 
(some 
#'(lambda (clause) 
(let ((new-clause (rename-variables clause))) 

(prove-al1 
(append (clause-body new-clause) other-goals) 
(unify goal (clause-head new-clause) bindings) 
(- depth 1)))) 

clauses) 

The predicate's "clauses" can be an atom: 
;; a primitive function to call 
(funcall clauses (rest goal) bindings 

other-goals depth))))) 

prove and . rove - a 11 now implement search cutoff, but we need something to control 
the iterative deepening of the search. First we define parameters to control the 
iteration: one for the initial depth, one for the maximum depth, and one for the 
increment between iterations. Setting the initial and increment values to one will 
make the results come out in strict breadth-first order, but will duplicate more effort 
than a slightly larger value. 

<a id='page-484'></a>

(defparameter *depth-start* 5 
"The depth of the first round of iterative search.") 
(defparameter *depth-incr* 5 
"Increase each iteration of the search by this amount.") 
(defparameter *depth-max* most-positive-fixnum 
"The deepest we will ever search.") 

A new version of top-level - prove will be used to control the iteration. It calls 
prove-al 1 for all depths from the starting depth to the maximum depth, increasing 
by the increment. However, it only proceeds to the next iteration if the search was 
cut off at some point in the previous iteration. 

(defun top-level-prove (goals) 
(let ((all-goals 
*(,goals (show-prolog-vars ,@(variables-in goals))))) 
(loop for depth from *depth-start* to *depth-max* by *depth-incr* 

while (let ((*search-cut-off* nil)) 
(prove-all all-goals no-bindings depth) 
*search-cut-off*))) 

(format t "~&No.") 
(values)) 

There is one final complication. When we increase the depth of search, we may 
find some new proofs, but we will also find all the old proofs that were found on the 
previous iteration. We can modify show-prol og-vars to only print proofs that are 
found with a depth less than the increment - that is, those that were not found on the 
previous iteration. 

(defun show-prolog-vars (vars bindings other-goals depth) 
"Print each variable with its binding. 
Then ask the user if more solutions are desired." 
(if (> depth *depth-incr*) 

fail 
(progn 

(if (null vars) 
(format t "~&Yes") 
(dolist (var vars) 

(format t "~&~a = ~a" var 
(subst-bindings bindings var)))) 

(if (continue-p) 
fail 
(prove-all other-goals bindings depth))))) 

To test that this works, try setting *depth-max* to 5 and running the following 
assertions and query. The infinite loop is avoided, and the first four solutions 
are found. 

<a id='page-485'></a>
(<- (natural 0)) 
(<- (natural (1+ ?n)) (natural ?n)) 

> (?- (natural ?n)) 

?N = 0; 

?N = (1+ 0); 

?N = (1+ (1+ 0)); 

?N = (1+ (1+ (1+ 0))); 

No. 

14.10 Solutions to the Expressiveness Problems 
In this section we present solutions to three of the limitations described above: 

* Treatment of (limited) higher-order predications. 
* Introduction of a frame-based syntax. 
* Support for possible worlds, negation, and disjunction. 
We also introduce a way to attach functions to predicates to do forward-chaining 

and error detection, and we discuss ways to extend unification to handle Skolem 

constants and other problems. 

Higher-Order Predications 

First we will tackle the problem of answering questions like "What kinds of animals 
are there?" Paradoxically, the key to allowing more expressiveness in this case is to 
invent a new, more limited language and insist that all assertions and queries are 
made in that language. That way, queries that would have been higher-order in the 
original language become first-order in the restricted language. 

The language admits three types of objects: categones, relations, and individuals. 
A category corresponds to a one-place predicate, a relation to a two-place predicate, 
and an individual to constant, or zero-place predicate. Statements in the language 
musthaveoneof five primitive operators: sub, rel, ind. val , and and. They have 
the following form: 

(sub subcategorysupercategory) 
(rel relation domain-category range-category) 
(i nd individual category) 
(val relation individual value) 
(and assertion...) 

<a id='page-486'></a>

The following table gives some examples, along with English translations: 

(sub dog animal) Dog is a kind of animal. 
(rel birthday animal date) The birthday relation holds between each animal 
and some date. 
(ind fido dog) The individual Fido is categorized as a dog. 
(val birthday fido july-1) The birthday of Fido is July-1. 
(and AB) Both A and Bare true. 
For those who feel more comfortable with predicate calculus, the following table 
gives the formal definition of each primitive. The most complicated definition is for 
rel. The form (rel RAB) means that every R holds between an individual of A 
and an individual of B, and furthermore that every individual of A participates in at 

least one R relation. 
(sub AB) Va:: A(x) D 
(rel RAB) "rfx^y: R{x,y) D A{x) A B{y) 
A\/xA{x) D 3y : R{x, y) 
(ind IC) C{I) 
(val RIV) R{I,V) 
(and PQ...) PAQ.,. 

Queries in the language, not surprisingly, have the same form as assertions, 
except that they may contain variables as well as constants. Thus, to find out what 
kinds of animals there are, use the query (sub ?kind animal). To find out what 
individual animals there are, use the query (ind ?x animal). To find out what 
individual animals of what kinds there are, use: 

(and (sub ?kind animal) (ind ?x ?kind)) 

The implemention of this new language can be based directly on the previous implementation 
of dtrees. Each assertion is stored as a fact in a dtree, except that 
the components of an and assertion are stored separately. The function add-fact 
does this: 

(defun add-fact (fact) 

"Add the fact to the data base." 

(if (eq (predicate fact) 'and) 

(mapc #*add-fact (args fact)) 
(index fact))) 

Querying this new data base consists of querying the dtree just as before, but with 
a special case for conjunctive (and) queries. Conceptually, the function to do this, 
retri eve-fact, should be as simple as the following: 

<a id='page-487'></a>
(defun retrieve-fact (query) 
"Find all facts that match query. Return a list of bindings. 
Warning!! this version is incomplete." 
(if (eq (predicate query) 'and) 

(retrieve-conjunction (args query)) 
(retrieve query bindings))) 

Unfortunately, there are some complications. Think about what must be done in 
retrieve-conjunction. It is passed a list of conjuncts and must return a list of 
binding lists, where each binding list satisfies the query. For example, to find out 
what people were born on July 1st, we could use the query: 

(and (val birthday ?p july-1) (ind ?p person)) 

retrieve-conjunction could solve this problem by first calling retrieve-fact on 
(val birthday ?p july-1). Once that is done, there is only one conjunct remaining, 
but in general there could be several, so we need to call ret r i eve - conj uncti on recursively 
with two arguments: theremainingconjuncts,andtheresultthat retrieve-fact 
gave for the first solution. Since retrieve-fact returns a list of binding lists, it will 
be easiest if retri eve-conjunct i on accepts such a list as its second argument. Furthermore, 
when it comes time to call retri eve- fact on the second conjunct, we will 
want to respect the bindings set up by the first conjunct. So retri eve -fact must 
accept a binding list as its second argument. Thus we have: 

(defun retrieve-fact (query &optional (bindings no-bindings)) 
"Find all facts that match query. Return a list of bindings." 
(if (eq (predicate query) 'and) 

(retrieve-conjunction (args query) (list bindings)) 
(retrieve query bindings))) 

(defun retrieve-conjunction (conjuncts bindings-lists) 
"Return a list of binding lists satisfying the conjuncts." 
(mapcan 

#'(lambda (bindings) 

(cond ((eq bindings fail) nil) 
((null conjuncts) (list bindings)) 
(t (retrieve-conjunction 

(rest conjuncts) 

(retrieve-fact 
(subst-bindings bindings (first conjuncts)) 
bindings))))) 

bindings-lists)) 

Notice that retrieve and therefore mapc-retrieve now also must accept a binding 
list. The changes to them are shown in the following. In each case the extra argument 

<a id='page-488'></a>

is made optional so that previously written functions that call these functions without 
passing in the extra argument will still work. 

(defun mapc-retrieve (fn query &optional (bindings no-bindings)) 
"For every fact that matches the query, 
apply the function to the binding list. " 
(dolist (bucket (fetch query)) 

(dolist (answer bucket) 
(let ((new-bindings (unify query answer bindings))) 
(unless (eq new-bindings fail) 
(funcall fn new-bindings)))))) 

(defun retrieve (query &optional (bindings no-bindings)) 
"Find all facts that match query. Return a list of bindings." 
(let ((answers nil)) 

(mapc-retrieve #'(lambda (bindings) (push bindings ansviers)) 
query bindings) 
answers)) 

Now add - fact and ret r i eve - fact comprise all we need to implement the language. 
Here is a short example where add-fact is used to add facts about bears and dogs, 
both as individuals and as species: 

> (add-fact *(sub dog animal)) => . 
> (add-fact '(sub bear animal)) => . 
> (add-fact '(ind Fido dog)) => . 
> (add-fact '(ind Yogi bear)) . 
> (add-fact '(val color Yogi brown)) => . 
> (add-fact '(val color Fido golden)) . 
> (add-fact '(val latin-name bear ursidae)) => . 
> (add-fact '(val latin-name dog canis-familiaris)) => . 

Now retrieve -fact is used to answer three questions: What kinds of animals are 
there? What are the Latin names of each kind of animal? and What are the colors of 
each individual bear? 

> (retrieve-fact '(sub ?kind animal)) 
(((?KIND . DOG)) 
((?KIND . BEAR))) 

> (retrieve-fact '(and (sub ?kind animal) 
(val latin-name ?kind ?latin))) 
(((7LATIN . CANIS-FAMILIARIS) (7KIND . DOG)) 
((7LATIN . URSIDAE) (7KIND . BEAR))) 

<a id='page-489'></a>
> (retrieve-fact '(and (ind ?x bear) (val color ?x ?c))) 

(((?C . BROWN) (?X . YOGI))) 

Improvements 

There are quite a few improvements that can be made to this system. One direction 
is to provide different kinds of answers to queries. The following two functions 
are similar to retri eve-matches in that they return lists of solutions that match the 
query, rather than lists of possible bindings: 

(defun retrieve-bagof (query) 

"Find all facts that match query. 

Return a list of queries with bindings filled in." 

(mapcar #'(lambda (bindings) (subst-bindings bindings query)) 

(retrieve-fact query))) 

(defun retrieve-setof (query) 
"Find all facts that match query. 
Return a list of unique queries with bindings filled in. " 
(remove-duplicates (retrieve-bagof query) :test #'equal)) 

Another direction to take is to provide better error checking. The current system 
does not complain if a fact or query is ill-formed. It also relies on the user to input all 
facts, even those that could be derived automatically from the semantics of existing 
facts. Forexample, the semantics of sub imply that if (sub bear animal) and (sub 
polar-bear bear) are true, then (subpolar-bear animal) must also be true. This 
kind of implication can be handled in two ways. The typical Prolog approach would 
be to write rules that derive the additional sub facts by backward-chaining. Then 
every query would have to check if there were rules to run. The alternative is to use 
aforward-chaining approach, which caches each new sub fact by adding it to the data 
base. This latter alternative takes more storage, but because it avoids rederiving the 
same facts over and over again, it tends to be faster. 

The following version of add-fact does error checking, and it automatically 
caches facts that can be derived from existing facts. Both of these things are done by 
a set of functions that are attached to the primitive operators. It is done in a data-
driven style to make it easier to add new primitives, should that become necessary. 

The function add-fact checks that each argument to a primitive relation is a 
nonvariable atom, and it also calls fact-present-p to check if the fact is already 
present in the data base. If not, it indexes the fact and calls run-attached-f . to do 
additional checking and caching: 

(defparameter ^primitives* '(and sub ind rel val)) 

<a id='page-490'></a>

(defun add-fact (fact) 
"Add the fact to the data base." 
(cond ((eq (predicate fact) *and) 

(mapc #*add-fact (args fact))) 

((or (not (every #*atom (args fact))) 
(some #'variable-p (args fact)) 
(not (member (predicate fact) *primitives*))) 

(error "111-formed fact: ~a" fact)) 

((not (fact-present-p fact)) 
(index fact) 
(run-attached-fn fact))) 

t) 

(defun fact-present-p (fact) 
"Is this fact present in the data base?" 
(retrieve fact)) 

The attached functions are stored on the operator's property list under the indicator 

attached-fn: 

(defun run-attached-fn (fact) 
"Run the function associated with the predicate of this fact." 
(apply (get (predicate fact) 'attached-fn) (args fact))) 

(defmacro def-attached-fn (pred args &body body) 
"Define the attached function for a primitive." 
'(setf (get '.pred 'attached-fn) 

#'(lambda ,args ..body))) 

The attached functions for ind and val are fairly simple. If we know (sub bear 
ani mal), then when ( i nd Yogi bea r) is asserted, we have to also assert ( i nd Yogi 
animal). Similarly, the values in a val assertion must be individuals of the categories 
in the relation's rel assertion. That is, if ( rel bi rthday animal date) is a fact and 
(val birthday Lee ju1y-l) is added, then we can conclude (ind Lee animal) and 
(ind july-1 date). The followingfunctions add the appropriate facts: 

(def-attached-fn ind (individual category) 
Cache facts about inherited categories 
(query-bind (?super) '(sub .category ?super) 
(add-fact '(ind .individual .?super)))) 

<a id='page-491'></a>

(def-attached-fn val (relation indi ind2) 
Make sure the individuals are the right kinds 

(query-bind (?catl ?cat2) '(rel .relation ?catl ?cat2) 
(add-fact *(ind ,indl .?catl)) 
(add-fact '(ind .ind2 .?cat2)))) 

The attached function for rel simply runs the attached function for any individual of 
the given relation. Normally one would make all rel assertions before i nd assertions, 
so this will have no effect at all. But we want to be sure the data base stays consistent 
even if facts are asserted in an unusual order. 

(def-attached-fn rel (relation catl cat2) 
Run attached function for any IND's of this relation 
(query-bind (?a ?b) '(ind .relation ?a ?b) 
(run-attached-fn '(ind .relation .?a .?b)))) 

The most complicated attached function is for sub. Adding a fact such as (sub bear 
animal) causes the following to happen: 

* All of animal's supercategories (such as 1 iving-thing) become supercategories 
of all of bea r's subcategories (such as pol ar - bea r). 
* animal itself becomes a supercategory all of bear's subcategories. 
* bear itself becomes a subcategory of all of animal's supercategories. 
* All of the individuals of bear become individuals of animal and its supercategories. 
The following accomplishes these four tasks. It does it with four calls to 
index-new-fact, which is used instead of add-fact because we don't need to run 
the attached function on the new facts. We do, however, need to make sure that we 
aren't indexing the same fact twice. 

(def-attached-fn sub (subcat supercat) 
Cache SUB facts 

(query-bind (?super-super) '(sub .supercat ?super-super) 
(index-new-fact '(sub .subcat .?super-super)) 
(query-bind (?sub-sub) '(sub ?sub-sub .subcat) 

(index-new-fact '(sub .?sub-sub .?super-super)))) 
(query-bind (?sub-sub) '(sub ?sub-sub .subcat) 
(index-new-fact '(sub .?sub-sub .supercat))) 
Cache IND facts 
(query-bind (?super-super) '(sub .subcat ?super-super) 
(query-bind (?sub-sub) '(sub ?sub-sub .supercat) 
(query-bind (?ind) '(ind ?ind .?sub-sub) 
(index-new-fact '(ind .?ind .?super-super)))))) 

<a id='page-492'></a>

(defun index-new-fact (fact) 

"Index the fact in the data base unless it is already there." 

(unless (fact-present-p fact) 

(index fact))) 

The following function tests the attached functions. It shows that adding the single 
fact (sub bea r ani mal) to the given data base causes 18 new facts to be added. 

(defun test-bears () 

(clear-dtrees) 

(mapc #'add-fact 

'((sub animal living-thing) 

(sub living-thing thing) (sub polar-bear bear) 

(sub grizzly bear) (ind Yogi bear) (ind Lars polar-bear) 

(ind Helga grizzly))) 

(trace index) 

(add-fact '(sub bear animal)) 

(untrace index)) 

> (test-bears) 

(1 ENTER INDEX: (SUB BEAR ANIMAL)) 

(1 EXIT INDEX: T) 

(1 ENTER INDEX: (SUB BEAR THING)) 

(1 EXIT INDEX: T) 

(1 ENTER INDEX: (SUB GRIZZLY THING)) 

(1 EXIT INDEX: T) 

(1 ENTER INDEX: (SUB POLAR-BEAR THING)) 

(1 EXIT INDEX: T) 

(1 ENTER INDEX: (SUB BEAR LIVING-THING)) 

(1 EXIT INDEX: T) 

(1 ENTER INDEX: (SUB GRIZZLY LIVING-THING)) 

(1 EXIT INDEX: T) 

(1 ENTER INDEX: (SUB POLAR-BEAR LIVING-THING)) 

(1 EXIT INDEX: T) 

(1 ENTER INDEX: (SUB GRIZZLY ANIMAL)) 

(1 EXIT INDEX: T) 

(1 ENTER INDEX: (SUB POLAR-BEAR ANIMAL)) 

(1 EXIT INDEX: T) 

(1 ENTER INDEX: (IND LARS LIVING-THING)) 

(1 EXIT INDEX: T) 

(1 ENTER INDEX: (IND HELGA LIVING-THING)) 

(1 EXIT INDEX: T) 

(1 ENTER INDEX: (IND YOGI LIVING-THING)) 

(1 EXIT INDEX: T) 

(1 ENTER INDEX: (IND LARS THING)) 

(1 EXIT INDEX: T) 

(1 ENTER INDEX: (IND HELGA THING)) 

<a id='page-493'></a>
(1 EXIT INDEX: T) 
(1 ENTER INDEX: (IND YOGI THING)) 
(1 EXIT INDEX: T) 
(1 ENTER INDEX: (IND LARS ANIMAD) 
(1 EXIT INDEX: .) 
(1 ENTER INDEX: (IND HELGA ANIMAD) 
(1 EXIT INDEX: .) 
(1 ENTER INDEX: (IND YOGI ANIMAD) 
(1 EXIT INDEX: .) 
(INDEX) 

A Frame Language 

Another direction we can take is to provide an alternative syntax that will be easier 
to read and write. Many representation languages are based on the idea of frames, 
and their syntax reflects this. A frame is an object with slots. We will continue to use 
the same data base in the same format, but we will provide an alternative syntax that 
considers the individuals and categories as frames, and the relations as slots. 

Here is an example of the frame syntax for individuals, which uses the operator 

a.Note that it is more compact than the equivalent notation using the primitives. 
(a person (name Joe) (age 27)) = 

(and (ind personl person) 
(val name personl Joe) 
(val age personl 27)) 

The syntax also allows for nested expressions to appear as the values of slots. Notice 
that the Skolem constant personl was generated automatically; an alternative is 
to supply a constant for the individual after the category name. For example, the 
following says that Joe is a person of age 27 whose best friend is a person named Fran 
who is 28 and whose best friend is Joe: 

(a person pi (name Joe) (age 27) 
(best-friend (a person (name Fran) (age 28) 
(best-friend pi)))) = 

(and (ind pi person) (val name pi joe) (val age pi 27) 
(ind person2 person) (val name person2 fran) 
(val age person2 28) (val best-friend person2 pi) 
(val best-friend pi person2)) 

<a id='page-494'></a>

The frame syntax for categories uses the operator each. For example: 

(each person (isa animal) (name person-name) (age integer)) = 

(and (sub person animal) 
(rel name person person-name) 
(rel age person integer)) 

The syntax for queries is the same as for assertions, except that variables are used 
instead of the Skolem constants. This is true even when the Skolem constants are 
automatically generated, as in the following query: 

(a person (age 27)) = (AND (IND ?3 PERSON) (VAL AGE ?3 27)) 

To support the frame notation, we define the macros a and each to make assertions 
and ?? to make queries. 

(defmacro a (&rest args) 
"Define a new individual and assert facts about it in the data base." 
*(add-fact \(translate-exp (cons *a args)))) 

(defmacro each (&rest args) 
"Define a new category and assert facts about it in the data base." 
'(add-fact (translate-exp (cons 'each args)))) 

(defmacro ?? (&rest queries) 
"Return a list of answers satisfying the query or queries." 
*(retrieve-setof 

'.(translate-exp (maybe-add 'and (replace-?-vars queries)) 
rquery))) 

All three of these macros call on trans! ate - exp to translate from the frame syntax to 
the primitive syntax. Note that an a or ea ch expression is computing a conjunction of 
primitive relations, but it is also computing a term when it is used as the nested value 
of a slot. It would be possible to do this by returning multiple values, but it is easier to 
build translate - exp as a set of local functions that construct facts and push them on 
the local variable conj uncts. At the end, the list of conj uncts is returned as the value 
of the translation. The local functions trans! ate-a and trans! ate-each return the 
atom that represents the term they are translating. The local function translate 
translates any kind of expression, trans! ate -s! ot handles a slot, and co!! ect- f act 
is responsible for pushing a fact onto the list of conjuncts. The optional argument 
query-mode-p tells what to do if the individual is not provided in an a expression. If 
query-mode-p is true, the individual will be represented by a variable; otherwise it 
will be a Skolem constant. 

<a id='page-495'></a>
(defun translate-exp (exp &optional query-mode-p) 
"Translate exp into a conjunction of the four primitives." 
(let ((conjuncts nil)) 

(labels 
((collect-fact (&rest terms) (push terms conjuncts)) 

(translate (exp) 
Figure out what kind of expression this is 

(cond 
((atom exp) exp) 
((eq (first exp) *a) (translate-a (rest exp))) 
((eq (first exp) 'each) (translate-each (rest exp))) 
(t (apply #'collect-fact exp) exp))) 

(translate-a (args) 
translate (A category Cind] (rel filler)*) 
(let* ((category (pop args)) 
(self (cond ((and args (atom (first args))) 

(pop args)) 
(query-mode-p (gentemp "?")) 
(t (gentemp (string category)))))) 

(collect-fact 'ind self category) 
(dolist (slot args) 
(translate-slot 'val self slot)) 
self)) 

(translate-each (args) 
;; translate (EACH category [(isa cat*)] (slot cat)*) 
(let* ((category (pop args))) 

(when (eq (predicate (first args)) 'isa) 
(dolist (super (rest (pop args))) 
(collect-fact 'sub category super))) 
(dolist (slot args) 
(translate-slot 'rel category slot)) 
category)) 

(translate-slot (primitive self slot) 

translate (relation value) into a REL or SUB 
(assert (= (length slot) 2)) 
(collect-fact primitive (first slot) self 

(translate (second slot))))) 

Body of translate-exp: 
(translate exp) Build up the list of conjuncts 
(maybe-add 'and (nreverse conjuncts))))) 

<a id='page-496'></a>

The auxiliary functions maybe - add and repl ace -? - va r s are shown in the following: 

(defun maybe-add (op exps &optional if-nil) 
"For example, (maybe-add 'and exps t) returns 
t if exps is nil, (first exps) if there is only one. 
and (and expl exp2...) if there are several exps." 
(cond ((null exps) if-nil) 

((length=1 exps) (first exps)) 
(t (cons op exps)))) 

(defun length=1 (x) 
"Is X a list of length 1?" 
(and (consp x) (null (cdr x)))) 

(defun replace-?-vars (exp) 
"Replace each ? in exp with a temporary var: 7123" 
(cond ((eq exp '7) (gentemp "7")) 

((atom exp) exp) 

(t (reuse-cons (replace-7-vars (first exp)) 
(replace-7-vars (rest exp)) 
exp)))) 

Possible Worlds: Truth, Negation, and Disjunction 

In this section we address four problems: distinguishing unknown from f al se, representing 
negations, representing disjunctions, and representing multiple possible 
states of affairs. It turns out that all four problems can be solved by introducing 
two new techniques: possible worlds and negated predicates. The solution is not 
completely general, but it is practical in a wide variety of applications. 

There are two basic ways to distinguish unknown from false. The first possibility 
is to store a truth value - true or false - along with each proposition. The second 
possibility is to include the truth value as part of the proposition. There are several 
syntactic variations on this theme. The following table shows the possibilities for 
the propositions "Jan likes Dean is true" and "Jan likes Ian is false:" 

Approach True Prop. False Prop. 
(1) 
(2a) 
(likes(likes 
Jan Dean) 
true Jan Dean) 
-true 
(likes(likes 
Jan Ian) -false 
false Jan Ian) 
{2b) (likes Jan Dean) (not (likes Jan Dean)) 
(2c) (likes Jan Dean) (~likes Jan Dean) 

The difference between (1) and (2) shows up when we want to make a query. 
With (1), we make the single query (1 i kes JanDean) (or perhaps (1 i kes Jan ?x)), 
and the answers will tell us who Jan does and does not like. With (2), we make one 

<a id='page-497'></a>
query to find out what liking relationships are true, and another to find out which 
ones are false. In either approach, if there are no responses then the answer is truly 
unknown. 

Approach (1) is better for applications where most queries are of the form "Is 
this sentence true or false?" But applications that include backward-chaining rules 
are not like this. The typical backward-chaining rule says "Conclude X is true ifY is 
true." Thus, most queries will be of the type "Is Y true?" Therefore, some version of 
approach (2) is preferred. 

Representing true and false opens the door to a host of possible extensions. First, 
we could add multiple truth values beyond the simple "true" and "false." These 
could be symbolic values like "probably-true" or "false-by-default" or they could be 
numeric values representing probabilities or certainty factors. 

Second, we could introduce the idea of possible worlds. That is, the truth of a 
proposition could be unknown in the current world, but true if we assume p, and 
false if we assume q. In the possible world approach, this is handled by calling the 
current world W, and then creating a new world VFi, which is just like W except 
that . is true, and w2, which is just like W except that q is true. By doing reasoning 
in different worlds we can make predictions about the future, resolve ambiguitites 
about the current state, and do reasoning by cases. 

For example, possible worlds allow us to solve Moore's communism/democracy 
problem ([page 466](chapter14.md#page-466)). We create two new possible worlds, one where is a democracy 
and one where it is communist. In each world it is easy to derive that there is 
a democracy next to a communist country. The trick is to realize then that the 
two worlds form a partition, and that therefore the assertion holds in the original 
"real" world as well. This requires an interaction between the Prolog-based tactical 
reasoning going on within a world and the planning-based strategic reasoning that 
decides which worlds to consider. 

We could also add a truth maintenance system (or TMS) to keep track of the assumptions 
or justifications that lead to each fact being considered true. A truth 
maintenance system can lessen the need to backtrack in a search for a global solution. 
Although truth maintenance systems are an important part of AI programming, 
they will not be covered in this book. 

In this section we extend the dtree facility (section 14.8) to handle truth values 
and possible worlds. With so many options, it is difficult to make design choices. We 
will choose a fairly simple system, one that remains close to the simplicity and speed 
of Prolog but offers additional functionality when needed. We will adopt approach 
(2c) to truth values, using negated predicates. For example, the negated predicate of 
1 i kes is ~1 i kes, which is pronounced "not likes." 

We will also provide minimal support for possible worlds. Assume that there is 
always a current world, W, and that there is a way to create alternative worlds and 
change the current world to an alternative one. Assertions and queries will always be 
made with respect to the current world. Each fact is indexed by the atoms it contains. 

<a id='page-498'></a>

just as before. The difference is that the facts are also indexed by the current world. 
To support this, we need to modify the notion of the numbered list, or nlist, to 
include a numbered association list, or nal i st. The following is an nal i st showing 
six facts indexed under three different worlds: WO, Wl, and W2: 

(6 (WO #1# #2# #3#) (Wl #4#) (W2 #5# #6#)) 

The fetching routine will remain unchanged, but the postfetch processing will have 
to sort through the nalists to find only the facts in the current world. It would also be 
possible for fetch to do this work, but the reasoning is that most facts will be indexed 
under the "real world," and only a few facts will exist in alternative, hypothetical 
worlds. Therefore, we should delay the effort of sorting through the answers to 
eliminate those answers in the wrong world - it may be that the first answer fetched 
will suffice, and then it would have been a waste to go through and eliminate other 
answers. The following changes to i ndex and dtree -i ndex add support for worlds: 

(defvar *world* *W0 "The current world used by index and fetch.") 

(defun index (key &optional (world *world*)) 
"Store key in a dtree node. Key must be (predicate . args); 
it is stored in the dtree, indexed by the world." 
(dtree-index key key world (get-dtree (predicate key)))) 

(defun dtree-index (key value world dtree) 
"Index value under all atoms of key in dtree." 
(cond 

((consp key) ; index on both first and rest 
(dtree-index (first key) value world 
(or (dtree-first dtree) 
(setf (dtree-first dtree) (make-dtree)))) 
(dtree-index (rest key) value world 
(or (dtree-rest dtree) 
(setf (dtree-rest dtree) (make-dtree))))) 
((null key)) ; don't index on nil 

((variable-p key) ; index a variable 
(nalist-push world value (dtree-var dtree))) 
(t ;; Make sure there is an nlist for this atom, and add to it 
(nalist-push world value (lookup-atom key dtree))))) 

The new function nalist-push adds a value to an nalist, either by inserting the value 
in an existing key's list or by adding a new key/value list: 

<a id='page-499'></a>
(defun nalist-push (key val nalist) 
"Index val under key in a numbered al ist. " 
;; An nalist is of the form (count (key val*)*) 

Ex: (6 (nums 1 2 3) (letters a b c)) 
(incf (car nalist)) 
(let ((pair (assoc key (cdr nalist)))) 

(if pair 
(push val (cdr pair)) 
(push (list key val) (cdr nalist))))) 

In the following, fetch is used on the same data base created by tes t -i ndex, indexed 
under the world WO. This time the result is a list-of-lists of world/values a-lists. The 
count, 3, is the same as before. 

> (fetch '(p ?x c)) 
(((WO (P . C) (P A C))) 
((WO (P A ?X)))) 
3 

So far, worlds have been represented as symbols, with the implication that different 
symbols represent completely distinct worlds. That doesn't make worlds very easy 
to use. We would like to be able to use worlds to explore alternatives - create a 
new hypothetical world, make some assumptions (by asserting them as facts in the 
hypothetical world), and see what can be derived in that world. It would be tedious 
to have to copy all the facts from the real world into each hypothetical world. 

An alternative is to establish an inheritance hierarchy among worlds. Then a fact 

is considered true if it is indexed in the current world or in any world that the current 

world inherits from. 

To support inheritance, we will implement worlds as structures with a name field 
and a field for the list of parents the world inherits from. Searching through the 
inheritance lattice could become costly, so we will do it only once each time the user 
changes worlds, and mark all the current worlds by setting the current field on or 
off. Here is the definition for the world structure: 

(defstruct (world (:print-function print-world)) 
name parents current) 

We will need a way to get from the name of a world to the world structure. Assuming 
names are symbols, we can store the structure on the name's property list. The 
function get-worl d gets the structure for a name, or builds a new one and stores it. 
get - wor 1 d can also be passed a world instead of a name, in which case it just returns 
the world. We also include a definition of the default initial world. 

<a id='page-500'></a>

(defun get-world (name &optional current (parents (list *world*))) 
"Look up or create the world with this name. 
If the world is new, give it the list of parents." 
(cond ((world-p name) name) ; ok if it already is a world 

((get name 'world)) 
(t (setf (get name 'world) 
(make-world rname name .-parents parents 
.'current current))))) 

(defvar *world* (get-world 'WO nil nil) 
"The current world used by index and fetch.") 

The function use-worl d is used to switch to a new world. It first makes the current 
world and all its parents no longer current, and then makes the new chosen world and 
all its parents current. The function use-new-worl d is more efficient in the common 
case where you want to create a new world that inherits from the current world. It 
doesn't have to turn any worlds off; it j ust creates the new world and makes it current. 

(defun use-world (world) 
"Make this world current." 
;; If passed a name, look up the world it names 
(setf world (get-world world)) 
(unless (eq world *world*) 

Turn the old world(s) off and the new one(s) on, 
;; unless we are already using the new world 
(set-world-current *world* nil) 
(set-world-current world t) 
(setf *world* world))) 

(defun use-new-world () 
"Make up a new world and use it. 
The world inherits from the current world." 
(setf *world* (get-world (gensym "W"))) 
(setf (world-current *world*) t) 
*world*) 

(defun set-world-current (world on/off) 
"Set the current field of world and its parents on or off." 

nil is off, anything else is on. 
(setf (world-current world) on/off) 
(dolist (parent (world-parents world)) 

(set-world-current parent on/off))) 

We also add a print function for worlds, which just prints the world's name. 

<a id='page-501'></a>
(defun print-world (world &optional (stream t) depth) 
(declare (ignore depth)) 
(prinl (world-name world) stream)) 

The format of the dtree data base has changed to include worlds, so we need 
new retrieval functions to search through this new format. Here the functions 
mapc-retrieve, retrieve, and retrieve-bagof are modified to give new versions 
that treat worlds. To reflect this change, the new functions all have names ending in 

-in-world: 

(defun mapc-retrieve-in-world (fn query) 
"For every fact in the current world that matches the query, 
apply the function to the binding list. " 
(dolist (bucket (fetch query)) 

(dolist (world/entries bucket) 
(when (world-current (first world/entries)) 
(dolist (answer (rest world/entries)) 
(let ((bindings (unify query answer))) 
(unless (eq bindings fail) 
(funcall fn bindings)))))))) 

(defun retrieve-in-world (query) 
"Find all facts that match query. Return a list of bindings." 
(let ((answers nil)) 

(mapc-retrieve-in-world 
#'(lambda (bindings) (push bindings answers)) 
query) 

answers)) 

(defun retrieve-bagof-in-world (query) 
"Find all facts in the current world that match query. 
Return a list of queries with bindings filled in. " 
(mapcar #'(lambda (bindings) (subst-bindings bindings query)) 

(retrieve-in-world query))) 

Now let's see how these worlds work. First, in WO we see that the facts from 
test -i ndex are still in the data base: 

> *world* ^ WO 

> (retrieve-bagof-in-world *(p ?z c)) ^ 
((P A C) (P A C) (P . .) 

<a id='page-502'></a>

Now we create and use a new world that inherits from WO. Two new facts are added 
to this new world: 

> (use-new-world) W7031 
> (index *(p new c)) => . 
> (index 'Cp b b)) => . 

We see that the two new facts are accessible in this world: 

> (retrieve-bagof-in-world '(p ?z c)) 
((P A C) (P A C) (P . C) (P NEW O) 

> (retrieve-bagof-in-world '(^p ?x ?y)) ^ 
((~P . .)) 

Now we create another world as an alternative to the current one by first switching 
back to the original WO, then creating the new world, and then adding some facts: 

> (use-world *W0) WO 

> (use-new-world) W7173 

> (index *(p newest c)) ^ . 
> (index '(~p c newest)) . 

Here we see that the facts entered in W7031 are not accessible, but the facts in the new 
world and in WO are: 

> (retrieve-bagof-in-world '(p ?z c)) => 
((P A C) (P A C) (P . C) (P NEWEST O) 

> (retrieve-bagof-in-world '(^p ?x ?y)) 
ir? C NEWEST)) 

Unification, Equality, Types, and Skolem Constants 

The lesson of the zebra puzzle in section 11.4 was that unification can be used to 
lessen the need for backtracking, because an uninstantiated logic variable or partially 
instantiated term can stand for a whole range of possible solutions. However, this 
advantage can quickly disappear when the representation forces the problem solver 
to enumerate possible solutions rather than treating a whole range of solutions as one. 
For example, consider the following query in the frame language and its expansion 
into primitives: 

<a id='page-503'></a>
(a person (name Fran)) 
= (and (ind ?p person) (val name ?p fran)) 

The way to answer this query is to enumerate all individuals ?p of type person and 
then check the name slot of each such person. It would be more efficient if (i nd ?p 
person) did not act as an enumeration, but rather as a constraint on the possible 
values of ?p. This would be possible if we changed the definition of variables (and 
of the unification function) so that each variable had a type associated with it. In 
fact, there are at least three sources of information that have been implemented as 
constraints on variables terms: 

* The type or category of the term. 
* The members or size of a term considered as a set or list. 
* Other terms this term is equal or not equal to. 
Note that with a good solution to the problem of equality, we can solve the problem 
of Skolem constants. The idea is that a regular constant unifies with itself but no 
other regular constant. On the other hand, a Skolem constant can potentially unify 
with any other constant (regular or Skolem). The equality mechanism is used to keep 
track of each Skolem variable's possible bindings. 

14.11 History and References 
Brachman and Levesque (1985) collect thirty of the key papers in knowledge representation. 
Included are some early approaches to semantic network based (Quillian 
1967) and logic-based (McCarthy 1968) representation. Two thoughtful critiques 
of the ad hoc use of representations without defining their meaning are by Woods 
(1975) and McDermott (1978). It is interesting to contrast the latter with McDermott 
1987, which argues that logic by itself is not sufficient to solve the problems of AI. 
This argument should not be surprising to those who remember the slogan logic = 
algonthm -control. 

Genesereth and Nilsson's textbook (1987) cover the predicate-calculus-based approach 
to knowledge representation and AI in general. Ernest Davis (1990) presents 
a good overview of the field that includes specialized representations for time, space, 
qualitative physics, propositional attitudes, and the interaction between agents. 

Many representation languages focus on the problem of defining descriptions for 
categories of objects. These have come to be known as term-subsumption languages. 
Examples include KL-ONE (Schmolze and Lipkis 1983) and KRYPTON (Brachman, 
Fikes, and Levesque 1983). See Lakoff 1987 for much more on the problem of 
categories and prototypes. 

<a id='page-504'></a>

Hector Levesque (1986) points out that the areas Prolog has difficulty with - 
disjunction, negation, and existentials - all involve a degree of vagueness. In his 
term, they lack vividness. A vivid proposition is one that could be represented 
directly in a picture: the car is blue; she has a martini in her left hand; Albany is the 
capital of New York. Nonvivid propositions cannot be so represented: the car is not 
blue; she has a martini in one hand; either Albany or New York City is the capital 
of New York. There is interest in separating vivid from nonvivid reasoning, but no 
current systems are actually built this way. 

The possible world approach of section 14.10 was used in the MRS system (Russell 
1985). More recent knowledge representation systems tend to use truth maintenance 
systems instead of possible worlds. This approach was pioneered by Doyle (1979) 
and McAllester (1982). Doyle tried to change the name to "reason maintenance," in 
(1983), but it was too late. The version in widest used today is the assumption-based 
truth maintenance system, or ATMS, developed by de Kleer (1986a,b,c). Charniak 
et al. (1987) present a complete Common Lisp implementation of a McAllesterstyleTMS. 


There is little communication between the logic programming and knowledge 
representation communities, even though they cover overlapping territory. Colmerauer 
(1990) and Cohen (1990) describe Logic Programming languages that address 
some of the issues covered in this chapter. Key papers in equality reasoning include 
Caller and Fisher 1974, Kornfeld 1983,^ Jaffar, Lassez, and Maher 1984, and van 
Emden and Yukawa 1987. H&ouml;dobler's book (1987) includes an overview of the area. 
Papers on extending unification in ways other than equality include Ait-Kaci et al. 
1987 and Staples and Robinson 1988. Finally, papers on extending Prolog to cover 
disjunction and negation (i.e., non-Horn clauses) include Loveland 1987, Plaisted 
1988, and Stickell988. 

14.12 Exercises 
&#9635; Exercise 14.1 [m] Arrange to store dtrees in a hash table rather than on the property 
list of predicates. 

&#9635; Exercise 14.2 [m] Arrange to store the dtree-atoms in a hash table rather than in 
an association list. 

&#9635; Exercise 14.3 [m] Change the dtree code so that .i 1 is used as an atom index. Time 
the performance on an application and see if the change helps or hurts. 

^ A commentary on this paper appears in Elcock and Hoddinott 1986. 

<a id='page-505'></a>
&#9635; Exercise 14.4 [m] Consider the query (. a b c d e f g). If the index under a 
returns only one or two keys, then it is probably a waste of time for dtree-fetc h 
to consider the other keys in the hope of finding a smaller bucket. It is certainly 
a waste if there are no keys at all indexed under a. Make appropriate changes to 
dtree-fetch . 

&#9635; Exercise 14.5 [h] Arrange to delete elements from a dtree. 

&#9635; Exercise 14.6 [h] Implement iterative-deepening search in the Prolog compiler. 
You will have to change each function to accept the depth as an extra argument, and 
compile in checks for reaching the maximum depth. 

&#9635; Exercise 14.7 [d] Integrate the Prolog compiler with the dtree data base. Use 
the dtrees for predicates with a large number of clauses, and make sure that each 
predicate that is implemented as a dtree has a Prolog primitive accessing the dtree. 

&#9635; Exercise 14.8 [d] Add support for possible worlds to the Prolog compiler with 
dtrees. This support has already been provided for dtrees, but you will have to 
provide it for ordinary Prolog rules. 

&#9635; Exercise 14.9 [h] Integrate the language described in section 14.10 and the frame 
syntax from section 14.10 with the extended Prolog compiler from the previous 
exercise. 

&#9635; Exercise 14.10 [d] Build a strategic reasoner that decides when to create a possible 
world and does reasoning by cases over these worlds. Use it to solve Moore's problem 
([page 466](chapter14.md#page-466)). 

<a id='page-506'></a>

14.13 Answers 
Answer 14.1 

(let ((dtrees (make-hash-table :test #'eq))) 

(defun get-dtree (predicate) 
"Fetch (or make) the dtree for this predicate." 
(setf (gethash predicate dtrees) 

(or (gethash predicate dtrees) 
(make-dtree)))) 

(defun clear-dtrees () 
"Remove all the dtrees for all the predicates." 
(clrhash dtrees))) 

Answer 14.5 Hint: here is the code for nl i st - del ete. Now figure out how to find 
all the nlists that an item is indexed under. 

(defun nlist-delete (item nlist) 
"Remove an element from an nlist . 
Assumes that item is present exactly once." 
(decf (car nlist)) 
(setf (cdr nlist) (delete item (cdr nlist) rcount D) 
nlist) 

