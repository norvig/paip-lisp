# Chapter 4 {docsify-ignore}
<a id='page-109'></a>

GPS: The Genera 
Problem Solver 

There are now in the world machines that think. 

—Herbert Simon 
Nobel Prize-winning Al researcher 

I I 1 he General Problem Solver, developed in 1957 by Alan Newell and Herbert Simon, em-
I bodied a grandiose vision: a single computer program that could solve any problem, 

JL given a suitable description of the problem. GPS caused quite a stir when it was introduced, 
and some people in AI felt it would sweep in a grand new era of intelligent machines. 
Simon went so far as to make this statement about his creation: 

It is not my aim to surprise or shock you. ... But the simplest way I can summarize is to say 
that there are now in the world machines that think, that learn and create. Moreover, their 
ability to do these things is going to increase rapidly until-in a visible future-the range of 
problems they can handle will be coextensive with the range to which the human mind has 
been applied. 

<a id='page-110'></a>

Although GPS never lived up to these exaggerated claims, it was still an important 
program for historical reasons. It was the first program to separate its problem-
solving strategy from its knowledge of particular problems, and it spurred much 
further research in problem solving. For all these reasons, it is a fitting object 
of study. 

The original GPS program had a number of minor features that made it quite 
complex. In addition, it was written in an obsolete low-level language, IPL, that added 
gratuitous complexity. In fact, the confusing nature of IPL was probably an important 
reason for the grand claims about GPS. If the program was that complicated, it must 
do something important. We will be ignoring some of the subtleties of the original 
program, and we will use Common Lisp, a much more perspicuous language than 
IPL. The result will be a version of GPS that is quite simple, yet illustrates some 
important points about AI. 

On one level, this chapter is about GPS. But on another level, it is about the process 
of developing an AI computer program. We distinguish five stages in the development 
of a program. First is the problem description, which is a rough idea—usually 
written in English prose~of what we want to do. Second is the program specification, 
where we redescribe the problem in terms that are closer to a computable procedure. 
The third stage is the implementation of the program in a programming language 
such as Common Lisp, the fourth is testing, and the fifth is debugging and analysis. 
The boundaries between these stages are fluid, and the stages need not be completed 
in the order stated. Problems at any stage can lead to a change in the previous stage, 
or even to complete redesign or abandonment of the project. A programmer may 
prefer to complete only a partial description or specification, proceed directly to 
implementation and testing, and then return to complete the specification based on 
a better understanding. 

We follow all five stages in the development of our versions of GPS, with the hope 
that the reader will understand GPS better and will also come to understand better 
how to write a program of his or her own. To summarize, the five stages of an AI 
programming project are: 

1. Describe the problem in vague terms 
2. Specify the problem in algorithmic terms 
3. Implement the problem in a programming language 
4. Test the program on representative examples 
5. Debug and analyze the resulting program, and repeat the process 
<a id='page-111'></a>

4.1 Stage 1: Description 
As our problem description, we will start with a quote from Newell and Simon's 1972 
book. Human Problem Solving: 

The main methods of GPS jointly embody the heunstic ofmeans-ends analysis. 
Means-ends analysis is typified by the following kind of common-sense 
argument: 

I want to take my son to nursery school. What's the difference 
between what I have and what I want? One of distance. What 
changes distance? My automobile. My automobile won't work. 
What is needed to make it work? A new battery. What has new 
battenes? An auto repair shop. I want the repair shop to put in a 
new battery; but the shop doesn't know I need one. What is the 
difficulty? One of communication. What allows communication? 
Atelephone... and so on. 

The kind of analysis-classifying things in terms of the functions they serve and 
oscillating among ends, functions required, andmeans thatperform them-forms 
the basic system of heuristic of GPS. 

Of course, this kind of analysis is not exactly new. The theory of means-ends 
analysis was laid down quite elegantly by Aristotle 2300 years earlier in the chapter 
entitled "The nature of deliberation and its objects" of the Nicomachean Ethics (Book 
III. 3,1112b): 

We deliberate not about ends, but about means. For a doctor does not deliberate 
whether he shall heal, nor an orator whether he shall persuade, nor a statesman 
whether he shall produce law and order, nor does any one else deliberate about 
his end. They assume the end and consider how and by what means it is attained; 
and if it seems to be produced by several means they consider by which it is 
most easily and best produced, while if it is achieved by one only they consider 
how it will be achieved by this and by what means this will be achieved, till 
they come to the first cause, which in the order of discovery is last... and what 
is last in the order of analysis seems to be first in the order of becoming. And if 
we come on an impossibility, we give up the search, e.g., if we need money and 
this cannot be got; but if a thing appears possible we try to do it. 

Given this description of a theory of problem solving, how should we go about 
writing a program? First, we try to understand more fully the procedure outlined in 
the quotes. The main idea is to solve a problem using a process called means-ends 
analysis, where the problem is stated in terms of what we want to happen. In Newell 
and Simon's example, the problem is to get the kid to school, but in general we would 

<a id='page-112'></a>

like the program to be able to solve a broad class of problems. We can solve a problem 
if we can find some way to eliminate "the difference between what I have and what 
I want." For example, if what I have is a child at home, and what I want is a child 
at school, then driving may be a solution, because we know that driving leads to a 
change in location. We should be aware that using means-ends analysis is a choice: 
it is also possible to start from the current situation and search forward to the goal, 
or to employ a mixture of different search strategies. 

Some actions require the solving of preconditions as subproblems. Before we can 
drive the car, we need to solve the subproblem of getting the car in working condition. 
It may be that the car is already working, in which case we need do nothing to solve 
the subproblem. So a problem is solved either by taking appropriate action directly, 
or by first solving for the preconditions of an appropriate action and then taking 
the action. It is clear we will need some description of allowable actions, along 
with their preconditions and effects. We will also need to develop a definition of 
appropriateness. However, if we can define these notions better, it seems we won't 
need any new notions. Thus, we will arbitrarily decide that the problem description 
is complete, and move on to the problem specification. 

4.2 Stage 2: Specification 
At this point we have an idea—admittedly vague—of what it means to solve a problem 
in GPS. We can refine these notions into representations that are closer to Lisp as 
follows: 

* We can represent the current state of the world—"what I have"—or the goal 
state—"what I want"—as sets of conditions. Common Lisp doesn't have a data 
type for sets, but it does have Usts, which can be used to implement sets. Each 
condition can be represented by a symbol. Thus, a typical goal might be the list 
of two conditions (rich famous), and a typical current state might be (unknown 
poor). 
* We need a list of allowable operators. This list will be constant over the course 
of a problem, or even a series of problems, but we want to be able to change it 
and tackle a new problem domain. 
* An operator can be represented as a structure composed of an action, a list 
of preconditions, and a list of effects. We can place limits on the kinds of 
possible effects by saying that an effect either adds or deletes a condition from 
the current state. Thus, the list of effects can be split into an add-list and 
a delete-list. This was the approach taken by the STRIPS^ implementation of 
^STRIPS is the Stanford Research Institute Problem Solver, designed by Richard Pikes and 
NilsNilsson (1971). 

<a id='page-113'></a>

GPS, which we will be in effect reconstructing in this chapter. The original GPS 
allowed more flexibility in the specification of effects, but flexibility leads to 
inefficiency. 

* A complete problem is described to GPS in terms of a starting state, a goal state, 
and a set of known operators. Thus, GPS will be a function of three arguments. 
For example, a sample call might be: 
(GPS '(unknown poor) '(rich famous) list-of-ops) 

In other words, starting from the state of being poor and unknown, achieve the 
state of being rich and famous, using any combination of the known operators. 
GPS should return a true value only if it solves the problem, and it should print 
a record of the actions taken. The simplest approach is to go through the 
conditions in the goal state one at a time and try to achieve each one. If they 
can all be achieved, then the problem is solved. 

* A single goal condition can be achieved in two ways. If it is already in the 
current state, the goal is trivially achieved with no effort. Otherwise, we have 
to find some appropriate operator and try to apply it. 
* An operator is appropriate if one of the effects of the operator is to add the goal 
in question to the current state; in other words, if the goal is in the operator's 
add-list. 
* We can apply an operator if we can achieve all the preconditions. But this is 
easy, because we just defined the notion of achieving a goal in the previous 
paragraph. Once the preconditions have been achieved, applying an operator 
means executing the action and updating the current state in term of the operator's 
add-list and delete-list. Since our program is just a simulation—it won't 
be actually driving a car or dialing a telephone—we must be content simply to 
print out the action, rather than taking any real action. 
4.3 Stage 3: Implementation 
The specification is complete enough to lead directly to a complete Common Lisp 
program. Figure 4.1 summarizes the variables, data types, and functions that make 
up the GPS program, along with some of the Common Lisp functions used to implement 
it. 

<a id='page-114'></a>

Top-Level Function 
GPS Solve a goal from a state using a list of operators. 
Special Variables 
*state* The current state: a list of conditions. 
*ops* A list of available operators. 
Data Types 
op An operation with preconds, add-list and del-list. 
Functions 
achieve Achieve an individual goal. 
appropriate-p Decide if an operator is appropriate for a goal. 
apply-op Apply operator to current state. 
Selected Common Lisp Functions 
member Test if an element is a member of a list. (p. 78) 
set-difference All elements in one set but not the other. 
union All elements in either of two sets. 
every Test if every element of a list passes a test. (p. 62) 
some Test if any element of a list passes a test. 
Previously Defined Functions 
find-all A list of all matching elements, (p. 101) 
Figure 4.1: Glossary for the GPS Program 

Here is the complete GPS program itself: 

(defvar *state* nil "The current state: a list of conditions.") 

(defvar *ops* nil "A list of available operators.") 

(defstruct op "An operation" 
(action nil) (preconds nil) (add-list nil) (del-list nil)) 

(defun GPS (*state* goals *ops*) 
"General Problem Solver: achieve all goals using *ops*." 
(if (every #'achieve goals) 'solved)) 

(defun achieve (goal) 
"A goal is achieved if it already holds, 
or if there is an appropriate op for it that is applicable." 
(or (member goal *state*) 

(some #'apply-op 
(find-all goal *ops* :test #'appropriate-p)))) 

(defun appropriate-p (goal op) 
"An op is appropriate to a goal if it is in its add list." 
(member goal (op-add-list op))) 

<a id='page-115'></a>

(defun apply-op (op) 
"Print a message and update *state* if op is applicable." 
(when (every #*achieve (op-preconds op)) 

(print (list 'executing (op-action op))) 
(setf *state* (set-difference *state* (op-del-list op))) 
(setf *state* (union *state* (op-add-list op))) 
t)) 

We can see the program is made up of seven definitions. These correspond to the 
seven items in the specification above. In general, you shouldn't expect such a 
perfect fit between specification and implementation. There are two def var forms, 
one def s t r uct, and four defun forms. These are the Common Lisp forms for defining 
variables, structures, and functions, respectively. They are the most common top-
level forms in Lisp, but there is nothing magic about them; they are just special forms 
that have the side effect of adding new definitions to the Lisp environment. 

The two def var forms, repeated below, declare special variables named *state* 
and *ops*, which can then be accessed from anywhere in the program. 

(defvar *state* nil "The current state: a list of conditions.") 

(defvar *ops* nil "A list of available operators.") 

The defstruct form defines a structure called an op, which has slots called acti on, 
preconds, add -1 i st, and del -1 i st. Structures in Common Lisp are similar to structures 
in C, or records in Pascal. The defstruct automatically defines a constructor 
function, which is called make-op, and an access function for each slot of the structure. 
The access functions are called op-action, op-preconds, op-add-list, and 
op-del -1 ist. The defstruct also defines a copier function, copy-op, a predicate, 
op-p, and setf definitions for changing each slot. None of those are used in the GPS 
program. Roughly speaking, it is as if the defstruct form 

(defstruct op "An operation" 
(action nil) (preconds nil) (add-list nil) (del-list nil)) 

expanded into the following definitions: 

(defun make-op (&key action precondsadd-1ist del-1 ist) 
(vector 'op action preconds add-list del-list)) 

(defun op-action (op) (elt op 1)) 
(defun op-preconds (op) (elt op 2)) 
(defun op-add-list (op) (elt op 3)) 
(defun op-del-list (op) (elt op 4)) 

(defun copy-op (op) (copy-seq op)) 

<a id='page-116'></a>

(defun op-p (op) 
(and (vectorp op) (eq (elt op 0) Op))) 

(setf (documentation 'op 'structure) "An operation") 

Next in tlie GPS program are four function definitions. The main function, GPS, is 
passed three arguments. The first is the current state of the world, the second the 
goal state, and the third a list of allowable operators. The body of the function says 
simply that if we can achieve every one of the goals we have been given, then the 
problem is solved. The unstated alternative is that otherwise, the problem is not 
solved. 

The function a chi eve is given as an argument a single goal. The function succeeds 
if that goal is already true in the current state (in which case we don't have to do 
anything) or if we can apply an appropriate operator. This is accomplished by first 
building the list of appropriate operators and then testing each in turn until one can 
be applied, achieve calls find-al 1, which we defined on [page 101](chapter3.md#page-101). In this use, 
find-al 1 returns a list of operators that match the current goal, according to the 
predicate appropriate-p. 

The function appropriate-p tests if an operator is appropriate for achieving a 
goal. (It follows the Lisp naming convention that predicates end in - p.) 

Finally, the function apply-op says that if we can achieve all the preconditions 
for an appropriate operator, then we can apply the operator. This involves printing 
a message to that effect and changing the state of the world by deleting what was in 
the delete-list and adding what was in the add-Hst. apply-op is also a predicate; it 
returns t only when the operator can be applied. 

4.4 Stage 4: Test 
This section will define a list of operators applicable to the "driving to nursery school" 
domain and will show how to pose and solve some problems in that domain. First, 
we need to construct the list of operators for the domain. The defstruct form for the 
type op automatically defines the function make - op, which can be used as follows: 

(make-op :action 'drive-son-to-school 
ipreconds *(son-at-home car-works) 
:add-list '(son-at-school) 
:del-list '(son-at-home)) 

This expression returns an operator whose action is the symbol drive-son-to-school 
and whose preconditions, add-list and delete-list are the specified lists. The intent 

<a id='page-117'></a>

of this operator is that whenever the son is at home and the car works, dri ve-sonto-
school can be appHed, changing the state by deleting the fact that the son is at 
home, and adding the fact that he is at school. 

It should be noted that using long hyphenated atoms like son - at - home is a useful 
approach only for very simple examples like this one. A better representation would 
break the atom into its components: perhaps (at son home). The problem with 
the atom-based approach is one of combinatorics. If there are 10 predicates (such 
as at) and 10 people or objects, then there will be 10 . 10 . 10 = 1000 possible 
hyphenated atoms, but only 20 components. Clearly, it would be easier to describe 
the components. In this chapter we stick with the hyphenated atoms because it is 
simpler, and we do not need to describe the whole world. Subsequent chapters take 
knowledge representation more seriously. 

With this operator as a model, we can define other operators corresponding to 
Newell and Simon's quote on [page 109](chapter4.md#page-109). There will be an operator for installing a 
battery, telling the repair shop the problem, and telephoning the shop. We can fill in 
the "and so on" by adding operators for looking up the shop's phone number and for 
giving the shop money: 

(defparameter *school-ops* 
(list 

(make-op taction 'drive-son-to-school 
:preconds '(son-at-home car-works) 
:add-list '(son-at-school) 
:del-list '(son-at-home)) 

(make-op taction 'shop-installs-battery 
ipreconds '(car-needs-battery shop-knows-problem shop-has-money) 
:add-list '(car-works)) 

(make-op taction 'tell-shop-problem 
:preconds '(in-communication-with-shop) 
:add-list '(shop-knows-problem)) 

(make-op raction 'telephone-shop 
rpreconds '(know-phone-number) 
:add-list '(in-communication-with-shop)) 

(make-op .-action 'look-up-number 
ipreconds '(have-phone-book) 
:add-list '(know-phone-number)) 

(make-op taction 'give-shop-money 
ipreconds '(have-money) 
:add-list '(shop-has-money) 
:del-list '(have-money)))) 

The next step is to pose some problems to GPS and examine the solutions. Following 
are three sample problems. In each case, the goal is the same: to achieve the single 
condition son-at-school. The Hst of available operators is also the same in each 

<a id='page-118'></a>

problem; the difference is in the initial state. Each of the three examples consists of 
the prompt, ">", which is printed by the Lisp system, followed by a call to GPS, " (gps 
...which is typed by the user, then the output from the program, "(EXECUTING 
...)", and finally the result of the function call, which can be either SOLVED or NI L. 

> (gps '(son-at-home car-needs-battery have-money have-phone-book) 
'(son-at-school) 
*school-ops*) 

(EXECUTING LOOK-UP-NUMBER) 
(EXECUTING TELEPHONE-SHOP) 
(EXECUTING TELL-SHOP-PROBLEM) 
(EXECUTING GIVE-SHOP-MONEY) 
(EXECUTING SHOP-INSTALLS-BATTERY) 
(EXECUTING DRIVE-SON-TO-SCHOOL) 
SOLVED 

> (gps '(son-at-home car-needs-battery have-money) 
'(son-at-schoo1) 
*school-ops*) 

NIL 

> (gps '(son-at-home car-works) 
'(son-at-school) 
*school-ops*) 

(EXECUTING DRIVE-SON-TO-SCHOOL) 
SOLVED 

In all three examples the goal is to have the son at school. The only operator that 
has son-at-school in its add-list is drive-son-to-school, so GPS selects that operator 
initially. Before it can execute the operator, GPS has to solve for the preconditions. 
In the first example, the program ends up working backward through 
the operators shop-instal 1 s-battery, give-shop-money, tel 1 -shop-problem, and 
telephone-shop to look-up-number, whichhasnooutstandingpreconditions. Thus, 
the 1 ook-up-number action can be executed, and the program moves on to the other 
actions. As Aristotle said, "What is the last in the order of analysis seems to be first 
in the order of becoming." 

The second example starts out exactly the same, but the 1 ook - up-.umber operator 
fails because its precondition, have-phone-book, cannot be achieved. Knowing the 
phone number is a precondition, directly or indirectly, of all the operators, so no 
action is taken and GPS returns NIL. 

Finally, the third example is much more direct; the initial state specifies that the 
car works, so the driving operator can be applied immediately. 

<a id='page-119'></a>
4.5 Stage 5: Analysis, or ''We Lied about the C 
In the sections that follow, we examine the question of just how general this General 
Problem Solver is. The next four sections point out limitations of our version of GPS, 
and we will show how to correct these limitations in a second version of the program. 

One might ask if "limitations" is just a euphemism for "bugs." Are we "enhancing" 
the program, or are we "correcting" it? There are no clear answers on this point, 
because we never insisted on an unambiguous problem description or specification. 
AI programming is largely exploratory programming; the aim is often to discover 
more about the problem area rather than to meet a clearly defined specification. This 
is in contrast to a more traditional notion of programming, where the problem is 
completely specified before the first line of code is written. 

4.6 The Running Around the Block Problem 
Representing the operator "driving from home to school" is easy: the precondition 
and delete-list includes being at home, and the add-list includes being at school. But 
suppose we wanted to represent "running around the block." There would be no 
net change of location, so does that mean there would be no add- or delete-list? If 
so, there would be no reason ever to apply the operator. Perhaps the add-list should 
contain something like "got some exercise" or "feel tired," or something more general 
like "experience running around the block." We will return to this question later. 

4.7 The Clobbered Sibling Goal Problem 
Consider the problem of not only getting the child to school but also having some 
money left over to use for the rest of the day. GPS can easily solve this problem from 
the following initial condition: 

> (gps *(son-at-home have-money car-works) 
'(have-money son-at-school) 
*school-ops*) 

(EXECUTING DRIVE-SON-TO-SCHOOL) 
SOLVED 

However, in the next example GPS incorrectly reports success, when in fact it has 
spent the money on the battery. 

<a id='page-120'></a>

> (gps '(son-at-home car-needs-battery have-money have-phone-book) 
'(have-money son-at-school) 
^school-ops*) 

(EXECUTING LOOK-UP-NUMBER) 
(EXECUTING TELEPHONE-SHOP) 
(EXECUTING TELL-SHOP-PROBLEM) 
(EXECUTING GIVE-SHOP-MONEY) 
(EXECUTING SHOP-INSTALLS-BATTERY) 
(EXECUTING DRIVE-SON-TO-SCHOOL) 
SOLVED 

The "bug" is that GPS uses the expression (every #'achieve goals) to achieve 
a set of goals. If this expression returns true, it means that every one of the 
goals has been achieved in sequence, but it doesn't mean they are all still true 
at the end. In other words, the goal (have-money son-at-school), which we intended 
to mean "end up in a state where both have-money and son-at-school are 
true," was interpreted by GPS to mean "first achieve have-money, and then achieve 
son-at-school." Sometimes achieving one goal can undo another, previously 
achieved goal. We will call this the "prerequisite clobbers sibling goal" problem.^ 
That is, have-money and son-at-school are sibling goals, one of the prerequisites 
for the plan for son-at-school is car-works, and achieving that goal clobbers the 
have-money goal. 

Modifying the program to recognize the "prerequisite clobbers sibling goal" problem 
is straightforward. First note that we call (every #'achieve something) twice 
within the program, so let's replace those two forms with (achi eve - al 1 something). 
We can then define achi eve-al 1 as follows: 

(defun achieve-all (goals) 
"Try to achieve each goal, then make sure they still hold." 
(and (every #'achieve goals) (subsetp goals *state*))) 

The Common Lisp function subsetp returns true if its first argument is a subset of its 
second. In achi eve-al 1, it returns true if every one of the goals is still in the current 
state after achieving all the goals. This is just what we wanted to test. 

The introduction of achi eve-al 1 prevents GPS from returning true when one of 
the goals gets clobbered, but it doesn't force GPS to replan and try to recover from a 
clobbered goal. We won't consider that possibility now, but we will take it up again 
in the section on the blocks world domain, which was Sussman's primary example. 

^Gerald Sussman, in his book A Computer Model of Skill Acquisition, uses the term "prerequisite 
clobbers brother goal" or PCBG. I prefer to be gender neutral, even at the risk of being 
labeled a historical revisionist. 

<a id='page-121'></a>
4 . 8 The Leaping before You Look Problem 

Another way to address the "prerequisite clobbers sibling goal" problem is just to be 
more careful about the order of goals in a goal list. If we want to get the kid to school 
and still have some money left, why not just specify the goal as (son-at-school 
have-money) rather than (have-money son-at-school)? Let's see what happens 
when we try that: 

> (gps '(son-at-home car-needs-battery have-money have-phone-book) 
'(son-at-school have-money) 
*school-ops*) 

(EXECUTING LOOK-UP-NUMBER) 
(EXECUTING TELEPHONE-SHOP) 
(EXECUTING TELL-SHOP-PROBLEM) 
(EXECUTING GIVE-SHOP-MONEY) 
(EXECUTING SHOP-INSTALLS-BATTERY) 
(EXECUTING DRIVE-SON-TO-SCHOOL) 
NIL 

GPS returns nil, reflecting the fact that the goal cannot be achieved, but only after 
executing all actions up to and including driving to school. I call this the "leaping 
before you look" problem, because if you asked the program to solve for the two goals 
(j ump - of f -c 1 i f f 1 a nd -sa f e1 y) it would happily jump first, only to discover that it 
had no operator to land safely. This is less than prudent behavior. 

The problem arises because planning and execution are interleaved. Once the 
preconditions for an operator are achieved, the action is taken—and *sta te* is irrevocably 
changed—even if this action may eventually lead to a dead end. An alternative 
would be to replace the single global *state* with distinct local state variables, such 
that a new variable is created for each new state. This alternative is a good one for 
another, independent reason, as we shall see in the next section. 

4.9 The Recursive Subgoal Problem 
In our simulated nursery school world there is only one way to find out a phone 
number: to look it up in the phone book. Suppose we want to add an operator for 
finding out a phone number by asking someone. Of course, in order to ask someone 
something, you need to be in communication with him or her. The asking-for-a-
phone-number operator could be implemented as follows: 

<a id='page-122'></a>

(push (make-op :action 'ask-phone-number 
:preconds '(in-communication-with-shop) 
;add-list '(know-phone-number)) 

*school-ops*) 

(The special form (push item list) puts the item on the front of the list; it is equivalent 
to (setf list (cons item /fsO) in the simple case.) Unfortunately, something 
unexpected happens when we attempt to solve seemingly simple problems with this 
new set of operators. Consider the following: 

> (gps *(son-at-home car-needs-battery have-money) 
'(son-at-school) 
*school-ops*) 

>TRAP 14877 (SYSTEM:PDL-OVERFLOW EH::REGULAR) 
The regular push-down list has overflown. 
While in the function ACHIEVE <- EVERY <- REMOVE 

The error message (which will vary from one implementation of Common Lisp to 
another) means that too many recursively nested function calls were made. This 
indicates either a very complex problem or, more commonly, a bug in the program 
leading to infinite recursion. One way to try to see the cause of the bug is to trace a 
relevant function, such as achi eve: 

> (trace achieve) => (ACHIEVE) 

> (gps '(son-at-home car-needs-battery have-money) 
'(son-at-school) 
*school-ops*) 

(1 ENTER ACHIEVE: SON-AT-SCHOOL) 
(2 ENTER ACHIEVE: SON-AT-HOME) 
(2 EXIT ACHIEVE: (SON-AT-HOME CAR-NEEDS-BATTERY HAVE-MONEY)) 
(2 ENTER ACHIEVE: CAR-WORKS) 

(3 ENTER ACHIEVE: CAR-NEEDS-BATTERY) 
(3 EXIT ACHIEVE: (CAR-NEEDS-BATTERY HAVE-MONEY)) 
(3 ENTER ACHIEVE: SHOP-KNOWS-PROBLEM) 

(4 ENTER ACHIEVE: IN-COMMUNICATION-WITH-SHOP) 
(5 ENTER ACHIEVE: KNOW-PHONE-NUMBER) 
(6 ENTER ACHIEVE: IN-COMMUNICATION-WITH-SHOP) 
(7 ENTER ACHIEVE: KNOW-PHONE-NUMBER) 
(8 ENTER ACHIEVE: IN-COMMUNICATION-WITH-SHOP) 
(9 ENTER ACHIEVE: KNOW-PHONE-NUMBER) 

<a id='page-123'></a>
The output from trace gives us the necessary clues. Newell and Simon talk of 
"oscillating among ends, functions required, and means that perform them." Here 
it seems we have an infinite oscillation between being in communication with the 
shop (levels 4, 6, 8,...) and knowing the shop's phone number (levels 5, 7, 9,...). 
The reasoning is as follows: we want the shop to know about the problem with the 
battery, and this requires being in communication with him or her. One way to get in 
communication is to phone, but we don't have a phone book to look up the number. 
We could ask them their phone number, but this requires being in communication 
with them. As Aristotle put it, "If we are to be always deliberating, we shall have to 
go on to infinity." We will call this the "recursive subgoal" problem: trying to solve 
a problem in terms of itself. One way to avoid the problem is to have achi eve keep 
track of all the goals that are being worked on and give up if it sees a loop in the 
goal stack. 

4.10 The Lack of Intermediate Information 
Problem 
When GPS fails to find a solution, it just returns nil. This is annoying in cases where 
the user expected a solution to be found, because it gives no information about the 
cause of failure. The user could always trace some function, as we traced achi eve 
above, but the output from trace is rarely exactly the information desired. It would 
be nice to have a general debugging output tool where the programmer could insert 
print statements into his code and have them selectively printed, depending on the 
information desired. 

The function dbg provides this capability, dbg prints output in the same way as 
format, but it will only print when debugging output is desired. Each call to dbg is 
accompanied by an identifer that is used to specify a class of debugging messages. 
The functions debug and undebug are used to add or remove message classes to the 
list of classes that should be printed. In this chapter, all the debugging output will 
use the identifier :gps. Other programs will use other identifiers, and a complex 
program will use many identifiers. 

A call to dbg will result in output if the first argument to dbg, the identifier, is one 
that was specified in a call to debug. The other arguments to dbg are a format string 
followed by a list of arguments to be printed according to the format string. In other 
words, we will write functions that include calls to dbg like: 

(dbg :gps "The current goal is: ~a" goal) 

If we have turned on debugging with (debug :gps), then calls to dbg with the 
identifier :gps will print output. The output is turned off with (undebug :gps). 

<a id='page-124'></a>

debug and undebug are designed to be similar to trace and untrace, in that they turn 
diagnostic output on and off. They also follow the convention that debug with no 
arguments returns the current list of identifiers, and that undebug with no arguments 
turns all debugging off. However, they differ from trace and untrace in that they 
are functions, not macros. If you use only keywords and integers for identifiers, then 
you won't notice the difference. 

Two new built-in features are introduced here. First, *debug-io* is the stream 
normally used for debugging input/output. In all previous calls to format we have 
used t as the stream argument, which causes output to go to the *standa rd - output* 
stream. Sending different types of output to different streams allows the user some 
flexibility. For example, debugging output could be directed to a separate window, 
or it could be copied to a file. Second, the function fresh -1i ne advances to the next 
line of output, unless the output stream is already at the start of the line. 

(defvar *dbg-ids* nil "Identifiers used by dbg") 

(defun dbg (id format-string &rest args) 
"Print debugging info if (DEBUG ID) has been specified." 
(when (member id *dbg-ids*) 

(fresh-line *debug-io*) 
(apply #'format *debug-io* format-string args))) 

(defun debug (&rest ids) 
"Start dbg output on the given ids. " 
(setf *dbg-ids* (union ids *dbg-ids*))) 

(defun undebug (&rest ids) 
"Stop dbg on the ids. With no ids. stop dbg altogether." 
(setf *dbg-ids* (if (null ids) nil 

(set-difference *dbg-ids* ids)))) 

Sometimes it is easier to view debugging output if it is indented according to some 
pattern, such as the depth of nested calls to a function. To generate indented output, 
the function dbg -1 ndent is defined: 

(defun dbg-indent (id indent format-string &rest args) 
"Print indented debugging info if (DEBUG ID) has been specified." 
(when (member id *dbg-ids*) 

(fresh-line *debug-io*) 
(dotimes (i indent) (princ " " *debug-io*)) 
(apply #*format *debug-io* format-string args))) 

<a id='page-125'></a>
4.11 GPS Version 2: A More General 
Problem Solver 
At this point we are ready to put together a new version of GPS with solutions for 
the "running around the block," "prerequisite clobbers sibling goal," "leaping before 
you look," and "recursive subgoal" problems. The glossary for the new version is in 
figure 4.2. 

Top-Level Function 
GPS Solve a goal from a state using a list of operators. 
Special Variables 
*ops* A list of available operators. 
Data Types 
op An operation with preconds, add-list and del-Hst. 
Major Functions 
achieve-all Achieve a list of goals. 
achieve Achieve an individual goal. 
appropriate-p Decide if an operator is appropriate for a goal. 
apply-op Apply operator to current state. 
Auxiliary Functions 
executing-p Is a condition an executi ng form? 
starts-with Is the argument a list that starts with a given atom? 
convert-op Convert an operator to use the executi ng convention. 
op Create an operator. 
use Use a list of operators. 
member-equal Test if an element is equal to a member of a list. 
Selected Common Lisp Functions 
member Test if an element is a member of a list. (p. 78) 
set -difference All elements in one set but not the other. 
subsetp Is one set wholly contained in another? 
union All elements in either of two sets. 
every Test if every element of a list passes a test. (p. 62) 
some Test if any element of a list passes a test. 
remove-if Remove all items satisfying a test. 
Previously Defined Functions 
find-all A list of all matching elements, (p. 101) 
find-all-if A list of all elements satisfying a predicate. 
Figure 4.2: Glossary for Version 2 of GPS 

The most important change is that, instead of printing a message when each 
operator is applied, we will instead have GPS return the resulting state. A list of 

<a id='page-126'></a>

"messages" in each state indicates what actions have been taken. Each message is 
actuallyacondition,aHstof the form (executing operator). This solves the "running 
around the block" problem: we could call GPS with an initial goal of ((executing 
run -a round - bl ock)), and it would execute the run -a round - bl ock operator, thereby 
satisfying the goal. The following code defines a new function, op, which builds 
operators that include the message in their add-list. 

(defun executing-p (x) 
"Is X of the form: (executing ...) ? " 
(starts-with . 'executing)) 

(defun starts-with (list x) 
"Is this a list whose first element is x?" 
(and (consp list) (eql (first list) x))) 

(defun convert-op (op) 
"Make op conform to the (EXECUTING op) convention." 
(unless (some #'executing-p (op-add-list op)) 

(push (list 'executing (op-action op)) (op-add-list op))) 
op) 

(defun op (action &key preconds add-list del-list) 
"Make a new operator that obeys the (EXECUTING op) convention." 
(convert-op 

(make-op :action action :preconds preconds 
:add-list add-list :del-list del-list))) 

Operators built by op will be correct, but we can convert existing operators using 
convert-op directly: 

(mapc #'convert-op ^school-ops*) 

This is an example of exploratory programming: instead of starting all over when 
we discover a limitation of the first version, we can use Lisp to alter existing data 
structures for the new version of the program. 

The definition of the variable *ops* and the structure op are exactly the same as 
before, and the rest of the program consists of five functions we have already seen: 
GPS, achieve -all, achieve, appropriate-p, and apply-op. At the top level, the 
function GPS calls achieve-al 1, which returns either nil or a valid state. From this 
we remove all the atoms, which leaves only the elements of the final state that are 
lists—in other words, the actions of the form (executi ng operator). Thus, the value 
of GPS itself is the Hst of actions taken to arrive at the final state. GPS no longer returns 
SOLVED when it finds a solution, but it still obeys the convention of returning nil for 
failure, and non-nil for success. In general, it is a good idea to have a program return 

<a id='page-127'></a>
a meaningful value rather than print that value, if there is the possibility that some 
other program might ever want to use the value. 

(defvar *ops* nil "A list of available operators.") 

(defstruct op "An operation" 
(action nil) (preconds nil) (add-list nil) (del-list nil)) 

(defun GPS (state goals &optional (*ops* *ops*)) 
"General Problem Solver: from state, achieve goals using *ops*." 
(remove-if #'atom (achieve-all (cons '(start) state) goals nil))) 

The first major change in version 2 is evident from the first line of the program: there 
is no *state* variable. Instead, the program keeps track of local state variables. 
This is to solve the "leaping before you look" problem, as outlined before. The 
functions achieve, achieve-al 1, and apply-op all take an extra argument which is 
the current state, and all return a new state as their value. They also must still obey 
the convention of returning nil when they fail. 

Thus we have a potential ambiguity: does nil represent failure, or does it represent 
a valid state that happens to have no conditions? We resolve the ambiguity 
by adopting the convention that all states must have at least one condition. This 
convention is enforced by the function GPS. Instead of calling (achieve-al 1 state 
goals nil), GPS calls (achieve-all (cons '(start) state) goals nil). Soeven 
if the user passes GPS a null initial state, it will pass on a state containing (start ) 
to achieve-al 1. From then on, we are guaranteed that no state will ever become 
nil, because the only function that builds a new state is apply - op, and we can see by 
looking at the last line of appl y - op that it always appends something onto the state it 
is returning. (An add-list can never be nil, because if it were, the operator would not 
be appropriate. Besides, every operator includes the (executi ng ... ) condition.) 

Note that the final value we return from GPS has all the atoms removed, so we end 
up reporting only the actions performed, since they are represented by conditions 
of the form (executi ng action). Adding the (start ) condition at the beginning also 
serves to differentiate between a problem that cannot be solved and one that is solved 
without executing any actions. Failure returns nil, while a solution with no steps will 
at least include the (sta rt) condition, if nothing else. 

Functions that return nil as an indication of failure and return some useful value 
otherwise are known as semipredicates. They are error prone in just these cases 
where nil might be construed as a useful value. Be careful when defining and using 
semipredicates: (1) Decide if nil could ever be a meaningful value. (2) Insure that 
the user can't corrupt the program by supplying nil as a value. In this program, GPS 
is the only function the user should call, so once we have accounted for it, we're 
covered. (3) Insure that the program can't supply nil as a value. We did this by seeing 
that there was only one place in the program where new states were constructed, 
and that this new state was formed by appending a one-element list onto another 

<a id='page-128'></a>

state. By following this three-step procedure, we have an informal proof that the 
semipredicates involving states will function properly. This kind of informal proof 
procedure is a common element of good program design. 

The other big change in version 2 is the introduction of a goal stack to solve the 
recursive subgoal problem. The program keeps track of the goals it is working on 
and immediately fails if a goal appears as a subgoal of itself. This test is made in the 
second clause of achi eve. 

The function a chi eve -a 11 tries to achieve each one of the goals in turn, setting the 
variable state2 to be the value returned from each successive call to achi eve. If all 
goals are achieved in turn, and if all the goals still hold at the end (as subsetp checks 
for), then the final state is returned; otherwise the function fails, returning nil. 

Most of the work is done by achieve, which gets passed a state, a single goal 
condition, and the stack of goals worked on so far. If the condition is already in the 
state, then achieve succeeds and returns the state. On the other hand, if the goal 
condition is already in the goal stack, then there is no sense continuing—we will be 
stuck in an endless loop—so achi eve returns nil. Otherwise, achi eve looks through 
the list of operators, trying to find one appropriate to apply. 

(defun achieve-all (state goals goal-stack) 
"Achieve each goal, and make sure they still hold at the end." 
(let ((current-state state)) 

(if (and (every #'(lambda (g) 
(setf current-state 
(achieve current-state g goal-stack))) 
goals) 
(subsetp goals current-state rtest #'equal)) 
current-state))) 

(defun achieve (state goal goal-stack) 
"A goal is achieved if it already holds, 
or if there is an appropriate op for it that is applicable." 
(dbg-indent :gps (length goal-stack) "Goal: "a" goal) 
(cond ((member-equal goal state) state) 

((member-equal goal goal-stack) nil) 
(t (some #'(lambda (op) (apply-op state goal op goal-stack)) 
(find-all goal *ops* :test #*appropriate-p))))) 

The goal ((executing run-around-block)) is a list of one condition, where the 
condition happens to be a two-element list. Allowing lists as conditions gives us 
more flexibility, but we also have to be careful. The problem is that not all Usts that 
look alike actually are the same. The predicate equal essentially tests to see if its two 
arguments look alike, while the predicate eql tests to see if its two arguments actually 
are identical. Since functions like member use eql by default, we have to specify with 
a :test keyword that we want equal instead. Since this is done several times, we 

<a id='page-129'></a>
introduce the function member-equal. In fact, we could have carried the abstraction 
one step further and defined member-situation, a function to test if a condition is 
true in a situation. This would allow the user to change the matching function from 
eql to equal, and to anything else that might be useful. 

(defun member-equal (item list) 
(member item list :test #*equal)) 

The function apply-op, which used to change the state irrevocably and print a message 
reflecting this, now returns the new state instead of printing anything. It first 
computes the state that would result from achieving all the preconditions of the 
operator. If it is possible to arrive at such a state, then apply-op returns a new state 
derived from this state by adding what's in the add-list and removing everything in 
the delete-list. 

(defun apply-op (state goal op goal-stack) 
"Return a new, transformed state if op is applicable." 
(dbg-indent :gps (length goal-stack) "Consider: ~a" (op-action op)) 
(let ((state2 (achieve-all state (op-preconds op) 

(cons goal goal-stack)))) 

(unless (null state2) 
;; Return an updated state 
(dbg-indent :gps (length goal-stack) "Action: ~a" (op-action op)) 
(append (remove-if #*(lambda (x) 

(member-equal . (op-del-list op))) 
stateZ) 
(op-add-list op))))) 

(defun appropriate-p (goal op) 
"An op is appropriate to a goal if it is in its add-list." 
(member-equal goal (op-add-list op))) 

There is one last complication in the way we compute the new state. In version 
1 of GPS, states were (conceptually) unordered sets of conditions, so we could use 
uni on and set -di f f erence to operate on them. In version 2, states become ordered 
lists, because we need to preserve the ordering of actions. Thus, we have to use the 
functions append and remove-if, since these are defined to preserve order, while 
union and set -difference are not. 

Finally, the last difference in version 2 is that it introduces a new function: use. 
This function is intended to be used as a sort of declaration that a given list of operators 
is to be used for a series of problems. 

<a id='page-130'></a>

(defun use (oplist) 

"Use oplist as the default list of operators." 
Return something useful, but not too verbose: 
the number of operators, 

(length (setf *ops* oplist))) 

Calling use sets the parameter *ops*, so that it need not be specified on each call 
to GPS. Accordingly, in the definition of GPS itself the third argument, *ops*, is now 
optional; if it is not supplied, a default will be used. The default value for *ops* is 
given as *ops*. This may seem redundant or superfluous—how could a variable be 
its own default? The answer is that the two occurrences of *ops* look alike, but they 
actually refer to two completely separate bindings of the special variable *ops*. Most 
of the time, variables in parameter lists are local variables, but there is no rule against 
binding a special variable as a parameter. Remember that the effect of binding a 
special Vciriable is that all references to the special variable that occur anywhere in 
the program—even outside the lexical scope of the function—refer to the new binding 
of the special variable. So after a sequence of calls we eventually reach achieve, 
which references *ops*, and it will see the newly bound value of *ops*. 

The definition of GPS is repeated here, along with an alternate version that binds 
a local variable and explicitly sets and resets the special variable *ops*. Clearly, 
the idiom of binding a special variable is more concise, and while it can be initially 
confusing, it is useful once understood. 

(defun GPS (state goals &optional (*ops* *ops*)) 
"General Problem Solver: from state, achieve goals using *ops*." 
(remove-if #'atom (achieve-all (cons '(start) state) goals nil))) 

(defun GPS (state goals &optional (ops *ops*)) 
"General Problem Solver: from state, achieve goals using *ops*." 
(let ((old-ops *ops*)) 

(setf *ops* ops) 

(let ((result (remove-if #'atom (achieve-all 
(cons '(start) state) 
goalsnil)))) 

(setf *ops* old-ops) 
result))) 

Now let's see how version 2 performs. We use the list of operators that includes the 
"asking the shop their phone number" operator. First we make sure it will still do the 
examples version 1 did: 

> (use *school-ops*) 7 

<a id='page-131'></a>
> (gps '(son-at-home car-needs-battery have-money have-phone-book) 
'(son-at-school)) 

((START) 
(EXECUTING LOOK-UP-NUMBER) 
(EXECUTING TELEPHONE-SHOP) 
(EXECUTING TELL-SHOP-PROBLEM) 
(EXECUTING GIVE-SHOP-MONEY) 
(EXECUTING SHOP-INSTALLS-BATTERY) 
(EXECUTING DRIVE-SON-TO-SCHOOL)) 

> (debug :gps) => (:GPS) 

> (gps '(son-at-home car-needs-battery have-money have-phone-book) 

'(son-at-school)) 
Goal: SON-AT-SCHOOL 
Consider: DRIVE-SON-TO-SCHOOL 

Goal: SON-AT-HOME 
Goal: CAR-WORKS 
Consider: SHOP-INSTALLS-BATTERY 

Goal: CAR-NEEDS-BATTERY 
Goal: SHOP-KNOWS-PROBLEM 
Consider: TELL-SHOP-PROBLEM 

Goal: IN-COMMUNICATION-WITH-SHOP 

Consider: TELEPHONE-SHOP 
Goal: KNOW-PHONE-NUMBER 
Consider: ASK-PHONE-NUMBER 

Goal: IN-COMMUNICATION-WITH-SHOP 
Consider: LOOK-UP-NUMBER 
Goal: HAVE-PHONE-BOOK 
Action: LOOK-UP-NUMBER 

Action: TELEPHONE-SHOP 
Action: TELL-SHOP-PROBLEM 
Goal: SHOP-HAS-MONEY 
Consider: GIVE-SHOP-MONEY 

Goal: HAVE-MONEY 
Action: GIVE-SHOP-MONEY 

Action: SHOP-INSTALLS-BATTERY 
Action: DRIVE-SON-TO-SCHOOL 
((START) 

(EXECUTING LOOK-UP-NUMBER) 
(EXECUTING TELEPHONE-SHOP) 
(EXECUTING TELL-SHOP-PROBLEM) 
(EXECUTING GIVE-SHOP-MONEY) 
(EXECUTING SHOP-INSTALLS-BATTERY) 
(EXECUTING DRIVE-SON-TO-SCHOOL)) 

> (undebug) NIL 

<a id='page-132'></a>

> (gps *(son-at-home car-works) 
'(son-at-school)) 
((START) 
(EXECUTING DRIVE-SON-TO-SCHOOL)) 

Now we see that version 2 can also handle the three cases that version 1 got wrong. 
In each case, the program avoids an infinite loop, and also avoids leaping before 
it looks. 

> (gps '(son-at-home car-needs-battery have-money have-phone-book) 
'(have-money son-at-school)) 
NIL 

> (gps '(son-at-home car-needs-battery have-money have-phone-book) 
'(son-at-school have-money)) 
NIL 

> (gps '(son-at-home car-needs-battery have-money) 
'(son-at-school)) 
NIL 

Finally, we see that this version of GPS also works on trivial problems requiring no 
action: 

> (gps '(son-at-home) '(son-at-home)) => ((START)) 

4.12 The New Domain Problem: Monkey 
and Bananas 
To show that GPS is at all general, we have to make it work in different domains. We 
will start with a "classic" AI problem.^ Imagine the following scenario: a hungry 
monkey is standing at the doorway to a room. In the middle of the room is a bunch 
of bananas suspended from the ceiling by a rope, well out of the monkey's reach. 
There is a chair near the door, which is light enough for the monkey to push and tall 
enough to reach almost to the bananas. Just to make things complicated, assume the 
monkey is holding a toy ball and can only hold one thing at a time. 

In trying to represent this scenario, we have some flexibility in choosing what to 
put in the current state and what to put in with the operators. For now, assume we 
define the operators as follows: 

^Originally posed by Saul Amarel (1968). 

<a id='page-133'></a>
(defparameter *banana-ops* 
(list 

(op 'climb-on-chair 
ipreconds '(chair-at-middle-room at-middle-room on-floor) 
:add-list '(at-bananas on-chair) 
:del-list '(at-middle-room on-floor)) 

(op 'push-chair-from-door-to-middle-room 
:preconds '(chair-at-door at-door) 
:add-list '(chair-at-middle-room at-middle-room) 
:del-list '(chair-at-door at-door)) 

(op 'walk-from-door-to-middle-room 
ipreconds '(at-door on-floor) 
;add-list '(at-middle-room) 
:del-list '(at-door)) 

(op 'grasp-bananas 
rpreconds '(at-bananas empty-handed) 
:add-list '(has-bananas) 
:del-list '(empty-handed)) 

(op 'drop-ball 
ipreconds '(has-ball) 
ladd-list '(empty-handed) 
idel-list '(has-balD) 

(op 'eat-bananas 
ipreconds '(has-bananas) 
ladd-list '(empty-handed not-hungry) 
idel-list '(has-bananas hungry)))) 

Using these operators, we could pose the problem of becoming not-hungry, given 
the initial state of being at the door, standing on the floor, holding the ball, hungry, 
and with the chair at the door. GPS can find a solution to this problem: 

> (use *banana-ops*) => 6 

> (GPS '(at-door on-floor has-ball hungry chair-at-door) 
'(not-hungry)) 

((START) 
(EXECUTING PUSH-CHAIR-FROM-DOOR-TO-MIDDLE-ROOM) 
(EXECUTING CLIMB-ON-CHAIR) 
(EXECUTING DROP-BALL) 
(EXECUTING GRASP-BANANAS) 
(EXECUTING EAT-BANANAS)) 

Notice we did not need to make any changes at all to the GPS program. We just used 
a different set of operators. 

<a id='page-134'></a>

4.13 The Maze Searching Domain 
Now we will consider another "classic" problem, maze searching. We will assume a 
particular maze, diagrammed here. 

1 2 3 4 5 
6 7 8 9 10 
11 12 13 14 15 
16 17 18 19 20 
21 22 23 24 25 

It is much easier to define some functions to help build the operators for this 
domain than it would be to type in all the operators directly. The following code 
defines a set of operators for mazes in general, and for this maze in particular: 

(defun make-maze-ops (pair) 
"Make maze ops in both directions" 
(list (make-maze-op (first pair) (second pair)) 

(make-maze-op (second pair) (first pair)))) 

(defun make-maze-op (here there) 
"Make an operator to move between two places" 
(op '(move from ,here to .there) 

ipreconds '((at .here)) 
:add-list '((at .there)) 
:del-list '((at .here)))) 

(defparameter *maze-ops* 
(mappend #'make-maze-ops 

'((1 2) (2 3) (3 4) (4 9) (9 14) (9 8) (8 7) (7 12) (12 13) 
(12 11) (11 6) (11 16) (16 17) (17 22) (21 22) (22 23) 
(23 18) (23 24) (24 19) (19 20) (20 15) (15 10) (10 5) (20 25)))) 

Note the backquote notation, (It is covered in section 3.2, [page 67](chapter3.md#page-67). 
We can now use this list of operators to solve several problems with this maze. 
And we could easily create another maze by giving another list of connections. Note 
that there is nothing that says the places in the maze are arranged in a five-by-five 
layout—that is just one way of visualizing the connectivity. 

> (use *maze-ops*) 48 

<a id='page-135'></a>
> (gps '((at D) '((at 25))) 

((START) 
(EXECUTING (MOVE FROM 1 TO 2)) 
(EXECUTING (MOVE FROM 2 TO 3)) 
(EXECUTING (MOVE FROM 3 TO 4)) 
(EXECUTING (MOVE FROM 4 TO 9)) 
(EXECUTING (MOVE FROM 9 TO 8)) 
(EXECUTING (MOVE FROM 8 TO 7)) 
(EXECUTING (MOVE FROM 7 TO 12)) 
(EXECUTING (MOVE FROM 12 TO ID ) 
(EXECUTING (MOVE FROM 11 TO 16)) 
(EXECUTING (MOVE FROM 16 TO 17)) 
(EXECUTING (MOVE FROM 17 TO 22)) 
(EXECUTING (MOVE FROM 22 TO 23)) 
(EXECUTING (MOVE FROM 23 TO 24)) 
(EXECUTING (MOVE FROM 24 TO 19)) 
(EXECUTING (MOVE FROM 19 TO 20)) 
(EXECUTING (MOVE FROM 20 TO 25)) 
(AT 25)) 

There is one subtle bug that the maze domain points out. We wanted GPS to return 
a list of the actions executed. However, in order to account for the case where the 
goal can be achieved with no action, I included (START) in the value returned by 
GPS. These examples include the START and EXECUTING forms but also a list of the 
form (AT n), for some n. This is the bug. If we go back and look at the function 
GPS, we find that it reports the result by removing all atoms from the state returned 
by achieve-al 1 . This is a "pun"—we said remove atoms, when we really meant 
to remove all conditions except the (START) and (EXECUTING action) forms. Up to 
now, all these conditions were atoms, so this approach worked. The maze domain 
introduced conditions of the form (AT n), so for the first time there was a problem. 
The moral is that when a programmer uses puns—saying what's convenient instead 
of what's really happening—there's bound to be trouble. What we really want to do 
is not to remove atoms but to find all elements that denote actions. The code below 
says what we mean: 

(defun GPS (state goals &optional (*ops* *ops*)) 
"General Problem Solver: from state, achieve goals using *ops*." 
(find-all-if #*action-p 

(achieve-all (cons '(start) state) goals nil))) 

<a id='page-136'></a>

(defun action-p (x) 
"Is . something that is (start) or (executing ...)? " 
(or (equal . '(start)) (executing-p x))) 

The domain of maze solving also points out an advantage of version 2: that it returns 
a representation of the actions taken rather than just printing them out. The reason 
this is an advantage is that we may want to use the results for something, rather than 
just look at them. Suppose we wanted a function that gives us a path through a maze 
as a list of locations to visit in turn. We could do this by calling GPS as a subfunction 
and then manipulating the results: 

(defun find-path (start end) 
"Search a maze for a path from start to end." 
(let ((results (GPS '((at .start)) '((at .end))))) 

(unless (null results) 
(cons start (mapcar #'destination 
(remove '(start) results 
:test #'equal)))))) 

(defun destination (action) 
"Find the Y in (executing (move from X to Y))" 
(fifth (second action))) 

The function f i nd - path calls GPS to get the resul ts. If this is ni 1, there is no answer, 
but if it is not, then take the rest of results (in other words, ignore the (START) part). 
Pick out the destination,!/, from each (EXECUTING (MOVE FROM . TO y)) form, and 
remember to include the starting point. 

> (use *maze-ops*) => 48 

> (find-path 1 25) ^ 
(1 2 3 4 9 8 7 12 11 16 17 22 23 24 19 20 25) 

> (find-path 1 1) (1) 

> (equal (find-path 1 25) (reverse (find-path 25 1))) => . 

4.14 The Blocks World Domain 

Another domain that has attracted more than its share of attention in AI circles is 
the blocks world domain. Imagine a child's set of building blocks on a table top. 
The problem is to move the blocks from their starting configuration into some goal 
configuration. We will assume that each block can have only one other block directly 

<a id='page-137'></a>
on top of it, although they can be stacked to arbitrary height. The only action that 
can be taken in this world is to move a single block that has nothing on top of it either 
to the top of another block or onto the table that represents the block world. We will 
create an operator for each possible block move. 

(defun make-block-ops (blocks) 
(let ((ops nil)) 
(dolist (a blocks) 
(dolist (b blocks) 
(unless (equal a b) 
(dolist (c blocks) 
(unless (or (equal c a) (equal c b)) 

(push (move-op abc) ops))) 
(push (move-op a 'table b) ops) 
(push (move-op a b 'table) ops)))) 

ops)) 

(defun move-op (a b c) 
"Make an operator to move A from . to C. " 
(op '(move .a from .b to ,c) 

ipreconds '((space on ,a) (space on ,c) (,a on .b)) 
ladd-list (move-ons abc) 
idel-list (move-ons a c b))) 

(defun move-ons (a b c) 

(if (eq b 'table) 
*((,a on ,c)) 
*((.a on ,c) (space on ,b)))) 

Now we try these operators out on some problems. The simplest possible problem 
is stacking one block on another: 

. 
start goal 

> (use (make-block-ops '(a b))) => 4 

> (gps '((a on table) (b on table) (space on a) (space on b) 
(space on table)) 
'((a on b) (b on table))) 
((START) 
(EXECUTING (MOVE A FROM TABLE TO B))) 

<a id='page-138'></a>

Here is a slightly more complex problem: inverting a stack of two blocks. This time 
we show the debugging output. 

start goa 

> (debug :gps) (:GPS) 

> (gps *((a on b) (b on table) (space on a) (space on table)) 

'((b on a))) 
Goal: (B ON A) 
Consider: (MOVE . FROM TABLE TO A) 

Goal: (SPACE ON B) 

Consider: (MOVE A FROM . TO TABLE) 
Goal: (SPACE ON A) 
Goal: (SPACE ON TABLE) 
Goal: (A ON B) 

Action: (MOVE A FROM . TO TABLE) 
Goal: (SPACE ON A) 
Goal: (B ON TABLE) 

Action: (MOVE . FROM TABLE TO A) 

((START) 
(EXECUTING (MOVE A FROM . TO TABLE)) 
(EXECUTING (MOVE . FROM TABLE TO A))) 

> (undebug) NIL 

Sometimes it matters what order you try the conjuncts in. For example, you can't 
have your cake and eat it too, but you can take a picture of your cake and eat it too, as 
long as you take the picture before eating it. In the blocks world, we have: 

A 
_B_ 
C 

start

> (use (make-block-ops '(a b c))) 18 

> (gps '((a on b) (b on c) (c on table)
'((b on a) (c on b))) 

((START) 
(EXECUTING (MOVE A FROM . TO TABLE)) 
(EXECUTING (MOVE . FROM C TO A)) 
(EXECUTING (MOVE C FROM TABLE TO B))) 

C 

_B_ 

A 

 goal 

 (space on a) (space on table)) 

<a id='page-139'></a>
> (gps '((a on b) (b on c) (c on table) (space on a) (space on table)) 
'((c on b) (b on a))) 
NIL 

In the first case, the tower was built by putting . on A first, and then C on B. In 
the second case, the program gets C on . first, but clobbers that goal while getting . 
on A. The "prerequisite clobbers sibling goal" situation is recognized, but the program 
doesn't do anything about it. One thing we could do is try to vary the order of the 
conjunct goals. That is, we could change achieve-al 1 as follows: 

(defun achieve-all (state goals goal-stack) 
"Achieve each goal, trying several orderings." 
(some #'(lambda (goals) (achieve-each state goals goal-stack)) 

(orderings goals))) 

(defun achieve-each (state goals goal-stack) 
"Achieve each goal, and make sure they still hold at the end." 
(let ((current-state state)) 

(if (and (every #'(lambda (g) 
(setf current-state 
(achieve current-state g goal-stack))) 
goals) 
(subsetp goals current-state :test #*equal)) 
current-state))) 

(defun orderings (1) 

(if (> (length 1) 1) 
(1 ist 1 (reverse 1)) 
(list 1))) 

Now we can represent the goal either way, and we'll still get an answer. Notice that 
we only consider two orderings: the order given and the reversed order. Obviously, 
for goal sets of one or two conjuncts this is all the orderings. In general, if there 
is only one interaction per goal set, then one of these two orders will work. Thus, 
we are assuming that "prerequisite clobbers sibling goal" interactions are rare, and 
that there will seldom be more than one interaction per goal set. Another possibility 
would be to consider all possible permutations of the goals, but that could take a long 
time with large goal sets. 

Another consideration is the efficiency of solutions. Consider the simple task of 
getting block C on the table in the following diagram: 

A] \B] [A] [B] 
start goal 
<a id='page-140'></a>

> (gps '((c on a) (a on table) (b on table) 
(space on c) (space on b) (space on table)) 
'((c on table))) 

((START) 
(EXECUTING (MOVE C FROM A TO B)) 
(EXECUTING (MOVE C FROM . TO TABLE))) 

The solution is correct, but there is an easier solution that moves C directly to the 
table. The simpler solution was not found because of an accident: it happens that 
make-bl ock-ops defines the operators so that moving C from . to the table comes 
before moving C from A to the table. So the first operator is tried, and it succeeds 
provided C is on B. Thus, the two-step solution is found before the one-step solution is 
ever considered. The following example takes four steps when it could be done in two: 

. 
start goal 

> (gps '((c on a) (a on table) (b on table) 
(space on c) (space on b) (space on table)) 
'((c on table) (a on b))) 

((START) 
(EXECUTING (MOVE C FROM A TO B)) 
(EXECUTING (MOVE C FROM . TO TABLE)) 
(EXECUTING (MOVE A FROM TABLE TO O ) 
(EXECUTING (MOVE A FROM C TO B))) 

How could we find shorter solutions? One way would be to do a full-fledged search: 
shorter solutions are tried first, temporarily abandoned when something else looks 
more promising, and then reconsidered later on. This approach is taken up in 
chapter 6, using a general searching function. A less drastic solution is to do a limited 
rearrangement of the order in which operators are searched: the ones with fewer 
unfulfilled preconditions are tried first. In particular, this means that operators with 
all preconditions filled would always be tried before other operators. To implement 
this approach, we change achi eve: 

(defun achieve (state goal goal-stack) 
"A goal is achieved if it already holds, 
or if there is an appropriate op for it that is applicable." 
(dbg-indent :gps (length goal-stack) "Goal:~a" goal) 
(cond ((member-equal goal state) state) 

((member-equal goal goal-stack) nil) 

<a id='page-141'></a>

(t (some #'(lambda (op) (apply-op state goal op goal-stack)) 
(appropriate-ops goal state))))) 

(defun appropriate-ops (goal state) 
"Return a list of appropriate operators, 
sorted by the number of unfulfilled preconditions." 
(sort (copy-list (find-all goal *ops* :test #'appropriate-p)) #'< 

:key #*(lambda (op) 
(count-if #'(lambda (precond) 
(not (member-equal precond state))) 
(op-preconds op))))) 

Now we get the solutions we wanted: 

start goal 

> (gps '((c on a) (a on table) (b on table) 
(space on c) (space on b) (space on table)) 
'((c on table) (a on b))) 

((START) 
(EXECUTING (MOVE C FROM A TO TABLE)) 
(EXECUTING (MOVE A FROM TABLE TO B))) 

start goal 

> (gps '((a on b) (b on c) (c on table) (space on a) (space on table)) 
'((b on a) (c on b))) 

((START) 
(EXECUTING (MOVE A FROM . TO TABLE)) 
(EXECUTING (MOVE . FROM C TO A)) 
(EXECUTING (MOVE C FROM TABLE TO B))) 

> (gps '((a on b) (b on c) (c on table) (space on a) (space on table)) 
'((c on b) (b on a))) 

((START) 
(EXECUTING (MOVE A FROM . TO TABLE)) 
(EXECUTING (MOVE . FROM C TO A)) 
(EXECUTING (MOVE C FROM TABLE TO B))) 

<a id='page-142'></a>

The Sussman Anomaly 

Surprisingly, there are problems that can't be solved by any reordering of goals. 
Consider: 

. A 
Start goal 

This doesn't look too hard, so let's see how our GPS handles it: 

> (setf start '((c on a) (a on table) (b on table) (space on c) 
(space on b) (space on table))) 
((C ON A) (A ON TABLE) (B ON TABLE) (SPACE ON C) 
(SPACE ON B) (SPACE ON TABLE)) 

> (gps start '((a on b) (b on c))) NIL 

> (gps start *((b on c) (a on b))) => NIL 

There is a "prerequisite clobbers sibling goal" problem regardless of which way we 
order the conjuncts! In other words, no combination of plans for the two individual 
goals can solve the conjunction of the two goals. This is a surprising fact, and the 
example has come to be known as "the Sussman anomaly."^ We will return to this 
problem in chapter 6. 

4.15 Stage 5 Repeated: Analysis of Version 2 
We have shown that GPS is extensible to multiple domains. The main point is that 
we didn't need to change the program itself to get the new domains to work; we 
just changed the list of operators passed to GPS. Experience in different domains 
did suggest changes that could be made, and we showed how to incorporate a few 
changes. Although version 2 is a big improvement over version 1, it still leaves much 
to be desired. Now we will discover a few of the most troubling problems. 

^ A footnote in Waldinger 1977 says, 'This problem was proposed by Allen Brown. Perhaps 
many children thought of it earlier but did not recognize that it was hard." The problem is 
named after Gerald Sussman because he popularized it in Sussman 1973. 

<a id='page-143'></a>
4.16 The Not Looking after You Don^t 
Leap Problem 
We solved the "leaping before you look" problem by introducing variables to hold a 
representation of possible future states, rather than just a single variable representing 
the current state. This prevents GPS from taking an ill-advised action, but we shall 
see that even with all the repair strategies introduced in the last section, it doesn't 
guarantee that a solution will be found whenever one is possible. 

To see the problem, add another operator to the front of the ^school - ops* Hst 
and turn the debugging output back on: 

(use(push (op 'taxi-son-to-school 
:preconds *(son-at-home have-money) 
:add-list '(son-at-school) 
:del-list '(son-at-home have-money)) 

*school-ops*)) 

(debug :gps) 

Now, consider the problem of getting the child to school without using any money: 

> (gps '(son-at-home have-money car-works) 

'(son-at-school have-money)) 
Goal: SON-AT-SCHOOL 
Consider: TAXI-SON-TO-SCHOOL 

Goal: SON-AT-HOME 

Goal: HAVE-MONEY 
Action: TAXI-SON-TO-SCHOOL 
Goal: HAVE-MONEY 
Goal: HAVE-MONEY 
Goal: SON-AT-SCHOOL 
Consider: TAXI-SON-TO-SCHOOL 

Goal: SON-AT-HOME 

Goal: HAVE-MONEY 
Action: TAXI-SON-TO-SCHOOL 
NIL 

The first five lines of output succesfully solve the son-at-school goal with the 
TAX I - SON - TO- SCHOO L action. The next line shows an unsuccesf ul attempt to solve the 
have - money goal. The next step is to try the other ordering. This time, the have - money 
goal is tried first, and succeeds. Then, the son-at-school goal is achieved again by 
the TAX I - SON - TO- SCHOO L action. But the check for consistency in achi eve-each fails, 
and there are no repairs available. The goal fails, even though there is a valid solution: 
driving to school. 

<a id='page-144'></a>

The problem is that achi eve uses some to look at the appropri ate-ops. Thus, if 
there is some appropriate operator, achi eve succeeds. If there is only one goal, this 
will yield a correct solution. However, if there are multiple goals, as in this case, 
achi eve will still only find one way to fulfill the first goal. If the first solution is a bad 
one, the only recourse is to try to repair it. In domains like the block world and maze 
world, repair often works, because all steps are reversible. But in the taxi example, no 
amount of plan repair can get the money back once it is spent, so the whole plan fails. 

There are two ways around this problem. The first approach is to examine all 
possible solutions, not just the first solution that achieves each subgoal. The language 
Prolog, to be discussed in chapter 11, does just that. The second approach is to have 
achi eve and achi eve-al 1 keep track of a list of goals that must be protected. In the 
taxi example, we would trivially achieve the have-money goal and then try to achieve 
son-at-school, while protecting the goal have-money. An operator would only 
be appropriate if it didn't delete any protected goals. This approach still requires 
some kind of repair or search through multiple solution paths. If we tried only 
one ordering-achieving son - at - school and then trying to protect it while achieving 
have - money—then we would not find the solution. David Warren's WARPLAN planner 
makes good use of the idea of protected goals. 

4.17 The Lack of Descriptive Power Problem 
It would be a lot more economical, in the maze domain, to have one operator that 
says we can move from here to there if we are at "here," and if there is a connection 
from "here" to "there." Then the input to a particular problem could list the valid 
connections, and we could solve any maze with this single operator. Similarly, we 
have defined an operator where the monkey pushes the chair from the door to the 
middle of the room, but it would be better to have an operator where the monkey 
can push the chair from wherever it is to any other nearby location, or better yet, an 
operator to push any "pushable" object from one location to a nearby one, as long 
as there is no intervening obstacle. The conclusion is that we would like to have 
variables in the operators, so we could say something like: 

(op '(push X from A to B) 

:preconds '((monkey at A) (X at A) (pushable X) (path A B)) 

:add-list '((monkey at B) (X at B)) 

:del-list '((monkey at A) (X at A))) 

Often we want to characterize a state in terms of something more abstract than a 
list of conditions. For example, in solving a chess problem, the goal is to have the 
opponent in checkmate, a situation that cannot be economically described in terms 
of primitives like (bl ack ki ng on A 4), so we need to be able to state some kind 

<a id='page-145'></a>
of constraint on the goal state, rather than just listing its components. We might 
want to be able to achieve a disjunction or negation of conditions, where the current 
formalism allows only a conjunction. 

It also is important, in many domains, to be able to state problems dealing with 
time: we want to achieve X before time To, and then achieve Y before time T2, but 
not before Ti. Scheduling work on a factory floor or building a house are examples 
of planning where time plays an important role. 

Often there are costs associated with actions, and we want to find a solution 
with minimal, or near-minimal costs. The cost might be as simple as the number of 
operators required for a solution—we saw in the blocks world domain that sometimes 
an operator that could be applied immediately was ignored, and an operator that 
needed several preconditions satisfied was chosen instead. Or we may be satisfied 
with a partial solution, if a complete solution is impossible or too expensive. We may 
also want to take the cost (and time) of computation into account. 

4.18 The Perfect Information Problem 
All the operators we have seen so far have unambiguous results; they add or delete 
certain things from the current state, and GPS always knows exactly what they are 
going to do. In the real world, things are rarely so cut and dried. Going back to the 
problem of becoming rich, one relevant operator would be playing the lottery. This 
operator has the effect of consuming a few dollars, and once in a while paying off a 
large sum. But we have no way to represent a payoff "once in a while." Similarly, 
we have no way to represent unexpected difficulties of any kind. In the nursery 
school problem, we could represent the problem with the car battery by having GPS 
explicitly check to see if the car was working, or if it needed a battery, every time 
the program considered the driving operator. In the real world, we are seldom this 
careful; we get in the car, and only when it doesn't start do we consider the possibility 
of a dead battery. 

4.19 The Interacting Goals Problem 
People tend to have multiple goals, rather than working on one at a time. Not only do 
I want to get the kid to nursery school, but I want to avoid getting hit by another car, 
get to my job on time, get my work done, meet my friends, have some fun, continue 
breathing, and so on. I also have to discover goals on my own, rather than work on 
a set of predefined goals passed to me by someone else. Some goals I can keep in 
the background for years, and then work on them when the opportunity presents 
itself. There is never a notion of satisfying all possible goals. Rather, there is a 

<a id='page-146'></a>

continual process of achieving some goals, partially achieving others, and deferring 
or abandoning still others. 

In addition to having active goals, people also are aware of undesirable situations 
that they are trying to avoid. For example, suppose I have a goal of visiting a friend 
in the hospital. This requires being at the hospital. One appHcable operator might 
be to walk to the hospital, while another would be to severly injure myself and wait 
for the ambulance to take me there. The second operator achieves the goal just as 
well (perhaps faster), but it has an undesirable side effect. This could be addressed 
either with a notion of solution cost, as outlined in the last section, or with a list of 
background goals that every solution attempts to protect. 

Herb Simon coined the term "satisficing" to describe the strategy of satisfying a 
reasonable number of goals to a reasonable degree, while abandoning or postponing 
other goals. GPS only knows success and failure, and thus has no way of maximizing 
partial success. 

4.20 The End of GPS 
These last four sections give a hint as to the scope of the limitations of GPS. In fact, it 
is not a very general problem solver at all. Itis general in the sense that the algorithm 
is not tied to a particular domain; we can change domain by changing the operators. 
But GPS fails to be general in that it can't solve many interesting problems. It is 
confined to small tricks and games. 

There is an important yet subtle reason why GPS was destined to fail, a reason 
that was not widely appreciated in 1957 but now is at the core of computer science. 
It is now recognized that there are problems that computers can't solve—not because 
a theoretically correct program can't be written, but because the execution of the 
program will take too long. A large number of problems can be shown to fall into 
the class of "NP-hard" problems. Computing a solution to these problems takes 
time that grows exponentially as the size of the problem grows. This is a property 
of the problems themselves, and holds no matter how clever the programmer is. 
Exponential growth means that problems that can be solved in seconds for, say, a 
five-input case may take trillions of years when there are 100 inputs. Buying a faster 
computer won't help much. After all, if a problem would take a trillion years to solve 
on your computer, it won't help much to buy 1000 computers each 1000 times faster 
than the one you have: you're still left with a million years wait. For a theoretical 
computer scientist, discovering that a problem is NP-hard is an end in itself. But for 
an AI worker, it means that the wrong question is being asked. Many problems are 
NP-hard when we insist on the optimal solution but are much easier when we accept 
a solution that might not be the best. 

The input to GPS is essentially a program, and the execution of GPS is the execution 
of that program. If GPS's input language is general enough to express any program. 

<a id='page-147'></a>
then there will be problems that can't be solved, either because they take too long 
to execute or because they have no solution. Modern problem-solving programs 
recognize this fundamental limitation, and either limit the class of problems they try 
to solve or consider ways of finding approximate or partial solutions. Some problem 
solvers also monitor their own execution time and know enough to give up when a 
problem is too hard. 

The following quote from Drew McDermott's article "Artificial Intelligence Meets 
Natural Stupidity" sums up the current feeling about GPS. Keep it in mind the next 
time you have to name a program. 

Remember GPS? By now, "GPS" is a colorless term denoting a particularly stupid 
program to solve puzzles. But it originally meant ''General Problem Solver," 
which caused everybody a lot of needless excitement and distraction. It should 
have been called LFGNS-"Loca/ Feature-Guided Network Searcher." 

Nonetheless, GPS has been a useful vehicle for exploring programming in general, 
and AI programming in particular. More importantly, it has been a useful vehicle 
for exploring "the nature of deliberation." Surely we'll admit that Aristotle was 
a smarter person than you or me, yet with the aid of the computational model of 
mind as a guiding metaphor, and the further aid of a working computer program 
to help explore the metaphor, we have been led to a more thorough appreciation of 
means-ends analysis—at least within the computational model. We must resist the 
temptation to believe that all thinking follows this model. 

The appeal of AI can be seen as a split between means and ends. The end of a 
successful AI project can be a program that accomplishes some useful task better, 
faster, or cheaper than it could be before. By that measure, GPS is a mostly a failure, 
as it doesn't solve many problems particularly well. But the means toward that end 
involved an investigation and formalization of the problem-solving process. By that 
measure, our reconstruction of GPS is a success to the degree in which it leads the 
reader to a better understanding of the issues. 

4.21 History and References 
The original GPS is documented in Newell and Simon's 1963 paper and in their 1972 
book. Human Problem Solving, as well as in Ernst and Newell 1969. The implementation 
in this chapter is based on the STRIPS program (Fikes and Nilsson 1971). 

There are other important planning programs. Earl Sacerdoti's ABSTRIPS program 
was a modification of STRIPS that allowed for hierarchical planning. The idea was to 
sketch out a skeletal plan that solved the entire program at an abstract level, and then 
fill in the details. David Warren's WARPLAN planner is covered in Warren 1974a,b 
and in a section of Coelho and Cotta 1988. Austin Tate's NONLIN system (Tate 1977) 

<a id='page-148'></a>

achieved greater efficiency by considering a plan as a partially ordered sequence of 
operations rather than as a strictly ordered sequence of situations. David Chapman's 
TWEAK synthesizes and formalizes the state of the art in planning as of 1987. 
All of these papers-an d quite a few other important planning papers-ar e 
reprinted in Allen, Hendler, and Tate 1990. 
4.22 Exercises 
&#9635; Exercise 4.1 [m] It is possible to implement dbg using a single call to format. Can 
you figure out the format directives to do this? 

&#9635; Exercise 4.2 [m] Write a function that generates all permutations of its input. 

&#9635; Exercise 4.3 [h] GPS does not recognize the situation where a goal is accidentally 
solved as part of achieving another goal. Consider the goal of eating dessert. Assume 
that there are two operators available: eating ice cream (which requires having the 
ice cream) and eating cake (which requires having the cake). Assume that we can 
buy a cake, and that the bakery has a deal where it gives out free ice cream to each 
customer who purchases and eats a cake. (1) Design a list of operators to represent 
this situation. (2) Give gps the goal of eating dessert. Show that, with the right list 
of operators, gps will decide to eat ice cream, then decide to buy and eat the cake in 
order to get the free ice cream, and then go ahead and eat the ice cream, even though 
the goal of eating dessert has already been achieved by eating the cake. (3) Fix gps so 
that it does not manifest this problem. 
The following exercises address the problems in version 2 of the program. 

&#9635; Exercise 4.4 [h] The Not Looking after You Don't Leap Problem. Write a program that 
keeps track of the remaining goals so that it does not get stuck considering only one 
possible operation when others will eventually lead to the goal. Hint: have achi eve 
take an extra argument indicating the goals that remain to be achieved after the 
current goal is achieved, achi eve should succeed only if it can achieve the current 
goal and also achi eve-all the remaining goals. 

&#9635; Exercise 4.5 [d] Write a planning program that, like Warren's WARPLAN, keeps 
track of the list of goals that remain to be done as well as the list of goals that have 
been achieved and should not be undone. The program should never undo a goal 
that has been achieved, but it should allow for the possibility of reordering steps that 

<a id='page-149'></a>

have already been taken. In this way, the program will solve the Sussman anomaly 
and similar problems. 

&#9635; Exercise 4.6 [d] The Lack of Descriptive Power Problem. Read chapters 5 and 6 to learn 
about pattern matching. Write a version of GPS that uses the pattern matching tools, 
and thus allows variables in the operators. Apply it to the maze and blocks world 
domains. Your program will be more efficient if, like Chapman's TWEAK program, 
you allow for the possibility of variables that remain unbound as long as possible. 

&#9635; Exercise 4.7 [d] Speculate on the design of a planner that can address the Perfect 
Information and Interacting Goals problems. 

4.23 Answers 
Answer 4.1 In this version, the format string " ~&~V@T~?" breaks down as follows: 

means go to a fresh line; "~V@T" means insert spaces (@T) but use the next 
argument (V) to get the number of spaces. The " ~?" is the indirection operator: use 
the next argument as a format string, and the argument following that as the list of 
arguments for the format string. 

(defun dbg-indent (id indent format-string &rest args) 
"Print indented debugging info if (DEBUG ID) has been specified." 
(when (member id *dbg-ids*) 

(format *debug-io* "~&~v@T~?" (* 2 indent) format-string args))) 

<a id='page-150'></a>

Answer 4.2 Here is one solution. The sophisticated Lisp programmer should also 
see the exercise on [page 680](chapter19.md#page-680). 

(defun permutations (bag) 

"Return a list of all the permutations of the input." 
If the input is nil, there is only one permutation: 
nil itself 

(if (null bag) 

'(()) 
Otherwise, take an element, e, out of the bag. 
Generate all permutations of the remaining elements. 
And add e to the front of each of these. 
Do this for all possible e to generate all permutations, 

(mapcan #'(lambda (e) 
(mapcar #*(lambda (p) (cons e p)) 
(permutations 
(remove e bag :count 1 :test #'eq)))) 
bag))) 

