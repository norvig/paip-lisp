# Chapter 13 {docsify-ignore}
<a id='page-434'></a>

Object-Oriented 
Programming 

r I 1 he programs in this book cover a wide range of problems. It is only natural that a 

I wide range of programming styles have been introduced to attack these problems. One 

JL style not yet covered that has gained popularity in recent years is called object-oriented 
programming. To understand what object-oriented programming entails, we need to place it in 
the context of other styles. 

Historically, the first computer programs were written in an imperative programming style. A 
program was construed as a series of instructions, where each instruction performs some action: 
changing the value of a memory location, printing a result, and so forth. Assembly language is 
an example of an imperative language. 

As experience (and ambition) grew, programmers looked for ways of controlling the complexity 
of programs. The invention of subroutines marked the algorithmic or procedural programming 
style, a subclass of the imperative style. Subroutines are helpful for two reasons: breaking 
up the problem into small pieces makes each piece easier to understand, and it also makes it 
possible to reuse pieces. Examples of procedural languages are FORTRAN, C, Pascal, and Lisp 
with setf. 

<a id='page-435'></a>

Subroutines are still dependent on global state, so they are not completely separate 
pieces. The use of a large number of global variables has been criticized as a 
factor that makes it difficult to develop and maintain large programs. To eliminate 
this problem, the functional programming style insists that functions access only the 
parameters that are passed to them, and always return the same result for the same 
inputs. Functional programs have the advantage of being mathematically clean—it 
is easy to prove properties about them. However, some applications are more naturally 
seen as taking action rather than calculating functional values, and are therefore 
unnatural to program in a functional style. Examples of functional languages are FP 
and Lisp without setf. 

In contrast to imperative languages are declarative languages, which attempt to 
express "what to do" rather than "how to do it." One type of declarative programming 
is rule-based programming, where a set of rules states how to transform a problem 
into a solution. Examples of rule-based systems are ELIZA and STUDENT. 

An important kind of declarative programming is logic programming, where axioms 
are used to describe constraints, and computation is done by a constructive proof of 
a goal. An example of logic language is Prolog. 

Object-oriented programming is another way to tame the problem of global state. 
Instead of prohibiting global state (as functional programming does), object-oriented 
programming breaks up the unruly mass of global state and encapsulates it into small, 
manageable pieces, or objects. This chapter covers the object-oriented approach. 

13,1 Object-Oriented Programming 

Object-oriented programming turns the world of computing on its side: instead 
of viewing a program primarily as a set of actions which manipulate objects, it is 
viewed as a set of objects that are manipulated by actions. The state of each object 
and the actions that manipulate that state are defined once and for all when the 
object is created. This can lead to modular, robust systems that are easy to use and 
extend. It also can make systems correspond more closely to the "real world," which 
we humans perceive more easily as being made up of objects rather than actions. 
Examples of object-oriented languages are Simula, C++, and CLOS, the Common 
Lisp Object System. This chapter will first introduce object-oriented programming 
in general, and then concentrate on the Common Lisp Object System. 

Many people are promoting object-oriented programming as the solution to the 
software development problem, but it is hard to get people to agree on just what 
object-orientation means. Peter Wegner 1987 proposes the following formula as a 
definition: 

Object-orientation = Objects + Classes + Inheritance 

<a id='page-436'></a>

Briefly, objects are modules that encapsulate some data and operations on that data. 
The idea of information /z/dm^—insulating the representation of that data from operations 
outside of the object—is an important part of this concept. Classes are groups 
of similar objects with identical behavior. Objects are said to be instances of classes. 
Inheritance is a means of defining new classes as variants of existing classes. The new 
class inherits the behavior of the parent class, and the programmer need only specify 
how the new class is different. 

The object-oriented style brings with it a new vocabulary, which is summarized in 
the following glossary. Each term will be explained in more detail when it comes up. 

class: A group of similar objects with identical behavior. 
class variable: A variable shared by all members of a class. 
delegation: Passing a message from an object to one of its components. 
generic function: A function that accepts different types or classes of 

arguments. 
inheritance: A means of defining new classes as variants of existing 

classes. 
instance: An instance of a class is an object. 
instance variable: A variable encapsulated within an object. 
message: A name for an action. Equivalent to generic function. 
method: A means of handling a message for a particular class. 
multimethod: A method that depends on more than one argument. 
multiple inheritance: Inheritance from more than one parent class. 
object: An encapsulation of local state and behavior. 

13.2 Objects 
Object-oriented programming, by definition, is concerned with objects. Any datum 
that can be stored in computer memory can be thought of as an object. Thus, the 
number 3, the atom x, and the string "hel 1 o" are all objects. Usually, however, the 
term object is used to denote a more complex object, as we shall see. 

Of course, all programming is concerned with objects, and with procedures 
operating on those objects. Writing a program to solve a particular problem will 
necessarily involve writing definitions for both objects and procedures. What distinguishes 
object-oriented programming is that the primary way of decomposing the 
problem into modules is based on the objects rather than on the procedures. The 
difference can best be seen with an example. Here is a simple program to create bank 
accounts and keep track of withdrawals, deposits, and accumulation of interest. 
First, the program is written in traditional procedural style: 

(defstruct account 
(name "") (balance 0.00) (interest-rate .06)) 

<a id='page-437'></a>

(defun account-withdraw (account amt) 
"Make a withdrawal from this account." 
(if (<= amt (account-balance account)) 

(decf (account-balance account) amt) 

'insufficient-funds)) 

(defun account-deposit (account amt) 
"Make a deposit to this account." 
(incf (account-balance account) amt)) 

(defun account-interest (account) 
"Accumulate interest in this account." 
(incf (account-balance account) 

(* (account-interest-rate account) 
(account-balance account)))) 

We can create new bank accounts with make-a ccount and modify them with 
account-wi thdraw, account-deposi t, and account-i nterest. This is a simple problem, 
and this simple solution suffices. Problems appear when we change the specification 
of the problem, or when we envision ways that this implementation could 
be inadvertently used in error. For example, suppose a programmer looks at the 
account structure and decides to use (decf (account-balance account)) directly 
instead of going through the account-wi thdraw function. This could lead to negative 
account balances, which were not intended. Or suppose that we want to create a 
new kind of account, where only a certain maximum amount can be withdrawn at 
one time. There would be no way to ensure that account-withdraw would not be 
applied to this new, limited account. 

The problem is that once we have created an account, we have no control over 

what actions are applied to it. The object-oriented style is designed to provide that 

control. Here is the same program written in object-oriented style (using plain Lisp): 

(defun new-account (name &optional (balance 0.00) 

(interest-rate .06)) 
"Create a new account that knows the following messages:" 
#'(lambda (message) 

(case message 
(withdraw #*(lambda (amt) 
(if (<= amt balance) 
(decf balance amt) 

'insufficient-funds))) 
(deposit #'(lambda (amt) (incf balance amt))) 
(balance #'(lambda () balance)) 
(name #'(lambda () name)) 
(interest #*(lambda () 
(incf balance 
(* interest-rate balance))))))) 

<a id='page-438'></a>

The function new-account creates account objects, which are implemented as closures 
that encapsulate three variables: the name, balance, and interest rate of the 
account. An account object also encapsulates functions to handle the five messages 
to which the object can respond. An account object can do only one thing: receivea 
message and return the appropriate function to execute that message. For example, 
if you pass the message wi thdraw to an account object, it will return a function that, 
when applied to a single argument (the amount to withdraw), will perform the withdrawal 
action. This function is called the method that implements the message. The 
advantage of this approach is that account objects are completely encapsulated; the 
information corresponding to the name, balance, and interest rate is only accessible 
through the five messages. We have a guarantee that no other code can manipulate 
the information in the account in any other way.^ 

The function get - method finds the method that implements a message for a given 
object. The function send gets the method and applies it to a list of arguments. The 
name send comes from the Flavors object-oriented system, which is discussed in the 
history section ([page 456](chapter13.md#page-456)). 

(defun get-method (object message) 
"Return the method that implements message for this object." 
(funcall object message)) 

(defun send (object message &rest args) 
"Get the function to implement the message, 
and apply the function to the args." 
(apply (get-method object message) args)) 

Here is an example of the use of new- account and send: 

> (setf acct (new-account "J. Random Customer" 1000.00)) => 
#<CLOSURE 23652465> 

> (send acct 'withdraw 500.00) 500.0 

> (send acct 'deposit 123.45) => 623.45 

> (send acct 'name) ^ "J. Random Customer" 

> (send acct 'balance) => 623.45 

^More accurately, we have a guarantee that there is no way to get at the inside of a closure 
using portable Common Lisp code. Particular implementations may provide debugging tools 
for getting at this hidden information, such as i .spect. So closures are not perfect at hiding 
information from these tools. Of course, no information-hiding method will be guaranteed 
against such covert channels—even with the most sophisticated software security measures, 
it is always possible to, say, wipe a magnet over the computer's disks and alter sensitive data. 

<a id='page-439'></a>
13.3 Generic Functions 
The send syntax is awkward, as it is different from the normal Lisp function-calling 
syntax, and it doesn't fit in with the other Lisp tools. For example, we might like to 
say (ma pea . ' ba 1 anee accounts), but with messages we would have to write that as: 

(mapcar #*(lambda (acct) (send acct 'balance)) accounts) 

We can fix this problem by deiining generic functions that find the right method to 
execute a message. For example, we could define: 

(defun withdraw (object &rest args) 
"Define withdraw as a generic function on objects." 
(apply (get-method object 'withdraw) args)) 

and then write (withdraw acct .) instead of (send acct 'withdraw x). The 
function wi thdraw is generic because it not only works on account objects but also 
works on any other class of object that handles the wi thdraw message. For example, 
we might have a totally unrelated class, army, which also implements a withdraw 
method. Then we could say (send 5th-army 'withdraw) or (withdraw 5th-army) 
and have the correct method executed. So object-oriented programming eliminates 
many problems with name clashes that arise in conventional programs. 

Many of the built-in Common Lisp functions can be considered generic functions, 
in that they operate on different types of data. For example, sqrt does one thing 
when passed an integer and quite another when passed an imaginary number. The 
sequence functions (like findordelete) operate on lists, vectors, or strings. These 
functions are not implemented like wi thd raw, but they still act like generic functions.^ 

13.4 Classes 
It is possible to write macros to make the object-oriented style easier to read and 
write. The macro def i ne - cl ass defines a class with its associated message-handling 
methods. It also defines a generic function for each message. Finally, it allows the 
programmer to make a distinction between variables that are associated with each 
object and those that are associated with a class and are shared by all members of the 
class. For example, you might want to have all instances of the class account share 
the same interest rate, but you wouldn't want them to share the same balance. 

^There is a technical sense of "generic function" that is used within CLOS. These functions 
are not generic according to this technical sense. 

<a id='page-440'></a>

(defmacro define-class (class inst-vars class-vars &body methods) 
"Define a class for object-oriented programming." 
Define constructor and generic functions for methods 

'(let ,class-vars 
(mapcar #'ensure-generic-fn \(mapcar #'first methods)) 
(defun .class ,inst-vars 

#*(lambda (message) 
(case message 
,(mapcar #'make-clause methods)))))) 

(defun make-clause (clause) 
"Translate a message from define-class into a case clause." 
'(.(first clause) #'(lambda .(second clause) ..(rest2 clause)))) 

(defun ensure-generic-fn (message) 
"Define an object-oriented dispatch function for a message, 
unless it has already been defined as one." 
(unless (generic-fn-p message) 

(let ((fn #'(lambda (object &rest args) 

(apply (get-method object message) args)))) 
(setf (symbol-function message) fn) 
(setf (get message *generic-fn) fn)))) 

(defun generic-fn-p (fn-name) 
"Is this a generic function?" 
(and (fboundp fn-name) 

(eq (get fn-name 'generic-fn) (symbol-function fn-name)))) 

Now we define the class account with this macro. We make i nterest- rate a class 
variable, one that is shared by all accounts: 

(define-class account (name &optional (balance 0.00)) 
((interest-rate .06)) 

(withdraw (amt) (if (<= amt balance) 
(decf balance amt) 
'insufficient-funds)) 

(deposit (amt) (incf balance amt)) 
(balance () balance) 
(name () name) 

(interest () (incf balance (* interest-rate balance)))) 

Here we use the generic functions defined by this macro: 

> (setf acct2 (account "A. User" 2000.00)) #<CL0SURE 24003064> 

> (deposit acct2 42.00) => 2042.0 

> (interest acct2) 2164.52 

<a id='page-441'></a>

> (balance acct2) ^ 2164.52 

> (balance acct) => 623.45 

In this last line, the generic function bal anee is applied to acct, an object that was 
created before we even defined the account class and the function balance. But 
bal anee still works properly on this object, because it obeys the message-passing 
protocol. 

13.5 Delegation 
Suppose we want to create a new kind of account, one that requires a password for 
each action. We can define a new class, password-account, that has two message 
clauses. The first clause allows for changing the password (if you have the original 
password), and the second is an otherwi se clause, which checks the password given 
and, if it is correct, passes the rest of the arguments on to the account that is being 
protected by the password. 

The definition of password-account takes advantage of the internal details of 
define-class in two ways: it makes use of the fact that otherwise can be used 
as a catch-all clause in a case form, and it makes use of the fact that the dispatch 
variable is called message. Usually, it is not a good idea to rely on details about the 
implementation of a macro, and soon we will see cleaner ways of defining classes. 
But for now, this simple approach works: 

(define-class password-account (password acct) () 
(change-password (pass new-pass) 

(if (equal pass password) 
(setf password new-pass) 
'wrong-password)) 

(otherwise (pass &rest args) 

(if (equal pass password) 
(apply message acct args) 
'wrong-password))) 

Now we see how the class password-account can be used to provide protection for 
an existing account: 

(setf acct3 (password-account "secret" acct2)) => #<CLOSURE 33427277> 
> (balance acct3 "secret") => 2164.52 
> (withdraw acct3 "guess" 2000.00) => WRONG-PASSWORD 
> (withdraw acct3 "secret" 2000.00) 164.52 

Now let's try one more example. Suppose we want to have a new class of account 

<a id='page-442'></a>

where only a limited amount of money can be withdrawn at any time. We could 
define the class 1 i mi ted - account: 

(define-class limited-account (limit acct) () 
(withdraw (amt) 

(if (> amt limit) 
'over-limit 
(withdraw acct amt))) 

(otherwise (&rest args) 
(apply message acct args))) 

This definition redefines the wi t hd raw message to check if the limit is exceeded before 
passing on the message, and it uses the otherwi se clause simply to pass on all other 
messages unchanged. In the following example, we set up an account with both a 
password and a limit: 

> (setf acct4 (password-account "pass" 
(limited-account 100.00 
(account "A. Thrifty Spender" 500.00)))) 
#<CLOSURE 34136775> 

> (withdraw acct4 "pass" 200.00) ^ OVER-LIMIT 

> (withdraw acct4 "pass" 20.00) => 480.0 

> (withdraw acct4 "guess" 20.00) => WRONG-PASSWORD 

Note that functions like wi thdraw are still simple generic functions that just find the 
right method and apply it to the arguments. The trick is that each class defines a different 
way to handle the withdraw message. Calling wi thdraw with acct4 as argument 
results in the following flow of control. First, the method in the password-account 
class checks that the password is correct. If it is, it calls the method from the 
1i mi ted-account class. If the limit is not exceeded, we finally call the method from 
the account class, which decrements the balance. Passing control to the method of 
a component is called delegation. 

The advantage of the object-oriented style is that we can introduce a new class 
by writing one definition that is localized and does not require changing any existing 
code. If we had written this in traditional procedural style, we would end up with 
functions like the following: 

(defun withdraw (acct amt &optional pass) 
(cond ((and (typep acct 'password-account) 
(not (equal pass (account-password acct)))) 
'wrong-password) 
((and (typep acct 'limited-account) 

<a id='page-443'></a>
(> amt (account-limit account))) 
'over-limit) 
((> amt balance) 
'insufficient-funds) 
(t (decf balance amt)))) 

There is nothing wrong with this, as an individual function. The problem is that 
when the bank decides to offer a new kind of account, we will have to change this 
function, along with all the other functions that implement actions. The "definition" 
of the new account is scattered rather than localized, and altering a bunch of existing 
functions is usually more error prone than writing a new class definition. 

13.6 Inheritance 
In the following table, data types (classes) are listed across the horizontal axis, and 
functions (messages) are listed up and down the vertical axis. A complete program 
needs to fill in all the boxes, but the question is how to organize the process of filling 
them in. In the traditional procedural style, we write function definitions that fill in 
a row at a time. In the object-oriented style, we write class definitions that fill in a 
column at a time. A third style, the data-dnven or generic style, fills in only one box at 
a time. 

account limited-password-
account account 
name object 
deposit oriented 
withdraw function oriented 
balance 
interest generic 

In this table there is no particular organization to either axis; both messages and 
classes are listed in random order. This ignores the fact that classes are organized hierarchically: 
both Hmited-account and password-account are subclasses of account. 
This was implicit in the definition of the classes, because both 1 i mi ted - account and 
password-account contain accounts as components and delegate messages to those 
components. But it would be cleaner to make this relationship explicit. 

The defstruct mechanism does allow for just this kind of explicit inheritance. If 
we had defined account as a structure, then we could define 1 i mi ted - account with: 

<a id='page-444'></a>

(defstruct (limited-account (:include account)) limit) 

Two things are needed to provide an inheritance facility for classes. First, we should 
modify define-class so that it takes the name of the class to inherit from as the 
second argument. This will signal that the new class will inherit all the instance 
variables, class variables, and methods from the parent class. The new class can, of 
course, define new variables and methods, or it can shadow the parent's variables and 
methods. In the form below, we define 1 i ml ted - account to be a subclass of account 
that adds a new instance variable, 11 mi t, and redefines the wi thdraw method so that 
it checks for amounts that are over the limit. If the amount is acceptable, then it uses 
the function cal 1 -next-method (not yet defined) to get at the withdraw method for 
the parent class, account. 

(define-class limited-account account (limit) () 
(withdraw (amt) 

(if (> amt limit) 
Over-limit 
(call-next-method)))) 

If inheritance is a good thing, then multiple inheritance is an even better thing. For 
example, assuming we have defined the classes 1 i mi ted - account and 
password - account, it is very convenient to define the following class, which inherits 
from both of them: 

(define-class limited-account-with-password 
(password-account 1 i mi ted-account)) 

Notice that this new class adds no new variables or methods. All it does is combine 

the functionality of two parent classes into one. 

&#9635; Exercise 13.1 [d] Define a version of def i ne-cl ass that handles inheritance and 
call-next-method. 

&#9635; Exercise 13.2 [d] Define a version of def i ne-cl ass that handles multiple inheritance. 


<a id='page-445'></a>
13.7 GLOS: The Common Lisp Object System 
So far, we have developed an object-oriented programming system using a macro, 
define-class, and a protocol for implementing objects as closures. There have 
been many proposals for adding object-oriented features to Lisp, some similar to 
our approach, some quite different. Recently, one approach has been approved to 
become an official part of Common Lisp, so we will abandon our ad hoc approach 
and devote the rest of this chapter to CLOS, the Common Lisp Object System. The 
correspondence between our system and CLOS is summarized here: 

our system CLOS 
define-class defclass 
methods defined in class defmethod 
class-name make-instance 
call-next-method call-next-method 
ensure-generic-fn ensure-generic-function 

Like most object-oriented systems, CLOS is primarily concerned with defining 
classes and methods for them, and in creating instances of the classes. In CLOS the 
macro def class defines a class, defmethod defines a method, and make-instance 
creates an instance of a class—an object. The general form of the macro def cl ass is: 

(def cl ass class-name (superclass...) (slot-specifier...) optional-class-option...) 

The class-options are rarely used, def cl ass can be used to define the class account: 

(defclass account () 

((name :initarg -.name :reader name) 

(balance linitarg rbalance linitform 0.00 raccessor balance) 

(interest-rate :allocation :class :initform .06 

:reader interest-rate))) 

In the definition of account, we see that the Ust of superclasses is empty, because 
account does not inherit from any classes. There are three slot specifiers, for the 
name, bal anee, and i nterest - rate slots. Each slot name can be followed by optional 
keyword/value pairs defining how the slot is used. The name slot has an : i ni targ 
option, which says that the name can be specified when a new account is created 
with make-instance. The :reader slot creates a method called name to get at the 
current value of the slot. 

The balance slot has three options: another :initarg, saying that the balance 
can be specified when a new account is made; an rinitform, which says that if 
the balance is not specified, it defaults to 0.00, and an raccessor, which creates a 

<a id='page-446'></a>

method for getting at the slot's value just as : reader does, and also creates a method 
for updating the slot with setf . 

The i nterest- rate slot has an : i ni tf orm option to give it a defauh value and an 
rail ocati on option to say that this slot is part of the class, not of each instance of the 
class. 

Here we see the creation of an object, and the application of the automatically 
defined methods to it. 

> (setf al (make-instance 'account chalanee 5000.00 

:name "Fred")) #<ACCOUNT 26726272> 

> (name al) ^ "Fred" 

> (balance al) 5000.0 

> (interest-rate al) ^ 0.06 

CLOS differs from most object-oriented systems in that methods are defined separately 
from classes. To define a method (besides the ones defined automatically by 
: reader, :writer, or :accessor options) we use the defmethod macro. It is similar 
to defun in form: 

(defmethod method-name (parameter..:) body...) 

Required parameters to a defmethod can be of the form (var class), meaning that 
this is a method that applies only to arguments of that class. Here is the method for 
withdrawing from an account. Note that CLOS does not have a notion of instance 
variable, only instance slot. So we have to use the method (bal ance acct) rather 
than the instance variable bal anee: 

(defmethod withdraw ((acct account) amt) 

(if (< amt (balance acct)) 
(decf (balance acct) amt) 
'i nsuffi ci ent-funds)) 

With CLOS it is easy to define a 1 imited-account as a subclass of account, and to 
define the wi thd raw method for 11 mi ted - accounts: 

(defclass limited-account (account) 
((limit :initarg ilimit -.reader limit))) 

(defmethod withdraw ((acct limited-account) amt) 

(if (> amt (limit acct)) 
Over-limit 
(call-next-method))) 

<a id='page-447'></a>
Note the use of cal1 -next-method to invoke the withdraw method for the account 
class. Also note that all the other methods for accounts automatically work on 
instances of the class limited-account, because it is defined to inherit from account. In 
the following example, we show that the name method is inherited, that the wi thdraw 
method for 1 i mi ted-account is invoked first, and that the withdraw method for 
account is invoked by the cal1 -next-method function: 

> (setf a2 (make-instance 'limited-account 
:name "A. Thrifty Spender" 
:balance 500.00 :limit 100.00)) ^ 

#<LIMITED-ACCOUNT 24155343> 

> (name a2) ^ "A. Thrifty Spender" 

> (withdraw a2 200.00) ^ OVER-LIMIT 

> (withdraw a2 20.00) 480.0 

In general, there may be several methods appropriate to a given message. In that case, 
all the appropriate methods are gathered together and sorted, most specific first. The 
most specific method is then called. That is why the method for 1 i mi ted - account is 
called first rather than the method for account. The function cal 1 -next-method can 
be used within the body of a method to call the next most specific method. 

The complete story is actually even more complicated than this. As one example 
of the complication, consider the class audi ted-a ccount, which prints and keeps 
a trail of all deposits and withdrawals. It could be defined as follows using a new 
feature of CLOS, : before and : after methods: 

(defclass audited-account (account) 
((audit-trail :initform nil :accessor audit-trail))) 

(defmethod withdraw ibefore ((acct audited-account) amt) 
(push (print '(withdrawing .amt)) 
(audit-trail acct))) 

(defmethod withdraw rafter ((acct audited-account) amt) 
(push (print '(withdrawal (.amt) done)) 
(audit-trail acct))) 

Now a call to withdraw with a audited-account as the first argument yields three 

applicable methods: the primary method from account and the : before and rafter 

methods. In general, there might be several of each kind of method. In that case, 

all the : before methods are called in order, most specific first. Then the most 

specific primary method is called. It may choose to invoke cal 1 - next-method to 

get at the other methods. (It is an error for a : before or : after method to use 

cal 1 -next-method.) Finally, all the rafter methods are called, least specific first. 

<a id='page-448'></a>

The values from the : before and : after methods are ignored, and the value from 
the primary method is returned. Here is an example: 

> (setf a3 (make-instance 'audited-account .-balance 1000.00)) 
#<AUDITED-ACCOUNT 33555607> 

> (withdraw a3 100.00) 

(WITHDRAWING 100.0) 

(WITHDRAWAL (100.0) DONE) 

900.0 

> (audit-trail a3) 
((WITHDRAWAL (100.0) DONE) (WITHDRAWING 100.0)) 

> (setf (audit-trail a3) nil) 
NIL 

The last interaction shows the biggest flaw in CLOS: it fails to encapsulate information. 
In order to make the audi t-trai 1 accessible to the wi thdraw methods, we had 
to give it accessor methods. We would like to encapsulate the writer function for 
audit-trail so that it can only be used with deposit and withdraw. But once the 
writer function is defined it can be used anywhere, so an unscrupulous outsider can 
destroy the audit trail, setting it to nil or anything else. 

13.8 A CLOS Example: Searching Tools 
CLOS is most appropriate whenever there are several types that share related behavior. 
A good example of an application that fits this description is the set of searching 
tools defined in section 6.4. There we defined functions for breadth-first, depth-
first, and best-first search, as well as tree- and graph-based search. We also defined 
functions to search in particular domains, such as planning a route between cities. 

If we had written the tools in a straightforward procedural style, we would have 
ended up with dozens of similar functions. Instead, we used higher-order functions 
to control the complexity. In this section, we see how CLOS can be used to break up 
the complexity in a slightly different fashion. 

We begin by defining the class of search problems. Problems will be classified 
according to their domain (route planning, etc.), their topology (tree or graph) and 
their search strategy (breadth-first or depth-first, etc.). Each combination of these 
features results in a new class of problem. This makes it easy for the user to add a new 
class to represent a new domain, or a new search strategy. The basic class, probl em, 
contains a single-instance variable to hold the unexplored states of the problem. 

<a id='page-449'></a>
(defclass problem () 
((states linitarg estates :accessor problem-states))) 

The function searcher is similar to the function tree-search of section 6.4. The 
main difference is that searcher uses generic functions instead of passing around 
functional arguments. 

(defmethod searcher ((prob problem)) 
"Find a state that solves the search problem." 
(cond ((no-states-p prob) fail) 

((goal-p prob) (current-state prob)) 
(t (let ((current (pop-state prob))) 
(setf (problem-states prob) 

(problem-combiner 
prob 
(problem-successors prob current) 
(problem-states prob)))) 

(searcher prob)))) 

searcher does not assume that the problem states are organized in a list; rather, it 
uses the generic function no-states-p to test if there are any states, pop-state to 
remove and return the first state, and current - state to access the first state. For the 
basic probl em class, we will in fact implement the states as a list, but another class of 
problem is free to use another representation. 

(defmethod current-state ((prob problem)) 
"The current state is the first of the possible states." 
(first (problem-states prob))) 

(defmethod pop-state ((prob problem)) 
"Remove and return the current state." 
(pop (problem-states prob))) 

(defmethod no-states-p ((prob problem)) 
"Are there any more unexplored states?" 
(null (problem-states prob))) 

In tree - sea rch, we included a statement to print debugging information. We can do 
that here, too, but we can hide it in a separate method so as not to clutter up the main 
definition of searcher. It is a :before method because we want to see the output 
before carrying out the operation. 

<a id='page-450'></a>

(defmethod searcher .-before ((prob problem)) 
(dbg 'search "~&;; Search: ~a" (problem-states prob))) 

The generic functions that remain to be defined are goal -p, probl em-combi ner, and 
probl em-successors. We will address goal -p first, by recognizing that for many 
problems we will be searching for a state that is eql to a specified goal state. We 
define the class eql -probl em to refer to such problems, and specify goal -p for that 
class. Note that we make it possible to specify the goal when a problem is created, 
but not to change the goal: 

(defclass eql-problem (problem) 
((goal :initarg :goal :reader problem-goal))) 

(defmethod goal-p ((prob eql-problem)) 
(eql (current-state prob) (problem-goal prob))) 

Now we are ready to specify two search strategies: depth-first search and 
breadth-first search. We define problem classes for each strategy and specify the 
probl em- combi ner function: 

(defclass dfs-problem (problem) () 
(:documentation "Depth-first search problem.")) 

(defclass bfs-problem (problem) () 
(:documentation "Breadth-first search problem.")) 

(defmethod problem-combiner ((prob dfs-problem) new old) 
"Depth-first search looks at new states first." 
(append new old)) 

(defmethod problem-combiner ((prob bfs-problem) new old) 
"Depth-first search looks at old states first." 
(append old new)) 

While this code will be sufficient for our purposes, it is less than ideal, because it 
breaks an information-hiding barrier. It treats the set of old states as a list, which is the 
default for the . r obi em class but is not necessarily the implementation that every class 
will use. It would have been cleaner to define generic functions add - sta tes - to - end 
and add-states-to-front and then define them with append in the default class. 
But Lisp provides such nice list-manipulation primitives that it is difficult to avoid 
the temptation of using them directly. 

Of course, the user who defines a new implementation for probl em-states 
could just redefine probl em- combi ner for the offending classes, but this is precisely 
what object-oriented programming is designed to avoid: specializing one abstraction 
(states) should not force us to change anything in another abstraction (search 
strategy). 

<a id='page-451'></a>
The last step is to define a class that represents a particular domain, and define 
problem-successors for that domain. As the first example, consider the simple 
binary tree search from section 6.4. Naturally, this gets represented as a class: 

(defclass binary-tree-problem (problem) ()) 

(defmethod problem-successors ((prob binary-tree-problem) state) 
(let ((n (* 2 state))) 
(list . (+ . 1)))) 

Now suppose we want to solve a binary-tree problem with breadth-first search, 
searching for a particular goal. Simply create a class that mixes in 
binary-tree-problem, eql-problem and bfs-problem, create an instance of that 
class, and call searcher on that instance: 

(defclass binary-tree-eql-bfs-problem 
(binary-tree-problem eql-problem bfs-problem) ()) 

> (setf pi (make-instance 'binary-tree-eql-bfs-problem 
istates '(1) :goal 12)) 
#<BINARY-TREE-EQL-BFS-PROBLEM 26725536> 

> (searcher pi) 
Search: (1) 
Search: (2 3) 
Search: (3 4 5) 
Search: (4 5 6 7) 
Search: (5 6 7 8 9) 
Search: (6 7 8 9 10 11) 
Search: (7 8 9 10 11 12 13) 
Search: (8 9 10 11 12 13 14 15) 
Search: (9 10 11 12 13 14 15 16 17) 
Search: (10 11 12 13 14 15 16 17 18 19) 
Search: (11 12 13 14 15 16 17 18 19 20 21) 
Search: (12 13 14 15 16 17 18 19 20 21 22 23) 

12 

Best-First Search 

It should be clear how to proceed to define best-first search: define a class to represent 
best-first search problems, and then define the necessary methods for that class. 
Since the search strategy only affects the order in which states are explored, the only 
method necessary will be for probl em- combi ner. 

<a id='page-452'></a>

(defclass best-problem (problem) 0 
(.'documentation "A Best-first search problem.")) 

(defmethod problem-combiner ((prob best-problem) new old) 
"Best-first search sorts new and old according to cost-fn." 
(sort (append new old) #'< 

:key #'(lambda (state) (cost-fn prob state)))) 

This introduces the new function cost -f n; naturally it will be a generic function. The 
following is a cos t -f . that is reasonable for any eq 1 - . rob1 em dealing with numbers, 
but it is expected that most domains will specialize this function. 

(defmethod cost-fn ((prob eql-problem) state) 
(abs (- state (problem-goal prob)))) 

Beam search is a modification of best-first search where all but the best b states are 
thrown away on each iteration. A beam search problem is represented by a class 
where the instance variable beam-width holds the parameter b. If this nil, then full 
best-first search is done. Beam search is implemented by an : a round method on 
problem-combiner. It calls the next method to get the list of states produced by 
best-first search, and then extracts the first 6 elements. 

(defclass beam-problem (problem) 
((beam-width :initarg :beam-width :initform nil 
:reader problem-beam-width))) 

(defmethod problem-combiner raround ((prob beam-problem) new old) 
(let ((combined (call-next-method))) 
(subseq combined 0 (min (problem-beam-width prob) 
(length combined))))) 

Now we apply beam search to the binary-tree problem. As usual, we have to make 
up another class to represent this type of problem: 

(defclass binary-tree-eql-best-beam-problem 
(binary-tree-problem eql-problem best-problem beam-problem) 
()) 

> (setf p3 (make-instance 'binary-tree-eql-best-beam-problem 
rstates '(1) :goal 12 :beam-width 3)) 
#<BINARY-TREE-EQL-BEST-BEAM-PROBLEM 27523251> 

> (searcher p3) 
Search: (1) 
Search: (3 2) 
Search: (7 6 2) 
Search: (14 15 6) 
Search: (15 6 28) 

<a id='page-453'></a>
Search: (6 28 30) 
Search: (12 13 28) 
12 

So far the case for CLOS has not been compelling. The code in this section duplicates 
the functionality of code in section 6.4, but the CLOS code tends to be more verbose, 
and it is somewhat disturbing that we had to make up so many long class names. 
However, this verbosity leads to flexibility, and it is easier to extend the CLOS code by 
adding new specialized classes. It is useful to make a distinction between the systems 
programmer and the applications programmer. The systems programmer would 
supply a library of classes like dfs-problem and generic functions like searcher. 
The applications programmer then just picks what is needed from the library. From 
the following we see that it is not too difficult to pick out the right code to define a 
trip-planning searcher. Compare this with the definition of tri . on [page 198](chapter6.md#page-198) to see 
if you prefer CLOS in this case. The main difference is that here we say that the cost 
function is a i r-di stance and the successors are the nei ghbors by defining methods; 
in tri . we did it by passing parameters. The latter is a little more succint, but the 
former may be more clear, especially as the number of parameters grows. 

(defclass trip-problem (binary-tree-eql-best-beam-problem) 
((beam-width :initform 1))) 

(defmethod cost-fn ((prob trip-problem) city) 
(air-distance (problem-goal prob) city)) 

(defmethod problem-successors ((prob trip-problem) city) 
(neighbors city)) 

With the definitions in place, it is easy to use the searching tool: 

> (setf p4 (make-instance 'trip-problem 
estates (list (city 'new-york)) 
:goal (city 'san-francisco))) 

#<TRIP-PROBLEM 31572426> 

> (searcher p4) 
Search: ((NEW-YORK 73.58 40.47)) 
Search: ((PITTSBURG 79.57 40.27)) 
Search: ((CHICAGO 87.37 41.5)) 
Search: ((KANSAS-CITY 94.35 39.06)) 
Search: ((DENVER 105.0 39.45)) 

;; Search: ((FLAGSTAFF 111.41 35.13)) 

Search: ((RENO 119.49 39.3)) 
;: Search: ((SAN-FRANCISCO 122.26 37.47)) 
(SAN-FRANCISCO 122.26 37.47) 

<a id='page-454'></a>

13.9 Is CLOS Object-Oriented? 
There is some argument whether CLOS is really object-oriented at all. The arguments 
are: 

CLOS IS an object-oriented system because it provides all three of the main criteria 
for object-orientation: objects with internal state, classes of objects with specialized 
behavior for each class, and inheritance between classes. 

CLOS is not an object-oriented system because it does not provide modular 
objects with information-hiding. In the audi ted-account example, we would like to 
encapsulate the audit-trail instance variable so that only the withdraw methods 
can change it. But because methods are written separately from class definitions, 
we could not do that. Instead, we had to define an accessor for audi t-trai 1. That 
enabled us to write the withdraw methods, but it also made it possible for anyone 
else to alter the audit trail as well. 

CLOS ismore general than an object-oriented system because it allows for methods 
that specialize on more than one argument. In true object-oriented systems, methods 
are associated with objects of a particular class. This association is lexically obvious 
(and the message-passing metaphor is clear) when we write the methods inside the 
definition of the class, asinourdef i ne-cl ass macro. The message-passing metaphor 
is still apparent when we write generic functions that dispatch on the class of their 
first argument, which is how we've been using CLOS so far. 

But CLOS methods can dispatch on the class of any required argument, or any 
combination of them. Consider the following definition of cone, which is like append 
except that it works for vectors as well as lists. Rather than writing cone using 
conditional statements, we can use the multimethod dispatch capabilities of CLOS 
to define the four cases: (1) the first argument is nil, (2) the second argument is nil, 

(3) both arguments are lists, and (4) both arguments are vectors. Notice that if one of 
the arguments is nil there will be two applicable methods, but the method for nul 1 
will be used because the class nul 1 is more specific than the class list. 
(defmethod cone ((x null) y) y) 

(defmethod cone (x (y null)) x) 

(defmethod cone ((x list) (y list)) 
(cons (first x) (cone (rest x) y))) 

(defmethod cone ((x vector) (y vector)) 

(let ((vect (make-array (+ (length x) (length y))))) 

(replace vect x) 

(replace vect y rstartl (length x)))) 

<a id='page-455'></a>
Here we see that this definition works: 

> (cone nil '(a b c)) => (A . C) 

> (cone '(a b c) nil) => (A . C) 

> (cone '(a b c) '(d e f)) (A . C D . F) 

> (cone '#(a b e) '#(d e f)) => #(A . C D . F) 

It works, but one might well ask: where are the objects? The metaphor of passing a 
message to an object does not apply here, unless we consider the object to be the list 
of arguments, rather than a single privileged argument. 

It is striking that this style of method definition is very similar to the style used 
in Prolog. As another example, compare the following two definitions of 1 en, a 
relation/function to compute the length of a list: 

CLOS %% Prolog 
(defmethod len ((x null)) 0) len(C].0). 

(defmethod len ((x eons)) len([XIL].N1) :(+ 
1 (len (rest x)))) len(L.N). Nl is N+1. 

13.10 Advantages of Object-Oriented 
Programming 
Bertrand Meyer, in his book on the object-oriented language Eiffel (1988), lists five 
qualities that contribute to software quality: 

* Correctness. Clearly, a correct program is of the upmost importance. 
* Robustness. Programs should continue to function in a reasonable manner even 
for input that is beyond the original specifications. 
* Extendability. Programs should be easy to modify when the specifications 
change. 
* Reusability. Program components should be easy to transport to new programs, 
thus amortizing the cost of software development over several projects. 
* Compatibility. Programs should interface well with other programs. For example, 
a spreadsheet program should not only manipulate numbers correctly but 
also be compatible with word processing programs, so that spreadsheets can 
easily be included in documents. 
<a id='page-456'></a>

Here we list how the object-oriented approach in general and CLOS in particular 
can effect these measures of quality: 

* Conectness. Correctness is usually achieved in two stages: correctness of 
individual modules and correctness of the whole system. The object-oriented 
approach makes it easier to prove correctness for modules, since they are 
clearly defined, and it may make it easier to analyze interactions between 
modules, since the interface is strictly limited. CLOS does not provide for 
information-hiding the way other systems do. 
* Robustness. Generic functions make it possible for a function to accept, at run 
time, a class of argument that the programmer did not anticipate at compile 
time. This is particularly true in CLOS, because multiple inheritance makes it 
feasible to write default methods that can be used by a wide range of classes. 
* Extendability. Object-oriented systems with inheritance make it easy to define 
new classes that are slight variants on existing ones. Again, CLOS's multiple 
inheritance makes extensions even easier than in single-inheritance systems. 
* Reusability. This is the area where the object-oriented style makes the biggest 
contribution. Instead of writing each new program from scratch, object-
oriented programmers can look over a library of classes, and either reuse 
existing classes as is, or specialize an existing class through inheritance. Large 
libraries of CLOS classes have not emerged yet. Perhaps they will when the 
language is more established. 
* Compatibility. The more programs use standard components, the more they will 
be able to communicate with each other. Thus, an object-oriented program will 
probably be compatible with other programs developed from the same library 
of classes. 
13.11 History and References 
The first object-oriented language was Simula, which was designed by Ole-Johan 
Dahl and Krysten Nygaard (1966, Nygaard and Dahl 1981) as an extension of Algol 60. 
It is still in use today, mostly in Norway and Sweden. Simula provides the ability to 
define classes with single inheritance. Methods can be inherited from a superclass 
or overridden by a subclass. It also provides coroutines, class instances that execute 
continuously, saving local state in instance variables but periodically pausing to let 
other coroutines run. Although Simula is a general-purpose language, it provides 
special support for simulation, as the name implies. The built-in class simul ation 
allows a programmer to keep track of simulated time while running a set of processes 
as coroutines. 

<a id='page-457'></a>
In 1969 Alan Kay was a graduate student at the University of Utah. He became 
aware of Simula and realized that the object-oriented style was well suited to his 
research in graphics (Kay 1969). A few years later, at Xerox, he joined with Adele 
Goldberg and Daniel Ingalls to develop the Smalltalk language (see Goldberg and 
Robinson 1983). While Simula can be viewed as an attempt to add object-oriented 
features to strongly typed Algol 60, Smalltalk can be seen as an attempt to use the 
dynamic, loosely typed features of Lisp, but with methods and objects replacing 
functions and s-expressions. In Simula, objects existed alongside traditional data 
types like numbers and strings; in Smalltalk, every datum is an object. This gave 
Smalltalk the feel of an integrated Lisp environment, where the user can inspect, copy, 
or edit any part of the environment. In fact, it was not the object-oriented features of 
Smalltalk per se that have made a lasting impression but rather the then-innovative 
idea that every user would have a large graphical display and could interact with the 
system using a mouse and menus rather than by typing commands. 

Guy Steele's LAMBDA: The Ultimate Declarative (1976a and b) was perhaps the 
first paper to demonstrate how object-oriented programming can be done in Lisp. As 
the title suggests, it was all done using 1 ambda, in a similar way to our def i ne-cl ass 
example. Steele summarized the approach with the equation "Actors = Closures 
(mod Syntax)," refering to Carl Hewitt's "Actors" object-oriented formalism. 

In 1979, the MIT Lisp Machine group developed the Flavors system based on this 
approach but offering considerable extensions (Cannon 1980, Weinreb 1980, Moon 
et al. 1983). "Flavor" was a popular jargon word for "type" or "kind" at MIT, so it was 
natural that it became the term for what we call classes. 

The Flavor system was the first to support multiple inheritance. Other languages 
shunned multiple inheritance because it was too dynamic. With single inheritance, 
each instance variable and method could be assigned a unique offset number, and 
looking up a variable or method was therefore trivial. But with multiple inheritance, 
these computations had to be done at run time. The Lisp tradition enabled programmers 
to accept this dynamic computation, when other languages would not. 
Once it was accepted, the MIT group soon came to embrace it. They developed 
complex protocols for combining different flavors into new ones. The concept of 
mix-ins was developed by programmers who frequented Steve's Ice Cream parlor in 
nearby Davis Square. Steve's offered a list of ice cream flavors every day but also 
offered to create new flavors—dynamically—by mixing in various cookies, candies, 
or fruit, at the request of the individual customer. For example, Steve's did not have 
chocolate-chip ice cream on the menu, but you could always order vanilla ice cream 
with chocolate chips mixed in.^ 

This kind of "flavor hacking" appealed to the MIT Lisp Machine group, who 

^Flavor fans will be happy to know that Steve's Ice Cream is now sold nationally in the 
United States. Alas, it is not possible to create flavors dynamically. Also, be warned that 
Steve's was bought out by his Teal Square rival, Joey's. The original Steve retired from the 
business for years, then came back with a new line of stores under his last name, Harrell. 

<a id='page-458'></a>

adopted the metaphor for their object-oriented programming system. All flavors 
inherited from the top-most flavor in the hierarchy: vanilla. In the window system, for 
example, the flavor basi c-wi ndow was defined to support the minimal functionality 
of all windows, and then new flavors of window were defined by combining mix-in 
flavors such as scroll -bar-mixin, label -mixin, and border-mixin. These mix-in 
flavors were used only to define other flavors. Just as you couldn't go into Steve's and 
order "crushed Heath bars, hold the ice cream," there was a mechanism to prohibit 
instantiation of mix-ins. 

A complicated repetoire of method combinations was developed. The default 
method combination on Flavors was similar to CLOS: first do all the : before methods, 
then the most specific primary method, then the : after methods. But it was 
possible to combine methods in other ways as well. For example, consider the 
i ns i de - wi dth method, which returns the width in pixels of the usuable portion of a 
window. A programmer could specify that the combined method for i nsi de-wi dth 
was to be computed by calling all applicable methods and summing them. Then an 
inside-width method for the basic-window flavor would be defined to return the 
width of the full window, and each mix-in would have a simple method to say how 
much of the width it consumed. For example, if borders are 8 pixels wide and scroll 
bars are 12 pixels wide, then the i nsi de-wi dth method for border-mi xi . returns -8 
andscroll -bar-mixinreturns -12. Thenany window, no matter how many mix-ins 
it is composed of, automatically computes the proper inside width. 

In 1981, Symbolics came out with a more efficient implementation of Flavors. 
Objects were no longer just closures. They were still funcallable, but there was 
additional hardware support that distinguished them from other functions. After a 
few years Symbolics abandoned the (send object message) syntax in favor of a new 
syntax based on generic functions. This system was known as New Flavors. It had a 
strong influence on the eventual CLOS design. 

The other strong influence on CLOS was the CommonLoops system developed 
at Xerox PARC. (See Bobrow 1982, Bobrow et al. 1986, Stefik and Bobrow 1986.) 
CommonLoops continued the New Flavors trend away from message passing by 
introducing multimethods: methods that specialize on more than one argument. 

As of summer 1991, CLOS itself is in a state of limbo. It was legitimitized by its 
appearance in Common Lisp the Language, 2d edition, but it is not yet official, and an 
important part, the metaobject protocol, is not yet complete. A tutorial on CLOS is 
Keenel989. 

We have seen how easy it is to build an object-oriented system on top of Lisp, 
using 1 ambda as the primary tool. An interesting alternative is to build Lisp on top of 
an object-oriented system. That is the approach taken in the Oaklisp system of Lang 
and Perlmutter (1988). Instead of defining methods using 1 ambda as the primitive, 
OakHsp has add-method as a primitive and defines 1 ambda as a macro that adds a 
method to an anonymous, empty operation. 

Of course, object-oriented systems are thriving outside the Lisp world. With the 

<a id='page-459'></a>

success of UNIX-based workstations, C has become one of the most widely available 
programming languages. C is a fairly low-level language, so there have been several 
attempts to use it as a kind of portable assembly language. The most succesful of 
these attempts is C++, a language developed by Bjarne Stroustrup of AT&T Bell Labs 
(Stroustrup 1986). C++ provides a number of extensions, including the ability to 
define classes. However, as an add-on to an existing language, it does not provide as 
many features as the other languages discussed here. Crucially, it does not provide 
garbage collection, nor does it support fully generic functions. 

Eiffel (Meyer 1988) is an attempt to define an object-oriented system from the 
ground up rather than tacking it on to an existing language. Eiffel supports multiple 
inheritance and garbage collection and a limited amount of dynamic dispatching. 

So-called modern languages like Ada and Modula support information-hiding 
through generic functions and classes, but they do not provide inheritance, and thus 
can not be classified as true object-oriented languages. 

Despite these other languages, the Lisp-based object-oriented systems are the 
only ones since Smalltalk to introduce important new concepts: multiple inheritance 
and method combination from Flavors, and multimethods from CommonLoops. 

13.12 Exercises 
&#9635; Exercise 13.3 [m] Implement deposit and interest methods for the account class 
using CLOS. 

&#9635; Exercise 13.4 [m] Implement the password-account class using CLOS. Can it be 
done as cleanly with inheritance as it was done with delegation? Or should you use 
delegation within CLOS? 

&#9635; Exercise 13.5 [h] Implement graph searching, search paths, and A* searching as 
classes in CLOS. 

&#9635; Exercise 13.6 [h] Implement a priority queue to hold the states of a problem. Instead 
of a list, the probl em-states will be a vector of lists, each initially null. Each 
new state will have a priority (determined by the generic function priori ty) which 
must be an integer between zero and the length of the vector, where zero indicates the 
highest priority. A new state with priority . is pushed onto element . of the vector, 
and the state to be explored next is the first state in the first nonempty position. As 
stated in the text, some of the previously defined methods made the unwarranted 
assumption that probl em-states would always hold a Hst. Change these methods. 

