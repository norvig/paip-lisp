# Chapter 2 {docsify-ignore}
<a id='page-34'></a>

## A Simple Lisp Program 

> *Cerium quod factum.*  
> (One is certain of only what one builds.) 
> 
> -Giovanni Battista Vico (1668-1744)  
> Italian royal historiographer 

You will never become proficient in a foreign language by studying vocabulary lists. 
Rather, you must hear and speak (or read and write) the language to gain proficiency. 
The same is true for learning computer languages. 

This chapter shows how to combine the basic functions and special forms of Lisp into a 
complete program. If you can learn how to do that, then acquiring the remaining vocabulary of 
Lisp (as outlined in chapter 3) will be easy. 

<a id='page-35'></a>
## 2.1 A Grammar for a Subset of English 
The program we will develop in this chapter generates random English sentences. 
Here is a simple grammar for a tiny portion of English: 

> *Sentence &rArr; Noun-Phrase + Verb-Phrase  
Noun-Phrase &rArr; Article + Noun  
Verb-Phrase &rArr; Verb + Noun-Phrase  
Article &rArr; the, a, ...  
Noun &rArr; man, ball, woman, table...  
Verb &rArr; hit, took, saw, liked...*

To be technical, this description is called a *context-free phrase-structure grammar,* and 
the underlying paradigm is called *generative syntax.* The idea is that anywhere we 
want a sentence, we can generate a noun phrase followed by a verb phrase. Anywhere 
a noun phrase has been specified, we generate instead an article followed by a noun. 
Anywhere an article has been specified, we generate either "the," "a," or some other 
article. The formalism is "context-free" because the rules apply anywhere regardless 
of the surrounding words, and the approach is "generative" because the rules as a 
whole define the complete set of sentences in a language (and by contrast the set of 
nonsentences as well). In the following we show the derivation of a single sentence 
using the rules: 


* To get a *Sentence,* append a *Noun-Phrase* and a *Verb-Phrase* 
  * To get a *Noun-Phrase*, append an *Article* and a *Noun* 
    * Choose *"the"* for the *Article* 
    * Choose *"man"* for the *Noun* 
  * The resulting *Noun-Phrase* is "the man" 
  * To get a *Verb-Phrase,* append a *Verb* and a *Noun-Phrase* 
    * Choose *"hit"* for the *Verb* 
    * To get a *Noun-Phrase*, append an *Article* and a *Noun* 
      * Choose *"the"* for the *Article* 
      * Choose *"ball"* for the *Noun* 
    * The resulting *Noun-Phrase* is "the ball" 
  * The resulting *Verb-Phrase* is "hit the ball" 
* The resulting Sentence is "The man hit the ball" 

## 2.2 A Straightforward Solution 
We will develop a program that generates random sentences from a phrase-structure 
grammar. The most straightforward approach is to represent each grammar rule by 
a separate Lisp function: 

<a id='page-36'></a>

```lisp
(defun sentence ()    (append (noun-phrase) (verb-phrase))) 
(defun noun-phrase () (append (Article) (Noun))) 
(defun verb-phrase () (append (Verb) (noun-phrase))) 
(defun Article ()     (one-of '(the a))) 
(defun Noun ()        (one-of '(man ball woman table)) ) 
(defun Verb ()        (one-of '(hit took saw liked)) ) 
```

Each of these function definitions has an empty parameter list, `()`. That means the 
functions take no arguments. This is unusual because, strictly speaking, a function 
with no arguments would always return the same thing, so we would use a constant 
instead. However, these functions make use of the `random` function (as we will see 
shortly), and thus can return different results even with no arguments. Thus, they 
are not functions in the mathematical sense, but they are still called functions in Lisp, 
because they return a value. 

All that remains now is to define the function `one-of`. It takes a list of possible 
choices as an argument, chooses one of these at random, and returns a one-element 
list of the element chosen. This last part is so that all functions in the grammar will 
return a list of words. That way, we can freely apply append to any category. 

```lisp
(defun one-of (set) 
  "Pick one element of set, and make a list of it." 
  (list (random-elt set))) 

(defun random-elt (choices) 
  "Choose an element from a list at random." 
  (elt choices (random (length choices)))) 
```

There are two new functions here, `elt` and `random`, `elt` picks an element out of a list. 
The first argument is the list, and the second is the position in the list. The confusing 
part is that the positions start at 0, so `(elt choices 0)` is the first element of the list, 
and `(elt choices 1)` is the second. Think of the position numbers as telling you 
how far away you are from the front. The expression `(random n)` returns an integer 
from 0 to n-1, so that `(random 4)` would return either 0, 1, 2, or 3. 

Now we can test the program by generating a few random sentences, along with 
a noun phrase and a verb phrase: 

<a id='page-37'></a>

```lisp
> (sentence) => (THE WOMAN HIT THE BALL) 

> (sentence) => (THE WOMAN HIT THE MAN) 

> (sentence) =>(THE BALL SAW THE WOMAN) 

> (sentence) => (THE BALL SAW THE TABLE) 

> (noun-phrase) => (THE MAN) 

> (verb-phrase) => (LIKED THE WOMAN) 

> (trace sentence noun-phrase verb-phrase article noun verb) => 
(SENTENCE NOUN-PHRASE VERB-PHRASE ARTICLE NOUN VERB) 

> (sentence) => 
(1 ENTER SENTENCE) 
  (1 ENTER NOUN-PHRASE) 
    (1 ENTER ARTICLE) 
    (1 EXIT ARTICLE: (THE)) 
    (1 ENTER NOUN) 
    (1 EXIT NOUN: (MAN)) 
  (1 EXIT NOUN-PHRASE: (THE MAN)) 
  (1 ENTER VERB-PHRASE) 
    (1 ENTER VERB) 
    (1 EXIT VERB: (HIT)) 
    (1 ENTER NOUN-PHRASE) 
      (1 ENTER ARTICLE) 
      (1 EXIT ARTICLE: (THE)) 
      (1 ENTER NOUN) 
      (1 EXIT NOUN: (BALL) 
    (1 EXIT NOUN-PHRASE: (THE BALL) 
  (1 EXIT VERB-PHRASE: (HIT THE BALL) 
(1 EXIT SENTENCE: (THE MAN HIT THE BALL) 
(THE MAN HIT THE BALL) 
```

The program works fine, and the trace looks just like the sample derivation above, 
but the Lisp definitions are a bit harder to read than the original grammar rules. 
This problem will be compounded as we consider more complex rules. Suppose we 
wanted to allow noun phrases to be modified by an indefinite number of adjectives 
and an indefinite number of prepositional phrases. In grammatical notation, we 
might have the following rules: 

> *Noun-Phrase &rArr; Article + Adj\* + Noun + PP\*  
> Adj\* &rArr; 0&#x0338;, Adj + Adj\*  
> PP\* &rArr; 0&#x0338;, PP + PP\*  
> PP &rArr; Prep + Noun-Phrase  
> Adj &rArr; big, little, blue, green, ...  
> Prep &rArr; to, in, by, with, ...*


In this notation, 0&#x0338; indicates a choice of nothing at all, a comma indicates a choice of 
several alternatives, and the asterisk is nothing special—as in Lisp, it's just part of the 
name of a symbol. However, the convention used here is that names ending in an 
asterisk denote zero or more repetitions of the underlying name. That is, *PP*\* denotes 
zero or more repetitions of *PP.* This is known as "Kleene star" notation (pronounced 
<a id='page-38'></a>
"clean-E") after the mathematician Stephen Cole Kleene.[TK - fn1] 
The problem is that the rules for Adj * and PP * contain choices that we would have 
to represent as some kind of conditional in Lisp. For example: 

```lisp
(defun Adj* () 
  (if (= (random 2) 0) 
      nil 
      (append (Adj) (Adj*)))) 

(defun PP* () 
  (if (random-elt '(t nil)) 
      (append (PP) (PP*)) 
      nil)) 

(defun noun-phrase () (append (Article) (Adj*) (Noun) (PP*))) 
(defun PP () (append (Prep) (noun-phrase))) 
(defun Adj () (one-of '(big little blue green adiabatic))) 
(defun Prep () (one-of '(to in by with on))) 
```

I've chosen two different implementations for `Adj*` and `PP*`; either approach would 
work in either function. We have to be careful, though; here are two approaches that 
would not work: 

```lisp
(defun Adj* () 
  "Warning - incorrect definition of Adjectives." 
  (one-of '(nil (append (Adj) (Adj*))))) 

(defun Adj* () 
  "Warning - incorrect definition of Adjectives." 
  (one-of (list nil (append (Adj) (Adj*))))) 
```

The first definition is wrong because it could return the literal expression `((append (Adj) (Adj *)))` 
rather than a list of words as expected. The second definition would 
cause infinite recursion, because computing the value of `(Adj*)` always involves a 
recursive call to `(Adj*)`. The point is that what started out as simple functions are 
now becoming quite complex. To understand them, we need to know many Lisp 
conventions—`defun`, `()`, `case`, `if`, `quote`, and the rules for order of evaluation—when 
ideally the implementation of a grammar rule should use only *linguistic* conventions. 
If we wanted to develop a larger grammar, the problem could get worse, because the 
rule-writer might have to depend more and more on Lisp. 

[TK, fn1] We will soon see "Kleene plus" notation, wherein *PP+* denotes one or more repetitions 
of *PP.* 

<a id='page-39'></a>
## 2.3 A Rule-Based Solution 
An alternative implementation of this program v^ould concentrate on making it easy 
to write grammar rules and would worry later about how they will be processed. 
Let's look again at the original grammar rules: 

> *Sentence &rArr; Noun-Phrase + Verb-Phrase  
Noun-Phrase &rArr; Article + Noun  
Verb-Phrase &rArr; Verb + Noun-Phrase  
Article &rArr; the, a, ...  
Noun &rArr; man, ball, woman, table...  
Verb &rArr; hit, took, saw, liked...*

Each rule consists of an arrow with a symbol on the left-hand side and something on 
the right-hand side. The complication is that there can be two kinds of right-hand 
sides: a concatenated list of symbols, as in *"Noun-Phrase &rArr; Article+Noun,"* or a list of 
alternate words, as in *"Noun => man, ball, ..."* We can account for these possibilities 
by deciding that every rule will have a list of possibilities on the right-hand side, and 
that a concatenated list, *for example "Article+Noun,"* will be represented as a Lisp list, 
*for example* "`(Article Noun)`". The list of rules can then be represented as follows: 

```lisp
(defparameter *simple-grammar* 
  '((sentence -> (noun-phrase verb-phrase)) 
    (noun-phrase -> (Article Noun)) 
    (verb-phrase -> (Verb noun-phrase)) 
    (Article -> the a) 
    (Noun -> man ball woman table) 
    (Verb -> hit took saw liked)) 
  "A grammar for a trivial subset of English.") 

(defvar *grammar* *simple-grammar* 
  "The grammar used by generate. Initially, this is 
  *simple-grammar*, but we can switch to other grammars.") 
```

Note that the Lisp version of the rules closely mimics the original version. In particular, 
I include the symbol "`->`", even though it serves no real purpose; it is purely 
decorative. 

The special forms `defvar` and `defparameter` both introduce special variables 
and assign a value to them; the difference is that a *variable,* like `*grammar*`, is 
routinely changed during the course of running the program. A *parameter,* like 
`*simple-grammar*`, on the other hand, will normally stay constant. A change to a 
parameter is considered a change to the program, not a change *by* the program. 

Once the list of rules has been defined, it can be used to find the possible rewrites 
of a given category symbol. The function `assoc` is designed for just this sort of task. 

<a id='page-40'></a>

It takes two arguments, a "key" and a list of lists, and returns the first element of the 
list of lists that starts with the key. If there is none, it returns `nil`. Here is an example: 

```lisp
> (assoc 'noun *grammar*) (NOUN -> MAN BALL WOMAN TABLE) 
```

Although rules are quite simply implemented as lists, it is a good idea to impose a 
layer of abstraction by defining functions to operate on the rules. We will need three 
functions: one to get the right-hand side of a rule, one for the left-hand side, and one 
to look up all the possible rewrites (right-hand sides) for a category. 

```lisp
(defun rule-lhs (rule) 
  "The left-hand side of a rule." 
  (first rule)) 

(defun rule-rhs (rule) 
  "The right-hand side of a rule." 
  (rest (rest rule))) 

(defun rewrites (category) 
  "Return a list of the possible rewrites for this category." 
  (rule-rhs (assoc category *grammar*))) 
```

Defining these functions will make it easier to read the programs that use them, 
and it also makes changing the representation of rules easier, should we ever decide 
to do so. 

We are now ready to address the main problem: defining a function that will 
generate sentences (or noun phrases, or any other category). We will call this function 
`generate`. It will have to contend with three cases: (1) In the simplest case, `generate` 
is passed a symbol that has a set of rewrite rules associated with it. We choose one of 
those at random, and then generate from that. (2) If the symbol has no possible rewrite 
rules, it must be a terminal symbol—a word, rather than a grammatical category—and 
we want to leave it alone. Actually, we return the list of the input word, because, as 
in the previous program, we want all results to be lists of words. (3) In some cases, 
when the symbol has rewrites, we will pick one that is a list of symbols, and try to 
generate from that. Thus, `generate` must also accept a list as input, in which case 
it should generate each element of the list, and then append them all together. In 
the following, the first clause in `generate` handles this case, while the second clause 
handles (1) and the third handles (2). Note that we used the `mappend` function from 
section 1.7 ([page 18](chapter1.md#page-18)). 

<a id='page-41'></a>

```lisp
(defun generate (phrase) 
  "Generate a random sentence or phrase" 
  (cond ((listp phrase) 
        (mappend #'generate phrase)) 
        ((rewrites phrase) 
         (generate (random-elt (rewrites phrase)))) 
        (t (list phrase)))) 
```

Like many of the programs in this book, this function is short, but dense with 
information: the craft of programming includes knowing what *not* to write, as well 
as what to write. 

This style of programming is called *data-driven* programming, because the data 
(the list of rewrites associated with a category) drives what the program does next. It 
is a natural and easy-to-use style in Lisp, leading to concise and extensible programs, 
because it is always possible to add a new piece of data with a new association without 
having to modify the original program. 

Here are some examples of `generate` in use: 

```lisp
> (generate 'sentence) => (THE TABLE SAW THE BALL) 

> (generate 'sentence) => (THE WOMAN HIT A TABLE) 

> (generate 'noun-phrase) => (THE MAN) 

> (generate 'verb-phrase) (TOOK A TABLE) 
```

There are many possible ways to write `generate`. The following version uses `if` 
instead of `cond`: 

```lisp
(defun generate (phrase) 
  "Generate a random sentence or phrase" 
  (if (listp phrase) 
      (mappend #'generate phrase) 
      (let ((choices (rewrites phrase))) 
        (if (null choices) 
            (list phrase) 
            (generate (random-elt choices)))))) 
```

This version uses the special form `let`, which introduces a new variable (in this case, 
`choices`) and also binds the variable to a value. In this case, introducing the variable 
saves us from calling the function `rewrites` twice, as was done in the `cond` version 
of `generate`. The general form of a `let` form is: 

> `(let` ((var value)...)  
> &nbsp;&nbsp; body-containing-vars)*

`let` is the most common way of introducing variables that are not parameters of 
functions. One must resist the temptation to use a variable without introducing it: 

<a id='page-42'></a>

```lisp
(defun generate (phrase) 
  (setf choices ...)         ;; wrong! 
  ... choices ...) 
```

This is wrong because the symbol `choices` now refers to a special or global variable, 
one that may be shared or changed by other functions. Thus, the function `generate` 
is not reliable, because there is no guarantee that `choices` will retain the same value 
from the time it is set to the time it is referenced again. With `let` we introduce a brand 
new variable that nobody else can access; therefore it is guaranteed to maintain the 
proper value. 

&#9635; Exercise 2.1 [m] Write a version of `generate` that uses `cond` but avoids calling 
`rewrites` twice. 

&#9635; Exercise 2.2 [m] Write a version of `generate` that explicitly differentiates between 
terminal symbols (those with no rewrite rules) and nonterminal symbols. 

## 2.4 Two Paths to Follow 
The two versions of the preceding program represent two alternate approaches that 
come up time and time again in developing programs: (1) Use the most straightforward 
mapping of the problem description directly into Lisp code. (2) Use the most 
natural notation available to solve the problem, and then worry about writing an 
interpreter for that notation. 

Approach (2) involves an extra step, and thus is more work for small problems. 
However, programs that use this approach are often easier to modify and expand. 
This is especially true in a domain where there is a lot of data to account for. The 
grammar of natural language is one such domain—in fact, most AI problems fit this 
description. The idea behind approach (2) is to work with the problem as much as 
possible in its own terms, and to minimize the part of the solution that is written 
directly in Lisp. 

Fortunately, it is very easy in Lisp to design new notations—in effect, new programming 
languages. Thus, Lisp encourages the construction of more robust programs. 
Throughout this book, we will be aware of the two approaches. The reader may 
notice that in most cases, we choose the second. 

<a id='page-43'></a>
## 2.5 Changing the Grammar without Changing the Program 
We show the utility of approach (2) by defining a new grammar that includes adjectives, 
prepositional phrases, proper names, and pronouns. We can then apply the 
`generate` function without modification to this new grammar. 

```lisp
(defparameter *bigger-grammar* 
  '((sentence -> (noun-phrase verb-phrase)) 
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun)) 
    (verb-phrase -> (Verb noun-phrase PP*)) 
    (PP* -> () (PP PP*)) 
    (Adj* -> () (Adj Adj*)) 
    (PP -> (Prep noun-phrase)) 
    (Prep -> to in by with on) 
    (Adj -> big little blue green adiabatic) 
    (Article -> the a) 
    (Name -> Pat Kim Lee Terry Robin) 
    (Noun -> man ball woman table) 
    (Verb -> hit took saw liked) 
    (Pronoun -> he she it these those that))) 

(setf *grammar* *bigger-grammar*) 

> (generate 'sentence) 
(A TABLE ON A TABLE IN THE BLUE ADIABATIC MAN SAW ROBIN 
 WITH A LITTLE WOMAN) 

> (generate 'sentence) 
(TERRY SAW A ADIABATIC TABLE ON THE GREEN BALL BY THAT WITH KIM 
 IN THESE BY A GREEN WOMAN BY A LITTLE ADIABATIC TABLE IN ROBIN 
 ON LEE) 

> (generate 'sentence) 
(THE GREEN TABLE HIT IT WITH HE) 
```

Notice the problem with case agreement for pronouns: the program generated "with 
he," although "with him" is the proper grammatical form. Also, it is clear that the 
program does not distinguish sensible from silly output. 

## 2.6 Using the Same Data for Several Programs 
Another advantage of representing information in a declarative form-as rules or 
facts rather than as Lisp functions-is that it can be easier to use the information for 
multiple purposes. Suppose we wanted a function that would generate not just the 
<a id='page-44'></a>
list of words in a sentence but a representation of the complete syntax of a sentence. 
For example, instead of the list `(a woman took a ball)`, we want to get the nested list: 

```lisp
(SENTENCE (NOUN-PHRASE (ARTICLE A) (NOUN WOMAN)) 
          (VERB-PHRASE (VERB TOOK) 
                       (NOUN-PHRASE (ARTICLE A) (NOUN BALL)))) 
```

This corresponds to the tree that linguists draw as in figure 2.1. 

```
TK: diagram
sentence 

art noun verb art noun 
I I I I I 
a woman took a ball 
```

Figure 2.1: Sentence Parse Tree 

Using the "straightforward functions" approach we would be stuck; we'd have to 
rewrite every function to generate the additional structure. With the "new notation" 
approach we could keep the grammar as it is and just write one new function: a 
version of `generate` that produces nested lists. The two changes are to `cons` the 
category onto the front of each rewrite, and then not to append together the results 
but rather just list them with `mapcar`: 

```lisp
(defun generate-tree (phrase) 
  "Generate a random sentence or phrase, 
  with a complete parse tree." 
  (cond ((listp phrase) 
         (mapcar #'generate-tree phrase)) 
        ((rewrites phrase) 
         (cons phrase 
               (generate-tree (random-elt (rewrites phrase))))) 
        (t (list phrase)))) 
```

Here are some examples: 

<a id='page-45'></a>

```lisp
> (generate-tree 'Sentence) 
(SENTENCE (NOUN-PHRASE (ARTICLE A) 
                       (ADJ*) 
                       (NOUN WOMAN) 
                       (PP*)) 
      (VERB-PHRASE (VERB HIT) 
                       (NOUN-PHRASE (PRONOUN HE)) 
                       (PP*))) 

> (generate-tree 'Sentence) 
(SENTENCE (NOUN-PHRASE (ARTICLE A) 
                       (NOUN WOMAN)) 
          (VERB-PHRASE (VERB TOOK) 
                       (NOUN-PHRASE (ARTICLE A) (NOUN BALL)))) 
```

As another example of the one-data/multiple-program approach, we can develop a 
function to generate all possible rewrites of a phrase. The function `generate-all`  
returns a list of phrases rather than just one, and we define an auxiliary function, 
`combine-all`, to manage the combination of results. Also, there are four cases instead 
of three, because we have to check for nil explicitly. Still, the complete program is 
quite simple: 

```lisp
(defun generate-all (phrase) 
  "Generate a list of all possible expansions of this phrase." 
  (cond ((null phrase) (list nil)) 
        ((listp phrase) 
         (combine-all (generate-all (first phrase)) 
                      (generate-all (rest phrase)))) 
        ((rewrites phrase) 
         (mappend #'generate-all (rewrites phrase))) 
        (t (list (list phrase))))) 

(defun combine-all (xlist ylist) 
  "Return a list of lists formed by appending a y to an x. 
  E.g., (combine-all '((a) (b)) '((1) (2))) 
  -> ((A 1) (B 1) (A 2) (B 2))." 
  (mappend #'(lambda (y) 
               (mapcar #'(lambda (x) (append . y)) xlist)) 
           ylist)) 
```

We can now use `generate-all` to test our original little grammar. Note that a serious 
drawback of `generate-all` is that it can't deal with recursive grammar rules like 
`Adj* => Adj + Adj*` that appear in `*bigger-grammar*`, since these lead to an infinite 
number of outputs. But it works fine for finite languages, like the language generated 
by `*simple-grammar*`: 

<a id='page-46'></a>

```lisp
> (generate-all 'Article) 

((THE) (A)) 

> (generate-all 'Noun) 

((MAN) (BALL) (WOMAN) (TABLE)) 

> (generate-all 'noun-phrase) 
((A MAN) (A BALL) (A WOMAN) (A TABLE) 
 (THE MAN) (THE BALL) (THE WOMAN) (THE TABLE)) 

> (length (generate-all 'sentence)) 
256 
```

There are 256 sentences because every sentence in this language has the form Article-
Noun-Verb-Article-Noun, and there are two articles, four nouns and four verbs 
(2 &times; 4 &times; 4 &times; 2 &times; 4 = 256). 

## 2.7 Exercises 
&#9635; Exercise 2.3 [h] Write a trivial grammar for some other language. This can be a 
natural language other than English, or perhaps a subset of a computer language. 

&#9635; Exercise 2.4 [m] One way of describing `combine-all` is that it calculates the cross-product 
of the function a ppend on the argument lists. Write the higher-order function 
`cross-product`, and define `combine-all` in terms of it. \
The moral is to make your code as general as possible, because you never know what 
you may want to do with it next. 

## 2.8 Answers 
### Answer 2.1 
```lisp
  (defun generate (phrase) 
  "Generate a random sentence or phrase" 
  (let ((choices nil)) 
    (cond ((listp phrase) 
        (mappend #'generate phrase)) 
       ((setf choices (rewrites phrase)) 
        (generate (random-elt choices))) 
       (t (list phrase))))) 
```

<a id='page-47'></a>
### Answer 2.2 

```lisp
(defun generate (phrase) 
  "Generate a random sentence or phrase" 
  (cond ((listp phrase) 
         (mappend #'generate phrase)) 
        ((non-terminal-p phrase) 
         (generate (random-elt (rewrites phrase)))) 
        (t (list phrase)))) 

(defun non-terminal-p (category) 
  "True if this is a category in the grammar." 
  (not (null (rewrites category)))) 
```

### Answer 2.4 
```lisp
(defun cross-product (fn xlist ylist) 
  "Return a list of all (fn . y) values." 
  (mappend #'(lambda (y) 
               (mapcar #'(lambda (x) (funcall fn x y)) 
                       xlist)) 
           ylist)) 

(defun combine-all (xlist ylist) 
  "Return a list of lists formed by appending a y to an x" 
  (cross-product #'append xlist ylist)) 
```

Now we can use the `cross-product` in other ways as well: 

```
> (cross-product #'+ '(1 2 3) '(10 20 30)) 
(11 12 13 
 21 22 23 
 31 32 33) 

> (cross-product #'list '(a b c d e f g h) 
                        '(1 2 3 4 5 6 7 8)) 
((A 1) (B 1) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1) 
 (A 2) (B 2) (C 2) (D 2) (E 2) (F 2) (G 2) (H 2) 
 (A 3) (B 3) (C 3) (D 3) (E 3) (F 3) (G 3) (H 3) 
 (A 4) (B 4) (C 4) (D 4) (E 4) (F 4) (G 4) (H 4) 
 (A 5) (B 5) (C 5) (D 5) (E 5) (F 5) (G 5) (H 5) 
 (A 6) (B 6) (C 6) (D 6) (E 6) (F 6) (G 6) (H 6) 
 (A 7) (B 7) (C 7) (D 7) (E 7) (F 7) (G 7) (H 7) 
 (A 8) (B 8) (C 8) (D 8) (E 8) (F 8) (G 8) (H 8)) 
```

