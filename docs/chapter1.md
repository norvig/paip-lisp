# Chapter 1
## Introduction to Lisp

> You think you know when you learn, are more sure when you can write, even more when you can teach, but certain when you can program.
>
> -Alan Perlis \
> Yale University computer scientist

This chapter is for people with little or no experience in Lisp.
Readers who feel confident in their Lisp programming ability can quickly skim the chapter or skip it entirely.
This chapter necessarily moves quickly, so those with little programming experience, or any reader who finds this chapter tough going, should seek out a supplementary introductory text.
My recommendations are in the preface.

Computers allow one to carry out computations.
A word processing program deals with words while a calculator deals with numbers, but the principles are the same.
In both cases, you provide the input (words or numbers) and specify the operations (such as deleting a word or adding two numbers) to yield a result (a completed document or calculation).

We will refer to anything that can be represented in the memory of a computer as a *computational object,* or just an *object.*
So, words, paragraphs, and numbers can be objects.
And because the operations (deleting and adding) must be represented somewhere in the computer's memory, they are objects, too.

Normally, the distinction between a computer "user" and a computer "programmer" is that the user provides new input, or data (words or numbers), while the programmer defines new *operations*, or programs, as well as new *types* of data.
Every new object, be it datum or operation, must be defined in terms of previously defined objects.
The bad news is that it can be quite tedious to get these definitions right.
The good news is that each new object can in turn be used in the definition of future objects.
Thus, even complex programs can be built out of smaller, simpler objects.
This book covers a number of typical AI problems, showing how each problem can be broken down into manageable pieces, and also how each piece can be described in the programming language Common Lisp.
Ideally, readers will learn enough through studying these examples to attack new AI problems with style, grace, and success.

Let's consider a simple example of a computation: finding the sum of two numbers, let's say 2 and 2.
If we had a calculator handy, we would type "2 + 2 =" and see the answer displayed.
On a calculator using reverse Polish notation, we would have to type "2 2 +" to see the same answer.
In Lisp, as with the calculator, the user carries out an interactive dialog with the computer by typing in an expression and seeing the computer print the value of that expression.
This interactive mode is different from many other programming languages that only offer a batch mode, wherein an entire program is compiled and run before any output can be seen.

We start up a pocket calculator by flipping the on/off switch.
The Lisp program must also be started, but the details vary from one computer to another, so I can't explain how your Lisp will work.
Assuming we have managed to start up Lisp, we are likely to see a *prompt* of some kind.
On my computer, Lisp types "`>`" to indicate it is ready to accept the next computation.
So we are faced with a screen that looks like this:

```lisp
>
```

We may now type in our computation and see the result displayed.
It turns out that the Lisp convention for arithmetic expressions is slightly different: a computation consists of a parenthesized list with the operation name first, followed by any number of operands, or arguments.
This is called *prefix notation.*

```lisp
> (+ 2 2)
4
>
```

We see that Lisp has printed the answer, 4, and then another prompt, >, to indicate it is ready for the next computation.
Throughout this book, all Lisp expressions will be displayed in `typewriter` font.
Text on the same line as the ">" prompt is input typed by the user, and text following it is output printed by the computer.
Usually, input that is typed by the programmer will be in `lowercase` letters, while output that is printed back by the computer will be in `UPPERCASE` letters.
Of course, with symbols like + and 4 there is no difference.

To save space on the page, the output will sometimes be shown on the same line as the input, separated by an arrow (=>), which can be read as "evaluates to," and can also be thought of as standing for the return or enter key that the user presses to complete the input:

```lisp
> (+ 2 2) => 4
```

One advantage of parenthesized prefix notation is that the parentheses clearly mark the beginning and end of an expression.
If we want, we can give + more than two arguments, and it will still add them all:

```lisp
> (+ 1 2 3 4 5 6 7 8 9 10) => 55
```

This time we try (9000 + 900 + 90 + 9) - (5000 + 500 + 50 + 5):

```lisp
> (- (+ 9000 900 90 9) (+ 5000 500 50 5)) => 4444
```

This example shows that expressions can be nested.
The arguments to the - function are parenthesized lists, while the arguments to each `+` are atoms.
The Lisp notation may look unusual compared to standard mathematical notation, but there are advantages to this notation; since Lisp expressions can consist of a function followed by any number of arguments, we don't have to keep repeating the "`+`". More important than the notation is the rule for evaluation.
In Lisp, lists are evaluated by first evaluating all the arguments, then applying the function to the arguments, thereby computing the result.
This rule is much simpler than the rule for evaluating normal mathematical expressions, where there are many conventions to remember, such as doing multiplications and divisions before sums and differences.
We will see below that the actual Lisp evaluation rule is a little more complicated, but not much.

Sometimes programmers who are familiar with other languages have preconceptions that make it difficult for them to learn Lisp.
For them, three points are worth stressing here.
First, many other languages make a distinction between statements and expressions.
An expression, like `2 + 2`, has a value, but a statement, like `x = 2 + 2`, does not.
Statements have effects, but they do not return values.
In Lisp, there is no such distinction: every expression returns a value.
It is true that some expressions have effects, but even those expressions also return values.

Second, the lexical rules for Lisp are much simpler than the rules for other languages.
In particular, there are fewer punctuation characters: only parentheses, quote marks (single, double, and backward), spaces, and the comma serve to separate symbols from each other.
Thus, while the statement `y=a*x+3` is analyzed as seven separate tokens in other languages, in Lisp it would be treated as a single symbol.
<a id="tfn01-1"></a>
To get a list of tokens, we would have to insert spaces: `(y = a * x + 3)`.<sup>[1](#fn01-1)</sup>

Third, while many languages use semicolons to delimit statements, Lisp has no need of semicolons, since expressions are delimited by parentheses.
Lisp chooses to use semicolons for another purpose—to mark the beginning of a comment, which lasts until the end of the line:

```lisp
> (+ 2 2) ; this is a comment
4
```

## 1.1 Symbolic Computation

All we've done so far is manipulate numbers in the same way a simple pocket calculator would.
Lisp is more useful than a calculator for two main reasons.
First, it allows us to manipulate objects other than numbers, and second, it allows us to define new objects that might be useful in subsequent computations.
We will examine these two important properties in turn.

Besides numbers, Lisp can represent characters (letters), strings of characters, and arbitrary symbols, where we are free to interpret these symbols as referring to things outside the world of mathematics.
Lisp can also build nonatomic objects by combining several objects into a list.
This capability is fundamental and well supported in the language; in fact, the name Lisp is short for LISt Processing.

Here's an example of a computation on lists:

```lisp
> (append '(Pat Kim) '(Robin Sandy)) => (PAT KIM ROBIN SANDY)
```

This expression appends together two lists of names.
The rule for evaluating this expression is the same as the rule for numeric calculations: apply the function (in this case append) to the value of the arguments.

The unusual part is the quote mark `(')`, which serves to block the evaluation of the following expression, returning it literally.
If we just had the expression `(Pat Kim)`, it would be evaluated by considering `Pat` as a function and applying it to the value of the expression `Kim`.
This is not what we had in mind.
The quote mark instructs Lisp to treat the list as a piece of data rather than as a function call:

```lisp
> '(Pat Kim) => (PAT KIM)
```

In other computer languages (and in English), quotes usually come in pairs: one to mark the beginning, and one to mark the end.
In Lisp, a single quote is used to mark the beginning of an expression.
Since we always know how long a single expression is—either to the end of an atom or to the matching parenthesis of a list—we don't need an explicit punctuation mark to tell us where the expression ends.
Quotes can be used on lists, as in `'(Pat Kim)`, on symbols as in `'Robin`, and in fact on anything else.
Here are some examples:

```lisp
> 'John => JOHN

> '(John Q Public) => (JOHN Q PUBLIC)

> '2 => 2

> 2 => 2

> '(+ 2 2) => (+ 2 2)

> (+ 2 2) 4

> John => *Error: JOHN is not a bound variable*

> (John Q Public) => *Error: JOHN is not a function*
```

Note that `'2` evaluates to `2` because it is a quoted expression, and `2` evaluates to `2` because numbers evaluate to themselves.
Same result, different reason.
In contrast, `'John` evaluates to `John` because it is a quoted expression, but evaluating `John` leads to an error, because evaluating a symbol means getting the value of the symbol, and no value has been assigned to `John`.

Symbolic computations can be nested and even mixed with numeric computations.
The following expression builds a list of names in a slightly different way than we saw before, using the built-in function `list`.
We then see how to find the number of elements in the list, using the built-in function `length`:

```lisp
> (append '(Pat Kim) (list '(John Q Public) 'Sandy))
(PAT KIM (JOHN Q PUBLIC) SANDY)

> (length (append '(Pat Kim) (list '(John Q Public) 'Sandy)))
4
```

There are four important points to make about symbols:

*   First, it is important to remember that Lisp does not attach any external significance to the objects it manipulates.
For example, we naturally think of (`Robin Sandy`) as a list of two first names, and (`John Q Public`) as a list of one person's first name, middle initial, and last name.
Lisp has no such preconceptions.
To Lisp, both `Robin` and `xyzzy` are perfectly good symbols.

*   Second, to do the computations above, we had to know that `append`, `length`, and `+` are defined functions in Common Lisp.
Learning a language involves remembering vocabulary items (or knowing where to look them up) as well as learning the basic rules for forming expressions and determining what they mean.
Common Lisp provides over 700 built-in functions.
At some point the reader should flip through a reference text to see what's there, but most of the important functions are presented in part I of this book.

*   Third, note that symbols in Common Lisp are not case sensitive.
<a id="tfn01-2"></a>
By that I mean that the inputs `John`, `john`, and `jOhN` all refer to the same symbol, which is normally printed as `JOHN`.<sup>[2](#fn01-2)</sup>

*   Fourth, note that a wide variety of characters are allowed in symbols: numbers, letters, and other punctuation marks like `'+'` or `'!'`
The exact rules for what constitutes a symbol are a little complicated, but the normal convention is to use symbols consisting mostly of letters, with words separated by a dash `(-)`, and perhaps with a number at the end.
Some programmers are more liberal in naming variables, and include characters like `'?!$/<=>'`.
For example, a function to convert dollars to yen might be named with the symbol `$-to-yen` or `$->yen` in Lisp, while one would use something like `DollarsToYen, dollars_to_yen` or `dol2yen` in Pascal or C.
There are a few exceptions to these naming conventions, which will be dealt with as they come up.

## 1.2 Variables

We have seen some of the basics of symbolic computation.
Now we move on to perhaps the most important characteristic of a programming language: the ability to define new objects in terms of others, and to name these objects for future use.
Here symbols again play an important role-they are used to name variables.
A variable can take on a value, which can be any Lisp object.
One way to give a value to a variable is with `setf`:

```lisp
> (setf p '(John Q Public)) => (JOHN Q PUBLIC)
> p => (JOHN Q PUBLIC)
> (setf x 10) => 10
> (+ x x) => 20
> (+ x (length p)) => 13
```

After assigning the value (`John Q Public`) to the variable named `p`, we can refer to the value with the name `p`.
Similarly, after assigning a value to the variable named `x`, we can refer to both `x` and `p`.

Symbols are also used to name functions in Common Lisp.
Every symbol can be used as the name of a variable or a function, or both, although it is rare (and potentially confusing) to have symbols name both.
For example, `append` and `length` are symbols that name functions but have no values as variables, and `pi` does not name a function but is a variable whose value is 3.1415926535897936 (or thereabout).

## 1.3 Special Forms

The careful reader will note that `setf` violates the evaluation rule.
We said earlier that functions like `+`, `-` and `append` work by first evaluating all their arguments and then applying the function to the result.
But `setf` doesn't follow that rule, because `setf` is not a function at all.
Rather, it is part of the basic syntax of Lisp.
Besides the syntax of atoms and function calls, Lisp has a small number of syntactic expressions.
They are known as *special forms.*
They serve the same purpose as statements in other programming languages, and indeed have some of the same syntactic markers, such as `if` and `loop`.
There are two main differences between Lisp's syntax and other languages.
First, Lisp's syntactic forms are always lists in which the first element is one of a small number of privileged symbols.
`setf` is one of these symbols, so (`setf x 10`) is a special form.
Second, special forms are expressions that return a value.
This is in contrast to statements in most languages, which have an effect but do not return a value.

In evaluating an expression like `(setf x (+ 1 2)`), we set the variable named by the symbol `x` to the value of `(+ 1 2)`, which is `3`.
If `setf` were a normal function, we would evaluate both the symbol `x` and the expression `(+ 1 2)` and do something with these two values, which is not what we want at all.
`setf` is called a special form because it does something special: if it did not exist, it would be impossible to write a function that assigns a value to a variable.
The philosophy of Lisp is to provide a small number of special forms to do the things that could not otherwise be done, and then to expect the user to write everything else as functions.

The term *special form* is used confusingly to refer both to symbols like `setf` and expressions that start with them, like `(setf x 3)`.
In the book *Common LISPcraft,* Wilensky resolves the ambiguity by calling `setf` a *special function,* and reserving the term *special form* for (`setf x 3`).
This terminology implies that `setf` is just another function, but a special one in that its first argument is not evaluated.
Such a view made sense in the days when Lisp was primarily an interpreted language.
The modern view is that `setf` should not be considered some kind of abnormal function but rather a marker of special syntax that will be handled specially by the compiler.
Thus, the special form `(setf x (+ 2 1))` should be considered the equivalent of `x = 2 + 1` in `C`.
When there is risk of confusion, we will call `setf` a *special form operator* and `(setf x 3)` a *special form expression.*

It turns out that the quote mark is just an abbreviation for another special form.
The expression '*x* is equivalent to `(quote` *x*`)`, a special form expression that evaluates to *x*.
The special form operators used in this chapter are:

| []()            |                                              |
|-----------------|----------------------------------------------|
| `defun`         | define function                              |
| `defparameter`  | define special variable                      |
| `setf`          | set variable or field to new value           |
| `let`           | bind local variable(s)                       |
| `case`          | choose one of several alternatives           |
| `if`            | do one thing or another, depending on a test |
| `function (#')` | refer to a function                          |
| `quote (')`     | introduce constant data                      |

## 1.4 Lists

So far we have seen two functions that operate on lists: `append` and `length`. Since lists are important, let's look at some more list processing functions:

```lisp
> p => (JOHN Q PUBLIC)

> (first p) JOHN

> (rest p) => (Q PUBLIC)

> (second p) => Q

> (third p) => PUBLIC

> (fourth p) => NIL

> (length p) => 3
```

The functions `first`, `second`, `third`, and `fourth` are aptly named: `first` returns the first element of a list, `second` gives you the second element, and so on.
The function `rest` is not as obvious; its name stands for "the rest of the list after the first element." The symbol `nil` and the form `()` are completely synonymous; they are both representations of the empty list.
`nil` is also used to denote the "false" value in Lisp.
Thus, `(fourth p)` is `nil` because there is no fourth element of `p`.
Note that lists need not be composed only of atoms, but can contain sublists as elements:

```lisp
> (setf x '((1st element) 2 (element 3) ((4)) 5))
((1ST ELEMENT) 2 (ELEMENT 3) ((4)) 5)

> (length x) => 5

> (first x) => (1ST ELEMENT)

> (second x) => 2

> (third x) => (ELEMENT 3)

> (fourth x) => ((4))

> (first (fourth x)) => (4)

> (first (first (fourth x))) => 4

> (fifth x) => 5

> (first x) => (1ST ELEMENT)

> (second (first x)) => ELEMENT
```

So far we have seen how to access parts of lists.
It is also possible to build up new lists, as these examples show:

```lisp
> p => (JOHN Q PUBLIC)

> (cons 'Mr p) => (MR JOHN Q PUBLIC)

> (cons (first p) (rest p)) => (JOHN Q PUBLIC)

> (setf town (list 'Anytown 'USA)) => (ANYTOWN USA)

> (list p 'of town 'may 'have 'already 'won!) =>
((JOHN Q PUBLIC) OF (ANYTOWN USA) MAY HAVE ALREADY WON!)

> (append p '(of) town '(may have already won!)) =>
(JOHN Q PUBLIC OF ANYTOWN USA MAY HAVE ALREADY WON!)

> p => (JOHN Q PUBLIC)
```

The function cons stands for "construct."
<a id="tfn01-3"></a>
It takes as arguments an element and a list,<sup>[3](#fn01-3)</sup> and constructs a new list whose first is the element and whose rest is the original list.
`list` takes any number of elements as arguments and returns a new list containing those elements in order.
We've already seen `append`, which is similar to `list`; it takes as arguments any number of lists and appends them all together, forming one big list.
Thus, the arguments to `append` must be lists, while the arguments to `list` may be lists or atoms.
It is important to note that these functions create new lists; they don't modify old ones.
When we say `(append p q)`, the effect is to create a brand new list that starts with the same elements that were in `p`.
`p` itself remains unchanged.

Now let's move away from abstract functions on lists, and consider a simple problem: given a person's name in the form of a list, how might we extract the family name?
For `(JOHN Q PUBLIC)` we could just use the function `third`, but that wouldn't work for someone with no middle name.
There is a function called `last` in Common Lisp; perhaps that would work.
We can experiment:

```lisp
> (last p) => (PUBLIC)

> (first (last p)) => PUBLIC
```

<a id="tfn01-4"></a>
It turns out that `last` perversely returns a list of the last element, rather than the last element itself.<sup>[4](#fn01-4)</sup>
Thus we need to combine `first` and `last` to pick out the actual last element.
We would like to be able to save the work we've done, and give it a proper description, like `last-name`.
We could use `setf` to save the last name of `p`, but that wouldn't help determine any other last name.
Instead we want to define a new function that computes the last name of *any* name that is represented as a list.
The next section does just that.

## 1.5 Defining New Functions

The special form `defun` stands for "define function."
It is used here to define a new function called `last-name`:

```lisp
(defun last-name (name)
  "Select the last name from a name represented as a list."
  (first (last name)))
```

We give our new function the name `last-name`. It has a *parameter list* consisting of a single parameter: (`name`).
This means that the function takes one argument, which we will refer to as `name`.
It also has a *documentation string* that states what the function does.
This is not used in any computation, but documentation strings are crucial tools for debugging and understanding large systems.
The body of the definition is `(first (last name))`, which is what we used before to pick out the last name of `p`.
The difference is that here we want to pick out the last name of any `name`, not just of the particular name `p`.

In general, a function definition takes the following form (where the documentation string is optional, and all other parts are required):

`(defun` *function-name* (*parameter...*)
&nbsp;&nbsp;&nbsp;&nbsp;"*documentation string*"
&nbsp;&nbsp;&nbsp;&nbsp;*function-body...*)

The function name must be a symbol, the parameters are usually symbols (with some complications to be explained later), and the function body consists of one or more expressions that are evaluated when the function is called.
The last expression is returned as the value of the function call.

Once we have defined `last-name`, we can use it just like any other Lisp function:

```lisp
> (last-name p) => PUBLIC

> (last-name '(Rear Admiral Grace Murray Hopper)) => HOPPER

> (last-name '(Rex Morgan MD)) => MD

> (last-name '(Spot)) => SPOT

> (last-name '(Aristotle)) => ARISTOTLE
```

The last three examples point out an inherent limitation of the programming enterprise.
When we say `(defun last-name...)` we are not really defining what it means for a person to have a last name; we are just defining an operation on a representation of names in terms of lists.
Our intuitions-that MD is a title, Spot is the first name of a dog, and Aristotle lived before the concept of last name was invented-are not represented in this operation.
However, we could always change the definition of `last-name` to incorporate these problematic cases.

We can also define the function `first-name`.
Even though the definition is trivial (it is the same as the function `first`), it is still good practice to define `first-name` explicitly.
Then we can use the function `first-name` when we are dealing with names, and `first` when we are dealing with arbitrary lists.
The computer will perform the same operation in each case, but we as programmers (and readers of programs) will be less confused.
Another advantage of defining specific functions like `first-name` is that if we decide to change the representation of names we will only have to change the definition of `first-name`.
This is a much easier task than hunting through a large program and changing the uses of `first` that refer to names, while leaving other uses alone.

```lisp
(defun first-name (name)
  "Select the first name from a name represented as a list."
  (first name))

> p => (JOHN Q PUBLIC)

> (first-name p) => JOHN

> (first-name '(Wilma Flintstone)) => WILMA

> (setf names '((John Q Public) (Malcolm X)
              (Admiral Grace Murray Hopper) (Spot)
              (Aristotle) (A A Milne) (Z Z Top)
              (Sir Larry Olivier) (Miss Scarlet))) =>

((JOHN Q PUBLIC) (MALCOLM X) (ADMIRAL GRACE MURRAY HOPPER)
 (SPOT) (ARISTOTLE) (A A MILNE) (Z Z TOP) (SIR LARRY OLIVIER)
 (MISS SCARLET))

> (first-name (first names)) => JOHN
```

In the last expression we used the function `first` to pick out the first element in a list of names, and then the function `first-name` to pick out the first name of that element.
We could also have said `(first (first names))` or even `(first (first-name names))` and still have gotten `JOHN`, but we would not be accurately representing what is being considered a name and what is being considered a list of names.

## 1.6 Using Functions

One good thing about defining a list of names, as we did above, is that it makes it easier to test our functions.
Consider the following expression, which can be used to test the `last-name` function:

```lisp
> (mapcar #'last-name names)
(PUBLIC X HOPPER SPOT ARISTOTLE MILNE TOP OLIVIER SCARLET)
```

The funny `#'` notation maps from the name of a function to the function itself.
This is analogous to `'x` notation.
The built-in function `mapcar` is passed two arguments, a function and a list.
It returns a list built by calling the function on every element of the input list.
In other words, the `mapcar` call above is equivalent to:

```lisp
(list (last-name (first names))
      (last-name (second names))
      (last-name (third names))
      ...)
```

`mapcar`'s name comes from the fact that it "maps" the function across each of the arguments.
The `car` part of the name refers to the Lisp function `car`, an old name for `first`.
`cdr` is the old name for `rest`.
The names stand for "contents of the address register" and "contents of the decrement register," the instructions that were used in the first implementation of Lisp on the IBM 704.
I'm sure you'll agree that `first` and `rest` are much better names, and they will be used instead of `car` and `cdr` whenever we are talking about lists.
However, we will continue to use `car` and `cdr` on occasion when we are considering a pair of values that are not considered as a list.
Beware that some programmers still use `car` and `cdr` for lists as well.

Here are some more examples of `mapcar`:

```lisp
> (mapcar #'- '(1 2 3 4)) => (-1 -2 -3 -4)

> (mapcar #'+ '(1 2 3 4) '(10 20 30 40)) => (11 22 33 44)
```

This last example shows that `mapcar` can be passed three arguments, in which case the first argument should be a binary function, which will be applied to corresponding elements of the other two lists.
In general, `mapcar` expects an *n*-ary function as its first argument, followed by *n* lists.
It first applies the function to the argument list obtained by collecting the first element of each list.
Then it applies the function to the second element of each list, and so on, until one of the lists is exhausted.
It returns a list of all the function values it has computed.

Now that we understand `mapcar`, let's use it to test the `first-name` function:

```lisp
> (mapcar #'first-name names)
(JOHN MALCOLM ADMIRAL SPOT ARISTOTLE A Z SIR MISS)
```

We might be disappointed with these results.
Suppose we wanted a version of `first-name` which ignored titles like Admiral and Miss, and got to the "real" first name.
We could proceed as follows:

```lisp
(defparameter *titles*
  '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General)
  "A list of titles that can appear at the start of a name.")
```

We've introduced another new special form, `defparameter`, which defines a parameter-a variable that does not change over the course of a computation, but that might change when we think of new things to add (like the French Mme or the military Lt.).
The `defparameter` form both gives a value to the variable and makes it possible to use the variable in subsequent function definitions.
In this example we have exercised the option of providing a documentation string that describes the variable.
It is a widely used convention among Lisp programmers to mark special variables by spelling their names with asterisks on either end.
This is just a convention; in Lisp, the asterisk is just another character that has no particular meaning.

<a id="tfn01-5"></a>
We next give a new definition for `first-name`, which supersedes the previous definition.<sup>[5](#fn01-5)</sup>
This definition says that if the first word of the name is a member of the list of titles, then we want to ignore that word and return the `first-name` of the rest of the words in the name.
Otherwise, we use the first word, just as before.
Another built-in function, `member`, tests to see if its first argument is an element of the list passed as the second argument.

The special form `if` has the form `(if` *test then-part else-part*).
There are many special forms for performing conditional tests in Lisp; `if` is the most appropriate for this example.
An `if` form is evaluated by first evaluating the *test* expression.
If it is true, the *then-part* is evaluated and returned as the value of the `if` form; otherwise the *else-part* is evaluated and returned.
While some languages insist that the value of a conditional test must be either `true` or `false`, Lisp is much more forgiving.
The test may legally evaluate to any value at all.
Only the value `nil` is considered false; all other values are considered true.
In the definition of `first-name` below, the function `member` will return a non-nil (hence true) value if the first element of the name is in the list of titles, and will return `nil` (hence false) if it is not.
Although all non-nil values are considered true, by convention the constant `t` is usually used to represent truth.

```lisp
(defun first-name (name)
  "Select the first name from a name represented as a list."
  (if (member (first name) *titles*)
      (first-name (rest name))
      (first name)))
```

When we map the new `first-name` over the list of names, the results are more encouraging.
In addition, the function gets the "right" result for `'(Madam Major General Paula Jones)` by dropping off titles one at a time.

```lisp
> (mapcar #'first-name names)
(JOHN MALCOLM GRACE SPOT ARISTOTLE A Z LARRY SCARLET)

> (first-name '(Madam Major General Paula Jones))
PAULA
```

We can see how this works by *tracing* the execution of `first-name`, and seeing the values passed to and returned from the function.
The special forms `trace` and `untrace` are used for this purpose.

```lisp
> (trace first-name)
(FIRST-NAME)

> (first-name '(John Q Public))
(1 ENTER FIRST-NAME: (JOHN Q PUBLIC))
(1 EXIT FIRST-NAME: JOHN)
JOHN
```

When `first-name` is called, the definition is entered with the single argument, `name`, taking on the value `(JOHN Q PUBLIC)`.
The value returned is `JOHN`.
Trace prints two lines indicating entry and exit from the function, and then Lisp, as usual, prints the final result, `JOHN`.

The next example is more complicated.
The function `first-name` is used four times.
First, it is entered with `name` bound to `(Madam Major General Paula Jones)`.
The first element of this list is `Madam`, and since this is a member of the list of titles, the result is computed by calling `first-name` again on the rest of the name-`(Major General Paula Jones)`.
This process repeats two more times, and we finally enter `first-name` with name bound to (`Paula Jones`).
Since `Paula` is not a title, it becomes the result of this call to `first-name`, and thus the result of all four calls, as trace shows.
Once we are happy with the workings of `first-name`, the special form `untrace` turns off tracing.

```lisp
> (first-name '(Madam Major General Paula Jones)) =>
(1 ENTER FIRST-NAME: (MADAM MAJOR GENERAL PAULA JONES))
  (2 ENTER FIRST-NAME: (MAJOR GENERAL PAULA JONES))
    (3 ENTER FIRST-NAME: (GENERAL PAULA JONES))
      (4 ENTER FIRST-NAME: (PAULA JONES))
      (4 EXIT FIRST-NAME: PAULA)
    (3 EXIT FIRST-NAME: PAULA)
  (2 EXIT FIRST-NAME: PAULA)
(1 EXIT FIRST-NAME: PAULA)
PAULA

> (untrace first-name) => (FIRST-NAME)

> (first-name '(Mr Blue Jeans)) => BLUE
```

The function `first-name` is said to be *recursive* because its definition includes a call to itself.
Programmers who are new to the concept of recursion sometimes find it mysterious.
But recursive functions are really no different from nonrecursive ones.
Any function is required to return the correct value for the given input(s).
Another way to look at this requirement is to break it into two parts: a function must return a value, and it must not return any incorrect values.
This two-part requirement is equivalent to the first one, but it makes it easier to think about and design function definitions.

Next I show an abstract description of the `first-name` problem, to emphasize the design of the function and the fact that recursive solutions are not tied to Lisp in any way:

`function first-name(name):`
&nbsp;&nbsp;&nbsp;&nbsp;`if` *the first element of name is a title*
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`then` *do something complicated to get the first-name*
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`else` *return the first element of the name*

This breaks up the problem into two cases.
In the second case, we return an answer, and it is in fact the correct answer.
We have not yet specified what to do in the first case.
But we do know that it has something to do with the rest of the name after the first element, and that what we want is to extract the first name out of those elements.
The leap of faith is to go ahead and use `first-name`, even though it has not been fully defined yet:

`function first-name(name):`
&nbsp;&nbsp;&nbsp;&nbsp;`if` *the first element of name is a title*
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`then` *return the* `first-name` *of the rest of the name*
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`else` *return the first element of the name*

Now the first case in `first-name` is recursive, and the second case remains unchanged.
We already agreed that the second case returns the correct answer, and the first case only returns what `first-name` returns.
So `first-name` as a whole can only return correct answers.
Thus, we're halfway to showing that the function is correct; the other half is to show that it eventually returns some answer.
But every recursive call chops off the first element and looks at the rest, so for an *n*-element list there can be at most *n* recursive calls.
This completes the demonstration that the function is correct.
Programmers who learn to think this way find recursion to be a valuable tool rather than a confusing mystery.

## 1.7 Higher-Order Functions

Functions in Lisp can not only be "called," or applied to arguments, they can also be manipulated just like any other kind of object.
A function that takes another function as an argument is called a *higher-order function.*
`mapcar` is an example.
To demonstrate the higher-order-function style of programming, we will define a new function called `mappend`. It takes two arguments, a function and a list.
`mappend` maps the function over each element of the list and appends together all the results.
The first definition follows immediately from the description and the fact that the function `apply` can be used to apply a function to a list of arguments.

```lisp
(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn the-list)))
```

Now we experiment a little to see how `apply` and `mappend` work.
The first example applies the addition function to a list of four numbers.

```lisp
> (apply #'+ '(1 2 3 4)) => 10
```

The next example applies append to a list of two arguments, where each argument is a list.
If the arguments were not lists, it would be an error.

```lisp
> (apply #'append '((1 2 3) (a b c))) => (1 2 3 A B C)
```

Now we define a new function, `self-and-double`, and apply it to a variety of arguments.

```lisp
> (defun self-and-double (x) (list x (+ x x)))

> (self-and-double 3) => (3 6)

> (apply #'self-and-double '(3)) => (3 6)
```

If we had tried to apply `self-and-double` to a list of more than one argument, or to a list that did not contain a number, it would be an error, just as it would be an error to evaluate (`self-and-double 3 4`) or (`self-and-double 'Kim`).
Now let's return to the mapping functions:

```lisp
> (mapcar #'self-and-double '(1 10 300)) => ((1 2) (10 20) (300 600))

> (mappend #'self-and-double '(1 10 300)) => (1 2 10 20 300 600)
```

When `mapcar` is passed a function and a list of three arguments, it always returns a list of three values.
Each value is the result of calling the function on the respective argument.
In contrast, when `mappend` is called, it returns one big list, which is equal to all the values that `mapcar` would generate appended together.
It would be an error to call `mappend` with a function that didn't return lists, because `append` expects to see lists as its arguments.

Now consider the following problem: given a list of elements, return a list consisting of all the numbers in the original list and the negation of those numbers.
For example, given the list (`testing 1 2 3 test`), return (`1 -1 2 -2 3 -3`).
This problem can be solved very easily using `mappend` as a component:

```lisp
(defun numbers-and-negations (input)
  "Given a list, return only the numbers and their negations."
  (mappend #'number-and-negation input))

(defun number-and-negation (x)
  "If x is a number, return a list of x and -x."
  (if (numberp x)
      (list x (- x))
      nil))

> (numbers-and-negations '(testing 1 2 3 test)) => (1 -1 2 -2 3 -3)
```

The alternate definition of `mappend` shown in the following doesn't make use of `mapcar;` instead it builds up the list one element at a time:

```lisp
(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (if (null the-list)
      nil
      (append (funcall fn (first the-list))
              (mappend fn (rest the-list)))))
```

`funcall` is similar to `apply;` it too takes a function as its first argument and applies the function to a list of arguments, but in the case of `funcall`, the arguments are listed separately:

```lisp
> (funcall #'+ 2 3) => 5

> (apply #'+ '(2 3)) => 5

> (funcall #'+ '(2 3)) => *Error: (2 3) is not a number.*
```

These are equivalent to `(+ 2 3)`, `(+ 2 3)`, and `(+ '(2 3))`, respectively.

So far, every function we have used has been either predefined in Common Lisp or introduced with a `defun`, which pairs a function with a name.
It is also possible to introduce a function without giving it a name, using the special syntax `lambda`.

The name *lambda* comes from the mathematician Alonzo Church's notation for functions (Church 1941).
Lisp usually prefers expressive names over terse Greek letters, but lambda is an exception.
A better name would be `make-function`.
Lambda derives from the notation in Russell and Whitehead's *Principia Mathematica,* which used a caret over bound variables: *x&#x302;*(*x + x*).

Church wanted a one-dimensional string, so he moved the caret in front: *^x*(*x + x*).
The caret looked funny with nothing below it, so Church switched to the closest thing, an uppercase lambda, *&Lambda;x*(*x + x*).
The &Lambda; was easily confused with other symbols, so eventually the lowercase lambda was substituted: *&lambda;x*(*x + x*).
John McCarthy was a student of Church's at Princeton, so when McCarthy invented Lisp in 1958, he adopted the lambda notation.
There were no Greek letters on the keypunches of that era, so McCarthy used (`lambda (x) (+ x x)`), and it has survived to this day.
In general, the form of a lambda expression is

`(lambda` (*parameters...*) *body...*)

A lambda expression is just a nonatomic *name* for a function, just as `append` is an atomic name for a built-in function.
As such, it is appropriate for use in the first position of a function call, but if we want to get at the actual function, rather than its name, we still have to use the `#'` notation.
For example:

```lisp
> ((lambda (x) (+ x 2)) 4) => 6

> (funcall #'(lambda (x) (+ x 2)) 4) => 6
```

To understand the distinction we have to be clear on how expressions are evaluated in Lisp.
The normal rule for evaluation states that symbols are evaluated by looking up the value of the variable that the symbol refers to.
So the `x` in `(+ x 2)` is evaluated by looking up the value of the variable named `x`.
A list is evaluated in one of two ways.
If the first element of the list is a special form operator, then the list is evaluated according to the syntax rule for that special form.
Otherwise, the list represents a function call.
The first element is evaluated in a unique way, as a function.
This means it can either be a symbol or a lambda expression.
In either case, the function named by the first element is applied to the values of the remaining elements in the list.
These values are determined by the normal evaluation rules.
If we want to refer to a function in a position other than the first element of a function call, we have to use the `#'` notation.
Otherwise, the expressions will be evaluated by the normal evaluation rule, and will not be treated as functions.
For example:

```lisp
> append => *Error: APPEND is not a bound variable*

> (lambda (x) (+ x 2)) => *Error: LAMBDA is not a function*
```

Here are some more examples of the correct use of functions:

```lisp
> (mapcar #'(lambda (x) (+ x x))
         '(1 2 3 4 5)) =>
(2 4 6 8 10)

> (mappend #'(lambda (l) (list l (reverse l)))
           '((1 2 3) (a b c))) =>
((1 2 3) (3 2 1) (A B C) (C B A))
```

Programmers who are used to other languages sometimes fail to see the point of lambda expressions.
There are two reasons why lambda expressions are very useful.

First, it can be messy to clutter up a program with superfluous names.
Just as it is clearer to write `(a+b)*(c+d)` rather than to invent variable names like `temp1` and `temp2` to hold `a+b` and `c+d`, so it can be clearer to define a function as a lambda expression rather than inventing a name for it.

Second, and more importantly, lambda expressions make it possible to create new functions at run time.
This is a powerful technique that is not possible in most programming languages.
These run-time functions, known as *closures,* will be covered in section 3.16.

## 1.8 Other Data Types

So far we have seen just four kinds of Lisp objects: numbers, symbols, lists, and functions.
Lisp actually defines about 25 different types of objects: vectors, arrays, structures, characters, streams, hash tables, and others.
At this point we will introduce one more, the string.
As you can see in the following, strings, like numbers, evaluate to themselves.
Strings are used mainly for printing out messages, while symbols are used for their relationships to other objects, and to name variables.
The printed representation of a string has a double quote mark `(")` at each end.

```lisp
> "a string" => "a string"

> (length "a string") => 8

> (length "") => 0
```

## 1.9 Summary: The Lisp Evaluation Rule

We can now summarize the evaluation rule for Lisp.

*   Every expression is either a *list* or an *atom.*

*   Every list to be evaluated is either a *special form expression* or a *function application*.

*   A *special form expression* is defined to be a list whose first element is a special form operator.
The expression is evaluated according to the operator's idiosyncratic evaluation rule.
For example, the evaluation rule for `setf` is to evaluate the second argument according to the normal evaluation rule, set the first argument to that value, and return the value as the result.
The rule for `defun` is to define a new function, and return the name of the function.
The rule for quote is to return the first argument unevaluated.
The notation `'x` is actually an abbreviation for the special form expression `(quote x)`.
Similarly, the notation `#'f` is an abbreviation for the special form expression `(function f)`.

```lisp
'John ≡ (quote John) => JOHN

(setf p 'John) => JOHN

(defun twice (x) (+ x x)) => TWICE

(if (= 2 3) (error) (+ 5 6)) => 11
```

*   A *function application* is evaluated by first evaluating the arguments (the rest of the list) and then finding the function named by the first element of the list and applying it to the list of evaluated arguments.

```lisp
(+ 2 3) => 5
(- (+ 90 9) (+ 50 5 (length '(Pat Kim)))) => 42
```

Note that if `'(Pat Kim)` did not have the quote, it would be treated as a function application of the function `pat` to the value of the variable `kim`.

*   Every atom is either a *symbol* or a *nonsymbol.*

*   A *symbol* evaluates to the most recent value that has been assigned to the variable named by that symbol.
Symbols are composed of letters, and possibly digits and, rarely, punctuation characters.
<a id="tfn01-6"></a>
To avoid confusion, we will use symbols composed mostly of the letters `a-z` and the `'-'` character, with a few exceptions.<sup>[6](#fn01-6)</sup>

```lisp
names
p
*print-pretty*
```

*   A *nonsymbol atom* evaluates to itself.
For now, numbers and strings are the only such non-symbol atoms we know of.
Numbers are composed of digits, and possibly a decimal point and sign.
There are also provisions for scientific notation, rational and complex numbers, and numbers with different bases, but we won't describe the details here.
Strings are delimited by double quote marks on both sides.

```lisp
42 => 42
-273.15 => -273.15
"a string" => "a string"
```

There are some minor details of Common Lisp that complicate the evaluation rules, but this definition will suffice for now.

One complication that causes confusion for beginning Lispers is the difference between *reading* and *evaluating* an expression.
Beginners often imagine that when they type an expression, such as

```lisp
> (+ (* 3 4) (* 5 6))
```

the Lisp system first reads the (`+`, then fetches the addition function, then reads `(* 3 4)` and computes `12`, then reads `(* 5 6)` and computes 30, and finally computes 42.
In fact, what actually happens is that the system first reads the entire expression, the list `(+ (* 3 4) (* 5 6))`.
Only after it has been read does the system begin to evaluate it.
This evaluation can be done by an interpreter that looks at the list directly, or it can be done by a compiler that translates the list into machine language instructions and then executes those instructions.

We can see now that it was a little imprecise to say, "Numbers are composed of digits, and possibly a decimal point and sign." It would be more precise to say that the printed representation of a number, as expected by the function read and as produced by the function print, is composed of digits, and possibly a decimal point and sign.
The internal representation of a number varies from one computer to another, but you can be sure that it will be a bit pattern in a particular memory location, and it will no longer contain the original characters used to represent the number in decimal notation.
Similarly, it is the printed representation of a string that is surrounded by double quote marks; the internal representation is a memory location marking the beginning of a vector of characters.

Beginners who fail to grasp the distinction between reading and evaluating may have a good model of what expressions evaluate to, but they usually have a terrible model of the efficiency of evaluating expressions.
One student used only one-letter variable names, because he felt that it would be faster for the computer to look up a one-letter name than a multiletter name.
While it may be true that shorter names can save a microsecond at read time, this makes no difference at all at evaluation time.
Every variable, regardless of its name, is just a memory location, and the time to access the location does not depend on the name of the variable.

## 1.10 What Makes Lisp Different?

What is it that sets Lisp apart from other languages?
Why is it a good language for AI applications?
There are at least eight important factors:

*   Built-in Support for Lists
*   Automatic Storage Management
*   Dynamic Typing
*   First-Class Functions
*   Uniform Syntax
*   Interactive Environment
*   Extensibility
*   History

In sum, these factors allow a programmer to delay making decisions.
In the example dealing with names, we were able to use the built-in list functions to construct and manipulate names without making a lot of explicit decisions about their representation.
If we decided to change the representation, it would be easy to go back and alter parts of the program, leaving other parts unchanged.

This ability to delay decisions-or more accurately, to make temporary, nonbinding decisions-is usually a good thing, because it means that irrelevant details can be ignored.
There are also some negative points of delaying decisions.
First, the less we tell the compiler, the greater the chance that it may have to produce inefficient code.
Second, the less we tell the compiler, the less chance it has of noticing inconsistencies and warning us.
Errors may not be detected until the program is run.
Let's consider each factor in more depth, weighing the advantages and disadvantages:

*   *Built-in Support for Lists.*
The list is a very versatile data structure, and while lists can be implemented in any language, Lisp makes it easy to use them.
Many AI applications involve lists of constantly changing size, making fixed-length data structures like vectors harder to use.
Early versions of Lisp used lists as their only aggregate data structure.
Common Lisp provides other types as well, because lists are not always the most efficient choice.

*   *Automatic Storage Management.*
The Lisp programmer needn't keep track of memory allocation; it is all done automatically.
This frees the programmer of a lot of effort, and makes it easy to use the functional style of programming.
Other languages present programmers with a choice.
Variables can be allocated on the stack, meaning that they are created when a procedure is entered, and disappear when the procedure is done.
This is an efficient use of storage, but it rules out functions that return complex values.
The other choice is for the programmer to explicitly allocate and free storage.
This makes the functional style possible but can lead to errors.

For example, consider the trivial problem of computing the expression *a* x (b + c), where *a*, *b*, and *c* are numbers.
The code is trivial in any language; here it is in Pascal and in Lisp:

```pascal
/* Pascal */
a * (b + c)
```

```lisp
;;; Lisp
(* a (+ b c))
```

The only difference is that Pascal uses infix notation and Lisp uses prefix.
Now consider computing *a \* (b + c)* when *a*, *b*, and *c* are matrices.
Assume we have procedures for matrix multiplication and addition.
In Lisp the form is exactly the same; only the names of the functions are changed.
In Pascal we have the choice of approaches mentioned before.
We could declare temporary variables to hold intermediate results on the stack, and replace the functional expression with a series of procedure calls:

```pascal
/* Pascal */
var temp, result: matrix;
add(b,c,temp);
mult(a,temp,result);
return(result);
```

```lisp
;;; Lisp
(mult a (add b c))
```

The other choice is to write Pascal functions that allocate new matrices on the heap.
Then one can write nice functional expressions like `mult(a,add(b,c))` even in Pascal.
However, in practice it rarely works this nicely, because of the need to manage storage explicitly:

```pascal
/* Pascal */
var a,b,c,x,y: matrix;
x := add(b.c);
y := mult(a,x);
free(x);
return(y);
```

```lisp
;;; Lisp
(mult a (add b c))
```

In general, deciding which structures to free is a difficult task for the Pascal programmer.
If the programmer misses some, then the program may run out of memory.
Worse, if the programmer frees a structure that is still being used, then strange errors can occur when that piece of memory is reallocated.
Lisp automatically allocates and frees structures, so these two types of errors can *never* occur.

*   *Dynamic Typing.*
Lisp programmers don't have to provide type declarations, because the language keeps track of the type of each object at run time, rather than figuring out all types at compile time.
This makes Lisp programs shorter and hence faster to develop, and it also means that functions can often be extended to work for objects to which they were not originally intended to apply.
In Pascal, we can write a procedure to sort an array of 100 integers, but we can't use that same procedure to sort 200 integers, or 100 strings.
In Lisp, one `sort` fits all.
One way to appreciate this kind of flexibility is to see how hard it is to achieve in other languages.
It is impossible in Pascal; in fact, the language Modula was invented primarily to fix this problem in Pascal.
The language Ada was designed to allow flexible generic functions, and a book by Musser and Stepanov (1989) describes an Ada package that gives some of the functionality of Common Lisp's sequence functions.
But the Ada solution is less than ideal: it takes a 264-page book to duplicate only part of the functionality of the 20-page chapter 14 from [Steele (1990)](bibliography.md#bb1160), and Musser and Stepanov went through five Ada compilers before they found one that would correctly compile their package.
Also, their package is considerably less powerful, since it does not handle vectors or optional keyword parameters.
In Common Lisp, all this functionality comes for free, and it is easy to add more.
On the other hand, dynamic typing means that some errors will go undetected until run time.
The great advantage of strongly typed languages is that they are able to give error messages at compile time.
The great frustration with strongly typed languages is that they are only able to warn about a small class of errors.
They can tell you that you are mistakenly passing a string to a function that expects an integer, but they can't tell you that you are passing an odd number to a function that expects an even number.

*   *First-Class Functions.*
A *first-class* object is one that can be used anywhere and can be manipulated in the same ways as any other kind of object.
In Pascal or C, for example, functions can be passed as arguments to other functions, but they are not first-class, because it is not possible to create new functions while the program is running, nor is it possible to create an anonymous function without giving it a name.
In Lisp we can do both those things using `lambda`.
This is explained in section 3.16, page 92.

*   *Uniform Syntax.*
The syntax of Lisp programs is simple.
This makes the language easy to learn, and very little time is wasted correcting typos.
In addition, it is easy to write programs that manipulate other programs or define whole new languages-a very powerful technique.
The simple syntax also makes it easy for text editing programs to parse Lisp.
Your editor program should be able to indent expressions automatically and to show matching parentheses.
This is harder to do for languages with complex syntax.
On the other hand, some people object to all the parentheses.
There are two answers to this objection.
First, consider the alternative: in a language with "conventional" syntax, Lisp's parentheses pairs would be replaced either by an implicit operator precedence rule (in the case of arithmetic and logical expressions) or by a `begin/end` pair (in the case of control structures).
But neither of these is necessarily an advantage.
Implicit precedence is notoriously error-prone, and `begin/end` pairs clutter up the page without adding any content.
Many languages are moving away from `begin/end`: `C` uses `{` and `}`, which are equivalent to parentheses, and several modern functional languages (such as Haskell) use horizontal blank space, with no explicit grouping at all.
Second, many Lisp programmers *have* considered the alternative.
There have been a number of preprocessors that translate from "conventional" syntax into Lisp.
None of these has caught on.
It is not that Lisp programmers find it *tolerable* to use all those parentheses, rather, they find it *advantageous.*
With a little experience, you may too.
It is also important that the syntax of Lisp data is the same as the syntax of programs.
Obviously, this makes it easy to convert data to program.
Less obvious is the time saved by having universal functions to handle input and output.
The Lisp functions `read` and `print` will automatically handle any list, structure, string, or number.
This makes it trivial to test individual functions while developing your program.
In a traditional language like C or Pascal, you would have to write special-purpose functions to read and print each data type you wanted to debug, as well as a special-purpose driver to call the routines.
Because this is time-consuming and error-prone, the temptation is to avoid testing altogether.
Thus, Lisp encourages better-tested programs, and makes it easier to develop them faster.

*   *Interactive Environment.*
Traditionally, a programmer would write a complete program, compile it, correct any errors detected by the compiler, and then run and debug it.
This is known as the *batch* mode of interaction.
For long programs, waiting for the compiler occupied a large portion of the debugging time.
In Lisp one normally writes a few small functions at a time, getting feedback from the Lisp system after evaluating each one.
This is known as an *interactive* environment.
When it comes time to make a change, only the changed functions need to be recompiled, so the wait is much shorter.
In addition, the Lisp programmer can debug by typing in arbitrary expressions at any time.
This is a big improvement over editing the program to introduce print statements and recompiling.
Notice that the distinction between *interactive* and a *batch* languages is separate from the distinction between *interpreted* and *compiled* languages.
It has often been stated, incorrectly, that Lisp has an advantage by virtue of being an interpreted language.
Actually, experienced Common Lisp programmers tend to use the compiler almost exclusively.
The important point is interaction, not interpretation.
The idea of an interactive environment is such a good one that even traditional languages like C and Pascal are starting to offer interactive versions, so this is not an exclusive advantage of Lisp.
However, Lisp still provides much better access to the interactive features.
A C interpreter may allow the programmer to type in an expression and have it evaluated immediately, but it will not allow the programmer to write a program that, say, goes through the symbol table and finds all the user-defined functions and prints information on them.
In C-even interpreted C-the symbol table is just a Cheshire-cat-like invention of the interpreter's imagination that disappears when the program is run.
<a id="tfn01-7"></a>
In Lisp, the symbol table is a first-class object<sup>[7](#fn01-7)</sup> that can be accessed and modified with functions like `read, intern` and `do-symbols`.
Common Lisp offers an unusually rich set of useful tools, including over 700 built-in functions (ANSI Common Lisp has over 900).
Thus, writing a new program involves more gathering of existing pieces of code and less writing of new code from scratch.
In addition to the standard functions, Common Lisp implementations usually provide extensions for interacting with the editor, debugger, and window system.

*   *Extensibility*.
When Lisp was invented in 1958, nobody could have foreseen the advances in programming theory and language design that have taken place in the last thirty years.
Other early languages have been discarded, replaced by ones based on newer ideas.
However, Lisp has been able to survive, because it has been able to adapt.
Because Lisp is extensible, it has been changed to incorporate the newest features as they become popular.
The easiest way to extend the language is with macros.
When so-called structured programming constructs such as *case* and *if-then-else* arose, they were incorporated into Lisp as macros.
But the flexibility of Lisp goes beyond adding individual constructs.
Brand new styles of programming can easily be implemented.
Many AI applications are based on the idea of *rule-based* programming.
<a id="tfn01-8"></a>
Another new style is *object-oriented* programming, which has been incorporated with the Common Lisp Object System (CLOS),<sup>[8](#fn01-8)</sup> a set of macros, functions, and data types that have been integrated into ANSI Common Lisp.

To show how far Lisp has come, here's the only sample program given in the *Lisp/MTS Programmer's Guide* (Hafner and Wilcox 1974):

```lisp
(PROG (LIST DEPTH TEMP RESTLIST)
(SETQ RESTLIST (LIST (CONS (READ) O)))
A (COND
((NOT RESTLIST) (RETURN 'DONE))
(T (SETQ LIST (UNCONS (UNCONS RESTLIST
     RESTLIST) DEPTH))
(COND ((ATOM LIST)
(MAPC 'PRIN1 (LIST '"ATOM:" LIST '"," 'DEPTH DEPTH))
(TERPRI))
(T (SETQ TEMP (UNCONS LIST LIST))
(COND (LIST
(SETQ RESTLIST (CONS(CONS LIST DEPTH) RESTLIST))))
(SETQ RESTLIST (CONS (CONS TEMP
     (ADD1 DEPTH)) RESTLIST))
))))
(GO A))
```

Note the use of the now-deprecated goto `(GO)` statement, and the lack of consistent indentation conventions.
The manual also gives a recursive version of the same program:

```lisp
(PROG NIL (
(LABEL ATOMPRINT (LAMBDA (RESTLIST)
(COND ((NOT RESTLIST) (RETURN 'DONE))
((ATOM (CAAR RESTLIST)) (MAPC 'PRIN1
     (LIST '"ATOM:" (CAAR RESTLIST)
          '"," 'DEPTH (CDAR RESTLIST)))
(TERPRI)
(ATOMPRINT (CDR RESTLIST)))
( T (ATOMPRINT (GRAFT
(LIST (CONS (CAAAR RESTLIST) (ADD1 (CDAR RESTLIST))))
(AND (CDAAR RESTLIST) (LIST (CONS (CDAAR RESTLIST)
     (CDAR RESTLIST))))
           (CDR RESTLIST)))))))
(LIST (CONS (READ) 0))))
```

Both versions are very difficult to read.
With our modern insight (and text editors that automatically indent), a much simpler program is possible:

```lisp
(defun atomprint (exp &optional (depth 0))
  "Print each atom in exp, along with its depth of nesting."
  (if (atom exp)
      (format t "~&ATOM: ~a, DEPTH ~d" exp depth)
      (dolist (element exp)
        (atomprint element (+ depth 1)))))
```

## 1.11 Exercises

&#9635; **Exercise  1.1 [m]** Define a version of `last-name` that handles "Rex Morgan MD," "Morton Downey, Jr.," and whatever other cases you can think of.

&#9635; **Exercise  1.2 [m]** Write a function to exponentiate, or raise a number to an integer power.
For example: `(power 3 2)` = 3<sup>2</sup> = 9.

&#9635; **Exercise  1.3 [m]** Write a function that counts the number of atoms in an expression.
For example: `(count-atoms '(a (b) c)) = 3`.
Notice that there is something of an ambiguity in this: should (`a nil c`) count as three atoms, or as two, because it is equivalent to (`a () c`)?

&#9635; **Exercise  1.4 [m]** Write a function that counts the number of times an expression occurs anywhere within another expression.
Example: `(count-anywhere 'a '(a ((a) b) a)) => 3`.

&#9635; **Exercise  1.5 [m]** Write a function to compute the dot product of two sequences of numbers, represented as lists.
The dot product is computed by multiplying corresponding elements and then adding up the resulting products.
Example:

```lisp
(dot-product '(10 20) '(3 4)) = 10 x 3 + 20 x 4 = 110
```

## 1.12 Answers

### Answer 1.2
```lisp
(defun power (x n)
  "Power raises x to the nth power.  N must be an integer >= 0.
   This executes in log n time, because of the check for even n."
  (cond ((= n 0) 1)
        ((evenp n) (expt (power x (/ n 2)) 2))
        (t (* x (power x (- n 1))))))
```

### Answer 1.3

```lisp
(defun count-atoms (exp)
  "Return the total number of non-nil atoms in the expression."
  (cond ((null exp) 0)
        ((atom exp) 1)
        (t (+ (count-atoms (first exp))
              (count-atoms (rest exp))))))

(defun count-all-atoms (exp &optional (if-null 1))
  "Return the total number of atoms in the expression,
  counting nil as an atom only in non-tail position."
  (cond ((null exp) if-null)
        ((atom exp) 1)
        (t (+ (count-all-atoms (first exp) 1)
              (count-all-atoms (rest exp) 0)))))
```

### Answer 1.4

```lisp
(defun count-anywhere (item tree)
  "Count the times item appears anywhere within tree."
  (cond ((eql item tree) 1)
        ((atom tree) 0)
        (t (+ (count-anywhere item (first tree))
              (count-anywhere item (rest tree))))))
```

### Answer 1.5
Here are three versions:


```lisp
(defun dot-product (a b)
  "Compute the mathematical dot product of two vectors."
  (if (or (null a) (null b))
      0
      (+ (* (first a) (first b))
         (dot-product (rest a) (rest b)))))

(defun dot-product (a b)
  "Compute the mathematical dot product of two vectors."
  (let ((sum 0))
    (dotimes (i (length a))
      (incf sum (* (elt a i) (elt b i))))
    sum))

(defun dot-product (a b)
  "Compute the mathematical dot product of two vectors."
  (apply #'+ (mapcar #'* a b)))
```

----------------------

<a id="fn01-1"></a><sup>[1](#tfn01-1)</sup>
This list of symbols is not a legal Lisp assignment statement, but it is a Lisp data object.

<a id="fn01-2"></a><sup>[2](#tfn01-2)</sup>
The variable `*print-case*` controls how symbols will be printed.
By default, the value of this variable is `:upcase`, but it can be changed to `:downcase` or `:capitalize`.

<a id="fn01-3"></a><sup>[3](#tfn01-3)</sup>
Later we will see what happens when the second argument is not a list.

<a id="fn01-4"></a><sup>[4](#tfn01-4)</sup>
In ANSI Common Lisp, `last` is defined to return a list of the last *n* elements, where n defaults to 1.
Thus `(last p) = (last p 1) = (PUBLIC)`,and `(last p 2) = (Q PUBLIC)`.
This may make the definition of `last` seem less perverse.

<a id="fn01-5"></a><sup>[5](#tfn01-5)</sup>
Just as we can change the value of a variable, we can also change the value of a function in Lisp.
It is not necessary to recompile everything when a change is made, as it would be in other languages.

<a id="fn01-6"></a><sup>[6](#tfn01-6)</sup>
For example, symbols that denote so-called *special* variables usually begin and end in asterisks.
Also, note that I did not hesitate to use the symbol `won!` on page 11.

<a id="fn01-7"></a><sup>[7](#tfn01-7)</sup>
Actually, there can be several symbol tables.
They are known as *packages* in Common Lisp.

<a id="fn01-8"></a><sup>[8](#tfn01-8)</sup>
Pronounced "see-loss." An alternate pronunciation, "klaus," seems to be losing favor.
