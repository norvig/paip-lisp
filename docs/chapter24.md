# Chapter 24
## ANSI Common Lisp

This chapter briefly covers some advanced features of Common Lisp that were not used in the rest of the book.
The first topic, packages, is crucial in building large systems but was not covered in this book, since the programs are concise.
The next four topics–error handling, pretty printing, series, and the loop macro–are covered in *Common Lisp the Language,* 2d edition, but not in the first edition of the book.
Thus, they may not be applicable to your Lisp compiler.
The final topic, sequence functions, shows how to write efficient functions that work for either lists or vectors.

## [ ](#){:#st0010}24.1 Packages
{:#s0010}
{:.h1hd}

A *package* is a symbol table that maps from strings to symbols named by those strings.
When read is confronted with a sequence of characters like `list`, it uses the symbol table to determine that this refers to the symbol `list`.
The important point is that every use of the symbol name `list` refers to the same symbol.
That makes it easy to refer to predefined symbols, but it also makes it easy to introduce unintended name conflicts.
For example, if I wanted to hook up the `emycin` expert system from [chapter 16](B9780080571157500169.xhtml) with the parser from [chapter 19](B9780080571157500194.xhtml), there would be a conflict because both programs use the symbol `defrule` to mean different things.

Common Lisp uses the package system to help resolve such conflicts.
Instead of a single symbol table, Common Lisp allows any number of packages.
The function `read` always uses the current package, which is defined to be the value of the special variable `*package*`.
By default, Lisp starts out in the `common-lisp-user` package.[1](#fn0010){:#xfn0010} That means that if we type a new symbol, like `zxv®!?+qw`, it will be entered into that package.
Converting a string to a symbol and placing it in a package is called *interning.* It is done automatically by `read`, and can be done by the function `intern` if necessary.
Name conflicts arise when there is contention for names within the `common-lisp-user` package.

To avoid name conflicts, simply create your new symbols in another package, one that is specific to your program.
The easiest way to implement this is to split each system into at least two files–one to define the package that the system resides in, and the others for the system itself.
For example, the `emycin` system should start with a file that defines the `emycin` package.
The following form defines the `emycin` package to use the `lisp` package.
That means that when the current package is `emycin`, you can still refer to ail the built-in Lisp symbols.

[ ](#){:#l0010}`(make-package "EMYCIN" :use '("LISP"))`
!!!(p) {:.unnumlist}

The file containing the package definition should always be loaded before the rest of the system.
Those files should start with the following call, which insures that all new symbols will be interned in the `emycin` package:

[ ](#){:#l0015}`(in-package "EMYCIN")`
!!!(p) {:.unnumlist}

Packages are used for information-hiding purposes as well as for avoiding name clashes.
A distinction is made between *internal* and *external* symbols.
External symbols are those that a user of a system would want to refer to, while internal symbols are those that help implement the system but are not needed by a user of the system.
The symbol `rule` would probably be internai to both the `emycin` and `parser` package, but `defrule` would be external, because a user of the `emycin` system uses `defrule` to define new rules.
The designer of a system is responsible for advertising which symbols are external.
The proper call is:

[ ](#){:#l0020}`(export '(emycin defrule defcontext defparm yes/no yes no is))`
!!!(p) {:.unnumlist}

Now the user who wants to refer to symbols in the `emycin` package has four choices.
First, he or she can use the *package prefix* notation.
To refer to the symbol `defrule` in the emycin package, type `emycin:defrule`.
Second, the user can make `emycin` be the current package with `(in-package "EMYCIN").` Then, of course, we need only type `defrule`.
Third, if we only need part of the functionality of a system, we can import specific symbols into the current package.
For example, we could call `(import ' emycin:defrule)`.
From then on, typing `defrule` (in the current package) will refer to `emycin:defrule`.
Fourth, if we want the full functionality of the system, wecall `(use-package "EMYCIN")`.
This makes ail the external symbols ofthe `emycin` package accessible in the current package.

While packages help eliminate name conflicts, `import` and `use-package` allow them to reappear.
The advantage is that there will only be conflicts between external symbols.
Since a carefully designed package should have far fewer external than internal symbols, the problem has at least been reduced.
But if two packages both have an external `defrule` symbol, then we cannot `use-package` both these packages, nor `import` both symbols without producing a genuine name conflict.
Such conflicts can be resolved by *shadowing* one symbol or the other; see *Common Lisp the Language* for details.

The careful reader may be confused by the distinction between `"EMYCIN"` and `emycin`.
In *Common Lisp the Language*, it was not made clear what the argument to package functions must be.
Thus, some implementations signal an error when given a symbol whose print name is a package.
In ANSI Common Lisp, all package functions are specified to take either a package, a package name (a string), or a symbol whose print name is a package name.
In addition, ANSI Common Lisp adds the convenient `defpackage` macro.
It can be used as a replacement for separate calls to `make-package, use-package, import`, and `export`.
Also note that ANSI renames the `lisp package` as `common-lisp`.

[ ](#){:#l0025}`(defpackage emycin`
!!!(p) {:.unnumlist}

 `(:use common-lisp)`
!!!(p) {:.unnumlist}

 `(:export emycin defrule defcontext defparm yes/no yes no is))`
!!!(p) {:.unnumlist}

For more on packages and building systems, see [section 25.16](B978008057115750025X.xhtml#s0110) or *Common Lisp the Language.*

### [ ](#){:#st0015}The Seven Name Spaces
{:#s0015}
{:.h2hd}

One important fact to remember about packages is that they deal with symbols, and only indirectly deal with the uses those symbols might have.
For example, you may think of `(export 'parse)` as exporting the function `parse`, but really it is exporting the symbol `parse`, which may happen to have a function definition associated with it.
However, if the symbol is put to another use–perhaps as a variable or a data type–then those uses are made accessible by the `export` statement as well.

Common Lisp has at least seven name spaces.
The two we think of most often are (1) for functions and macros and (2) for variables.
We have seen that Scheme confiates these two name spaces, but Common Lisp keeps them separate, so that in a function application like `(f)` the function/macro name space is consulted for the value of `f`, but in `(+ f)`, f is treated as a variable name.
Those who understand the scope and extent rules of Common Lisp know that (3) special variables form a distinct name space from lexical variables.
So the `f` in `(+ f)` is treated as either a special or lexical variable, depending on if there is an applicable `special` declaration.
There is also a name space (4) for data types.
Even if `f` is defined as a function and/or a variable, it can also be defined as a data type with `defstruct`, `deftype`, or `defclass`.
It can also be defined as (5) a label for `go` statements within a `tagbody` or (6) a block name for `return-from` statements within a `block`.
Finally, symbols inside a quoted expression are treated as constants, and thus form name space (7).
These symbols are often used as keys in user-defined tables, and in a sense each such table defines a new name space.
One example is the *tag* name space, used by catch and `throw`.
Another is the package name space.

It is a good idea to limit each symbol to only one name space.
Common Lisp will not be confused if a symbol is used in multiple ways, but the poor human reader probably will be.

In the following example `f`, can you identify which of the twelve uses of `f` refer to which name spaces?

[ ](#){:#l0030}`(defun f (f)`
!!!(p) {:.unnumlist}

 `(block f`
!!!(p) {:.unnumlist}

  `(tagbody`
!!!(p) {:.unnumlist}

   `f (catch ’f`
!!!(p) {:.unnumlist}

    `(if (typep f ’f)`
!!!(p) {:.unnumlist}

     `(throw ’f (go f)))`
!!!(p) {:.unnumlist}

    `(funcall #’f (get (symbol-value ’f) ’f))))))`
!!!(p) {:.unnumlist}

## [ ](#){:#st0020}24.2 Conditions and Error Handling
{:#s0020}
{:.h1hd}

An extraordinary feature of ANSI Common Lisp is the facility for handling errors.
In most languages it is very difficult for the programmer to arrange to recover from an error.
Although Ada and some implementations of C provide functions for error recovery, they are not generally part of the repertoire of most programmers.
Thus, we find C programs that exit with the ungraceful message `Segmentation violation: core dumped`.

Common Lisp provides one of the most comprehensive and easy-to-use error-handling mechanism of any programming language, which leads to more robust programs.
The process of error handling is divided into two parts: signaling an error, and handling it.

### [ ](#){:#st0025}Signaling Errors
{:#s0025}
{:.h2hd}

An *error* is a condition that the program does not know how to handle.
Since the program does not know what to do, its only recourse is to announce the occurrence of the error, with the hope that some other program or user will know what to do.
This announcement is called *signaling* an error.
An error can be signaled by a Common Lisp built-in function, as when `( / 3 0 )` signals a divide-by-zero error.
Errors can also be signaled explicitly by the programmer, as in a call to `(error "Illegal value.")`.

Actually, it is a bit of a simplification to talk only of *signaling errors.* The precise term is *signaling a condition.* Some conditions, like end-of-file, are not considered errors, but nevertheless they are unusual conditions that must be dealt with.
The condition system in Common Lisp allows for the definition of all kinds of conditions, but we will continue to talk about errors in this brief discussion, since most conditions are in fact error conditions.

### [ ](#){:#st0030}Handling Errors
{:#s0030}
{:.h2hd}

By default, signaling an error invokes the debugger.
In the following example, the >> prompt means that the user is in the debugger rather than at the top level.

[ ](#){:#l0035}`> (/ 3 0)`
!!!(p) {:.unnumlist}

`Error: An attempt was made to divide by zero.`
!!!(p) {:.unnumlist}

`>>`
!!!(p) {:.unnumlist}

ANSI Common Lisp provides ways of changing this default behavior.
Conceptually, this is done by setting up an *error handler* which handles the error in some way.
Error handlers are bound dynamically and are used to process signaled errors.
An error handler is much like a `catch`, and signaling an error is like a `throw`.
In fact, in many systems `catch` and `throw` are implemented with the error-condition system.

The simplest way of handling an error is with the macro `ignore-errors`.
If noerror occurs, `ignore-errors` is just like `progn`.
But if an error does occur, `ignore-errors` will return `nil` as its first value and `t` as its second, to indicate that an error has occurred but without doing anything else:

[ ](#){:#l0040}`> (ignore-errors (/ 3 1))`⇒ `3 NIL`
!!!(p) {:.unnumlist}

`> (ignore-errors (/ 3 0))`⇒ `NIL T`
!!!(p) {:.unnumlist}

`ignore-errors` is a very coarse-grain tool.
In an interactive interpreter, `ignore-errors` can be used to recover from any and all errors in the response to one input and get back to the read-process-print loop for the next input.
If the errors that are ignored are not serious ones, this can be a very effective way of transforming a buggy program into a useful one.

But some errors are too important to ignore.
If the error is running out of memory, then ignoring it will not help.
Instead, we need to find some way of freeing up memory and continuing.

The condition-handling system can be used to handle only certain errors.
The macro `handler-case`, is a convenient way to do this.
Like `case`, its first argument is evaluated and used to determine what to do next.
If no error is signaled, then the value of the expression is returned.
But if an error does occur, the following clauses are searched for one that matches the type of the error.
In the following example, `handler-case` is used to handle division by zero and other arithmetic errors (perhaps floating-point underflow), but it allows all other errors to pass unhandled.

[ ](#){:#l0045}`(defun div (x y)`
!!!(p) {:.unnumlist}

 `(handler-case (/ x y)`
!!!(p) {:.unnumlist}

  `(division-by-zero () most-positive-fixnum)`
!!!(p) {:.unnumlist}

  `(arithmetic-error () 0)))`
!!!(p) {:.unnumlist}

`> (div 8 2)`⇒ `4`
!!!(p) {:.unnumlist}

`> (div 3 0)`⇒ `16777215`
!!!(p) {:.unnumlist}

`> (div 'xyzzy 1)`
!!!(p) {:.unnumlist}

`Error: The value of NUMBER, XYZZY, should be a number`
!!!(p) {:.unnumlist}

Through judicious use of `handler-case`, the programmer can create robust code that reacts well to unexpected situations.
For more details, see chapter 29 of *Common Lisp the Language,* 2d edition.

## [ ](#){:#st0035}24.3 Pretty Printing
{:#s0035}
{:.h1hd}

ANSI Common Lisp adds a facility for user-controlled pretty printing.
In general, *pretty printing* refers to the process of printing complex expressions in a format that uses indentation to improve readability.
The function `pprint` was always available, but before ANSI Common Lisp it was left unspecified, and it could not be extended by the user.
Chapter 27 of *Common Lisp the Language,* 2d edition presents a pretty-printing facility that gives the user fine-grained control over the printing of all types of objects.
In addition, the facility is integrated with the `format` function.

## [ ](#){:#st0040}24.4 Series
{:#s0040}
{:.h1hd}

The functional style of programming with higher-order functions is one of the attractions of Lisp.
The following expression to sum the square roots of the positive numbers in the list `nums` is clear and concise:

[ ](#){:#l0050}`(reduce #'+ (mapcar #'sqrt (find-all-if #'plusp nums)))`
!!!(p) {:.unnumlist}

Unfortunately, it is inefficient: both `find-all-if` and `mapcar` cons up intermediate lists that are not needed in the final sum.
The following two versions using `loop` and `dolist` are efficient but not as pretty:

[ ](#){:#l0055}`;; Using Loop           ;; Using dolist`
!!!(p) {:.unnumlist}

`(loop for num in nums   (let ((sum 0))`
!!!(p) {:.unnumlist}

  `when (plusp num)        (dolist (num nums sum)`
!!!(p) {:.unnumlist}

  `sum (sqrt num))            (when (plusp num)`
!!!(p) {:.unnumlist}

                                                            `(incf sum num))))`
!!!(p) {:.unnumlist}

A compromise between the two approaches is provided by the *series* facility, defined in appendix A of *Common Lisp the Language*, 2d edition.
The example using series would look like:

[ ](#){:#l0060}`(collect-sum (#Msqrt (choose-if #'plusp nums)))`
!!!(p) {:.unnumlist}

This looks very much like the functional version: only the names have been changed.
However, it compiles into efficient iterative code very much like the `dolist` version.

Like pipes (see [section 9.3](B9780080571157500091.xhtml#s0015)), elements of a series are only evaluated when they are needed.
So we can write `(scan-range :from 0)` to indicate the infinite series of integers starting from 0, but if we only use, say, the first five elements of this series, then only the first five elements will be generated.

The series facility offers a convenient and efficient alternative to iterative loops and sequence functions.
Although the series proposai has not yet been adopted as an official part of ANSI Common Lisp, its inclusion in the reference manual has made it increasingly popular.

## [ ](#){:#st0045}24.5 The Loop Macro
{:#s0045}
{:.h1hd}

The original specification of Common Lisp included a simple `loop` macro.
The body of the loop was executed repeatedly, until a `return` was encountered.
ANSI Common Lisp officially introduces a far more complex `loop` macro, one that had been used in ZetaLisp and its predecessors for some time.
This book has occasionally used the complex `loop` in place of alternatives such as `do, dotimes, dolist`, and the mapping functions.

If your Lisp does not include the complex `loop` macro, this chapter gives a definition that will run all the examples in this book, although it does not support all the features of `loop`.
This chapter also serves as an example of a complex macro.
As with any macro, the first thing to do is to look at some macro calls and what they might expand into.
Here are two examples:

[ ](#){:#l0065}`(loop for i from 1 to n do (print (sqrt i))) ≡`
!!!(p) {:.unnumlist}

`(LET* ((I 1)`
!!!(p) {:.unnumlist}

    `(TEMP N))`
!!!(p) {:.unnumlist}

 `(TAGBODY`
!!!(p) {:.unnumlist}

   `LOOP`
!!!(p) {:.unnumlist}

    `(IF (> I TEMP)`
!!!(p) {:.unnumlist}

       `(GO END))`
!!!(p) {:.unnumlist}

    `(PRINT (SQRT I))`
!!!(p) {:.unnumlist}

    `(SETF I (+ I 1))`
!!!(p) {:.unnumlist}

    `(GO LOOP)`
!!!(p) {:.unnumlist}

   `END))`
!!!(p) {:.unnumlist}

`(loop for v in list do (print v)) ≡`
!!!(p) {:.unnumlist}

`(LET* ((IN LIST)`
!!!(p) {:.unnumlist}

    `(V (CAR IN)))`
!!!(p) {:.unnumlist}

   `(TAGBODY`
!!!(p) {:.unnumlist}

   `LOOP`
!!!(p) {:.unnumlist}

    `(IF (NULL IN)`
!!!(p) {:.unnumlist}

       `(GO END))`
!!!(p) {:.unnumlist}

    `(PRINT V)`
!!!(p) {:.unnumlist}

    `(SETF IN (CDR IN))`
!!!(p) {:.unnumlist}

    `(SETF V (CAR IN))`
!!!(p) {:.unnumlist}

    `(GO LOOP)`
!!!(p) {:.unnumlist}

   `END))`
!!!(p) {:.unnumlist}

Each loop initializes some variables, then enters a loop with some exit tests and a body.
So the template is something like:

[ ](#){:#l0070}`(let* (*variables…*)`
!!!(p) {:.unnumlist}

 `(tagbody`
!!!(p) {:.unnumlist}

  `loop`
!!!(p) {:.unnumlist}

   `(if *exit-tests*`
!!!(p) {:.unnumlist}

    `(go end))`
!!!(p) {:.unnumlist}

   *`Body`*
!!!(p) {:.unnumlist}

   `(go loop)`
!!!(p) {:.unnumlist}

  `end))`
!!!(p) {:.unnumlist}

Actually, there's more we might need in the general case.
There may be a prologue that appears before the loop but after the variable initialization, and similarly there may be an epilogue after the loop.
This epilogue may involve returning a value, and since we want to be able to return from the loop in any case, we need to wrap a `block` around it.
So the complete template is:

[ ](#){:#l0075}`(let* (*variables…*)`
!!!(p) {:.unnumlist}

 `(block *name*`
!!!(p) {:.unnumlist}

  *`Prologue`*
!!!(p) {:.unnumlist}

  `(tagbody`
!!!(p) {:.unnumlist}

   `Loop`
!!!(p) {:.unnumlist}

    *`body`*
!!!(p) {:.unnumlist}

    `(go loop)`
!!!(p) {:.unnumlist}

   `end`
!!!(p) {:.unnumlist}

    *`epilogue`*
!!!(p) {:.unnumlist}

    `(return *result*))))`
!!!(p) {:.unnumlist}

To generate this template from the body of a `loop` form, we will employ a structure with fields for each of the parts of the template:

[ ](#){:#l0080}`(defstruct loop`
!!!(p) {:.unnumlist}

  `"A structure to hold parts of a loop as it is built."`
!!!(p) {:.unnumlist}

  `(vars nil) (prologue nil) (body nil) (steps nil)`
!!!(p) {:.unnumlist}

  `(epilogue nil) (result nil) (name nil))`
!!!(p) {:.unnumlist}

Now the `loop` macro needs to do four things: (1) decide if this is a use of the simple, non-keyword `loop` or the complex ANSI `loop`.
If it is the latter, then (2) make an instance of the `loop` structure, (3) process the body of the loop, filling in apprpriate fields of the structure, and (4) place the filled fields into the template.
Here is the `loop` macro:

[ ](#){:#l0085}`(defmacro loop (&rest exps)`
!!!(p) {:.unnumlist}

  `"Supports both ANSI and simple LOOP.`
!!!(p) {:.unnumlist}

  `Warning: Not every loop keyword is supported."`
!!!(p) {:.unnumlist}

  `(if (every #'listp exps)`
!!!(p) {:.unnumlist}

    `;; No keywords implies simple loop:`
!!!(p) {:.unnumlist}

    `’(block nil (tagbody loop ,@exps (go loop)))`
!!!(p) {:.unnumlist}

    `;; otherwise process loop keywords:`
!!!(p) {:.unnumlist}

    `(let ((l (make-loop)))`
!!!(p) {:.unnumlist}

      `(parse-loop-body l exps)`
!!!(p) {:.unnumlist}

      `(fill-loop-template l))))`
!!!(p) {:.unnumlist}

`(defun fill-loop-template (l)`
!!!(p) {:.unnumlist}

  `"Use a loop-structure instance to fill the template."`
!!!(p) {:.unnumlist}

  `'(let* .(nreverse (loop-vars l))`
!!!(p) {:.unnumlist}

    `(block ,(loop-name l)`
!!!(p) {:.unnumlist}

     `,@(nreverse (loop-prologue l)`
!!!(p) {:.unnumlist}

     `(tagbody`
!!!(p) {:.unnumlist}

      `loop`
!!!(p) {:.unnumlist}

        `,@(nreverse (loop-body l))`
!!!(p) {:.unnumlist}

        `,@(nreverse (loop-steps l))`
!!!(p) {:.unnumlist}

        `(go loop)`
!!!(p) {:.unnumlist}

      `end`
!!!(p) {:.unnumlist}

        `,@(nreverse (loop-epilogue l))`
!!!(p) {:.unnumlist}

        `(return ,(loop-result l))))))`
!!!(p) {:.unnumlist}

Most of the work is in writing `parse-loop-body`, which takes a list of expressions and parses them into the proper fields of a loop structure.
It will use the following auxiliary functions:

[ ](#){:#l0090}`(defun add-body (l exp) (push exp (loop-body l)))`
!!!(p) {:.unnumlist}

`(defun add-test (l test)`
!!!(p) {:.unnumlist}

  `"Put in a test for loop termination."`
!!!(p) {:.unnumlist}

  `(push '(if .test (go end)) (loop-body l)))`
!!!(p) {:.unnumlist}

`(defun add-var (l var init &optional (update nil update?))`
!!!(p) {:.unnumlist}

  `"Add a variable, maybe including an update step."`
!!!(p) {:.unnumlist}

  `(unless (assoc var (loop-vars l))`
!!!(p) {:.unnumlist}

    `(push (list var init) (loop-vars l)))`
!!!(p) {:.unnumlist}

  `(when update?`
!!!(p) {:.unnumlist}

    `(push '(setq ,var ,update) (loop-steps l))))`
!!!(p) {:.unnumlist}

There are a number of alternative ways of implementing this kind of processing.
One would be to use special variables: `*prologue*, *body*, *epilogue*`, and so on.
This would mean we wouldn't have to pass around the loop structure `l`, but there would be significant clutter in having seven new special variables.
Another possibility is to use local variables and close the definitions of `loop`, along with the `add-` functions in that local environment:

[ ](#){:#l0095}`(let (body prologue epilogue steps vars name result)`
!!!(p) {:.unnumlist}

  `(defmacro loop …)`
!!!(p) {:.unnumlist}

  `(defun add-body …)`
!!!(p) {:.unnumlist}

  `(defun add-test …)`
!!!(p) {:.unnumlist}

  `(defun add-var …))`
!!!(p) {:.unnumlist}

This is somewhat cleaner style, but some early Common Lisp compilers do not support embedded `defuns`, so I chose to write in a style that I knew would work in all implementations.
Another design choice would be to return multiple values for each of the components and have `parse-loop-body` put them all together.
This is in fact done in one of the Lisp Machine implementations of `loop`, but I think it is a poor decision: seven components are too many to keep track of by positional notation.

### [ ](#){:#st0050}Anatomy of a Loop
{:#s0050}
{:.h2hd}

All this has just been to set up for the real work: parsing the expressions that make up the loop with the function `parse-loop-body`.
Every loop consists of a sequence of clauses, where the syntax of each clause is determined by the first expression of the clause, which should be a known symbol.
These symbols are called *loop keywords,* although they are not in the keyword package.

The loop keywords will be defined in a data-driven fashion.
Every keyword has a function on its property list under the `loop-fn` indicator.
The function takes three arguments: the `loop` structure being built, the very next expression in the loop body, and a list of the remaining expressions after that.
The function is responsible for updating the `loop` structure (usually by making appropriate calls to the `add-` functions) and then returning the unparsed expressions.
The three-argument calling convention is used because many of the keywords only look at one more expression.
So those functions see that expression as their first argument, and they can conveniently return their second argument as the unparsed remainder.
Other functions will want to look more carefully at the second argument, parsing some of it and returning the rest.

The macro `defloop` is provided to add new loop keywords.
This macro enforces the three-argument calling convention.
If the user supplies only two arguments, then a third argument is automatically added and returned as the remainder.
Also, if the user specifies another symbol rather than a list of arguments, this is taken as an alias, and a function is constructed that calls the function for that keyword:

[ ](#){:#l0100}`(defun parse-loop-body (l exps)`
!!!(p) {:.unnumlist}

  `"Parse the exps based on the first exp being a keyword.`
!!!(p) {:.unnumlist}

  `Continue until all the exps are parsed."`
!!!(p) {:.unnumlist}

  `(unless (null exps)`
!!!(p) {:.unnumlist}

    `(parse-loop-body`
!!!(p) {:.unnumlist}

      `l (call-loop-fn l (first exps) (rest exps)))))`
!!!(p) {:.unnumlist}

`(defun call-loop-fn (l key exps)`
!!!(p) {:.unnumlist}

  `"Return the loop parsing function for this keyword."`
!!!(p) {:.unnumlist}

  `(if (and (symbolp key) (get key 'loop-fn))`
!!!(p) {:.unnumlist}

    `(funcall (get key 'loop-fn) l (first exps) (rest exps))`
!!!(p) {:.unnumlist}

    `(error "Unknown loop key: ˜a" key)))`
!!!(p) {:.unnumlist}

`(defmacro defloop (key args &rest body)`
!!!(p) {:.unnumlist}

  `"Define a new LOOP keyword."`
!!!(p) {:.unnumlist}

  `;; If the args do not have a third arg, one is supplied.`
!!!(p) {:.unnumlist}

  `;; Also, we can define an alias with (defloop key other-key)`
!!!(p) {:.unnumlist}

  `'(setf (get ',key 'loop-fn)`
!!!(p) {:.unnumlist}

    `,(cond ((and (symbolp args) (null body))`
!!!(p) {:.unnumlist}

      `'#'(lambda (1 x y)`
!!!(p) {:.unnumlist}

          `(call-loop-fn l '.args (cons x y))))`
!!!(p) {:.unnumlist}

       `((and (listp args) (= (length args) 2))`
!!!(p) {:.unnumlist}

        `'#'(lambda (.@args -exps-) ,@body -exps-))`
!!!(p) {:.unnumlist}

       `(t '#'(lambda .args ,@body)))))`
!!!(p) {:.unnumlist}

Now we are ready to define some `loop` keywords.
Each of the following sections refers to (and implements the loop keywords in) a section of chapter 26 of *Common Lisp the Language*, 2d edition.

### [ ](#){:#st0055}Iteration Control (26.6)
{:#s0055}
{:.h2hd}

Here we define keywords for iterating over elements of a sequence and for stopping the iteration.
The following cases are covered, where uppercase words represent loop keywords:

[ ](#){:#l0105}`(LOOP REPEAT n …)`
!!!(p) {:.unnumlist}

`(LOOP FOR i FROM s TO e BY inc …)`
!!!(p) {:.unnumlist}

`(LOOP FOR v IN l …)`
!!!(p) {:.unnumlist}

`(LOOP FOR v ON l …)`
!!!(p) {:.unnumlist}

`(LOOP FOR v = expr [THEN step] …)`
!!!(p) {:.unnumlist}

The implementation is straightforward, although somewhat tedious for complex keywords like `for`.
Take the simpler keyword, `repeat`.
To handle it, we generate a new variable that will count down the number of times to repeat.
We call `add-var` to add that variable, with its initial value, to the loop structure.
We also give this variable an update expression, which decrements the variable by one each time through the loop.
Then ail we need to do is call `add-test` to insert code that will exit the loop when the variable reaches zero:

[ ](#){:#l0110}`(defloop repeat (l times)`
!!!(p) {:.unnumlist}

  `"(LOOP REPEAT n …) does loop body n times."`
!!!(p) {:.unnumlist}

  `(let ((i (gensym "REPEAT")))`
!!!(p) {:.unnumlist}

    `(add-var l i times '(− ,i 1))`
!!!(p) {:.unnumlist}

    `(add-test l '(<= ,i 0))))`
!!!(p) {:.unnumlist}

The loop keyword `for` is more complicated, but each case can be analyzed in the same way as `repeat`:

[ ](#){:#l0115}`(defloop as for) ;; AS is the same as FOR`
!!!(p) {:.unnumlist}

`(defloop for (l var exps)`
!!!(p) {:.unnumlist}

  `"4 of the 7 cases for FOR are covered here:`
!!!(p) {:.unnumlist}

  `(LOOP FOR i FROM s TO e BY inc …) does arithemtic iteration`
!!!(p) {:.unnumlist}

  `(LOOP FOR v IN l …) iterates for each element of l`
!!!(p) {:.unnumlist}

  `(LOOP FOR v ON l …) iterates for each tail of l`
!!!(p) {:.unnumlist}

  `(LOOP FOR v = expr [THEN step]) initializes and iterates v"`
!!!(p) {:.unnumlist}

  `(let ((key (first exps))`
!!!(p) {:.unnumlist}

      `(source (second exps))`
!!!(p) {:.unnumlist}

      `(rest (rest2 exps)))`
!!!(p) {:.unnumlist}

    `(ecase key`
!!!(p) {:.unnumlist}

      `((from downfrom upfrom to downto upto by)`
!!!(p) {:.unnumlist}

     `(loop-for-arithmetic l var exps))`
!!!(p) {:.unnumlist}

      `(in (let ((v (gensym "IN")))`
!!!(p) {:.unnumlist}

           `(add-var l v source '(cdr ,v))`
!!!(p) {:.unnumlist}

           `(add-var l var '(car ,v) '(car ,v))`
!!!(p) {:.unnumlist}

           `(add-test l '(null ,v))`
!!!(p) {:.unnumlist}

           `rest))`
!!!(p) {:.unnumlist}

      `(on (add-var l var source '(cdr ,var))`
!!!(p) {:.unnumlist}

          `(add-test l '(null .var))`
!!!(p) {:.unnumlist}

          `rest)`
!!!(p) {:.unnumlist}

      `(= (if (eq (first rest) 'then)`
!!!(p) {:.unnumlist}

              `(progn`
!!!(p) {:.unnumlist}

                `(pop rest)`
!!!(p) {:.unnumlist}

                `(add-var l var source (pop rest)))`
!!!(p) {:.unnumlist}

              `(progn`
!!!(p) {:.unnumlist}

                `(add-var l var nil)`
!!!(p) {:.unnumlist}

                `(add-body l '(setq ,var .source))))`
!!!(p) {:.unnumlist}

          `rest)`
!!!(p) {:.unnumlist}

      `;; ACROSS.
BEING clauses omitted`
!!!(p) {:.unnumlist}

      `)))`
!!!(p) {:.unnumlist}

`(defun loop-for-arithmetic (l var exps)`
!!!(p) {:.unnumlist}

  `"Parse loop expressions of the form:`
!!!(p) {:.unnumlist}

  `(LOOP FOR var [FROM | DOWNFROM | UPFROM exp1] [TO | DOWNTO | UPTO exp2]`
!!!(p) {:.unnumlist}

       `[BY exp3]"`
!!!(p) {:.unnumlist}

  `;; The prepositions BELOW and ABOVE are omitted`
!!!(p) {:.unnumlist}

  `(let ((exp1 0)`
!!!(p) {:.unnumlist}

       `(exp2 nil)`
!!!(p) {:.unnumlist}

       `(exp3 1)`
!!!(p) {:.unnumlist}

       `(down?
nil))`
!!!(p) {:.unnumlist}

    `;; Parse the keywords:`
!!!(p) {:.unnumlist}

    `(when (member (first exps) '(from downfrom upfrom))`
!!!(p) {:.unnumlist}

     `(setf exp1 (second exps)`
!!!(p) {:.unnumlist}

         `down?
(eq (first exps) 'downfrom)`
!!!(p) {:.unnumlist}

         `exps (rest2 exps)))`
!!!(p) {:.unnumlist}

    `(when (member (first exps) '(to downto upto))`
!!!(p) {:.unnumlist}

     `(setf exp2 (second exps)`
!!!(p) {:.unnumlist}

         `down?
(or down?
(eq (first exps) 'downto))`
!!!(p) {:.unnumlist}

         `exps (rest2 exps)))`
!!!(p) {:.unnumlist}

    `(when (eq (first exps) 'by)`
!!!(p) {:.unnumlist}

     `(setf exp3 (second exps)`
!!!(p) {:.unnumlist}

         `exps (rest2 exps)))`
!!!(p) {:.unnumlist}

    `;; Add variables and tests:`
!!!(p) {:.unnumlist}

    `(add-var l var exp1`
!!!(p) {:.unnumlist}

         `'(,(if down?
'- '+) ,var ,(maybe-temp l exp3)))`
!!!(p) {:.unnumlist}

    `(when exp2`
!!!(p) {:.unnumlist}

      `(add-test l '(,(if down?
'< '>) ,var ,(maybe-temp l exp2))))`
!!!(p) {:.unnumlist}

    `;; and return the remaining expressions:`
!!!(p) {:.unnumlist}

         `exps))`
!!!(p) {:.unnumlist}

`(defun maybe-temp (l exp)`
!!!(p) {:.unnumlist}

  `"Generate a temporary variable, if needed."`
!!!(p) {:.unnumlist}

  `(if (constantp exp)`
!!!(p) {:.unnumlist}

    `exp`
!!!(p) {:.unnumlist}

    `(let ((temp (gensym "TEMP")))`
!!!(p) {:.unnumlist}

      `(add-var l temp exp)`
!!!(p) {:.unnumlist}

      `temp)))`
!!!(p) {:.unnumlist}

### [ ](#){:#st0060}End-Test Control (26.7)
{:#s0060}
{:.h2hd}

In this section we cover the following clauses:

[ ](#){:#l0120}`(LOOP UNTIL test …)`
!!!(p) {:.unnumlist}

`(LOOP WHILE test …)`
!!!(p) {:.unnumlist}

`(LOOP ALWAYS condition …)`
!!!(p) {:.unnumlist}

`(LOOP NEVER condition …)`
!!!(p) {:.unnumlist}

`(LOOP THEREIS condition …)`
!!!(p) {:.unnumlist}

`(LOOP … (LOOP-FINISH) …)`
!!!(p) {:.unnumlist}

Each keyword is quite simple:

[ ](#){:#l0125}`(defloop until (l test) (add-test l test))`
!!!(p) {:.unnumlist}

`(defloop while (l test) (add-test l ‘(not .test)))`
!!!(p) {:.unnumlist}

`(defloop always (l test)`
!!!(p) {:.unnumlist}

  `(setf (loop-result l) t)`
!!!(p) {:.unnumlist}

  `(add-body l ‘(if (not ,test) (return nil))))`
!!!(p) {:.unnumlist}

`(defloop never (l test)`
!!!(p) {:.unnumlist}

  `(setf (loop-result l) t)`
!!!(p) {:.unnumlist}

  `(add-body l ‘(if ,test (return nil))))`
!!!(p) {:.unnumlist}

`(defloop thereis (l test) (add-body l ‘(return-if ,test)))`
!!!(p) {:.unnumlist}

`(defmacro return-if (test)`
!!!(p) {:.unnumlist}

  `"Return TEST if it is non-nil."`
!!!(p) {:.unnumlist}

  `(once-only (test)`
!!!(p) {:.unnumlist}

    `‘(if ,test (return ,test))))`
!!!(p) {:.unnumlist}

`(defmacro loop-finish () ‘(go end))`
!!!(p) {:.unnumlist}

### [ ](#){:#st0065}Value Accumulation (26.8)
{:#s0065}
{:.h2hd}

The `collect` keyword poses another challenge.
How do you collect a list of expressions presented one at a time?
The answer is to view the expressions as a queue, one where we add items to the rear but never remove them from the front of the queue.
Then we can use the queue functions defined in [section 10.5](B9780080571157500108.xhtml#s0025).

Unlike the other clauses, value accumulation clauses can communicate with each other.
There can be, say, two `collect` and an append clause in the same loop, and they all build onto the same list.
Because of this, I use the same variable name for the accumulator, rather than gensyming a new variable for each use.
The name chosen is stored in the global variable `*acc*`.
In the official `loop` standard it is possible for the user to specify the variable with an `into` modifier, but I have not implemented that option.
The clauses covered are:

[ ](#){:#l0130}`(LOOP COLLECT item …)`
!!!(p) {:.unnumlist}

`(LOOP NCONC item …)`
!!!(p) {:.unnumlist}

`(LOOP APPEND item …)`
!!!(p) {:.unnumlist}

`(LOOP COUNT item …)`
!!!(p) {:.unnumlist}

`(LOOP SUM item …)`
!!!(p) {:.unnumlist}

`(LOOP MAXIMIZE item …)`
!!!(p) {:.unnumlist}

`(LOOP MINIMIZE item …)`
!!!(p) {:.unnumlist}

The implementation is:

[ ](#){:#l0135}`(defconstant *acc* (gensym "ACC")`
!!!(p) {:.unnumlist}

  `"Variable used for value accumulation in LOOP.")`
!!!(p) {:.unnumlist}

`;;; INTO preposition is omitted`
!!!(p) {:.unnumlist}

`(defloop collect (l exp)`
!!!(p) {:.unnumlist}

  `(add-var l *acc* '(make-queue))`
!!!(p) {:.unnumlist}

  `(add-body l '(enqueue ,exp .*acc*))`
!!!(p) {:.unnumlist}

  `(setf (loop-result l) ‘(queue-contents ,*acc*)))`
!!!(p) {:.unnumlist}

`(defloop nconc (l exp)`
!!!(p) {:.unnumlist}

  `(add-var l *acc* '(make-queue))`
!!!(p) {:.unnumlist}

  `(add-body l '(queue-nconc ,*acc* .exp))`
!!!(p) {:.unnumlist}

  `(setf (loop-result l) '(queue-contents .*acc*)))`
!!!(p) {:.unnumlist}

`(defloop append (l exp exps)`
!!!(p) {:.unnumlist}

  `(call-loop-fn l 'nconc '((copy-list .exp) .,exps)))`
!!!(p) {:.unnumlist}

`(defloop count (l exp)`
!!!(p) {:.unnumlist}

  `(add-var l *acc* 0)`
!!!(p) {:.unnumlist}

  `(add-body l '(when .exp (incf .*acc*)))`
!!!(p) {:.unnumlist}

  `(setf (loop-result l) *acc*))`
!!!(p) {:.unnumlist}

`(defloop sum (l exp)`
!!!(p) {:.unnumlist}

  `(add-var l *acc* 0)`
!!!(p) {:.unnumlist}

  `(add-body l '(incf ,*acc* .exp))`
!!!(p) {:.unnumlist}

  `(setf (loop-result l) *acc*))`
!!!(p) {:.unnumlist}

`(defloop maximize (l exp)`
!!!(p) {:.unnumlist}

  `(add-var l *acc* nil)`
!!!(p) {:.unnumlist}

  `(add-body l '(setf ,*acc*`
!!!(p) {:.unnumlist}

        `(if ,*acc*`
!!!(p) {:.unnumlist}

            `(max ,*acc* ,exp)`
!!!(p) {:.unnumlist}

            `,exp)))`
!!!(p) {:.unnumlist}

  `(setf (loop-result l) *acc*))`
!!!(p) {:.unnumlist}

`(defloop minimize (l exp)`
!!!(p) {:.unnumlist}

  `(add-var 1 *acc* nil)`
!!!(p) {:.unnumlist}

  `(add-body l '(setf ,*acc*`
!!!(p) {:.unnumlist}

        `(if ,*acc*`
!!!(p) {:.unnumlist}

            `(min ,*acc* ,exp)`
!!!(p) {:.unnumlist}

            `,exp)))`
!!!(p) {:.unnumlist}

  `(setf (loop-result l) *acc*))`
!!!(p) {:.unnumlist}

`(defloop collecting collect)`
!!!(p) {:.unnumlist}

`(defloop nconcing nconc)`
!!!(p) {:.unnumlist}

`(defloop appending append)`
!!!(p) {:.unnumlist}

`(defloop counting count)`
!!!(p) {:.unnumlist}

`(defloop summing sum)`
!!!(p) {:.unnumlist}

`(defloop maximizing maximize)`
!!!(p) {:.unnumlist}

`(defloop minimizing minimize)`
!!!(p) {:.unnumlist}

**Exercise 24.1**`loop` lets us build aggregates (lists, maximums, sums, etc.) over the body of the loop.
Sometimes it is inconvenient to be restricted to a single-loop body.
For example, we might want a list of all the nonzero elements of a two-dimensional array.
One way to implement this is with a macro, `with-collection`, that sets up and returns a queue structure that is built by calls to the function `collect`.
For example:

[ ](#){:#l0140}`> (let ((A ’#2a((l 0 0) (0 2 4) (0 0 3))))`
!!!(p) {:.unnumlist}

  `(with-collection`
!!!(p) {:.unnumlist}

    `(loop for i from 0 to 2 do`
!!!(p) {:.unnumlist}

      `(loop for j from 0 to 2 do`
!!!(p) {:.unnumlist}

        `(if (> (aref a i j) 0)`
!!!(p) {:.unnumlist}

          `(collect (aref A i j)))))))`
!!!(p) {:.unnumlist}

`(1 2 4 3)`
!!!(p) {:.unnumlist}

Implement `with-collection` and `collect`.

### [ ](#){:#st0070}Variable Initialization (26.9)
{:#s0070}
{:.h2hd}

The `with` clause allows local variables–I have included it, but recommend using a `let` instead.
I have not included the `and` preposition, which allows the variables to nest at different levels.

[ ](#){:#l0145}`;;;; 26.9.
Variable Initializations ("and" omitted)`
!!!(p) {:.unnumlist}

`(defloop with (l var exps)`
!!!(p) {:.unnumlist}

  `(let ((init nil))`
!!!(p) {:.unnumlist}

    `(when (eq (first exps) '=)`
!!!(p) {:.unnumlist}

      `(setf init (second exps)`
!!!(p) {:.unnumlist}

        `exps (rest2 exps)))`
!!!(p) {:.unnumlist}

    `(add-var l var init)`
!!!(p) {:.unnumlist}

    `exps))`
!!!(p) {:.unnumlist}

### [ ](#){:#st0075}Conditional Execution (26.10)
{:#s0075}
{:.h2hd}

`loop` also provides forms for conditional execution.
These should be avoided whenever possible, as Lisp already has a set of perfectly good conditional macros.
However, sometimes you want to make, say, a `collect` conditional on some test.
In that case, loop conditionals are acceptable.
The clauses covered here are:

[ ](#){:#l0150}(`LOOP WHEN test … CELSE …]) ; IF` is asynonym for `WHEN`
!!!(p) {:.unnumlist}

`(LOOP UNLESS test … [ELSE …])`
!!!(p) {:.unnumlist}

Here is an example of `when`:

[ ](#){:#l0155}`> (loop for`× `from 1 to 10`
!!!(p) {:.unnumlist}

     `when (oddp x)`
!!!(p) {:.unnumlist}

         `collect x`
!!!(p) {:.unnumlist}

     `else collect (− x))`
!!!(p) {:.unnumlist}

`(1 -2 3 -4 5- 6 7 -8 9 -10)`
!!!(p) {:.unnumlist}

Of course, we could have said `collect (if (oddp x ) x ( − x ) )` and done without the conditional.
There is one extra feature in loop's conditionals: the value of the test is stored in the variable it for subsequent use in the THEN or ELSE parts.
(This is just the kind of feature that makes some people love `loop` and others throw up their hands in despair.) Here is an example:

[ ](#){:#l0160}`> (loop for x from 1 to 10`
!!!(p) {:.unnumlist}

    `when (second (assoc x '((l one) (3 three) (5 five))))`
!!!(p) {:.unnumlist}

    `collect it)`
!!!(p) {:.unnumlist}

`(ONE THREE FIVE)`
!!!(p) {:.unnumlist}

The conditional clauses are a little tricky to implement, since they involve parsing other clauses.
The idea is that `call-loop-fn` parses the THEN and ELSE parts, adding whatever is necessary to the body and to other parts of the loop structure.
Then `add-body` is used to add labels and go statements that branch to the labels as needed.
This is the same technique that is used to compile conditionals in [chapter 23](B9780080571157500236.xhtml); see the function `comp-if` on [page 787](B9780080571157500236.xhtml#p787).
Here is the code:

[ ](#){:#l0165}`(defloop when (l test exps)`
!!!(p) {:.unnumlist}

  `(loop-unless l '(not ,(maybe-set-it test exps)) exps))`
!!!(p) {:.unnumlist}

`(defloop unless (l test exps)`
!!!(p) {:.unnumlist}

  `(loop-unless l (maybe-set-it test exps) exps))`
!!!(p) {:.unnumlist}

`(defun maybe-set-it (test exps)`
!!!(p) {:.unnumlist}

  `"Return value, but if the variable IT appears in exps,`
!!!(p) {:.unnumlist}

  `then return code that sets IT to value."`
!!!(p) {:.unnumlist}

  `(if (find-anywhere 'it exps)`
!!!(p) {:.unnumlist}

    `'(setq it .test)`
!!!(p) {:.unnumlist}

    `test))`
!!!(p) {:.unnumlist}

`(defloop if when)`
!!!(p) {:.unnumlist}

`(defun loop-unless (l test exps)`
!!!(p) {:.unnumlist}

  `(let ((label (gensym "L")))`
!!!(p) {:.unnumlist}

    `(add-var l 'it nil )`
!!!(p) {:.unnumlist}

    `;; Emit code for the test and the THEN part`
!!!(p) {:.unnumlist}

    `(add-body l '(if .test (go ,label)))`
!!!(p) {:.unnumlist}

    `(setf exps (call-loop-fn l (first exps) (rest exps)))`
!!!(p) {:.unnumlist}

    `;; Optionally emit code for the ELSE part`
!!!(p) {:.unnumlist}

    `(if (eq (first exps) 'else)`
!!!(p) {:.unnumlist}

      `(progn`
!!!(p) {:.unnumlist}

        `(let ((label2 (gensym "L")))`
!!!(p) {:.unnumlist}

          `(add-body l '(go ,label2))`
!!!(p) {:.unnumlist}

          `(add-body l label)`
!!!(p) {:.unnumlist}

          `(setf exps (call-loop-fn l (second exps) (rest2 exps)))`
!!!(p) {:.unnumlist}

          `(add-body l label2)))`
!!!(p) {:.unnumlist}

        `(add-body l label)))`
!!!(p) {:.unnumlist}

  `exps)`
!!!(p) {:.unnumlist}

### [ ](#){:#st0080}Unconditional Execution (26.11)
{:#s0080}
{:.h2hd}

The unconditional execution keywords are do and return:

[ ](#){:#l0170}`(defloop do (l exp exps)`
!!!(p) {:.unnumlist}

  `(add-body l exp)`
!!!(p) {:.unnumlist}

  `(loop (if (symbolp (first exps)) (RETURN exps))`
!!!(p) {:.unnumlist}

    `(add-body l (pop exps))))`
!!!(p) {:.unnumlist}

`(defloop return (l exp) (add-body l '(return ,exp)))`
!!!(p) {:.unnumlist}

### [ ](#){:#st0085}Miscellaneous Features (26.12)
{:#s0085}
{:.h2hd}

Finally, the miscellaneous features include the keywords `initially` and `finally`, which define the loop prologue and epilogue, and the keyword named, which gives a name to the loop for use by a `return-from` form.
I have omitted the data-type declarations and destructuring capabilities.

[ ](#){:#l0175}`(defloop initially (l exp exps)`
!!!(p) {:.unnumlist}

  `(push exp (loop-prologue l))`
!!!(p) {:.unnumlist}

  `(loop (if (symbolp (first exps)) (RETURN exps))`
!!!(p) {:.unnumlist}

    `(push (pop exps) (loop-prologue l))))`
!!!(p) {:.unnumlist}

`(defloop finally (l exp exps)`
!!!(p) {:.unnumlist}

  `(push exp (loop-epilogue l))`
!!!(p) {:.unnumlist}

  `(loop (if (symbolp (first exps)) (RETURN exps))`
!!!(p) {:.unnumlist}

    `(push (pop exps) (loop-epilogue l))))`
!!!(p) {:.unnumlist}

`(defloop named (l exp) (setf (loop-name l) exp))`
!!!(p) {:.unnumlist}

## [ ](#){:#st0090}24.6 Sequence Functions
{:#s0090}
{:.h1hd}

Common Lisp provides sequence functions to make the programmer’s life easier: the same function can be used for lists, vectors, and strings.
However, this ease of use comes at a cost.
Sequence functions must be written very carefully to make sure they are efficient.
There are three main sources of indeterminacy that can lead to inefficiency: (1) the sequences can be of different types; (2) some functions have keyword arguments; (3) some functions have a `&rest` argument.
Careful coding can limit or eliminate these sources of inefficiency, by making as many choices as possible at compile time and making the remaining choices outside of the main loop.

In this section we see how to implement the new ANSI sequence function `map-into` and the updated function reduce efficiently.
This is essential for those without an ANSI compiler.
Even those who do have access to an ANSI compiler will benefit from seeing the efficiency techniques used here.

Before defining the sequence functions, the macro `once-only` is introduced.

### [ ](#){:#st0095}Once-only: A Lesson in Macrology
{:#s0095}
{:.h2hd}

The macro `once-only` has been around for a long time on various systems, although it didn’t make it into the Common Lisp standard.
I include it here for two reasons: first, it is used in the following `funcall-if` macro, and second, if you can understand how to write and when to use `once-only`, then you truly understand macro.

First, you have to understand the problem that `once-only` addresses.
Suppose we wanted to have a macro that multiplies its input by itself:[2](#fn0015){:#xfn0015}

[ ](#){:#l0180}`(defmacro square (x) '(* ,x ,x))`
!!!(p) {:.unnumlist}

This definition works fine in the following case:

[ ](#){:#l0185}`> (macroexpand ’(square z)) => (* Z Z)`
!!!(p) {:.unnumlist}

But it doesn't work as well here:

[ ](#){:#l0190}`> (macroexpand ’(square (print (incf i))))`
!!!(p) {:.unnumlist}

`(* (PRINT (INCF I)) (PRINT (INCF I)))`
!!!(p) {:.unnumlist}

The problem is that `i` will get incremented twice, not once, and two different values will get printed, not one.
We need to bind `(print (incf i))` to a local variable before doing the multiplication.
On the other hand, it would be superfluous to bind z to a local variable in the previous example.
This is where `once-only` comes in.
It allows us to write macro definitions like this:

[ ](#){:#l0195}`(defmacro square (x) (once-only (x) '(* ,x ,x)))`
!!!(p) {:.unnumlist}

and have the generated code be just what we want:

[ ](#){:#l0200}`> (macroexpand ’(square z))`
!!!(p) {:.unnumlist}

`(* Z Z)`
!!!(p) {:.unnumlist}

`> (macroexpand ’(square (print (incf i))))`
!!!(p) {:.unnumlist}

`(LET ((G3811 (PRINT (INCF I))))`
!!!(p) {:.unnumlist}

  `(* G3811 G3811))`
!!!(p) {:.unnumlist}

You have now learned lesson number one of `once-only` : you know how macros differ from functions when it comes to arguments with side effects, and you now know how to handle this.
Lesson number two comes when you try to write (or even understand) a definition of `once-only`–only when you truly understand the nature of macros will you be able to write a correct version.
As always, the first thing to determine is what a call to `once-only` should expand into.
The generated code should test the variable to see if it is free of side effects, and if so, generate the body as is; otherwise it should generate code to bind a new variable, and use that variable in the body of the code.
Here's roughly what we want:

[ ](#){:#l0205}`> (macroexpand ’(once-only (x) ‘(* ,x ,x)))`
!!!(p) {:.unnumlist}

`(if (side-effect-free-p x)`
!!!(p) {:.unnumlist}

  `‘(* ,x ,x)`
!!!(p) {:.unnumlist}

  `‘(let ((g00l ,x))`
!!!(p) {:.unnumlist}

    `, (let ((x ’g00l))`
!!!(p) {:.unnumlist}

      `‘(* x ,x))))`
!!!(p) {:.unnumlist}

where `g001` is a new symbol, to avoid conflicts with the `x` or with symbols in the body.
Normally, we generate macro bodies using backquotes, but if the macro body itself has a backquote, then what?
It is possible to nest backquotes (and [appendix C](B9780080571157500273.xhtml) of *Common Lisp the Language*, 2d edition has a nice discussion of doubly and triply nested backquotes), but it certainly is not trivial to understand.
I recommend replacing the inner backquote with its equivalent using `list` and `quote`:

[ ](#){:#l0210}`(if (side-effect-free-p x)`
!!!(p) {:.unnumlist}

  `‘(* ,x ,x)`
!!!(p) {:.unnumlist}

  `(list ’let (list (list ’g00l x))`
!!!(p) {:.unnumlist}

    `(let ((x ’g00l))`
!!!(p) {:.unnumlist}

      `‘(* ,x ,x))))`
!!!(p) {:.unnumlist}

Now we can write `once-only`.
Note that we have to account for the case where there is more than one variable and where there is more than one expression in the body.

[ ](#){:#l0215}`(defmacro once-only (variables &rest body)`
!!!(p) {:.unnumlist}

  `"Returns the code built by BODY.
If any of VARIABLES`
!!!(p) {:.unnumlist}

  `might have side effects.
they are evaluated once and stored`
!!!(p) {:.unnumlist}

  `in temporary variables that are then passed to BODY."`
!!!(p) {:.unnumlist}

  `(assert (every #’symbolp variables))`
!!!(p) {:.unnumlist}

  `(let ((temps (loop repeat (length variables) collect (gensym))))`
!!!(p) {:.unnumlist}

    `‘(if (every #'side-effect-free-p (list .,variables))`
!!!(p) {:.unnumlist}

      `(progn .,body)`
!!!(p) {:.unnumlist}

      `(list ’let`
!!!(p) {:.unnumlist}

        `,‘(list .@(mapcar #’(lambda (tmp var)`
!!!(p) {:.unnumlist}

          `‘(list '.tmp .var))`
!!!(p) {:.unnumlist}

        `temps variables))`
!!!(p) {:.unnumlist}

         `(let .(mapcar #'(lambda (var tmp) ‘(.var ’,tmp))`
!!!(p) {:.unnumlist}

      `variables temps)`
!!!(p) {:.unnumlist}

     `.,body)))))`
!!!(p) {:.unnumlist}

`(defun side-effect-free-p (exp)`
!!!(p) {:.unnumlist}

  `"Is exp a constant, variable, or function,`
!!!(p) {:.unnumlist}

  `or of the form (THE type x) where x is side-effect-free?"`
!!!(p) {:.unnumlist}

  `(or (constantp exp) (atom exp) (starts-with exp ’function)`
!!!(p) {:.unnumlist}

    `(and (starts-with exp ’the)`
!!!(p) {:.unnumlist}

      `(side-effect-free-p (third exp)))))`
!!!(p) {:.unnumlist}

Here we see the expansion of the call to `once-only` and a repeat of the expansions of two calls to `square`:

[ ](#){:#l0220}`> (macroexpand ’(once-only (x) ‘(* ,x ,x)))`
!!!(p) {:.unnumlist}

`(IF (EVERY #’SIDE-EFFECT-FREE-P (LIST X))`
!!!(p) {:.unnumlist}

    `(PROGN`
!!!(p) {:.unnumlist}

      `‘(* ,X ,X))`
!!!(p) {:.unnumlist}

    `(LIST ’LET (LIST (LIST ’G3763 X))`
!!!(p) {:.unnumlist}

          `(LET ((X ’G3763))`
!!!(p) {:.unnumlist}

            `‘(* ,X ,X))))`
!!!(p) {:.unnumlist}

`> (macroexpand ’(square z))`
!!!(p) {:.unnumlist}

`(* Z Z)`
!!!(p) {:.unnumlist}

`> (macroexpand ’(square (print (incf i))))`
!!!(p) {:.unnumlist}

`(LET ((G3811 (PRINT (INCF I))))`
!!!(p) {:.unnumlist}

  `(* G3811 G3811))`
!!!(p) {:.unnumlist}

This output was produced with `*print-gensym*` setto `nil`.
When this variable is non-nil, uninterned symbols are printed with a prefix `#`:,as in `#:G3811`.
This insures that the symbol will not be interned by a subsequent read.

It is worth noting that Common Lisp automatically handles problems related to multiple evaluation of subforms in setf methods.
See [page 884](B978008057115750025X.xhtml#p884) for an example.

### [ ](#){:#st0100}Avoid Overusing Macros
{:#s0100}
{:.h2hd}

A word to the wise: don't get carried away with macros.
Use macros freely to represent your *problem*, but shy away from new macros in the implementation of your *solution,* unless absolutely necessary.
So, it is good style to introduce a macro, say, `defrule`, which defines rules for your application, but adding macros to the code itself may just make things harder for others to use.

Here is a story.
Before `if` was a standard part of Lisp, I defined my own version of `if`.
Unlike the simple `if`, my version took any number of test/result pairs, followed by an optional else result.
In general, the expansion was:

[ ](#){:#l0225}`(if *a b c d…x)* => (cond *(a b)* (*c d*) … (T *x*))`
!!!(p) {:.unnumlist}

My `if` also had one more feature: the symbol `‘that’` could be used to refer to the value of the most recent test.
For example, I could write:

[ ](#){:#l0230}`(if (assoc item a-list)`
!!!(p) {:.unnumlist}

  `(process (cdr that)))`
!!!(p) {:.unnumlist}

which would expand into:

[ ](#){:#l0235}`(LET (THAT)`
!!!(p) {:.unnumlist}

  `(COND`
!!!(p) {:.unnumlist}

    `((SETQ THAT (ASSOC ITEM A-LIST)) (PROCESS (CDR THAT)))))`
!!!(p) {:.unnumlist}

This was a convenient feature (compare it to the => feature of Scheme's cond, as discussed on [page 778](B9780080571157500224.xhtml#p778)), but it backfired often enough that I eventually gave up on my version of `if`.
Here's why.
I would write code like this:

[ ](#){:#l0240}`(if (total-score x)`
!!!(p) {:.unnumlist}

  `(print (/ that number-of-trials))`
!!!(p) {:.unnumlist}

  `(error "No scores"))`
!!!(p) {:.unnumlist}

and then make a small change:

[ ](#){:#l0245}`(if (total-score x)`
!!!(p) {:.unnumlist}

  `(if *print-scores* (print (/ that number-of-trials)))`
!!!(p) {:.unnumlist}

  `(error "No scores"))`
!!!(p) {:.unnumlist}

The problem is that the variable `that` now refers to `*print-scores*`, not `(total-score x),` as it did before.
My macro violates referential transparency.
In general, that's the whole point of macros, and it is why macros are sometimes convenient.
But in this case, violating referential transparency can lead to confusion.

### [ ](#){:#st0105}MAP-INTO
{:#s0105}
{:.h2hd}

The function `map-into` is used on [page 632](B9780080571157500182.xhtml#p632).
This function, added for the ANSI version of Common Lisp, is like `map`, except that instead of building a new sequence, the first argument is changed to hold the results.
This section describes how to write a fairly efficient version of `map-into`, using techniques that are applicable to any sequence function.
We'll start with a simple version:

[ ](#){:#l0250}`(defun map-into (result-sequence function &rest sequences)`
!!!(p) {:.unnumlist}

  `"Destructively set elements of RESULT-SEQUENCE to the results`
!!!(p) {:.unnumlist}

  `of applying FUNCTION to respective elements of SEQUENCES."`
!!!(p) {:.unnumlist}

  `(replace result-sequence (apply #'map 'list function sequences)))`
!!!(p) {:.unnumlist}

This does the job, but it defeats the purpose of `map-into`, which is to avoid generating garbage.
Here's a version that generates less garbage:

[ ](#){:#l0255}`(defun map-into (result-sequence function &rest sequences)`
!!!(p) {:.unnumlist}

  `"Destructively set elements of RESULT-SEQUENCE to the results`
!!!(p) {:.unnumlist}

  `of applying FUNCTION to respective elements of SEQUENCES."`
!!!(p) {:.unnumlist}

  `(let ((n (loop for seq in (cons result-sequence sequences)`
!!!(p) {:.unnumlist}

              `minimize (length seq))))`
!!!(p) {:.unnumlist}

    `(dotimes (i n)`
!!!(p) {:.unnumlist}

      `(setf (elt result-sequence i)`
!!!(p) {:.unnumlist}

        `(apply function`
!!!(p) {:.unnumlist}

          `(mapcar #'(lambda (seq) (elt seq i))`
!!!(p) {:.unnumlist}

            `sequences))))))`
!!!(p) {:.unnumlist}

There are three problems with this definition.
First, it wastes space: mapcar creates a new argument list each time, only to have the list be discarded.
Second, it wastes time: doing a `setf` of the ith element of a list makes the algorithm *O*(*n2*) instead of *O*(*n*), where *n* is the length of the list.
Third, it is subtly wrong: if `result-sequence` is a vector with a fill pointer, then `map-into` is supposed to ignore `result-sequence's` current length and extend the fill pointer as needed.
The following version fixes those problems:

[ ](#){:#l0260}`(defun map-into (result-sequence function &rest sequences)`
!!!(p) {:.unnumlist}

  `"Destructively set elements of RESULT-SEQUENCE to the results`
!!!(p) {:.unnumlist}

  `of applying FUNCTION to respective elements of SEQUENCES."`
!!!(p) {:.unnumlist}

  `(let ((arglist (make-list (length sequences)))`
!!!(p) {:.unnumlist}

    `(n (if (listp result-sequence)`
!!!(p) {:.unnumlist}

      `most-positive-fixnum`
!!!(p) {:.unnumlist}

      `(array-dimension result-sequence 0))))`
!!!(p) {:.unnumlist}

   `;; arglist is made into a list of args for each call`
!!!(p) {:.unnumlist}

   `;; n is the length of the longest vector`
!!!(p) {:.unnumlist}

   `(when sequences`
!!!(p) {:.unnumlist}

     `(setf n (min n (loop for seq in sequences`
!!!(p) {:.unnumlist}

       `minimize (length seq)))))`
!!!(p) {:.unnumlist}

   `;; Define some shared functions:`
!!!(p) {:.unnumlist}

   `(flet`
!!!(p) {:.unnumlist}

    `((do-one-call (i)`
!!!(p) {:.unnumlist}

      `(loop for seq on sequences`
!!!(p) {:.unnumlist}

        `for arg on arglist`
!!!(p) {:.unnumlist}

        `do (if (listp (first seq))`
!!!(p) {:.unnumlist}

          `(setf (first arg)`
!!!(p) {:.unnumlist}

            `(pop (first seq)))`
!!!(p) {:.unnumlist}

          `(setf (first arg)`
!!!(p) {:.unnumlist}

            `(aref (first seq) i))))`
!!!(p) {:.unnumlist}

      `(apply function arglist))`
!!!(p) {:.unnumlist}

    `(do-result (i)`
!!!(p) {:.unnumlist}

      `(if (and (vectorp result-sequence)`
!!!(p) {:.unnumlist}

        `(array-has-fill-pointer-p result-sequence))`
!!!(p) {:.unnumlist}

      `(setf (fill-pointer result-sequence)`
!!!(p) {:.unnumlist}

  `(max i (fill-pointer result-sequence))))))`
!!!(p) {:.unnumlist}

   `(declare (inline do-one-call))`
!!!(p) {:.unnumlist}

   `;; Decide if the result is a list or vector,`
!!!(p) {:.unnumlist}

   `;; and loop through each element`
!!!(p) {:.unnumlist}

   `(if (listp result-sequence)`
!!!(p) {:.unnumlist}

    `(loop for i from 0 to (− n 1)`
!!!(p) {:.unnumlist}

     `for r on result-sequence`
!!!(p) {:.unnumlist}

     `do (setf (first r)`
!!!(p) {:.unnumlist}

        `(do-one-call i)))`
!!!(p) {:.unnumlist}

    `(loop for i from 0 to (− n 1)`
!!!(p) {:.unnumlist}

     `do (setf (aref result-sequence i)`
!!!(p) {:.unnumlist}

        `(do-one-call i))`
!!!(p) {:.unnumlist}

     `finally (do-result n))))`
!!!(p) {:.unnumlist}

   `result-sequence))`
!!!(p) {:.unnumlist}

There are several things worth noticing here.
First, I split the main loop into two versions, one where the result is a list, and the other where it is a vector.
Rather than duplicate code, the local functions `do-one-call` and `do-result` are defined.
The former is declared inline because it it called often, while the latter is not.
The arguments are computed by looking at each sequence in turn, taking the ith element if it is a vector, and popping the sequence if it is a list.
The arguments are stored into the list `arglist`, which has been preallocated to the correct size.
All in all, we compute the answer fairly efficiently, without generating unnecessary garbage.

The application could be done more efficiently, however.
Think what apply must do: scan down the argument list, and put each argument into the location expected by the function-calling conventions, and then branch to the function.
Some implementations provide a better way of doing this.
For example, the TI Lisp Machine provides two low-level primitive functions, `%push` and `%call`, that compile into single instructions to put the arguments into the right locations and branch to the function.
With these primitives, the body of `do-one-call` would be:

[ ](#){:#l0265}`(loop for seq on sequences`
!!!(p) {:.unnumlist}

  `do (if (listp (first seq))`
!!!(p) {:.unnumlist}

    `(%push (pop (first seq)))`
!!!(p) {:.unnumlist}

    `(%push (aref (first seq) i))))`
!!!(p) {:.unnumlist}

`(%call function length-sequences)`
!!!(p) {:.unnumlist}

There is a remaining inefficiency, though.
Each sequence is type-checked each time through the loop, even though the type remains constant once it is determined the first time.
Theoretically, we could code separate loops for each combination of types, just as we coded two loops depending on the type of the result sequence.
But that would mean 2*n* loops for *n* sequences, and there is no limit on how large *n* can be.

It might be worth it to provide specialized functions for small values of *n*, and dispatch to the appropriate function.
Here's a start at that approach:

[ ](#){:#l0270}`(defun map-into (result function &rest sequences)`
!!!(p) {:.unnumlist}

  `(apply`
!!!(p) {:.unnumlist}

   `(case (length sequences)`
!!!(p) {:.unnumlist}

    `(0 (if (listp result) #'map-into-list-0 #'map-into-vect-0))`
!!!(p) {:.unnumlist}

    `(1 (if (listp result)`
!!!(p) {:.unnumlist}

     `(if (listp (first sequences))`
!!!(p) {:.unnumlist}

       `#'map-into-list-l-list #'map-into-list-1-vect)`
!!!(p) {:.unnumlist}

     `(if (listp (first sequences))`
!!!(p) {:.unnumlist}

       `#'map-into-vect-l-list #'map-into-vect-l-vect)) )`
!!!(p) {:.unnumlist}

    `(2 (if (listp result)`
!!!(p) {:.unnumlist}

     `(if (listp (first sequences))`
!!!(p) {:.unnumlist}

      `(if (listp (second sequences))`
!!!(p) {:.unnumlist}

       `#'map-into-list-2-list-list`
!!!(p) {:.unnumlist}

       `#'map-into-list-2-list-vect)`
!!!(p) {:.unnumlist}

      `…)))`
!!!(p) {:.unnumlist}

    `(t (if (listp result) #'map-into-list-n #'map-into-vect-n)))`
!!!(p) {:.unnumlist}

   `result function sequences))`
!!!(p) {:.unnumlist}

The individual functions are not shown.
This approach is efficient in execution time, but it takes up a lot of space, considering that `map-into` is a relatively obscure function.
If `map-into` is declared `inline` and the compiler is reasonably good, then it will produce code that just calls the appropriate function.

### [ ](#){:#st0110}REDUCE with :key
{:#s0110}
{:.h2hd}

Another change in the ANSI proposal is to add a : key keyword to `reduce`.
This is a useful addition–in fact, for years I had been using a `reduce-by` function that provided just this functionality.
In this section we see how to add the : key keyword.

At the top level, I define reduce as an interface to the keywordless function `reduce*`.
They are both proclaimed inline, so there will be no overhead for the keywords in normal uses of reduce.

[ ](#){:#l0275}`(proclaim ’(inline reduce reduce*))`
!!!(p) {:.unnumlist}

 `(defun reduce* (fn seq from-end start end key init init-p)`
!!!(p) {:.unnumlist}

     `(funcall (if (listp seq) #’reduce-list #’reduce-vect)`
!!!(p) {:.unnumlist}

          `fn seq from-end (or start 0) end key init init-p))`
!!!(p) {:.unnumlist}

`(defun reduce (function sequence &key from-end start end key`
!!!(p) {:.unnumlist}

               `(initial-value nil initial-value-p))`
!!!(p) {:.unnumlist}

`    (reduce* function sequence from-end start end`
!!!(p) {:.unnumlist}

                  `key initial-value initial-value-p))`
!!!(p) {:.unnumlist}

The easier case is when the sequence is a vector:

[ ](#){:#l0280}`(defun reduce-vect (fn seq from-end start end key init init-p)`
!!!(p) {:.unnumlist}

    `(when (null end) (setf end (length seq)))`
!!!(p) {:.unnumlist}

    `(assert (<= 0 start end (length seq)) (start end)`
!!!(p) {:.unnumlist}

              `"Illegal subsequence of ~ a --- :start ~ d :end ~ d"`
!!!(p) {:.unnumlist}

                 `seq start end)`
!!!(p) {:.unnumlist}

   `(case (− end start)`
!!!(p) {:.unnumlist}

         `(0 (if init-p init (funcall fn)))`
!!!(p) {:.unnumlist}

         `(1 (if init-p`
!!!(p) {:.unnumlist}

             `(funcall fn init (funcall-if key (aref seq start)))`
!!!(p) {:.unnumlist}

             `(funcall-if key (aref seq start))))`
!!!(p) {:.unnumlist}

         `(t (if (not from-end)`
!!!(p) {:.unnumlist}

             `(let ((result`
!!!(p) {:.unnumlist}

                 `(if init-p`
!!!(p) {:.unnumlist}

                  `(funcall fn init`
!!!(p) {:.unnumlist}

                   `(funcall-if key (aref seq start)))`
!!!(p) {:.unnumlist}

                 `(funcall`
!!!(p) {:.unnumlist}

                      `fn`
!!!(p) {:.unnumlist}

                          `(funcall-if key (aref seq start))`
!!!(p) {:.unnumlist}

                          `(funcall-if key (aref seq (+ start 1)))))))`
!!!(p) {:.unnumlist}

             `(loop for i from (+ start (if init-p 1 2))`
!!!(p) {:.unnumlist}

                     `to (− end 1)`
!!!(p) {:.unnumlist}

                     `do (setf result`
!!!(p) {:.unnumlist}

                       `(funcall`
!!!(p) {:.unnumlist}

                        `fn result`
!!!(p) {:.unnumlist}

                        `(funcall-if key (aref seq i)))))`
!!!(p) {:.unnumlist}

                 `result)`
!!!(p) {:.unnumlist}

             `(let ((result`
!!!(p) {:.unnumlist}

                 `(if init-p`
!!!(p) {:.unnumlist}

               `(funcall`
!!!(p) {:.unnumlist}

       `fn`
!!!(p) {:.unnumlist}

       `(funcall-if key (aref seq (− end 1)))`
!!!(p) {:.unnumlist}

               `init)`
!!!(p) {:.unnumlist}

          `(funcall`
!!!(p) {:.unnumlist}

              `fn`
!!!(p) {:.unnumlist}

               `(funcall-if key (aref seq (− end 2)))`
!!!(p) {:.unnumlist}

               `(funcall-if key (aref seq (− end 1)))))))`
!!!(p) {:.unnumlist}

 `(loop for i from (− end (if init-p 2 3)) downto start`
!!!(p) {:.unnumlist}

         `do (setf result`
!!!(p) {:.unnumlist}

                `(funcall`
!!!(p) {:.unnumlist}

                                `fn`
!!!(p) {:.unnumlist}

                                `(funcall-if key (aref seq i))`
!!!(p) {:.unnumlist}

                                `result)))`
!!!(p) {:.unnumlist}

`result)))))`
!!!(p) {:.unnumlist}

When the sequence is a list, we go to some trouble to avoid Computing the length, since that is an *O(n)* operation on lists.
The hardest decision is what to do when the list is to be traversed from the end.
There are four choices:

* [ ](#){:#l0285}• **recurse.** We could recursively walk the list until we hit the end, and then compute the results on the way back up from the recursions.
However, some implementations may have fairly small bounds on the depths of recursive calls, and a system function like reduce should never run afoul of such limitations.
In any event, the amount of stack space consumed by this approach would normally be more than the amount of heap space consumed in the next approach.

* • **reverse.** We could reverse the list and then consider `from-end` true.
The only drawback is the time and space needed to construct the reversed list.

* • **nreverse.** We could destructively reverse the list in place, do the reduce computation, and then destructively reverse the list back to its original state (perhaps with an unwind-protect added).
Unfortunately, this is just incorrect.
The list may be bound to some variable that is accessible to the function used in the reduction.
If that is so, the function will see the reversed list, not the original list.

* • **coerce.** We could convert the list to a vector, and then use `reduce-vect`.
This has an advantage over the reverse approach in that vectors generally take only half as much storage as lists.
Therefore, this is the approach I adopt.

[ ](#){:#l0290}`(defmacro funcall-if (fn arg)`
!!!(p) {:.unnumlist1}

   `(once-only (fn)`
!!!(p) {:.unnumlist1}

       `‘(if .fn (funcall .fn .arg) .arg)))`
!!!(p) {:.unnumlist1}

`(defun reduce-list (fn seq from-end start end key init init-p)`
!!!(p) {:.unnumlist1}

    `(when (null end) (setf end most-positive-fixnum))`
!!!(p) {:.unnumlist1}

    `(cond ((> start 0)`
!!!(p) {:.unnumlist1}

             `(reduce-list fn (nthcdr start seq) from-end 0`
!!!(p) {:.unnumlist1}

                   `(− end start) key init init-p))`
!!!(p) {:.unnumlist1}

             `((or (null seq) (eql start end))`
!!!(p) {:.unnumlist1}

             `(if init-p init (funcall fn)))`
!!!(p) {:.unnumlist1}

             `((= (− end start) 1)`
!!!(p) {:.unnumlist1}

             `(if init-p`
!!!(p) {:.unnumlist1}

                `(funcall fn init (funcall-if key (first seq)))`
!!!(p) {:.unnumlist1}

                `(funcall-if key (first seq))))`
!!!(p) {:.unnumlist1}

          `(from-end`
!!!(p) {:.unnumlist1}

             `(reduce-vect fn (coerce seq 'vector) t start end`
!!!(p) {:.unnumlist1}

                   `key init init-p))`
!!!(p) {:.unnumlist1}

                `((null (rest seq))`
!!!(p) {:.unnumlist1}

             `(if init-p`
!!!(p) {:.unnumlist1}

                `(funcall fn init (funcall-if key (first seq)))`
!!!(p) {:.unnumlist1}

                `(funcall-if key (first seq))))`
!!!(p) {:.unnumlist1}

          `(t (let ((result`
!!!(p) {:.unnumlist1}

          `(if init-p`
!!!(p) {:.unnumlist1}

                 `(funcall`
!!!(p) {:.unnumlist1}

                        `fn init`
!!!(p) {:.unnumlist1}

                        `(funcall-if key (pop seq)))`
!!!(p) {:.unnumlist1}

                 `(funcall`
!!!(p) {:.unnumlist1}

                        `fn`
!!!(p) {:.unnumlist1}

                        `(funcall-if key (pop seq))`
!!!(p) {:.unnumlist1}

                        `(funcall-if key (pop seq))))))`
!!!(p) {:.unnumlist1}

          `(if end`
!!!(p) {:.unnumlist1}

                `(loop repeat (− end (if init-p 1 2)) while seq`
!!!(p) {:.unnumlist1}

                 `do (setf result`
!!!(p) {:.unnumlist1}

                        `(funcall`
!!!(p) {:.unnumlist1}

                        `fn result`
!!!(p) {:.unnumlist1}

                     `(funcall-if key (pop seq)))))`
!!!(p) {:.unnumlist1}

             `(loop while seq`
!!!(p) {:.unnumlist1}

                 `do (setf result`
!!!(p) {:.unnumlist1}

               `(funcall`
!!!(p) {:.unnumlist1}

                  `fn result`
!!!(p) {:.unnumlist1}

                  `(funcall-if key (pop seq)))))`
!!!(p) {:.unnumlist1}

             `result)))))`
!!!(p) {:.unnumlist1}

## [ ](#){:#st0115}24.7 Exercises
{:#s0115}
{:.h1hd}

**Exercise 24.2 [m]** The function reduce is a very useful one, especially with the key keyword.
Write nonrecursive definitions for append and 1 ength using reduce.
What other common functions can be written with reduce?

**Exercise 24.3** The so-called loop keywords are not symbols in the keyword package.
The preceding code assumes they are all in the current package, but this is not quite right.
Change the definition of `loop` so that any symbol with the same name as a loop keyword acts as a keyword, regardless of the symbol's package.

**Exercise 24.4** Can there be a value for *exp* for which the following expressions are not equivalent?
Either demonstrate such an *exp* or argue why none can exist.

[ ](#){:#l0295}`(loop for x in list collect *exp*)`
!!!(p) {:.unnumlist}

`(mapcar #'(lambda (x) *exp)* list))`
!!!(p) {:.unnumlist}

**Exercise 24.5** The object-oriented language Eiffel provides two interesting `loop` keywords: `invariant` and `variant`.
The former takes a Boolean-valued expression that must remain true on every iteration of the loop, and the latter takes a integervalued expression that must decrease on every iteration, but never becomes negative.
Errors are signaled if these conditions are violated.
Use def `loop` to implement these two keywords.
Make them generate code conditionally, based on a global flag.

## [ ](#){:#st0120}24.8 Answers
{:#s0120}
{:.h1hd}

**Answer 24.1**

[ ](#){:#l0300}`(defvar *queue*)`
!!!(p) {:.unnumlist}

`(defun collect (item) (enqueue item *queue*))`
!!!(p) {:.unnumlist}

`(defmacro with-collection (&body body)`
!!!(p) {:.unnumlist}

     `‘(let ((*queue* (make-queue)))`
!!!(p) {:.unnumlist}

                 `,@body`
!!!(p) {:.unnumlist}

           `(queue-contents *queue*)))`
!!!(p) {:.unnumlist}

Here's another version that allows the collection variable to be named.
That way, more than one collection can be going on at the same time.

[ ](#){:#l0305}`(defun collect (item &optional (queue *queue*))`
!!!(p) {:.unnumlist}

      `(enqueue item queue))`
!!!(p) {:.unnumlist}

`(defmacro with-collection ((&optional (queue '*queue*))`
!!!(p) {:.unnumlist}

                               `&body body)`
!!!(p) {:.unnumlist}

      `‘(let ((,queue (make-queue)))`
!!!(p) {:.unnumlist}

       `,@body`
!!!(p) {:.unnumlist}

      `(queue-contents .queue)))`
!!!(p) {:.unnumlist}

**Answer 24.2**

[ ](#){:#l0310}`(defun append-r (x y)`
!!!(p) {:.unnumlist}

      `(reduce #'cons x :initial-value y :from-end t))`
!!!(p) {:.unnumlist}

`(defun length-r (list)`
!!!(p) {:.unnumlist}

      `(reduce #'+ list :key #'(lambda (x) 1)))`
!!!(p) {:.unnumlist}

**Answer 24.4** The difference between `loop` and `mapcar` is that the former uses only one variable `x`, while the latter uses a different `x` each time.
If `x`'s extent is no bigger than its scope (as it is in most expressions) then this makes no difference.
But if any `x` is captured, giving it a longer extent, then a difference shows up.
Consider *exp =*`#'(lambda () x).`

[ ](#){:#l0315}`> (mapcar #’funcall (loop for x in ’(1 2 3) collect`
!!!(p) {:.unnumlist}

                     `#’(lambda O x)))`
!!!(p) {:.unnumlist}

`(3 3 3)`
!!!(p) {:.unnumlist}

`>(mapcar #’funcall (mapcar #’(lambda (x) #’(lambda () x))`
!!!(p) {:.unnumlist}

                          `’(1 2 3)))`
!!!(p) {:.unnumlist}

`(1 2 3)`
!!!(p) {:.unnumlist}

**Answer 24.5**

[ ](#){:#l0320}`(defvar *check-invariants* t`
!!!(p) {:.unnumlist}

      `"Should VARIANT and INVARIANT clauses in LOOP be checked?")`
!!!(p) {:.unnumlist}

`(defloop invariant (l exp)`
!!!(p) {:.unnumlist}

      `(when *check-invariants*`
!!!(p) {:.unnumlist}

                `(add-body l '(assert .exp () "Invariant violated."))))`
!!!(p) {:.unnumlist}

`(defloop variant (l exp)`
!!!(p) {:.unnumlist}

 `(when *check-invariants*`
!!!(p) {:.unnumlist}

           `(let ((var (gensym "INV")))`
!!!(p) {:.unnumlist}

                `(add-var l var nil)`
!!!(p) {:.unnumlist}

                `(add-body l '(setf ,var (update-variant .var .exp))))))`
!!!(p) {:.unnumlist}

     `(defun update-variant (old new)`
!!!(p) {:.unnumlist}

      `(assert (or (null old) (< new old)) ()`
!!!(p) {:.unnumlist}

                `"Variant is not monotonically decreasing")`
!!!(p) {:.unnumlist}

      `(assert (> new 0) () "Variant is no longer positive")`
!!!(p) {:.unnumlist}

     `new)`
!!!(p) {:.unnumlist}

Here's an example:

[ ](#){:#l0325}`(defun gcd2 (a b)`
!!!(p) {:.unnumlist}

      `"Greatest common divisor.
For two positive integer arguments."`
!!!(p) {:.unnumlist}

      `(check-type a (integer 1))`
!!!(p) {:.unnumlist}

      `(check-type b (integer 1))`
!!!(p) {:.unnumlist}

      `(loop with x = a with y = b`
!!!(p) {:.unnumlist}

                `invariant (and (> x 0) (> y 0)) ;; (= (gcd x y) (gcd a b))`
!!!(p) {:.unnumlist}

                `variant (max x y)`
!!!(p) {:.unnumlist}

                `until (= x y)`
!!!(p) {:.unnumlist}

                `do (if (> x y) (decf x y) (decf y x))`
!!!(p) {:.unnumlist}

                `finally (return x)))`
!!!(p) {:.unnumlist}

Here the invariant is written semi-informally.
We could include the calls to `gcd`, but that seems to be defeating the purpose of `gcd2`, so that part is left as a comment.
The idea is that the comment should help the reader prove the correctness of the code, and the executable part serves to notify the lazy reader when something is demonstrably wrong at run time.

----------------------

[1](#xfn0010){:#np0010} Or in the user package in non-ANSI systems.
!!!(p) {:.ftnote1}

[2](#xfn0015){:#np0015} As was noted before, the proper way to do this is to proclaim squa re as an inline function, not a macro, but please bear with the example.
!!!(p) {:.ftnote1}

