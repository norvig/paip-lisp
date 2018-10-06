# Chapter 24
## ANSI Common Lisp

This chapter briefly covers some advanced features of Common Lisp that were not used in the rest of the book.
The first topic, packages, is crucial in building large systems but was not covered in this book, since the programs are concise.
The next four topics-error handling, pretty printing, series, and the loop macro-are covered in *Common Lisp the Language,* 2d edition, but not in the first edition of the book.
Thus, they may not be applicable to your Lisp compiler.
The final topic, sequence functions, shows how to write efficient functions that work for either lists or vectors.

## 24.1 Packages
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
By default, Lisp starts out in the `common-lisp-user` package.[1](#fn0010) That means that if we type a new symbol, like `zxv!!!(char) ®!?+qw`, it will be entered into that package.
Converting a string to a symbol and placing it in a package is called *interning.* It is done automatically by `read`, and can be done by the function `intern` if necessary.
Name conflicts arise when there is contention for names within the `common-lisp-user` package.

To avoid name conflicts, simply create your new symbols in another package, one that is specific to your program.
The easiest way to implement this is to split each system into at least two files-one to define the package that the system resides in, and the others for the system itself.
For example, the `emycin` system should start with a file that defines the `emycin` package.
The following form defines the `emycin` package to use the `lisp` package.
That means that when the current package is `emycin`, you can still refer to ail the built-in Lisp symbols.

```lisp
(make-package "EMYCIN" :use '("LISP"))
```

The file containing the package definition should always be loaded before the rest of the system.
Those files should start with the following call, which insures that all new symbols will be interned in the `emycin` package:

```lisp
(in-package "EMYCIN")
```

Packages are used for information-hiding purposes as well as for avoiding name clashes.
A distinction is made between *internal* and *external* symbols.
External symbols are those that a user of a system would want to refer to, while internal symbols are those that help implement the system but are not needed by a user of the system.
The symbol `rule` would probably be internai to both the `emycin` and `parser` package, but `defrule` would be external, because a user of the `emycin` system uses `defrule` to define new rules.
The designer of a system is responsible for advertising which symbols are external.
The proper call is:

```lisp
(export '(emycin defrule defcontext defparm yes/no yes no is))
```

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

```lisp
(defpackage emycin
```

  `(:use common-lisp)`

  `(:export emycin defrule defcontext defparm yes/no yes no is))`

For more on packages and building systems, see [section 25.16](B978008057115750025X.xhtml#s0110) or *Common Lisp the Language.*

### The Seven Name Spaces
{:#s0015}
{:.h2hd}

One important fact to remember about packages is that they deal with symbols, and only indirectly deal with the uses those symbols might have.
For example, you may think of `(export 'parse)` as exporting the function `parse`, but really it is exporting the symbol `parse`, which may happen to have a function definition associated with it.
However, if the symbol is put to another use-perhaps as a variable or a data type-then those uses are made accessible by the `export` statement as well.

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

```lisp
(defun f (f)
```

  `(block f`

    `(tagbody`

      `f (catch 'f`

        `(if (typep f 'f)`

          `(throw 'f (go f)))`

        `(funcall #'f (get (symbol-value 'f) 'f))))))`

## 24.2 Conditions and Error Handling
{:#s0020}
{:.h1hd}

An extraordinary feature of ANSI Common Lisp is the facility for handling errors.
In most languages it is very difficult for the programmer to arrange to recover from an error.
Although Ada and some implementations of C provide functions for error recovery, they are not generally part of the repertoire of most programmers.
Thus, we find C programs that exit with the ungraceful message `Segmentation violation: core dumped`.

Common Lisp provides one of the most comprehensive and easy-to-use error-handling mechanism of any programming language, which leads to more robust programs.
The process of error handling is divided into two parts: signaling an error, and handling it.

### Signaling Errors
{:#s0025}
{:.h2hd}

An *error* is a condition that the program does not know how to handle.
Since the program does not know what to do, its only recourse is to announce the occurrence of the error, with the hope that some other program or user will know what to do.
This announcement is called *signaling* an error.
An error can be signaled by a Common Lisp built-in function, as when `( / 3 0 )` signals a divide-by-zero error.
Errors can also be signaled explicitly by the programmer, as in a call to `(error "Illegal value.")`.

Actually, it is a bit of a simplification to talk only of *signaling errors.* The precise term is *signaling a condition.* Some conditions, like end-of-file, are not considered errors, but nevertheless they are unusual conditions that must be dealt with.
The condition system in Common Lisp allows for the definition of all kinds of conditions, but we will continue to talk about errors in this brief discussion, since most conditions are in fact error conditions.

### Handling Errors
{:#s0030}
{:.h2hd}

By default, signaling an error invokes the debugger.
In the following example, the >> prompt means that the user is in the debugger rather than at the top level.

```lisp
> (/ 3 0)
Error: An attempt was made to divide by zero.
>>
```

ANSI Common Lisp provides ways of changing this default behavior.
Conceptually, this is done by setting up an *error handler* which handles the error in some way.
Error handlers are bound dynamically and are used to process signaled errors.
An error handler is much like a `catch`, and signaling an error is like a `throw`.
In fact, in many systems `catch` and `throw` are implemented with the error-condition system.

The simplest way of handling an error is with the macro `ignore-errors`.
If noerror occurs, `ignore-errors` is just like `progn`.
But if an error does occur, `ignore-errors` will return `nil` as its first value and `t` as its second, to indicate that an error has occurred but without doing anything else:

`> (ignore-errors (/ 3 1))`=> `3 NIL`

`> (ignore-errors (/ 3 0))`=> `NIL T`

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

```lisp
(defun div (x y)
```

  `(handler-case (/ x y)`

    `(division-by-zero () most-positive-fixnum)`

    `(arithmetic-error () 0)))`

`> (div 8 2)`=> `4`

`> (div 3 0)`=> `16777215`

```lisp
> (div 'xyzzy 1)
Error: The value of NUMBER, XYZZY, should be a number
```

Through judicious use of `handler-case`, the programmer can create robust code that reacts well to unexpected situations.
For more details, see chapter 29 of *Common Lisp the Language,* 2d edition.

## 24.3 Pretty Printing
{:#s0035}
{:.h1hd}

ANSI Common Lisp adds a facility for user-controlled pretty printing.
In general, *pretty printing* refers to the process of printing complex expressions in a format that uses indentation to improve readability.
The function `pprint` was always available, but before ANSI Common Lisp it was left unspecified, and it could not be extended by the user.
Chapter 27 of *Common Lisp the Language,* 2d edition presents a pretty-printing facility that gives the user fine-grained control over the printing of all types of objects.
In addition, the facility is integrated with the `format` function.

## 24.4 Series
{:#s0040}
{:.h1hd}

The functional style of programming with higher-order functions is one of the attractions of Lisp.
The following expression to sum the square roots of the positive numbers in the list `nums` is clear and concise:

```lisp
(reduce #'+ (mapcar #'sqrt (find-all-if #'plusp nums)))
```

Unfortunately, it is inefficient: both `find-all-if` and `mapcar` cons up intermediate lists that are not needed in the final sum.
The following two versions using `loop` and `dolist` are efficient but not as pretty:

```lisp
;; Using Loop                      ;; Using dolist
(loop for num in nums      (let ((sum 0))
```

    `when (plusp num)                (dolist (num nums sum)`

    `sum (sqrt num))                        (when (plusp num)`

                                                                                                                        `(incf sum num))))`

A compromise between the two approaches is provided by the *series* facility, defined in appendix A of *Common Lisp the Language*, 2d edition.
The example using series would look like:

```lisp
(collect-sum (#Msqrt (choose-if #'plusp nums)))
```

This looks very much like the functional version: only the names have been changed.
However, it compiles into efficient iterative code very much like the `dolist` version.

Like pipes (see [section 9.3](B9780080571157500091.xhtml#s0015)), elements of a series are only evaluated when they are needed.
So we can write `(scan-range :from 0)` to indicate the infinite series of integers starting from 0, but if we only use, say, the first five elements of this series, then only the first five elements will be generated.

The series facility offers a convenient and efficient alternative to iterative loops and sequence functions.
Although the series proposai has not yet been adopted as an official part of ANSI Common Lisp, its inclusion in the reference manual has made it increasingly popular.

## 24.5 The Loop Macro
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

```lisp
(loop for i from 1 to n do (print (sqrt i))) =
(LET* ((I 1)
```

        `(TEMP N))`

  `(TAGBODY`

      `LOOP`

        `(IF (> I TEMP)`

              `(GO END))`

        `(PRINT (SQRT I))`

        `(SETF I (+ I 1))`

        `(GO LOOP)`

      `END))`

```lisp
(loop for v in list do (print v)) =
(LET* ((IN LIST)
```

        `(V (CAR IN)))`

      `(TAGBODY`

      `LOOP`

        `(IF (NULL IN)`

              `(GO END))`

        `(PRINT V)`

        `(SETF IN (CDR IN))`

        `(SETF V (CAR IN))`

        `(GO LOOP)`

      `END))`

Each loop initializes some variables, then enters a loop with some exit tests and a body.
So the template is something like:

```lisp
(let* (*variables...*)
```

  `(tagbody`

    `loop`

      `(if *exit-tests*`

        `(go end))`

      *`Body`*

      `(go loop)`

    `end))`

Actually, there's more we might need in the general case.
There may be a prologue that appears before the loop but after the variable initialization, and similarly there may be an epilogue after the loop.
This epilogue may involve returning a value, and since we want to be able to return from the loop in any case, we need to wrap a `block` around it.
So the complete template is:

```lisp
(let* (*variables...*)
```

  `(block *name*`

    *`Prologue`*

    `(tagbody`

      `Loop`

        *`body`*

        `(go loop)`

      `end`

        *`epilogue`*

        `(return *result*))))`

To generate this template from the body of a `loop` form, we will employ a structure with fields for each of the parts of the template:

```lisp
(defstruct loop
```

    `"A structure to hold parts of a loop as it is built."`

    `(vars nil) (prologue nil) (body nil) (steps nil)`

    `(epilogue nil) (result nil) (name nil))`

Now the `loop` macro needs to do four things: (1) decide if this is a use of the simple, non-keyword `loop` or the complex ANSI `loop`.
If it is the latter, then (2) make an instance of the `loop` structure, (3) process the body of the loop, filling in apprpriate fields of the structure, and (4) place the filled fields into the template.
Here is the `loop` macro:

```lisp
(defmacro loop (&rest exps)
```

    `"Supports both ANSI and simple LOOP.`

    `Warning: Not every loop keyword is supported."`

    `(if (every #'listp exps)`

        `;; No keywords implies simple loop:`

        `'(block nil (tagbody loop ,@exps (go loop)))`

        `;; otherwise process loop keywords:`

        `(let ((l (make-loop)))`

            `(parse-loop-body l exps)`

            `(fill-loop-template l))))`

```lisp
(defun fill-loop-template (l)
```

    `"Use a loop-structure instance to fill the template."`

    `'(let* .(nreverse (loop-vars l))`

        `(block ,(loop-name l)`

          `,@(nreverse (loop-prologue l)`

          `(tagbody`

            `loop`

                `,@(nreverse (loop-body l))`

                `,@(nreverse (loop-steps l))`

                `(go loop)`

            `end`

                `,@(nreverse (loop-epilogue l))`

                `(return ,(loop-result l))))))`

Most of the work is in writing `parse-loop-body`, which takes a list of expressions and parses them into the proper fields of a loop structure.
It will use the following auxiliary functions:

```lisp
(defun add-body (l exp) (push exp (loop-body l)))
(defun add-test (l test)
```

    `"Put in a test for loop termination."`

    `(push '(if .test (go end)) (loop-body l)))`

```lisp
(defun add-var (l var init &optional (update nil update?))
```

    `"Add a variable, maybe including an update step."`

    `(unless (assoc var (loop-vars l))`

        `(push (list var init) (loop-vars l)))`

    `(when update?`

        `(push '(setq ,var ,update) (loop-steps l))))`

There are a number of alternative ways of implementing this kind of processing.
One would be to use special variables: `*prologue*, *body*, *epilogue*`, and so on.
This would mean we wouldn't have to pass around the loop structure `l`, but there would be significant clutter in having seven new special variables.
Another possibility is to use local variables and close the definitions of `loop`, along with the `add-` functions in that local environment:

```lisp
(let (body prologue epilogue steps vars name result)
```

    `(defmacro loop ...)`

    `(defun add-body ...)`

    `(defun add-test ...)`

    `(defun add-var ...))`

This is somewhat cleaner style, but some early Common Lisp compilers do not support embedded `defuns`, so I chose to write in a style that I knew would work in all implementations.
Another design choice would be to return multiple values for each of the components and have `parse-loop-body` put them all together.
This is in fact done in one of the Lisp Machine implementations of `loop`, but I think it is a poor decision: seven components are too many to keep track of by positional notation.

### Anatomy of a Loop
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

```lisp
(defun parse-loop-body (l exps)
```

    `"Parse the exps based on the first exp being a keyword.`

    `Continue until all the exps are parsed."`

    `(unless (null exps)`

        `(parse-loop-body`

            `l (call-loop-fn l (first exps) (rest exps)))))`

```lisp
(defun call-loop-fn (l key exps)
```

    `"Return the loop parsing function for this keyword."`

    `(if (and (symbolp key) (get key 'loop-fn))`

        `(funcall (get key 'loop-fn) l (first exps) (rest exps))`

        `(error "Unknown loop key: "a" key)))`

```lisp
(defmacro defloop (key args &rest body)
```

    `"Define a new LOOP keyword."`

    `;; If the args do not have a third arg, one is supplied.`

    `;; Also, we can define an alias with (defloop key other-key)`

    `'(setf (get ',key 'loop-fn)`

        `,(cond ((and (symbolp args) (null body))`

            `'#'(lambda (1 x y)`

                    `(call-loop-fn l '.args (cons x y))))`

              `((and (listp args) (= (length args) 2))`

                `'#'(lambda (.@args -exps-) ,@body -exps-))`

              `(t '#'(lambda .args ,@body)))))`

Now we are ready to define some `loop` keywords.
Each of the following sections refers to (and implements the loop keywords in) a section of chapter 26 of *Common Lisp the Language*, 2d edition.

### Iteration Control (26.6)
{:#s0055}
{:.h2hd}

Here we define keywords for iterating over elements of a sequence and for stopping the iteration.
The following cases are covered, where uppercase words represent loop keywords:

```lisp
(LOOP REPEAT n ...)
(LOOP FOR i FROM s TO e BY inc ...)
(LOOP FOR v IN l ...)
(LOOP FOR v ON l ...)
(LOOP FOR v = expr [THEN step] ...)
```

The implementation is straightforward, although somewhat tedious for complex keywords like `for`.
Take the simpler keyword, `repeat`.
To handle it, we generate a new variable that will count down the number of times to repeat.
We call `add-var` to add that variable, with its initial value, to the loop structure.
We also give this variable an update expression, which decrements the variable by one each time through the loop.
Then ail we need to do is call `add-test` to insert code that will exit the loop when the variable reaches zero:

```lisp
(defloop repeat (l times)
```

    `"(LOOP REPEAT n ...) does loop body n times."`

    `(let ((i (gensym "REPEAT")))`

        `(add-var l i times '(- ,i 1))`

        `(add-test l '(<= ,i 0))))`

The loop keyword `for` is more complicated, but each case can be analyzed in the same way as `repeat`:

```lisp
(defloop as for) ;; AS is the same as FOR
(defloop for (l var exps)
```

    `"4 of the 7 cases for FOR are covered here:`

    `(LOOP FOR i FROM s TO e BY inc ...) does arithmetic iteration`

    `(LOOP FOR v IN l ...) iterates for each element of l`

    `(LOOP FOR v ON l ...) iterates for each tail of l`

    `(LOOP FOR v = expr [THEN step]) initializes and iterates v"`

    `(let ((key (first exps))`

            `(source (second exps))`

            `(rest (rest2 exps)))`

        `(ecase key`

            `((from downfrom upfrom to downto upto by)`

          `(loop-for-arithmetic l var exps))`

            `(in (let ((v (gensym "IN")))`

                      `(add-var l v source '(cdr ,v))`

                      `(add-var l var '(car ,v) '(car ,v))`

                      `(add-test l '(null ,v))`

                      `rest))`

            `(on (add-var l var source '(cdr ,var))`

                    `(add-test l '(null .var))`

                    `rest)`

            `(= (if (eq (first rest) 'then)`

                            `(progn`

                                `(pop rest)`

                                `(add-var l var source (pop rest)))`

                            `(progn`

                                `(add-var l var nil)`

                                `(add-body l '(setq ,var .source))))`

                    `rest)`

            `;; ACROSS.
BEING clauses omitted`

            `)))`

```lisp
(defun loop-for-arithmetic (l var exps)
```

    `"Parse loop expressions of the form:`

    `(LOOP FOR var [FROM | DOWNFROM | UPFROM exp1] [TO | DOWNTO | UPTO exp2]`

              `[BY exp3]"`

    `;; The prepositions BELOW and ABOVE are omitted`

    `(let ((exp1 0)`

              `(exp2 nil)`

              `(exp3 1)`

              `(down?
nil))`

        `;; Parse the keywords:`

        `(when (member (first exps) '(from downfrom upfrom))`

          `(setf exp1 (second exps)`

                  `down?
(eq (first exps) 'downfrom)`

                  `exps (rest2 exps)))`

        `(when (member (first exps) '(to downto upto))`

          `(setf exp2 (second exps)`

                  `down?
(or down?
(eq (first exps) 'downto))`

                  `exps (rest2 exps)))`

        `(when (eq (first exps) 'by)`

          `(setf exp3 (second exps)`

                  `exps (rest2 exps)))`

        `;; Add variables and tests:`

        `(add-var l var exp1`

                  `'(,(if down?
'- '+) ,var ,(maybe-temp l exp3)))`

        `(when exp2`

            `(add-test l '(,(if down?
'< '>) ,var ,(maybe-temp l exp2))))`

        `;; and return the remaining expressions:`

                  `exps))`

```lisp
(defun maybe-temp (l exp)
```

    `"Generate a temporary variable, if needed."`

    `(if (constantp exp)`

        `exp`

        `(let ((temp (gensym "TEMP")))`

            `(add-var l temp exp)`

            `temp)))`

### End-Test Control (26.7)
{:#s0060}
{:.h2hd}

In this section we cover the following clauses:

```lisp
(LOOP UNTIL test ...)
(LOOP WHILE test ...)
(LOOP ALWAYS condition ...)
(LOOP NEVER condition ...)
(LOOP THEREIS condition ...)
(LOOP ... (LOOP-FINISH) ...)
```

Each keyword is quite simple:

```lisp
(defloop until (l test) (add-test l test))
(defloop while (l test) (add-test l '(not .test)))
(defloop always (l test)
```

    `(setf (loop-result l) t)`

    `(add-body l '(if (not ,test) (return nil))))`

```lisp
(defloop never (l test)
```

    `(setf (loop-result l) t)`

    `(add-body l '(if ,test (return nil))))`

```lisp
(defloop thereis (l test) (add-body l '(return-if ,test)))
(defmacro return-if (test)
```

    `"Return TEST if it is non-nil."`

    `(once-only (test)`

        `'(if ,test (return ,test))))`

```lisp
(defmacro loop-finish () '(go end))
```

### Value Accumulation (26.8)
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

```lisp
(LOOP COLLECT item ...)
(LOOP NCONC item ...)
(LOOP APPEND item ...)
(LOOP COUNT item ...)
(LOOP SUM item ...)
(LOOP MAXIMIZE item ...)
(LOOP MINIMIZE item ...)
```

The implementation is:

```lisp
(defconstant *acc* (gensym "ACC")
```

    `"Variable used for value accumulation in LOOP.")`

```lisp
;;; INTO preposition is omitted
(defloop collect (l exp)
```

    `(add-var l *acc* '(make-queue))`

    `(add-body l '(enqueue ,exp .*acc*))`

    `(setf (loop-result l) '(queue-contents ,*acc*)))`

```lisp
(defloop nconc (l exp)
```

    `(add-var l *acc* '(make-queue))`

    `(add-body l '(queue-nconc ,*acc* .exp))`

    `(setf (loop-result l) '(queue-contents .*acc*)))`

```lisp
(defloop append (l exp exps)
```

    `(call-loop-fn l 'nconc '((copy-list .exp) .,exps)))`

```lisp
(defloop count (l exp)
```

    `(add-var l *acc* 0)`

    `(add-body l '(when .exp (incf .*acc*)))`

    `(setf (loop-result l) *acc*))`

```lisp
(defloop sum (l exp)
```

    `(add-var l *acc* 0)`

    `(add-body l '(incf ,*acc* .exp))`

    `(setf (loop-result l) *acc*))`

```lisp
(defloop maximize (l exp)
```

    `(add-var l *acc* nil)`

    `(add-body l '(setf ,*acc*`

                `(if ,*acc*`

                        `(max ,*acc* ,exp)`

                        `,exp)))`

    `(setf (loop-result l) *acc*))`

```lisp
(defloop minimize (l exp)
```

    `(add-var 1 *acc* nil)`

    `(add-body l '(setf ,*acc*`

                `(if ,*acc*`

                        `(min ,*acc* ,exp)`

                        `,exp)))`

    `(setf (loop-result l) *acc*))`

```lisp
(defloop collecting collect)
(defloop nconcing nconc)
(defloop appending append)
(defloop counting count)
(defloop summing sum)
(defloop maximizing maximize)
(defloop minimizing minimize)
```

**Exercise  24.1**`loop` lets us build aggregates (lists, maximums, sums, etc.) over the body of the loop.
Sometimes it is inconvenient to be restricted to a single-loop body.
For example, we might want a list of all the nonzero elements of a two-dimensional array.
One way to implement this is with a macro, `with-collection`, that sets up and returns a queue structure that is built by calls to the function `collect`.
For example:

```lisp
> (let ((A '#2a((l 0 0) (0 2 4) (0 0 3))))
```

    `(with-collection`

        `(loop for i from 0 to 2 do`

            `(loop for j from 0 to 2 do`

                `(if (> (aref a i j) 0)`

                    `(collect (aref A i j)))))))`

```lisp
(1 2 4 3)
```

Implement `with-collection` and `collect`.

### Variable Initialization (26.9)
{:#s0070}
{:.h2hd}

The `with` clause allows local variables-I have included it, but recommend using a `let` instead.
I have not included the `and` preposition, which allows the variables to nest at different levels.

`;;;; 26.9.
Variable Initializations ("and" omitted)`

```lisp
(defloop with (l var exps)
```

    `(let ((init nil))`

        `(when (eq (first exps) '=)`

            `(setf init (second exps)`

                `exps (rest2 exps)))`

        `(add-var l var init)`

        `exps))`

### Conditional Execution (26.10)
{:#s0075}
{:.h2hd}

`loop` also provides forms for conditional execution.
These should be avoided whenever possible, as Lisp already has a set of perfectly good conditional macros.
However, sometimes you want to make, say, a `collect` conditional on some test.
In that case, loop conditionals are acceptable.
The clauses covered here are:

(`LOOP WHEN test ... CELSE ...]) ; IF` is asynonym for `WHEN`

```lisp
(LOOP UNLESS test ... [ELSE ...])
```

Here is an example of `when`:

`> (loop for`x `from 1 to 10`

          `when (oddp x)`

                  `collect x`

          `else collect (- x))`

```lisp
(1 -2 3 -4 5- 6 7 -8 9 -10)
```

Of course, we could have said `collect (if (oddp x ) x ( - x ) )` and done without the conditional.
There is one extra feature in loop's conditionals: the value of the test is stored in the variable it for subsequent use in the THEN or ELSE parts.
(This is just the kind of feature that makes some people love `loop` and others throw up their hands in despair.) Here is an example:

```lisp
> (loop for x from 1 to 10
```

        `when (second (assoc x '((l one) (3 three) (5 five))))`

        `collect it)`

```lisp
(ONE THREE FIVE)
```

The conditional clauses are a little tricky to implement, since they involve parsing other clauses.
The idea is that `call-loop-fn` parses the THEN and ELSE parts, adding whatever is necessary to the body and to other parts of the loop structure.
Then `add-body` is used to add labels and go statements that branch to the labels as needed.
This is the same technique that is used to compile conditionals in [chapter 23](B9780080571157500236.xhtml); see the function `comp-if` on [page 787](B9780080571157500236.xhtml#p787).
Here is the code:

```lisp
(defloop when (l test exps)
```

    `(loop-unless l '(not ,(maybe-set-it test exps)) exps))`

```lisp
(defloop unless (l test exps)
```

    `(loop-unless l (maybe-set-it test exps) exps))`

```lisp
(defun maybe-set-it (test exps)
```

    `"Return value, but if the variable IT appears in exps,`

    `then return code that sets IT to value."`

    `(if (find-anywhere 'it exps)`

        `'(setq it .test)`

        `test))`

```lisp
(defloop if when)
(defun loop-unless (l test exps)
```

    `(let ((label (gensym "L")))`

        `(add-var l 'it nil )`

        `;; Emit code for the test and the THEN part`

        `(add-body l '(if .test (go ,label)))`

        `(setf exps (call-loop-fn l (first exps) (rest exps)))`

        `;; Optionally emit code for the ELSE part`

        `(if (eq (first exps) 'else)`

            `(progn`

                `(let ((label2 (gensym "L")))`

                    `(add-body l '(go ,label2))`

                    `(add-body l label)`

                    `(setf exps (call-loop-fn l (second exps) (rest2 exps)))`

                    `(add-body l label2)))`

                `(add-body l label)))`

    `exps)`

### Unconditional Execution (26.11)
{:#s0080}
{:.h2hd}

The unconditional execution keywords are do and return:

```lisp
(defloop do (l exp exps)
```

    `(add-body l exp)`

    `(loop (if (symbolp (first exps)) (RETURN exps))`

        `(add-body l (pop exps))))`

```lisp
(defloop return (l exp) (add-body l '(return ,exp)))
```

### Miscellaneous Features (26.12)
{:#s0085}
{:.h2hd}

Finally, the miscellaneous features include the keywords `initially` and `finally`, which define the loop prologue and epilogue, and the keyword named, which gives a name to the loop for use by a `return-from` form.
I have omitted the data-type declarations and destructuring capabilities.

```lisp
(defloop initially (l exp exps)
```

    `(push exp (loop-prologue l))`

    `(loop (if (symbolp (first exps)) (RETURN exps))`

        `(push (pop exps) (loop-prologue l))))`

```lisp
(defloop finally (l exp exps)
```

    `(push exp (loop-epilogue l))`

    `(loop (if (symbolp (first exps)) (RETURN exps))`

        `(push (pop exps) (loop-epilogue l))))`

```lisp
(defloop named (l exp) (setf (loop-name l) exp))
```

## 24.6 Sequence Functions
{:#s0090}
{:.h1hd}

Common Lisp provides sequence functions to make the programmer's life easier: the same function can be used for lists, vectors, and strings.
However, this ease of use comes at a cost.
Sequence functions must be written very carefully to make sure they are efficient.
There are three main sources of indeterminacy that can lead to inefficiency: (1) the sequences can be of different types; (2) some functions have keyword arguments; (3) some functions have a `&rest` argument.
Careful coding can limit or eliminate these sources of inefficiency, by making as many choices as possible at compile time and making the remaining choices outside of the main loop.

In this section we see how to implement the new ANSI sequence function `map-into` and the updated function reduce efficiently.
This is essential for those without an ANSI compiler.
Even those who do have access to an ANSI compiler will benefit from seeing the efficiency techniques used here.

Before defining the sequence functions, the macro `once-only` is introduced.

### Once-only: A Lesson in Macrology
{:#s0095}
{:.h2hd}

The macro `once-only` has been around for a long time on various systems, although it didn't make it into the Common Lisp standard.
I include it here for two reasons: first, it is used in the following `funcall-if` macro, and second, if you can understand how to write and when to use `once-only`, then you truly understand macro.

First, you have to understand the problem that `once-only` addresses.
Suppose we wanted to have a macro that multiplies its input by itself:[2](#fn0015)

```lisp
(defmacro square (x) '(* ,x ,x))
```

This definition works fine in the following case:

```lisp
> (macroexpand '(square z)) => (* Z Z)
```

But it doesn't work as well here:

```lisp
> (macroexpand '(square (print (incf i))))
(* (PRINT (INCF I)) (PRINT (INCF I)))
```

The problem is that `i` will get incremented twice, not once, and two different values will get printed, not one.
We need to bind `(print (incf i))` to a local variable before doing the multiplication.
On the other hand, it would be superfluous to bind z to a local variable in the previous example.
This is where `once-only` comes in.
It allows us to write macro definitions like this:

```lisp
(defmacro square (x) (once-only (x) '(* ,x ,x)))
```

and have the generated code be just what we want:

```lisp
> (macroexpand '(square z))
(* Z Z)
> (macroexpand '(square (print (incf i))))
(LET ((G3811 (PRINT (INCF I))))
```

    `(* G3811 G3811))`

You have now learned lesson number one of `once-only` : you know how macros differ from functions when it comes to arguments with side effects, and you now know how to handle this.
Lesson number two comes when you try to write (or even understand) a definition of `once-only`-only when you truly understand the nature of macros will you be able to write a correct version.
As always, the first thing to determine is what a call to `once-only` should expand into.
The generated code should test the variable to see if it is free of side effects, and if so, generate the body as is; otherwise it should generate code to bind a new variable, and use that variable in the body of the code.
Here's roughly what we want:

```lisp
> (macroexpand '(once-only (x) '(* ,x ,x)))
(if (side-effect-free-p x)
```

    `'(* ,x ,x)`

    `'(let ((g001 ,x))`

        `, (let ((x 'g001))`

            `'(* x ,x))))`

where `g001` is a new symbol, to avoid conflicts with the `x` or with symbols in the body.
Normally, we generate macro bodies using backquotes, but if the macro body itself has a backquote, then what?
It is possible to nest backquotes (and [appendix C](B9780080571157500273.xhtml) of *Common Lisp the Language*, 2d edition has a nice discussion of doubly and triply nested backquotes), but it certainly is not trivial to understand.
I recommend replacing the inner backquote with its equivalent using `list` and `quote`:

```lisp
(if (side-effect-free-p x)
```

    `'(* ,x ,x)`

    `(list 'let (list (list 'g001 x))`

        `(let ((x 'g001))`

            `'(* ,x ,x))))`

Now we can write `once-only`.
Note that we have to account for the case where there is more than one variable and where there is more than one expression in the body.

```lisp
(defmacro once-only (variables &rest body)
```

    `"Returns the code built by BODY.
If any of VARIABLES`

    `might have side effects.
they are evaluated once and stored`

    `in temporary variables that are then passed to BODY."`

    `(assert (every #'symbolp variables))`

    `(let ((temps (loop repeat (length variables) collect (gensym))))`

        `'(if (every #'side-effect-free-p (list .,variables))`

            `(progn .,body)`

            `(list 'let`

                `,'(list .@(mapcar #'(lambda (tmp var)`

                    `'(list '.tmp .var))`

                `temps variables))`

                  `(let .(mapcar #'(lambda (var tmp) '(.var ',tmp))`

            `variables temps)`

          `.,body)))))`

```lisp
(defun side-effect-free-p (exp)
```

    `"Is exp a constant, variable, or function,`

    `or of the form (THE type x) where x is side-effect-free?"`

    `(or (constantp exp) (atom exp) (starts-with exp 'function)`

        `(and (starts-with exp 'the)`

            `(side-effect-free-p (third exp)))))`

Here we see the expansion of the call to `once-only` and a repeat of the expansions of two calls to `square`:

```lisp
> (macroexpand '(once-only (x) '(* ,x ,x)))
(IF (EVERY #'SIDE-EFFECT-FREE-P (LIST X))
```

        `(PROGN`

            `'(* ,X ,X))`

        `(LIST 'LET (LIST (LIST 'G3763 X))`

                    `(LET ((X 'G3763))`

                        `'(* ,X ,X))))`

```lisp
> (macroexpand '(square z))
(* Z Z)
> (macroexpand '(square (print (incf i))))
(LET ((G3811 (PRINT (INCF I))))
```

    `(* G3811 G3811))`

This output was produced with `*print-gensym*` setto `nil`.
When this variable is non-nil, uninterned symbols are printed with a prefix `#`:,as in `#:G3811`.
This insures that the symbol will not be interned by a subsequent read.

It is worth noting that Common Lisp automatically handles problems related to multiple evaluation of subforms in setf methods.
See [page 884](B978008057115750025X.xhtml#p884) for an example.

### Avoid Overusing Macros
{:#s0100}
{:.h2hd}

A word to the wise: don't get carried away with macros.
Use macros freely to represent your *problem*, but shy away from new macros in the implementation of your *solution,* unless absolutely necessary.
So, it is good style to introduce a macro, say, `defrule`, which defines rules for your application, but adding macros to the code itself may just make things harder for others to use.

Here is a story.
Before `if` was a standard part of Lisp, I defined my own version of `if`.
Unlike the simple `if`, my version took any number of test/result pairs, followed by an optional else result.
In general, the expansion was:

```lisp
(if *a b c d...x)* => (cond *(a b)* (*c d*) ... (T *x*))
```

My `if` also had one more feature: the symbol `'that'` could be used to refer to the value of the most recent test.
For example, I could write:

```lisp
(if (assoc item a-list)
```

    `(process (cdr that)))`

which would expand into:

```lisp
(LET (THAT)
```

    `(COND`

        `((SETQ THAT (ASSOC ITEM A-LIST)) (PROCESS (CDR THAT)))))`

This was a convenient feature (compare it to the => feature of Scheme's cond, as discussed on [page 778](B9780080571157500224.xhtml#p778)), but it backfired often enough that I eventually gave up on my version of `if`.
Here's why.
I would write code like this:

```lisp
(if (total-score x)
```

    `(print (/ that number-of-trials))`

    `(error "No scores"))`

and then make a small change:

```lisp
(if (total-score x)
```

    `(if *print-scores* (print (/ that number-of-trials)))`

    `(error "No scores"))`

The problem is that the variable `that` now refers to `*print-scores*`, not `(total-score x),` as it did before.
My macro violates referential transparency.
In general, that's the whole point of macros, and it is why macros are sometimes convenient.
But in this case, violating referential transparency can lead to confusion.

### MAP-INTO
{:#s0105}
{:.h2hd}

The function `map-into` is used on [page 632](B9780080571157500182.xhtml#p632).
This function, added for the ANSI version of Common Lisp, is like `map`, except that instead of building a new sequence, the first argument is changed to hold the results.
This section describes how to write a fairly efficient version of `map-into`, using techniques that are applicable to any sequence function.
We'll start with a simple version:

```lisp
(defun map-into (result-sequence function &rest sequences)
```

    `"Destructively set elements of RESULT-SEQUENCE to the results`

    `of applying FUNCTION to respective elements of SEQUENCES."`

    `(replace result-sequence (apply #'map 'list function sequences)))`

This does the job, but it defeats the purpose of `map-into`, which is to avoid generating garbage.
Here's a version that generates less garbage:

```lisp
(defun map-into (result-sequence function &rest sequences)
```

    `"Destructively set elements of RESULT-SEQUENCE to the results`

    `of applying FUNCTION to respective elements of SEQUENCES."`

    `(let ((n (loop for seq in (cons result-sequence sequences)`

                            `minimize (length seq))))`

        `(dotimes (i n)`

            `(setf (elt result-sequence i)`

                `(apply function`

                    `(mapcar #'(lambda (seq) (elt seq i))`

                        `sequences))))))`

There are three problems with this definition.
First, it wastes space: mapcar creates a new argument list each time, only to have the list be discarded.
Second, it wastes time: doing a `setf` of the ith element of a list makes the algorithm *O*(*n2*) instead of *O*(*n*), where *n* is the length of the list.
Third, it is subtly wrong: if `result-sequence` is a vector with a fill pointer, then `map-into` is supposed to ignore `result-sequence's` current length and extend the fill pointer as needed.
The following version fixes those problems:

```lisp
(defun map-into (result-sequence function &rest sequences)
```

    `"Destructively set elements of RESULT-SEQUENCE to the results`

    `of applying FUNCTION to respective elements of SEQUENCES."`

    `(let ((arglist (make-list (length sequences)))`

        `(n (if (listp result-sequence)`

            `most-positive-fixnum`

            `(array-dimension result-sequence 0))))`

      `;; arglist is made into a list of args for each call`

      `;; n is the length of the longest vector`

      `(when sequences`

          `(setf n (min n (loop for seq in sequences`

              `minimize (length seq)))))`

      `;; Define some shared functions:`

      `(flet`

        `((do-one-call (i)`

            `(loop for seq on sequences`

                `for arg on arglist`

                `do (if (listp (first seq))`

                    `(setf (first arg)`

                        `(pop (first seq)))`

                    `(setf (first arg)`

                        `(aref (first seq) i))))`

            `(apply function arglist))`

        `(do-result (i)`

            `(if (and (vectorp result-sequence)`

                `(array-has-fill-pointer-p result-sequence))`

            `(setf (fill-pointer result-sequence)`

    `(max i (fill-pointer result-sequence))))))`

      `(declare (inline do-one-call))`

      `;; Decide if the result is a list or vector,`

      `;; and loop through each element`

      `(if (listp result-sequence)`

        `(loop for i from 0 to (- n 1)`

          `for r on result-sequence`

          `do (setf (first r)`

                `(do-one-call i)))`

        `(loop for i from 0 to (- n 1)`

          `do (setf (aref result-sequence i)`

                `(do-one-call i))`

          `finally (do-result n))))`

      `result-sequence))`

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

```lisp
(loop for seq on sequences
```

    `do (if (listp (first seq))`

        `(%push (pop (first seq)))`

        `(%push (aref (first seq) i))))`

```lisp
(%call function length-sequences)
```

There is a remaining inefficiency, though.
Each sequence is type-checked each time through the loop, even though the type remains constant once it is determined the first time.
Theoretically, we could code separate loops for each combination of types, just as we coded two loops depending on the type of the result sequence.
But that would mean 2*n* loops for *n* sequences, and there is no limit on how large *n* can be.

It might be worth it to provide specialized functions for small values of *n*, and dispatch to the appropriate function.
Here's a start at that approach:

```lisp
(defun map-into (result function &rest sequences)
```

    `(apply`

      `(case (length sequences)`

        `(0 (if (listp result) #'map-into-list-0 #'map-into-vect-0))`

        `(1 (if (listp result)`

          `(if (listp (first sequences))`

              `#'map-into-list-l-list #'map-into-list-1-vect)`

          `(if (listp (first sequences))`

              `#'map-into-vect-l-list #'map-into-vect-l-vect)) )`

        `(2 (if (listp result)`

          `(if (listp (first sequences))`

            `(if (listp (second sequences))`

              `#'map-into-list-2-list-list`

              `#'map-into-list-2-list-vect)`

            `...)))`

        `(t (if (listp result) #'map-into-list-n #'map-into-vect-n)))`

      `result function sequences))`

The individual functions are not shown.
This approach is efficient in execution time, but it takes up a lot of space, considering that `map-into` is a relatively obscure function.
If `map-into` is declared `inline` and the compiler is reasonably good, then it will produce code that just calls the appropriate function.

### REDUCE with :key
{:#s0110}
{:.h2hd}

Another change in the ANSI proposal is to add a : key keyword to `reduce`.
This is a useful addition-in fact, for years I had been using a `reduce-by` function that provided just this functionality.
In this section we see how to add the : key keyword.

At the top level, I define reduce as an interface to the keywordless function `reduce*`.
They are both proclaimed inline, so there will be no overhead for the keywords in normal uses of reduce.

```lisp
(proclaim '(inline reduce reduce*))
```

  `(defun reduce* (fn seq from-end start end key init init-p)`

          `(funcall (if (listp seq) #'reduce-list #'reduce-vect)`

                    `fn seq from-end (or start 0) end key init init-p))`

```lisp
(defun reduce (function sequence &key from-end start end key
```

                              `(initial-value nil initial-value-p))`

```lisp
        (reduce* function sequence from-end start end
```

                                    `key initial-value initial-value-p))`

The easier case is when the sequence is a vector:

```lisp
(defun reduce-vect (fn seq from-end start end key init init-p)
```

        `(when (null end) (setf end (length seq)))`

        `(assert (<= 0 start end (length seq)) (start end)`

                            `"Illegal subsequence of ~  a --- :start ~  d :end ~  d"`

                                  `seq start end)`

      `(case (- end start)`

                  `(0 (if init-p init (funcall fn)))`

                  `(1 (if init-p`

                          `(funcall fn init (funcall-if key (aref seq start)))`

                          `(funcall-if key (aref seq start))))`

                  `(t (if (not from-end)`

                          `(let ((result`

                                  `(if init-p`

                                    `(funcall fn init`

                                      `(funcall-if key (aref seq start)))`

                                  `(funcall`

                                            `fn`

                                                    `(funcall-if key (aref seq start))`

                                                    `(funcall-if key (aref seq (+ start 1)))))))`

                          `(loop for i from (+ start (if init-p 1 2))`

                                          `to (- end 1)`

                                          `do (setf result`

                                              `(funcall`

                                                `fn result`

                                                `(funcall-if key (aref seq i)))))`

                                  `result)`

                          `(let ((result`

                                  `(if init-p`

                              `(funcall`

              `fn`

              `(funcall-if key (aref seq (- end 1)))`

                              `init)`

                    `(funcall`

                            `fn`

                              `(funcall-if key (aref seq (- end 2)))`

                              `(funcall-if key (aref seq (- end 1)))))))`

  `(loop for i from (- end (if init-p 2 3)) downto start`

                  `do (setf result`

                                `(funcall`

                                                                `fn`

                                                                `(funcall-if key (aref seq i))`

                                                                `result)))`

```lisp
result)))))
```

When the sequence is a list, we go to some trouble to avoid computing the length, since that is an *O(n)* operation on lists.
The hardest decision is what to do when the list is to be traversed from the end.
There are four choices:

*   **recurse.** We could recursively walk the list until we hit the end, and then compute the results on the way back up from the recursions.
However, some implementations may have fairly small bounds on the depths of recursive calls, and a system function like reduce should never run afoul of such limitations.
In any event, the amount of stack space consumed by this approach would normally be more than the amount of heap space consumed in the next approach.

*   **reverse.** We could reverse the list and then consider `from-end` true.
The only drawback is the time and space needed to construct the reversed list.

*   **nreverse.** We could destructively reverse the list in place, do the reduce computation, and then destructively reverse the list back to its original state (perhaps with an unwind-protect added).
Unfortunately, this is just incorrect.
The list may be bound to some variable that is accessible to the function used in the reduction.
If that is so, the function will see the reversed list, not the original list.

*   **coerce.** We could convert the list to a vector, and then use `reduce-vect`.
This has an advantage over the reverse approach in that vectors generally take only half as much storage as lists.
Therefore, this is the approach I adopt.

```lisp
(defmacro funcall-if (fn arg)
```

      `(once-only (fn)`

              `'(if .fn (funcall .fn .arg) .arg)))`

```lisp
(defun reduce-list (fn seq from-end start end key init init-p)
```

        `(when (null end) (setf end most-positive-fixnum))`

        `(cond ((> start 0)`

                          `(reduce-list fn (nthcdr start seq) from-end 0`

                                      `(- end start) key init init-p))`

                          `((or (null seq) (eql start end))`

                          `(if init-p init (funcall fn)))`

                          `((= (- end start) 1)`

                          `(if init-p`

                                `(funcall fn init (funcall-if key (first seq)))`

                                `(funcall-if key (first seq))))`

                    `(from-end`

                          `(reduce-vect fn (coerce seq 'vector) t start end`

                                      `key init init-p))`

                                `((null (rest seq))`

                          `(if init-p`

                                `(funcall fn init (funcall-if key (first seq)))`

                                `(funcall-if key (first seq))))`

                    `(t (let ((result`

                    `(if init-p`

                                  `(funcall`

                                                `fn init`

                                                `(funcall-if key (pop seq)))`

                                  `(funcall`

                                                `fn`

                                                `(funcall-if key (pop seq))`

                                                `(funcall-if key (pop seq))))))`

                    `(if end`

                                `(loop repeat (- end (if init-p 1 2)) while seq`

                                  `do (setf result`

                                                `(funcall`

                                                `fn result`

                                          `(funcall-if key (pop seq)))))`

                          `(loop while seq`

                                  `do (setf result`

                              `(funcall`

                                    `fn result`

                                    `(funcall-if key (pop seq)))))`

                          `result)))))`

## 24.7 Exercises
{:#s0115}
{:.h1hd}

**Exercise  24.2 [m]** The function reduce is a very useful one, especially with the key keyword.
Write nonrecursive definitions for append and 1 ength using reduce.
What other common functions can be written with reduce?

**Exercise  24.3** The so-called loop keywords are not symbols in the keyword package.
The preceding code assumes they are all in the current package, but this is not quite right.
Change the definition of `loop` so that any symbol with the same name as a loop keyword acts as a keyword, regardless of the symbol's package.

**Exercise  24.4** Can there be a value for *exp* for which the following expressions are not equivalent?
Either demonstrate such an *exp* or argue why none can exist.

```lisp
(loop for x in list collect *exp*)
(mapcar #'(lambda (x) *exp)* list))
```

**Exercise  24.5** The object-oriented language Eiffel provides two interesting `loop` keywords: `invariant` and `variant`.
The former takes a Boolean-valued expression that must remain true on every iteration of the loop, and the latter takes a integervalued expression that must decrease on every iteration, but never becomes negative.
Errors are signaled if these conditions are violated.
Use def `loop` to implement these two keywords.
Make them generate code conditionally, based on a global flag.

## 24.8 Answers
{:#s0120}
{:.h1hd}

**Answer 24.1**

```lisp
(defvar *queue*)
(defun collect (item) (enqueue item *queue*))
(defmacro with-collection (&body body)
```

          `'(let ((*queue* (make-queue)))`

                                  `,@body`

                      `(queue-contents *queue*)))`

Here's another version that allows the collection variable to be named.
That way, more than one collection can be going on at the same time.

```lisp
(defun collect (item &optional (queue *queue*))
```

            `(enqueue item queue))`

```lisp
(defmacro with-collection ((&optional (queue '*queue*))
```

                                                              `&body body)`

            `'(let ((,queue (make-queue)))`

              `,@body`

            `(queue-contents .queue)))`

**Answer 24.2**

```lisp
(defun append-r (x y)
```

            `(reduce #'cons x :initial-value y :from-end t))`

```lisp
(defun length-r (list)
```

            `(reduce #'+ list :key #'(lambda (x) 1)))`

**Answer 24.4** The difference between `loop` and `mapcar` is that the former uses only one variable `x`, while the latter uses a different `x` each time.
If `x`'s extent is no bigger than its scope (as it is in most expressions) then this makes no difference.
But if any `x` is captured, giving it a longer extent, then a difference shows up.
Consider *exp =*`#'(lambda () x).`

```lisp
> (mapcar #'funcall (loop for x in '(1 2 3) collect
```

                                          `#'(lambda O x)))`

```lisp
(3 3 3)
>(mapcar #'funcall (mapcar #'(lambda (x) #'(lambda () x))
```

                                                    `'(1 2 3)))`

```lisp
(1 2 3)
```

**Answer 24.5**

```lisp
(defvar *check-invariants* t
```

            `"Should VARIANT and INVARIANT clauses in LOOP be checked?")`

```lisp
(defloop invariant (l exp)
```

            `(when *check-invariants*`

                                `(add-body l '(assert .exp () "Invariant violated."))))`

```lisp
(defloop variant (l exp)
```

  `(when *check-invariants*`

                      `(let ((var (gensym "INV")))`

                                `(add-var l var nil)`

                                `(add-body l '(setf ,var (update-variant .var .exp))))))`

          `(defun update-variant (old new)`

            `(assert (or (null old) (< new old)) ()`

                                `"Variant is not monotonically decreasing")`

            `(assert (> new 0) () "Variant is no longer positive")`

          `new)`

Here's an example:

```lisp
(defun gcd2 (a b)
```

            `"Greatest common divisor.
For two positive integer arguments."`

            `(check-type a (integer 1))`

            `(check-type b (integer 1))`

            `(loop with x = a with y = b`

                                `invariant (and (> x 0) (> y 0)) ;; (= (gcd x y) (gcd a b))`

                                `variant (max x y)`

                                `until (= x y)`

                                `do (if (> x y) (decf x y) (decf y x))`

                                `finally (return x)))`

Here the invariant is written semi-informally.
We could include the calls to `gcd`, but that seems to be defeating the purpose of `gcd2`, so that part is left as a comment.
The idea is that the comment should help the reader prove the correctness of the code, and the executable part serves to notify the lazy reader when something is demonstrably wrong at run time.

----------------------

[1](#xfn0010) Or in the user package in non-ANSI systems.
!!!(p) {:.ftnote1}

[2](#xfn0015) As was noted before, the proper way to do this is to proclaim squa re as an inline function, not a macro, but please bear with the example.
!!!(p) {:.ftnote1}

