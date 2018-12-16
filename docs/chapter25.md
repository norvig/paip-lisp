# Chapter 25
## Troubleshooting

> Perhaps if we wrote programs from childhood on, as adults we'd be able to read them.

> -Alan Perlis

When you buy a new appliance such as a television, it comes with an instruction booklet that lists troubleshooting hints in the following form:

**PROBLEM**: Nothing works.

**Diagnosis**: Power is off.

**Remedy:** Plug in outlet and turn on power switch.

If your Lisp compiler came without such a handy instruction booklet, this chapter may be of some help.
It lists some of the most common difficulties that Lisp programmers encounter.

## 25.1 Nothing Happens

**PROBLEM:** You type an expression to Lisp's read-eval-print loop and get no response-no result, no prompt.

**Diagnosis:** There are two likely reasons why output wasn't printed: either Lisp is still doing read or it is still doing `eval`.
These possibilities can be broken down further into four cases:

**Diagnosis:** If the expression you type is incomplete, Lisp will wait for more input to complete it.
An expression can be incomplete because you have left off a right parenthesis (or inserted an extra left parenthesis).
Or you may have started a string, atom, or comment without finishing it.
This is particularly hard to spot when the error spans multiple lines.
A string begins and ends with double-quotes: `"string"`; an atom containing unusual characters can be delimited by vertical bars: `| AN ATOM |` ; and a comment can be of the form `# | a comment | #`.
Here are four incomplete expressions:

```lisp
(+ (* 3 (sqrt 5) 1)
```

`(format t "~&X=~a, Y=~a.
x y)`

```lisp
(get '|strange-atom 'prop)
(if (= x 0) #1 test if x is zero
        y
        x)
```

**Remedy:** Add a ), ", `|`, and `| #`, respectively.
Or hit the interrupt key and type the input again.

**Diagnosis:** Your program may be waiting for input.

**Remedy:** Never do a `(read)` without first printing a prompt of some kind.
If the prompt does not end with a newline, a call to `finish-output` is also in order.
In fact, it is a good idea to call a function that is at a higher level than `read`.
Several systems define the function `prompt-and-read`.
Here is one version:

```lisp
(defun prompt-and-read (ctl-string &rest args)
  "Print a prompt and read a reply."
  (apply #'format t ctl-string args)
  (finish-output)
  (read))
```

**Diagnosis:** The program may be caught in an infinite loop, either in an explicit `loop` or in a recursive function.

**Remedy:** Interrupt the computation, get a back trace, and see what functions are active.
Check the base case and loop variant on active functions and loops.

**Diagnosis:** Even a simple expression like (`mapc #'sqrt list`) or (`length list`) will cause an infinite loop if `list` is an infinite list-that is, a list that has some tail that points back to itself.

**Remedy:** Be very careful any time you modify a structure with `nconc`, `delete`, `setf`, and so forth.

**PROBLEM:** You get a new prompt from the read-eval-print loop, but no output was printed.

**Diagnosis:** The expression you evaluated must have returned no values at all, that is, the result `(values)`.

## 25.2 Change to Variable Has No Effect

**PROBLEM:** You redefined a variable, but the new value was ignored.

**Diagnosis:** Altering a variable by editing and re-evaluating a `defvar` form will not change the variable's value, `defvar` only assigns an initial value when the variable is unbound.

**Remedy:** Use setf to update the variable, or change the `defvar` to a `defparameter`.

**Diagnosis:** Updating a locally bound variable will not affect a like-named variable outside that binding.
For example, consider:

```lisp
(defun check-ops (*ops*)
  (if (null *ops*)
          (setf *ops* *default-ops*))
  (mapcar #'check-op *ops*))
```

If `check - ops` is called with a null argument, the `*ops*` that is a parameter of `check - ops` will be updated, but the global `*ops*` will not be, even if it is declared special.

**Remedy:** Don't shadow variables you want to update.
Use a different name for the local variable.
It is important to distinguish special and local variables.
Stick to the naming convention for special variables: they should begin and end with asterisks.
Don't forget to introduce a binding for all local variables.
The following excerpt from a recent textbook is an example of this error:

```lisp
(defun test ()
  (setq x 'test-data)      :*Warning!*
  (solve-problem x))        :*Don't do this.*
```

This function should have been written:

```lisp
(defun test ()
  (let ((x 'test-data))      :*Do this instead.*
      (solve-problem x)))
```

## 25.3 Change to Function Has No Effect

**PROBLEM:** You redefined a function, but the change was ignored.

**Diagnosis:** When you change a macro, or a function that has been declared inline, the change will not necessarily be seen by users of the changed function.
(It depends on the implementation.)

**Remedy:** Recompile after changing a macro.
Don't use inline functions until everything is debugged.
(`Use (declare (notinline f)`) to cancel an inline declaration).

**Diagnosis:** If you change a normal (non-inline) function, that change *will* be seen by code that refers to the function by *name*, but not by code that refers to the old value of the function itself.
Consider:

```lisp
(defparameter *scorer* #'score-fn)
(defparameter *printer* 'print-fn)
(defun show (values)
  (funcall *printer*
      (funcall *scorer* values)
      (reduce #'better values)))
```

Now suppose that the definitions of `score - fn, print - fn`, and `better` are all changed.
Does any of the prior code have to be recompiled?
The variable *`printer`* can stay as is.
When it is funcalled, the symbol `print-fn` will be consulted for the current functional value.
Within show, the expression # ' `better` is compiled into code that will get the current version of `better`, so it too is safe.
However, the variable *`scorer`* must be changed.
Its value is the old definition of `score-fn`.

**Remedy:** Re-evaluate the definition of *`scorer`*.
It is unfortunate, but this problem encourages many programmers to use symbols where they really mean functions.
Symbols will be coerced to the global function they name when passed to `funcall`or `apply`, but this can be the source of another error.
In the following example, the symbol `local - fn` will not refer to the locally bound function.
One needs to use `#'local - fn` to refer to it.

```lisp
(flet ((local-fn (x) ...))
  (mapcar 'local-fn list))
```

**Diagnosis:** If you changed the name of a function, did you change the name every-where?
For example, if you decide to change the name of `print-fn` to `print-function` but forget to change the value of *`printer`*, then the old function will be called.

**Remedy:** Use your editor's global replace command.
To be even safer, redefine obsolete functions to call `error`.
The following function is handy for this purpose:

```lisp
(defun make-obsolete (fn-name)
  "Print an error if an obsolete function is called."
  (setf (symbol-function fn-name)
        #'(lambda (&rest args)
              (declare (ignore args))
              (error "Obsolete function."))))
```

**Diagnosis:** Are you using `labels` and `flet` properly?
Consider again the function `replace-?-vars`, which was defined in [section 11.3](B978008057115750011X.xhtml#s0025) to replace an anonymous logic variable with a unique new variable.

```lisp
(defun replace-?-vars (exp)
  "Replace any ? within exp with a var of the form ?123."
  (cond ((eq exp '?) (gensym "?"))
      ((atom exp) exp)
      (t (cons (replace-?-vars (first exp))
          (replace-?-vars (rest exp))))))
```

It might occur to the reader that gensyming a different variable each time is wasteful.
The variables must be unique in each clause, but they can be shared across clauses.
So we could generate variables in the sequence `?1, ?2, ...`, intern them, and thus reuse these variables in the next clause (provided we warn the user never to use such variable names).
One way to do that is to introduce a local variable to hold the variable number, and then a local function to do the computation:

```lisp
(defun replace-?-vars (exp)
  "Replace any ? within exp with a var of the form ?123."
  ;;*** Buggy Version ***
  (let ((n 0))
      (flet
        ((replace-?-vars (exp)
```

`          (cond ((eq exp '?) (symbol '?
(incf n)))`

```lisp
          ((atom exp) exp)
          (t (cons (replace-?-vars (first exp))
                  (replace-?-vars (rest exp)))))))
      (replace-?-vars exp))))
```

This version doesn't work.
The problem is that `flet`, like `let`, defines a new function within the body of the `flet` but not within the new function's definition.
So two lessons are learned here: use `labels` instead of `flet` to define recursive functions, and don't shadow a function definition with a local definition of the same name (this second lesson holds for variables as well).
Let's fix the problem by changing `labels` to `flet` and naming the local function `recurse`:

```lisp
(defun replace-?-vars (exp)
  "Replace any ? within exp with a var of the form ?123."
  ;;*** Buggy Version ***
  (let ((n 0))
      (labels
        ((recurse (exp)
```

`          (cond ((eq exp '?) (symbol '?
(incf n)))`

```lisp
          ((atom exp) exp)
          (t (cons (replace-?-vars (first exp))
            (replace-?-vars (rest exp)))))))
        (recurse exp))))
```

Annoyingly, this version still doesn't work!
This time, the problem is carelessness; we changed the `replace- ? - vars to recurse` in two places, but not in the two calls in the body of `recurse`.

**Remedy:** In general, the lesson is to make sure you call the right function.
If there are two functions with similar effects and you call the wrong one, it can be hard to see.
This is especially true if they have similar names.

**PROBLEM:** Your closures don't seem to be working.

**Diagnosis:** You may be erroneously creating a lambda expression by consing up code.
Here's an example from a recent textbook:

```lisp
(defun make-specialization (c)
  (let (pred newc)
    ...
  (setf (get newc 'predicate)
    '(lambda (obj)    :Warning
      (and ,(cons pred '(obj))    :Don't do this.
      (apply '.(get c 'predicate) (list obj)))))
    ...))
```

Strictly speaking, this is legal according to *Common Lisp the Language*, although in ANSI Common Lisp it will *not* be legal to use a list beginning with `lambda` as a function.
But in either version, it is a bad idea to do so.
A list beginning with `lambda` is just that: a list, not a closure.
Therefore, it cannot capture lexical variables the way a closure does.

**Remedy:** The correct way to create a closure is to evaluate a call to the special form `function`, or its abbreviation, # '.
Here is a replacement for the code beginning with '(`lambda ....` Note that it is a closure, closed over `pred` and `c`.
Also note that it gets the `predicate` each time it is called; thus, it is safe to use even when predicates are being changed dynamically.
The previous version would not work when a predicate is changed.

```lisp
#'(lambda (obj)            ; *Do this instead.*
      (and (funcall pred obj)
          (funcall (get c 'predicate) obj)))
```

It is important to remember that `function` (and thus # ') is a special form, and thus only returns the right value when it is evaluated.
A common error is to use # ' notation in positions that are not evaluated:

```lisp
(defvar *obscure-fns* '(#'cis #'cosh #'ash #'bit-orc2)) ; *wrong*
```

This does not create a list of four functions.
Rather, it creates a list of four sublists; the first sublist is (`function cis`).
It is an error to funcall or apply such an object.
The two correct ways to create a list of functions are shown below.
The first assures that each function special form is evaluated, and the second uses function names instead of functions, thus relying on `funcall` or `apply` to coerce the names to the actual functions.

```lisp
(defvar *obscure-fns* (list #'cis #'cosh #'ash #'bit-orc2))
(defvar *obscure-fns* '(cis cosh ash bit-orc2))
```

Another common `error` is to expect # ' `if` or # ' `or` to return a function.
This is an error because special forms are just syntactic markers.
There is no function named `if` or `or`; they should be thought of as directives that tell the compiler what to do with a piece of code.

By the way, the function `make` - `specialization` above is bad not only for its lack of `function` but also for its use of backquote.
The following is a better use of backquote:

```lisp
'(lambda (obj)
    (and (,pred obj)
        (,(get c 'predicate) obj)))
```

## 25.4 Values Change "by Themselves"

**PROBLEM:** You deleted/removed something, but it didn't take effect.
For example:

`> (setf numbers '(1 2 3 4 5))`=> `(1 2 3 4 5)`

`> (remove 4 numbers)`=> `(1 2 3 5)`

`> numbers`=> `(1 2 3 4 5)`

`> (delete 1 numbers)`=> `(2 3 4 5)`

`> numbers`=> `(1 2 3 4 5)`

**Remedy:** Use (`setf numbers` (`delete 1 numbers`)).
Note that `remove` is a non-destructive function, so it will never alter its arguments, `delete` is destructive, but when asked to delete the first element of a list, it returns the rest of the list, and thus does not alter the list itself.
That is why `setf` is necessary.
Similar remarks hold for `nconc`, `sort`, and other destructive operations.

**PROBLEM:** You created a hundred different structures and changed a field in one of them.
Suddenly, all the other ones magically changed!

**Diagnosis:** Different structures may share identical subfields.
For example, suppose you had:

```lisp
(defstruct block
  (possible-colors '(red green blue))
  ...)
  (setf bl (make-block))
  (setf b2 (make-block))
  ...
  (delete 'green (block-possible-colors bl))
```

Both `b1` and `b2` share the initial list of possible colors.
The `delete` function modifies this shared list, so `green` is deleted from `b2`'s possible colors list just as surely as it is deleted from `b1`'s.

**Remedy:** Don't share pieces of data that you want to alter individually.
In this case, either use `remove` instead of `delete`, or allocate a different copy of the list to each instance:

```lisp
(defstruct block
  (possible-colors (list 'red 'green 'blue))
  ...)
```

Remember that the initial value field of a defstruct is an expression that is evaluated anew each time `make-block` is called.
It is incorrect to think that the initial form is evaluated once when the `defstruct` is defined.

## 25.5 Built-In Functions Don't Find Elements

**PROBLEM:** You tried (`find item list`), and you know it is there, but it wasn't found.

**Diagnosis:** By default, many built-in functions use `eql` as an equality test, `find` is one of them.
If `item` is, say, a list that is `equal` but not `eql` to one of the elements of `list`, it will not be found.

**Remedy:** Use (`find item list :test #'equal`)

**Diagnosis:** If the `item` is nil, then nil will be returned whether it is found or not.

**Remedy:** Use `member` or `position` instead of `find` whenever the item can be nil.

## 25.6 Multiple Values Are Lost

**PROBLEM:** You only get one of the multiple values you were expecting.

**Diagnosis:** In certain contexts where a value must be tested by Lisp, multiple values are discarded.
For example, consider:

```lisp
(or (mv-1 x) (mv-2 x))
(and (mv-1 x) (mv-2 x))
(cond ((mv-1 x))
  (t (mv-2 x)))
```

In each case, if `mv-2` returns multiple values, they will all be passed on.
But if `mv-1` returns multiple values, only the first value will be passed on.
This is true even in the last clause of a cond.
So, while the final clause (`t (mv-2 x)`) passes on multiple values, the final clause (`(mv-2 x )`) would not.

**Diagnosis:** Multiple values can be inadvertently lost in debugging as well.
Suppose I had:

```lisp
(multiple-value-bind (a b c)
  (mv-1 x)
    ...)
```

Now, if I become curious as to what `mv -1` returns, I might change this code to:

```lisp
(multiple-value-bind (a b c)
  (print (mv-1 x)) ;*** debugging output
  ...)
```

Unfortunately, `print` will see only the first value returned by `mv-1`, and will return only that one value to be bound to the variable a.
The other values will be discarded, and `b` and `c` will be bound to `nil`.

## 25.7 Declarations Are Ignored

**PROBLEM:** Your program uses 1024 x 1024 arrays of floating-point numbers.
But you find that it takes 15 seconds just to initialize such an array to zeros!
Imagine how inefficient it is to actually do any computation!
Here is your function that zeroes an array:

```lisp
(defun zero-array (arr)
  "Set the 1024x1024 array to all zeros."
  (declare (type (array float) arr))
  (dotimes (i 1024)
    (dotimes (j 1024)
      (setf (aref arr i j) 0.0))))
```

**Diagnosis:** The main problem here is an ineffective declaration.
The type (`array float`) does not help the compiler, because the array could be displaced to an array of another type, and because `float` encompasses both single- and double-precision floating-point numbers.
Thus, the compiler is forced to allocate storage for a new copy of the number 0.0 for each of the million elements of the array.
The function is slow mainly because it generates so much garbage.

**Remedy:** The following version uses a much more effective type declaration: a simple array of single-precision numbers.
It also declares the size of the array and turns safety checks off.
It runs in under a second on a SPARCstation, which is slower than optimized C, but faster than unoptimized C.

```lisp
(defun zero-array (arr)
  "Set the array to all zeros."
  (declare (type (simple-array single-float (1024 1024)) arr)
          (optimize (speed 3) (safety 0)))
  (dotimes (i 1024)
    (dotimes (j 1024)
      (setf (aref arr i j) 0.0))))
```

Another common error is to use something like (`simple-vector fixnum`) asatype specifier.
It is a quirk of Common Lisp that the `simple-vector` type specifier only accepts a size, not a type, while the `array, vector` and `simple-array` specifiers all accept an optional type followed by an optional size or list of sizes.
To specify a simple vector of fixnums, use (`simple-array fixnum (*)`).

To be precise, `simple-vector` means (`simple-array t (*)`).
This means that `simple-vector` cannot be used in conjunction with any other type specifier.
A common mistake is to think that the type (`and simple-vector (vector fixnum)`) is equivalent to (`simple-array fixnum (*)`), a simple, one-dimensional vector of fixnums.
Actually, it is equivalent to (`simple-array t (*)`), a simple one-dimensional array of any type elements.
To eliminate this problem, avoid `simple- vector` altogether.

## 25.8 My Lisp Does the Wrong Thing

When all else fails, it is tempting to shift the blame for an error away from your own code and onto the Common Lisp implementation.
It is certainly true that errors are found in existing implementations.
But it is also true that most of the time, Common Lisp is merely doing something the user did not expect rather than something that is in error.

For example, a common "bug report" is to complain about read - `from- string`.
A user might write:

```lisp
(read-from-string "a b c" :start 2)
```

expecting the expression to start reading at position `2` and thus return `b`.
In fact, this expression returns a.
The angry user thinks the implementation has erroneously ignored the :`start` argument and files a bug report,[1](#fn0010) only to get back the following explanation:

The function `read-from-string` takes two optional arguments, `eof-errorp` and `eof-value`, in addition to the keyword arguments.
Thus, in the expression above, : `start` is taken as the value of `eof-errorp`, with `2` as the value of `eof-value`.
The correct answer is in fact to read from the start of the string and return the very first form, a.

The functions `read-from-string` and `parse-namestring` are the only built-in functions that have this problem, because they are the only ones that have both optional and keyword arguments, with an even number of optional arguments.
The functions `write-line` and `write-string` have keyword arguments and a single optional argument (the stream), so if the stream is accidently omitted, an error will be signaled.
(If you type (`write-line str :start 4`), the system will complain either that : `start` is not a stream or that 4 is not a keyword.)

The moral is this: functions that have both optional and keyword arguments are confusing.
Take care when using existing functions that have this problem, and abstain from using both in your own functions.

## 25.9 How to Find the Function You Want

Veteran Common Lisp programmers often experience a kind of software *d&eacute;j&agrave; vu:* they believe that the code they are writing could be done by a built-in Common Lisp function, but they can't remember the name of the function.

Here's an example: while coding up a problem I realized I needed a function that, given the lists (`a b c d`) and (`c d`), would return (`a b`), that is, the part of the first list without the second list.
I thought that this was the kind of function that might be in the standard, but I didn't know what it would be called.
The desired function is similar to `set-difference`, so I looked that up in the index of *Common Lisp the Language* and was directed to page 429.
I browsed through the section on "using lists as sets" but found nothing appropriate.
However, I was reminded of the function `butlast`, which is also similar to the desired function.
The index directed me to page 422 for `butlast`, and on the same page I found `ldiff`, which was exactly the desired function.
It might have been easier to find (and remember) if it were called `list-difference`, but the methodology of browsing near similar functions paid off.

If you think you know part of the name of the desired function, then you can use apropos to find it.
For example, suppose I thought there was a function to push a new element onto the front of an array.
Looking under `array, push-array`, and `array - push` in the index yields nothing.
But I can turn to Lisp itself and ask:

```lisp
> (apropos "push")
PUSH                              Macro          (VALUE PLACE), plist
PUSHNEW                        Macro          (VALUE PLACE &KEY ...), plist
VECTOR-PUSH                function    (NEW-ELEMENT VECTOR), plist
VECTOR-PUSH-EXTEND  function    (DATA VECTOR &OPTIONAL ...), plist
```

This should be enough to remind me that `vector-push` is the answer.
If not, I can get more information from the manual or from the online functions `documentation` or `describe`:

```lisp
> (documentation 'vector-push 'function)
"Add NEW-ELEMENT as an element at the end of VECTOR.
The fill pointer (leader element 0) is the index of the next
```

`element to be added.
If the array is full, VECTOR-PUSH returns`

```lisp
NIL and the array is unaffected; use VECTOR-PUSH-EXTEND instead
if you want the array to grow automatically."
```

Another possibility is to browse through existing code that performs a similar purpose.
That way, you may find the exact function you want, and you may get additional ideas on how to do things differently.

## 25.10 Syntax of LOOP

`loop` by itself is a powerful programming language, one with a syntax quite different from the rest of Lisp.
It is therefore important to exercise restraint in using `loop`, lest the reader of your program become lost.
One simple rule for limiting the complexity of `loops` is to avoid the `with` and and keywords.
This eliminates most problems dealing with binding and scope.

When in doubt, macro-expand the loop to see what it actually does.
But if you need to macro-expand, then perhaps it would be clearer to rewrite the loop with more primitive constructs.

## 25.11 Syntax of COND

For many programmers, the special form cond is responsible for more syntax errors than any other, with the possible exception of `loop`.
Because most cond-clause start with two left parentheses, beginners often come to the conclusion that every clause must.
This leads to errors like the following:

```lisp
(let ((entry (assoc item list)))
  (cond ((entry (process entry)))
          ...))
```

Here entry is a variable, but the urge to put in an extra parenthesis means that the cond-clause attempts to call entry as a function rather than testing its value as a variable.

The opposite problem, leaving out a parenthesis, is also a source of error:

```lisp
(cond (lookup item list)
  (t nil))
```

In this case, `lookup` is accessed as a variable, when the intent was to call it as a function.
In Common Lisp this will usually lead to an unbound variable error, but in Scheme this bug can be very difficult to pin down: the value of `lookup` is the function itself, and since this is not null, the test will succeed, and the expression will return `list` without complaining.

The moral is to be careful with cond, especially when using Scheme.
Note that `if` is much less error prone and looks just as nice when there are no more than two branches.

## 25.12 Syntax of CASE

In a `case` special form, each clause consists of a key or list of keys, followed by the value of that case.
The thing to watch out for is when the key is `t`, `otherwise`, or `nil`.
For example:

```lisp
(case letter
  (s ...)
  (t ...)
  (u ...))
```

Here the t is taken as the default clause; it will always succeed, and all subsequent clauses will be ignored.
Similarly, using a () `ornil` as a key will not have the desired effect: it will be interpreted as an empty key list.
If you want to be completely safe, you can use a list of keys for every clause.[2](#fn0015) This is a particularly good idea when you write a macro that expands into a `case`.
The following code correctly tests for `t` and `nil` keys:

```lisp
(case letter
  ((s) ...)
  ((t) ...)
  ((u) ...)
  ((nil) ...))
```

## 25.13 Syntax of LET and LET*

A common error is leaving off a layer of parentheses in `let`, just like in cond.
Another error is to refer to a variable that has not yet been bound in a `let`.
To avoid this problem, use `let*` whenever a variable's initial binding refers to a previous variable.

## 25.14 Problems with Macros

In [section 3.2](B9780080571157500030.xhtml#s0015) we described a four-part approach to the design of macros:

*   Decide if the macro is really necessary.

*   Write down the syntax of the macro.

*   Figure out what the macro should expand into.

*   Use `defmacro` to implement the syntax/expansion correspondence.
This section shows the problems that can arise in each part, starting with the first:

*   Decide if the macro is really necessary.

Macros extend the rules for evaluating an expression, while function calls obey the rules.
Therefore, it can be a mistake to define too many macros, since they can make it more difficult to understand a program.
A common mistake is to define macros that *do not* violate the usual evaluation rules.
One recent book on AI programming suggests the following:

```lisp
(defmacro binding-of (binding)    ; *Warning!*
    '(cadr .binding))                          ; *Don't do this.*
```

The only possible reason for this macro is an unfounded desire for efficiency.
Always use an `inline` function instead of a macro for such cases.
That way you get the efficiency gain, you have not introduced a spurious macro, and you gain the ability to `apply` or `map` the function # ' `binding - of`, something you could not do with a macro:

```lisp
(proclaim '(inline binding-of))
(defun binding-of (binding)    ; *Do this instead.*
  (second binding))
```

*   Write down the syntax of the macro.

Try to make your macro follow conventions laid down by similar macros.
For example, if your macro defines something, it should obey the conventions of `defvar, defstruct, defmacro,` and the rest: start with the letters `def`, take the name of the thing to be defined as the first argument, then a lambda-list if appropriate, then a value or body.
It would be nice to allow for optional declarations and documentation strings.

If your macro binds some variables or variablelike objects, use the conventions laid down by `let, let*,` and `labels`: allow for a list of variable or ( *variable init-val)* pairs.
If you are iterating over some kind of sequence, follow `dotimes` and `dolist`.
For example, here is the syntax of a macro to iterate over the leaves of a tree of conses:

```lisp
(defmacro dotree ((var tree &optional result) &body body)
  "Perform body with var bound to every leaf of tree,
```

`  then return result.
Return and Go can be used in body."`

```lisp
  ...)
```

*   Figure out what the macro should expand into.

*   Use defmacro to implement the syntax/expansion correspondence.

There are a number of things to watch out for in figuring out how to expand a macro.
First, make sure you don't shadow local variables.
Consider the following definition for `pop - end`, a function to pop off and return the last element of a list, while updating the list to no longer contain the last element.
The definition uses `last1`, which was defined on page 305 to return the last element of a list, and the built-in function `nbutlast` returns all but the last element of a list, destructively altering the list.

```lisp
(defmacro pop-end (place)    ; *Warning!Buggy!*
  "Pop and return last element of the list in PLACE."
  '(let ((result (lastl .place)))
      (setf .place (nbutlast .place))
      result))
```

This will do the wrong thing for (`pop-end result`), or for other expressions that mention the variable `result`.
The solution is to use a brand new local variable that could not possibly be used elsewhere:

```lisp
(defmacro pop-end (place)    ; *Less buggy*
  "Pop and return last element of the list in PLACE."
  (let ((result (gensym)))
  '(let ((,result (lastl ,place)))
    (setf ,place (nbutlast ,place))
      ,result)))
```

There is still the problem of shadowing local *functions.* For example, a user who writes:

```lisp
(flet ((lastl (x) (sqrt x)))
  (pop-end list)
  ...)
```

will be in for a surprise, pop-end will expand into code that calls `lastl`, but since `lastl` has been locally defined to be something else, the code won't work.
Thus, the expansion of the macro violates referential transparency.
To be perfectly safe, we could try:

```lisp
(defmacro pop-end (place)    ; *Less buggy*
  "Pop and return last element of the list in PLACE."
  (let ((result (gensym)))
    '(let ((.result (funcall .#'lastl .place)))
      (setf .place (funcall .#'nbutlast .place))
        ,result)))
```

This approach is sometimes used by Scheme programmers, but Common Lisp programmers usually do not bother, since it is rarer to define local functions in Common Lisp.
Indeed, in *Common Lisp the Language*, 2d edition, it was explicitly stated (page 260) that a user function cannot redefine or even bind any built-in function, variable, or macro.
Even if it is not prohibited in your implementation, redefining or binding a built-in function is confusing and should be avoided.

Common Lisp programmers expect that arguments will be evaluated in left-to-right order, and that no argument is evaluated more than once.
Our definition of `pop-end` violates the second of these expectations.
Consider:

```lisp
(pop-end (aref lists (incf i))) =
(LET ((#:G3096 (LAST1 (AREF LISTS (INCF I)))))
  (SETF (AREF LISTS (INCF I)) (NBUTLAST (AREF LISTS (INCF I))))
  #:G3096)
```

This increments `i` three times, when it should increment it only once.
We could fix this by introducing more local variables into the expansion:

```lisp
(let* ((templ (incf i))
      (temp2 (AREF LISTS temp1))
      (temp3 (LAST1 temp2)))
  (setf (aref lists templ) (nbutlast temp2))
  temp3)
```

This kind of left-to-right argument processing via local variables is done automatically by the Common Lisp setf mechanism.
Fortunately, the mechanism is easy to use.
We can redefine `pop-end` to call `pop` directly:

```lisp
(defmacro pop-end (place)
  "Pop and return last element of the list in PLACE."
  '(pop (last ,place)))
```

Now all we need to do is define the `setf` method for `last`.
Here is a simple definition.
It makes use of the function `last2`, which returns the last two elements of a list.
In ANSI Common Lisp we could use (`last list 2`), but with a pre-ANSI compiler we need to define `last2`:

```lisp
(defsetf last (place) (value)
  '(setf (cdr (last2 .place)) .value))
(defun last2 (list)
  "Return the last two elements of a list."
  (if (null (rest2 list))
      list
      (last2 (rest list))))
```

Here are some macro-expansions of calls to `pop-end` and to the `setf` method for `last`.
Different compilers will produce different code, but they will always respect the left-to-right, one-evaluation-only semantics:

```lisp
> (pop-end (aref (foo lists) (incf i))) =
(LET ((G0128 (AREF (FOO LISTS) (SETQ I (+ I 1)))))
  (PROG1
  (CAR (LAST G0128))
  (SYS:SETCDR (LAST2 G0128) (CDR (LAST G0128)))))
> (setf (last (append x y)) 'end) =
(SYS:SETCDR (LAST2 (APPEND X Y)) 'END)
```

Unfortunately, there is an error in the `setf` method for `last`.
It assumes that the list will have at least two elements.
If the list is empty, it is probably an error, but if a list has exactly one element, then (`setf` (`last`*list) val)* should have the same effect as (`setf`*list val).* But there is no way to do that with `defsetf`, because the `setf` method defined by `defsetf` never sees *list* itself.
Instead, it sees a local variable that is automatically bound to the value of *list.* In other words, `defsetf` evaluates the *list* and *val* for you, so that you needn't worry about evaluating the arguments out of order, or more than once.

To solve the problem we need to go beyond the simple `defsetf` macro and delve into the complexities of `define-setf-method`, one of the trickiest macros in all of Common Lisp.
`define-setf-method` defines a setf method not by writing code directly but by specifying five values that will be used by Common Lisp to write the code for a call to `setf`.
The five values give more control over the exact order in which expressions are evaluated, variables are bound, and results are returned.
The five values are: (1) a list of temporary, local variables used in the code; (2) a list of values these variables should be bound to; (3) a list of one variable to hold the value specified in the call to `setf`; (4) code that will store the value in the proper place; (5) code that will access the value of the place.
This is necessary for variations of `setf` like `inef` and `pop`, which need to both access and store.

In the following `setf` method for `last`, then, we are defining the meaning of (`setf` (`last place`) `value`).
We keep track of all the variables and values needed to evaluate `place`, and add to that three more local variables: `last2`-var will hold the last two elements of the list, `last2`-p will be true only if there are two or more elements in the list, and `last-var` will hold the form to access the last element of the list.
We also make up a new variable, `result`, to hold the `value`.
The code to store the value either modifies the cdr of `last2-var`, if the list is long enough, or it stores directly into `place`.
The code to access the value just retrieves `last - var`.

```lisp
(define-setf-method last (place)
  (multiple-value-bind (temps vais stores store-form access-form)
        (get-setf-method place)
    (let ((result (gensym))
          (last2-var (gensym))
          (last2-p (gensym))
          (last-var (gensym)))
        ;; Return 5 vais: temps vais stores store-form access-form
        (values
          '(.@temps .last2-var .last2-p .last-var)
          '(.@vais (last2 .access-form)
            (= (length .last2-var) 2)
            (if .last2-p (rest .last2-var) .access-form))
          (list result)
          '(if .last2-p
            (setf (cdr .last2-var) .result)
            (let ((.(first stores) .result))
              .store-form))
          last-var))))
```

It should be mentioned that `setf` methods are very useful and powerful things.
It is often better to provide a `setf` method for an arbitrary function, `f`, than to define a special setting function, say, `set-f`.
The advantage of the `setf` method is that it can be used in idioms like `incf` and `pop`, in addition to `setf` itself.
Also, in ANSI Common Lisp, it is permissible to name a function with # ' (`setf f`), so you can also use map or apply the `setf` method.
Most `setf` methods are for functions that just access data, but it is permissible to define `setf` methods for functions that do any computation whatsoever.
As a rather fanciful example, here is a `setf` method for the square-root function.
It makes (`setf (sqrt x) 5`) be almost equivalent to (`setf x (* 5 5)`) ; the difference is that the first returns 5 while the second returns 25.

```lisp
(define-setf-method sqrt (num)
  (multiple-value-bind (temps vals stores store-form access-form)
        (get-setf-method num)
    (let ((store (gensym)))
        (values temps
                    vals
                    (list store)
                    '(let ((,(first stores) (* .store .store)))
                        ,store-form
                        ,store)
                    '(sqrt .access-form)))))
```

Turning from `setf` methods back to macros, another hard part about writing portable macros is anticipating what compilers might warn about.
Let's go back to the `dotree` macro.
Its definition might look in part like this:

```lisp
(defmacro dotree ((var tree &optional result) &body body)
  "Perform body with var bound to every leaf of tree.
```

`  then return result.
Return and Go can be used in body."`

```lisp
  '(let ((.var))
      ...
      ,@body))
```

Now suppose a user decides to count the leaves of a tree with:

```lisp
(let ((count 0))
    (dotree (leaf tree count)
        (incf count)))
```

The problem is that the variable `leaf` is not used in the body of the macro, and a compiler may well issue a warning to that effect.
To make matters worse, a conscientious user might write:

```lisp
(let ((count 0))
  (dotree (leaf tree count)
    (declare (ignore leaf))
      (incf count)))
```

The designer of a new macro must decide if declarations are allowed and must make sure that compiler warnings will not be generated unless they are warranted.

Macros have the full power of Lisp at their disposal, but the macro designer must remember the purpose of a macro is to translate macro code into primitive code, and not to do any computations.
Consider the following macro, which assumes that `translate - rule-body` is defined elsewhere:

`(defmacro defrule (name &body body)    ; Warning!
buggy!`

```lisp
  "Define a new rule with the given name."
  (setf (get name 'rule)
        '#'(lambda O ,(translate-rule-body body))))
```

The idea is to store a function under the `rule` property of the rule's name.
But this definition is incorrect because the function is stored as a side effect of expanding the macro, rather than as an effect of executing the expanded macro code.
The correct definition is:

```lisp
(defmacro defrule (name &body body)
  "Define a new rule with the given name."
  '(setf (get '.name 'rule)
  #'(lambda () .(translate-rule-body body))))
```

Beginners sometimes fail to see the difference between these two approaches, because they both have the same result when interpreting a file that makes use of `defrule`.
But when the file is compiled and later loaded into a different Lisp image, the difference becomes clear: the first definition erroneously stores the function in the compiler's image, while the second produces code that correctly stores the function when the code is loaded.

Beginning macro users have asked, "How can I have a macro that expands into code that does more than one thing?
Can I splice in the results of a macro?"

If by this the beginner wants a macro that just *does* two things, the answer is simply to use a progn.
There will be no efficiency problem, even if the progn forms are nested.
That is, if macro-expansion results in code like:

```lisp
(progn (progn (progn *a b*) c) (progn *d e))*
```

the compiler will treat it the same as `(progn *abc de).*`

On the other hand, if the beginner wants a macro that *returns* two values, the proper form is val ues, but it must be understood that the calling function needs to arrange specially to see both values.
There is no way around this limitation.
That is, there is no way to write a macro-or a function for that matter-that will "splice in" its results to an arbitrary call.
For example, the function `floor` returns two values (the quotient and remainder), as does i ntern (the symbol and whether or not the symbol already existed).
But we need a special form to capture these values.
For example, compare:

```lisp
> (list (floor 11 5) (intern 'x))=M2 X)
> (multiple-value-call #'list
  (floor 11 5) (intern 'x))=>(2 1 X :INTERNAL)
```

## 25.15 A Style Guide to Lisp

In a sense, this whole book is a style guide to writing quality Lisp programs.
But this section attempts to distill some of the lessons into a set of guidelines.

### When to Define a Function

Lisp programs tend to consist of many short functions, in contrast to some languages that prefer a style using fewer, longer functions.
New functions should be introduced for any of the following reasons:

1.  For a specific, easily stated purpose.
!!!(p) {:.numlist}

2.  To break up a function that is too long.
!!!(p) {:.numlist}

3.  When the name would be useful documentation.
!!!(p) {:.numlist}

4.  When it is used in several places.
!!!(p) {:.numlist}

In (2), it is interesting to consider what "too long" means.
[Charniak et al.
(1987)](B9780080571157500285.xhtml#bb0180) suggested that 20 lines is the limit.
But now that large bit-map displays have replaced 24-line terminals, function definitions have become longer.
So perhaps one screenful is a better limit than 20 lines.
The addition of `flet` and `labels` also contributes to longer function definitions.

### When to Define a Special Variable

In general, it is a good idea to minimize the use of special variables.
Lexical variables are easier to understand, precisely because their scope is limited.
Try to limit special variables to one of the following uses:

1.  For parameters that are used in many functions spread throughout a program.
!!!(p) {:.numlist}

2.  For global, persistant, mutable data, such as a data base of facts.
!!!(p) {:.numlist}

3.  For infrequent but deeply nested use.
!!!(p) {:.numlist}

An example of (3) might be a variable like `*standard-output*`, which is used by low-level priniting functions.
It would be confusing to have to pass this variable around among all your high-level functions just to make it available to `print`.

### When to Bind a Lexical Variable

In contrast to special variables, lexical variables are encouraged.
You should feel free to introduce a lexical variable (with `a let, lambda` or `defun`) for any of the following reasons:

1.  To avoid typing in the same expression twice.
!!!(p) {:.numlist}

2.  To avoid computing the same expression twice.
!!!(p) {:.numlist}

3.  When the name would be useful documentation.
!!!(p) {:.numlist}

4.  To keep the indentation manageable.
!!!(p) {:.numlist}

### How to Choose a Name

Your choice of names for functions, variables, and other objects should be clear, meaningful, and consistent.
Some of the conventions are listed here:

1.  Use mostly letters and hyphens, and use full words: `delete-file`.
!!!(p) {:.numlist}

2.  You can introduce an abbreviation if you are consistent: `get-dtree`, `dtree-fetch`.
For example, this book uses `fn` consistently as the abbreviation for "function."
!!!(p) {:.numlist}

3.  Predicates end in - `p` (or ? in Scheme), unless the name is already a predicate: `variable-p`, `occurs-in`.
!!!(p) {:.numlist}

4.  Destructive functions start with n (or end in ! in Scheme): nreverse.
!!!(p) {:.numlist}

5.  Generalized variable-setting macros end in `f`: `setf`, `incf`.
(`Push` is an exception.)
!!!(p) {:.numlist}

6.  Slot selectors created by `defstruct` are of the form *type-slot.* Use this for `non-defstruct` selectors as well: `char-bits`.
!!!(p) {:.numlist}

7.  Many functions have the form *action-object:*`copy-list, delete-file`.
!!!(p) {:.numlist}

8.  Other functions have the form *object-modifier:*`list-length, char-lessp`.
Be consistent in your choice between these two forms.
Don't have `print-edge` and `vertex-print` in the same system.
!!!(p) {:.numlist}

9.  A function of the form *modulename-functionname* is an indication that packages are needed.
Use parser: `print-tree` instead of `parser-print-tree`.
!!!(p) {:.numlist}

10.  Special variables have asterisks: `*db*, *print-length*`.
!!!(p) {:.numlista}

11.  Constants do not have asterisks: `pi, most-positive-fixnum`.
!!!(p) {:.numlista}

12.  Parameters are named by type: (`defun length (sequence) ...)` or by purpose: (`defun subsetp(subset superset) ...`) or both: (`defun / (number &rest denominator-numbers) ...`)
!!!(p) {:.numlista}

13.  Avoid ambiguity.
A variable named `last-node` could have two meanings; use `previous` -`node` or `final` - `node` instead.
!!!(p) {:.numlista}

14.  A name like `propagate-constraints-to-neighboring-vertexes` is too long, while `prp-con` is too short.
In deciding on length, consider how the name will be used: `propagate-constraints` is just right, because a typical call will be `(propagate-const rai nts vertex)`, so it will be obvious what the constraints are propagating to.
!!!(p) {:.numlista}

### Deciding on the Order of Parameters

Once you have decided to define a function, you must decide what parameters it will take, and in what order.
In general,

1.  Put important parameters first (and optional ones last).
!!!(p) {:.numlist}

2.  Make it read like prose if possible: (`push element stack`).
!!!(p) {:.numlist}

3.  Group similar parameters together.
!!!(p) {:.numlist}

Interestingly, the choice of a parameter list for top-level functions (those that the user is expected to call) depends on the environment in which the user will function.
In many systems the user can type a keystroke to get back the previous input to the top level, and can then edit that input and re-execute it.
In these systems it is preferable to have the parameters that are likely to change be at the end of the parameter list, so that they can be easily edited.
On systems that do not offer this kind of editing, it is better to either use keyword parameters or make the highly variable parameters first in the list (with the others optional), so that the user will not have to type as much.

Many users want to have *required* keyword parameters.
It turns out that all keyword parameters are optional, but the following trick is equivalent to a required keyword parameter.
First we define the function `required` to signal an error, and then we use a call to `required` as the default value for any keyword that we want to make required:

```lisp
(defun required ()
  (error "A required keyword argument was not supplied."))
(defun fn (x &key (y (required)))
  ...)
```

## 25.16 Dealing with Files, Packages, and Systems

While this book has covered topics that are more advanced than any other Lisp text available, it is still concerned only with programming in the small: a single project at a time, capable of being implemented by a single programmer.
More challenging is the problem of programming in the large: building multiproject, multiprogrammer systems that interact well.

This section briefly outlines an approach to organizing a larger project into man-ageable components, and how to place those components in files.

Every system should have a separate file that defines the other files that comprise the system.
I recommend defining any packages in that file, although others put package definitions in separate files.

The following is a sample file for the mythical system Project-X.
Each entry in the file is discussed in turn.

1.  The first line is a comment known as the *mode line.* The text editor emacs will parse the characters between -*- delimiters to discover that the file contains Lisp code, and thus the Lisp editing commands should be made available.
The dialect of Lisp and the package are also specified.
This notation is becoming widespread as other text editors emulate emacs's conventions.
!!!(p) {:.numlist}

2.  Each file should have a description of its contents, along with information on the authors and what revisions have taken place.
!!!(p) {:.numlist}

3.  Comments with four semicolons (`;;;;`) denote header lines.
Many text editors supply a command to print all such lines, thus achieving an outline of the major parts of a file.
!!!(p) {:.numlist}

4.  The first executable form in every file should be an `in-package`.
Here we use the user package.
We will soon create the `project-x package`, and it will be used in all subsequent files.
!!!(p) {:.numlist}

5.  We want to define the Project-X system as a collection of files.
Unfortunately, Common Lisp provides no way to do that, so we have to load our own system-definition functions explicitly with a call to `load`.
!!!(p) {:.numlist}

6.  The call to `define - system` specifies the files that make up Project-X.
We provide a name for the system, a directory for the source and object files, and a list of *modules* that make up the system.
Each module is a list consisting of the module name (a symbol) followed by a one or more files (strings or pathnames).
We have used keywords as the module names to eliminate any possible name conflicts, but any symbol could be used.
!!!(p) {:.numlist}

7.  The call to `defpackage` defines the package `project-x`.
For more on packages, see section 24.1.
!!!(p) {:.numlist}

8.  The final form prints instructions on how to load and run the system.
!!!(p) {:.numlist}

```lisp
;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: User -*-
;;; (Brief description of system here.)
;;;; Define the Project-X system.
(in-package "USER")
(load "/usr/norvig/defsys.lisp") ; load define-system
(define-system ;; Define the system Project-X
  :name :project-x
  :source-dir "/usr/norvig/project-x/*.lisp"
  :object-dir "/usr/norvig/project-x/*.bin"
  :modules '((:macros "header" "macros")
    (:main "parser" "transformer" "optimizer"
        "commands" "database" "output")
    (:windows "xwindows" "clx" "client")))
(defpackage :project-x ;; Define the package Project-X
  (:export "DEFINE-X" "DO-X" "RUN-X")
  (:nicknames "PX")
  (:use common-lisp))
(format *debug-io* To load the Project-X system, type
  (make-system marne :project-x)
To run the system, type
  (project-x:run-x)")
```

Each of the files that make up the system will start like this:

```lisp
;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Project-X -*-
(in-package "PROJECT-X")
```

Now we need to provide the system-definition functions, `define-system` and `make-system`.
The idea is that `define-system` is used to define the files that make up a system, the modules that the system is comprised of, and the files that make up each module.
It is necessary to group files into modules because some files may depend on others.
For example, all macros, special variables, constants, and inline functions need to be both compiled and loaded before any other files that reference them are compiled.
In Project-X, all `defvar, defparameter, defconstant,` and `defstruct`[3](#fn0020) forms are put in the file header, and all defmacro forms are put in the file macros.
Together these two files form the first module, named : macros, which will be loaded before the other two modules (: `main` and :`windows`) are compiled and loaded.

define-system also provides a place to specify a directory where the source and object files will reside.
For larger systems spread across multiple directories, `define - system` will not be adequate.

Here is the first part of the file `defsys.lisp`, showing the definition of `define-system` and the structure sys.

```lisp
;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: User -*-
; ; ; ; A Facility for Defining Systems and their Components
(in-package "USER")
(defvar *systems* nil "List of all systems defined.")
(defstruct sys
  "A system containing a number of source and object files."
  name source-dir object-dir modules)
(defun define-system (&key name source-dir object-dir modules)
  "Define a new system."
```

`  ;; Delete any old system of this name.
and add the new one.`

```lisp
  (setf *systems* (delete name *systems* :test #'string-equal
      :key #'sys-name))
  (push (make-sys
      :name (string name)
      :source-dir (pathname source-dir)
      :object-dir (pathname object-dir)
      :modules '((:all ..(mapcar #'first modules)) ..modules))
    *systems*)
name)
```

The function `make` - `systemis` used to compile and/or load a previously defined system.
The name supplied is used to look up the definition of a system, and one of three actions is taken on the system.
The keyword : `cload` means to compile and then load files.
: `load` means to load files; if there is an object (compiled) file and it is newer than the source file, then it will be loaded, otherwise the source file will be loaded.
Finally, : `update` means to compile just those source files that have been changed since their corresponding source files were last altered, and to load the new compiled version.

```lisp
(defun make-system (&key (module : al 1 ) (action :cload)
                  (name (sys-name (first *systems*))))
    "Compile and/or load a system or one of its modules."
    (let ((system (find name *systems* :key #'sys-name
            :test #'string-equal)))
      (check-type system (not null))
      (check-type action (member : cload : update :load))
      (with-compilation-unit O (sys-action module system action))
  (defun sys-action (x system action)
    "Perform the specified action to x in this system.
```

`    X can be a module name (symbol).
file name (string)`

```lisp
    or a list."
    (typecase x
      (symbol (let ((files (rest (assoc x (sys-modules system)))))
            (if (null files)
              (warn "No files for module ~  a" x)
              (sys-action files system action))))
      (list (dolist (file x)
          (sys-action file system action)))
      ((string pathname)
          (let ((source (merge-pathnames
                x (sys-source-dir system)))
            (object (merge-pathnames
                x (sys-object-dir system))))
          (case action
  (:cload (compile-file source) (load object))
  (:update (unless (newer-file-p object source)
      (compile-file source))
    (load object))
  (:load (if (newer-file-p object source)
      (load object)
      (load source))))))
(t (warn "Don't know how to ~  a "~a in system ~  a"
    action x system))))
```

To support this, we need to be able to compare the write dates on files.
This is not hard to do, since Common Lisp provides the function `file-write-date`.

```lisp
(defun newer-file-p (file1 file2)
  "Is file1 newer than (written later than) file2?"
  (>-num (if (probe-file filel) (file-write-date filel))
  (if (probe-file file2) (file-write-date file2))))
(defun >-num (x y)
```

`  "True if x and y are numbers.
and x > y."`

```lisp
  (and (numberp x) (numberp y) (> x y)))
```

## 25.17 Portability Problems

Programming is difficult.
All programmers know the frustration of trying to get a program to work according to the specification.
But one thing that really defines the professional programmer is the ability to write portable programs that will work on a variety of systems.
A portable program not only must work on the computer it was tested on but also must anticipate the difference between your computer and other ones.
To do this, you must understand the Common Lisp specification in the abstract, not just how it is implemented on your particular machine.

There are three ways in which Common Lisp systems can vary: in the treatment of "is an error" situations, in the treatment of unspecified results, and in extensions to the language.

*Common Lisp the Language* specifies that it "is an error" to pass a non-number to an arithmetic function.
For example, it is an error to evaluate (`+ nil 1`).
However, it is not specified what should be done in this situation.
Some implementations may signal an error, but others may not.
An implementation would be within its right to return 1, or any other number or non-number as the result.

An unsuspecting programmer may code an expression that is an error but still computes reasonable results in his or her implementation.
A common example is applying get to a non-symbol.
This is an error, but many implementations will just return nil, so the programmer may write (`get x ' prop`) when `(if ( symbol p x) (get x 'prop) nil`) is actually needed for portable code.
Another common problem is with subseq and the sequence functions that take : end keywords.
It is an error if the : end parameter is not an integer less than the length of the sequence, but many implementations will not complain if : end is nil or is an integer greater than the length of the sequence.

The Common Lisp specification often places constraints on the result that a function must compute, without fully specifying the result.
For example, both of the following are valid results:

`> (union '(a b c) '(b c d))`=>`(A B C D)`

`> (union '(a b c) '(b c d))`=>`(D A B C)`

A program that relies on one order or the other will not be portable.
The same warning applies to `intersection` and `set-difference`.
Many functions do not specify how much the result shares with the input.
The following computation has only one possible printed result:

```lisp
> (remove 'x'(a b c d)) (A B C D)
```

However, it is not specified whether the output is `eq` or only `equal` to the second input.

Input/output is particularly prone to variation, as different operating systems can have very different conceptions of how I/O and the file system works.
Things to watch out for are whether `read-char` echoes its input or not, the need to include `finish-output`, and variationin where newlines are needed, particularly with respect to the top level.

Finally, many implementations provide extensions to Common Lisp, either by adding entirely new functions or by modifying existing functions.
The programmer must be careful not to use such extensions in portable code.

## 25.18 Exercises

**Exercise  251 [h]** On your next programming project, keep a log of each bug you detect and its eventual cause and remedy.
Classify each one according to the taxon-omy given in this chapter.
What kind of mistakes do you make most often?
How could you correct that?

**Exercise  25.2 [s-d]** Take a Common Lisp program and get it to work with a different compiler on a different computer.
Make sure you use conditional compilation read macros (#+ and #-) so that the program will work on both systems.
What did you have to change?

**Exercise  25.3 [m]** Write a `setf` method for `if` that works like this:

```lisp
(setf (if test (first x) y) (+  2 3))=
(let ((temp (+  2 3)))
  (if test
    (setf (first x) temp)
    (setf y temp)))
```

You will need to use `define-setf-method`, not `defsetf`.
(Why?) Make sure you handle the case where there is no else part to the `if`.

**Exercise  25.4 [h]** Write a `setf` method for `lookup`, a function to get the value for a key in an association list.

```lisp
(defun lookup (key alist)
  "Get the cdr of key's entry in the association list."
  (cdr (assoc key alist)))
```

## 25.19 Answers

**Answer 25.4** Here is the setf method for `lookup`.
It looks for the key in the a-list, and if the key is there, it modifies the cdr of the pair containing the key; otherwise it adds a new key/value pair to the front of the a-list.

```lisp
(define-setf-method lookup (key alist-place)
  (multiple-value-bind (temps vais stores store-form access-form)
      (get-setf-method alist-place)
  (let ((key-var (gensym))
          (pair-var (gensym))
          (result (gensym)))
      (values
        '(.key-var .@temps .pair-var)
        '(.key .@vais (assoc .key-var ,access-form))
        '(.result)
        '(if .pair-var
            (setf (cdr .pair-var) .result)
            (let ((.(first stores)
                (acons ,key-var .result .access-form)))
              .store-form
              ,result))
        '(cdr .pair-var)))))
```

----------------------

[1](#xfn0010) This misunderstanding has shown up even in published articles, such as [Baker 1991](B9780080571157500285.xhtml#bb0060).
!!!(p) {:.ftnote1}

[2](#xfn0015) Scheme requires a list of keys in each clause.
Now you know why.
!!!(p) {:.ftnote1}

[3](#xfn0020) def struct forms are put here because they may create inline functions.
!!!(p) {:.ftnote1}



