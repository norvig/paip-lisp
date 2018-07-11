# Chapter 23
## Compiling Lisp

Many textbooks show simple interpreters for Lisp, because they are simple to write, and because it is useful to know how an interpreter works.
Unfortunately, not as many textbooks show how to write a compiler, even though the same two reasons hold.
The simplest compiler need not be much more complex than an interpreter.

One thing that makes a compiler more complex is that we have to describe the output of the compiler: the instruction set of the machine we are compiling for.
For the moment let's assume a stack-based machine.
The calling sequence on this machine for a function call with *n* arguments is to push the *n* arguments onto the stack and then push the function to be called.
A `“CALL *n*”` instruction saves the return point on the stack and goes to the first instruction of the called function.
By convention, the first instruction of a function will always be `"ARGS *n*"`, which pops *n* arguments off the stack, putting them in the new function's environment, where they can be accessed by `LVAR` and `LSET` instructions.
The function should return with a `RETURN` instruction, which resets the program counter and the environment to the point of the original `CALL` instruction.

In addition, our machine has three `JUMP` instructions; one that branches unconditionally, and two that branch depending on if the top of the stack is nil or non-nil.
There is also an instruction for popping unneeded values off the stack, and for accessing and altering global variables.
The instruction set is shown in [figure 23.1](#f0010).
A glossary for the compiler program is given in [figure 23.2](#f0015).
A summary of a more complex version of the compiler appears on [page 795](#p795).

![f23-01-9780080571157](images/B9780080571157500236/f23-01-9780080571157.jpg)     
Figure 23.1
!!!(span) {:.fignum}
Instruction Set for Hypothetical Stack Machine
![f23-02-9780080571157](images/B9780080571157500236/f23-02-9780080571157.jpg)     
Figure 23.2
!!!(span) {:.fignum}
Glossary for the Scheme Compiler
As an example, the procedure

[ ](#){:#l0010}`(lambda () (if (= x y) (f (g x)) (h x y (h 1 2))))`
!!!(p) {:.unnumlist}

should compile into the following instructions:

[ ](#){:#t0010}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| | `ARGS` | `0` |
| | `GVAR` | `X` |
| | `GVAR` | `Y` |
| | `GVAR` | `=` |
| | `CALL` | `2` |
| | `FJUMP` | `L1` |
| | `GVAR` | `X` |
| | `GVAR` | `G` |
| | `CALL` | `1` |
| | `GVAR` | `F` |
| | `CALL` | `1` |
| | `JUMP` | `L2` |
| `L1:` | `GVAR` | `X` |
| | `GVAR` | `Y` |
| | `CONST` | `1` |
| | `CONST` | `2` |
| | `GVAR` | `H` |
| | `CALL` | `2` |
| | `GVAR` | `H` |
| | `CALL` | `3` |
| `L2:` | `RETURN` | |

The first version of the Scheme compiler is quite simple.
It mimics the structure of the Scheme evaluator.
The difference is that each case generates code rather than evaluating a subexpression:

[ ](#){:#l0015}`(defun comp (x env)`
!!!(p) {:.unnumlist}

`  "Compile the expression x into a list of instructions."`
!!!(p) {:.unnumlist}

`  (cond`
!!!(p) {:.unnumlist}

`    ((symbolp x) (gen-var x env))`
!!!(p) {:.unnumlist}

`    ((atom x) (gen 'CONST x))`
!!!(p) {:.unnumlist}

`    ((scheme-macro (first x)) (comp (scheme-macro-expand x) env))`
!!!(p) {:.unnumlist}

`    ((case (first x)`
!!!(p) {:.unnumlist}

`      (QUOTE (gen 'CONST (second x)))`
!!!(p) {:.unnumlist}

`      (BEGIN (comp-begin (rest x) env))`
!!!(p) {:.unnumlist}

`      (SET!
(seq (comp (third x) env) (gen-set (second x) env)))`
!!!(p) {:.unnumlist}

`      (IF (comp-if (second x) (third x) (fourth x) env))`
!!!(p) {:.unnumlist}

`      (LAMBDA (gen 'FN (comp-lambda (second x) (rest (rest x)) env)))`
!!!(p) {:.unnumlist}

`      ;; Procedure application:`
!!!(p) {:.unnumlist}

`      ;; Compile args, then fn, then the call`
!!!(p) {:.unnumlist}

`      (t  (seq (mappend #'(lambda (y) (comp y env)) (rest x))`
!!!(p) {:.unnumlist}

`               (comp (first x) env)`
!!!(p) {:.unnumlist}

`          (gen 'call (length (rest x)))))))))`
!!!(p) {:.unnumlist}

The compiler `comp` has the same nine cases–in fact the exact same structure–as the interpreter `interp` from [chapter 22](B9780080571157500224.xhtml).
Each case is slightly more complex, so the three main cases have been made into separate functions: `comp-begin`, `comp-if`, and `comp-lambda.` A `begin` expression is compiled by compiling each argument in turn but making sure to pop each value but the last off the stack after it is computed.
The last element in the `begin` stays on the stack as the value of the whole expression.
Note that the function `gen` generates a single instruction (actually a list of one instruction), and `seq` makes a sequence of instructions out of two or more subsequences.

[ ](#){:#l0020}`(defun comp-begin (exps env)`
!!!(p) {:.unnumlist}

`  "Compile a sequence of expressions, popping all but the last."`
!!!(p) {:.unnumlist}

`  (cond ((null exps) (gen 'CONST nil))`
!!!(p) {:.unnumlist}

`        ((length=l exps) (comp (first exps) env))`
!!!(p) {:.unnumlist}

`        (t (seq (comp (first exps) env)`
!!!(p) {:.unnumlist}

`                (gen 'POP)`
!!!(p) {:.unnumlist}

`                (comp-begin (rest exps) env)))))`
!!!(p) {:.unnumlist}

An `if` expression is compiled by compiling the predicate, then part, and else part, and by inserting appropriate branch instructions.

[ ](#){:#l0025}`(defun comp-if (pred then else env)`
!!!(p) {:.unnumlist}

`  "Compile a conditional expression."`
!!!(p) {:.unnumlist}

`  (let ((L1 (gen-label))`
!!!(p) {:.unnumlist}

`        (L2 (gen-label)))`
!!!(p) {:.unnumlist}

`    (seq (comp pred env) (gen 'FJUMP L1)`
!!!(p) {:.unnumlist}

`         (comp then env) (gen 'JUMP L2)`
!!!(p) {:.unnumlist}

`         (list L1) (comp else env)`
!!!(p) {:.unnumlist}

`         (list L2))))`
!!!(p) {:.unnumlist}

Finally, a `lambda` expression is compiled by compiling the body, surrounding it with one instruction to set up the arguments and another to return from the function, and then storing away the resulting compiled code, along with the environment.
The data type `fn` is implemented as a structure with slots for the body of the code, the argument list, and the name of the function (for printing purposes only).

[ ](#){:#l0030}`(defstruct (fn (:print-function print-fn))`
!!!(p) {:.unnumlist}

`  code (env nil)(name nil) (args nil))`
!!!(p) {:.unnumlist}

`(defun comp-lambda (args body env)`
!!!(p) {:.unnumlist}

`  "Compile a lambda form into a closure with compiled code."`
!!!(p) {:.unnumlist}

`  (assert (and (listp args) (every #'symbolp args)) ()`
!!!(p) {:.unnumlist}

`          "Lambda arglist must be a list of symbols, not ~ a" args)`
!!!(p) {:.unnumlist}

`  ;; For now.
no &rest parameters.`
!!!(p) {:.unnumlist}

`  ;; The next version will support Scheme's version of &rest`
!!!(p) {:.unnumlist}

`  (make-fn`
!!!(p) {:.unnumlist}

`    :env env :args args`
!!!(p) {:.unnumlist}

`    :code (seq (gen 'ARGS (length args))`
!!!(p) {:.unnumlist}

`               (comp-begin body (cons args env))`
!!!(p) {:.unnumlist}

`               (gen 'RETURN))))`
!!!(p) {:.unnumlist}

The advantage of compiling over interpreting is that much can be decided at compile time.
For example, the compiler can determine if a variable reference is to a global or lexical variable, and if it is to a lexical variable, exactly where that lexical variable is stored.
This computation is done only once by the compiler, but it has to be done each time the expression is encountered by the interpreter.
Similarly, the compiler can count up the number of arguments once and for all, while the interpreter must go through a loop, counting up the number of arguments, and testing for the end of the arguments after each one is interpreted.
So it is clear that the compiler can be more efficient than the interpreter.

Another advantage is that the compiler can be more robust.
For example, in `comp-lambda,` we check that the parameter list of a lambda expression is a list containing only symbols.
It would be too expensive to make such checks in an interpreter, but in a compiler it is a worthwhile trade-off to check once at compile time for error conditions rather than checking repeatedly at run time.

Before we show the rest of the compiler, here's a useful top-level interface to `comp`:

[ ](#){:#l0035}`(defvar *label-num* 0)`
!!!(p) {:.unnumlist}

`(defun compiler (x)`
!!!(p) {:.unnumlist}

`  "Compile an expression as if it were in a parameterless lambda."`
!!!(p) {:.unnumlist}

`  (setf *label-num* 0)`
!!!(p) {:.unnumlist}

`  (comp-lambda '() (list x) nil))`
!!!(p) {:.unnumlist}

`(defun comp-show (x)`
!!!(p) {:.unnumlist}

`  "Compile an expression and show the resulting code"`
!!!(p) {:.unnumlist}

` (show-fn (compiler x))`
!!!(p) {:.unnumlist}

`  (values))`
!!!(p) {:.unnumlist}

Now here's the code to generate individual instructions and sequences of instructions.
A sequence of instructions is just a list, but we provide the function `seq` rather than using `append` directly for purposes of data abstraction.
A label is just an atom.

[ ](#){:#l0040}`(defun gen (opcode &rest args)`
!!!(p) {:.unnumlist}

`  "Return a one-element list of the specified instruction."`
!!!(p) {:.unnumlist}

`  (list (cons opcode args)))`
!!!(p) {:.unnumlist}

`(defun seq (&rest code)`
!!!(p) {:.unnumlist}

`  "Return a sequence of instructions"`
!!!(p) {:.unnumlist}

`  (apply #'append code))`
!!!(p) {:.unnumlist}

`(defun gen-label (&optional (label 'L))`
!!!(p) {:.unnumlist}

`  "Generate a label (a symbol of the form Lnnn)"`
!!!(p) {:.unnumlist}

`  (intern (format nil "~a~d" label (incf *label-num*))))`
!!!(p) {:.unnumlist}

Environments are now represented as lists of frames, where each frame is a sequence of variables.
Local variables are referred to not by their name but by two integers: the index into the list of frames and the index into the individual frame.
As usual, the indexes are zero-based.
For example, given the code:

[ ](#){:#l0045}`(let ((a 2.0)`
!!!(p) {:.unnumlist}

`     (b 2.1))`
!!!(p) {:.unnumlist}

` (let ((c 1.0)`
!!!(p) {:.unnumlist}

`      (d 1.1))`
!!!(p) {:.unnumlist}

`  (let ((e 0.0)`
!!!(p) {:.unnumlist}

`     (f 0.1))`
!!!(p) {:.unnumlist}

`   (+ a b c d e f))))`
!!!(p) {:.unnumlist}

the innermost environment is `((e f) (c d) (a b))`.
The function `in-env-p` tests if a variable appears in an environment.
If this environment were called `env`, then `(in-env-p 'f env)` would return `(2 1)` and `(in-env-p 'x env)` would return `nil`.

[ ](#){:#l0050}`(defun gen-var (var env)`
!!!(p) {:.unnumlist}

`  "Generate an instruction to reference a variable's value."`
!!!(p) {:.unnumlist}

`  (let ((p (in-env-p var env)))`
!!!(p) {:.unnumlist}

`    (if p`
!!!(p) {:.unnumlist}

`        (gen 'LVAR (first p) (second p) ";" var)`
!!!(p) {:.unnumlist}

`        (gen 'GVAR var))))`
!!!(p) {:.unnumlist}

`(defun gen-set (var env)`
!!!(p) {:.unnumlist}

`  "Generate an instruction to set a variable to top-of-stack."`
!!!(p) {:.unnumlist}

`  (let ((p (in-env-p var env)))`
!!!(p) {:.unnumlist}

`    (if p`
!!!(p) {:.unnumlist}

`        (gen 'LSET (first p) (second p) ";" var)`
!!!(p) {:.unnumlist}

`        (gen 'GSET var))))`
!!!(p) {:.unnumlist}

Finally, we have some auxiliary functions to print out the results, to distinguish between labels and instructions, and to determine the index of a variable in an environment.
Scheme functions now are implemented as structures, which must have a field for the code, and one for the environment.
In addition, we provide a field for the name of the function and for the argument list; these are used only for debugging purposes, We'll adopt the convention that the `define` macro sets the function's name field, by calling `name` !
(which is not part of standard Scheme).

[ ](#){:#l0055}`(def-scheme-macro define (name &rest body)`
!!!(p) {:.unnumlist}

` (if (atom name)`
!!!(p) {:.unnumlist}

`   '(name!
(set!
,name .
,body) ',name)`
!!!(p) {:.unnumlist}

`  (scheme-macro-expand`
!!!(p) {:.unnumlist}

`    '(define ,(first name)`
!!!(p) {:.unnumlist}

`     (lambda ,(rest name) .
,body)))))`
!!!(p) {:.unnumlist}

`(defun name!
(fn name)`
!!!(p) {:.unnumlist}

` "Set the name field of fn, if it is an un-named fn."`
!!!(p) {:.unnumlist}

` (when (and (fn-p fn) (null (fn-name fn)))`
!!!(p) {:.unnumlist}

`  (setf (fn-name fn) name))`
!!!(p) {:.unnumlist}

` name)`
!!!(p) {:.unnumlist}

`;; This should also go in init-scheme-interp:`
!!!(p) {:.unnumlist}

`(set-global-var!
'name!
#'name!)`
!!!(p) {:.unnumlist}

`(defun print-fn (fn &optional (stream *standard-output*) depth)`
!!!(p) {:.unnumlist}

` (declare (ignore depth))`
!!!(p) {:.unnumlist}

` (format stream "{~ a}" (or (fn-name fn) '??)))`
!!!(p) {:.unnumlist}

`(defun show-fn (fn &optional (stream *standard-output*) (depth 0))`
!!!(p) {:.unnumlist}

`  "Print all the instructions in a function.`
!!!(p) {:.unnumlist}

`  If the argument is not a function, just princ it,`
!!!(p) {:.unnumlist}

`  but in a column at least 8 spaces wide."`
!!!(p) {:.unnumlist}

`  (if (not (fn-p fn))`
!!!(p) {:.unnumlist}

`      (format stream "~8a" fn)`
!!!(p) {:.unnumlist}

`      (progn`
!!!(p) {:.unnumlist}

`        (fresh-line)`
!!!(p) {:.unnumlist}

`        (incf depth 8)`
!!!(p) {:.unnumlist}

`        (dolist (instr (fn-code fn))`
!!!(p) {:.unnumlist}

`          (if (label-p instr)`
!!!(p) {:.unnumlist}

`              (format stream "~a:" instr)`
!!!(p) {:.unnumlist}

`              (progn`
!!!(p) {:.unnumlist}

`                (format stream "~VT" depth)`
!!!(p) {:.unnumlist}

`                (dolist (arg instr)`
!!!(p) {:.unnumlist}

`                  (show-fn arg stream depth))`
!!!(p) {:.unnumlist}

`                (fresh-line)))))))`
!!!(p) {:.unnumlist}

`(defun label-p (x) "Is x a label?" (atom x))`
!!!(p) {:.unnumlist}

`(defun in-env-p (symbol env)`
!!!(p) {:.unnumlist}

`  "If symbol is in the environment.
return its index numbers."`
!!!(p) {:.unnumlist}

`  (let ((frame (find symbol env :test #'find)))`
!!!(p) {:.unnumlist}

`    (if frame (list (position frame env) (position symbol frame)))))`
!!!(p) {:.unnumlist}

Now we are ready to show the compiler at work:

[ ](#){:#t0015}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `> (comp-show '(if (= x y) (f (g x)) (h x y (h 1 2))))` |
| | `ARGS` | `0` |
| | `GVAR` | `X` |
| | `GVAR` | `Y` |
| | `GVAR` | `=` |
| | `CALL` | `2` |
| | `FJUMP` | `L1` |
| | `GVAR` | `X` |
| | `GVAR` | `G` |
| | `CALL` | `1` |
| | `GVAR` | `F` |
| | `CALL` | `1` |
| | `JUMP` | `L2` |
| `L1:` | `GVAR` | `X` |
| | `GVAR` | `Y` |
| | `CONST` | `1` |
| | `CONST` | `2` |
| | `GVAR` | `H` |
| | `CALL` | `2` |
| | `GVAR` | `H` |
| | `CALL` | `3` |
| `L2:` | `RETURN` | |

![t0015](images/B9780080571157500236/t0015.png)

This example should give the reader a feeling for the code generated by the compiler.

Another reason a compiler has an advantage over an interpreter is that the compiler can afford to spend some time trying to find a more efficient encoding of an expression, while for the interpreter, the overhead of searching for a more efficient interpretation usually offsets any advantage gained.
Here are some places where a compiler could do better than an interpreter (although our compiler currently does not):

[ ](#){:#t0020}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `> (comp-show '(begin "doc" (write x) y))` |
| | `ARGS` | `0` |
| | `CONST` | `doc` |
| | `POP` | |
| | `GVAR` | `X` |
| | `GVAR` | `WRITE` |
| | `CALL` | `1` |
| | `POP` | |
| | `GVAR` | `Y` |
| | `RETURN` | |

![t0020](images/B9780080571157500236/t0020.png)

In this example, code is generated to push the constant "`doc`" on the stack and then immediately pop it off.
If we have the compiler keep track of what expressions are compiled "for value"—as y is the value of the expression above-and which are only compiled "for effect," then we can avoid generating any code at all for a reference to a constant or variable for effect.
Here's another example:

[ ](#){:#t0025}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `> (comp-show '(begin (+ (* a x) (f x)) x))` |
| `ARGS` | `0` |
| `GVAR` | `A` |
| `GVAR` | `X` |
| `GVAR` | `*` |
| `CALL` | `2` |
| `GVAR` | `X` |
| `GVAR` | `F` |
| `CALL` | `1` |
| `GVAR` | `+` |
| `CALL` | `2` |
| `POP` | |
| `GVAR` | `X` |
| `RETURN` | |

![t0025](images/B9780080571157500236/t0025.png)

In this expression, if we can be assured that + and * refer to the normal arithmetic functions, then we can compile this as if it were `(begin (f x) x)`.
Furthermore, it is reasonable to assume that + and * will be instructions in our machine that can be invoked inline, rather than having to call out to a function.
Many compilers spend a significant portion of their time optimizing arithmetic operations, by taking into account associativity, commutativity, distributivity, and other properties.

Besides arithmetic, compilers often have expertise in conditional expressions.
Consider the following:

[ ](#){:#t0030}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `> (comp-show '(if (and p q) x y))` |
| | `ARGS` | `0` |
| | `GVAR` | `P` |
| | `FJUMP` | `L3` |
| | `GVAR` | `Q` |
| | `JUMP` | `L4` |
| `L3:` | `GVAR` | `NIL` |
| `L4:` | `FJUMP` | `L1` |
| | `GVAR` | `X` |
| | `JUMP` | `L2` |
| `L1:` | `GVAR` | `Y` |
| `L2:` | `RETURN` | |

![t0030](images/B9780080571157500236/t0030.png)

Note that `(and p q)` macro-expands to `(if p q nil)`.
The resulting compiled code is correct, but inefficient.
First, there is an unconditional jump to `L4`, which labels a conditional jump to `L1`.
This could be replaced with a conditional jump to `L1`.
Second, at `L3` we load `NIL` and then jump on nil to `L1`.
These two instructions could be replaced by an unconditional jump to `L1`.
Third, the `FJUMP` to `L3` could be replaced by an `FJUMP` to `L1`, since we now know that the code at `L3` unconditionally goes to `L1`.

Finally, some compilers, particularly Lisp compilers, have expertise in function calling.
Consider the following:

[ ](#){:#t0035}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `> (comp-show '(f (g x y)))` |
| | `ARGS` | `0` |
| | `GVAR` | `X` |
| | `GVAR` | `Y` |
| | `GVAR` | `G` |
| | `CALL` | `2` |
| | `GVAR` | `F` |
| | `CALL` | `1` |
| | `RETURN` | |

![t0035](images/B9780080571157500236/t0035.png)

Here we call `g` and when `g` returns we call `f` , and when `f` returns we return from this function.
But this last return is wasteful; we push a return address on the stack, and then pop it off, and return to the next return address.
An alternative function-calling protocol involves pushing the return address before calling `g,` but then not pushing a return address before calling `f;` when `f` returns, it returns directly to the calling function, whatever that is.

Such an optimization looks like a small gain; we basically eliminate a single instruction.
In fact, the implications of this new protocol are enormous: we can now invoke a recursive function to an arbitrary depth without growing the stack at all—as long as the recursive call is the last statement in the function (or in a branch of the function when there are conditionals).
A function that obeys this constraint on its recursive calls is known as a *properly tail-recursive* function.
This subject was discussed in [section 22.3.](B9780080571157500224.xhtml#s0020)

All the examples so far have only dealt with global variables.
Here's an example using local variables:

[ ](#){:#t0040}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `> (comp-show '((lambda (x) ((lambda (y z) (f x y z)) 3 x)) 4))` |
| `ARGS` | `0` | | | | | |
| `CONST` | `4` | | | | | |
| `FN` | | | | | | |
| | `ARGS` | `1` | | | | |
| | `CONST` | `3` | | | | |
| | `LVAR` | `0` | `0` | ; | `X` | |
| | `FN` | | | | | |
| | | `ARGS` | `2` | | | |
| | | `LVAR` | `1` | `0` | ; | `X` |
| | | `LVAR` | `0` | `0` | ; | `Y` |
| | | `LVAR` | `0` | `1` | `;` | `Z` |
| | | `GVAR` | `F` | | | |
| | | `CALL` | `3` | | | |
| | | `RETURN` | | | | |
| | `CALL` | `2` | | | | |
| | `RETURN` | | | | | |
| `CALL` | `1` | | | | | |
| `RETURN` | | | | | | |

![t0040](images/B9780080571157500236/t0040.png)

The code is indented to show nested functions.
The top-level function loads the constant 4 and an anonymous function, and calls the function.
This function loads the constant 3 and the local variable `x`, which is the first (0th) element in the top (0th) frame.
It then calls the double-nested function on these two arguments.
This function loads `x, y`, and `z: x` is now the 0th element in the next-to-top (1st) frame, and `y` and `z` are the 0th and 1st elements of the top frame.
With all the arguments in place, the function `f` is finally called.
Note that no continuations are stored–`f` can return directly to the caller of this function.

However, all this explicit manipulation of environments is inefficient; in this case we could have compiled the whole thing by simply pushing 4, 3, and 4 on the stack and calling `f`.

## [ ](#){:#st0010}23.1 A Properly Tail-Recursive Lisp Compiler
{:#s0010}
{:.h1hd}

In this section we describe a new version of the compiler, first by showing examples of its output, and then by examining the compiler itself, which is summarized in [figure 23.3](#f0020).
The new version of the compiler also makes use of a different function calling sequence, using two new instructions, `CALLJ` and `SAVE`.
As the name implies, `SAVE` saves a return address on the stack.
The `CALLJ` instruction no longer saves anything; it can be seen as an unconditional jump–hence the `J` in its name.

![f23-03-9780080571157](images/B9780080571157500236/f23-03-9780080571157.jpg)     
Figure 23.3
!!!(span) {:.fignum}
Glossary of the Scheme Compiler, Second Version
First, we see how nested function calls work:

[ ](#){:#t0045}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `> (comp-show '(f (g x)))` |
| | `ARGS` | `0` |
| | `SAVE` | `K1` |
| | `GVAR` | `X` |
| | `GVAR` | `G` |
| | `CALLJ` | `1` |
| `K1:` | `GVAR` | `F` |
| | `CALLJ` | `1` |

![t0045](images/B9780080571157500236/t0045.png)

The continuation point `K1` is saved so that g can return to it, but then no continuation is saved for f, so f returns to whatever continuation is on the stack.
Thus, there is no need for an explicit `RETURN` instruction.
The final `CALL` is like an unconditional branch.

The following example shows that all functions but the last `(f)` need a continuation point:

[ ](#){:#t0050}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `> (comp-show '(f (g (h x) (h y))))` |
| | `ARGS` | `0` |
| | `SAVE` | `K1` |
| | `SAVE` | `K2` |
| | `GVAR` | `X` |
| | `GVAR` | `H` |
| | `CALLJ` | `1` |
| `K2:` | `SAVE` | `K3` |
| | `GVAR` | `Y` |
| | `GVAR` | `H` |
| | `CALLJ` | `1` |
| `K3:` | `GVAR` | `G` |
| | `CALLJ` | `2` |
| `K1:` | `GVAR` | `F` |
| | `CALLJ` | `1` |

![t0050](images/B9780080571157500236/t0050.png)

This code first computes `(h x)` and returns to `K2`.
Then it computes `(h y)` and returns to `K3`.
Next it calls `g` on these two values, and returns to `K1` before transferring to `f`.
Since whatever `f` returns will also be the final value of the function we are compiling, there is no need to save a continuation point for `f` to return to.

In the next example we see that unneeded constants and variables in `begin` expressions are ignored:

[ ](#){:#t0055}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `> (comp-show '(begin "doc" x (f x) y))` |
| | `ARGS` | `0` |
| | `SAVE` | `K1` |
| | `GVAR` | `X` |
| | `GVAR` | `F` |
| | `CALLJ` | `1` |
| `K1:` | `POP` | |
| | `GVAR` | `Y` |
| | `RETURN` | |

![t0055](images/B9780080571157500236/t0055.png)

One major flaw with the first version of the compiler is that it could pass data around, but it couldn't actually *do* anything to the data objects.
We fix that problem by augmenting the machine with instructions to do arithmetic and other primitive operations.
Unneeded primitive operations, like variables constants, and arithmetic operations are ignored when they are in the nonfinal position within `begins`.
Contrast the following two expressions:

[ ](#){:#t0060}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `> (comp-show '(begin (+ (* a x) (f x)) x))` |
| | `ARGS` | `0` |
| | `SAVE` | `K1` |
| | `GVAR` | `X` |
| | `GVAR` | `F` |
| | `CALLJ` | `1` |
| `K1:` | `POP` | |
| | `GVAR` | `X` |
| | `RETURN` | |
| `> (comp-show '(begin (+ (* a x) (f x))))` |
| | `ARGS` | `0` |
| | `GVAR` | `A` |
| | `GVAR` | `X` |
| | `*` | |
| | `SAVE` | `K1` |
| | `GVAR` | `X` |
| | `GVAR` | `F` |
| | `CALLJ` | `1` |
| `K1:` | `+` | |
| | `RETURN` | |

![t0060](images/B9780080571157500236/t0060.png)

The first version of the compiler was context-free, in that it compiled all equivalent expressions equivalently, regardless of where they appeared.
A properly tail-recursive compiler needs to be context-sensitive: it must compile a call that is the final value of a function differently than a call that is used as an intermediate value, or one whose value is ignored.
In the first version of the compiler, `comp-lambda` was responsible for generating the `RETURN` instruction, and all code eventually reached that instruction.
To make sure the `RETURN` was reached, the code for the two branches of `if` expressions had to rejoin at the end.

In the tail-recursive compiler, each piece of code is responsible for inserting its own `RETURN` instruction or implicitly returning by calling another function without saving a continuation point.

We keep track of these possibilities with two flags.
The parameter `val?` is true when the expression we are compiling returns a value that is used elsewhere.
The parameter `more?` is false when the expression represents the final value, and it is true when there is more to compute.
In summary, there are three possibilities:

[ ](#){:#t0065}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `val?` | `more?` | example: the `X` in: |
| true | true | `(if X y z)`*or*`(f X y)` |
| true | false | `(if p X z)`*or*`(begin y X)` |
| false | true | `(begin X y)` |
| false | false | *impossible* |

The code for the compiler employing these conventions follows:

[ ](#){:#l0060}`(defun comp (x env val?
more?)`
!!!(p) {:.unnumlist}

`  "Compile the expression x into a list of instructions."`
!!!(p) {:.unnumlist}

`   (cond`
!!!(p) {:.unnumlist}

`    ((member x '(t nil)) (comp-const x val?
more?))`
!!!(p) {:.unnumlist}

`    ((symbolp x) (comp-var x env val?
more?))`
!!!(p) {:.unnumlist}

`    ((atom x) (comp-const x val?
more?))`
!!!(p) {:.unnumlist}

`   ((scheme-macro (first x)) (comp (scheme-macro-expand x) env val?
more?))`
!!!(p) {:.unnumlist}

`    ((case (first x)`
!!!(p) {:.unnumlist}

`       (QUOTE (arg-count x 1)`
!!!(p) {:.unnumlist}

`              (comp-const (second x) val?
more?))`
!!!(p) {:.unnumlist}

`       (BEGIN (comp-begin (rest x) env val?
more?))`
!!!(p) {:.unnumlist}

`       (SET!
 (arg-count x 2)`
!!!(p) {:.unnumlist}

`              (assert (symbolp (second x)) (x)`
!!!(p) {:.unnumlist}

`                      "Only symbols can be set!, not ~ a in ~ a"`
!!!(p) {:.unnumlist}

`                      (second x) x)`
!!!(p) {:.unnumlist}

`              (seq (comp (third x) env t t)`
!!!(p) {:.unnumlist}

`                   (gen-set (second x) env)`
!!!(p) {:.unnumlist}

`                   (if (not val?) (gen 'POP))`
!!!(p) {:.unnumlist}

`                   (unless more?
(gen 'RETURN))))`
!!!(p) {:.unnumlist}

`      (IF  (arg-count x 2 3)`
!!!(p) {:.unnumlist}

`           (comp-if (second x) (third x) (fourth x)`
!!!(p) {:.unnumlist}

`                    env val?
more?))`
!!!(p) {:.unnumlist}

`      (LAMBDA (when val?`
!!!(p) {:.unnumlist}

`               (let ((f (comp-lambda (second x) (rest2 x) env)))`
!!!(p) {:.unnumlist}

`                 (seq (gen 'FN f) (unless more?
(gen 'RETURN))))))`
!!!(p) {:.unnumlist}

`      (t (comp-funcall (first x) (rest x) env val?
more?))))))`
!!!(p) {:.unnumlist}

Here we've added one more case: `t` and `nil` compile directly into primitive instructions, rather than relying on them being bound as global variables.
(In real Scheme, the Boolean values are `#t` and `#f`, which need not be quoted, the empty list is `()`, which must be quoted, and `t` and `nil` are ordinary symbols with no special significance.)

I've also added some error checking for the number of arguments supplied to quote, `set!` and `if`.
Note that it is reasonable to do more error checking in a compiler than in an interpreter, since the checking need be done only once, not each time through.
The function to check arguments is as follows:

[ ](#){:#l0065}`(defun arg-count (form min &optional (max min))`
!!!(p) {:.unnumlist}

`  "Report an error if form has wrong number of args."`
!!!(p) {:.unnumlist}

`  (let ((n-args (length (rest form))))`
!!!(p) {:.unnumlist}

`    (assert (<= min n-args max) (form)`
!!!(p) {:.unnumlist}

`      "Wrong number of arguments for ~ a in ~ a:`
!!!(p) {:.unnumlist}

`      ~d supplied, ~ d~@[ to ~ d ~] expected"`
!!!(p) {:.unnumlist}

`     (first form) form n-args min (if (/= min max) max))))`
!!!(p) {:.unnumlist}

**Exercise 23.1 [m]** Modify the compiler to check for additional compile-time errors suggested by the following erroneous expression:

[ ](#){:#l0070}`(cdr (+ (list x y) 'y (3 x) (car 3 x)))`
!!!(p) {:.unnumlist}

The tail-recursive compiler still has the familiar nine cases, but I have introduced `comp-var, comp-const, comp-if,` and `comp-funcall` to handle the increased complexity introduced by the `var?` and `more?` parameters.

Let's go through the `comp-` functions one at a time.
First, `comp-begin` and `comp-list` just handle and pass on the additional parameters.
`comp-list` will be used in `comp-funcall`, a new function that will be introduced to compile a procedure application.

[ ](#){:#l0075}`(defun comp-begin (exps env val?
more?)`
!!!(p) {:.unnumlist}

`  "Compile a sequence of expressions,`
!!!(p) {:.unnumlist}

`  returning the last one as the value."`
!!!(p) {:.unnumlist}

`  (cond ((null exps) (comp-const nil val?
more?))`
!!!(p) {:.unnumlist}

`        ((length=l exps) (comp (first exps) env val?
more?))`
!!!(p) {:.unnumlist}

`        (t (seq (comp (first exps) env nil t)`
!!!(p) {:.unnumlist}

`                (comp-begin (rest exps) env val?
more?)))))`
!!!(p) {:.unnumlist}

`(defun comp-list (exps env)`
!!!(p) {:.unnumlist}

`  "Compile a list, leaving them all on the stack."`
!!!(p) {:.unnumlist}

`  (if (null exps) nil`
!!!(p) {:.unnumlist}

`      (seq (comp (first exps) env t t)`
!!!(p) {:.unnumlist}

`           (comp-list (rest exps) env))))`
!!!(p) {:.unnumlist}

Then there are two trivial functions to compile variable access and constants.
If the value is not needed, these produce no instructions at all.
If there is no more to be done, then these functions have to generate the return instruction.
This is a change from the previous version of `comp`, where the caller generated the return instruction.
Note I have extended the machine to include instructions for the most common constants: t, nil, and some small integers.

[ ](#){:#l0080}`(defun comp-const (x val?
more?)`
!!!(p) {:.unnumlist}

`  "Compile a constant expression."`
!!!(p) {:.unnumlist}

`  (if val?
(seq (if (member x '(t nil − 1 0 1 2))`
!!!(p) {:.unnumlist}

`                    (gen x)`
!!!(p) {:.unnumlist}

`                    (gen 'CONST x))`
!!!(p) {:.unnumlist}

`                 (unless more?
(gen 'RETURN)))))`
!!!(p) {:.unnumlist}

`(defun comp-var (x env val?
more?)`
!!!(p) {:.unnumlist}

`  "Compile a variable reference."`
!!!(p) {:.unnumlist}

`  (if val?
(seq (gen-var x env) (unless more?
(gen 'RETURN)))))`
!!!(p) {:.unnumlist}

The remaining two functions are more complex.
First consider `comp-if` .
Rather than blindly generating code for the predicate and both branches, we will consider some special cases.
First, it is clear that `(if t x y)` can reduce to `x` and `(if nil x y)` can reduce to `y`.
It is perhaps not as obvious that `(if p x x)` can reduce to `(begin p x)`, or that the comparison of equality between the two branches should be done on the object code, not the source code.
Once these trivial special cases have been considered, we're left with three more cases: `(if p x nil), (if p nil y),` and `(if p x y)`.
The pattern of labels and jumps is different for each.

[ ](#){:#l0085}`(defun comp-if (pred then else env val?
more?)`
!!!(p) {:.unnumlist}

`  "Compile a conditional (IF) expression."`
!!!(p) {:.unnumlist}

`  (cond`
!!!(p) {:.unnumlist}

`    ((null pred) ; (if nil x y) ==> y`
!!!(p) {:.unnumlist}

`     (comp else env val?
more?))`
!!!(p) {:.unnumlist}

`    ((constantp pred) ; (if t x y) ==> x`
!!!(p) {:.unnumlist}

`     (comp then env val?
more?))`
!!!(p) {:.unnumlist}

`    ((and (listp pred) ; (if (not p) x y) ==> (if p y x)`
!!!(p) {:.unnumlist}

`          (length=l (rest pred))`
!!!(p) {:.unnumlist}

`          (primitive-p (first pred) env 1)`
!!!(p) {:.unnumlist}

`          (eq (prim-opcode (primitive-p (first pred) env 1)) 'not))`
!!!(p) {:.unnumlist}

`     (comp-if (second pred) else then env val?
more?))`
!!!(p) {:.unnumlist}

`    (t (let ((pcode (comp pred env t t))`
!!!(p) {:.unnumlist}

`             (tcode (comp then env val?
more?))`
!!!(p) {:.unnumlist}

`             (ecode (comp else env val?
more?)))`
!!!(p) {:.unnumlist}

`         (cond`
!!!(p) {:.unnumlist}

`           ((equal tcode ecode) ; (if p x x) ==> (begin p x)`
!!!(p) {:.unnumlist}

`            (seq (comp pred env nil t) ecode))`
!!!(p) {:.unnumlist}

`           ((null tcode) ; (if p nil y) ==> p (TJUMP L2) y L2:`
!!!(p) {:.unnumlist}

`            (let ((L2 (gen-label)))`
!!!(p) {:.unnumlist}

`             (seq pcode (gen 'TJUMP L2) ecode (list L2)`
!!!(p) {:.unnumlist}

`                  (unless more?
(gen 'RETURN)))))`
!!!(p) {:.unnumlist}

`           ((null ecode) ; (if p x) ==> p (FJUMP L1) x L1:`
!!!(p) {:.unnumlist}

`            (let ((L1 (gen-label)))`
!!!(p) {:.unnumlist}

`             (seq pcode (gen 'FJUMP L1) tcode (list L1)`
!!!(p) {:.unnumlist}

`                  (unless more?
(gen 'RETURN)))))`
!!!(p) {:.unnumlist}

`           (t             ; (if p x y) ==> p (FJUMP L1) x L1: y`
!!!(p) {:.unnumlist}

`                          ; or p (FJUMP L1) x (JUMP L2) L1: y L2:`
!!!(p) {:.unnumlist}

`            (let ((L1 (gen-label))`
!!!(p) {:.unnumlist}

`                  (L2 (if more?
(gen-label))))`
!!!(p) {:.unnumlist}

`              (seq pcode (gen 'FJUMP L1) tcode`
!!!(p) {:.unnumlist}

`                   (if more?
(gen 'JUMP L2))`
!!!(p) {:.unnumlist}

`                   (list L1) ecode (if more?
(list L2))))))))))`
!!!(p) {:.unnumlist}

Here are some examples of `if` expressions.
First, a very simple example:

[ ](#){:#t0070}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `> (comp-show '(if p (+ x y) (* x y)))` |
| | `ARGS` | `0` |
| | `GVAR` | `P` |
| | `FJUMP` | `L1` |
| | `GVAR` | `X` |
| | `GVAR` | `Y` |
| | `+` | |
| | `RETURN` | |
| `L1 :` | `GVAR` | `X` |
| | `GVAR` | `Y` |
| | `*` | |
| | `RETURN` | |

![t0070](images/B9780080571157500236/t0070.png)

Each branch has its own `RETURN` instruction.
But note that the code generated is sensitive to its context.
For example, if we put the same expression inside a `begin` expression, we get something quite different:

[ ](#){:#t0075}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `> (comp-show '(begin (if p (+ x y) (* x y)) z))` |
| | `ARGS` | `0` |
| | `GVAR` | `Z` |
| | `RETURN` | |

![t0075](images/B9780080571157500236/t0075.png)

What happens here is that `(+ x y)` and `(* x y)`, when compiled in a context where the value is ignored, both resuit in no generated code.
Thus, the `if` expression reduces to `(if p nil nil)`, which is compiled like `(begin p nil)`, which also generates no code when not evaluated for value, so the final code just references `z`.
The compiler can only do this optimization because it knows that `+` and `*` are side-effect-free operations.
Consider what happens when we replace + with `f` :

[ ](#){:#t0080}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `> (comp-show '(begin (if p (f x) (* x x)) z))` |
| | `ARGS` | `0` |
| | `GVAR` | `P` |
| | `FJUMP` | `L2` |
| | `SAVE` | `K1` |
| | `GVAR` | `X` |
| | `GVAR` | `F` |
| | `CALLJ` | `1` |
| `K1:` | `POP` | |
| `L2:` | `GVAR` | `Z` |
| | `RETURN` | |

![t0080](images/B9780080571157500236/t0080.png)

Here we have to call `(f x)` if `p` is true (and then throw away the value returned), but we don't have to compute `(* x x)` when `p` is false.

These examples have inadvertently revealed some of the structure of `comp-funcall`, which handles five cases.
First, it knows some primitive functions that have corresponding instructions and compiles these instructions inline when their values are needed.
If the values are not needed, then the function can be ignored, and just the arguments can be compiled.
This assumes true functions with no side effects.
If there are primitive operations with side effects, they too can be compiled inline, but the operation can never be ignored.
The next case is when the function is a lambda expression of no arguments.
We can just compile the body of the lambda expression as if it were a `begin` expression.
Nonprimitive functions require a function call.
There are two cases: when there is more to compile we have to save a continuation point, and when we are compiling the final value of a function, we can just branch to the called function.
The whole thing looks like this:

[ ](#){:#l0090}`(defun comp-funcall (f args env val?
more?)`
!!!(p) {:.unnumlist}

`  "Compile an application of a function to arguments."`
!!!(p) {:.unnumlist}

`  (let ((prim (primitive-p f env (length args))))`
!!!(p) {:.unnumlist}

`    (cond`
!!!(p) {:.unnumlist}

`      (prim ; function compilable to a primitive instruction`
!!!(p) {:.unnumlist}

`       (if (and (not val?) (not (prim-side-effects prim)))`
!!!(p) {:.unnumlist}

`            ;; Side-effect free primitive when value unused`
!!!(p) {:.unnumlist}

`            (comp-begin args env nil more?)`
!!!(p) {:.unnumlist}

`            ;; Primitive with value or call needed`
!!!(p) {:.unnumlist}

`            (seq (comp-list args env)`
!!!(p) {:.unnumlist}

`                 (gen (prim-opcode prim))`
!!!(p) {:.unnumlist}

`                 (unless val?
(gen 'POP))`
!!!(p) {:.unnumlist}

`                 (unless more?
(gen 'RETURN)))))`
!!!(p) {:.unnumlist}

`      ((and (starts-with f 'lambda) (null (second f)))`
!!!(p) {:.unnumlist}

`       ;; ((lambda () body)) => (begin body)`
!!!(p) {:.unnumlist}

`       (assert (null args) () "Too many arguments supplied")`
!!!(p) {:.unnumlist}

`       (comp-begin` (`rest2 f) env val?
more?))`
!!!(p) {:.unnumlist}

`      (more?
; Need to save the continuation point`
!!!(p) {:.unnumlist}

`       (let ((k (gen-label 'k)))`
!!!(p) {:.unnumlist}

`         (seq (gen 'SAVE k)`
!!!(p) {:.unnumlist}

`              (comp-list args env)`
!!!(p) {:.unnumlist}

`              (comp f env t t)`
!!!(p) {:.unnumlist}

`              (gen 'CALLJ (length args))`
!!!(p) {:.unnumlist}

`              (list k)`
!!!(p) {:.unnumlist}

`              (if (not val?) (gen 'POP)))))`
!!!(p) {:.unnumlist}

`       (t     ; function call as rename plus goto`
!!!(p) {:.unnumlist}

`        (seq (comp-list args env)`
!!!(p) {:.unnumlist}

`             (comp f env t t)`
!!!(p) {:.unnumlist}

`             (gen 'CALLJ (length args)))))))`
!!!(p) {:.unnumlist}

The support for primitives is straightforward.
The `prim` data type has five slots.
The first holds the name of a symbol that is globally bound to a primitive operation.
The second, `n-args`, is the number of arguments that the primitive requires.
We have to take into account the number of arguments to each function because we want `(+ x y)` to compile into a primitive addition instruction, while `(+ x y z)` should not.
It will compile into a call to the + function instead.
The `opcode` slot gives the opcode that is used to implement the primitive.
The `always` field is true if the primitive always returns non-nil, `false` if it always returns nil, and nil otherwise.
It is used in exercise 23.6.
Finally, the `side-effects` field says if the function has any side effects, like doing I/O or changing the value of an object.

[ ](#){:#l0095}`(defstruct (prim (:type list))`
!!!(p) {:.unnumlist}

`  symbol n-args opcode always side-effects)`
!!!(p) {:.unnumlist}

`(defparameter *primitive-fns*`
!!!(p) {:.unnumlist}

`  '((+ 2 + true) (− 2 - true) (* 2 * true) (/ 2 / true)`
!!!(p) {:.unnumlist}

`    (< 2 <) (> 2 >) (<= 2 <=) (>= 2 >=) (/= 2 /=) (= 2 =)`
!!!(p) {:.unnumlist}

`    (eq?
2 eq) (equal?
2 equal) (eqv?
2 eql)`
!!!(p) {:.unnumlist}

`    (not 1 not) (null?
1 not)`
!!!(p) {:.unnumlist}

`    (car 1 car) (cdr 1 cdr) (cadr 1 cadr) (cons 2 cons true)`
!!!(p) {:.unnumlist}

`    (list 1 list1 true) (list 2 list2 true) (list 3 list3 true)`
!!!(p) {:.unnumlist}

`    (read 0 read nil t) (write 1 write nil t) (display 1 display nil t)`
!!!(p) {:.unnumlist}

`    (newline 0 newline nil t) (compiler 1 compiler t)`
!!!(p) {:.unnumlist}

`    (name!
2 name!
true t) (random 1 random true nil)))`
!!!(p) {:.unnumlist}

`(defun primitive-p (f env n-args)`
!!!(p) {:.unnumlist}

`  "F is a primitive if it is in the table, and is not shadowed`
!!!(p) {:.unnumlist}

`  by something in the environment, and has the right number of args."`
!!!(p) {:.unnumlist}

`  (and (not (in-env-p f env))`
!!!(p) {:.unnumlist}

`       (find f *primitive-fns*`
!!!(p) {:.unnumlist}

`             :test #'(lambda (f prim)`
!!!(p) {:.unnumlist}

`                       (and (eq f (prim-symbol prim))`
!!!(p) {:.unnumlist}

`                            (= n-args (prim-n-args prim)))))))`
!!!(p) {:.unnumlist}

`(defun list1 (x) (list x))`
!!!(p) {:.unnumlist}

`(defun list2 (x y) (list x y))`
!!!(p) {:.unnumlist}

`(defun list3 (x y z) (list x y z))`
!!!(p) {:.unnumlist}

`(defun display (x) (princ x))`
!!!(p) {:.unnumlist}

`(defun newline () (terpri))`
!!!(p) {:.unnumlist}

These optimizations only work if the symbols are permanently bound to the global values given here.
We can enforce that by altering `gen-set` to preserve them as constants:

[ ](#){:#l0100}`(defun gen-set (var env)`
!!!(p) {:.unnumlist}

`  "Generate an instruction to set a variable to top-of-stack."`
!!!(p) {:.unnumlist}

`  (let ((p (in-env-p var env)))`
!!!(p) {:.unnumlist}

`    (if p`
!!!(p) {:.unnumlist}

`        (gen 'LSET (first p) (second p) ";" var)`
!!!(p) {:.unnumlist}

`        (if (assoc var *primitive-fns*)`
!!!(p) {:.unnumlist}

`            (error "Can't alter the constant ~ a" var)`
!!!(p) {:.unnumlist}

`            (gen 'GSET var)))))`
!!!(p) {:.unnumlist}

Now an expression like `(+ x 1)` will be properly compiled using the + instruction rather than a subroutine call, and an expression like `(set !
+ *)` will be flagged as an error when + is a global variable, but allowed when it has been locally bound.
However, we still need to be able to handle expressions like `(set !
add +)` and then `(add x y)`.
Thus, we need some function object that + will be globally bound to, even if the compiler normally optimizes away references to that function.
The function `init-scheme-comp` takes care of this requirement:

[ ](#){:#l0105}`(defun init-scheme-comp ()`
!!!(p) {:.unnumlist}

`  "Initialize the primitive functions."`
!!!(p) {:.unnumlist}

`  (dolist (prim *primitive-fns*)`
!!!(p) {:.unnumlist}

`     (setf (get (prim-symbol prim) 'global-val)`
!!!(p) {:.unnumlist}

`           (new-fn :env nil :name (prim-symbol prim)`
!!!(p) {:.unnumlist}

`                   :code (seq (gen 'PRIM (prim-symbol prim))`
!!!(p) {:.unnumlist}

`                              (gen 'RETURN))))))`
!!!(p) {:.unnumlist}

There is one more change to make–rewriting `comp-lambda`.
We still need to get the arguments off the stack, but we no longer generate a `RETURN` instruction, since that is done by `comp-begin`, if necessary.
At this point we'll provide a hook for a peephole optimizer, which will be introduced in [section 23.4](#s0025), and for an assembler to convert the assembly language to machine code, `new-fn` provides this interface, but for now, `new-fn` acts just like `make-fn`.

We also need to account for the possibility of rest arguments in a lambda list.
A new function, `gen-rgs`, generates the single instruction to load the arguments of the stack.
It introduces a new instruction, `ARGS`., into the abstract machine.
This instruction works just like `ARGS`, except it also conses any remaining arguments on the stack into a list and stores that list as the value of the rest argument.
With this innovation, the new version of `comp-lambda` looks like this:

[ ](#){:#l0110}`(defun comp-lambda (args body env)`
!!!(p) {:.unnumlist}

`  "Compile a lambda form into a closure with compiled code."`
!!!(p) {:.unnumlist}

`  (new-fn :env env :args args`
!!!(p) {:.unnumlist}

`          :code (seq (gen-args args 0)`
!!!(p) {:.unnumlist}

`                     (comp-begin body`
!!!(p) {:.unnumlist}

`                                 (cons (make-true-list args) env)`
!!!(p) {:.unnumlist}

`                                 t nil))))`
!!!(p) {:.unnumlist}

`(defun gen-args (args n-so-far)`
!!!(p) {:.unnumlist}

`  "Generate an instruction to load the arguments."`
!!!(p) {:.unnumlist}

`  (cond ((null args) (gen 'ARGS n-so-far))`
!!!(p) {:.unnumlist}

`        ((symbolp args) (gen 'ARGS.
n-so-far))`
!!!(p) {:.unnumlist}

`        ((and (consp args) (symbolp (first args)))`
!!!(p) {:.unnumlist}

`         (gen-args (rest args) (+ n-so-far 1)))`
!!!(p) {:.unnumlist}

`        (t (error "Illegal argument list"))))`
!!!(p) {:.unnumlist}

`(defun make-true-list (dotted-list)`
!!!(p) {:.unnumlist}

`  "Convert a possibly dotted list into a true, non-dotted list."`
!!!(p) {:.unnumlist}

`  (cond ((null dotted-list) nil)`
!!!(p) {:.unnumlist}

`        ((atom dotted-list) (list dotted-list))`
!!!(p) {:.unnumlist}

`        (t (cons (first dotted-list)`
!!!(p) {:.unnumlist}

`                 (make-true-list (rest dotted-list))))))`
!!!(p) {:.unnumlist}

`(defun new-fn (&key code env name args)`
!!!(p) {:.unnumlist}

`  "Build a new function."`
!!!(p) {:.unnumlist}

`  (assemble (make-fn :env env :name name :args args`
!!!(p) {:.unnumlist}

`                     :code (optimize code))))`
!!!(p) {:.unnumlist}

`new-fn` includes calls to an assembler and an optimizer to generate actual machine code.
For the moment, both will be identity functions:

[ ](#){:#l0115}`(defun optimize (code) code)`
!!!(p) {:.unnumlist}

`(defun assemble (fn) fn)`
!!!(p) {:.unnumlist}

Here are some more examples of the compiler at work:

[ ](#){:#t0085}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `> (comp-show ’(if (null? (car l)) (f (+ (* a x) b)) (g (/ x 2))))` |
| | `ARGS` | `0` | |
| | `GVAR` | `L` | |
| | `CAR` | | |
| | `FJUMP` | `L1` | |
| | `GVAR` | `X` | |
| | `2` | | |
| | / | | |
| | `GVAR` | `G` | |
| | `CALLJ` | `1` | |
| `L1:` | `GVAR` | `A` | |
| | | `GVAR` | `X` |
| | | `*` | |
| | | `GVAR` | `B` |
| | | `+` | |
| | | `GVAR` | `F` |
| | | `CALLJ` | `1` |

![t0085](images/B9780080571157500236/t0085.png)

There is no need to save any continuation points in this code, because the only calls to nonprimitive functions occur as the final values of the two branches of the function.

[ ](#){:#l0120}`> (comp-show ’(define (lastl l)`
!!!(p) {:.unnumlist}

`              (if (null?
(cdr l)) (car l)`
!!!(p) {:.unnumlist}

`                  (last1 (cdr l)))))`
!!!(p) {:.unnumlist}

[ ](#){:#t0090}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| | `ARGS` | `0` | | | | |
| | `FN` | | | | | |
| | `ARGS` | `1` | | | | |
| | `LVAR` | `0` | `0` | `;` | `L` | |
| | `CDR` | | | | | |
| | `FJUMP` | `L1` | | | | |
| | `LVAR` | `0` | `0` | `;` | `L` | |
| | `CDR` | | | | | |
| | `GVAR` | `LAST1` | | | | |
| | `CALLJ` | `1` | | | | |
| `L1:` | | `LVAR` | `0` | `0` | `;` | `L` |
| | | `CAR` | | | | |
| | | `RETURN` | | | | |
| | `GSET` | `LAST1` | | | | |
| | `CONST` | `LAST1` | | | | |
| | `NAME!` | | | | | |
| | `RETURN` | | | | | |

![t0090](images/B9780080571157500236/t0090.png)

The top-level function just assigns the nested function to the global variable `last1`.
Since `last1` is tail-recursive, it has only one return point, for the termination case, and just calls itself without saving continuations until that case is executed.

Contrast that to the non-tail-recursive definition of `length` below.
It is not tail-recursive because before it calls `length` recursively, it must save a continuation point, `K1`, so that it will know where to return to to add 1.

[ ](#){:#l0125}`> (comp-show ’(define (length l)`
!!!(p) {:.unnumlist}

`                (if (null?
l) 0 (+ 1 (length (cdr l))))))`
!!!(p) {:.unnumlist}

[ ](#){:#t0095}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| | `ARGS` | `0` | | | | |
| | `FN` | | | | | |
| | | `ARGS` | `1` | | | |
| | | `LVAR` | `0` | `0` | `;` | `L` |
| | | `FJUMP` | `L2` | | | |
| | | `1` | | | | |
| | | `SAVE` | `K1` | | | |
| | | `LVAR` | `0` | `0` | `;` | `L` |
| | | `CDR` | | | | |
| | | `GVAR` | `LENGTH` | | | |
| | | `CALLJ` | `1` | | | |
| `K1:` | | `+` | | | | |
| | | `RETURN` | | | | |
| `L2` | | `0` | | | | |
| | | `RETURN` | | | | |
| | `GSET` | `LENGTH` | | | | |
| | `CONST` | `LENGTH` | | | | |
| | `NAME!` | | | | | |
| | `RETURN` | | | | | |

![t0095](images/B9780080571157500236/t0095.png)

Of course, it is possible to write `length` in tail-recursive fashion:

[ ](#){:#l0130}`> (comp-show ’(define (length l)`
!!!(p) {:.unnumlist}

`              (letrec ((len (lambda (l n)`
!!!(p) {:.unnumlist}

`                              (if (null?
l) n`
!!!(p) {:.unnumlist}

`                                  (len (rest l) (+ n l))))))`
!!!(p) {:.unnumlist}

`                (len l 0))))`
!!!(p) {:.unnumlist}

[ ](#){:#t0100}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| | `ARGS` | `0` | | | | | | |
| | `FN` | | | | | | | |
| | | `ARGS` | `1` | | | | | |
| | | `NIL` | | | | | | |
| | | `FN` | | | | | | |
| | | | `ARGS` | `1` | | | | |
| | | | `FN` | | | | | |
| | | | | `ARGS` | `2` | | | |
| | | | | `LVAR` | `0` | `0` | `;` | `L` |
| | | | | `FJUMP` | `L2` | | | |
| | | | | `SAVE` | `K1` | | | |
| | | | | `LVAR` | `0` | `0` | `;` | `L` |
| | | | | `GVAR` | `REST` | | | |
| | | | | `CALLJ` | `1` | | | |
| `K1:` | | | | `LVAR` | `0` | `1` | `;` | `N` |
| | | | | `1` | | | | |
| | | | | `+` | | | | |
| | | | | `LVAR` | `1` | `0` | `;` | `LEN` |
| | | | | `CALLJ` | `2` | | | |
| `L2:` | | | | `LVAR` | `0` | `1` | `;` | `N` |
| | | | | `RETURN` | | | | |
| | | | `LSET` | `0` | `0` | `;` | `LEN` | |
| | | | `POP` | | | | | |
| | | | `LVAR` | `1` | `0` | `;` | `L` | |
| | | | `0` | | | | | |
| | | | `LVAR` | `0` | `0` | `;` | `LEN` | |
| | | | `CALLJ` | `2` | | | | |
| | | `CALLJ` | `1` | | | | | |
| | `GSET` | `LENGTH` | | | | | | |
| | `CONST` | `LENGTH` | | | | | | |
| | `NAME!` | | | | | | | |
| | `RETURN` | | | | | | | |

![t0100](images/B9780080571157500236/t0100.png)

Let’s look once again at an example with nested conditionals:

[ ](#){:#t0105}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `> (comp-show ’(if (not (and p q (not r))) x y))` |
| | `ARGS` | `0` |
| | `GVAR` | `P` |
| | `FJUMP` | `L3` |
| | `GVAR` | `Q` |
| | `FJUMP` | `L1` |
| | `GVAR` | `R` |
| | `NOT` | |
| | `JUMP` | `L2` |
| `L1:` | `NIL` | |
| `L2:` | `JUMP` | `L4` |
| `L3:` | `NIL` | |
| `L4:` | `FJUMP` | `L5` |
| | `GVAR` | `Y` |
| | `RETURN` | |
| `L5:` | `GVAR` | `X` |
| | `RETURN` | |

![t0105](images/B9780080571157500236/t0105.png)

Here the problem is with multiple `JUMP`s and with not recognizing negation.
If `p` is false, then the and expression is false, and the whole predicate is true, so we should return `x`.
The code does in fact return `x`, but it first jumps to `L3`, loads `NIL`, and then does an `FJUMP` that will always jump to `L5`.
Other branches have similar inefficiencies.
A sufficiently clever compiler should be able to generate the following code:

[ ](#){:#t0110}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| | `ARGS` | `0` |
| | `GVAR` | `P` |
| | `FJUMP` | `L1` |
| | `GVAR` | `Q` |
| | `FJUMP` | `L1` |
| | `GVAR` | `R` |
| | `TJUMP` | `L1` |
| | `GVAR` | `Y` |
| | `RETURN` | |
| `L1:` | `GVAR X` | |
| | `RETURN` | |

## [ ](#){:#st0015}23.2 Introducing Call/cc
{:#s0015}
{:.h1hd}

Now that the basic compiler works, we can think about how to implement `call/cc` in our compiler.
First, remember that `call/cc` is a normal function, not a special form.
So we could define it as a primitive, in the manner of `car` and `cons`.
However, primitives as they have been defined only get to see their arguments, and `call/cc` will need to see the run-time stack, in order to save away the current continuation.
One choice is to install `call/cc` as a normal Scheme nonprimitive function but to write its body in assembly code ourselves.
We need to introduce one new instruction, `CC`, which places on the stack a function (to which we also have to write the assembly code by hand) that saves the current continuation (the stack) in its environment, and, when called, fetches that continuation and installs it, by setting the stack back to that value.
This requires one more instruction, `SET-CC`.
The details of this, and of all the other instructions, are revealed in the next section.

## [ ](#){:#st0020}23.3 The Abstract Machine
{:#s0020}
{:.h1hd}

So far we have defined the instruction set of a mythical abstract machine and generated assembly code for that instruction set.
It’s now time to actually execute the assembly code and hence have a useful compiler.
There are several paths we could pursue: we could implement the machine in hardware, software, or microcode, or we could translate the assembly code for our abstract machine into the assembly code of some existing machine.
Each of these approaches has been taken in the past.

**Hardware.** If the abstract machine is simple enough, it can be implemented directly in hardware.
The Scheme-79 and Scheme-81 Chips ([Steele and Sussman 1980](B9780080571157500285.xhtml#bb1180); [Batali et al.
1982](B9780080571157500285.xhtml#bb0070)) were VLSI implementations of a machine designed specifically to run Scheme.

**Macro-Assembler.** In the translation or macro-assembler approach, each instruction in the abstract machine language is translated into one or more instructions in the host computer’s instruction set.
This can be done either directly or by generating assembly code and passing it to the host computer’s assembler.
In general this will lead to code expansion, because the host computer probably will not provide direct support for Scheme’s data types.
Thus, whereas in our abstract machine we could write a single instruction for addition, with native code we might have to execute a series of instructions to check the type of the arguments, do an integer add if they are both integers, a floating-point add if they are both floating-point numbers, and so on.
We might also have to check the result for overflow, and perhaps convert to bignum representation.
Compilers that generate native code often include more sophisticated data-flow analysis to know when such checks are required and when they can be omitted.

**Microcode.** The MIT Lisp Machine project, unlike the Scheme Chip, actually resulted in working machines.
One important decision was to go with microcode instead of a single chip.
This made it easy to change the system as experienced was gained, and as the host language was changed from ZetaLisp to Common Lisp.
The most important architectural feature of the Lisp Machine was the inclusion of tag bits on each word to specify data types.
Also important was microcode to implement certain frequently used generic operations.
For example, in the Symbolics 3600 Lisp Machine, the microcode for addition simultaneously did an integer add, a floating-point add, and a check of the tag bits.
If both arguments turned out to be either integers or floating-point numbers, then the appropriate result was taken.
Otherwise, a trap was signaled, and a converison routine was entered.
This approach makes the compiler relatively simple, but the trend in architecture is away from highly microcoded processors toward simpler (RISC) processors.

**Software.** We can remove many of these problems with a technique known as *byte-code assembly.* Here we translate the instructions into a vector of bytes and then interpret the bytes with a byte-code interpreter.
This gives us (almost) the machine we want; it solves the code expansion problem, but it may be slower than native code compilation, because the byte-code interpreter is written in software, not hardware or microcode.

Each opcode is a single byte (we have less than 256 opcodes, so this will work).
The instructions with arguments take their arguments in the following bytes of the instruction stream.
So, for example, a `CALL` instruction occupies two bytes; one for the opcode and one for the argument count.
This means we have imposed a limit of 256 arguments to a function call.
An `LVAR` instruction would take three bytes; one for the opcode, one for the frame offset, and one for the offset within the frame.
Again, we have imposed 256 as the limit on nesting level and variables per frame.
These limits seem high enough for any code written by a human, but remember, not only humans write code.
It is possible that some complex macro may expand into something with more than 256 variables, so a full implementation would have some way of accounting for this.
The `GVAR` and `CONST` instructions have to refer to an arbitrary object; either we can allocate enough bytes to fit a pointer to this object, or we can add a `constants` field to the `fn` structure, and follow the instructions with a single-byte index into this vector of constants.
This latter approach is more common.

We can now handle branches by changing the program counter to an index into the code vector.
(It seems severe to limit functions to 256 bytes of code; a two-byte label allows for 65536 bytes of code per function.) In summary, the code is more compact, branching is efficient, and dispatching can be fast because the opcode is a small integer, and we can use a branch table to go to the right piece of code for each instruction.

Another source of inefficiency is implementing the stack as a list, and consing up new cells every time something is added to the stack.
The alternative is to implement the stack as a vector with a fill-pointer.
That way a push requires no consing, only a change to the pointer (and a check for overflow).
The check is worthwhile, however, because it allows us to detect infinite loops in the user’s code.

Here follows an assembler that generates a sequence of instructions (as a vector).
This is a compromise between byte codes and the assembly language format.
First, we need some accessor functions to get at parts of an instruction:

[ ](#){:#l0135}`(defun opcode (instr) (if (label-p instr) :label (first instr)))`
!!!(p) {:.unnumlist}

`(defun args (instr) (if (listp instr) (rest instr)))`
!!!(p) {:.unnumlist}

`(defun arg1 (instr) (if (listp instr) (second instr)))`
!!!(p) {:.unnumlist}

`(defun arg2 (instr) (if (listp instr) (third instr)))`
!!!(p) {:.unnumlist}

`(defun arg3 (instr) (if (listp instr) (fourth instr)))`
!!!(p) {:.unnumlist}

`(defsetf arg1 (instr) (val) ‘(setf (second ,instr) ,val))`
!!!(p) {:.unnumlist}

Now we write the assembler, which already is integrated into the compiler with a hook in `new-fn`.

[ ](#){:#l0140}`(defun assemble (fn)`
!!!(p) {:.unnumlist}

`  "Turn a list of instructions into a vector."`
!!!(p) {:.unnumlist}

`  (multiple-value-bind (length labels)`
!!!(p) {:.unnumlist}

`     (asm-first-pass (fn-code fn))`
!!!(p) {:.unnumlist}

`   (setf (fn-code fn)`
!!!(p) {:.unnumlist}

`         (asm-second-pass (fn-code fn)`
!!!(p) {:.unnumlist}

`                          length labels))`
!!!(p) {:.unnumlist}

`   fn))`
!!!(p) {:.unnumlist}

`(defun asm-first-pass (code)`
!!!(p) {:.unnumlist}

`  "Return the labels and the total code length."`
!!!(p) {:.unnumlist}

`  (let ((length 0)`
!!!(p) {:.unnumlist}

`        (labels nil))`
!!!(p) {:.unnumlist}

`    (dolist (instr code)`
!!!(p) {:.unnumlist}

`      (if (label-p instr)`
!!!(p) {:.unnumlist}

`          (push (cons instr length) labels)`
!!!(p) {:.unnumlist}

`          (incf length)))`
!!!(p) {:.unnumlist}

`      (values length labels)))`
!!!(p) {:.unnumlist}

`(defun asm-second-pass (code length labels)`
!!!(p) {:.unnumlist}

`  "Put code into code-vector, adjusting for labels."`
!!!(p) {:.unnumlist}

`  (let ((addr 0)`
!!!(p) {:.unnumlist}

`        (code-vector (make-array length)))`
!!!(p) {:.unnumlist}

`    (dolist (instr code)`
!!!(p) {:.unnumlist}

`      (unless (label-p instr)`
!!!(p) {:.unnumlist}

`        (if (is instr ‘(JUMP TJUMP FJUMP SAVE))`
!!!(p) {:.unnumlist}

`            (setf (arg1 instr)`
!!!(p) {:.unnumlist}

`                  (cdr (assoc (arg1 instr) labels))))`
!!!(p) {:.unnumlist}

`        (setf (aref code-vector addr) instr)`
!!!(p) {:.unnumlist}

`        (incf addr)))`
!!!(p) {:.unnumlist}

`    code-vector))`
!!!(p) {:.unnumlist}

If we want to be able to look at assembled code, we need a new printing function:

[ ](#){:#l0145}`(defun show-fn (fn &optional (stream *standard-output*) (indent 2))`
!!!(p) {:.unnumlist}

`  "Print all the instructions in a function.`
!!!(p) {:.unnumlist}

`  If the argument is not a function, just princ it,`
!!!(p) {:.unnumlist}

`  but in a column at least 8 spaces wide."`
!!!(p) {:.unnumlist}

`  ;; This version handles code that has been assembled into a vector`
!!!(p) {:.unnumlist}

`  (if (not (fn-p fn))`
!!!(p) {:.unnumlist}

`      (format stream "~8a" fn)`
!!!(p) {:.unnumlist}

`      (progn`
!!!(p) {:.unnumlist}

`        (fresh-line)`
!!!(p) {:.unnumlist}

`        (dotimes (i (length (fn-code fn)))`
!!!(p) {:.unnumlist}

`          (let ((instr (elt (fn-code fn) i)))`
!!!(p) {:.unnumlist}

`            (if (label-p instr)`
!!!(p) {:.unnumlist}

`       (format stream "~a:" instr)`
!!!(p) {:.unnumlist}

`       (progn`
!!!(p) {:.unnumlist}

`        (format stream "~VT~2d: " indent i)`
!!!(p) {:.unnumlist}

`        (dolist (arg instr)`
!!!(p) {:.unnumlist}

`         (show-fn arg stream (+ indent 8)))`
!!!(p) {:.unnumlist}

`        (fresh-line))))))))`
!!!(p) {:.unnumlist}

`(defstruct ret-addr fn pc env)`
!!!(p) {:.unnumlist}

`(defun is (instr op)`
!!!(p) {:.unnumlist}

`   "True if instr’s opcode is OP, or one of OP when OP is a list."`
!!!(p) {:.unnumlist}

`   (if (listp op)`
!!!(p) {:.unnumlist}

`       (member (opcode instr) op)`
!!!(p) {:.unnumlist}

`       (eq (opcode instr) op)))`
!!!(p) {:.unnumlist}

`(defun top (stack) (first stack))`
!!!(p) {:.unnumlist}

`(defun machine (f)`
!!!(p) {:.unnumlist}

`   "Run the abstract machine on the code for f."`
!!!(p) {:.unnumlist}

`   (let* ((code (fn-code f))`
!!!(p) {:.unnumlist}

`            (pc 0)`
!!!(p) {:.unnumlist}

`            (env nil )`
!!!(p) {:.unnumlist}

`            (stack nil)`
!!!(p) {:.unnumlist}

`            (n-args 0)`
!!!(p) {:.unnumlist}

`            (instr))`
!!!(p) {:.unnumlist}

`   (loop`
!!!(p) {:.unnumlist}

`      (setf instr (elt code pc))`
!!!(p) {:.unnumlist}

`      (incf pc)`
!!!(p) {:.unnumlist}

`      (case (opcode instr)`
!!!(p) {:.unnumlist}

`         ;; Variable/stack manipulation instructions:`
!!!(p) {:.unnumlist}

`         (LVAR (push (elt (elt env (arg1 instr)) (arg2 instr))`
!!!(p) {:.unnumlist}

`                                     stack))`
!!!(p) {:.unnumlist}

`         (LSET (setf (elt (elt env (arg1 instr)) (arg2 instr))`
!!!(p) {:.unnumlist}

`                                     (top stack)))`
!!!(p) {:.unnumlist}

`         (GVAR (push (get (arg1 instr) ’global-val) stack))`
!!!(p) {:.unnumlist}

`         (GSET (setf (get (arg1 instr) ’global-val) (top stack)))`
!!!(p) {:.unnumlist}

`         (POP (pop stack))`
!!!(p) {:.unnumlist}

`         (CONST (push (arg1 instr) stack))`
!!!(p) {:.unnumlist}

`         ;; Branching instructions:`
!!!(p) {:.unnumlist}

`         (JUMP (setf pc (arg1 instr)))`
!!!(p) {:.unnumlist}

`         (FJUMP (if (null (pop stack)) (setf pc (arg1 instr))))`
!!!(p) {:.unnumlist}

`         (TJUMP (if (pop stack) (setf pc (arg1 instr))))`
!!!(p) {:.unnumlist}

`         ;; Function call/return instructions:`
!!!(p) {:.unnumlist}

`         (SAVE (push (make-ret-addr :pc (arg1 instr)`
!!!(p) {:.unnumlist}

`                                                       :fn f :env env)`
!!!(p) {:.unnumlist}

`                               stack))`
!!!(p) {:.unnumlist}

`         (RETURN ;; return value is top of stack; ret-addr is second`
!!!(p) {:.unnumlist}

`           (setf f (ret-addr-fn (second stack))`
!!!(p) {:.unnumlist}

`                   code (fn-code f)`
!!!(p) {:.unnumlist}

`                   env (ret-addr-env (second stack))`
!!!(p) {:.unnumlist}

`                   pc (ret-addr-pc (second stack)))`
!!!(p) {:.unnumlist}

`           ;; Get rid of the ret-addr, but keep the value`
!!!(p) {:.unnumlist}

`           (setf stack (cons (first stack) (rest2 stack))))`
!!!(p) {:.unnumlist}

`         (CALLJ (pop env)                  ; discard the top frame`
!!!(p) {:.unnumlist}

`                       (setf f (pop stack)`
!!!(p) {:.unnumlist}

`                       code (fn-code f)`
!!!(p) {:.unnumlist}

`                       env (fn-env f)`
!!!(p) {:.unnumlist}

`                       pc 0`
!!!(p) {:.unnumlist}

`                       n-args (arg1 instr)))`
!!!(p) {:.unnumlist}

`         (ARGS (assert (= n-args (arg1 instr)) ()`
!!!(p) {:.unnumlist}

`                                         "Wrong number of arguments:~`
!!!(p) {:.unnumlist}

`                                         ~d expected, ~ d supplied"`
!!!(p) {:.unnumlist}

`                                         (arg1 instr) n-args)`
!!!(p) {:.unnumlist}

`                          (push (make-array (arg1 instr)) env)`
!!!(p) {:.unnumlist}

`                          (loop for i from (− n-args 1) downto 0 do`
!!!(p) {:.unnumlist}

`                                   (setf (elt (first env) i) (pop stack))))`
!!!(p) {:.unnumlist}

`        (ARGS.
(assert (>= n-args (arg1 instr)) ()`
!!!(p) {:.unnumlist}

`                                         "Wrong number of arguments:~`
!!!(p) {:.unnumlist}

`                                         ~d or more expected, ~ d supplied"`
!!!(p) {:.unnumlist}

`                                         (arg1 instr) n-args)`
!!!(p) {:.unnumlist}

`                          (push (make-array (+ 1 (arg1 instr))) env)`
!!!(p) {:.unnumlist}

`                          (loop repeat (− n-args (arg1 instr)) do`
!!!(p) {:.unnumlist}

`                                    (push (pop stack) (elt (first env) (arg1 instr))))`
!!!(p) {:.unnumlist}

`                          (loop for i from (- (arg1 instr) 1) downto 0 do`
!!!(p) {:.unnumlist}

`                                    (setf (elt (first env) i) (pop stack))))`
!!!(p) {:.unnumlist}

`        (FN (push (make-fn :code (fn-code (arg1 instr))`
!!!(p) {:.unnumlist}

`                                       :env env) stack))`
!!!(p) {:.unnumlist}

`        (PRIM (push (apply (arg1 instr)`
!!!(p) {:.unnumlist}

`                                    (loop with args = nil repeat n-args`
!!!(p) {:.unnumlist}

`                                              do (push (pop stack) args)`
!!!(p) {:.unnumlist}

`                                              finally (return args)))`
!!!(p) {:.unnumlist}

`                              stack))`
!!!(p) {:.unnumlist}

`        ;; Continuation instructions:`
!!!(p) {:.unnumlist}

`        (SET-CC (setf stack (top stack)))`
!!!(p) {:.unnumlist}

`        (CC    (push(make-fn`
!!!(p) {:.unnumlist}

`                             :env (list (vector stack))`
!!!(p) {:.unnumlist}

`                             :code ’((ARGS 1) (LVAR 1 0 ";" stack) (SET-CC)`
!!!(p) {:.unnumlist}

`                                        (LVAR 0 0) (RETURN)))`
!!!(p) {:.unnumlist}

`                              stack))`
!!!(p) {:.unnumlist}

`        ;; Nullary operations:`
!!!(p) {:.unnumlist}

`        ((SCHEME-READ NEWLINE)`
!!!(p) {:.unnumlist}

`          (push (funcall (opcode instr)) stack))`
!!!(p) {:.unnumlist}

`        ;; Unary operations:`
!!!(p) {:.unnumlist}

`        ((CAR CDR CADR NOT LIST1 COMPILER DISPLAY WRITE RANDOM)`
!!!(p) {:.unnumlist}

`        (push (funcall (opcode instr) (pop stack)) stack))`
!!!(p) {:.unnumlist}

`        ;; Binary operations:`
!!!(p) {:.unnumlist}

`        ((+−*/<><= >=/== CONS LIST2 NAME!
EQ EQUAL EQL)`
!!!(p) {:.unnumlist}

`         (setf stack (cons (funcall (opcode instr) (second stack)`
!!!(p) {:.unnumlist}

`                                              (first stack))`
!!!(p) {:.unnumlist}

`                                    (rest2 stack))))`
!!!(p) {:.unnumlist}

`        ;; Ternary operations:`
!!!(p) {:.unnumlist}

`        (LIST3`
!!!(p) {:.unnumlist}

`         (setf stack (cons (funcall (opcode instr) (third stack)`
!!!(p) {:.unnumlist}

`                                               (second stack) (first stack))`
!!!(p) {:.unnumlist}

`                                    (rest3 stack))))`
!!!(p) {:.unnumlist}

`        ;; Constants:`
!!!(p) {:.unnumlist}

`        ((T NIL -1 0 12)`
!!!(p) {:.unnumlist}

`         (push (opcode instr) stack))`
!!!(p) {:.unnumlist}

`        ;; Other:`
!!!(p) {:.unnumlist}

`        ((HALT) (RETURN (top stack)))`
!!!(p) {:.unnumlist}

`        (otherwise (error "Unknown opcode: ~ a" instr))))))`
!!!(p) {:.unnumlist}

`(defun init-scheme-comp ()`
!!!(p) {:.unnumlist}

`   "Initialize values (including call/cc) for the Scheme compiler."`
!!!(p) {:.unnumlist}

`   (set-global-var!
’exit`
!!!(p) {:.unnumlist}

`      (new-fn :name ’exit :args ’(val) :code ’((HALT))))`
!!!(p) {:.unnumlist}

`   (set-global-var!
’call/cc`
!!!(p) {:.unnumlist}

`      (new-fn :name ’call/cc :args ’(f)`
!!!(p) {:.unnumlist}

`                   :code ’((ARGS 1) (CC) (LVAR 0 0 “;” f) (CALLJ 1))))`
!!!(p) {:.unnumlist}

`   (dolist (prim *primitive-fns*)`
!!!(p) {:.unnumlist}

`       (setf (get (prim-symbol prim) ’global-val)`
!!!(p) {:.unnumlist}

`                   (new-fn :env nil :name (prim-symbol prim)`
!!!(p) {:.unnumlist}

`                                           :code (seq (gen ’PRIM (prim-symbol prim))`
!!!(p) {:.unnumlist}

`                                                      (gen ’RETURN))))))`
!!!(p) {:.unnumlist}

Here’s the Scheme top level.
Note that it is written in Scheme itself; we compile the definition of the read-eval-print loop,[1](#fn0010){:#xfn0010} load it into the machine, and then start executing it.
There’s also an interface to compile and execute a single expression, `comp-go`.

[ ](#){:#l0150}`(defconstant scheme-top-level`
!!!(p) {:.unnumlist}

`   ’(begin(define (scheme)`
!!!(p) {:.unnumlist}

`                 (newline)`
!!!(p) {:.unnumlist}

`                 (display "=> ")`
!!!(p) {:.unnumlist}

`                 (write ((compiler (read))))`
!!!(p) {:.unnumlist}

`                 (scheme))`
!!!(p) {:.unnumlist}

`             (scheme)))`
!!!(p) {:.unnumlist}

`(defun scheme ( )`
!!!(p) {:.unnumlist}

`   "A compiled Scheme read-eval-print loop"`
!!!(p) {:.unnumlist}

`   (init-scheme-comp)`
!!!(p) {:.unnumlist}

`   (machine (compiler scheme-top-level)))`
!!!(p) {:.unnumlist}

`(defun comp-go (exp)`
!!!(p) {:.unnumlist}

`   "Compile and execute the expression."`
!!!(p) {:.unnumlist}

`   (machine (compiler ‘(exit ,exp))))`
!!!(p) {:.unnumlist}

**Exercise 23.2 [m]** This implementation of the machine is wasteful in its representation of environments.
For example, consider what happens in a tail-recursive function.
Each `ARG` instruction builds a new frame and pushes it on the environment.
Then each `CALL` pops the latest frame off the environment.
So, while the stack does not grow with tail-recursive calls, the heap certainly does.
Eventually, we will have to garbage-collect all those unused frames (and the cons cells used to make lists out of them).
How could we avoid or limit this garbage collection?

## [ ](#){:#st0025}23.4 A Peephole Optimizer
{:#s0025}
{:.h1hd}

In this section we investigate a simple technique that will generate slightly better code in cases where the compiler gives inefficient sequences of instructions.
The idea is to look at short sequences of instructions for prespecified patterns and replace them with equivalent but more efficient instructions.

In the following example, `comp-if` has already done some source-level optimization, such as eliminating the `(f x)` call.

[ ](#){:#t0115}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `> (comp-show ’(begin (if (if t 1 (f x)) (set! x 2)) x))` |
| `0:` | `ARGS` | `0` |
| 1: | 1 | |
| `2:` | `FJUMP` | `6` |
| `3:` | `2` | |
| `4:` | `GSET` | `X` |
| `5:` | `POP` | |
| `6:` | `GVAR` | `X` |
| `7:` | `RETURN` | |

![t0115](images/B9780080571157500236/t0115.png)

But the generated code could be made much better.
This could be done with more source-level optimizations to transform the expression into `(set!
x 2)`.
Alternatively, it could also be done by looking at the preceding instruction sequence and transforming local inefficiencies.
The optimizer presented in this section is capable of generating the following code:

[ ](#){:#t0120}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `> (comp-show ‘(begin (if (if t 1 (f x)) (set! x 2)) x))` |
| `0:` | `ARGS` | `0` |
| 1: | 2 | |
| `2:` | `GSET` | `X` |
| `3:` | `RETURN` | |

![t0120](images/B9780080571157500236/t0120.png)

The function `optimize` is implemented as a data-driven function that looks at the opcode of each instruction and makes optimizations based on the following instructions.
To be more specific, `optimize` takes a list of assembly language instructions and looks at each instruction in order, trying to apply an optimization.
If any changes at all are made, then `optimize` will be called again on the whole instruction list, because further changes might be triggered by the first round of changes.

[ ](#){:#l0155}`(defun optimize (code)`
!!!(p) {:.unnumlist}

`   "Perform peephole optimization on assembly code."`
!!!(p) {:.unnumlist}

`   (let ((any-change nil))`
!!!(p) {:.unnumlist}

`       ;; Optimize each tail`
!!!(p) {:.unnumlist}

`       (loop for code-tail on code do`
!!!(p) {:.unnumlist}

`                (setf any-change (or (optimize-1 code-tail code)`
!!!(p) {:.unnumlist}

`                                                any-change)))`
!!!(p) {:.unnumlist}

`       ;; If any changes were made, call optimize again`
!!!(p) {:.unnumlist}

`       (if any-change`
!!!(p) {:.unnumlist}

`           (optimize code)`
!!!(p) {:.unnumlist}

`           code)))`
!!!(p) {:.unnumlist}

The function `optimize-1` is responsible for each individual attempt to optimize.
It is passed two arguments: a list of instructions starting at the current one and going to the end of the list, and a list of all the instructions.
The second argument is rarely used.
The whole idea of a peephole optimizer is that it should look at only a few instructions following the current one.
`optimize-1` is data-driven, based on the opcode of the first instruction.
Note that the optimizer functions do their work by destructively modifying the instruction sequence, *not* by consing up and returning a new sequence.

[ ](#){:#l0160}`(defun optimize-1 (code all-code)`
!!!(p) {:.unnumlist}

`   "Perform peephole optimization on a tail of the assembly code.`
!!!(p) {:.unnumlist}

`   If a change is made, return true."`
!!!(p) {:.unnumlist}

`   ;; Data-driven by the opcode of the first instruction`
!!!(p) {:.unnumlist}

`   (let* ((instr (first code))`
!!!(p) {:.unnumlist}

`             (optimizer (get-optimizer (opcode instr))))`
!!!(p) {:.unnumlist}

`      (when optimizer`
!!!(p) {:.unnumlist}

`        (funcall optimizer instr code all-code))))`
!!!(p) {:.unnumlist}

We need a table to associate the individual optimizer functions with the opcodes.
Since opcodes include numbers as well as symbols, an `eql` hash table is an appropriate choice:

[ ](#){:#l0165}`(let ((optimizers (make-hash-table :test #’eql)))`
!!!(p) {:.unnumlist}

`   (defun get-optimizer (opcode)`
!!!(p) {:.unnumlist}

`       "Get the assembly language optimizer for this opcode."`
!!!(p) {:.unnumlist}

`       (gethash opcode optimizers))`
!!!(p) {:.unnumlist}

`   (defun put-optimizer (opcode fn)`
!!!(p) {:.unnumlist}

`       "Store an assembly language optimizer for this opcode."`
!!!(p) {:.unnumlist}

`       (setf (gethash opcode optimizers) fn)))`
!!!(p) {:.unnumlist}

We could now build a table with `put-optimizer`, but it is worth defining a macro to make this a little neater:

[ ](#){:#l0170}`(defmacro def-optimizer (opcodes args &body body)`
!!!(p) {:.unnumlist}

`   "Define assembly language optimizers for these opcodes."`
!!!(p) {:.unnumlist}

`   (assert (and (listp opcodes) (listp args) (= (length args) 3)))`
!!!(p) {:.unnumlist}

`   ‘(dolist (op ’.opcodes)`
!!!(p) {:.unnumlist}

`        (put-optimizer op #’(lambda .args ..body))))`
!!!(p) {:.unnumlist}

Before showing example optimizer functions, we will introduce three auxiliary functions.
`gen1` generates a single instruction, `target` finds the code sequence that a jump instruction branches to, and `next-instr` finds the next actual instruction in a sequence, skipping labels.

[ ](#){:#l0175}`(defun gen1 (&rest args) "Generate a single instruction" args)`
!!!(p) {:.unnumlist}

`(defun target (instr code) (second (member (arg1 instr) code)))`
!!!(p) {:.unnumlist}

`(defun next-instr (code) (find-if (complement #’label-p) code))`
!!!(p) {:.unnumlist}

Here are six optimizer functions that implement a few important peephole optimizations.

[ ](#){:#l0180}`(def-optimizer (: LABEL) (instr code all-code)`
!!!(p) {:.unnumlist}

`   ;; … L … => ;if no reference to L`
!!!(p) {:.unnumlist}

`   (when (not (find instr all-code :key #’arg1))`
!!!(p) {:.unnumlist}

`        (setf (first code) (second code)`
!!!(p) {:.unnumlist}

`                (rest code) (rest2 code))`
!!!(p) {:.unnumlist}

`        t))`
!!!(p) {:.unnumlist}

`(def-optimizer (GSET LSET) (instr code all-code)`
!!!(p) {:.unnumlist}

`   ;; ex: (begin (set!
x y) (if x z))`
!!!(p) {:.unnumlist}

`   ;; (SET X) (POP) (VAR X) ==> (SET X)`
!!!(p) {:.unnumlist}

`   (when (and (is (second code) ’POP)`
!!!(p) {:.unnumlist}

`              (is (third code) ’(GVAR LVAR))`
!!!(p) {:.unnumlist}

`              (eq (arg1 instr) (arg1 (third code))))`
!!!(p) {:.unnumlist}

`       (setf (rest code) (nthcdr 3 code))`
!!!(p) {:.unnumlist}

`       t))`
!!!(p) {:.unnumlist}

`(def-optimizer (JUMP CALL CALLJ RETURN) (instr code all-code)`
!!!(p) {:.unnumlist}

`   ;; (JUMP L1) …dead code… L2 ==> (JUMP L1) L2`
!!!(p) {:.unnumlist}

`   (setf (rest code) (member-if #’label-p (rest code)))`
!!!(p) {:.unnumlist}

`   ;; (JUMP L1) … L1 (JUMP L2) ==> (JUMP L2) … L1 (JUMP L2)`
!!!(p) {:.unnumlist}

`   (when (and (is instr ’JUMP)`
!!!(p) {:.unnumlist}

`                      (is (target instr code) ’(JUMP RETURN))`
!!!(p) {:.unnumlist}

`      (setf (first code) (copy-list (target instr code)))`
!!!(p) {:.unnumlist}

`      t)))`
!!!(p) {:.unnumlist}

`(def-optimizer (TJUMP FJUMP) (instr code all-code)`
!!!(p) {:.unnumlist}

`   ;; (FJUMP L1) … L1 (JUMP L2) ==> (FJUMP L2) … L1 (JUMP L2)`
!!!(p) {:.unnumlist}

`   (when (is (target instr code) ‘JUMP)`
!!!(p) {:.unnumlist}

`      (setf (second instr) (arg1 (target instr code)))`
!!!(p) {:.unnumlist}

`      t))`
!!!(p) {:.unnumlist}

`(def-optimizer (T -1 0 1 2) (instr code all-code)`
!!!(p) {:.unnumlist}

`   (case (opcode (second code))`
!!!(p) {:.unnumlist}

`      (NOT ;; (T) (NOT) ==> NIL`
!!!(p) {:.unnumlist}

`        (setf (first code) (gen1 ’NIL)`
!!!(p) {:.unnumlist}

`                (rest code) (rest2 code))`
!!!(p) {:.unnumlist}

`        t)`
!!!(p) {:.unnumlist}

`      (FJUMP ;; (T) (FJUMP L) … =>…`
!!!(p) {:.unnumlist}

`        (setf (first code) (third code)`
!!!(p) {:.unnumlist}

`                (rest code) (rest3 code))`
!!!(p) {:.unnumlist}

`        t)`
!!!(p) {:.unnumlist}

`      (TJUMP ;; (T) (TJUMP L) … => (JUMP L) …`
!!!(p) {:.unnumlist}

`        (setf (first code) (gen1 ‘JUMP (arg1 (next-instr code))))`
!!!(p) {:.unnumlist}

`        t)))`
!!!(p) {:.unnumlist}

`(def-optimizer (NIL) (instr code all-code)`
!!!(p) {:.unnumlist}

`   (case (opcode (second code))`
!!!(p) {:.unnumlist}

`     (NOT ;; (NIL) (NOT) ==> T`
!!!(p) {:.unnumlist}

`        (setf (first code) (gen1 ’T)`
!!!(p) {:.unnumlist}

`              (rest code) (rest2 code))`
!!!(p) {:.unnumlist}

`        t)`
!!!(p) {:.unnumlist}

`   (TJUMP ;; (NIL) (TJUMP L) … =>…`
!!!(p) {:.unnumlist}

`   (setf (first code) (third code)`
!!!(p) {:.unnumlist}

`           (rest code) (rest3 code))`
!!!(p) {:.unnumlist}

`   t)`
!!!(p) {:.unnumlist}

`   (FJUMP ;; (NIL) (FJUMP L) ==> (JUMP L)`
!!!(p) {:.unnumlist}

`   (setf (first code) (gen1 ’JUMP (arg1 (next-instr code))))`
!!!(p) {:.unnumlist}

`   t)))`
!!!(p) {:.unnumlist}

## [ ](#){:#st0030}23.5 Languages with Different Lexical Conventions
{:#s0030}
{:.h1hd}

This chapter has shown how to evaluate a language with Lisp-like syntax, by writing a read-eval-print loop where only the `eval` needs to be replaced.
In this section we see how to make the `read` part slightly more general.
We still read Lisp-like syntax, but the lexical conventions can be slightly different.

The Lisp function `read` is driven by an object called the *readtable,* which is stored in the special variable `*readtable*.` This table associates some action to take with each of the possible characters that can be read.
The entry in the readtable for the character `#\(`, for example, would be directions to read a list.
The entry for `#\;` would be directions to ignore every character up to the end of the line.

Because the readtable is stored in a special variable, it is possible to alter completely the way read works just by dynamically rebinding this variable.

The new function `scheme - read` temporarily changes the readtable to a new one, the Scheme readtable.
It also accepts an optional argument, the stream to read from, and it returns a special marker on end of file.
This can be tested for with the predicate `eof-object?`.
Note that once `scheme-read` is installed as the value of the Scheme `symbol-read` we need do no more—`scheme-read` will always be called when appropriate (by the top level of Scheme, and by any user Scheme program).

[ ](#){:#l0185}`(defconstant eof "EoF")`
!!!(p) {:.unnumlist}

`(defun eof-object?
(x) (eq x eof))`
!!!(p) {:.unnumlist}

`(defvar *scheme-readtable* (copy-readtable))`
!!!(p) {:.unnumlist}

`(defun scheme-read (&optional (stream *standard-input*))`
!!!(p) {:.unnumlist}

`   (let ((*readtable* *scheme-readtable*))`
!!!(p) {:.unnumlist}

`      (read stream nil eof)))`
!!!(p) {:.unnumlist}

The point of having a special `eof` constant is that it is unforgeable.
The user cannot type in a sequence of characters that will be read as something `eq` to `eof`.
In Common Lisp, but not Scheme, there is an escape mechanism that makes `eof` forgable.
The user can type `#.eof` to get the effect of an end of file.
This is similar to the `^D` convention in UNIX systems, and it can be quite handy.

So far the Scheme readtable is just a copy of the standard readtable.
The next step in implementing `scheme-read` is to alter `*scheme-readtable*`, adding read macros for whatever characters are necessary.
Here we define macros for `#t` and `#f` (the true and false values), for `#d` (decimal numbers) and for the backquote read macro (called quasiquote in Scheme).
Note that the backquote and comma characters are defined as read macros, but the `@` in ,`@` is processed by reading the next character, not by a read macro on `@`.

[ ](#){:#l0190}`(set-dispatch-macro-character #\# #\t`
!!!(p) {:.unnumlist}

`   #’(lambda (&rest ignore) t)`
!!!(p) {:.unnumlist}

`   *scheme-readtable*)`
!!!(p) {:.unnumlist}

`(set-dispatch-macro-character #\# #\f`
!!!(p) {:.unnumlist}

`   #’(lambda (&rest ignore) nil)`
!!!(p) {:.unnumlist}

`   *scheme-readtable*)`
!!!(p) {:.unnumlist}

`(set-dispatch-macro-character #\# #\d`
!!!(p) {:.unnumlist}

`   ;; In both Common Lisp and Scheme,`
!!!(p) {:.unnumlist}

`   ;; #x, #o and #b are hexidecimal, octal, and binary,`
!!!(p) {:.unnumlist}

`   ;; e.g.
#xff - #o377 - #b11111111 - 255`
!!!(p) {:.unnumlist}

`   ;; In Scheme only, #d255 is decimal 255.`
!!!(p) {:.unnumlist}

`   #’(lambda (stream &rest ignore)`
!!!(p) {:.unnumlist}

`          (let ((*read-base* 10)) (scheme-read stream)))`
!!!(p) {:.unnumlist}

`   *scheme-readtable*)`
!!!(p) {:.unnumlist}

`(set-macro-character #\‘`
!!!(p) {:.unnumlist}

`   #’(lambda (s ignore) (list ‘quasiquote (scheme-read s)))`
!!!(p) {:.unnumlist}

`   nil *scheme-readtable*)`
!!!(p) {:.unnumlist}

`(set-macro-character #\,`
!!!(p) {:.unnumlist}

`   #’(lambda (stream ignore)`
!!!(p) {:.unnumlist}

`          (let ((ch (read-char stream)))`
!!!(p) {:.unnumlist}

`             (if (char = ch #\@)`
!!!(p) {:.unnumlist}

`                 (list ’unquote-splicing (read stream))`
!!!(p) {:.unnumlist}

`                 (progn (unread-char ch stream)`
!!!(p) {:.unnumlist}

`                        (list ’unquote (read stream))))))`
!!!(p) {:.unnumlist}

`   nil *scheme-readtable*)`
!!!(p) {:.unnumlist}

Finally, we install `scheme-read` and `eof-object?` as primitives:

[ ](#){:#l0195}`(defparameter *primitive-fns*`
!!!(p) {:.unnumlist}

`   ’((+ 2 + true nil) (- 2 - true nil) (* 2 * true nil) (/ 2 / true nil)`
!!!(p) {:.unnumlist}

`    (< 2 < nil nil) (> 2 > nil nil) (<= 2 <= nil nil) (>= 2 >= nil nil)`
!!!(p) {:.unnumlist}

`    (/= 2 /= nil nil) (= 2 = nil nil)`
!!!(p) {:.unnumlist}

`    (eq?
2 eq nil nil) (equal?
2 equal nil nil) (eqv?
2 eql nil nil)`
!!!(p) {:.unnumlist}

`    (not 1 not nil nil) (null?
1 not nil nil) (cons 2 cons true nil)`
!!!(p) {:.unnumlist}

`    (car 1 car nil nil) (cdr 1 cdr nil nil) (cadr 1 cadr nil nil)`
!!!(p) {:.unnumlist}

`    (list 1 list1 true nil) (list 2 list2 true nil) (list 3 list3 true nil)`
!!!(p) {:.unnumlist}

`    (read 0 read nil t) (write 1 write nil t) (display 1 display nil t)`
!!!(p) {:.unnumlist}

`    (newline 0 newline nil t) (compiler 1 compiler t nil)`
!!!(p) {:.unnumlist}

`    (name!
2 name!
true t) (random 1 random true nil)))`
!!!(p) {:.unnumlist}

Here we test `scheme-read`.
The characters in italics were typed as a response to the `scheme-read`.

[ ](#){:#l0200}`> (scheme-read) #*t*`
!!!(p) {:.unnumlist}

`T`
!!!(p) {:.unnumlist}

`> (scheme-read) #f`
!!!(p) {:.unnumlist}

`NIL`
!!!(p) {:.unnumlist}

`> (scheme-read) *‘(a,b,@cd)*`
!!!(p) {:.unnumlist}

`(QUASIQUOTE (A (UNQUOTE B) (UNQUOTE-SPLICING C) D))`
!!!(p) {:.unnumlist}

The final step is to make quasi quote a macro that expands into the proper sequence of calls to `cons`, `list`, and `append`.
The careful reader will keep track of the difference between the form returned by `scheme-read` (something starting with `quasiquote`), the expansion of this form with the Scheme macro `quasiquote` (which is implemented with the Common Lisp function `quasi-q`), and the eventual evaluation of the expansion.
In an environment where `b` is bound to the number 2 and `c` is bound to the list `(c1 c2)`, we might have:

[ ](#){:#t0125}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| Typed: | `‘(a ,b ,@c d)` |
| Read: | `(quasiquote (a (unquote b) (unquote-splicing c) d))` |
| Expanded: | `(cons ‘a (cons b (append c ‘(d))))` |
| Evaluated: | `(a 2 c1 c2 d)` |

The implementation of the `quasiquote` macro is modeled closely on the one given in Charniak et al.’s *Artificial Intelligence Programming.* I added support for vectors.
In `combine-quasiquote` I add the trick of reusing the old cons cell `x` rather than consing together `left` and `right` when that is possible.
However, the implementation still wastes cons cells—a more efficient version would pass back multiple values rather than consing `quote` onto a list, only to strip it off again.

[ ](#){:#l0205}`(setf (scheme-macro ’quasiquote) ’quasi-q)`
!!!(p) {:.unnumlist}

`(defun quasi-q (x)`
!!!(p) {:.unnumlist}

`   "Expand a quasiquote form into append, list.
and cons calls."`
!!!(p) {:.unnumlist}

`   (cond`
!!!(p) {:.unnumlist}

`      ((vectorp x)`
!!!(p) {:.unnumlist}

`       (list ‘apply ‘vector (quasi-q (coerce x ’list))))`
!!!(p) {:.unnumlist}

`      ((atom x)`
!!!(p) {:.unnumlist}

`       (if (constantp x) x (list ’quote x)))`
!!!(p) {:.unnumlist}

`      ((starts-with x ’unquote)`
!!!(p) {:.unnumlist}

`       (assert (and (rest x) (null (rest2 x))))`
!!!(p) {:.unnumlist}

`       (second x))`
!!!(p) {:.unnumlist}

`      ((starts-with x ’quasiquote)`
!!!(p) {:.unnumlist}

`       (assert (and (rest x) (null (rest2 x))))`
!!!(p) {:.unnumlist}

`       (quasi-q (quasi-q (second x))))`
!!!(p) {:.unnumlist}

`      ((starts-with (first x) ’unquote-splicing)`
!!!(p) {:.unnumlist}

`       (if (null (rest x))`
!!!(p) {:.unnumlist}

`           (second (first x))`
!!!(p) {:.unnumlist}

`           (list ’append (second (first x)) (quasi-q (rest x)))))`
!!!(p) {:.unnumlist}

`       (t (combine-quasiquote (quasi-q (car x))`
!!!(p) {:.unnumlist}

`                              (quasi-q (cdr x))`
!!!(p) {:.unnumlist}

`                              x))))`
!!!(p) {:.unnumlist}

`(defun combine-quasiquote (left right x)`
!!!(p) {:.unnumlist}

`   "Combine left and right (car and cdr), possibly re-using x."`
!!!(p) {:.unnumlist}

`   (cond ((and (constantp left) (constantp right))`
!!!(p) {:.unnumlist}

`              (if (and (eql (eval left) (first x))`
!!!(p) {:.unnumlist}

`                          (eql (eval right) (rest x)))`
!!!(p) {:.unnumlist}

`                     (list ’quote x)`
!!!(p) {:.unnumlist}

`                     (list ’quote (cons (eval left) (eval right)))))`
!!!(p) {:.unnumlist}

`              ((null right) (list ‘list left))`
!!!(p) {:.unnumlist}

`              ((starts-with right ‘list)`
!!!(p) {:.unnumlist}

`              (list* ‘list left (rest right)))`
!!!(p) {:.unnumlist}

`              (t (list ‘cons left right))))`
!!!(p) {:.unnumlist}

Actually, there is a major problem with the `quasiquote` macro, or more accurately, in the entire approach to macro-expansion based on textual substitution.
Suppose we wanted a function that acted like this:

[ ](#){:#l0210}`(extrema ’(3 1 10 5 20 2))`
!!!(p) {:.unnumlist}

`((max 20) (min 1))`
!!!(p) {:.unnumlist}

We could write the Scheme function:

[ ](#){:#l0215}`(define (extrema list)`
!!!(p) {:.unnumlist}

`   ;; Given a list of numbers.
return an a-list`
!!!(p) {:.unnumlist}

`   ;; with max and min values`
!!!(p) {:.unnumlist}

`   ‘((max ,(apply max list)) (min ,(apply min list))))`
!!!(p) {:.unnumlist}

After expansion of the quasiquote, the definition of `extrema` will be:

[ ](#){:#l0220}`(define extrema`
!!!(p) {:.unnumlist}

`   (lambda (list)`
!!!(p) {:.unnumlist}

`     (list (list ’max (apply max list))`
!!!(p) {:.unnumlist}

`           (list ’min (apply min list)))))`
!!!(p) {:.unnumlist}

The problem is that `list` is an argument to the function `extrema`, and the argument shadows the global definition of `list` as a function.
Thus, the function will fail.
One way around this dilemma is to have the macro-expansion use the global value of `list` rather than the symbol `list` itself.
In other words, replace the `’list` in `quasi-q` with (`get-globa1-var ’list`).
Then the expansion can be used even in an environment where `list` is locally bound.
One has to be careful, though: if this tack is taken, then `comp-funcall` should be changed to recognize function constants, and to do the right thing with respect to primitives.

It is problems like these that made the designers of Scheme admit that they don’t know the best way to specify macros, so there is no standard macro definition mechanism in Scheme.
Such problems rarely come up in Common Lisp because functions and variables have different name spaces, and because local function definitions (with `flet` or `labels`) are not widely used.
Those who do define local functions tend not to use already established names like `list` and `append.`

## [ ](#){:#st0035}23.6 History and References
{:#s0035}
{:.h1hd}

Guy Steele’s 1978 MIT master’s thesis on the language Scheme, rewritten as Steele 1983, describes an innovative and influential compiler for Scheme, called RABBIT.
!!!(span) {:.smallcaps} [2](#fn0015){:#xfn0015} A good article on an “industrial-strength” Scheme compiler based on this approach is described in [Kranz et al.’s 1986](B9780080571157500285.xhtml#bb0675) paper on ORBIT, !!!(span) {:.smallcaps} the compiler for the T dialect of Scheme.

Abelson and Sussman’s *Structure and Interpretation of Computer Programs* (1985) contains an excellent chapter on compilation, using slightly different techniques and compiling into a somewhat more confusing machine language.
Another good text is [John Allen’s *Anatomy of Lisp* (1978)](B9780080571157500285.xhtml#bb0040).
It presents a very clear, simple compiler, although it is for an older, dynamically scoped dialect of Lisp and it does not address tail-recursion or `call/cc`.

The peephole optimizer described here is based on the one in [Masinter and Deutsch 1980](B9780080571157500285.xhtml#bb0780).

## [ ](#){:#st0040}23.7 Exercises
{:#s0040}
{:.h1hd}

**Exercise 23.3 [h]** Scheme’s syntax for numbers is slightly different from Common Lisp’s.
In particular, complex numbers are written like `3+4i` rather than `#c(3 4)`.
How could you make `scheme-read` account for this?

**Exercise 23.4 [m]** Is it possible to make the core Scheme language even smaller, by eliminating any of the five special forms `(quote, begin, set!, if, lambda)` and replacing them with macros?

**Exercise 23.5 [m]** Add the ability to recognize internal defines (see [page 779](B9780080571157500224.xhtml#p779)).

**Exercise 23.6 [h]** In `comp-if` we included a special case for `(if t x y)` and `(if nil x y)`.
But there are other cases where we know the value of the predicate.
For example, `(if (*a b) x y)` can also reduce to `x`.
Arrange for these optimizations to be made.
Note the `prim-always` field of the `prim structure` has been provided for this purpose.

**Exercise 23.7 [m]** Consider the following version of the quicksort algorithm for sorting a vector:

[ ](#){:#l0225}`(define (sort-vector vector test)`
!!!(p) {:.unnumlist}

`   (define (sort lo hi)`
!!!(p) {:.unnumlist}

`       (if (>= lo hi)`
!!!(p) {:.unnumlist}

`            vector`
!!!(p) {:.unnumlist}

`            (let ((pivot (partition vector lo hi test)))`
!!!(p) {:.unnumlist}

`                (sort lo pivot)`
!!!(p) {:.unnumlist}

`            (sort (+ pivot 1) hi))))`
!!!(p) {:.unnumlist}

`   (sort 0 (− (vector-length vector 1))))`
!!!(p) {:.unnumlist}

Here the function `partition` takes a vector, two indices into the vector, and a comparison function, `test`.
It modifies the vector and returns an index, `pivot`, such that all elements of the vector below `pivot` are less than all elements at `pivot` or above.

It is well known that quicksort takes time proportional to *n* log *n* to sort a vector of *n* elements, if the pivots are chosen well.
With poor pivot choices, it can take time proportional to *n*2.

The question is, what is the space required by quicksort?
Besides the vector itself, how much additional storage must be temporarily allocated to sort a vector?

Now consider the following modified version of quicksort.
What time and space complexity does it have?

[ ](#){:#l0230}`(define (sort-vector vector test)`
!!!(p) {:.unnumlist}

`   (define (sort lo hi)`
!!!(p) {:.unnumlist}

`     (if (>= lo hi)`
!!!(p) {:.unnumlist}

`         vector`
!!!(p) {:.unnumlist}

`         (let ((pivot (partition vector lo hi)))`
!!!(p) {:.unnumlist}

`            (if (> (- hi pivot) (− pivot lo))`
!!!(p) {:.unnumlist}

`                 (begin (sort lo pivot)`
!!!(p) {:.unnumlist}

`                           (sort (+ pivot 1) hi))`
!!!(p) {:.unnumlist}

`                 (begin (sort (+ pivot 1) hi)`
!!!(p) {:.unnumlist}

`                           (sort lo pivot))))))`
!!!(p) {:.unnumlist}

`   (sort 0 (− (vector-length vector 1))))`
!!!(p) {:.unnumlist}

The next three exercises describe extensions that are not part of the Scheme standard.

**Exercise 23.8 [h]** The set!
special form is defined only when its first argument is a symbol.
Extend `set!` to work like `setf` when the first argument is a list.
That is, `(set!
(car x) y)` should expand into something like `((setter car) y x)`, where `(setter car)` evaluates to the primitive procedure `set-car!`.
You will need to add some new primitive functions, and you should also provide a way for the user to define new `set!` procedures.
One way to do that would be with a `setter` function for `set!`, for example:

[ ](#){:#l0235}`(set!
(setter third)`
!!!(p) {:.unnumlist}

`      (lambda (val list) (set-car!
(cdr (cdr list)) val)))`
!!!(p) {:.unnumlist}

**Exercise 23.9 [m]** It is a curious asymmetry of Scheme that there is a special notation for lambda expressions within `define` expressions, but not within `let`.
Thus, we see the following:

[ ](#){:#l0240}`(define square (lambda (x) (* x x)))`*;is the same as*
!!!(p) {:.unnumlist}

`(define (square x) (* x x))`
!!!(p) {:.unnumlist}

`(let ((square (lambda (x) (* x x)))) …) ;`*is not the same as*
!!!(p) {:.unnumlist}

`(let (((square x) (* x x))) …) ;`*             <= illegal!*
!!!(p) {:.unnumlist}

Do you think this last expression should be legal?
If so, modify the macros for `let, let*`, and `letrec` to allow the new syntax.
If not, explain why it should not be included in the language.

**Exercise 23.10 [m]** Scheme does not define `funcall`, because the normal function-call syntax does the work of funcall.
This suggests two problems.
(1) Is it possible to define `funcall` in Scheme?
Show a definition or explain why there can’t be one.
Would you ever have reason to use `funcall` in a Scheme program?
(2) Scheme does define `apply`, as there is no syntax for an application.
One might want to extend the syntax to make `(+ .
numbers)` equivalent to `(apply + numbers)`.
Would this bea good idea?

**Exercise 23.11 [d]** Write a compiler that translates Scheme to Common Lisp.
This will involve changing the names of some procedures and special forms, figuring out a way to map Scheme’s single name space into Common Lisp’s distinct function and variable name spaces, and dealing with Scheme’s continuations.
One possibility is to translate a `call/cc` into a `catch` and `throw`, and disallow dynamic continuations.

## [ ](#){:#st0045}23.8 Answers
{:#s0045}
{:.h1hd}

**Answer 23.2** We can save frames by making a resource for frames, as was done on page 337.
Unfortunately, we can’t just use the def resource macro as is, because we need a separate resource for each size frame.
Thus, a two-dimensional array or a vector of vectors is necessary.
Furthermore, one must be careful in determining when a frame is no longer needed, and when it has been saved and may be used again.
Some compilers will generate a special calling sequence for a tail-recursive call where the environment can be used as is, without discarding and then creating a new frame for the arguments.
Some compilers have varied and advanced representations for environments.
An environment may never be represented explicitly as a list of frames; instead it may be represented implicitly as a series of values in registers.

**Answer 23.3** We could read in Scheme expressions as before, and then convert any symbols that looked like complex numbers into numbers.
The following routines do this without consing.

[ ](#){:#l0245}`(defun scheme-read (&optional (stream *standard-input*))`
!!!(p) {:.unnumlist}

`   (let ((*readtable* *scheme-readtable*))`
!!!(p) {:.unnumlist}

`     (convert-numbers (read stream nil eof))))`
!!!(p) {:.unnumlist}

`(defun convert-numbers (x)`
!!!(p) {:.unnumlist}

`   "Replace symbols that look like Scheme numbers with their values."`
!!!(p) {:.unnumlist}

`   ;; Don’t copy structure, make changes in place.`
!!!(p) {:.unnumlist}

`   (typecase x`
!!!(p) {:.unnumlist}

`     (cons (setf (car x) (convert-numbers (car x)))`
!!!(p) {:.unnumlist}

`             (setf (cdr x) (convert-numbers (cdr x)))`
!!!(p) {:.unnumlist}

`             x)`
!!!(p) {:.unnumlist}

`     (symbol (or (convert-number x) x))`
!!!(p) {:.unnumlist}

`     (vector (dotimes (i (length x))`
!!!(p) {:.unnumlist}

`                (setf (aref x i) (convert-numbers (aref x i))))`
!!!(p) {:.unnumlist}

`               x)`
!!!(p) {:.unnumlist}

`        (t x)))`
!!!(p) {:.unnumlist}

`(defun convert-number (symbol)`
!!!(p) {:.unnumlist}

`   "If str looks like a complex number, return the number."`
!!!(p) {:.unnumlist}

`   (let* ((str (symbol-name symbol))`
!!!(p) {:.unnumlist}

`            (pos (position-if #’sign-p str))`
!!!(p) {:.unnumlist}

`            (end (− (length str) 1)))`
!!!(p) {:.unnumlist}

`       (when (and pos (char-equal (char str end) #\i))`
!!!(p) {:.unnumlist}

`         (let ((re (read-from-string str nil nil :start 0 :end pos))`
!!!(p) {:.unnumlist}

`                (im (read-from-string str nil nil :start pos rend end)))`
!!!(p) {:.unnumlist}

`            (when (and (numberp re) (numberp im))`
!!!(p) {:.unnumlist}

`              (complex re im))))))`
!!!(p) {:.unnumlist}

`(defun sign-p (char) (find char "+−"))`
!!!(p) {:.unnumlist}

Actually, that’s not quite good enough, because a Scheme complex number can have multiple signs in it, as in `3.
4e- 5+6.
7e+8i`, and it need not have two numbers, as in `3i` or `4+i` or just `+ i`.
The other problem is that complex numbers can only have a lowercase `i`, but read does not distinguish between the symbols `3+4i` and `3+4I`.

**Answer 23.4** Yes, it is possible to implement `begin` as a macro:

[ ](#){:#l0250}`(setf (scheme-macro ’begin)`
!!!(p) {:.unnumlist}

`        #’(lambda (&rest exps) ‘((lambda () .,exps))))`
!!!(p) {:.unnumlist}

With some work we could also eliminate quote.
Instead of `’x`, we could use `(string->symbol "X" )`, and instead of `’(1 2)`, wecoulduse something like `(list 1 2)`.
The problem is in knowing when to reuse the same list.
Consider:

[ ](#){:#l0255}`=> (define (one-two) ’(1 2))`
!!!(p) {:.unnumlist}

`ONE-TWO`
!!!(p) {:.unnumlist}

`=> (eq?
(one-two) (one-two))`
!!!(p) {:.unnumlist}

`T`
!!!(p) {:.unnumlist}

`=> (eq?
’(1 2) ’(1 2))`
!!!(p) {:.unnumlist}

`NIL`
!!!(p) {:.unnumlist}

A clever memoized macro for quote could handle this, but it would be less efficient than having `quote` as a special form.
In short, what’s the point?

It is also (nearly) possible to replace `if` with alternate code.
The idea is to replace:

[ ](#){:#l0260}`(if`*test then-part else-part*)
!!!(p) {:.unnumlist}

with

[ ](#){:#l0265}(*test*`(delay`*then-part*) `(delay`*else-part*))
!!!(p) {:.unnumlist}

Now if we are assured that any *test* returns either `#t` or `#f`, then we can make the following definitions:

[ ](#){:#l0270}`(define #t (lambda (then-part else-part) (force then-part)))`
!!!(p) {:.unnumlist}

`(define #f (lambda (then-part else-part) (force else-part)))`
!!!(p) {:.unnumlist}

The only problem with this is that any value, not just `#t`, counts as true.

This seems to be a common phenomenon in Scheme compilers: translating everything into a few very general constructs, and then recognizing special cases of these constructs and compiling them specially.
This has the disadvantage (compared to explicit use of many special forms) that compilation may be slower, because all macros have to be expanded first, and then special cases have to be recognized.
It has the advantage that the optimizations will be applied even when the user did not have a special construct in mind.
Common Lisp attempts to get the advantages of both by allowing implementations to play loose with what they implement as macros and as special forms.

**Answer 23.6** We define the predicate `always` and install it in two places in `comp-if` :

[ ](#){:#l0275}`(defun always (pred env)`
!!!(p) {:.unnumlist}

`   "Does predicate always evaluate to true or false?"`
!!!(p) {:.unnumlist}

`   (cond ((eq pred t) ‘true)`
!!!(p) {:.unnumlist}

`            ((eq pred nil) ’false)`
!!!(p) {:.unnumlist}

`            ((symbolp pred) nil)`
!!!(p) {:.unnumlist}

`            ((atom pred) ’true)`
!!!(p) {:.unnumlist}

`            ((scheme-macro (first pred))`
!!!(p) {:.unnumlist}

`             (always (scheme-macro-expand pred) env))`
!!!(p) {:.unnumlist}

`            ((case (first pred)`
!!!(p) {:.unnumlist}

`                (QUOTE (if (null (second pred)) ’false ’true))`
!!!(p) {:.unnumlist}

`                (BEGIN (if (null (rest pred)) ’false`
!!!(p) {:.unnumlist}

`                                  (always (last1 pred) env)))`
!!!(p) {:.unnumlist}

`                (SET!
(always (third pred) env))`
!!!(p) {:.unnumlist}

`    (IF (let ((test (always (second pred)) env)`
!!!(p) {:.unnumlist}

`      (then (always (third pred)) env)`
!!!(p) {:.unnumlist}

`      (else (always (fourth pred)) env))`
!!!(p) {:.unnumlist}

`                (cond ((eq test ’true) then)`
!!!(p) {:.unnumlist}

`                                  ((eq test ’false) else)`
!!!(p) {:.unnumlist}

`                                  ((eq then else) then))))`
!!!(p) {:.unnumlist}

`    (LAMBDA ’true)`
!!!(p) {:.unnumlist}

`    (t (let ((prim (primitive-p (first pred) env`
!!!(p) {:.unnumlist}

`                       (length (rest pred)))))`
!!!(p) {:.unnumlist}

`           (if prim (prim-always prim))))))))`
!!!(p) {:.unnumlist}

`(defun comp-if (pred then else env val?
more?)`
!!!(p) {:.unnumlist}

`   (case (always pred env)`
!!!(p) {:.unnumlist}

`     (true ; (if nil x y) = => y ; ***`
!!!(p) {:.unnumlist}

`       (comp then env val?
more?)) ; ***`
!!!(p) {:.unnumlist}

`     (false ; (if t x y) = => x ; ***`
!!!(p) {:.unnumlist}

`       (comp else env val?
more?)) ; ***`
!!!(p) {:.unnumlist}

`     (otherwise`
!!!(p) {:.unnumlist}

`       (let ((pcode (comp pred env t t))`
!!!(p) {:.unnumlist}

`              (tcode (comp then env val?
more?))`
!!!(p) {:.unnumlist}

`              (ecode (comp else env val?
more?)))`
!!!(p) {:.unnumlist}

`       (cond`
!!!(p) {:.unnumlist}

`         ((and (listp pred) ; (if (not p) x y) ==> (if p y x)`
!!!(p) {:.unnumlist}

`                  (length=1 (rest pred))`
!!!(p) {:.unnumlist}

`                  (primitive-p (first pred) env 1)`
!!!(p) {:.unnumlist}

`                  (eq (prim-opcode (primitive-p (first pred) env 1))`
!!!(p) {:.unnumlist}

`                         ’not))`
!!!(p) {:.unnumlist}

`         (comp-if (second pred) else then env val?
more?))`
!!!(p) {:.unnumlist}

`        ((equal tcode ecode) ; (if p x x) ==> (begin p x)`
!!!(p) {:.unnumlist}

`         (seq (comp pred env nil t) ecode))`
!!!(p) {:.unnumlist}

`        ((null tcode) ; (if p nil y) ==> p (TJUMP L2) y L2:`
!!!(p) {:.unnumlist}

`         (let ((L2 (gen-label)))`
!!!(p) {:.unnumlist}

`             (seq pcode (gen ‘TJUMP L2) ecode (list L2)`
!!!(p) {:.unnumlist}

`         (unless more?
(gen ’RETURN)))))`
!!!(p) {:.unnumlist}

`      ((null ecode) ; (if p x) ==> p (FJUMP L1) x L1:`
!!!(p) {:.unnumlist}

`      (let ((L1 (gen-label)))`
!!!(p) {:.unnumlist}

`          (seq pcode (gen TJUMP L1) tcode (list L1)`
!!!(p) {:.unnumlist}

`                 (unless more?
(gen ’RETURN)))))`
!!!(p) {:.unnumlist}

`      (t                               ; (if p x y) ==> p (FJUMP L1) x L1: y`
!!!(p) {:.unnumlist}

`                                       ; or p (FJUMP L1) x (JUMP L2) L1: y L2:`
!!!(p) {:.unnumlist}

`      (let ((L1 (gen-label))`
!!!(p) {:.unnumlist}

`             (L2 (if more?
(gen-label))))`
!!!(p) {:.unnumlist}

`        (seq pcode (gen ‘FJUMP L1) tcode`
!!!(p) {:.unnumlist}

`               (if more?
(gen ’JUMP L2))`
!!!(p) {:.unnumlist}

`               (list L1) ecode (if more?
(list L2))))))))))`
!!!(p) {:.unnumlist}

Development note: originally, I had coded `always` as a predicate that took a Boolean value as input and returned true if the expression always had that value.
Thus, you had to ask first if the predicate was always true, and then if it was always false.
Then I realized this was duplicating much effort, and that the duplication was exponential, not just linear: for a triply-nested conditional I would have to do eight times the work, not twice the work.
Thus I switched to the above formulation, where `always` is a three-valued function, returning `true`, `false`, or `nil` for none-of-the-above.
But to demonstrate that the right solution doesn’t always appear the first time, I give my original definition as well:

[ ](#){:#l0280}`(defun always (boolean pred env)`
!!!(p) {:.unnumlist}

`   "Does predicate always evaluate to boolean in env?"`
!!!(p) {:.unnumlist}

`   (if (atom pred)`
!!!(p) {:.unnumlist}

`     (and (constantp pred) (equiv boolean pred))`
!!!(p) {:.unnumlist}

`     (case (first pred)`
!!!(p) {:.unnumlist}

`        (QUOTE (equiv boolean pred))`
!!!(p) {:.unnumlist}

`        (BEGIN (if (null (rest pred)) (equiv boolean nil)`
!!!(p) {:.unnumlist}

`                          (always boolean (last1 pred) env)))`
!!!(p) {:.unnumlist}

`        (SET!
(always boolean (third pred) env))`
!!!(p) {:.unnumlist}

`        (IF (or (and (always t (second pred) env)`
!!!(p) {:.unnumlist}

`                           (always boolean (third pred) env))`
!!!(p) {:.unnumlist}

`                     (and (always nil (second pred) env)`
!!!(p) {:.unnumlist}

`                           (always boolean (fourth pred) env))`
!!!(p) {:.unnumlist}

`                     (and (always boolean (third pred) env)`
!!!(p) {:.unnumlist}

`                           (always boolean (fourth pred) env))))`
!!!(p) {:.unnumlist}

`        (LAMBDA (equiv boolean t))`
!!!(p) {:.unnumlist}

`        (t (let ((prim (primitive-p (first pred) env`
!!!(p) {:.unnumlist}

`                                             (length (rest pred)))))`
!!!(p) {:.unnumlist}

`            (and prim`
!!!(p) {:.unnumlist}

`                    (eq (prim-always prim)`
!!!(p) {:.unnumlist}

`                          (if boolean ’true ’false))))))))`
!!!(p) {:.unnumlist}

`(defun equiv (x y) "Boolean equivalence" (eq (not x) (not y)))`
!!!(p) {:.unnumlist}

**Answer 23.7** The original version requires *O*(*n*) stack space for poorly chosen pivots.
Assuming a properly tail-recursive compiler, the modified version will never require more than *O*(log *n*) space, because at each step at least half of the vector is being sorted tail-recursively.

**Answer 23.10** (1) `(defun (funcall fn .
args) (apply fn args))` (2) Suppose you changed the piece of code `(+ .
numbers)` to `(+ .
(map sqrt numbers))`.
The latter is the same expression as (+ `map sqrt numbers),` which is not the intended resuit at all.
So there would be an arbitrary restriction: the last argument in an apply form would have to be an atom.
This kind of restriction goes against the grain of Scheme.

----------------------

[1](#xfn0010){:#np0010} Strictly speaking, this is a read-compile-funcall-write loop.
!!!(p) {:.ftnote1}

[2](#xfn0015){:#np0015} At the time, the MacLisp compiler dealt with something called "lisp assembly code" or LAP.
The function to input LAP was called `lapin`.
Those who know French will get the pun.
!!!(p) {:.ftnote1}

