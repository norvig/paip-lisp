# Chapter 10
## Low-Level Efficiency Issues

> There are only two qualities in the world: efficiency and inefficiency; and only two sorts of people: the efficient and the inefficient

> —George Bernard Shaw

> John Bull's Other Island (1904)

The efficiency techniques of the previous chapter all involved fairly significant changes to an algorithm.
But what happens when you already are using the best imaginable algorithms, and performance is still a problem?
One answer is to find what parts of the program are used most frequently and make micro-optimizations to those parts.
This chapter covers the following six optimization techniques.
If your programs all run quickly enough, then feel free to skip this chapter.
But if you would like your programs to run faster, the techniques described here can lead to speed-ups of 40 times or more.

* [ ](#){:#l0010}• Use declarations.

* • Avoid generic functions.

* • Avoid complex argument lists.

* • Provide compiler macros.

* • Avoid unnecessary consing.

* • Use the right data structure.

## [ ](#){:#st0010}10.1 Use Declarations
{:#s0010}
{:.h1hd}

On general-purpose computers running Lisp, much time is spent on type-checking.
You can gain efficiency at the cost of robustness by declaring, or promising, that certain variables will always be of a given type.
For example, consider the following function to compute the sum of the squares of a sequence of numbers:

[ ](#){:#l0015}`(defun sum-squares (seq)`
!!!(p) {:.unnumlist}

` (let ((sum 0))`
!!!(p) {:.unnumlist}

`  (dotimes (i (length seq))`
!!!(p) {:.unnumlist}

`   (incf sum (square (elt seq i))))`
!!!(p) {:.unnumlist}

`  sum))`
!!!(p) {:.unnumlist}

`(defun square (x) (* x x))`
!!!(p) {:.unnumlist}

If this function will only be used to sum vectors of fixnums, we can make it a lot faster by adding declarations:

[ ](#){:#l0020}`(defun sum-squares (vect)`
!!!(p) {:.unnumlist}

` (declare (type (simple-array fixnum *) vect)`
!!!(p) {:.unnumlist}

`    (inline square) (optimize speed (safety 0)))`
!!!(p) {:.unnumlist}

` (let ((sum 0))`
!!!(p) {:.unnumlist}

`  (declare (fixnum sum))`
!!!(p) {:.unnumlist}

`  (dotimes (i (length vect))`
!!!(p) {:.unnumlist}

`   (declare (fixnum i))`
!!!(p) {:.unnumlist}

`   (incf sum (the fixnum (square (svref vect i)))))))`
!!!(p) {:.unnumlist}

`  sum))`
!!!(p) {:.unnumlist}

The fixnum declarations let the compiler use integer arithmetic directly, rather than checking the type of each addend.
The (`the fixnum`… ) special form is a promise that the argument is a fixnum.
The (`optimize speed (safety 0))` declaration tells the compiler to make the function run as fast as possible, at the possible expense of making the code less safe (by ignoring type checks and so on).
Other quantities that can be optimized are `compilation-speed, space` and in ANSI Common Lisp only, `debug` (ease of debugging).
Quantities can be given a number from 0 to 3 indicating how important they are; 3 is most important and is the default if the number is left out.

The (`inline square`) declaration allows the compiler to generate the multiplication specified by `square` right in the loop, without explicitly making a function call to square.
The compiler will create a local variable for (`svref vect i`) and will not execute the reference twice—inline functions do not have any of the problems associated with macros as discussed on [page 853](B9780080571157500248.xhtml#p853).
However, there is one drawback: when you redefine an inline function, you may need to recompile all the functions that call it.

You should declare a function `inline` when it is short and the function-calling overhead will thus be a significant part of the total execution time.
You should not declare a function `inline` when the function is recursive, when its definition is likely to change, or when the function's definition is long and it is called from many places.

In the example at hand, declaring the function inline saves the overhead of a function call.
In some cases, further optimizations are possible.
Consider the predicate `starts-with`:

[ ](#){:#l0025}`(defun starts-with (list x)`
!!!(p) {:.unnumlist}

` "Is this a list whose first element is x?"`
!!!(p) {:.unnumlist}

` (and (consp list) (eql (first list) x)))`
!!!(p) {:.unnumlist}

Suppose we have a code fragment like the following:

[ ](#){:#l0030}`(if (consp list) (starts-with list x) …)`
!!!(p) {:.unnumlist}

If `starts-with` is declared `inline` this will expand to:

[ ](#){:#l0035}`(if (consp list) (and (consp list) (eql (first list) x)) …)`
!!!(p) {:.unnumlist}

which many compilers will simplify to:

[ ](#){:#l0040}`(if (consp list) (eql (first list) x) …)`
!!!(p) {:.unnumlist}

Very few compilers do this kind of simplification across functions without the hint provided by `inline`.

Besides eliminating run-time type checks, declarations also allow the compiler to choose the most efficient representation of data objects.
Many compilers support both *boxed* and *unboxed* representations of data objects.
A boxed representation includes enough information to determine the type of the object.
An unboxed representation is just the "raw bits" that the computer can deal with directly.
Consider the following function, which is used to clear a 1024 × 1024 array of floating point numbers, setting each one to zero:

[ ](#){:#l0045}`(defun clear-m-array (array)`
!!!(p) {:.unnumlist}

` (declare (optimize (speed 3) (safety 0)))`
!!!(p) {:.unnumlist}

` (declare (type (simple-array single-float (1024 1024)) array))`
!!!(p) {:.unnumlist}

` (dotimes (i 1024)`
!!!(p) {:.unnumlist}

`  (dotimes (j 1024)`
!!!(p) {:.unnumlist}

`   (setf (aref array i j) 0.0))))`
!!!(p) {:.unnumlist}

In Allegro Common Lisp on a Sun SPARCstation, this compiles into quite good code, comparable to that produced by the C compiler for an equivalent C program.
If the declarations are omitted, however, the performance is about 40 times worse.

The problem is that without the declarations, it is not safe to store the raw floating point representation of `0.0` in each location of the array.
Instead, the program has to box the `0.0`, allocating storage for a typed pointer to the raw bits.
This is done inside the nested loops, so the result is that each call to the version of `clear-m-array` without declarations calls the floating-point-boxing function 1048567 times, allocating a megaword of storage.
Needless to say, this is to be avoided.

Not all compilers heed all declarations; you should check before wasting time with declarations your compiler may ignore.
The function `disassemble` can be used to show what a function compiles into.
For example, consider the trivial function to add two numbers together.
Here it is with and without declarations:

[ ](#){:#l0050}`(defun f (x y)`
!!!(p) {:.unnumlist}

` (declare (fixnum x y) (optimize (safety 0) (speed 3)))`
!!!(p) {:.unnumlist}

` (the fixnum (+ x y)))`
!!!(p) {:.unnumlist}

`(defun g (x y) (+ x y))`
!!!(p) {:.unnumlist}

Here is the disassembled code for f from Allegro Common Lisp for a Motorola 68000-series processor:

[ ](#){:#t0010}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `> (disassemble 'f)` |
| `;; disassembling #<Function f @ #x83ef79 >` |
| `;; formals: x y` |
| `;; code vector @ #x83ef44` |
| `0:` | `link` | `a6.#0` |
| `4:` | `move.l` | `a2,-(a7)` |
| `6:` | `move.l` | `a5,-(a7)` |
| `8:` | `move.l` | `7(a2),a5` |
| `12:` | `move.l` | `8(a6).d4 ; y` |
| `16:` | `add.l` | `12(a6),d4 ; x` |
| `20:` | `move.l` | `#1,d1` |
| `22:` | `move.l` | `-8(a6),a5` |
| `26:` | `unlk` | `a6` |
| `28:` | `rtd` | `#8` |

![t0010](images/B9780080571157500108/t0010.png)

This may look intimidating at first glance, but you don't have to be an expert at 68000 assembler to gain some appreciation of what is going on here.
The instructions labeled 0–8 (labels are in the leftmost column) comprise the typical function preamble for the 68000.
They do subroutine linkage and store the new function object and constant vector into registers.
Since f uses no constants, instructions 6, 8, and 22 are really unnecessary and could be omitted.
Instructions 0,4, and 26 could also be omitted if you don't care about seeing this function in a stack trace during debugging.
More recent versions of the compiler will omit these instructions.

The heart of function `f` is the two-instruction sequence 12–16.
Instruction 12 retrieves `y`, and 16 adds `y` to `x`, leaving the result in `d4`, which is the "result" register.
Instruction 20 sets `dl`, the "number of values returned" register, to 1.

Contrast this to the code for `g`, which has no declarations and is compiled at default speed and safety settings:

[ ](#){:#t0015}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `> (disassemble 'g)` `;; disassembling #<Function g @ #x83dbd1 >` `;; formals: x y` `;; code vector @ #x83db64` |
| `0:` | `add.l` | `#8,31(a2)` | |
| `4:` | `sub.w` | `#2,dl` | |
| `6:` | `beq.s` | `12` | |
| `8:` | `jmp` | `16(a4)` | `; wnaerr` |
| `12:` | `link` | `a6,#0` | |
| `16:` | `move.l` | `a2,-(a7)` | |
| `18:` | `move.l` | `a5,-(a7)` | |
| `20:` | `move.l` | `7(a2),a5` | |
| `24:` | `tst.b` | `− 208(a4)` | `; signal-hit` |
| `28` | `beq.s` | `34` | |
| `30:` | `jsr` | `872(a4)` | `; process-sig` |
| `34:` | `move.l` | `8(a6),d4` | `; y` |
| `38:` | `move.l` | `12(a6),d0` | `; x` |
| `42:` | `or.l` | `d4,d0` | |
| `44:` | `and.b` | `#7,d0` | |
| `48:` | `bne.s` | `62` | |
| `50:` | `add.l` | `12(a6),d4 ;` | `x` |
| `54:` | `bvc.s` | `76` | |
| `56:` | `jsr` | `696(a4)` | `; add-overflow` |
| `60:` | `bra.s` | `76` | |
| `62:` | `move.l` | `12(a6),-(a7)` | `; x` |
| `66:` | `move.l` | `d4,-(a7)` | |
| `68:` | `move.l` | `#2,d1` | |
| `70:` | `move.l` | `-304(a4),a0` | `; + _2op` |
| `74:` | `jsr` | `(a4)` | |
| `76:` | `move.l` | `#1,d1` | |
| `78:` | `move.l` | `-8(a6),a5` | |
| `82:` | `unlk` | `a6` | |
| `84:` | `rtd` | `#8` | |

![t0015](images/B9780080571157500108/t0015.png)

See how much more work is done.
The first four instructions ensure that the right number of arguments have been passed to `g`.
If not, there is a jump to `wnaerr` (wrong-number-of-arguments-error).
Instructions 12–20 have the argument loading code that was at 0–8 in `f`.
At 24–30 there is a check for asynchronous signals, such as the user hitting the abort key.
After `x` and `y` are loaded, there is a type check (42–48).
If the arguments are not both fixnums, then the code at instructions 62–74 sets up a call to `+ _2op`, which handles type coercion and non-fixnum addition.
If all goes well, we don't have to call this routine, and do the addition at instruction 50 instead.
But even then we are not done—just because the two arguments were fixnums does not mean the result will be.
Instructions 54–56 check and branch to an overflow routine if needed.
Finally, instructions 76–84 return the final value, just as in `f`.

Some low-quality compilers ignore declarations altogether.
Other compilers don't need certain declarations, because they can rely on special instructions in the underlying architecture.
On a Lisp Machine, both `f` and `g` compile into the same code:

[ ](#){:#t0020}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `6 PUSH` | `ARG|0` | `; X` |
| `7 +` | `ARG|1` | `; Y` |
| `8 RETURN` | `PDL-POP` | |

The Lisp Machine has a microcoded + instruction that simultaneously does a fixnum add and checks for non-fixnum arguments, branching to a subroutine if either argument is not a fixnum.
The hardware does the work that the compiler has to do on a conventional processor.
This makes the Lisp Machine compiler simpler, so compiling a function is faster.
However, on modem pipelined computers with instruction caches, there is little or no advantage to microcoding.
The current trend is away from microcode toward reduced instruction set computers (RISC).

On most computers, the following declarations are most likely to be helpful:

* [ ](#){:#l0055}• `fixnum and float`.
Numbers declared as fixnums or floating-point numbers can be handled directly by the host computer's arithmetic instructions.
On some systems, `float` by itself is not enough; you have to say `single-float` or `double-float`.
Other numeric declarations will probably be ignored.
For example, declaring a variable as integer does not help the compiler much, because bignums are integers.
The code to add bignums is too complex to put inline, so the compiler will branch to a general-purpose routine (like `+ _2op` in Allegro), the same routine it would use if no declarations were given.

* • `list and array`.
Many Lisp systems provide separate functions for the list- and array- versions of commonly used sequence functions.
For example, `(delete × (the list 1 ))` compiles into `(sys: delete-list-eql × 1)` on a TI Explorer Lisp Machine.
Another function, `sys:delete-vector`, is used for arrays, and the generic function `delete` is used only when the compiler can't tell what type the sequence is.
So if you know that the argument to a generic function is either a `1ist` or an `array`, then declare it as such.

* • `simple-vector and simple-array`.
Simple vectors and arrays are those that do not share structure with other arrays, do not have fill pointers, and are not adjustable.
In many implementations it is faster to aref a `simple-vector` than a `vector`.
It is certainly much faster than taking an `elt` of a sequence of unknown type.
Declare your arrays to be simple (if they in fact are).

* • `(array *type*)`.
It is often important to specialize the type of array elements.
For example, an `(array short-f1oat)` may take only half the storage of a general array, and such a declaration will usually allow computations to be done using the CPU's native floating-point instructions, rather than converting into and out of Common Lisp's representation of floating points.
This is very important because the conversion normally requires allocating storage, but the direct computation does not.
The specifiers `(simple-array *type*)` and `(vector *type*)` should be used instead of `(array *type*)` when appropriate.
A very common mistake is to declare `(simple-vector *type*)`.
This is an error because Common Lisp expects `(simple-vector *size*)`—don't ask me why.

* • `(array **dimensions*)`.
The full form of an array or `simple-array` type specifier is `(array *type dimensions*)`.
So, for example, `(array bit (* *))` is a two-dimensional bit array, and `(array bit (1024 1024))` is a 1024 × 1024 bit array.
It is very important to specify the number of dimensions when known, and less important to specify the exact size, although with multidimensional arrays, declaring the size is more important.
The format for a vector type specifier is `(vector *type size*)`.

Note that several of these declarations can apply all at once.
For example, in

[ ](#){:#l9060}`(position # \ .
(the simple-string file-name))`
!!!(p) {:.unnumlist}

the variable `filename` has been declared to be a vector, a simple array, and a sequence of type `string-char`.
All three of these declarations are helpful.
The type `simple-string` is an abbreviation for `(simple-array string-char)`.

This guide applies to most Common Lisp systems, but you should look in the implementation notes for your particular system for more advice on how to fine-tune your code.

## [ ](#){:#st0015}10.2 Avoid Generic Functions
{:#s0015}
{:.h1hd}

Common Lisp provides functions with great generality, but someone must pay the price for this generality.
For example, if you write `(elt x 0)`, different machine instruction will be executed depending on if x is a list, string, or vector.
Without declarations, checks will have to be done at runtime.
You can either provide declarations, as in `(elt (the list x) 0)`, or use a more specific function, such as `(first x)` in the case of lists, `(char x 0)` for strings, `(aref x0)` for vectors, and `(svref x 0)` for simple vectors.
Of course, generic functions are useful–I wrote `random-elt` as shown following to work on lists, when I could have written the more efficient `random-mem` instead.
The choice paid off when `I` wanted a function to choose a random character from a string–`random-elt` does the job unchanged, while `random-mem` does not.

[ ](#){:#l0060}`(defun random-elt (s) (elt s (random (length s))))`
!!!(p) {:.unnumlist}

`(defun random-mem (l) (nth (random (length (the list l))) l))`
!!!(p) {:.unnumlist}

This example was simple, but in more complicated cases you can make your sequence functions more efficient by having them explicitly check if their arguments are lists or vectors.
See the definition of `map-into` on [page 857](B9780080571157500248.xhtml#p857).

## [ ](#){:#st0020}10.3 Avoid Complex Argument Lists
{:#s0020}
{:.h1hd}

Functions with keyword arguments suffer a large degree of overhead.
This may also be true for optional and rest arguments, although usually to a lesser degree.
Let's look at some simple examples:

[ ](#){:#l0065}`(defun reg (a b c d) (list a b c d))`
!!!(p) {:.unnumlist}

`(defun rst (abc &rest d) (list* a b c d))`
!!!(p) {:.unnumlist}

`(defun opt (&optional a b (c 1) (d (sqrt a))) (list a b c d))`
!!!(p) {:.unnumlist}

`(defun key (&key a b (c 1) (d (sqrt a))) (list a b c d))`
!!!(p) {:.unnumlist}

We can see what these compile into for the TI Explorer, but remember that your compiler may be quite different.

[ ](#){:#t0025}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `> (disassemble 'reg)` |
| `   8 PUSH` | `ARG|0` | `; A` |
| `   9 PUSH` | `ARG|1` | `; B` |
| `  10 PUSH` | `ARG|2` | `; C` |
| `  11 PUSH` | `ARG|3` | `; D` |
| `  12 TAIL-REC CALL-4` | `FEF|3` | `; #’LIST` |
| `> (disassemble 'rst)` |
| `   8 PUSH` | `ARG|0` | `; A` |
| `   9 PUSH` | `ARG|1` | `; B` |
| `  10 PUSH` | `ARG|2` | `; C` |
| `  11 PUSH` | `LOCAL|0` | `; D` |
| `  12 RETURN CALL-4` | `FEF|3` | `; #’LIST*` |

![t0025](images/B9780080571157500108/t0025.png)

With the regular argument list, we just push the four variables on the argument stack and branch to the list function.
([Chapter 22](B9780080571157500224.xhtml) explains why a tail-recursive call is just a branch statement.)

With a rest argument, things are almost as easy.
It turns out that on this machine, the microcode for the calling sequence automatically handles rest arguments, storing them in local variable 0.
Let's compare with optional arguments:

[ ](#){:#t0030}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `(defun opt (&optional a b` (`c 1) (d (sqrt a))) (list a b c d))` `> (disassemble 'opt)` |
| ` 24 DISPATCH` | `FEF|5` | `; [0`⇒`25;1`⇒`25;2`⇒`25;3`⇒`27;ELSE`⇒`30]` |
| ` 25 PUSH-NUMBER` | `1` | |
| ` 26 POP` | `ARG|2` | ; `C` |
| ` 27 PUSH` | `ARG|0` | ; `A` |
| ` 28 PUSH CALL-1` | `FEF|3` | ; `#'SQRT` |
| ` 29 POP` | `ARG|3` | ; `D` |
| ` 30 PUSH` | `ARG|0` | ; `A` |
| ` 31 PUSH` | `ARG|1` | ; `B` |
| ` 32 PUSH` | `ARG|2` | ; `C` |
| ` 33 PUSH` | `ARG|3` | ; `D` |
| ` 34 TAIL-REC CALL-4` | `FEF|4` | ; `#'LIST` |

![t0030](images/B9780080571157500108/t0030.png)

Although this assembly language may be harder to read, it turns out that optional arguments are handled very efficiently.
The calling sequence stores the number of optional arguments on top of the stack, and the `DISPATCH` instruction uses this to index into a table stored at location `FEF|5` (an offset five words from the start of the function).
The result is that in one instruction the function branches to just the right place to initialize any unspecified arguments.
Thus, a function with optional arguments that are all supplied takes only one more instruction (the dispatch) than the "regular" case.
Unfortunately, keyword arguments don't fare as well:

[ ](#){:#t0035}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `(defun key (&key a b` (`c 1`) `(d (sqrt a))) (list a b c d))` `> (disassemble 'key)` |
| ` 14 PUSH-NUMBER` | `1` | |
| ` 15 POP` | `LOCAL|3` | `; C` |
| ` 16 PUSH` | `FEF|3` | `; SYS:-.KEYWORD-GARBAGE` |
| ` 17 POP` | `LOCAL|4` | |
| ` 18 TEST` | `LOCAL|0` | |
| ` 19 BR-NULL` | `24` | |
| ` 20 PUSH` | `FEF|4` | `; '(:A :B :C :D)` |
| ` 21 SET-NIL` | `PDL-PUSH` | |
| ` 22 PUSH-LOC` | `LOCAL|1` | `; A` |
| ` 23 (AUX) %STORE-KEY-WORD-ARGS` | | |
| ` 24 PUSH` | `LOCAL|1` | `; A` |
| ` 25 PUSH` | `LOCAL|2` | `; B` |
| ` 26 PUSH` | `LOCAL|3` | `; C` |
| ` 27 PUSH` | `|4` | |
| ` 28 EQ` | `FEF|3` | `; SYS::KEYW0RD-GARBAGE` |
| ` 29 BR-NULL` | `33` | |
| ` 30 PUSH` | `LOCAL|1` | `; A` |
| ` 31 PUSH CALL-1` | `FEF|5` | `; #'SQRT` |
| ` 32 RETURN CALL-4` | `FEF|6` | `; #'LIST` |
| ` 33 PUSH` | `LOCAL|4` | |
| ` 34 RETURN CALL-4` | `FEF|6` | ;`#'LIST` |

![t0035](images/B9780080571157500108/t0035.png)

It is not important to be able to read all this assembly language.
The point is that there is considerable overhead, even though this architecture has a specific instruction `(%STORE-KEY-WORD-ARGS)` to help deal with keyword arguments.

Now let's look at the results on another system, the Allegro compiler for the 68000.
First, here's the assembly code for reg, to give you an idea of the minimal calling sequence:[1](#fn0015){:#xfn0015}

[ ](#){:#t0040}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `> (disassemble 'reg)` `;; disassembling #<Function reg @ #x83db59 >` `;; formals: a b c d` `;; code vector @ #x83dblc` |
| `0:` | `link` | `a6,#0` | |
| `4:` | `move.l` | `a2,-(a7)` | |
| `6:` | `move.l` | `a5,-(a7)` | |
| `8:` | `move.l` | `7(a2),a5` | |
| `12:` | `move.l` | `20(a6),-(a7)` | `; a` |
| `16:` | `move.l` | `16(a6).-(a7)` | `; b` |
| `20:` | `move.l` | `12(a6),-(a7)` | `; c` |
| `24:` | `move.l` | `8(a6),-(a7)` | `; d` |
| `28:` | `move.l` | `#4,dl` | |
| `30:` | `jsr` | `848(a4)` | `; list` |
| `34:` | `move.l` | `− 8(a6),a5` | |
| `38:` | `unlk` | `a6` | |
| `40:` | `rtd` | `#10` | |

![t0040](images/B9780080571157500108/t0040.png)

Now we see that `&rest` arguments take a lot more code in this system:

[ ](#){:#t0045}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `> (disassemble 'rst)` `;; disassembling #<Function rst @ #x83de89 >` `;; formals: a b c &rest d` `code vector @ #x83de34` |
| `0:` | `sub.w` | `#3,dl` | |
| `2:` | `bge.s` | `8` | |
| `4:` | `jmp` | `16(a4)` | `; wnaerr` |
| `8:` | `move.l` | `(a7)+,al` | |
| `10`: | `move.l` | `d3,-(a7)` | `; nil` |
| `12`: | `sub.w` | `#l,dl` | |
| `14:` | `bit.s` | `38` | |
| `16:` | `move.l` | `al, − 52(a4)` | `; c_protected-retaddr` |
| `20:` | `jsr` | `40(a4)` | `; cons` |
| `24:` | `move.l` | `d4,-(a7)` | |
| `26:` | `dbra` | `dl,20` | |
| `30:` | `move.l` | `− 52(a4),al` | `; c_protected-retaddr` |
| `34:` | `clr.l` | `− 52(a4)` | `; c_protected-retaddr` |
| `38:` | `move.l` | `al,` | `−(a7)` |
| `40:` | `link` | `a6,#0` | |
| `44:` | `move.l` | `a2,-(a7)` | |
| `46:` | `move.l` | `a5,-(a7)` | |
| `48:` | `move.l` | `7(a2),a5` | |
| `52:` | `move.l` | `− 332(a4),a0` | `; list*` |
| `56:` | `move.l` | `− 8(a6),a5` | |
| `60:` | `unlk` | `a6` | |
| `62`: | `move.l` | `#4,dl` | |
| `64` | `jmp` | `(a4)` | |

![t0045](images/B9780080571157500108/t0045.png)

The loop from 20–26 builds up the `&rest` list one cons at a time.
Part of the difficulty is that cons could initiate a garbage collection at any time, so the list has to be built in a place that the garbage collector will know about.
The function with optional arguments is even worse, taking 34 instructions (104 bytes), and keywords are worst of all, weighing in at 71 instructions (178 bytes), and including a loop.
The overhead for optional arguments is proportional to the number of optional arguments, while for keywords it is proportional to the product of the number of parameters allowed and the number of arguments actually supplied.

A good guideline to follow is to use keyword arguments primarily as an interface to infrequently used functions, and to provide versions of these functions without keywords that can be used in places where efficiency is important.
Consider:

[ ](#){:#l0070}`(proclaim '(inline key))`
!!!(p) {:.unnumlist}

`(defun key (&key a b (c 1) (d (sqrt a))) (*no-key a b c d))`
!!!(p) {:.unnumlist}

`(defun *no-key (a b c d) (list a b c d))`
!!!(p) {:.unnumlist}

Here the function `key` is used as an interface to the function `no-key`, which does the real work.
The inline proclamation should allow the compiler to compile a call to key as a call to `no-key` with the appropriate arguments:

[ ](#){:#t0050}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `> (disassemble #'(lambda (x y) (key :b x :a y)))` |
| ` 10 PUSH` | `ARG|1` | `; Y` |
| ` 11 PUSH` | `ARG|0` | `; X` |
| ` 12 PUSH-NUMBER` | `1` | |
| ` 13 PUSH` | `ARG|1` | `; Y` |
| ` 14 PUSH CALL-1` | `FEF|3` | `; #’SORT` |
| ` 15 TAIL-REC CALL-4` | `FEF|4` | `; #’NO-KEY` |

![t0050](images/B9780080571157500108/t0050.png)

The overhead only comes into play when the keywords are not known at compile time.
In the following example, the compiler is forced to call key, not `no-key`, because it doesn't know what the keyword `k` will be at run time:

[ ](#){:#t0055}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `> (disassemble #'(lambda (k x y) (key k x :a y)))` |
| ` 10 PUSH` | `ARG|0` | ; `K` |
| ` 11 PUSH` | `ARG|1` | ; `X` |
| ` 12 PUSH` | `FEF|3` | `; ':A` |
| ` 13 PUSH` | `ARG|2` | ; `Y` |
| ` 14 TAIL-REC CALL-4` | `FEF|4` | ; `#'KEY` |

![t0055](images/B9780080571157500108/t0055.png)

Of course, in this simple example I could have replaced `no-key` with `list`, but in general there will be some more complex processing.
If I had proclaimed `no-key` inline as well, then I would get the following:

[ ](#){:#t0060}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `> (disassemble #'(lambda (x y) (key :b x :a y)))` |
| ` 10 PUSH` | `ARG|1` | `; Y` |
| ` 11 PUSH` | `ARG|0` | `; X` |
| ` 12 PUSH-NUMBER` | `1` | |
| ` 13 PUSH` | `ARG|1` | `; Y` |
| ` 14 PUSH CALL-1` | `FEF|3` | `; #’SORT` |
| ` 15 TAIL-REC CALL-4` | `FEF|4` | `; #’LIST` |

![t0060](images/B9780080571157500108/t0060.png)

If you like, you can define a macro to automatically define the interface to the keyword-less function:

[ ](#){:#l0075}`(defmacro defun* (fn-name arg-list &rest body)`
!!!(p) {:.unnumlist}

` "Define two functions.
one an interface to a &keyword-less`
!!!(p) {:.unnumlist}

` version.
Proclaim the interface function inline."`
!!!(p) {:.unnumlist}

` (if (and (member '&key arg-list)`
!!!(p) {:.unnumlist}

`    (not (member '&rest arg-list)))`
!!!(p) {:.unnumlist}

`   (let ((no-key-fn-name (symbol fn-name '*no-key))`
!!!(p) {:.unnumlist}

`    (args (mapcar #'first-or-self`
!!!(p) {:.unnumlist}

`       (set-difference`
!!!(p) {:.unnumlist}

`        arg-list`
!!!(p) {:.unnumlist}

`        1ambda-list-keywords))))`
!!!(p) {:.unnumlist}

`   '(progn`
!!!(p) {:.unnumlist}

`    (proclaim '(inline ,fn-name))`
!!!(p) {:.unnumlist}

`    (defun ,no-key-fn-name ,args`
!!!(p) {:.unnumlist}

`     .,body)`
!!!(p) {:.unnumlist}

`    (defun ,fn-name ,arg-list`
!!!(p) {:.unnumlist}

`     (,no-key-fn-name .,args))))`
!!!(p) {:.unnumlist}

`  '(defun ,fn-name ,arg-list`
!!!(p) {:.unnumlist}

`   .,body)))`
!!!(p) {:.unnumlist}

`>(macroexpand '(defun* key (&key a b (c 1) (d (sqrt a)))`
!!!(p) {:.unnumlist}

`      (list a b c d)))`
!!!(p) {:.unnumlist}

`(PROGN (PROCLAIM '(INLINE KEY))`
!!!(p) {:.unnumlist}

`  (DEFUN KEY*NO-KEY (A B C D) (LIST A B C D))`
!!!(p) {:.unnumlist}

`  (DEFUN KEY (&KEY A B (C 1) (D (SQRT A)))`
!!!(p) {:.unnumlist}

`   (KEY*NO-KEY A B C D)))`
!!!(p) {:.unnumlist}

`>(macroexpand '(defun* reg (a b c d) (list a b c d)))`
!!!(p) {:.unnumlist}

`(DEFUN REG (A B C D) (LIST A B C D))`
!!!(p) {:.unnumlist}

There is one disadvantage to this approach: a user who wants to declare key inline or not inline does not get the expected result.
The user has to know that key is implemented with `key*no- key`, and declare `key*no- key` inline.

An alternative is just to proclaim the function that uses `&key` to be inline.
Rob MacLachlan provides an example.
In CMU Lisp, the function `member` has the following definition, which is proclaimed inline:

[ ](#){:#l0080}`(defun member (item list &key (key #'identity)`
!!!(p) {:.unnumlist}

`        (test #'eql testp)(test-not nil notp))`
!!!(p) {:.unnumlist}

` (do ((list list (cdr list)))`
!!!(p) {:.unnumlist}

`   ((null list) nil)`
!!!(p) {:.unnumlist}

`  (let ((car (car list)))`
!!!(p) {:.unnumlist}

`   (if (cond`
!!!(p) {:.unnumlist}

`    (testp`
!!!(p) {:.unnumlist}

`     (funcall test item`
!!!(p) {:.unnumlist}

`        (funcall key car)))`
!!!(p) {:.unnumlist}

`    (notp`
!!!(p) {:.unnumlist}

`     (not`
!!!(p) {:.unnumlist}

`   (funcall test-not item`
!!!(p) {:.unnumlist}

`      (funcall key car))))`
!!!(p) {:.unnumlist}

`  (t`
!!!(p) {:.unnumlist}

`   (funcall test item`
!!!(p) {:.unnumlist}

`      (funcall key car))))`
!!!(p) {:.unnumlist}

` (return list)))))`
!!!(p) {:.unnumlist}

A call like `(member`[ch 1](B9780080571157500017.xhtml)`:key #'first-letter :test #'char =)` expands into the equivalent of the following code.
Unfortunately, not all compilers are this clever with inline declarations.

[ ](#){:#l0085}`(do ((list list (cdr list)))`
!!!(p) {:.unnumlist}

`   ((null list) nil)`
!!!(p) {:.unnumlist}

`  (let ((car (car list)))`
!!!(p) {:.unnumlist}

`   (if (char = ch (first-letter car))`
!!!(p) {:.unnumlist}

`    (return list))))`
!!!(p) {:.unnumlist}

This chapter is concerned with efficiency and so has taken a stand against the use of keyword parameter s in frequently used functions.
But when maintainability is considered, keyword parameters look much better.
When a program is being developed, and it is not clear if a function will eventually need additional arguments, keyword parameters may be the best choice.

## [ ](#){:#st0025}10.4 Avoid Unnecessary Consing
{:#s0025}
{:.h1hd}

The `cons` function may appear to execute quite quickly, but like all functions that allocate new storage, it has a hidden cost.
When large amounts of storage are used, eventually the system must spend time garbage collecting.
We have not mentioned it earlier, but there are actually two relevant measures of the amount of space consumed by a program: the amount of storage allocated, and the amount of storage retained.
The difference is storage that is used temporarily but eventually freed.
Lisp guarantees that unused space will eventually be reclaimed by the garbage collector.
This happens automatically—the programmer need not and indeed can not explicitly free storage.
The problem is that the efficiency of garbage collection can vary widely.
Garbage collection is particularly worrisome for real-time systems, because it can happen at any time.

The antidote to garbage woes is to avoid unnecessary copying of objects in often-used code.
Try using destructive operations, like `nreverse, delete`, and `nconc`, rather than their nondestructive counterparts, (like reverse, remove, and append) whenever it is safe to do so.
Or use vectors instead of lists, and reuse values rather than creating copies.
As usual, this gain in efficiency may lead to errors that can be difficult to debug.
However, the most common kind of unnecessary copying can be eliminated by simple reorganization of your code.
Consider the following version of `flatten`, which returns a list of all the atoms in its input, preserving order.
Unlike the version in [chapter 5](B9780080571157500054.xhtml), this version returns a single list of atoms, with no embedded lists.

[ ](#){:#l0090}`(defun flatten (input)`
!!!(p) {:.unnumlist}

` "Return a flat list of the atoms in the input.`
!!!(p) {:.unnumlist}

` Ex: (flatten '((a) (b (c) d))) => (a b c d)."`
!!!(p) {:.unnumlist}

` (cond ((null input) nil)`
!!!(p) {:.unnumlist}

`   ((atom input) (list input))`
!!!(p) {:.unnumlist}

`   (t (append (flatten (first input))`
!!!(p) {:.unnumlist}

`      (flatten (rest input))))))`
!!!(p) {:.unnumlist}

This definition is quite simple, and it is easy to see that it is correct.
However, each call to `append` requires copying the first argument, so this version can cons *O*(*n*2) cells on an input with *n* atoms.
The problem with this approach is that it computes the list of atoms in the `first` and `rest` of each subcomponent of the input.
But the `first` sublist by itself is not part of the final answer—that's why we have to call `append.` We could avoid generating garbage by replacing `append` with `nconc,` but even then we would still be wasting time, because `nconc` would have to scan through each sublist to find its end.

The version below makes use of an *accumulator* to keep track of the atoms that have been collected in the rest, and to add the atoms in the `first` one at a time with cons, rather than building up unnecessary sublists and appending them.
This way no garbage is generated, and no subcomponent is traversed more than once.

[ ](#){:#l0095}`(defun flatten (input &optional accumulator)`
!!!(p) {:.unnumlist}

` "Return a flat list of the atoms in the input.`
!!!(p) {:.unnumlist}

` Ex: (flatten '((a) (b (c) d))) => (a b c d)."`
!!!(p) {:.unnumlist}

` (cond ((null input) accumulator)`
!!!(p) {:.unnumlist}

`   ((atom input) (cons input accumulator))`
!!!(p) {:.unnumlist}

`   (t (flatten (first input)`
!!!(p) {:.unnumlist}

`      (flatten (rest input) accumulator)))))`
!!!(p) {:.unnumlist}

The version with the accumulator may be a little harder to understand, but it is far more efficient than the original version.
Experienced Lisp programmers become quite skilled at replacing calls to `append` with accumulators.

Some of the early Lisp machines had unreliable garbage-collection, so users just turned garbage collection off, used the machine for a few days, and rebooted when they ran out of space.
With a large virtual memory system this is a feasible approach, because virtual memory is a cheap resource.
The problem is that real memory is still an expensive resource.
When each page contains mostly garbage and only a little live data, the system will spend a lot of time paging data in and out.
Compacting garbage-collection algorithms can relocate live data, packing it into a minimum number of pages.

Some garbage-collection algorithms have been optimized to deal particularly well with just this case.
If your system has an *ephemeral* or *generational* garbage collector, you need not be so concerned with short-lived objects.
Instead, it will be the medium-aged objects that cause problems.
The other problem with such systems arises when an object in an old generation is changed to point to an object in a newer generation.
This is to be avoided, and it may be that reverse is actually faster than nreverse in such cases.
To decide what works best on your particular system, design some test cases and time them.

As an example of efficient use of storage, here is a version of `pat-match` that eliminates (almost) all consing.
The original version of `pat-match,` as used in ELIZA !!!(span) {:.smallcaps} ([page 180](B9780080571157500066.xhtml#p180)), used an association list of variable/value pairs to represent the binding list.
This version uses two sequences: a sequence of variables and a sequence of values.
The sequences are implemented as vectors instead of lists.
In general, vectors take half as much space as lists to store the same information, since half of every list is just pointing to the next element.

In this case, the savings are much more substantial than just half.
Instead of building up small binding lists for each partial match and adding to them when the match is extended, we will allocate a sufficiently large vector of variables and values just once, and use them over and over for each partial match, and even for each invocation of `pat-match.` To do this, we need to know how many variables we are currently using.
We could initialize a counter variable to zero and increment it each time we found a new variable in the pattern.
The only difficulty would be when the counter variable exceeds the size of the vector.
We could just give up and print an error message, but there are more user-friendly alternatives.
For example, we could allocate a larger vector for the variables, copy over the existing ones, and then add in the new one.

It turns out that Common Lisp has a built-in facility to do just this.
When a vector is created, it can be given a *fill pointer*.
This is a counter variable, but one that is conceptually stored inside the vector.
Vectors with fill pointers act like a cross between a vector and a stack.
You can push new elements onto the stack with the functions `vector - push` or `vector - push - extend`.
The latter will automatically allocate a larger vector and copy over elements if necessary.
You can remove elements with `vector - pop`, or you can explicitly look at the fill pointer with `fi1l - pointer`, or change it with a `setf`.
Here are some examples (with `*print-array*` set to t so we can see the results):

[ ](#){:#l0100}`> (setf a (make-array 5 :fi11-pointer 0))`⇒ `#()`
!!!(p) {:.unnumlist}

`> (vector-push 1 a)`⇒ `0`
!!!(p) {:.unnumlist}

`> (vector-push 2 a)`⇒ `1`
!!!(p) {:.unnumlist}

`> a`⇒ `#(1 2)`
!!!(p) {:.unnumlist}

`> (vector-pop a)`⇒ `2`
!!!(p) {:.unnumlist}

`> a`⇒ `#(1)`
!!!(p) {:.unnumlist}

`> (dotimes (i 10) (vector-push-extend 'x a))`⇒ `NIL`
!!!(p) {:.unnumlist}

`> a`⇒ `#(1 XXXXXXXXXX)`
!!!(p) {:.unnumlist}

`> (fill- pointer a)`⇒ `11`
!!!(p) {:.unnumlist}

`> (setf (fill-pointer a) 1)`⇒ `1`
!!!(p) {:.unnumlist}

`> a`⇒ `#(1)`
!!!(p) {:.unnumlist}

`> (find 'x a)`⇒ `NIL NIL ;`*FIND can't find past the fill pointer*
!!!(p) {:.unnumlist}

`> (aref a 2)`⇒ `X` ; *But AREF can see beyond the fill pointer*
!!!(p) {:.unnumlist}

Using vectors with fill pointers in `pat-match,` the total storage for binding lists is just twice the number of variables in the largest pattern.
I have arbitrarily picked 10 as the maximum number of variables, but even this is not a hard limit, because `vector-push-extend` can increase it.
In any case, the total storage is small, fixed in size, and amortized over all calls to `pat-match.` These are just the features that indicate a responsible use of storage.

However, there is a grave danger with this approach: the value returned must be managed carefully.
The new `pat-match` returns the value of `success` when it matches.
`success` is bound to a cons of the variable and value vectors.
These can be freely manipulated by the calling routine, but only up until the next call to `pat - match.` At that time, the contents of the two vectors can change.
Therefore, if any calling function needs to hang on to the returned value after another call to `pat-match,` it should make a copy of the returned value.
So it is not quite right to say that this version of `pat-match` eliminates all consing.
It will cons when `vector-push-extend` runs out of space, or when the user needs to make a copy of a returned value.

Here is the new definition of `pat-match.` It is implemented by closing the definition of `pat-match` and its two auxilliary functions inside a `let` that establishes the bindings of `vars, vals`, and `success`, but that is not crucial.
Those three variables could have been implemented as global variables instead.
Note that it does not support segment variables, or any of the other options implemented in the `pat-match` of [chapter 6](B9780080571157500066.xhtml).

[ ](#){:#l0105}`(let* ((vars (make-array 10 :fill-pointer 0 :adjustable t))`
!!!(p) {:.unnumlist}

`   (vals (make-array 10 :fill-pointer 0 :adjustable t))`
!!!(p) {:.unnumlist}

`   (success (cons vars vals)))`
!!!(p) {:.unnumlist}

`(defun efficient-pat-match (pattern input)`
!!!(p) {:.unnumlist}

` "Match pattern against input."`
!!!(p) {:.unnumlist}

` (setf (fill-pointer vars) 0)`
!!!(p) {:.unnumlist}

` (setf (fill-pointer vals) 0)`
!!!(p) {:.unnumlist}

` (pat-match-1 pattern input))`
!!!(p) {:.unnumlist}

`(defun pat-match-1 (pattern input)`
!!!(p) {:.unnumlist}

` (cond ((variable-p pattern) (match-var pattern input))`
!!!(p) {:.unnumlist}

`   ((eql pattern input) success)`
!!!(p) {:.unnumlist}

`   ((and (consp pattern) (consp input))`
!!!(p) {:.unnumlist}

`    (and (pat-match-1 (first pattern) (first input))`
!!!(p) {:.unnumlist}

`      (pat-match-1 (rest pattern) (rest input))))`
!!!(p) {:.unnumlist}

`   (t fail)))`
!!!(p) {:.unnumlist}

`(defun match-var (var input)`
!!!(p) {:.unnumlist}

` "Match a single variable against input."`
!!!(p) {:.unnumlist}

` (let ((i (position var vars)))`
!!!(p) {:.unnumlist}

`  (cond ((null i)`
!!!(p) {:.unnumlist}

`     (vector-push-extend var vars)`
!!!(p) {:.unnumlist}

`     (vector-push-extend input vals) success)`
!!!(p) {:.unnumlist}

`   ((equal input (aref vals i)) success)`
!!!(p) {:.unnumlist}

`   (t fail)))))`
!!!(p) {:.unnumlist}

An example of its use:

[ ](#){:#l0110}`>(efficient-pat-match '(?x + ?x = ?y .
?z)`
!!!(p) {:.unnumlist}

`        '(2 + 2 = (3 + 1) is true))`
!!!(p) {:.unnumlist}

`(#(?X ?Y ?Z) .
#(2 (3 + 1) (IS TRUE)))`
!!!(p) {:.unnumlist}

Extensible vectors with fill pointers are convenient, and much more efficient than consing up lists.
However, there is some overhead involved in using them, and for those sections of code that must be most efficient, it is best to stick with simple vectors.
The following version of `efficient-pat-match` explicitly manages the size of the vectors and explicitly replaces them with new ones when the size is exceeded:

[ ](#){:#l0120}`(let* ((current-size 0)`
!!!(p) {:.unnumlist}

`   (max-size 1)`
!!!(p) {:.unnumlist}

`   (vars (make-array max-size))`
!!!(p) {:.unnumlist}

`   (vals (make-array max-size))`
!!!(p) {:.unnumlist}

`   (success (cons vars vals)))`
!!!(p) {:.unnumlist}

` (declare (simple-vector vars vals)`
!!!(p) {:.unnumlist}

`     (fixnum current-size max-size))`
!!!(p) {:.unnumlist}

`(defun efficient-pat-match (pattern input)`
!!!(p) {:.unnumlist}

` "Match pattern against input."`
!!!(p) {:.unnumlist}

` (setf current-size 0)`
!!!(p) {:.unnumlist}

` (pat-match-1 pattern input))`
!!!(p) {:.unnumlist}

`;; pat-match-1 is unchanged`
!!!(p) {:.unnumlist}

`(defun match-var (var input)`
!!!(p) {:.unnumlist}

` "Match a single variable against input."`
!!!(p) {:.unnumlist}

` (let ((i (position var vars)))`
!!!(p) {:.unnumlist}

`  (cond`
!!!(p) {:.unnumlist}

`   ((null i)`
!!!(p) {:.unnumlist}

`    (when (= current-size max-size)`
!!!(p) {:.unnumlist}

`     ;; Make new vectors when we run out of space`
!!!(p) {:.unnumlist}

`     (setf max-size (* 2 max-size)`
!!!(p) {:.unnumlist}

`       vars (replace (make-array max-size) vars)`
!!!(p) {:.unnumlist}

`       vals (replace (make-array max-size) vals)`
!!!(p) {:.unnumlist}

`       success (cons vars vals)))`
!!!(p) {:.unnumlist}

`    ;; Store var and its value in vectors`
!!!(p) {:.unnumlist}

`    (setf (aref vars current-size) var)`
!!!(p) {:.unnumlist}

`    (setf (aref vals current-size) input)`
!!!(p) {:.unnumlist}

`    (incf current-size)``    success)`
!!!(p) {:.unnumlist}

`   ((equal input (aref vals i)) success)`
!!!(p) {:.unnumlist}

`   (t fail)))))`
!!!(p) {:.unnumlist}

In conclusion, replacing lists with vectors can often save garbage.
But when you must use lists, it pays to use a version of cons that avoids consing when possible.
The following is such a version:

[ ](#){:#l0125}`(proclaim '(inline reuse-cons))`
!!!(p) {:.unnumlist}

`(defun reuse-cons (x y x-y)`
!!!(p) {:.unnumlist}

` "Return (cons x y), or just x-y if it is equal to (cons x y)."`
!!!(p) {:.unnumlist}

` (if (and (eql x (car x-y)) (eql y (cdr x-y)))`
!!!(p) {:.unnumlist}

`   x-y`
!!!(p) {:.unnumlist}

`   (cons x y)))`
!!!(p) {:.unnumlist}

The trick is based on the definition of subst in Steele's *Common Lisp the Language*.
Here is a definition for a version of `remove` that uses `reuse-cons`:

[ ](#){:#l0130}`(defun remq (item list)`
!!!(p) {:.unnumlist}

` "Like REMOVE, but uses EQ, and only works on lists."`
!!!(p) {:.unnumlist}

` (cond ((null list) nil )`
!!!(p) {:.unnumlist}

`   ((eq item (first list)) (remq item (rest list)))`
!!!(p) {:.unnumlist}

`   (t (reuse-cons (first list)`
!!!(p) {:.unnumlist}

`        (remq item (rest list))`
!!!(p) {:.unnumlist}

`        list))))`
!!!(p) {:.unnumlist}

### [ ](#){:#st9000}Avoid Consing: Unique Lists
{:#s9000}
{:.h2hd}

Of course, `reuse-cons` only works when you have candidate cons cells around.
That is, (`reuse-cons a b c`) only saves space when `c` is (or might be) equal to (`cons a b`).
For some applications, it is useful to have a version of `cons` that returns a unique cons cell without needing `c` as a hint.
We will call this version `ucons` for "unique cons." `ucons` maintains a double hash table: `*uniq - cons - table*` is a hash table whose keys are the `cars` of cons cells.
The value for each `car` is another hash table whose keys are the `cdrs` of cons cells.
The value of each `cdr` in this second table is the original cons cell.
So two different cons cells with the same `car` and `cdr` will retrieve the same value.
Here is an implementation of `ucons`:

[ ](#){:#l0135}`(defvar *uniq-cons-table* (make-hash-table :test #'eq))`
!!!(p) {:.unnumlist}

`(defun ucons (x y)`
!!!(p) {:.unnumlist}

` "Return a cons s.t.
(eq (ucons x y) (ucons x y)) is true."`
!!!(p) {:.unnumlist}

` (let ((car-table (or (gethash x *uniq-cons-table*)`
!!!(p) {:.unnumlist}

`        (setf (gethash x *uniq-cons-table*)`
!!!(p) {:.unnumlist}

`          (make-hash-table :test #'eq)))))`
!!!(p) {:.unnumlist}

`  (or (gethash y car-table)`
!!!(p) {:.unnumlist}

`    (setf (gethash y car-table) (cons x y)))))`
!!!(p) {:.unnumlist}

`ucons`, unlike `cons`, is a true function: it will always return the same value, given the same arguments, where "same" is measured by eq.
However, if `ucons` is given arguments that are equal but not eq, it will not return a unique result.
For that we need the function unique.
It has the property that `(unique x)` is eq to `(unique y)` whenever `x` and `y` are equal.
`unique` uses a hash table for atoms in addition to the double hash table for conses.
This is necessary because strings and arrays can be equal without being eq.
Besides `unique`, we also define `ulist` and uappend for convenience.

[ ](#){:#l0140}`(defvar *uniq-atom-table* (make-hash-table :test #'equal))`
!!!(p) {:.unnumlist}

` (defun unique (exp)`
!!!(p) {:.unnumlist}

`  "Return a canonical representation that is EQUAL to exp,`
!!!(p) {:.unnumlist}

`  such that (equal x y) implies (eq (unique x) (unique y))."`
!!!(p) {:.unnumlist}

`  (typecase exp`
!!!(p) {:.unnumlist}

`   (symbol exp)`
!!!(p) {:.unnumlist}

`   (fixnum exp) ;; Remove if fixnums are not eq in your Lisp`
!!!(p) {:.unnumlist}

`   (atom (or (gethash exp *uniq-atom-table*)`
!!!(p) {:.unnumlist}

`        (setf (gethash exp *uniq-atom-table*) exp)))`
!!!(p) {:.unnumlist}

`   (cons (unique-cons (car exp) (cdr exp)))))`
!!!(p) {:.unnumlist}

` (defun unique-cons (x y)`
!!!(p) {:.unnumlist}

`  "Return a cons s.t.
(eq (ucons x y) (ucons x2 y2)) is true`
!!!(p) {:.unnumlist}

`  whenever (equal x x2) and (equal y y2) are true."`
!!!(p) {:.unnumlist}

`  (ucons (unique x) (unique y)))`
!!!(p) {:.unnumlist}

` (defun ulist (&rest args)`
!!!(p) {:.unnumlist}

`  "A uni qui fied list."`
!!!(p) {:.unnumlist}

`  (unique args))`
!!!(p) {:.unnumlist}

` (defun uappend (x y)`
!!!(p) {:.unnumlist}

`  "A unique list equal to (append x y)."`
!!!(p) {:.unnumlist}

`  (if (null x)`
!!!(p) {:.unnumlist}

`    (unique y)`
!!!(p) {:.unnumlist}

`    (ucons (first x) (uappend (rest x) y))))`
!!!(p) {:.unnumlist}

The above code works, but it can be improved.
The problem is that when `unique` is applied to a tree, it always traverses the tree all the way to the leaves.
The function `unique-cons` is like `ucons,` except that `unique-cons` assumes its arguments are not yet unique.
We can modify `unique - cons` so that it first checks to see if its arguments are unique, by looking in the appropriate hash tables:

[ ](#){:#l0145}`(defun unique-cons (x y)`
!!!(p) {:.unnumlist}

` "Return a cons s.t.
(eq (ucons x y) (ucons x2 y2)) is true`
!!!(p) {:.unnumlist}

` whenever (equal x x2) and (equal y y2) are true."`
!!!(p) {:.unnumlist}

` (let ((ux) (uy)) ; unique x and y`
!!!(p) {:.unnumlist}

`  (let ((car-table`
!!!(p) {:.unnumlist}

`     (or (gethash x *uniq-cons-table*)`
!!!(p) {:.unnumlist}

`      (gethash (setf ux (unique x)) *uniq-cons-table*)`
!!!(p) {:.unnumlist}

`      (setf (gethash ux *uniq-cons-table*)`
!!!(p) {:.unnumlist}

`        (make-hash-table :test #'eq)))))`
!!!(p) {:.unnumlist}

`   (or (gethash y car-table)`
!!!(p) {:.unnumlist}

`    (gethash (setf uy (unique y)) car-table)`
!!!(p) {:.unnumlist}

`    (setf (gethash uy car-table)`
!!!(p) {:.unnumlist}

`      (cons ux uy))))))`
!!!(p) {:.unnumlist}

Another advantage of `unique` is that it can help in indexing.
If lists are unique, then they can be stored in an `eq` hash table instead of a equal hash table.
This can lead to significant savings when the list structures are large.
An `eq` hash table for lists is almost as good as a property list on symbols.

### [ ](#){:#st9005}Avoid Consing: Multiple Values
{:#s9005}
{:.h2hd}

Parameters and multiple values can also be used to pass around values, rather than building up lists.
For example, instead of :

[ ](#){:#l0150}`(defstruct point "A point in 3-D cartesian space." x y z)`
!!!(p) {:.unnumlist}

`(defun scale-point (k pt)`
!!!(p) {:.unnumlist}

` "Multiply a point by a constant, K."`
!!!(p) {:.unnumlist}

` (make-point :x (* k (point-x pt))`
!!!(p) {:.unnumlist}

`         :y (* k (point-y pt))`
!!!(p) {:.unnumlist}

`         :z (* k (point-z pt))))`
!!!(p) {:.unnumlist}

one could use the following approach, which doesn't generate structures:

[ ](#){:#l0155}`(defun scale-point (k x y z)`
!!!(p) {:.unnumlist}

` "Multiply the point (x,y,z) by a constant, K."`
!!!(p) {:.unnumlist}

` (values (* k x) (* k y) (* k z)))`
!!!(p) {:.unnumlist}

### [ ](#){:#st9010}Avoid Consing: Resources
{:#s9010}
{:.h2hd}

Sometimes it pays to manage explicitly the storage of instances of some data type.
A pool of these instances may be called a *resource*.
Explicit management of a resource is appropriate when: (1) instances are frequently created, and are needed only temporarily; (2) it is easy/possible to be sure when instances are no longer needed; and (3) instances are fairly large structures or take a long time to initialize, so that it is worth reusing them instead of creating new ones.
Condition (2) is the crucial one: If you deallocate an instance that is still being used, that instance will mysteriously be altered when it is reallocated.
Conversely, if you fail to deallocate unneeded instances, then you are wasting valuable memory space.
(The memory management scheme is said to leak in this case.)

The beauty of using Lisp's built-in memory management is that it is guaranteed never to leak and never to deallocate structures that are in use.
This eliminates two potential bug sources.
The penalty you pay for this guarantee is some inefficiency of the general-purpose memory management as compared to a custom user-supplied management scheme.
But beware: modem garbage-collection techniques are highly optimized.
In particular, the so-called *generation scavenging* or *ephemeral* garbage collectors look more often at recently allocated storage, on the grounds that recently made objects are more likely to become garbage.
If you hold on to garbage in your own data structures, you may end up with worse performance.

With all these warnings in mind, here is some code to manage resources:

[ ](#){:#l0160}`(defmacro defresource (name &key constructor (initial-copies 0)`
!!!(p) {:.unnumlist}

`         (size (max initial-copies 10)))`
!!!(p) {:.unnumlist}

` (let ((resource (symbol name '-resource))`
!!!(p) {:.unnumlist}

`   (deallocate (symbol 'deallocate- name))`
!!!(p) {:.unnumlist}

`   (allocate (symbol 'allocate- name)))`
!!!(p) {:.unnumlist}

`  '(let ((.resource (make-array ,size :fill-pointer 0)))`
!!!(p) {:.unnumlist}

`   (defun ,allocate ()`
!!!(p) {:.unnumlist}

`    "Get an element from the resource pool, or make one."`
!!!(p) {:.unnumlist}

`    (if (= (fill-pointer ,resource) 0)`
!!!(p) {:.unnumlist}

`      ,constructor`
!!!(p) {:.unnumlist}

`      (vector-pop ,resource)))`
!!!(p) {:.unnumlist}

`   (defun ,deallocate (.name)`
!!!(p) {:.unnumlist}

`    "Place a no-longer-needed element back in the pool."`
!!!(p) {:.unnumlist}

`    (vector-push-extend ,name ,resource))`
!!!(p) {:.unnumlist}

`   .(if (> initial-copies 0)`
!!!(p) {:.unnumlist}

`      '(mapc #',deallocate (loop repeat ,initial-copies`
!!!(p) {:.unnumlist}

`             collect (,allocate))))`
!!!(p) {:.unnumlist}

`   ',name)))`
!!!(p) {:.unnumlist}

Let's say we had some structure called a buffer which we were constantly making instances of and then discarding.
Furthermore, suppose that buffers are fairly complex objects to build, that we know we'll need at least 10 of them at a time, and that we probably won't ever need more than 100 at a time.
We might use the buffer resource as follows:

[ ](#){:#l0165}`(defresource buffer :constructor (make-buffer)`
!!!(p) {:.unnumlist}

`      :size 100 : initial-copies 10)`
!!!(p) {:.unnumlist}

This expands into the following code:

[ ](#){:#l0170}`(let ((buffer-resource (make-array 100 :fil1-pointer 0)))`
!!!(p) {:.unnumlist}

` (defun allocate-buffer ()`
!!!(p) {:.unnumlist}

`  "Get an element from the resource pool, or make one."`
!!!(p) {:.unnumlist}

`  (if (= (fill-pointer buffer-resource) 0)`
!!!(p) {:.unnumlist}

`   (make-buffer)`
!!!(p) {:.unnumlist}

`   (vector-pop buffer-resource)))`
!!!(p) {:.unnumlist}

` (defun deallocate-buffer (buffer)`
!!!(p) {:.unnumlist}

`  "Place a no-longer-needed element back in the pool."`
!!!(p) {:.unnumlist}

`  (vector-push-extend buffer buffer-resource))`
!!!(p) {:.unnumlist}

` (mapc #'deallocate-buffer`
!!!(p) {:.unnumlist}

`    (loop repeat 10 collect (allocate-buffer)))`
!!!(p) {:.unnumlist}

` 'buffer)`
!!!(p) {:.unnumlist}

We could then use:

[ ](#){:#l0175}`(let ((b (allocate-buffer)))`
!!!(p) {:.unnumlist}

` …`
!!!(p) {:.unnumlist}

` (process b)`
!!!(p) {:.unnumlist}

` …`
!!!(p) {:.unnumlist}

` (deallocate-buffer b)))`
!!!(p) {:.unnumlist}

The important thing to remember is that this works only if the buffer `b` really can be deallocated.
If the function `process` stored away a pointer to `b` somewhere, then it would be a mistake to deallocate `b,` because a subsequent allocation could unpredictably alter the stored buffer.
Of course, if `process` stored a *copy* of `b,` then everything is alright.
This pattern of allocation and deallocation is so common that we can provide a macro for it:

[ ](#){:#l0180}`(defmacro with-resource ((var resource &optional protect) &rest body)`
!!!(p) {:.unnumlist}

` "Execute body with VAR bound to an instance of RESOURCE."`
!!!(p) {:.unnumlist}

` (let ((allocate (symbol 'allocate- resource))`
!!!(p) {:.unnumlist}

`   (deallocate (symbol 'deallocate- resource)))`
!!!(p) {:.unnumlist}

`  (if protect`
!!!(p) {:.unnumlist}

`   '(let ((,var nil))`
!!!(p) {:.unnumlist}

`    (unwind-protect`
!!!(p) {:.unnumlist}

`     (progn (setf ,var (,allocate)) ,©body)`
!!!(p) {:.unnumlist}

`     (unless (null ,var) (,deallocate ,var))))`
!!!(p) {:.unnumlist}

`   '(let ((,var (,allocate)))`
!!!(p) {:.unnumlist}

`    ,©body`
!!!(p) {:.unnumlist}

`    (,deallocate ,var)))))`
!!!(p) {:.unnumlist}

The macro allows for an optional argument that sets up an `unwind` - protect environment, so that the buffer gets deallocated even when the body is abnormally exited.
The following expansions should make this clearer:

[ ](#){:#l0185}`>(macroexpand '(with-resource (b buffer)`
!!!(p) {:.unnumlist}

`        "…" (process b) "…"))`
!!!(p) {:.unnumlist}

`(let ((b (allocate-buffer)))`
!!!(p) {:.unnumlist}

` "…"`
!!!(p) {:.unnumlist}

` (process b)`
!!!(p) {:.unnumlist}

` "…"`
!!!(p) {:.unnumlist}

` (deallocate-buffer b))`
!!!(p) {:.unnumlist}

`> (macroexpand '(with-resource (b buffer t)`
!!!(p) {:.unnumlist}

`        "…" "…" (process b) "…"))`
!!!(p) {:.unnumlist}

`(let ((b nil))`
!!!(p) {:.unnumlist}

` (unwind-protect`
!!!(p) {:.unnumlist}

`   (progn (setf b (allocate-buffer))`
!!!(p) {:.unnumlist}

`     "…"`
!!!(p) {:.unnumlist}

`        (process b)`
!!!(p) {:.unnumlist}

`        "…")`
!!!(p) {:.unnumlist}

`      (unless (null b)`
!!!(p) {:.unnumlist}

`      (deallocate-buffer b))))`
!!!(p) {:.unnumlist}

An alternative to full resources is to just save a single data object.
Such an approach is simpler because there is no need to index into a vector of objects, but it is sufficient for some applications, such as a tail-recursive function call that only uses one object at a time.

Another possibility is to make the system slower but safer by having the `deal1ocate` function check that its argument is indeed an object of the correct type.

Keep in mind that using resources may put you at odds with the Lisp system's own storage management scheme.
In particular, you should be concerned with paging performance on virtual memory systems.
A common problem is to have only a few live objects on each page, thus forcing the system to do a lot of paging to get any work done.
Compacting garbage collectors can collect live objects onto the same page, but using resources may interfere with this.

## [ ](#){:#st0030}10.5 Use the Right Data Structures
{:#s0030}
{:.h1hd}

It is important to implement key data types with the most efficient implementation.
This can vary from machine to machine, but there are a few techniques that are universal.
Here we consider three case studies.

### [ ](#){:#st9015}The Right Data Structure: Variables
{:#s9015}
{:.h2hd}

As an example, consider the implementation of pattern-matching variables.
We saw from the instrumentation of `simplify` that `variable-p` was one of the most frequently used functions.
In compiling the matching expressions, I did away with all calls to `variable-p`, but let's suppose we had an application that required run-time use of variables.
The specification of the data type `variable` will include two operators, the recognizer `variable-p`, and the constructor `make-variable`, which gives a new, previously unused variable.
(This was not needed in the pattern matchers shown so far, but will be needed for unification with backward chaining.) One implementation of variables is as symbols that begin with the character #\?:

[ ](#){:#l0200}`(defun variable-p (x)`
!!!(p) {:.unnumlist}

` "Is x a variable (a symbol beginning with '?')?"`
!!!(p) {:.unnumlist}

` (and (symbolp x) (equal (elt (symbol-name x) 0) #\?)))`
!!!(p) {:.unnumlist}

`(defun make-variable O "Generate a new variable" (gentemp "?"))`
!!!(p) {:.unnumlist}

We could try to speed things up by changing the implementation of variables to be keywords and making the functions inline:

[ ](#){:#l0205}`(proclaim '(inline variable-p make-variable))`
!!!(p) {:.unnumlist}

`(defun variable-p (x) "Is x a variable?" (keywordp x))`
!!!(p) {:.unnumlist}

`(defun make-variable O (gentemp "X" #.(find-package "KEYWORD")))`
!!!(p) {:.unnumlist}

(The reader character sequence #.
means to evaluate at read time, rather than at execution time.) On my machine, this implementation is pretty fast, and I accepted it as a viable compromise.
However, other implementations were also considered.
One was to have variables as structures, and provide a read macro and print function:

[ ](#){:#l0210}`(defstruct (variable (:print-function print-variable)) name)`
!!!(p) {:.unnumlist}

`(defvar *vars* (make-hash-table))`
!!!(p) {:.unnumlist}

`(set-macro-character #\?`
!!!(p) {:.unnumlist}

` #'(lambda (stream char)`
!!!(p) {:.unnumlist}

`   ;; Find an old var, or make a new one with the given name`
!!!(p) {:.unnumlist}

`   (declare (ignore char))`
!!!(p) {:.unnumlist}

`   (let ((name (read stream t nil t)))`
!!!(p) {:.unnumlist}

`    (or (gethash name *vars*)`
!!!(p) {:.unnumlist}

`     (setf (gethash name *vars*) (make-variable :name name))))))`
!!!(p) {:.unnumlist}

`(defun print-variable (var stream depth)`
!!!(p) {:.unnumlist}

` (declare (ignore depth))`
!!!(p) {:.unnumlist}

` (format stream "?~a" (var-name var)))`
!!!(p) {:.unnumlist}

It turned out that, on all three Lisps tested, structures were slower than keywords or symbols.
Another alternative is to have the ?
read macro return a cons whose first is, say, `:var`.
This requires a special output routine to translate back to the ?
notation.
Yet another alternative, which turned out to be the fastest of all, was to implement variables as negative integers.
Of course, this means that the user cannot use negative integers elsewhere in patterns, but that turned out to be acceptable for the application at hand.
The moral is to know which features are done well in your particular implementation and to go out of your way to use them in critical situations, but to stick with the most straightforward implementation in noncritical sections.

Lisp makes it easy to rely on lists, but one must avoid the temptation to overuse lists; to use them where another data structure is more appropriate.
For example, if you need to access elements of a sequence in arbitrary order, then a vector is more appropriate than list.
If the sequence can grow, use an adjustable vector.
Consider the problem of maintaining information about a set of people, and searching that set.
A naive implementation might look like this:

[ ](#){:#l0215}`(defvar *people* nil "Will hold a list of people")`
!!!(p) {:.unnumlist}

`(defstruct person name address id-number)`
!!!(p) {:.unnumlist}

`(defun person-with-id (id)`
!!!(p) {:.unnumlist}

` (find id *people* :key #'person-id-number))`
!!!(p) {:.unnumlist}

In a traditional language like C, the natural solution is to include in the person structure a pointer to the next person, and to write a loop to follow these pointers.
Of course, we can do that in Lisp too:

[ ](#){:#l0220}`(defstruct person name address id-number next)`
!!!(p) {:.unnumlist}

`(defun person-with-id (id)`
!!!(p) {:.unnumlist}

` (loop for person = *people* then (person-next person)`
!!!(p) {:.unnumlist}

`   until (null person)`
!!!(p) {:.unnumlist}

`   do (when (eql id (person-id-number person))`
!!!(p) {:.unnumlist}

`     (RETURN person))))`
!!!(p) {:.unnumlist}

This solution takes less space and is probably faster, because it requires less memory accesses: one for each person rather than one for each person plus one for each cons cell.
So there is a small price to pay for using lists.
But Lisp programmers feel that price is worth it, because of the convenience and ease of coding and debugging afforded by general-purpose functions like `find`.

In any case, if there are going to be a large number of people, the list is definitely the wrong data structure.
Fortunately, Lisp makes it easy to switch to more efficient data structures, for example:

[ ](#){:#l0225}`(defun person-with-id (id)`
!!!(p) {:.unnumlist}

` (gethash id *people*))`
!!!(p) {:.unnumlist}

### [ ](#){:#st9020}The Right Data Structure: Queues
{:#s9020}
{:.h2hd}

A *queue* is a data structure where one can add elements at the rear and remove them from the front.
This is almost like a stack, except that in a stack, elements are both added and removed at the same end.

Lists can be used to implement stacks, but there is a problem in using lists to implement queues: adding an element to the rear requires traversing the entire list.
So collecting *n* elements would be *O*(*n2*) instead of *O*(*n*).

An alternative implementation of queues is as a cons of two pointers: one to the list of elements of the queue (the contents), and one to the last cons cell in the list.
Initially, both pointers would be nil.
This implementation in fact existed in BBN Lisp and UCI Lisp under the function name `tconc`:

[ ](#){:#l0230}`;;; A queue is a (contents .
last) pair`
!!!(p) {:.unnumlist}

`(defun tconc (item q)`
!!!(p) {:.unnumlist}

` "Insert item at the end of the queue."`
!!!(p) {:.unnumlist}

` (setf (cdr q)`
!!!(p) {:.unnumlist}

`   (if (null (cdr q))`
!!!(p) {:.unnumlist}

`     (setf (car q) (cons item nil))`
!!!(p) {:.unnumlist}

`     (setf (rest (cdr q))`
!!!(p) {:.unnumlist}

`       (cons item nil)))))`
!!!(p) {:.unnumlist}

The `tconc` implementation has the disadvantage that adding the first element to the contents is different from adding subsequent elements, so an `if` statement is required to decide which action to take.
The definition of queues given below avoids this disadvantage with a clever trick.
First, the order of the two fields is reversed.
The `car` of the cons cell is the last element, and the `cdr` is the contents.
Second, the empty queue is a cons cell where the `cdr` (the contents field) is nil, and the `car` (the last field) is the cons itself.
In the definitions below, we change the name `tconc` to the more standard `enqueue`, and provide the other queue functions as well:

[ ](#){:#l0235}`;;; A queue is a (last .
contents) pair`
!!!(p) {:.unnumlist}

`(proclaim '(inline queue-contents make-queue enqueue dequeue`
!!!(p) {:.unnumlist}

`        front empty-queue-p queue-nconc))`
!!!(p) {:.unnumlist}

`(defun queue-contents (q) (cdr q))`
!!!(p) {:.unnumlist}

`(defun make-queue ()`
!!!(p) {:.unnumlist}

` "Build a new queue, with no elements."`
!!!(p) {:.unnumlist}

` (let ((q (cons nil nil)))`
!!!(p) {:.unnumlist}

`  (setf (car q) q)))`
!!!(p) {:.unnumlist}

`(defun enqueue (item q)`
!!!(p) {:.unnumlist}

` "Insert item at the end of the queue."`
!!!(p) {:.unnumlist}

` (setf (car q)`
!!!(p) {:.unnumlist}

`     (setf (rest (car q))`
!!!(p) {:.unnumlist}

`      (cons item nil)))`
!!!(p) {:.unnumlist}

` q)`
!!!(p) {:.unnumlist}

`(defun dequeue (q)`
!!!(p) {:.unnumlist}

` "Remove an item from the front of the queue."`
!!!(p) {:.unnumlist}

` (pop (cdr q))`
!!!(p) {:.unnumlist}

` (if (null (cdr q)) (setf (car q) q))`
!!!(p) {:.unnumlist}

` q)`
!!!(p) {:.unnumlist}

`(defun front (q) (first (queue-contents q)))`
!!!(p) {:.unnumlist}

`(defun empty-queue-p (q) (null (queue-contents q)))`
!!!(p) {:.unnumlist}

`(defun queue-nconc (q list)`
!!!(p) {:.unnumlist}

` "Add the elements of LIST to the end of the queue."`
!!!(p) {:.unnumlist}

` (setf (car q)`
!!!(p) {:.unnumlist}

`     (last (setf (rest (car q)) list))))`
!!!(p) {:.unnumlist}

### [ ](#){:#st9030}The Right Data Structure: Tables
{:#s9030}
{:.h2hd}

A *table* is a data structure to which one can insert a key and associate it with a value, and later use the key to look up the value.
Tables may have other operations, like counting the number of keys, clearing out all keys, or mapping a function over each key/value pair.

Lisp provides a wide variety of choices to implement tables.
An association list is perhaps the simplest: it is just a list of key/value pairs.
It is appropriate for small tables, up to a few dozen pairs.
The hash table is designed to be efficient for large tables, but may have significant overhead for small ones.
If the keys are symbols, property lists can be used.
If the keys are integers in a narrow range (or can be mapped into them), then a vector may be the most efficient choice.

Here we implement an alternative data structure, the *trie*.
A trie implements a table for keys that are composed of a finite sequence of components.
For example, if we were implementing a dictionary as a trie, each key would be a word, and each letter of the word would be a component.
The value of the key would be the word's definition.
At the top of the dictionary trie is a multiway branch, one for each possible first letter.
Each second-level node has a branch for every possible second letter, and so on.
To find an *n*-letter word requires *n* reads.
This kind of organization is especially good when the information is stored on secondary storage, because a single read can bring in a node with all its possible branches.

If the keys can be arbitrary list structures, rather than a simple sequence of letters, we need to regularize the keys, transforming them into a simple sequence.
One way to do that makes use of the fact that any tree can be written as a linear sequence of atoms and cons operations, in prefix form.
Thus, we would make the following transformation:

[ ](#){:#l0240}`(a (b c) d) ≡`
!!!(p) {:.unnumlist}

`(cons a (cons (cons b (cons c nil)) (cons d nil)))`≡
!!!(p) {:.unnumlist}

`(cons a cons cons b cons c nil cons d nil)`
!!!(p) {:.unnumlist}

In the implementation of tries below, this transformation is done on the fly: The four user-level functions are `make-trie` to create a new trie, `put-trie` and `get-trie` to add and retrieve key/value pairs, and `delete-trie` to remove them.

Notice that we use a distinguished value to mark deleted elements, and that `get-trie` returns two values: the actual value found, and a flag saying if anything was found or not.
This is consistent with the interface to `gethash` and `find`, and allows us to store null values in the trie.
It is an inobtrusive choice, because the programmer who decides not to store null values can just ignore the second value, and everything will work properly.

[ ](#){:#l0245}`(defstruct trie (value nil) (arcs nil))`
!!!(p) {:.unnumlist}

`(defconstant trie-deleted "deleted")`
!!!(p) {:.unnumlist}

`(defun put-trie (key trie value)`
!!!(p) {:.unnumlist}

` "Set the value of key in trie."`
!!!(p) {:.unnumlist}

` (setf (trie-value (find-trie key t trie)) value))`
!!!(p) {:.unnumlist}

`(defun get-trie (key trie)`
!!!(p) {:.unnumlist}

` "Return the value for a key in a trie, and t/nil if found."`
!!!(p) {:.unnumlist}

` (let* ((key-trie (find-trie key nil trie))`
!!!(p) {:.unnumlist}

`    (val (if key-trie (trie-value key-trie))))`
!!!(p) {:.unnumlist}

`  (if (or (null key-trie) (eq val trie-deleted))`
!!!(p) {:.unnumlist}

`    (values nil nil )`
!!!(p) {:.unnumlist}

`    (values val t))))`
!!!(p) {:.unnumlist}

`(defun delete-trie (key trie)`
!!!(p) {:.unnumlist}

` "Remove a key from a trie."`
!!!(p) {:.unnumlist}

` (put-trie key trie trie-deleted))`
!!!(p) {:.unnumlist}

`(defun find-trie (key extend?
trie)`
!!!(p) {:.unnumlist}

` "Find the trie node for this key.`
!!!(p) {:.unnumlist}

` If EXTEND?
is true, make a new node if need be."`
!!!(p) {:.unnumlist}

` (cond ((null trie) nil )`
!!!(p) {:.unnumlist}

`    ((atom key)`
!!!(p) {:.unnumlist}

`     (follow-arc key extend?
trie))`
!!!(p) {:.unnumlist}

`    (t (find-trie`
!!!(p) {:.unnumlist}

`       (cdr key) extend?`
!!!(p) {:.unnumlist}

`       (find-trie`
!!!(p) {:.unnumlist}

`        (car key) extend?`
!!!(p) {:.unnumlist}

`       (find-trie`
!!!(p) {:.unnumlist}

`        "." extend?
trie))))))`
!!!(p) {:.unnumlist}

`(defun follow-arc (component extend?
trie)`
!!!(p) {:.unnumlist}

` "Find the trie node for this component of the key.`
!!!(p) {:.unnumlist}

` If EXTEND?
is true, make a new node if need be."`
!!!(p) {:.unnumlist}

` (let ((arc (assoc component (trie-arcs trie))))`
!!!(p) {:.unnumlist}

`  (cond ((not (null arc)) (cdr arc))`
!!!(p) {:.unnumlist}

`     ((not extend?) nil)`
!!!(p) {:.unnumlist}

`     (t (let ((new-trie (make-trie)))`
!!!(p) {:.unnumlist}

`       (push (cons component new-trie)`
!!!(p) {:.unnumlist}

`         (trie-arcs trie))`
!!!(p) {:.unnumlist}

`       new-trie)))))`
!!!(p) {:.unnumlist}

There are a few subtleties in the implementation.
First, we test for deleted entries with an `eq` comparison to a distinguished marker, the string `trie-deleted`.
No other object will be `eq` to this string except `trie-deleted` itself, so this is a good test.
We also use a distinguished marker, the string "." to mark cons cells.
Components are implicitly compared against this marker with an `eql` test by the `associn fol1ow - arc`.
Maintaining the identity of this string is crucial; if, for example, you recompiled the definition of `find-trie` (without changing the definition at all), then you could no longer find keys that were indexed in an existing trie, because the ".
" used by `find-trie` would be a different one from the ".
" in the existing trie.

*Artificial Intelligence Programming* ([Charniak et al.
1987](B9780080571157500285.xhtml#bb0180)) discusses variations on the trie, particularly in the indexing scheme.
If we always use proper lists (no non-null `cdrs`), then a more efficient encoding is possible.
As usual, the best type of indexing depends on the data to be indexed.
It should be noted that Charniak et al.
call the trie a *discrimination net*.
In general, that term refers to any tree with tests at the nodes.

A trie is, of course, a kind of tree, but there are cases where it pays to convert a trie into a *dag*—a directed acyclic graph.
A dag is a tree where some of the subtrees are shared.
Imagine you have a spelling corrector program with a list of some 50,000 or so words.
You could put them into a trie, each word with the value t.
But there would be many subtrees repeated in this trie.
For example, given a word list containing *look*, *looks*, *looked*, and *looking* as well as *show*, *shows*, *showed*, and *showing*, there would be repetition of the subtree containing -s, − *ed* and -*ing*.
After the trie is built, we could pass the whole trie to un i que, and it would collapse the shared subtrees, saving storage.
Of course, you can no longer add or delete keys from the dag without risking unintended side effects.

This process was carried out for a 56,000 word list.
The trie took up 3.2Mbytes, while the dag was 1 .IMbytes.
This was still deemed unacceptable, so a more compact encoding of the dag was created, using a .2Mbytes vector.
Encoding the same word list in a hash table took twice this space, even with a special format for encoding suffixes.

Tries work best when neither the indexing key nor the retrieval key contains variables.
They work reasonably well when the variables are near the end of the sequence.
Consider looking up the pattern `"yello?
"` in the dictionary, where the " ?
" character indicates a match of any letter.
Following the branches for `"yel1o"` leads quickly to the only possible match, `"yel1ow"`.
In contrast, fetching with the pattern `"??llow"` is much less efficient.
The table lookup function would have to search all 26 top-level branches, and for each of those consider all possible second letters, and for each of those consider the path `"llow"`.
Quite a bit of searching is required before arriving at the complete set of matches: bellow, billow, fallow, fellow, follow, hallow, hollow, mallow, mellow, pillow, sallow, tallow, wallow, willow, and yellow.

We will return to the problem of discrimination nets with variables in [section 14.8](B9780080571157500145.xhtml#s0040), [page 472](B9780080571157500145.xhtml#p472).

## [ ](#){:#st0035}10.6 Exercises[ ](#){:#p2640}
{:#s0035}
{:.h1hd}

[ ](#){:#l0250}**Exercise 10.1 [h]** Define the macro `deftable,` such that `(deftable person assoc`) will act much like a `defstruct—`it will define a set of functions for manipulating a table of people: `get-person, put-person, clear-person,` and `map-person.` The table should be implemented as an association list.
Later on, you can change the representation of the table simply by changing the form to (`deftable person hash` ), without having to change anything else in your code.
Other implementation options include property lists and vectors.
`deftable` should also take three keyword arguments: `inline`, `size` and `test`.
Here is a possible macroexpansion:
!!!(p) {:.unnumlist}

[ ](#){:#l0255}`>(macroexpand '(deftableperson hash :-inline t :size 100))`≡
!!!(p) {:.unnumlist1}

` (progn`
!!!(p) {:.unnumlist1}

` (proclaim '(inline get-person put-person map-person))`
!!!(p) {:.unnumlist1}

` (defparameter *person-table*`
!!!(p) {:.unnumlist1}

`  (make-hash-table :test #eql :size 100))`
!!!(p) {:.unnumlist1}

` (defun get-person (x &optional default)`
!!!(p) {:.unnumlist1}

`  (gethash x *person-table* default))`
!!!(p) {:.unnumlist1}

` (defun put-person (x value)`
!!!(p) {:.unnumlist1}

`  (setf (gethash x *person-table*) value))`
!!!(p) {:.unnumlist1}

` (defun clear-person () (clrhash *person-table*))`
!!!(p) {:.unnumlist1}

` (defun map-person (fn) (maphash fn *person-table*))`
!!!(p) {:.unnumlist1}

` (defsetf get-person put-person)`
!!!(p) {:.unnumlist1}

` 'person)`
!!!(p) {:.unnumlist1}

**Exercise 10.2 [m]** We can use the :`type` option to `defstruct` to define structures implemented as lists.
However, often we have a two-field structure that we would like to implement as a cons cell rather than a two-element list, thereby cutting storage in half.
Since `defstruct` does not allow this, define a new macro that does.
!!!(p) {:.unnumlist}

**Exercise 10.3 [m]** Use `reuse - cons` to write a version of `f1atten` (see [page 329](B9780080571157500108.xhtml#p329)) that shares as much of its input with its output as possible.
!!!(p) {:.unnumlist}

**Exercise 10.4 [h]** Consider the data type *set*.
A set has two main operations: adjoin an element and test for membership.
It is convenient to also add a map-over-elements operation.
With these primitive operations it is possible to build up more complex operations like union and intersection.
!!!(p) {:.unnumlist}

As mentioned in [section 3.9](B9780080571157500030.xhtml#s0095), Common Lisp provides several implementations of sets.
The simplest uses lists as the underlying representation, and provides the functions `adjoin, member, union, intersection`, and `set-difference`.
Another uses bit vectors, and a similar one uses integers viewed as bit sequences.
Analyze the time complexity of each implementation for each operation.

Next, show how *sorted lists* can be used to implement sets, and compare the operations on sorted lists to their counterparts on unsorted lists.

## [ ](#){:#st0040}10.7 Answers
{:#s0040}
{:.h1hd}

**Answer 10.2**

[ ](#){:#l0260}`(defmacro def-cons-struct (cons car cdr &optional inline?)`
!!!(p) {:.unnumlist}

` "Define aliases for cons, car and cdr."`
!!!(p) {:.unnumlist}

` '(progn (proclaim '(,(if inline?
'inline 'notinline)`
!!!(p) {:.unnumlist}

`         ,car ,cdr ,cons))`
!!!(p) {:.unnumlist}

`     (defun ,car (x) (car x))`
!!!(p) {:.unnumlist}

`     (defun ,cdr (x) (cdr x))`
!!!(p) {:.unnumlist}

`     (defsetf ,car (x) (val) '(setf (car ,x) ,val))`
!!!(p) {:.unnumlist}

`     (defsetf ,cdr (x) (val) '(setf (cdr ,x) ,val))`
!!!(p) {:.unnumlist}

`     (defun ,cons (x y) (cons x y))))`
!!!(p) {:.unnumlist}

**Answer 10.3**

[ ](#){:#l0265}`(defun flatten (exp &optional (so-far nil) last-cons)`
!!!(p) {:.unnumlist}

` "Return a flat list of the atoms in the input.`
!!!(p) {:.unnumlist}

` Ex: (flatten '((a) (b (c) d))) => (a b c d)."`
!!!(p) {:.unnumlist}

` (cond ((null exp) so-far)`
!!!(p) {:.unnumlist}

`    ((atom exp) (reuse-cons exp so-far last-cons))`
!!!(p) {:.unnumlist}

`    (t (flatten (first exp)`
!!!(p) {:.unnumlist}

`         (flatten (rest exp) so-far exp)`
!!!(p) {:.unnumlist}

`         exp))))`
!!!(p) {:.unnumlist}

----------------------

[1](#xfn0015){:#np0015} These are all done with safety 0 and speed 3.
!!!(p) {:.ftnote1}

