# Chapter 10 {docsify-ignore}
<a id='page-315'></a>

### Low-Level Efficiency Issues 

> There are only two qualities in the world: efficiency 
and inefficiency; and only two sorts of people: the 
efficient and the inefficient 
>
> —George Bernard Shaw, 
> *John Bull's Other Island* (1904) 

The efficiency techniques of the previous chapter all involved fairly significant changes 
to an algorithm. But what happens when you already are using the best imaginable 
algorithms, and performance is still a problem? One answer is to find what parts of the 
program are used most frequently and make micro-optimizations to those parts. This chapter 
covers the following six optimization techniques. If your programs all run quickly enough, then 
feel free to skip this chapter. But if you would like your programs to run faster, the techniques 
described here can lead to speed-ups of 40 times or more. 

<a id='page-316'></a>

* Use declarations. 
* Avoid generic functions. 
* Avoid complex argument lists. 
* Provide compiler macros. 
* Avoid unnecessary consing. 
* Use the right data structure. 

### 10.1 Use Declarations 
On general-purpose computers running Lisp, much time is spent on type-checking. 
You can gain efficiency at the cost of robustness by declaring, or promising, that 
certain variables will always be of a given type. For example, consider the following 
function to compute the sum of the squares of a sequence of numbers: 

```lisp
(defun sum-squares (seq) 
  (let ((sum 0)) 
    (dotimes (i (length seq)) 
      (incf sum (square (elt seq i)))) 
    sum)) 

(defun square (x) (* . x)) 
```

If this function will only be used to sum vectors of fixnums, we can make it a lot faster 
by adding declarations: 

```lisp
(defun sum-squares (vect) 
  (declare (type (simple-array fixnum *) vect) 
           (inline square) (optimize speed (safety 0))) 

  (let ((sum 0)) 
    (declare (fixnum sum)) 
    (dotimes (i (length vect)) 
      (declare (fixnum i)) 
    (incf sum (the fixnum (square (svref vect i))))))) 
  sum)) 
```

The fixnum declarations let the compiler use integer arithmetic directly, rather than 
checking the type of each addend. The `(the fixnum ... )` special form is a promise 
that the argument is a fixnum. The `(optimize speed (safety 0))` declaration tells 
the compiler to make the function run as fast as possible, at the possible expense of 
<a id='page-317'></a>
making the code less safe (by ignoring type checks and so on). Other quantities that 
can be optimized are `compilation-speed`, `space` and in ANSI Common Lisp only, 
`debug` (ease of debugging). Quantities can be given a number from 0 to 3 indicating 
how important they are; 3 is most important and is the default if the number is left out. 

The `(inline square)` declaration allows the compiler to generate the multiplication 
specified by `square` right in the loop, without explicitly making a function 
call to square. The compiler will create a local variable for `(svref vect i)` and will 
not execute the reference twice—inline functions do not have any of the problems 
associated with macros as discussed on [page 853](chapter24.md#page-853). However, there is one drawback: 
when you redefine an inline function, you may need to recompile all the functions 
that call it. 

You should declare a function `inline` when it is short and the function-calling 
overhead will thus be a significant part of the total execution time. You should not 
declare a function `inline` when the function is recursive, when its definition is likely 
to change, or when the function's definition is long and it is called from many places. 

In the example at hand, declaring the function `inline` saves the overhead of 
a function call. In some cases, further optimizations are possible. Consider the 
predicate `starts-with`: 

```lisp
(defun starts-with (list x) 
  "Is this a list whose first element is x?" 
  (and (consp list) (eql (first list) x))) 
```

Suppose we have a code fragment like the following: 

```lisp
(if (consp list) (starts-with list x) ...) 
```

If `starts-with` is declared `inline` this will expand to: 

```lisp
(if (consp list) (and (consp list) (eql (first list) x)) ...) 
```

which many compilers will simplify to: 

```lisp
(if (consp list) (eql (first list) x) ...) 
```

Very few compilers do this kind of simplification across functions without the hint 
provided by `inline`. 

Besides eliminating run-time type checks, declarations also allow the compiler 
to choose the most efficient representation of data objects. Many compilers support 
both *boxed* and *unboxed* representations of data objects. A boxed representation 
includes enough information to determine the type of the object. An unboxed 
representation is just the "raw bits" that the computer can deal with directly. Consider 
<a id='page-318'></a>
the following function, which is used to clear a 1024x1024 array of floating point 
numbers, setting each one to zero: 

```lisp
(defun clear-m-array (array) 
  (declare (optimize (speed 3) (safety 0))) 
  (declare (type (simple-array single-float (1024 1024)) array)) 
  (dotimes (i 1024) 
    (dotimes (j 1024) 
      (setf (aref array i j) 0.0)))) 
```

In Allegro Common Lisp on a Sun SPARCstation, this compiles into quite good code, 
comparable to that produced by the C compiler for an equivalent C program. If the 
declarations are omitted, however, the performance is about 40 times worse. 

The problem is that without the declarations, it is not safe to store the raw floating 
point representation of `0.0` in each location of the array. Instead, the program 
has to box the `0.0`, allocating storage for a typed pointer to the raw bits. This 
is done inside the nested loops, so the result is that each call to the version of 
`clear-m-array` without declarations calls the floating-point-boxing function 1048567 
times, allocating a megaword of storage. Needless to say, this is to be avoided. 

Not all compilers heed all declarations; you should check before wasting time 
with declarations your compiler may ignore. The function `disassemble` can be used 
to show what a function compiles into. For example, consider the trivial function to 
add two numbers together. Here it is with and without declarations: 

```lisp
(defun f (x y) 
  (declare (fixnum . y) (optimize (safety 0) (speed 3))) 
  (the fixnum (+ . y))) 
(defun g (x y) (+ . y)) 
```

Here is the disassembled code for f from Allegro Common Lisp for a Motorola 
68000-series processor: 

```
> (disassemble 'f) 
;; disassembling #<Function f @ #x83ef79> 
;; formals: x y 
;; code vector @ #x83ef44 
0:      link    a6.#0 
4:      move.l  a2.-(a7) 
6:      move.l  a5,-(a7) 
8:      move.l  7(a2),a5 
12:     move.l  8(a6),d4 y 
16:     add.l   12(a6),d4 ; X 
20:     move.l  #l,d1 
```
<a id='page-319'></a>
```
22:     move.l  -8(a6),a5 
26:     unlk    a6 
28:     rtd     #8 
```

This may look intimidating at first glance, but you don't have to be an expert at 68000 
assembler to gain some appreciation of what is going on here. The instructions 
labeled 0-8 (labels are in the leftmost column) comprise the typical function preamble 
for the 68000. They do subroutine linkage and store the new function object and 
constant vector into registers. Since `f` uses no constants, instructions 6, 8, and 22 
are really unnecessary and could be omitted. Instructions 0,4, and 26 could also be 
omitted if you don't care about seeing this function in a stack trace during debugging. 
More recent versions of the compiler will omit these instructions. 

The heart of function `f` is the two-instruction sequence 12-16. Instruction 12 
retrieves `y`, and 16 adds `y` to `x`, leaving the result in `d4`, which is the "result" register. 
Instruction 20 sets `d1`, the "number of values returned" register, to 1. 

Contrast this to the code for `g`, which has no declarations and is compiled at 
default speed and safety settings: 

```
> (disassemble 'g) 
;; disassembling #<Function g @ #x83dbd1> 
;; formals: x y 
;; code vector @ #x83db64 
0:      add.l   #8.31(a2) 
4:      sub.w   #2,dl 
6:      beq.s   12 
8:      jmp     16(a4) ; wnaerr 
12:     link    a6.#0 
16:     move.l  a2,-(a7) 
18:     move.l  a5,-(a7) 
20:     move.l  7(a2),a5 
24:     tst.b   -208(a4) ; signal-hit 
28:     beq.s   34 
30:     jsr     872(a4) ; process-sig 
34:     move.l  8(a6),d4 ; y 
38:     move.l  12(a6),d0 ; X 
42:     or.l    d4,d0 
44:     and.b   #7.d0 
48:     bne.s   62 
50:     add.l   12(a6),d4 ; X 
54:     bvc.s   76 
56:     jsr     696(a4) ; add-overflow 
60:     bra.s   76 
62:     move.l  12(a6),-(a7) ; . 
66:     move.l  d4.-(a7) 
68:     move.l  #2.dl 
```
<a id='page-320'></a>

```
70:     move.l  -304(a4),a 0 ; +-2op 
74:     jsr     (a4) 
76:     move.l  #1,d1 
78:     move.l  -8(a6),a5 
82:     unlk    a6 
84:     rtd     #8 
```

See how much more work is done. The first four instructions ensure that the right 
number of arguments have been passed to `g`. If not, there is a jump to `wnaerr` (wrong-number-
of-arguments-error). Instructions 12-20 have the argument loading code 
that was at 0-8 in `f`. At 24-30 there is a check for asynchronous signals, such as the 
user hitting the abort key. After `x` and `y` are loaded, there is a type check (42-48). If 
the arguments are not both fixnums, then the code at instructions 62-74 sets up a 
call to `+_2op`, which handles type coercion and non-fixnum addition. If all goes well, 
we don't have to call this routine, and do the addition at instruction 50 instead. But 
even then we are not done—just because the two arguments were fixnums does not 
mean the result will be. Instructions 54-56 check and branch to an overflow routine 
if needed. Finally, instructions 76-84 return the final value, just as in `f`. 

Some low-quality compilers ignore declarations altogether. Other compilers 
don't need certain declarations, because they can rely on special instructions in the 
underlying architecture. On a Lisp Machine, both `f` and `g` compile into the same 
code: 

```
6 PUSH      ARG|0     ; X 
7 +         ARG|1     ; Y 
8 RETURN    PDL-POP 
```

The Lisp Machine has a microcoded `+` instruction that simultaneously does a fixnum 
add and checks for non-fixnum arguments, branching to a subroutine if either argument 
is not a fixnum. The hardware does the work that the compiler has to do on a 
conventional processor. This makes the Lisp Machine compiler simpler, so compiling 
a function is faster. However, on modern pipelined computers with instruction 
caches, there is little or no advantage to microcoding. The current trend is away from 
microcode toward reduced instruction set computers (RISC). 

On most computers, the following declarations are most likely to be helpful: 

* `fixnum` and `float`. Numbers declared as fixnums or floating-point numbers 
can be handled directly by the host computer's arithmetic instructions. On 
some systems, `float` by itself is not enough; you have to say `single-float` 
or `double-float`. Other numeric declarations will probably be ignored. For 
example, declaring a variable as `integer` does not help the compiler much, 
because bignums are integers. The code to add bignums is too complex to put 
<a id='page-321'></a>
inline, so the compiler will branch to a general-purpose routine (like `+_2op` in 
Allegro), the same routine it would use if no declarations were given. 

* `list` and `array`. Many Lisp systems provide separate functions for the list- and 
array- versions of commonly used sequence functions. For example, `(delete 
X (the list l))` compiles into `(sys: delete-list-eql x 1)` on a TI Explorer 
Lisp Machine. Another function, `sys:delete-vector`, is used for arrays, and 
the generic function `delete` is used only when the compiler can't tell what type 
the sequence is. So if you know that the argument to a generic function is either 
a `list` or an `array`, then declare it as such. 
* `simple-vector` and `simple-array`. Simple vectors and arrays are those that 
do not share structure with other arrays, do not have fill pointers, and are 
not adjustable. In many implementations it is faster to `aref` a `simple-vector` 
than a `vector`. It is certainly much faster than taking an `elt` of a sequence of 
unknown type. Declare your arrays to be simple (if they in fact are). 
* `(array` *type*`)`. It is often important to specialize the type of array elements. For 
example, an `(array short-float)` may take only half the storage of a general 
array, and such a declaration will usually allow computations to be done using 
the CPU's native floating-point instructions, rather than converting into and 
out of Common Lisp's representation of floating points. This is very important 
because the conversion normally requires allocating storage, but the direct 
computation does not. The specifiers `(simple-array type)` and `(vector type)` 
should be used instead of `(array type)` when appropriate. A very common 
mistake is to declare `(simple-vector type)`. This is an error because Common 
Lisp expects `(simple-vector size)`—don't ask me why. 
* `(array * dimensions)`. The full form of an `array` or `simple-array` type specifier 
is `(array type dimensions)`. So, for example, `(array bit (* *))` is a two-
dimensional bit array, and `(array bit (1024 1024))` is a 1024 &times; 1024 bit array. 
It is very important to specify the number of dimensions when known, and less 
important to specify the exact size, although with multidimensional arrays, 
declaring the size is more important. The format for a `vector` type specifier is 
`(vector type size)`. 
Note that several of these declarations can apply all at once. For example, in 

```lisp
(position #\. (the simple-string file-name)) 
```

the variable `filename` has been declared to be a vector, a simple array, and a sequence 
of type `string-char`. All three of these declarations are helpful. The type 
`simple-string` is an abbreviation for `(simple-array string-char)`. 

<a id='page-322'></a>

This guide applies to most Common Lisp systems, but you should look in the 
implementation notes for your particular system for more advice on how to fine-tune 
your code. 

### 10.2 Avoid Generic Functions 
Common Lisp provides functions with great generality, but someone must pay the 
price for this generality. For example, if you write `(elt x 0)`, different machine 
instruction will be executed depending on if `x` is a list, string, or vector. Without 
declarations, checks will have to be done at runtime. You can either provide declarations, 
as in `(elt (the list x) O)`, or use a more specific function, such as `(first x)` 
in the case of lists, `(char x 0)` for strings, `(aref x 0)` for vectors, and `(svref x 0)` 
for simple vectors. Of course, generic functions are useful—I wrote `random-elt` 
as shown following to work on lists, when I could have written the more efficient 
`random-mem` instead. The choice paid off when I wanted a function to choose a random 
character from a string—`random-elt` does the job unchanged, while `random-mem` 
does not. 
``` lisp
(defun random-elt (s) (elt s (random (length s)))) 
(defun random-mem (1) (nth (random (length (the list 1))) 1)) 
```

This example was simple, but in more complicated cases you can make your sequence 
functions more efficient by having them explicitly check if their arguments are lists 
or vectors. See the definition of `map-into` on [page 857](chapter24.md#page-857). 

### 10.3 Avoid Complex Argument Lists 
Functions with keyword arguments suffer a large degree of overhead. This may also 
be true for optional and rest arguments, although usually to a lesser degree. Let's 
look at some simple examples: 

```lisp
(defun reg (a b c d) (list a b c d)) 
(defun rst (a b c &rest d) (list* a b c d)) 
(defun opt (&optional a b (c 1) (d (sqrt a))) (list a b c d)) 
(defun key (&key a b (c 1) (d (sqrt a))) (list a b c d)) 
```

We can see what these compile into for the TI Explorer, but remember that your 
compiler may be quite different. 

<a id='page-323'></a>

```
> (disassemble 'reg) 
   8 PUSH           ARG|0    ; A 
   9 PUSH           ARG|I    ; B 
  10 PUSH           ARG|2    ; C 
  11 PUSH           ARG|3    ; D 
  12 TAIL-REC CALL-4  FEF|3  ; #'LIST 

> (disassemble 'rst) 
   8 PUSH           ARG|0    ; A 
   9 PUSH           ARG|1    ; B 
  10 PUSH           ARG|2    ; C 
  11 PUSH           LOCAL|0  ; D 
  12 RETURN CALL-4  FEF|3    ; #'LIST* 
```

With the regular argument list, we just push the four variables on the argument stack 
and branch to the list function. (Chapter 22 explains why a tail-recursive call is just 
a branch statement.) 

With a rest argument, things are almost as easy. It turns out that on this machine, 
the microcode for the calling sequence automatically handles rest arguments, storing 
them in local variable 0. Let's compare with optional arguments: 

```lisp
(defun opt (&optional a b (c 1) (d (sqrt a))) (list a b c d)) 

> (disassemble 'opt) 
24 DISPATCH       FEF|5     ; [0=>25;1=>25;2=>25;3=>27;ELSE=>30]
25 PUSH-NUMBER    1 
26 POP            ARG|2     ; C 
27 PUSH           ARG|0     ; A 
28 PUSH CALL-1    FEF|3     ; #'SQRT 
29 POP            ARG|3     ; D 
30 PUSH           ARG|0     ; A 
31 PUSH           ARG|1     ; B 
32 PUSH           ARG|2     ; C 
33 PUSH           ARG|3     ; D 
34 TAIL-REC CALL-4 FEFI4    ; #'LIST 
```

Although this assembly language may be harder to read, it turns out that optional 
arguments are handled very efficiently. The calling sequence stores the number of 
optional arguments on top of the stack, and the `DISPATCH` instruction uses this to 
index into a table stored at location `FEF|5` (an offset five words from the start of 
the function). The result is that in one instruction the function branches to just the 
right place to initialize any unspecified arguments. Thus, a function with optional 
arguments that are all supplied takes only one more instruction (the dispatch) than 
the "regular" case. Unfortunately, keyword arguments don't fare as well: 

```lisp
(defun key (&key a b (c 1) (d (sqrt a))) (list a b c d)) 
```

<a id='page-324'></a>

```lisp
> (disassemble 'key) 
14 PUSH-NUMBER      1 
15 POP              LOCAL|3   ; C 
16 PUSH             FEF|3     ; SYS::KEYWORD-GARBAGE 
17 POP              LOCAL|4 
18 TEST             LOCAL|0 
19 BR-NULL      24 
20 PUSH             FEF|4     ; '(:A :B :C :D) 
21 SET-NIL          PDL-PUSH 
22 PUSH-LOC         LOCAL|1   ; A
23 (AUX) %STORE-KEY-WORD-ARGS 
24 PUSH             LOCAL|1   ; A 
25 PUSH             LOCAL|2   ; B 
26 PUSH             LOCAL|3   ; C 
27 PUSH             LOCAL|4 
28 EQ               FEF|3     ; SYS::KEYWORD-GARBAGE 
29 BR-NULL      33 
30 PUSH             LOCAL|1   ; A 
31 PUSH CALL-1      FEF|5     ; #'SQRT 
32 RETURN CALL-4    FEF|6     ; #'LIST 
33 PUSH             LOCAL|4 
34 RETURN CALL-4    FEF|6     ; #'LIST 
```

It is not important to be able to read all this assembly language. The point is that there 
is considerable overhead, even though this architecture has a specific instruction 
`(%STORE-KEY-WORD-ARGS)` to help deal with keyword arguments. 

Now let's look at the results on another system, the Allegro compiler for the 
68000. First, here's the assembly code for `reg`, to give you an idea of the minimal 
calling sequence:[1](#fn-10-1)

```
> (disassemble 'reg) 
;; disassembling #<Function reg @ #x83db59> 
;; formals: a b c d 
;; code vector @ #x83db1c 
0:      link    a6,#0 
4:      move.l  a2.-(a7) 
6:      move.l  a5.-(a7) 
8:      move.l  7(a2),a5 
12:     move.l  20(a6),-(a7)    ; a 
16:     move.l  16(a6).-(a7)    ; b 
20:     move.l  12(a6),-(a7)    ; c 
24:     move.l  8(a6).-(a7)     ; d 
28:     move.l  #4.dl 
30:     jsr     848(a4)         ; list 
```

[fn-10-1] These are all done with safety 0 and speed 3. 

<a id='page-325'></a>

```
34:     move.l  -8(a6).a5 
38:     unlk    a6 
40:     rtd     #10 
```

Now we see that &rest arguments take a lot more code in this system: 

### WIP

> (disassemble 'rst) 
;; disassembling #<Function rst @ #x83de89> 
;; formals: a D c &rest d 
11 code vector @ #x83de34 
0: sub.w #3,dl 
2: bge.s 8 

4: jmp 16(a4) ; wnaerr 
8: move.l (a7)+.al 
10 move.l d3.-(a7) ; nil 
12 sub.w #l,dl 
14 blt.s 38 
16 move.l al,-52(a4) ; c-protected-retaddr 
20 jsr 40(a4) ; cons 
24 move.l d4,-(a7) 
26 dbra dl,20 
30 move.l -52(a4).al ; C-protected-retaddr 
34 clr.l -52(a4) ; C-protected-retaddr 
38 move.l al.-(a7) 
40 link a6.#0 
44 move.l a2.-(a7) 
46 move.l a5,-(a7) 
48 move.l 7(a2).a5 
52 move.l -332(a4),a0 ; list* 
56 move.l -8(a6),a5 
60 unlk a6 
62 move.l #4,dl 
64 jmp (a4) 
The loop from 20-26 builds up the &rest list one cons at a time. Part of the difficulty 
is that cons could initiate a garbage collection at any time, so the list has to be built 
in a place that the garbage collector will know about. The function with optional 
arguments is even worse, taking 34 instructions (104 bytes), and keywords are worst 
of all, weighing in at 71 instructions (178 bytes), and including a loop. The overhead 
for optional arguments is proportional to the number of optional arguments, while 
for keywords it is proportional to the product of the number of parameters allowed 
and the number of arguments actually supplied. 

A good guideline to follow is to use keyword arguments primarily as an interface 
to infrequently used functions, and to provide versions of these functions without 
keywords that can be used in places where efficiency is important. Consider: 

<a id='page-326'></a>

(proclaim '(inline key)) 
(defun key (&key a b (c 1) (d (sqrt a))) (*no-key abed)) 
(defun *no-key (abed) (list abed)) 

Here the function key is used as an interface to the function no - key, which does the 
real work. The inline proclamation should allow the compiler to compile a call to key 
as a call to no - key with the appropriate arguments: 

> (disassemble #'(lambda (x y) (key :b . :a y))) 

10 PUSH ARG II Y 
11 PUSH ARG 10 X 
12 PUSH-NUMBER 1 
13 PUSH ARG II Y 
14 PUSH CALL-1 FEFI3 #'SQRT 
15 TAIL-REC CALL-4 FEFI4 #*NO-KEY 

The overhead only comes into play when the keywords are not known at compile 
time. In the following example, the compiler is forced to call key, not no- key, because 
it doesn't know what the keyword k will be at run time: 

> (disassemble #*(lambda (k . y) (key k . :a y))) 
10 PUSH ARGIO . 
11 PUSH ARG 11 . 
12 PUSH FEFI3 ':. 
13 PUSH ARG 12 . 
14 TAIL-REC CALL-4 FEFI4 #... 

Of course, in this simple example I could have replaced no-key with 1 i st, but in 
general there will be some more complex processing. If I had proclaimed no-key 
inline as well, then I would get the following: 

> (disassemble #'(lambda (x y) (key :b . :a y))) 

10 PUSH ARG 11 ; Y 

11 PUSH ARG 10 ; . 

12 PUSH-NUMBER 1 

13 PUSH ARG II ; Y 

14 PUSH CALL-1 FEFI3 ; #'SQRT 

15 TAIL-REC CALL-4 FEFI4 ; #'LIST 

If you like, you can define a macro to automatically define the interface to the keyword-
less function: 

<a id='page-327'></a>

(defmacro defun* (fn-name arg-list &rest body) 
"Define two functions, one an interface to a &keyword-less 
version. Proclaim the interface function inline." 
(if (and (member '&key arg-list) 

(not (member *&rest arg-list))) 
(let ((no-key-fn-name (symbol fn-name '*no-key)) 
(args (mapcar #'first-or-self 

(set-difference 
arg-list 
1ambda-list-keywords)))) 

'(progn 
(proclaim '(inline ,fn-name)) 
(defun ,no-key-fn-name ,args 

..body) 
(defun ,fn-name ,arg-list 
(,no-key-fn-name ..args)))) 
'(defun ,fn-name ,arg-list 
..body))) 

> (macroexpand '(defun* key (&key a b (c 1) (d (sqrt a))) 
(list a b c d))) 

(PROGN (PROCLAIM '(INLINE KEY)) 
(DEFUN KEY*NO-KEY (A . C D) (LIST A . C D)) 
(DEFUN KEY (&KEY A . (C 1) (D (SQRT A))) 

(KEY*NO-KEY A . C D))) 

> (macroexpand '(defun* reg (abed) (list abed))) 
(DEFUN REG (A . C D) (LIST A . C D)) 

There is one disadvantage to this approach: a user who wants to declare key inHne 
or not inline does not get the expected result. The user has to know that key is 
implemented with key*no- key, and declare key*no- key inline. 

An alternative is just to proclaim the function that uses &key to be inline. Rob 
MacLachlan provides an example. In CMU Lisp, the function member has the following 
definition, which is proclaimed inline: 

(defun member (item list &key (key #'identity) 
(test #'eql testp)(test-not nil notp)) 
(do ((list list (cdr list))) 
((null list) nil) 
(let ((car (car list))) 
(if (cond 
(testp 
(funcall test item 
(funcall key car))) 
(notp 
(not 

<a id='page-328'></a>

(funcall test-not item 
(funcall key car)))) 
(t 
(funcall test item 
(funcall key car)))) 
(return list))))) 

A call like (member ch 1 :key #'first-letter rtest #'cha r=) expands into the 
equivalent of the following code. Unfortunately, not all compilers are this clever with 
inline declarations. 

(do ((list list (cdr list))) 
((null list) nil) 
(let ((car (car list))) 
(if (char= ch (first-letter car)) 
(return list)))) 

This chapter is concerned with efficiency and so has taken a stand against the use 
of keyword parameters in frequently used functions. But when maintainability 
is considered, keyword parameters look much better. When a program is being 
developed, and it is not clear if a function will eventually need additional arguments, 
keyword parameters may be the best choice. 

10.4 Avoid Unnecessary Consing 
The cons function may appear to execute quite quickly, but like all functions that 
allocate new storage, it has a hidden cost. When large amounts of storage are 
used, eventually the system must spend time garbage collecting. We have not 
mentioned it earlier, but there are actually two relevant measures of the amount of 
space consumed by a program: the amount of storage allocated, and the amount of 
storage retained. The difference is storage that is used temporarily but eventually 
freed. Lisp guarantees that unused space will eventually be reclaimed by the garbage 
collector. This happens automatically—the programmer need not and indeed can not 
explicitly free storage. The problem is that the efficiency of garbage collection can 
vary widely. Garbage collection is particularly worrisome for real-time systems, 
because it can happen at any time. 

The antidote to garbage woes is to avoid unnecessary copying of objects in often-
used code. Try using destructive operations, like nreverse, delete, and nconc, 
rather than their nondestructive counterparts, (like reverse, remove, and append) 
whenever it is safe to do so. Or use vectors instead of lists, and reuse values rather 
than creating copies. As usual, this gain in efficiency may lead to errors that can 

<a id='page-329'></a>
be difficult to debug. However, the most common kind of unnecessary copying 
can be eliminated by simple reorganization of your code. Consider the following 
version of f 1 a tten, which returns a list of all the atoms in its input, preserving order. 
Unlike the version in chapter 5, this version returns a single list of atoms, with no 
embedded lists. 

(defun flatten (input) 
"Return a flat list of the atoms in the input. 
Ex: (flatten '((a) (b (c) d))) => (a b c d)." 
(cond ((null input) nil) 

((atom input) (list input)) 
(t (append (flatten (first input)) 
(flatten (rest input)))))) 

This definition is quite simple, and it is easy to see that it is correct. However, each 
call to append requires copying the first argument, so this version can cons O(n^) cells 
on an input with . atoms. The problem with this approach is that it computes the 
list of atoms in the first and rest of each subcomponent of the input. But the first 
sublist by itself is not part of the final answer—that's why we have to call append. We 
could avoid generating garbage by replacing append with nconc, but even then we 
would still be wasting time, because nconc would have to scan through each sublist 
to find its end. 

The version below makes use of an accumulator to keep track of the atoms that 
have been collected in the rest, and to add the atoms in the first one at a time with 
cons, rather than building up unnecessary sublists and appending them. This way 
no garbage is generated, and no subcomponent is traversed more than once. 

(defun flatten (input &optional accumulator) 
"Return a flat list of the atoms in the input. 
Ex: (flatten '((a) (b (c) d))) => (a b c d)." 
(cond ((null input) accumulator) 

((atom input) (cons input accumulator)) 

(t (flatten (first input) 
(flatten (rest input) accumulator))))) 

The version with the accumulator may be a little harder to understand, but it is far 
more efficient than the original version. Experienced Lisp programmers become 
quite skilled at replacing calls to append with accumulators. 

Some of the early Lisp machines had unreliable garbage-collection, so users 
just turned garbage collection off, used the machine for a few days, and rebooted 
when they ran out of space. With a large virtual memory system this is a feasible 
approach, because virtual memory is a cheap resource. The problem is that real 
memory is still an expensive resource. When each page contains mostly garbage 

<a id='page-330'></a>

and only a little live data, the system will spend a lot of time paging data in and out. 
Compacting garbage-collection algorithms can relocate live data, packing it into a 
minimum number of pages. 

Some garbage-collection algorithms have been optimized to deal particularly well 
with just this case. If your system has an ephemeral or generational garbage collector, 
you need not be so concerned with short-lived objects. Instead, it will be the medium-
aged objects that cause problems. The other problem with such systems arises when 
an object in an old generation is changed to point to an object in a newer generation. 
This is to be avoided, and it may be that reverse is actually faster than nreverse in 
such cases. To decide what works best on your particular system, design some test 
cases and time them. 

As an example of efficient use of storage, here is a version of pat-match that 
eliminates (almost) all consing. The original version of pat-match, as used in ELIZA 
([page 180](chapter6.md#page-180)), used an association list of variable/value pairs to represent the binding 
list. This version uses two sequences: a sequence of variables and a sequence of 
values. The sequences are implemented as vectors instead of lists. In general, vectors 
take half as much space as lists to store the same information, since half of every list 
is just pointing to the next element. 

In this case, the savings are much more substantial than just half. Instead of 
building up small binding lists for each partial match and adding to them when the 
match is extended, we will allocate a sufficiently large vector of variables and values 
just once, and use them over and over for each partial match, and even for each 
invocation of pat-match. To do this, we need to know how many variables we are 
currently using. We could initialize a counter variable to zero and increment it each 
time we found a new variable in the pattern. The only difficulty would be when the 
counter variable exceeds the size of the vector. We could just give up and print an 
error message, but there are more user-friendly alternatives. For example, we could 
allocate a larger vector for the variables, copy over the existing ones, and then add in 
the new one. 

It turns out that Common Lisp has a built-in facility to do just this. When a 
vector is created, it can be given a fill pointer. This is a counter variable, but one that 
is conceptually stored inside the vector. Vectors with fill pointers act like a cross 
between a vector and a stack. You can push new elements onto the stack with the 
functions vector -push or vector-push-extend. The latter will automatically allocate 
a larger vector and copy over elements if necessary. You can remove elements with 
vector - pop, or you can explicitly look at the fill pointer with f .1 - poi . te r, or change 
it with a setf. Here are some examples (with *print-array* set to t so we can see 
the results): 

> (setf a (make-array 5 :fiH-pointer 0)) ^ #() 

> (vector-push 1 a) 0 

<a id='page-331'></a>
> (vector-push 2 a) =.> 1 

> a => #(1 2) 

> (vector-pop a) => 2 

> a #(1) 

> (dotimes (i 10) (vector-push-extend 'x a)) NIL 

>a=:^#(lXXXXXXXXXX) 

> (fill-pointer a) => 11 

> (setf (fill-pointer a) 1) 1 

> a => #(1) 

> (find *x a) => NIL NIL ; FIND can't find past the fill pointer 

> (aref a 2) => X ;But AREF can see beyond the fill pointer 

Using vectors with fill pointers in pat-match, the total storage for binding lists is 
just twice the number of variables in the largest pattern. I have arbitrarily picked 
10 as the maximum number of variables, but even this is not a hard limit, because 
vector -push-extend can increase it. In any case, the total storage is small, fixed 
in size, and amortized over all calls to pat-match. These are just the features that 
indicate a responsible use of storage. 

However, there is a grave danger with this approach: the value returned must 
be managed carefully. The new pat-match returns the value of success when it 
matches, success is bound to a cons of the variable and value vectors. These can be 
freely manipulated by the calling routine, but only up until the next call to pa t - ma tch. 
At that time, the contents of the two vectors can change. Therefore, if any calling 
function needs to hang on to the returned value after another call to pat-match, it 
should make a copy of the returned value. So it is not quite right to say that this 
version of pat-match eliminates all consing. It will cons when vector-push-extend 
runs out of space, or when the user needs to make a copy of a returned value. 

Here is the new definition of pat-match. It is implemented by closing the defi


nition of pat-match and its two auxilliary functions inside a 1 et that establishes the 

bindings of vars, val s, and success, but that is not crucial. Those three variables 

could have been implemented as global variables instead. Note that it does not sup


port segment variables, or any of the other options implemented in the pat-match 

of chapter 6. 

(let* ((vars (make-array 10 :fill-pointer 0 ladjustable t)) 
(vals (make-array 10 :fill-pointer 0 :adjustable t)) 
(success (cons vars vals))) 

<a id='page-332'></a>

(defun efficient-pat-match (pattern input) 
"Match pattern against input." 
(setf (fill-pointer vars) 0) 
(setf (fill-pointer vals) 0) 
(pat-match-1 pattern input)) 

(defun pat-match-1 (pattern input) 

(cond ((variable-p pattern) (match-var pattern input)) 
((eql pattern input) success) 
((and (consp pattern) (consp input)) 

(and (pat-match-1 (first pattern) (first input)) 
(pat-match-1 (rest pattern) (rest input)))) 
(t fail))) 

(defun match-var (var input) 
"Match a single variable against input." 
(let ((i (position var vars))) 

(cond ((null i) 
(vector-push-extend var vars) 
(vector-push-extend input vals) 
success) 

((equal input (aref vals i)) success) 

(t fail))))) 

An example of its use: 

> (efficient-pat-match '(Tx + ?x = ?y . ?z) 
'(2 + 2 = (3 + 1) is true)) 
(#(?X ?Y 11) . #(2 (3 + 1) (IS TRUE))) 

Extensible vectors with fill pointers are convenient, and much more efficient than 
consing up lists. However, there is some overhead involved in using them, and for 
those sections of code that must be most efficient, it is best to stick with simple 
vectors. The following version of ef f i cient-pat-match explicitly manages the size 
of the vectors and explicitly replaces them with new ones when the size is exceeded: 

(let* ((current-size 0) 
(max-size 1) 
(vars (make-array max-size)) 
(vals (make-array max-size)) 
(success (cons vars vals))) 

(declare (simple-vector vars vals) 
(fixnum current-size max-size)) 

<a id='page-333'></a>

(defun efficient-pat-match (pattern input) 
"Match pattern against input." 
(setf current-size 0) 
(pat-match-1 pattern input)) 

pat-match-1 is unchanged 

(defun match-var (var input) 
"Match a single variable against input." 
(let ((i (position var vars))) 

(cond 
((null i) 
(when (= current-size max-size) 
Make new vectors when we run out of space 

(setf max-size (* 2 max-size) 
vars (replace (make-array max-size) vars) 
vals (replace (make-array max-size) vals) 
success (cons vars vals))) 

;; Store var and its value in vectors 
(setf (aref vars current-size) var) 
(setf (aref vals current-size) input) 
(incf current-size) 
success) 

((equal input (aref vals i)) success) 

(t fail))))) 

In conclusion, replacing lists with vectors can often save garbage. But when you 
must use lists, it pays to use a version of cons that avoids consing when possible. The 
following is such a version: 

(proclaim '(inline reuse-cons)) 

(defun reuse-cons (x y x-y) 
"Return (cons . y), or just x-y if it is equal to (cons . y). " 
(if (and (eql . (car x-y)) (eql y (cdr x-y))) 

x-y 

(cons X y))) 

The trick is based on the definition of subst in Steele's Common Lisp the Language. 
Here is a definition for a version of remove that uses reuse- cons: 

<a id='page-334'></a>

(defun remq (item list) 
"Like REMOVE, but uses EQ, and only works on lists. " 
(cond ((null list) nil) 

((eq item (first list)) (remq item (rest list))) 

(t (reuse-cons (first list) 
(remq item (rest list)) 
list)))) 

Avoid Consing: Unique Lists 

Of course, reuse - cons only works when you have candidate cons cells around. That 
is, (reuse-cons a b c) only saves space when c is (or might be) equal to (cons a b). 
For some applications, it is useful to have a version of cons that returns a unique cons 
cell without needing c as a hint. We will call this version ucons for "unique cons." 
ucons maintains a double hash table: *uni q- cons - tabl e* is a hash table whose keys 
are the cars of cons cells. The value for each car is another hash table whose keys 
are the cdrs of cons cells. The value of each cdr in this second table is the original 
cons cell. So two different cons cells with the same ca r and cdr will retrieve the same 
value. Here is an implementation of ucons: 

(defvar *uniq-cons-table* (make-hash-table :test #'eq)) 

(defun ucons (x y) 
"Return a cons s.t. (eq (ucons . y) (ucons . y)) is true." 
(let ((car-table (or (gethash . *uniq-cons-table*) 

(setf (gethash . *uniq-cons-table*) 
(make-hash-table :test #'eq))))) 
(or (gethash y car-table) 
(setf (gethash y car-table) (cons . y))))) 

ucons, unlike cons, is a true function: it will always return the same value, given 
the same arguments, where "same" is measured by eq. However, if ucons is given 
arguments that are equal but not eq, it will not return a unique result. For that 
we need the function unique. It has the property that (unique x) is eq to (unique 

y) whenever . and y are equal. unique uses a hash table for atoms in addition to 
the double hash table for conses. This is necessary because strings and arrays can 
be equal without being eq. Besides unique, we also define ul ist and uappend for 
convenience. 
(defvar *uniq-atom-table* (make-hash-table .-test #'equal)) 

<a id='page-335'></a>
(defun unique (exp) 
"Return a canonical representation that is EQUAL to exp. 
such that (equal . y) implies (eq (unique x) (unique y))." 
(typecase exp 

(symbol exp) 
(fixnum exp) Remove if fixnums are not eq in your Lisp 
(atom (or (gethash exp *uniq-atom-table*) 

(setf (gethash exp *uniq-atom-table*) exp))) 
(cons (unique-cons (car exp) (cdr exp))))) 

(defun unique-cons (x y) 
"Return a cons s.t. (eq (ucons . y) (uconswhenever (equal . x2) and (equal y y2) are(ucons (unique x) (unique y))) 

(defun ulist (&rest args) 
"A uniquified list." 
(unique args)) 

(defun uappend (x y) 
"A unique list equal to (append . y). " 
(if (null X) 
(unique y) 

 x2 y2)) is true 
true." 

(ucons (first x) (uappend (rest x) y)))) 

The above code works, but it can be improved. The problem is that when uni que is 
applied to a tree, it always traverses the tree all the way to the leaves. The function 
unique-cons is like ucons, except that unique-cons assumes its arguments are not 
yet unique. We can modify uni que- cons so that it first checks to see if its arguments 
are unique, by looking in the appropriate hash tables: 

(defun unique-cons (x y) 
"Return a cons s.t. (eq (ucons . y) (ucons x2 y2)) is true 
whenever (equal . x2) and (equal y y2) are true." 
(let ((ux) (uy)) : unique . and y 

(let ((car-table 

(or (gethash . *uniq-cons-table*) 
(gethash (setf ux (unique x)) *uniq-cons-table*) 
(setf (gethash ux *uniq-cons-table*) 

(make-hash-table :test #*eq))))) 

(or (gethash y car-table) 
(gethash (setf uy (unique y)) car-table) 
(setf (gethash uy car-table) 

(cons ux uy)))))) 

Another advantage of uni que is that it can help in indexing. If lists are unique, 
then they can be stored in an eq hash table instead of a equal hash table. This can 

<a id='page-336'></a>

lead to significant savings v^hen the list structures are large. An eq hash table for 
lists is almost as good as a property list on symbols. 

Avoid Consing: Multiple Values 

Parameters and multiple values can also be used to pass around values, rather than 
building up lists. For example, instead of: 

(defstruct point "A point in 3-D cartesian space." . y z) 

(defun scale-point (k pt) 

"Multiply a point by a constant, K." 

(make-point :x (* k (point-x pt)) 

:y (* k (point-y pt)) 

:z (* k (point-z pt)))) 

one could use the following approach, which doesn't generate structures: 

(defun scale-point (k . y z) 

"Multiply the point (x,y,z) by a constant, K." 

(values (* k x) (* k y) (* k z))) 

Avoid Consing: Resources 

Sometimes it pays to manage explicitly the storage of instances of some data type. A 
pool of these instances may be called a resource. Explicit management of a resource 
is appropriate when: (1) instances are frequently created, and are needed only 
temporarily; (2) it is easy/possible to be sure when instances are no longer needed; 
and (3) instances are fairly large structures or take a long time to initialize, so that it 
is worth reusing them instead of creating new ones. Condition (2) is the crucial one: 
If you deallocate an instance that is still being used, that instance will mysteriously 
be altered when it is reallocated. Conversely, if you fail to deallocate unneeded 
instances, then you are wasting valuable memory space. (The memory management 
scheme is said to leak in this case.) 

The beauty of using Lisp's built-in memory management is that it is guaranteed 
never to leak and never to deallocate structures that are in use. This eliminates two 
potential bug sources. The penalty you pay for this guarantee is some inefficiency of 
the general-purpose memory management as compared to a custom user-supplied 
management scheme. But beware: modern garbage-collection techniques are highly 
optimized. In particular, the so-called generation scavenging or ephemeral garbage 
collectors look more often at recently allocated storage, on the grounds that recently 
made objects are more likely to become garbage. If you hold on to garbage in your 
own data structures, you may end up with worse performance. 

<a id='page-337'></a>

With all these warnings in mind, here is some code to manage resources: 

(defmacro defresource (name &key constructor (initial-copies 0) 
(size (max initial-copies 10))) 

(let ((resource (symbol name '-resource)) 
(deallocate (symbol 'deallocate- name)) 
(allocate (symbol 'allocate- name))) 

'(let ((.resource (make-array .size ifill-pointer 0))) 

(defun .allocate () 
"Get an element from the resource pool, or make one." 
(if (= (fill-pointer .resource) 0) 

.constructor 
(vector-pop .resource))) 

(defun .deallocate (.name) 
"Place a no-longer-needed element back in the pool." 
(vector-push-extend .name .resource)) 

.(if (> initial-copies 0) 
'(mapc #'.deallocate (loop repeat .initial-copies 
collect (.allocate)))) 
'.name))) 

Let's say we had some structure called a buffer which we were constantly making 
instances of and then discarding. Furthermore, suppose that buffers are fairly 
complex objects to build, that we know we'll need at least 10 of them at a time, and 
that we probably won't ever need more than 100 at a time. We might use the buffer 
resource as follows: 

(defresource buffer :constructor (make-buffer) 
:size 100 :initial-copies 10) 

This expands into the following code: 

(let ((buffer-resource (make-array 100 :fil 1-pointer 0))) 

(defun allocate-buffer () 
"Get an element from the resource pool, or make one." 
(if (= (fil1-pointer buffer-resource) 0) 

(make-buffer) 
(vector-pop buffer-resource))) 

(defun deallocate-buffer (buffer) 
"Place a no-longer-needed element back in the pool." 
(vector-push-extend buffer buffer-resource)) 

(mapc #'deanocate-buffer 
(loop repeat 10 collect (allocate-buffer))) 
'buffer) 

<a id='page-338'></a>

We could then use: 

(let ((b (allocate-buffer))) 

(process b) 

(deallocate-buffer b))) 

The important thing to remember is that this works only if the buffer b really can 
be deallocated. If the function process stored away a pointer to b somewhere, 
then it would be a mistake to deallocate b, because a subsequent allocation could 
unpredictably alter the stored buffer. Of course, if process stored a copy of b, then 
everything is alright. This pattern of allocation and deallocation is so common that 
we can provide a macro for it: 

(defmacro with-resource ((var resource &optional protect) &rest body) 
"Execute body with VAR bound to an instance of RESOURCE." 
(let ((allocate (symbol 'allocate- resource)) 

(deallocate (symbol 'deallocate- resource))) 
(if protect 

'(let ((.var nil)) 
(unwind-protect 
(progn (setf ,var (.allocate)) .body) 
(unless (null .var) (.deallocate .var)))) 

'(let ((.var (.allocate))) 
.body 
(.deallocate .var))))) 
The macro allows for an optional argument that sets up an unwi nd - protect environment, 
so that the buffer gets deallocated even when the body is abnormally exited. 
The following expansions should make this clearer: 

> (macroexpand '(with-resource (b buffer) 
"..." (process b) "...")) 
(let ((b (allocate-buffer))) 

. It 

(process b) 

11 II 

(deallocate-buffer b)) 

> (macroexpand '(with-resource (b buffer t) 
"..." (process b) "...")) 
(let ((b nil)) 
(unwind-protect 
(progn (setf b (allocate-buffer)) 

<a id='page-339'></a>
(process b) 
"...") 
(unless (null b) 
(deallocate-buffer b)))) 

An alternative to full resources is to just save a single data object. Such an approach 
is simpler because there is no need to index into a vector of objects, but it is sufficient 
for some applications, such as a tail-recursive function call that only uses one object 
at a time. 

Another possibility is to make the system slower but safer by having the 
deal 1 ocate function check that its argument is indeed an object of the correct type. 

Keep in mind that using resources may put you at odds with the Lisp system's own 
storage management scheme. In particular, you should be concerned with paging 
performance on virtual memory systems. A common problem is to have only a few 
live objects on each page, thus forcing the system to do a lot of paging to get any work 
done. Compacting garbage collectors can collect live objects onto the same page, but 
using resources may interfere with this. 

10.5 Use the Right Data Structures 
It is important to implement key data types with the most efficient implementation. 
This can vary from machine to machine, but there are a few techniques that are 
universal. Here we consider three case studies. 

The Right Data Structure: Variables 

As an example, consider the implementation of pattern-matching variables. We saw 
from the instrumentation of s i mp1 if y that variable-p was one of the most frequently 
used functions. In compiling the matching expressions, I did away with all calls to 
vari abl e-p, but let's suppose we had an application that required run-time use of 
variables. The specification of the data type vari abl e will include two operators, 
the recognizer vari abl e-p, and the constructor make-vari abl e, which gives a new, 
previously unused variable. (This was not needed in the pattern matchers shown so 
far, but will be needed for unification with backward chaining.) One implementation 
of variables is as symbols that begin with the character #\?: 

(defun variable-p (x) 
"Is X a variable (a symbol beginning with *?')?" 
(and (symbolp x) (equal (elt (symbol-name x) 0) #\?))) 

<a id='page-340'></a>

(defun make-variable () "Generate a new variable" (gentemp "?")) 

We could try to speed things up by changing the implementation of variables to be 
keywords and making the functions inline: 

(proclaim '(inline variable-p make-variable)) 
(defun variable-p (x) "Is . a variable?" (keywordp x)) 
(defun make-variable () (gentemp "X" #.(find-package "KEYWORD"))) 

(The reader character sequence #. means to evaluate at read time, rather than at 
execution time.) On my machine, this implementation is pretty fast, and I accepted 
it as a viable compromise. However, other implementations were also considered. 
One was to have variables as structures, and provide a read macro and print function: 

(defstruct (variable (iprint-function print-variable)) name) 

(defvar *vars* (make-hash-table)) 

(set-macro-character #\? 
##'(lambda (stream char) 

Find an old var, or make a new one with the given name 
(declare (ignore char)) 
(let ((name (read stream t nil t))) 

(or (gethash name *vars*) 
(setf (gethash name *vars*) (make-variable mame name)))))) 

(defun print-variable (var stream depth) 
(declare (ignore depth)) 
(format stream "?~a" (var-name var))) 

It turned out that, on all three Lisps tested, structures were slower than keywords 
or symbols. Another alternative is to have the ? read macro return a cons whose 
first is, say, : var. This requires a special output routine to translate back to the ? 
notation. Yet another alternative, which turned out to be the fastest of all, was to 
implement variables as negative integers. Of course, this means that the user cannot 
use negative integers elsewhere in patterns, but that turned out to be acceptable for 
the application at hand. The moral is to know which features are done well in your 
particular implementation and to go out of your way to use them in critical situations, 
but to stick with the most straightforward implementation in noncritical sections. 

Lisp makes it easy to rely on lists, but one must avoid the temptation to overuse 
lists; to use them where another data structure is more appropriate. For example, if 
you need to access elements of a sequence in arbitrary order, then a vector is more 
appropriate than list. If the sequence can grow, use an adjustable vector. Consider 
the problem of maintaining information about a set of people, and searching that set. 
A naive implementation might look like this: 

<a id='page-341'></a>
(defvar *people* nil "Will hold a list of people") 

(defstruct person name address id-number) 

(defun person-with-id (id) 
(find id *people* :key #'person-id-number)) 

In a traditional language like C, the natural solution is to include in the person 
structure a pointer to the next person, and to write a loop to follow these pointers. 
Of course, we can do that in Lisp too: 

(defstruct person name address id-number next) 

(defun person-with-id (id) 

(loop for person = *people* then (person-next person) 

until (null person) 

do (when (eql id (person-id-number person)) 

(RETURN person)))) 

This solution takes less space and is probably faster, because it requires less memory 
accesses: one for each person rather than one for each person plus one for each 
cons cell. So there is a small price to pay for using lists. But Lisp programmers feel 
that price is worth it, because of the convenience and ease of coding and debugging 
afforded by general-purpose functions like f i nd. 

In any case, if there are going to be a large number of people, the list is definitely 
the wrong data structure. Fortunately, Lisp makes it easy to switch to more efficient 
data structures, for example: 

(defun person-with-id (id) 
(gethash id *people*)) 

The Right Data Structure: Queues 

Aqueue is a data structure where one can add elements at the rear and remove them 
from the front. This is almost like a stack, except that in a stack, elements are both 
added and removed at the same end. 

Lists can be used to implement stacks, but there is a problem in using lists to 
implement queues: adding an element to the rear requires traversing the entire list. 
So collecting . elements would be O(n^) instead of 0{n). 

An alternative implementation of queues is as a cons of two pointers: one to the 
list of elements of the queue (the contents), and one to the last cons cell in the list. 
Initially, both pointers would be nil. This implementation in fact existed in BBN Lisp 
and UCI Lisp under the function name tconc: 

<a id='page-342'></a>

;;; A queue is a (contents . last) pair 

(defun tconc (item q) 
"Insert item at the end of the queue." 
(setf (cdr q) 

(if (null (cdr q)) 
(setf (car q) (cons item nil)) 
(setf (rest (cdr q)) 

(cons item nil))))) 

The tconc implementation has the disadvantage that adding the first element to 
the contents is different from adding subsequent elements, so an i f statement is 
required to decide which action to take. The definition of queues given below avoids 
this disadvantage with a clever trick. First, the order of the two fields is reversed. 
The car of the cons cell is the last element, and the cdr is the contents. Second, the 
empty queue is a cons cell where the cdr (the contents field) is nil, and the car (the 
last field) is the cons itself. In the definitions below, we change the name tconc to 
the more standard enqueue, and provide the other queue functions as well: 

;;; A queue is a (last . contents) pair 

(proclaim '(inline queue-contents make-queue enqueue dequeue 
front empty-queue-p queue-nconc)) 

(defun queue-contents (q) (cdr q)) 

(defun make-queue () 
"Build a new queue, with no elements." 
(let ((q (cons nil nil))) 

(setf (car q) q))) 

(defun enqueue (item q) 
"Insert item at the end of the queue." 
(setf (car q) 

(setf (rest (car q)) 
(cons item nil))) 
q) 

(defun dequeue (q) 
"Remove an item from the front of the queue." 
(pop (cdr q)) 

(if (null (cdr q)) (setf (car q) q)) 
q) 

(defun front (q) (first (queue-contents q))) 

(defun empty-queue-p (q) (null (queue-contents q))) 

<a id='page-343'></a>

(defun queue-nconc (q list) 
"Add the elements of LIST to the end of the queue." 
(setf (car q) 

(last (setf (rest (car q)) list)))) 

The Right Data Structure: Tables 

A table is a data structure to which one can insert a key and associate it with a value, 
and later use the key to look up the value. Tables may have other operations, like 
counting the number of keys, clearing out all keys, or mapping a function over each 
key/value pair. 

Lisp provides a wide variety of choices to implement tables. An association list 
is perhaps the simplest: it is just a list of key/value pairs. It is appropriate for small 
tables, up to a few dozen pairs. The hash table is designed to be efficient for large 
tables, but may have significant overhead for small ones. If the keys are symbols, 
property lists can be used. If the keys are integers in a narrow range (or can be 
mapped into them), then a vector may be the most efficient choice. 

Here we implement an alternative data structure, the trie. A trie implements a 
table for keys that are composed of a finite sequence of components. For example, 
if we were implementing a dictionary as a trie, each key would be a word, and 
each letter of the word would be a component. The value of the key would be the 
word's definition. At the top of the dictionary trie is a multiway branch, one for each 
possible first letter. Each second-level node has a branch for every possible second 
letter, and so on. To find an n-letter word requires . reads. This kind of organization 
is especially good when the information is stored on secondary storage, because a 
single read can bring in a node with all its possible branches. 

If the keys can be arbitrary list structures, rather than a simple sequence of letters, 
we need to regularize the keys, transforming them into a simple sequence. One way 
to do that makes use of the fact that any tree can be written as a linear sequence 
of atoms and cons operations, in prefix form. Thus, we would make the following 
transformation: 

(a (b c) d) = 
(cons a (cons (cons b (cons c nil)) (cons d nil))) = 
(cons a cons cons b cons c nil cons d nil) 

In the implementation of tries below, this transformation is done on the fly: The four 
user-level functions are make-trie to create a new trie, put-trie and get-trie to 
add and retrieve key/value pairs, and del ete-tri e to remove them. 

Notice that we use a distinguished value to mark deleted elements, and that 
get-trie returns two values: the actual value found, and a flag saying if anything 

<a id='page-344'></a>

was found or not. This is consistent with the interface to gethash and find, and 
allows us to store null values in the trie. It is an inobtrusive choice, because the 
programmer who decides not to store null values can just ignore the second value, 
and everything will work properly. 

(defstruct trie (value nil) (arcs nil)) 
(defconstant trie-deleted "deleted") 

(defun put-trie (key trie value) 
"Set the value of key in trie." 
(setf (trie-value (find-trie key t trie)) value)) 

(defun get-trie (key trie) 
"Return the value for a key in a trie, and t/nil if found." 
(let* ((key-trie (find-trie key nil trie)) 

(val (if key-trie (trie-value key-trie)))) 

(if (or (null key-trie) (eq val trie-deleted)) 
(values nil nil) 
(values val t)))) 

(defun delete-trie (key trie) 
"Remove a key from a trie." 
(put-trie key trie trie-deleted)) 

(defun find-trie (key extend? trie) 
"Find the trie node for this key. 
If EXTEND? is true, make a new node if need be." 
(cond ((null trie) nil) 

((atom key) 
(follow-arc key extend? trie)) 

(t (find-trie 
(cdr key) extend? 
(find-trie 

(car key) extend? 
(find-trie 
"." extend? trie)))))) 

(defun follow-arc (component extend? trie) 
"Find the trie node for this component of the key. 
If EXTEND? is true, make a new node if need be." 
(let ((arc (assoc component (trie-arcs trie)))) 

(cond ((not (null arc)) (cdr arc)) 
((not extend?) nil) 
(t (let ((new-trie (make-trie))) 

(push (cons component new-trie) 
(trie-arcs trie)) 
new-trie))))) 

<a id='page-345'></a>
There are a few subtleties in the implementation. First, we test for deleted entries 

with an eq comparison to a distinguished marker, the string tri e-de1 eted. No other 

object will be eq to this string except tri e-del eted itself, so this is a good test. We 

also use a distinguished marker, the string ".", to mark cons cells. Components are 

implicitly compared against this marker with an eql test by the assoc in fol 1 ow- arc. 

Maintaining the identity of this string is crucial; if, for example, you recompiled 

the definition of f i nd-tri e (without changing the definition at all), then you could 

no longer find keys that were indexed in an existing trie, because the "." used by 

fi nd-tri e would be a different one from the "." in the existing trie. 

Artificial Intelligence Programming (Charniak et al. 1987) discusses variations on 
the trie, particularly in the indexing scheme. If we always use proper lists (no non-null 
cdrs), then a more efficient encoding is possible. As usual, the best type of indexing 
depends on the data to be indexed. It should be noted that Charniak et al. call the trie 
a discrimination net. In general, that term refers to any tree with tests at the nodes. 

A trie is, of course, a kind of tree, but there are cases where it pays to convert a trie 
into a dag—di directed acyclic graph. A dag is a tree where some of the subtrees are 
shared. Imagine you have a spelUng corrector program with a list of some 50,000 or 
so words. You could put them into a trie, each word with the value t. But there would 
be many subtrees repeated in this trie. For example, given a word list containing look, 
looks, looked, and looking as well as show, shows, showed, and showing, there would 
be repetition of the subtree containing -s, -ed and -ing. After the trie is built, we 
could pass the whole trie to un i que, and it would collapse the shared subtrees, saving 
storage. Of course, you can no longer add or delete keys from the dag without risking 
unintended side effects. 

This process was carried out for a 56,000 word list. The trie took up 3.2Mbytes, 
while the dag was 1.1 Mbytes. This was still deemed unacceptable, so a more compact 
encoding of the dag was created, using a .2Mbytes vector. Encoding the same word 
list in a hash table took twice this space, even with a special format for encoding 
suffixes. 

Tries work best when neither the indexing key nor the retrieval key contains 
variables. They work reasonably well when the variables are near the end of the 
sequence. Consider looking up the pattern "yel 1 o?" in the dictionary, where the " ?" 
character indicates a match of any letter. Following the branches for "yel 1 o" leads 
quickly to the only possible match, "yel 1 ow". In contrast, fetching with the pattern 
" ??11 ow" is much less efficient. The table lookup function would have to search all 
26 top-level branches, and for each of those consider all possible second letters, and 
for each of those consider the path " 11 ow". Quite a bit of searching is required before 
arriving at the complete set of matches: bellow, billow, fallow, fellow, follow, hallow, 
hollow, mallow, mellow, pillow, sallow, tallow, wallow, willow, and yellow. 

We will return to the problem of discrimination nets with variables in section 14.8, 
[page 472](chapter14.md#page-472). 

<a id='page-346'></a>

10.6 Exercises 
&#9635; Exercise 10.1 [h] Define tlie macro deftable, such that (def table person assoc) 
will act much like a def struct—it will define a set of functions for manipulating a 
table of people: get-person, put-person, cl ear-person, and map-person. The table 
should be implemented as an association list. Later on, you can change the representation 
of the table simply by changing the form to (def tabl e person hash), without 
having to change anything else in your code. Other implementation options include 
property lists and vectors, def table should also take three keyword arguments: 
i nl i ne, si ze and test. Here is a possible macroexpansion: 

> (macroexpand '(deftableperson hash .-inline t :size 100)) = 

(progn 
(proclaim '(inline get-person put-person map-person)) 
(defparameter *person-table* 

(make-hash-table :test #'eql :size 100)) 
(defun get-person (x &optional default) 
(gethash . *person-table* default)) 
(defun put-person (x value) 

(setf (gethash . *person-table*) value)) 
(defun clear-person () (clrhash *person-table*)) 
(defun map-person (fn) (maphash fn *person-table*)) 
(defsetf get-person put-person) 
'person) 

&#9635; Exercise 10.2 [m] We can use the : type option to defstruct to define structures 
implemented as lists. However, often we have a two-field structure that we would 
like to implement as a cons cell rather than a two-element list, thereby cutting storage 
in half. Since defstruct does not allow this, define a new macro that does. 

&#9635; Exercise 10.3 [m] Use reuse - cons to write a version of f 1 atten (see [page 329](chapter10.md#page-329)) that 
shares as much of its input with its output as possible. 

&#9635; Exercise 10.4 [h] Consider the data type set. A set has two main operations: adjoin 
an element and test for membership. It is convenient to also add a map-over-elements 
operation. With these primitive operations it is possible to build up more complex 
operations like union and intersection. 
As mentioned in section 3.9, Common Lisp provides several implementations 
of sets. The simplest uses lists as the underlying representation, and provides the 

<a id='page-347'></a>

functions ad j oi ., member, uni on, i ntersecti on, and set-di f f erence. Another uses 
bit vectors, and a similar one uses integers viewed as bit sequences. Analyze the 
time complexity of each implementation for each operation. 

Next, show how sorted lists can be used to implement sets, and compare the 
operations on sorted lists to their counterparts on unsorted lists. 

10.7 Answers 
Answer 10.2 

(defmacro def-cons-struct (cons car cdr &optional inline?) 
"Define aliases for cons, car and cdr." 
'(progn (proclaim '(.(if inline? 'inline 'notinline) 

.car .cdr .cons)) 
(defun .car (x) (car x)) 
(defun .cdr (x) (cdr x)) 
(defsetf .car (x) (val) '(setf (car .x) .val)) 
(defsetf .cdr (x) (val) '(setf (cdr .x) .val)) 
(defun .cons (x y) (cons . y)))) 

Answer 10.3 

(defun flatten (exp &optional (so-far nil) last-cons) 
"Return a flat list of the atoms in the input. 
Ex: (flatten '((a) (b (c) d))) => (a b c d)." 
(cond ((null exp) so-far) 

((atom exp) (reuse-cons exp so-far last-cons)) 

(t (flatten (first exp) 
(flatten (rest exp) so-far exp) 
exp)))) 


