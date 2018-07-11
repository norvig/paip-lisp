# Chapter 9
## Efficiency Issues

> A Lisp programmer knows the value of everything, but the cost of nothing.

> —Alan J.
Perlis

[ ](#){:#p0015}
> Lisp is not inherently less efficient than other high-level languages.

> —Richard J.
Fateman

One of the reasons Lisp has enjoyed a long history is because it is an ideal language for what is now called *rapid-prototyping*–developing a program quickly, with little regards for details.
That is what we have done so far in this book: concentrated on getting a working algorithm.
Unfortunately, when a prototype is to be turned into a production-quality program, details can no longer be ignored.
Most “real” AI programs deal with large amounts of data, and with large search spaces.
Thus, efficiency considerations become very important.

However, this does not mean that writing an efficient program is fundamentaly different from writing a working program.
Ideally, developing an efficient program should be a three-step process.
First, develop a working program, using proper abstractions so that the program will be easy to change if necessary.
Second, *instrument* the program to determine where it is spending most of the time.
Third, replace the slow parts with faster versions, while maintaining the program’s correctness.

The term *efficiency* will be used primarily to talk about the *speed* or run time of a program.
To a lesser extent, *efficiency* is also used to refer to the *space* or amount of storage consumed by a program.
We will also talk about the cost of a program.
This is partly a use of the metaphor “time is money,” and partly rooted in actual monetary costs–if a critical program runs unacceptably slowly, you may need to buy a more expensive computer.

Lisp has been saddled with a reputation as an “inefficient language.” Strictly speaking, it makes no sense to call a *language* efficient or inefficient.
Rather, it is only a particular *implementation* of the language executing a particular program that can be measured for efficiency.
So saying Lisp is inefficient is partly a historical claim: some past implementations *have* been inefficient.
It is also partly a prediction: there are some reasons why future implementations are expected to suffer from inefficiencies.
These reasons mainly stem from Lisp’s flexibility.
Lisp allows many decisions to be delayed until run time, and that can make the run time take longer.
In the past decade, the “efficiency gap” between Lisp and “conventional languages” like FORTRAN or C has narrowed.
Here are the reasons–some deserved, some not–behind Lisp’s reputation for inefficiency:[ ](#){:#p0040}

* [ ](#){:#l0010}• Early implementations were interpreted rather than compiled, which made them inherently inefficient.
Common Lisp implementations have compilers, so this is no longer a problem.
While Lisp is (primarily) no longer an interpreted language, it is still an *interactive* language, so it retains its flexibility.

* • Lisp has often been used to write interpreters for embedded languages, thereby compounding the problem.
Consider this quote from [Cooper and Wogrin’s (1988)](B9780080571157500285.xhtml#bb0260) book on the rule-based programming language OPS5:

> The efficiency of implementations that compile rules into executable code compares favorably to that of programs written in most sequential languages such as FORTRAN or Pascal Implementations that compile rules into data structures to be interpreted, as do many Lisp-based ones, could be noticeably slower.

Here Lisp is guilty by association.
The fallacious chain of reasoning is: Lisp has been used to write interpreters; interpreters are slow; therefore Lisp is slow.
While it is true that Lisp makes it very easy to write interpreters, it also makes it easy to write compilers.
This book is the first that concentrates on using Lisp as both the implementation and target language for compilers.

* • Lisp encourages a style with lots of function calls, particularly recursive calls.
In some older systems, function calls were expensive.
But it is now understood that a function call can be compiled into a simple branch instruction, and that many recursive calls can be made no more expensive than an equivalent iterative loop (see [chapter 22](B9780080571157500224.xhtml)).
It is also possible to instruct a Common Lisp compiler to compile certain functions inline, so there is no calling overhead at all.
On the other hand, many Lisp systems require two fetches instead of one to find the code for a function, and thus will be slower.
This extra level of indirection is the price paid for the freedom of being able to redefine functions without reloading the whole program.

* • Run-time type-checking is slow.
Lisp provides a repertoire of generic functions.
For example, we can write `(+ x y)` without bothering to declare if `x` and `y` are integers, floating point, bignums, complex numbers, rationals, or some combination of the above.
This is very convenient, but it means that type checks must be made at run time, so the generic + will be slower than, say, a 16-bit integer addition with no check for overflow.
If efficiency is important, Common Lisp allows the programmer to include declarations that can eliminate run-time checks.
In fact, once the proper declarations are added, Lisp can be as fast or faster than conventional languages.
[Fateman (1973)](B9780080571157500285.xhtml#bb0375) compared the FORTRAN cube root routine on the PDP-10 to a MacLisp transliteration.
The MacLisp version produced almost identical numerical code, but was 18% faster overall, due to a superior function-calling sequence.[1](#fn0010){:#xfn0010}The epigraph at the beginning of this chapter is from this article.
[Berlin and Weise (1990)](B9780080571157500285.xhtml#bb0085) show that with a special compilation technique called *partial evaluation*, speeds 7 to 90 times faster than conventionally compiled code can be achieved.
Of course, partial evaluation could be used in any language, but it is very easy to do in Lisp.
The fact remains that Lisp objects must somehow represent their type, and even with declarations, not all of this overhead can be eliminated.
Most Lisp implementations optimize access to lists and fixnums but pay the price for the other, less commonly used data types.

* • Lisp automatically manages storage, and so it must periodically stop and collect the unused storage, or *garbage*.
In early systems, this was done by periodically sweeping through all of memory, resulting in an appreciable pause.
Modem systems tend to use incremental garbage-collection techniques, so pauses are shorter and usually unnoticed by the user (although the pauses may still be too long for real-time applications such as controlling a laboratory instrument).
The problem with automatic garbage collection these days is not that it is slow–in fact, the automatic systems do about as well as handcrafted storage allocation.
The problem is that they make it convenient for the programmer to generate a lot of garbage in the first place.
Programmers in conventional languages, who have to clean up their own garbage, tend to be more careful and use static rather than dynamic storage more often.
If garbage becomes a problem, the Lisp programmer can just adopt these static techniques.

* • Lisp systems are big and leave little room for other programs.
Most Lisp systems are designed to be complete environments, within which the programmer does all program development and execution.
For this kind of operation, it makes sense to have a large language like Common Lisp with a huge set of tools.
However, it is becoming more common to use Lisp as just one component in a Computing environment that may include UNIX, X Windows, emacs, and other interacting programs.
In this kind of heterogeneous environment, it would be useful to be able to define and run small Lisp processes that do not include megabytes of unused tools.
Some recent compilers support this option, but it is not widely available yet.

* • Lisp is a complicated high-level language, and it can be difficult for the programmer to anticipate the costs of various operations.
In general, the problem is not that an efficient encoding is impossible but that it is difficult to arrive at that efficient encoding.
In a language like C, the experienced programmer has a pretty good idea how each statement will compile into assembly language instructions.
But in Lisp, very similar statements can compile into widely different assembly-level instructions, depending on subtle interactions between the declarations given and the capabilities of the compiler.
[Page 318](B9780080571157500108.xhtml#p318) gives an example where adding a declaration speeds up a trivial function by 40 times.
Nonexperts do not understand when such declarations are necessary and are frustrated by the seeming inconsistencies.
With experience, the expert Lisp programmer eventually develops a good “efficiency model,” and the need for such declarations becomes obvious.
Recent compilers such as CMU’s Python provide feedback that eases this learning process.

In summary, Lisp makes it possible to write programs in a wide variety of styles, some efficient, some less so.
The programmer who writes Lisp programs in the same style as C programs will probably find Lisp to be of comparable speed, perhaps slightly slower.
The programmer who uses some of the more dynamic features of Lisp typically finds that it is much easier to develop a working program.
Then, if the resulting program is not efficient enough, there will be more time to go back and improve critical sections.
Deciding which parts of the program use the most resources is called *instrumentation*.
It is foolhardy to try to improve the efficiency of a program without first checking if the improvement will make a real difference.

One route to efficiency is to use the Lisp prototype as a specification and reimplement that specification in a lower-level language, such as C or C++.
Some commercial AI vendors are taking this route.
An alternative is to use Lisp as the language for both the prototype and the final implementation.
By adding declarations and making minor changes to the original program, it is possible to end up with a Lisp program that is similar in efficiency to a C program.

There are four very general and language-independent techniques for speeding up an algorithm:

* [ ](#){:#l0015}• *Caching* the results of computations for later reuse.

* • *Compiling* so that less work is done at run time.

* • *Delaying* the computation of partial results that may never be needed.

* • *Indexing* a data structure for quicker retrieval.

This chapter covers each of the four techniques in order.
It then addresses the important problem of *instrumentation*.
The chapter concludes with a case study of the simplify program.
The techniques outlined here result in a 130-fold speed-up in this program.

[Chapter 10](B9780080571157500108.xhtml) concentrates on lower-level “tricks” for improving efficiency further.

## [ ](#){:#st0010}9.1 Caching Results of Previous Computations: Memoization
{:#s0010}
{:.h1hd}

We start with a simple mathematical function to demonstrate the advantages of caching techniques.
Later we will demonstrate more complex examples.

The Fibonacci sequence is defined as the numbers 1,1,2,3,5,8,… where each number is the sum of the two previous numbers.
The most straightforward function to compute the nth number in this sequence is as follows:

[ ](#){:#l0020}`(defun fib (n)`
!!!(p) {:.unnumlist}

  `“Compute the nth number in the Fibonacci sequence.”`
!!!(p) {:.unnumlist}

 `(if (<= n 1) 1`
!!!(p) {:.unnumlist}

   `(+ (fib (− n 1)) (fib (− n 2)))))`
!!!(p) {:.unnumlist}

The problem with this function is that it computes the same thing over and over again.
To compute (`fib 5`) means Computing (`fib 4`) and (`fib 3`), but (`fib 4`) also requires (`fib 3`), they both require (`fib 2`), and so on.
There are ways to rewrite the function to do less computation, but wouldn’t it be nice to write the function as is, and have it automatically avoid redundant computation?
Amazingly, there is a way to do just that.
The idea is to use the function `fib` to build a new function that remembers previously computed results and uses them, rather than recompute them.
This process is called *memoization*.
The function `memo` below is a higher-order function that takes a function as input and returns a new function that will compute the same results, but not do the same computation twice.

[ ](#){:#l0025}`(defun memo (fn)`
!!!(p) {:.unnumlist}

   `“Return a memo-function of fn.”`
!!!(p) {:.unnumlist}

   `(let ((table (make-hash-table)))`
!!!(p) {:.unnumlist}

     `#’(lambda (x)`
!!!(p) {:.unnumlist}

         `(multiple-value-bind (val found-p)`
!!!(p) {:.unnumlist}

           `(gethash x table)`
!!!(p) {:.unnumlist}

       `(if found-p`
!!!(p) {:.unnumlist}

              `val`
!!!(p) {:.unnumlist}

              `(setf (gethash x table) (funcall fn x)))))))`
!!!(p) {:.unnumlist}

The expression (`memo #’fib`) will produce a function that remembers its results between calls, so that, for example, if we apply it to 3 twice, the first call will do the computation of (`fib 3`), but the second will just look up the result in a hash table.
With `fib` traced, it would look like this:

[ ](#){:#l0030}`> (setf memo-fib (memo #’fib)) ⇒ # < CL0SURE − 67300731 >`
!!!(p) {:.unnumlist}

[ ](#){:#l1000}`> (funcall memo-fib 3) ⇒`
!!!(p) {:.unnumlist}

`(1 ENTER FIB: 3)`
!!!(p) {:.unnumlist}

  `(2 ENTER FIB: 2)`
!!!(p) {:.unnumlist}

     `(3 ENTER FIB: 1)`
!!!(p) {:.unnumlist}

     `(3 EXIT FIB: 1)`
!!!(p) {:.unnumlist}

     `(3 ENTER FIB: 0)`
!!!(p) {:.unnumlist}

     `(3 EXIT FIB: 1)`
!!!(p) {:.unnumlist}

  `(2 EXIT FIB: 2)`
!!!(p) {:.unnumlist}

  `(2 ENTER FIB: 1)`
!!!(p) {:.unnumlist}

  `(2 EXIT FIB: 1)`
!!!(p) {:.unnumlist}

`(1 EXIT FIB: 3)`
!!!(p) {:.unnumlist}

`3`
!!!(p) {:.unnumlist}

[ ](#){:#l1005}`> (funcall memo-fib 3) = > 3`
!!!(p) {:.unnumlist}

The second time we call `memo-fib` with 3 as the argument, the answer is just retrieved rather than recomputed.
But the problem is that during the computation of (`fib 3`), we still compute (`fib 2`) multiple times.
It would be better if even the internal, recursive calls were memoized, but they are calls to fib, which is unchanged, not to `memo-fib`.
We can solve this problem easily enough with the function `memoize`:

[ ](#){:#l0035}`(defun memoize (fn-name)`
!!!(p) {:.unnumlist}

   `“Replace fn-name’s global definition with a memoized version.”`
!!!(p) {:.unnumlist}

   `(setf (symbol-function fn-name) (memo (symbol-function fn-name))))`
!!!(p) {:.unnumlist}

When passed a symbol that names a function, `memoize` changes the global definition of the function to a memo-function.
Thus, any recursive calls will go first to the memo-function, rather than to the original function.
This is just what we want.
In the following, we contrast the memoized and unmemoized versions of `fib`.
First, a call to (`fib 5`) with `fib` traced:

[ ](#){:#l0040}`> (fib 5) ⇒`
!!!(p) {:.unnumlist}

`(1 ENTER FIB: 5)`
!!!(p) {:.unnumlist}

   `(2 ENTER FIB: 4)`
!!!(p) {:.unnumlist}

      `(3 ENTER FIB: 3)`
!!!(p) {:.unnumlist}

         `(4 ENTER FIB: 2)`
!!!(p) {:.unnumlist}

             `(5 ENTER FIB: 1)`
!!!(p) {:.unnumlist}

             `(5 EXIT FIB: 1)`
!!!(p) {:.unnumlist}

             `(5 ENTER FIB: 0)`
!!!(p) {:.unnumlist}

             `(5 EXIT FIB: 1)`
!!!(p) {:.unnumlist}

         `(4 EXIT FIB: 2)`
!!!(p) {:.unnumlist}

         `(4 ENTER FIB: 1)`
!!!(p) {:.unnumlist}

         `(4 EXIT FIB: 1)`
!!!(p) {:.unnumlist}

      `(3 EXIT FIB: 3)`
!!!(p) {:.unnumlist}

      `(3 ENTER FIB: 2)`
!!!(p) {:.unnumlist}

         `(4 ENTER FIB: 1)`
!!!(p) {:.unnumlist}

         `(4 EXIT FIB: 1)`
!!!(p) {:.unnumlist}

         `(4 ENTER FIB: 0)`
!!!(p) {:.unnumlist}

         `(4 EXIT FIB: 1)`
!!!(p) {:.unnumlist}

      `(3 EXIT FIB: 2)`
!!!(p) {:.unnumlist}

   `(2 EXIT FIB: 5)`
!!!(p) {:.unnumlist}

   `(2 ENTER FIB: 3)`
!!!(p) {:.unnumlist}

      `(3 ENTER FIB: 2)`
!!!(p) {:.unnumlist}

         `(4 ENTER FIB: 1)`
!!!(p) {:.unnumlist}

         `(4 EXIT FIB: 1)`
!!!(p) {:.unnumlist}

         `(4 ENTER FIB: 0)`
!!!(p) {:.unnumlist}

         `(4 EXIT FIB: 1)`
!!!(p) {:.unnumlist}

      `(3 EXIT FIB: 2)`
!!!(p) {:.unnumlist}

      `(3 ENTER FIB: 1)`
!!!(p) {:.unnumlist}

      `(3 EXIT FIB: 1)`
!!!(p) {:.unnumlist}

   `(2 EXIT FIB: 3)`
!!!(p) {:.unnumlist}

`(1 EXIT FIB: 8)`
!!!(p) {:.unnumlist}

`8`
!!!(p) {:.unnumlist}

We see that (`fib 5`) and (`fib 4`) are each computed once, but (`fib 3`) is computed twice, (`fib 2`) three times,and (`fib 1`) five times.
Below we call (`memoize ’fib`) and repeat the calculation.
This time, each computation is done only once.
Furthermore, when the computation of (`fib 5`) is repeated, the answer is returned immediately with no intermediate computation, and a further call to (`fib 6`) can make use of the value of (`fib 5`).

[ ](#){:#l0045}`> (memoize ’fib) ⇒ # < CL0SURE 76626607 >`
!!!(p) {:.unnumlist}

[ ](#){:#l1050}`> (fib 5) ⇒`
!!!(p) {:.unnumlist}

`(1 ENTER FIB: 5)`
!!!(p) {:.unnumlist}

  `(2 ENTER FIB: 4)`
!!!(p) {:.unnumlist}

     `(3 ENTER FIB: 3)`
!!!(p) {:.unnumlist}

        `(4 ENTER FIB: 2)`
!!!(p) {:.unnumlist}

           `(5 ENTER FIB: 1)`
!!!(p) {:.unnumlist}

           `(5 EXIT FIB: 1)`
!!!(p) {:.unnumlist}

           `(5 ENTER FIB: 0)`
!!!(p) {:.unnumlist}

           `(5 EXIT FIB: 1)`
!!!(p) {:.unnumlist}

        `(4 EXIT FIB: 2)`
!!!(p) {:.unnumlist}

     `(3 EXIT FIB: 3)`
!!!(p) {:.unnumlist}

  `(2 EXIT FIB: 5)`
!!!(p) {:.unnumlist}

`(1 EXIT FIB: 8)`
!!!(p) {:.unnumlist}

`8`
!!!(p) {:.unnumlist}

[ ](#){:#l1100}`> (fib 5)  ⇒ 8`
!!!(p) {:.unnumlist}

[ ](#){:#l1105}`> (fib 6) ⇒`
!!!(p) {:.unnumlist}

`(1 ENTER FIB: 6)`
!!!(p) {:.unnumlist}

`(1 EXIT FIB: 13)`
!!!(p) {:.unnumlist}

`13`
!!!(p) {:.unnumlist}

Understanding why this works requires a clear understanding of the distinction between functions and function names.
The original (`defun fib …`) form does two things: builds a function and stores it as the `symbol - function` value of `fib`.
Within that function there are two references to `fib`; these are compiled (or interpreted) as instructions to fetch the `symbol - function` of `fib` and apply it to the argument.

What `memoize` does is fetch the original function and transform it with `memo` to a function that, when called, will first look in the table to see if the answer is already known.
If not, the original function is called, and a new value is placed in the table.
The trick is that `memoize` takes this new function and makes it the `symbol - function` value of the function name.
This means that all the references in the original function will now go to the new function, and the table will be properly checked on each recursive call.
One further complication to `memo:` the function `gethash` returns both the value found in the table and an indicator of whether the key was present or not.
We use `multiple-value-bind` to capture both values, so that we can distinguish the case when `nil` is the value of the function stored in the table from the case where there is no stored value.

If you make a change to a memoized function, you need to recompile the original definition, and then redo the call to memoize.
In developing your program, rather than saying `(memoize ’f)`, it might be easier to wrap appropriate definitions in a `memoize` form as follows:

[ ](#){:#l0050}`(memoize`
!!!(p) {:.unnumlist}

  `(defun f (x) …)`
!!!(p) {:.unnumlist}

  `)`
!!!(p) {:.unnumlist}

Or define a macro that combines `defun` and `memoize`:

[ ](#){:#l0055}`(defmacro defun-memo (fn args &body body)`
!!!(p) {:.unnumlist}

   `“Define a memoized function.”`
!!!(p) {:.unnumlist}

   `‘(memoize (defun ,fn ,args .
,body)))`
!!!(p) {:.unnumlist}

[ ](#){:#l1110}`(defun-memo f (x) …)`
!!!(p) {:.unnumlist}

Both of these approaches rely on the fact that `defun` returns the name of the function defined.

[ ](#){:#t0010}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| *n* | `(fib *n*)` | unmemoized | memoized | memoized up to |
| 25 | 121393 | 1.1 | .010 | 0 |
| 26 | 196418 | 1.8 | .001 | 25 |
| 27 | 317811 | 2.9 | .001 | 26 |
| 28 | 514229 | 4.7 | .001 | 27 |
| 29 | 832040 | 8.2 | .001 | 28 |
| 30 | 1346269 | 12.4 | .001 | 29 |
| 31 | 2178309 | 20.1 | .001 | 30 |
| 32 | 3524578 | 32.4 | .001 | 31 |
| 33 | 5702887 | 52.5 | .001 | 32 |
| 34 | 9227465 | 81.5 | .001 | 33 |
| 50 | 2.0el0 | – | .014 | 34 |
| 100 | 5.7e20 | – | .031 | 50 |
| 200 | 4.5e41 | – | .096 | 100 |
| 500 | 2.2el04 | – | .270 | 200 |
| 1000 | 7.0e208 | – | .596 | 500 |
| 1000 | 7.0e208 | – | .001 | 1000 |
| 1000 | 7.0e208 | - | .876 | 0 |

![t0010](images/B9780080571157500091/t0010.png)

Now we show a table giving the values of `(fib *n*)` for certain *n*, and the time in seconds to compute the value, before and after `(memoize ’fib)`.
For larger values of *n*, approximations are shown in the table, although `fib` actually returns an exact integer.
With the unmemoized version, I stopped at *n* = 34, because the times were getting too long.
For the memoized version, even *n* = 1000 took under a second.

Note there are three entries for (`fib 1000`).
The first entry represents the incremental computation when the table contains the memoized values up to 500, the second entry shows the time for a table lookup when (`fib 1000`) is already computed, and the third entry is the time for a complete computation starting with an empty table.

It should be noted that there are two general approaches to discussing the efficiency of an algorithm.
One is to time the algorithm on representative inputs, as we did in this table.
The other is to analyze the *asymptotic complexity* of the algorithm.
For the `fib` problem, an asymptotic analysis considers how long it takes to compute `(fib *n*)` as *n* approaches infinity.
The notation *O*(*f*(*n*)) is used to describe the complexity.
For example, the memoized version `fib` is an *O*(*n*) algorithm because the computation time is bounded by some constant times *n*, for any value of *n*.
The unmemoized version, it turns out, is *O*(1.
7*n*), meaning Computing `fib` of n + 1 can take up to 1.7 times as long as `fib` of *n*.
In simpler terms, the memoized version has *linear* complexity, while the unmemoized version has *exponential* complexity.
[Exercise 9.4](B9780080571157500091.xhtml#p4655) ([Page 308](B9780080571157500091.xhtml#p308)) describes where the 1.7 comes from, and gives a tighter bound on the complexity.

The version of `memo` presented above is inflexible in several ways.
First, it only works for functions of one argument.
Second, it only returns a stored value for arguments that are `eql`, because that is how hash tables work by default.
For some applications we want to retrieve the stored value for arguments that are `equal`.
Third, there is no way to delete entries from the hash table.
In many applications there are times when it would be good to clear the hash table, either because it has grown too large or because we have finished a set of related problems and are moving on to a new problem.

The versions of `memo` and `memoize` below handle these three problems.
They are compatible with the previous version but add three new keywords for the extensions.
The `name` keyword stores the hash table on the property list of that name, so it can be accessed by `clear-memoize`.
The `test` keyword tells what kind of hash table to create: `eq, eql, or equal`.
Finally, the `key` keyword tells which arguments of the function to index under.
The default is the first argument (to be compatible with the previous version), but any combination of the arguments can be used.
If you want to use all the arguments, specify `identity` as the key.
Note that if the key is a list of arguments, then you will have to use `equal` hash tables.

[ ](#){:#l0060}`(defun memo (fn name key test)`
!!!(p) {:.unnumlist}

   `“Return a memo-function of fn.”`
!!!(p) {:.unnumlist}

   `(let ((table (make-hash-table :test test)))`
!!!(p) {:.unnumlist}

     `(setf (get name ‘memo) table)`
!!!(p) {:.unnumlist}

     `#’(lambda (&rest args)`
!!!(p) {:.unnumlist}

         `(let ((k (funcall key args)))`
!!!(p) {:.unnumlist}

             `(multiple-value-bind (val found-p)`
!!!(p) {:.unnumlist}

                `(gethash k table)`
!!!(p) {:.unnumlist}

             `(if found-p val`
!!!(p) {:.unnumlist}

                       `(setf (gethash k table) (apply fn args))))))))`
!!!(p) {:.unnumlist}

[ ](#){:#l1200}`(defun memoize (fn-name &key (key #’first) (test #’eql))`
!!!(p) {:.unnumlist}

   `“Replace fn-name’s global definition with a memoized version.”`
!!!(p) {:.unnumlist}

   `(setf (symbol-function fn-name)`
!!!(p) {:.unnumlist}

         `(memo (symbol-function fn-name) fn-name key test)))`
!!!(p) {:.unnumlist}

[ ](#){:#l1205}`(defun clear-memoize (fn-name)`
!!!(p) {:.unnumlist}

   `“Clear the hash table from a memo function.”`
!!!(p) {:.unnumlist}

   `(let ((table (get fn-name ‘memo)))`
!!!(p) {:.unnumlist}

         `(when table (clrhash table))))`
!!!(p) {:.unnumlist}

## [ ](#){:#st0015}9.2 Compiling One Language into Another
{:#s0015}
{:.h1hd}

In [chapter 2](B9780080571157500029.xhtml) we defined a new language–the language of grammar rules–which was processed by an interpreter designed especially for that language.
An *interpreter* is a program that looks at some data structure representing a “program” or sequence of rules of some sort and interprets or evaluates those rules.
This is in contrast to a *compiler*, which translates some set of rules in one language into a program in another language.

The function `generate` was an interpreter for the “language” defined by the set of grammar rules.
Interpreting these rules is straightforward, but the process is some-what inefficient, in that generate must continually search through the `*grammar*` to find the appropriate rule, then count the length of the right-hand side, and so on.

A compiler for this rule-language would take each rule and translate it into a function.
These functions could then call each other with no need to search through the `*grammar*`.
We implement this approach with the function `compile-rule`.
It makes use of the auxiliary functions `one-of` and `rule-lhs` and `rule-rhs` from [Page 40](B9780080571157500029.xhtml#p40), repeated here:

[ ](#){:#l0065}`(defun rule-lhs (rule)`
!!!(p) {:.unnumlist}

 `“The left-hand side of a rule.”`
!!!(p) {:.unnumlist}

 `(first rule))`
!!!(p) {:.unnumlist}

[ ](#){:#l1210}`(defun rule-rhs (rule)`
!!!(p) {:.unnumlist}

 `“The right-hand side of a rule.”`
!!!(p) {:.unnumlist}

 `(rest (rest rule)))`
!!!(p) {:.unnumlist}

[ ](#){:#l1215}`(defun one-of (set)`
!!!(p) {:.unnumlist}

 `“Pick one element of set, and make a list of it.”`
!!!(p) {:.unnumlist}

 `(list (random-elt set)))`
!!!(p) {:.unnumlist}

[ ](#){:#l1220}`(defun random-elt (choices)`
!!!(p) {:.unnumlist}

 `“Choose an element from a list at random.”`
!!!(p) {:.unnumlist}

 `(elt choices (random (length choices))))`
!!!(p) {:.unnumlist}

The function `compile-rule` turns a rule into a function definition by building up Lisp code that implements all the actions that generate would take in interpreting the rule.
There are three cases.
If every element of the right-hand side is an atom, then the rule is a lexical rule, which compiles into a call to `one-of` to pick a word at random.
If there is only one element of the right-hand side, then `build-code` is called to generate code for it.
Usually, this will be a call to append to build up a list.
Finally, if there are several elements in the right-hand side, they are each turned into code by `build-code`; are given a number by `build-cases`; and then a `case` statement is constructed to choose one of the cases.

[ ](#){:#l0070}`(defun compile-rule (rule)`
!!!(p) {:.unnumlist}

 `“Translate a grammar rule into a LISP function definition.”`
!!!(p) {:.unnumlist}

 `(let ((rhs (rule-rhs rule)))`
!!!(p) {:.unnumlist}

   `‘(defun ,(rule-lhs rule) ()`
!!!(p) {:.unnumlist}

`    ,(cond ((every #’atom rhs) ‘(one-of ’,rhs))`
!!!(p) {:.unnumlist}

       `((length =l rhs) (build-code (first rhs)))`
!!!(p) {:.unnumlist}

       `(t ‘(case (random .(length rhs))`
!!!(p) {:.unnumlist}

         `,@(build-cases 0 rhs)))))))`
!!!(p) {:.unnumlist}

[ ](#){:#l2000}`(defun build-cases (number choices)`
!!!(p) {:.unnumlist}

 `“Return a list of case-clauses”`
!!!(p) {:.unnumlist}

 `(when choices`
!!!(p) {:.unnumlist}

   `(cons (list number (build-code (first choices)))`
!!!(p) {:.unnumlist}

       `(build-cases (+ number 1) (rest choices)))))`
!!!(p) {:.unnumlist}

[ ](#){:#l2005}`(defun build-code (choice)`
!!!(p) {:.unnumlist}

 `“Append together multiple constituents”`
!!!(p) {:.unnumlist}

 `(cond ((null choice) nil)`
!!!(p) {:.unnumlist}

       `((atom choice) (list choice))`
!!!(p) {:.unnumlist}

       `((length=1 choice) choice)`
!!!(p) {:.unnumlist}

       `(t ‘(append ,©(mapcar #’build-code choice)))))`
!!!(p) {:.unnumlist}

[ ](#){:#l2010}`(defun length=1 (x)`
!!!(p) {:.unnumlist}

 `“Is X a list of length 1?”`
!!!(p) {:.unnumlist}

 `(and (consp x) (null (rest x))))`
!!!(p) {:.unnumlist}

The Lisp code built by `compile-rule` must be compiled or interpreted to make it available to the Lisp system.
We can do that with one of the following forms.
Normally we would want to call `compile`, but during debugging it may be easier not to.

[ ](#){:#l0075}`(dolist (rule *grammar*) (eval (compile-rule rule)))`
!!!(p) {:.unnumlist}

`(dolist (rule *grammar*) (compile (eval (compile-rule rule))))`
!!!(p) {:.unnumlist}

One frequent way to use compilation is to define a macro that expands into the code generated by the compiler.
That way, we just type in calls to the macro and don’t have to worry about making sure all the latest rules have been compiled.
We might implement this as follows:

[ ](#){:#l0080}`(defmacro defrule (&rest rule)`
!!!(p) {:.unnumlist}

  `“Define a grammar rule”`
!!!(p) {:.unnumlist}

  `(compile-rule rule))`
!!!(p) {:.unnumlist}

[ ](#){:#l3000}`(defrule Sentence -> (NP VP))`
!!!(p) {:.unnumlist}

`(defrule NP -> (Art Noun))`
!!!(p) {:.unnumlist}

`(defrule VP -> (Verb NP))`
!!!(p) {:.unnumlist}

`(defrule Art -> the a)`
!!!(p) {:.unnumlist}

`(defrule Noun -> man bail woman table)`
!!!(p) {:.unnumlist}

`(defrule Verb -> hit took saw liked)`
!!!(p) {:.unnumlist}

Actually, the choice of using one big list of rules (like `*grammar*`) versus using individual macros to define rules is independent of the choice of compiler versus interpreter.
We could just as easily define defrule simply to push the rule onto `*grammar*`.
Macros like `defrule` are useful when you want to define rules in different places, perhaps in several separate files.
The `defparameter` method is appropriate when all the rules can be defined in one place.

We can see the Lisp code generated by `compile-rule` in two ways: by passing it a rule directly:

[ ](#){:#l0085}`> (compile-rule ’(Sentence -> (NP VP)))`
!!!(p) {:.unnumlist}

`(DEFUN SENTENCE ()`
!!!(p) {:.unnumlist}

   `(APPEND (NP) (VP)))`
!!!(p) {:.unnumlist}

[ ](#){:#l3005}`> (compile-rule ’(Noun -> man bail woman table))`
!!!(p) {:.unnumlist}

`(DEFUN NOUN ()`
!!!(p) {:.unnumlist}

   `(ONE-OF ’(MAN BALL WOMAN TABLE)))`
!!!(p) {:.unnumlist}

or by macroexpanding a `defrule` expression.
The compiler was designed to produce the same code we were writing in our first approach to the generation problem (see [Page 35](B9780080571157500029.xhtml#p35)).

[ ](#){:#l0090}`> (macroexpand ’(defrule Adj* -> () Adj (Adj Adj*)))`
!!!(p) {:.unnumlist}

`(DEFUN ADJ* ()`
!!!(p) {:.unnumlist}

 `(CASE (RANDOM 3)`
!!!(p) {:.unnumlist}

   `(0 NIL)`
!!!(p) {:.unnumlist}

   `(1 (ADJ))`
!!!(p) {:.unnumlist}

   `(2 (APPEND (ADJ) (ADJ*)))))`
!!!(p) {:.unnumlist}

Interpreters are usually easier to write than compilers, although in this case, even the compiler was not too difficult.
Interpreters are also inherently more flexible than compilers, because they put off making decisions until the last possible moment.
For example, our compiler considers the right-hand side of a rule to be a list of words only if every element is an atom.
In all other cases, the elements are treated as nonterminals.
This could cause problems if we extended the definition of `Noun` to include the compound noun “chow chow”:

[ ](#){:#l0095}`(defrule Noun -> man ball woman table (chow chow))`
!!!(p) {:.unnumlist}

The rule would expand into the following code:

[ ](#){:#l0100}`(DEFUN NOUN ()`
!!!(p) {:.unnumlist}

` (CASE (RANDOM 5)`
!!!(p) {:.unnumlist}

   `(0 (MAN))`
!!!(p) {:.unnumlist}

   `(1 (BALL))`
!!!(p) {:.unnumlist}

   `(2 (WOMAN))`
!!!(p) {:.unnumlist}

   `(3 (TABLE))`
!!!(p) {:.unnumlist}

   `(4 (APPEND (CHOW) (CHOW)))))`
!!!(p) {:.unnumlist}

The problem is that `man` and `ball` and all the others are suddenly treated as functions, not as literal words.
So we would get a run-time error notifying us of undefined functions.
The equivalent rule would cause no trouble for the interpreter, which waits until it actually needs to generate a symbol to decide if it is a word or a nonterminal.
Thus, the semantics of rules are different for the interpreter and the compiler, and we as program implementors have to be very careful about how we specify the actual meaning of a rule.
In fact, this was probably a bug in the interpreter version, since it effectively prohibits words like “noun” and “sentence” from occurring as words if they are also the names of categories.
One possible resolution of the conflict is to say that an element of a right-hand side represents a word if it is an atom, and a list of categories if it is a list.
If we did indeed settle on that convention, then we could modify both the interpreter and the compiler to comply with the convention.
Another possibility would be to represent words as strings, and categories as symbols.

The flip side of losing run-time flexibility is gaining compile-time diagnostics.
For example, it turns out that on the Common Lisp system I am currently using, I get some useful error messages when I try to compile the buggy version of `Noun:`

[ ](#){:#l0105}`> (defrule Noun -> man bail woman table (chow chow))`
!!!(p) {:.unnumlist}

`The following functions were referenced but don’t seem defined:`
!!!(p) {:.unnumlist}

 `CHOW referenced by NOUN`
!!!(p) {:.unnumlist}

 `TABLE referenced by NOUN`
!!!(p) {:.unnumlist}

 `WOMAN referenced by NOUN`
!!!(p) {:.unnumlist}

 `BALL referenced by NOUN`
!!!(p) {:.unnumlist}

 `MAN referenced by NOUN`
!!!(p) {:.unnumlist}

`NOUN`
!!!(p) {:.unnumlist}

Another problem with the compilation scheme outlined here is the possibility of *name clashes*.
Under the interpretation scheme, the only names used were the function generate and the variable `*grammar*`.
With compilation, every left-hand side of a rule becomes the name of a function.
The grammar writer has to make sure he or she is not using the name of an existing Lisp function, and hence redefining it.
Even worse, if more than one grammar is being developed at the same time, they cannot have any functions in common.
If they do, the user will have to recompile with every switch from one grammar to another.
This may make it difficult to compare grammars.
The best away around this problem is to use the Common Lisp idea of *packages*, but for small exercises name clashes can be avoided easily enough, so we will not explore packages until [section 24.1](B9780080571157500248.xhtml#s0010).

The major advantage of a compiler is speed of execution, when that makes a difference.
For identical grammars running in one particular implementation of Common Lisp on one machine, our interpreter generates about 75 sentences per second, while the compiled approach turns out about 200.
Thus, it is more than twice as fast, but the difference is negligible unless we need to generate many thousands of sentences.
In [section 9.6](#s0035) we will see another compiler with an even greater speed-up.

The need to optimize the code produced by your macros and compilers ultimately depends on the quality of the underlying Lisp compiler.
For example, consider the following code:

[ ](#){:#l0110}`(defun f1 (n l)`
!!!(p) {:.unnumlist}

`   (let ((l1 (first l))`
!!!(p) {:.unnumlist}

`         (l2 (second l)))`
!!!(p) {:.unnumlist}

        `(expt (* 1 (+ n 0))`
!!!(p) {:.unnumlist}

       `(− 4 (length (list l1 l2))))))`
!!!(p) {:.unnumlist}

`F1`
!!!(p) {:.unnumlist}

[ ](#){:#l4000}`> (defun f2 (n l) (* n n)) ⇒F2`
!!!(p) {:.unnumlist}

`> (disassemble ’fl)`
!!!(p) {:.unnumlist}

[ ](#){:#t0015}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `6 PUSH` | `ARGIO ; N` |
| `7 MOVEM` | `PDL-PUSH` |
| `8 *` | `PDL-POP` |
| `9 RETURN` | `PDL-POP` |

`Fl`
!!!(p) {:.unnumlist}

[ ](#){:#l4005}`> (disassemble ’f2)`
!!!(p) {:.unnumlist}

[ ](#){:#t0020}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `6 PUSH` | `ARGO ; N` |
| `7 MOVEM` | `PDL-PUSH` |
| `8 *` | `PDL-POP` |
| `9 RETURN` | `PDL-POP` |

`F2`
!!!(p) {:.unnumlist}

This particular Lisp compiler generates the exact same code for `f1` and `f2`.
Both functions square the argument `n`, and the four machine instructions say, “Take the 0th argument, make a copy of it, multiply those two numbers, and return the result.” It’s clear the compiler has some knowledge of the basic Lisp functions.
In the case of `f1`, it was smart enough to get rid of the local variables `l1` and `l2` (and their initialization), as well as the calls to `first, second, length,` and `list` and most of the arithmetic.
The compiler could do this because it has knowledge about the functions `length` and `list` and the arithmetic functions.
Some of this knowledge might be in the form of simplification rules.

As a user of this compiler, there’s no need for me to write clever macros or compilers that generate streamlined code as seen in `f2`; I can blindly generate code with possible inefficiencies like those in `f1`, and assume that the Lisp compiler will cover up for my laziness.
With another compiler that didn’t know about such optimizations, I would have to be more careful about the code I generate.

## [ ](#){:#st0020}9.3 Delaying Computation
{:#s0020}
{:.h1hd}

Back on [Page 45](B9780080571157500029.xhtml#p45), we saw a program to generate all strings derivable from a grammar.
One drawback of this program was that some grammars produce an infinite number of strings, so the program would not terminate on those grammars.

It turns out that we often want to deal with infinite sets.
Of course, we can’t enumerate all the elements of an infinite set, but we should be able to represent the set and pick elements out one at a time.
In other words, we want to be able to specify how a set (or other object) is constructed, but delay the actual construction, perhaps doing it incrementally over time.
This sounds like a job for closures: we can specify the set constructor as a function, and then call the function some time later.
We will implement this approach with the syntax used in Scheme–the macro `delay` builds a closure to be computed later, and the function `force` calls that function and caches away the value.
We use structures of type `delay` to implement this.
A delay structure has two fields: the value and the function.
Initially, the value field is undefined, and the function field holds the closure that will compute the value.
The first time the delay is forced, the function is called, and its result is stored in the value field.
The function field is then set to nil to indicate that there is no need to call the function again.
The function `force` checks if the function needs to be called, and returns the value.
If `force` is passed an argument that is not a delay, it just returns the argument.

[ ](#){:#l0115}`(defstruct delay (value nil) (function nil))`
!!!(p) {:.unnumlist}

[ ](#){:#l4050}`(defmacro delay (&rest body)`
!!!(p) {:.unnumlist}

 `“A computation that can be executed later by FORCE.”`
!!!(p) {:.unnumlist}

 `‘(make-delay :function #’(lambda () .
,body)))`
!!!(p) {:.unnumlist}

[ ](#){:#l4090}`(defun force (x)`
!!!(p) {:.unnumlist}

 `“Find the value of x, by Computing if it is a delay.”`
!!!(p) {:.unnumlist}

 `(if (not (delay-p x))`
!!!(p) {:.unnumlist}

      `x`
!!!(p) {:.unnumlist}

      `(progn`
!!!(p) {:.unnumlist}

      `(when (delay-function x)`
!!!(p) {:.unnumlist}

         `(setf (delay-value x)`
!!!(p) {:.unnumlist}

             `(funcall (delay-function x)))`
!!!(p) {:.unnumlist}

         `(setf (delay-function x) nil))`
!!!(p) {:.unnumlist}

      `(delay-value x))))`
!!!(p) {:.unnumlist}

Here’s an example of the use of `delay`.
The list `x` is constructed using a combination of normal evaluation and delayed evaluation.
Thus, the `1` is printed when `x` is created, `but` the `2` is not:

[ ](#){:#l0120}`(setf x (list (print 1) (delay (print 2)))) ⇒`
!!!(p) {:.unnumlist}

`1`
!!!(p) {:.unnumlist}

`(1 #S(DELAY .-FUNCTION (LAMBDA () (PRINT 2))))`
!!!(p) {:.unnumlist}

The second element is evaluated (and printed) when it is forced.
But then forcing it again just retrieves the cached value, rather than calling the function again:

[ ](#){:#l0125}`> (force (second x)) ⇒`
!!!(p) {:.unnumlist}

`2`
!!!(p) {:.unnumlist}

`2`
!!!(p) {:.unnumlist}

`> x ⇒ (1 #S(DELAY : VALUE 2))`
!!!(p) {:.unnumlist}

`> (force (second x)) ⇒ 2`
!!!(p) {:.unnumlist}

Now let’s see how delays can be used to build infinite sets.
An infinite set will be considered a special case of what we will call a *pipe*: a list with a `first` component that has been computed, and a `rest` component that is either a normal list or a delayed value.
Pipes have also been called delayed lists, generated lists, and (most commonly) streams.
We will use the term *pipe* because *stream* already has a meaning in Common Lisp.
The book *Artificial Intelligence Programming* ([Charniak et al.
1987](B9780080571157500285.xhtml#bb0180)) also calls these structures pipes, reserving streams for delayed structures that do not cache computed results.

To distinguish pipes from lists, we will use the accessors `head` and `tail` instead of `first` and `rest`.
We will also use `empty-pipe` instead of `nil, make-pipe` instead of `cons`, and `pipe-elt` instead of `elt`.
Note that `make-pipe` is a macro that delays evaluation of the tail.

[ ](#){:#l0130}`(defmacro make-pipe (head tail)`
!!!(p) {:.unnumlist}

 `“Create a pipe by evaluating head and delaying tail.”`
!!!(p) {:.unnumlist}

 `‘(cons ,head (delay ,tail)))`
!!!(p) {:.unnumlist}

[ ](#){:#l4100}`(defconstant empty-pipe nil)`
!!!(p) {:.unnumlist}

[ ](#){:#l4150}`(defun head (pipe) (first pipe))`
!!!(p) {:.unnumlist}

`(defun tail (pipe)(force (rest pipe)))`
!!!(p) {:.unnumlist}

[ ](#){:#l4155}`(defun pipe-elt (pipe i)`
!!!(p) {:.unnumlist}

 `“The i-th element of a pipe, 0-based”`
!!!(p) {:.unnumlist}

 `(if (= i 0)`
!!!(p) {:.unnumlist}

   `(head pipe)`
!!!(p) {:.unnumlist}

   `(pipe-elt (tail pipe) (− i 1))))`
!!!(p) {:.unnumlist}

Here’s a function that can be used to make a large or infinite sequence of integers with delayed evaluation:

[ ](#){:#l0135}`(defun integers (&optional (start 0) end)`
!!!(p) {:.unnumlist}

 `“A pipe of integers from START to END.`
!!!(p) {:.unnumlist}

 `If END is nil, this is an infinite pipe.”`
!!!(p) {:.unnumlist}

 `(if (or (null end) (<= start end))`
!!!(p) {:.unnumlist}

   `(make-pipe start (integers (+ start 1) end))`
!!!(p) {:.unnumlist}

   `nil))`
!!!(p) {:.unnumlist}

And here is an example of its use.
The pipe `c` represents the numbers from 0 to infinity.
When it is created, only the zeroth element, 0, is evaluated.
The computation of the other elements is delayed.

[ ](#){:#l0140}`> (setf c (integers 0)) ⇒ (0 .
#S(DELAY :FUNCTI0N # < CL0SURE − 77435477 >))`
!!!(p) {:.unnumlist}

`> (pipe-elt c 0) ⇒  0`
!!!(p) {:.unnumlist}

Calling `pipe-elt` to look at the third element causes the first through third elements to be evaluated.
The numbers 0 to 3 are cached in the correct positions, and further elements remain unevaluated.
Another call to `pipe-elt` with a larger index would force them by evaluating the delayed function.

[ ](#){:#l0145}`> (pipe-elt c 3) ⇒ 3`
!!!(p) {:.unnumlist}

[ ](#){:#l4355}`c ⇒`
!!!(p) {:.unnumlist}

`(0 .
#S(DELAY`
!!!(p) {:.unnumlist}

        `: VALUE`
!!!(p) {:.unnumlist}

        `(1 .
#S(DELAY`
!!!(p) {:.unnumlist}

                  `: VALUE`
!!!(p) {:.unnumlist}

                  `(2 .
#S(DELAY`
!!!(p) {:.unnumlist}

                          `: VALUE`
!!!(p) {:.unnumlist}

                          `(3 .
#S(DELAY`
!!!(p) {:.unnumlist}

                                   `:FUNCTION`
!!!(p) {:.unnumlist}

                                   `# < CLOSURE − 77432724 >))))))))`
!!!(p) {:.unnumlist}

While this seems to work fine, there is a heavy price to pay.
Every delayed value must be stored in a two-element structure, where one of the elements is a closure.
Thus, there is some storage wasted.
There is also some time wasted, as `tail` or `pipe-elt` must traverse the structures.

An alternate representation for pipes is as (*value.
closure*) pairs, where the closure values are stored into the actual cons cells as they are computed.
Previously we needed structures of type delay to distinguish a delayed from a nondelayed object, but in a pipe we know the rest can be only one of three things: nil, a list, or a delayed value.
Thus, we can use the closures directly instead of using `delay` structures, if we have some way of distinguishing closures from lists.
Compiled closures are atoms, so they can always be distinguished from lists.
But sometimes closures are implemented as lists beginning with `lambda` or some other implementation-dependent symbol.[2](#fn0015){:#xfn0015} The built-in function `functionp` is defined to be true of such lists, as well as of all symbols and all objects returned by `compile`.
But using `functionp` means that we cannot have a pipe that includes the symbol `lambda` as an element, because it will be confused for a closure:

[ ](#){:#l0150}`> (functionp (last ’(theta iota kappa lambda))) ⇒ T`
!!!(p) {:.unnumlist}

If we consistently use compiled functions, then we could eliminate the problem by testing with the built-in predicate `compiled-function-p`.
The following definitions do not make this assumption:

[ ](#){:#l0155}`(defmacro make-pipe (head tai1)`
!!!(p) {:.unnumlist}

 `“Create a pipe by evaluating head and delaying tail.”`
!!!(p) {:.unnumlist}

 `‘(cons ,head #’(lambda () ,tail)))`
!!!(p) {:.unnumlist}

[ ](#){:#l4200}`(defun tail (pipe)`
!!!(p) {:.unnumlist}

 `“Return tail of pipe or list, and destructively update`
!!!(p) {:.unnumlist}

 `the tail if it is a function.”`
!!!(p) {:.unnumlist}

 `(if (functionp (rest pipe))`
!!!(p) {:.unnumlist}

   `(setf (rest pipe) (funcall (rest pipe)))`
!!!(p) {:.unnumlist}

   `(rest pipe)))`
!!!(p) {:.unnumlist}

Everything else remains the same.
If we recompile `integers` (because it uses the `macro make-pipe`), we see the following behavior.
First, creation of the infinite pipe `c` is similar:

[ ](#){:#l0160}`> (setf c (integers 0)) ⇒ (0 .
# < CL0SURE 77350123 >)`
!!!(p) {:.unnumlist}

`> (pipe-elt c 0) ⇒ 0`
!!!(p) {:.unnumlist}

Accessing an element of the pipe forces evaluation of all the intervening elements, and as before leaves subsequent elements unevaluated:

[ ](#){:#l0165}`> (pipe-elt c 5) ⇒ 5`
!!!(p) {:.unnumlist}

`> c ⇒ (0 1 2 3 4 5 .
# < CL0SURE 77351636 >)`
!!!(p) {:.unnumlist}

Pipes can also be used for finite lists.
Here we see a pipe of length 11:

[ ](#){:#l0170}`> (setf i (integers 0 10)) ⇒ (0 .
# < CL0SURE 77375357 >)`
!!!(p) {:.unnumlist}

`> (pipe-elt i 10) ⇒ 10`
!!!(p) {:.unnumlist}

`> (pipe-elt i 11) ⇒ NIL`
!!!(p) {:.unnumlist}

`> i ⇒ (0 1 2 3 4 5 6 7 8 9 10)`
!!!(p) {:.unnumlist}

Clearly, this version wastes less space and is much neater about cleaning up after itself.
In fact, a completely evaluated pipe turns itself into a list!
This efficiency was gained at the sacrifice of a general principle of program design.
Usually we strive to build more complicated abstractions, like pipes, out of simpler ones, like delays.
But in this case, part of the functionality that delays were providing was duplicated by the cons cells that make up pipes, so the more efficient implementation of pipes does not use delays at all.

Here are some more utility functions on pipes:

[ ](#){:#l0175}`(defun enumerate (pipe &key count key (result pipe))`
!!!(p) {:.unnumlist}

 `“Go through all (or count) elements of pipe,`
!!!(p) {:.unnumlist}

 `possibly applying the KEY function.
(Try PRINT.)”`
!!!(p) {:.unnumlist}

 `;; Returns RESULT, which defaults to the pipe itself.`
!!!(p) {:.unnumlist}

 `(if (or (eq pipe empty-pipe) (eql count 0))`
!!!(p) {:.unnumlist}

       `result`
!!!(p) {:.unnumlist}

       `(progn`
!!!(p) {:.unnumlist}

       `(unless (null key) (funcall key (head pipe)))`
!!!(p) {:.unnumlist}

       `(enumerate (tail pipe) :count (if count (− count 1))`
!!!(p) {:.unnumlist}

                        `: key key : result result))))`
!!!(p) {:.unnumlist}

[ ](#){:#l4500}`(defun filter (pred pipe)`
!!!(p) {:.unnumlist}

 `“Keep only items in pipe satisfying pred.”`
!!!(p) {:.unnumlist}

 `(if (funcall pred (head pipe))`
!!!(p) {:.unnumlist}

   `(make-pipe (head pipe)`
!!!(p) {:.unnumlist}

                     `(filter pred (tail pipe)))`
!!!(p) {:.unnumlist}

   `(filter pred (tail pipe))))`
!!!(p) {:.unnumlist}

And here’s an application of pipes: generating prime numbers using the sieve of Eratosthenes algorithm:

[ ](#){:#l0180}`(defun sieve (pipe)`
!!!(p) {:.unnumlist}

 `(make-pipe (head pipe)`
!!!(p) {:.unnumlist}

       `(filter #’(lambda (x) (/= (mod x (head pipe)) 0))`
!!!(p) {:.unnumlist}

                `(sieve (tail pipe)))))`
!!!(p) {:.unnumlist}

[ ](#){:#l4600}`(defvar *primes* (sieve (integers 2)))`
!!!(p) {:.unnumlist}

[ ](#){:#l4605}`> *primes* ⇒ (2 .
# < CL0SURE 3075345 >)`
!!!(p) {:.unnumlist}

[ ](#){:#l4610}`> (enumerate *primes* :count 10) ⇒`
!!!(p) {:.unnumlist}

`(2 3 5 7 11 13 17 19 23 29 31 .
# < CL0SURE 5224472 >)`
!!!(p) {:.unnumlist}

Finally, let’s return to the problem of generating all strings in a grammar.
First we’re going to need some more utility functions:

[ ](#){:#l0185}`(defun map-pipe (fn pipe)`
!!!(p) {:.unnumlist}

 `“Map fn over pipe, delaying all but the first fn call.”`
!!!(p) {:.unnumlist}

 `(if (eq pipe empty-pipe)`
!!!(p) {:.unnumlist}

      `empty-pipe`
!!!(p) {:.unnumlist}

      `(make-pipe (funcall fn (head pipe))`
!!!(p) {:.unnumlist}

              `(map-pipe fn (tail pipe)))))`
!!!(p) {:.unnumlist}

[ ](#){:#l4650}`(defun append-pipes (x y)`
!!!(p) {:.unnumlist}

 `“Return a pipe that appends the elements of x and y.”`
!!!(p) {:.unnumlist}

 `(if (eq x empty-pipe)`
!!!(p) {:.unnumlist}

       `y`
!!!(p) {:.unnumlist}

       `(make-pipe (head x)`
!!!(p) {:.unnumlist}

                  `(append-pipes (tail x) y))))`
!!!(p) {:.unnumlist}

[ ](#){:#l4660}`(defun mappend-pipe (fn pipe)`
!!!(p) {:.unnumlist}

 `“Lazily map fn over pipe, appending results.”`
!!!(p) {:.unnumlist}

 `(if (eq pipe empty-pipe)`
!!!(p) {:.unnumlist}

          `empty-pipe`
!!!(p) {:.unnumlist}

          `(let ((x (funcall fn (head pipe))))`
!!!(p) {:.unnumlist}

            `(make-pipe (head x)`
!!!(p) {:.unnumlist}

                    `(append-pipes (tail x)`
!!!(p) {:.unnumlist}

                                `(mappend-pipe`
!!!(p) {:.unnumlist}

                                            `fn (tail pipe)))))))`
!!!(p) {:.unnumlist}

Now we can rewrite `generate-all` and `combine-all` to use pipes instead of lists.

Everything else is the same as on [Page 45](B9780080571157500029.xhtml#p45).

[ ](#){:#l0190}`(defun generate-all (phrase)`
!!!(p) {:.unnumlist}

 `“Generate a random sentence or phrase”`
!!!(p) {:.unnumlist}

 `(if (listp phrase)`
!!!(p) {:.unnumlist}

   `(if (null phrase)`
!!!(p) {:.unnumlist}

       `(list nil)`
!!!(p) {:.unnumlist}

       `(combine-all-pipes`
!!!(p) {:.unnumlist}

          `(generate-all (first phrase))`
!!!(p) {:.unnumlist}

          `(generate-all (rest phrase))))`
!!!(p) {:.unnumlist}

   `(let ((choices (rule-rhs (assoc phrase *grammar*))))`
!!!(p) {:.unnumlist}

    `(if choices`
!!!(p) {:.unnumlist}

          `(mappend-pipe #’generate-all choices)`
!!!(p) {:.unnumlist}

          `(list (list phrase))))))`
!!!(p) {:.unnumlist}

[ ](#){:#l4690}`(defun combine-all-pipes (xpipe ypipe)`
!!!(p) {:.unnumlist}

 `“Return a pipe of pipes formed by appending a y to an x”`
!!!(p) {:.unnumlist}

 `;; In other words, form the cartesian product.`
!!!(p) {:.unnumlist}

 `(mappend-pipe`
!!!(p) {:.unnumlist}

   `#’(lambda (y)`
!!!(p) {:.unnumlist}

         `(map-pipe #’(lambda (x) (append-pipes x y))`
!!!(p) {:.unnumlist}

                          `xpipe))`
!!!(p) {:.unnumlist}

   `ypipe))`
!!!(p) {:.unnumlist}

With these definitions, here’s the pipe of all sentences from `*grammar2*` (from [Page 43](B9780080571157500029.xhtml#p43)):

[ ](#){:#l0195}`> (setf ss (generate-all ‘sentence)) ⇒`
!!!(p) {:.unnumlist}

`((THE .
# < CL0SURE 27265720 >) .
# < CL0SURE 27266035>)`
!!!(p) {:.unnumlist}

[ ](#){:#l4715}`> (enumerate ss :count 5) ⇒`
!!!(p) {:.unnumlist}

`((THE .
# < CLOSURE 27265720 >)`
!!!(p) {:.unnumlist}

`(A .
# < CLOSURE 27273143 >)`
!!!(p) {:.unnumlist}

`(THE .
# < CLOSURE 27402545 >)`
!!!(p) {:.unnumlist}

`(A .
# < CLOSURE 27404344 >)`
!!!(p) {:.unnumlist}

`(THE .
# < CLOSURE 27404527 >)`
!!!(p) {:.unnumlist}

`(A .
# < CLOSURE 27405473 >) .
# < CLOSURE 27405600 >)`
!!!(p) {:.unnumlist}

[ ](#){:#l4720}`> (enumerate ss .-count 5 :key #’enumerate) ⇒`
!!!(p) {:.unnumlist}

`((THE MAN HIT THE MAN)`
!!!(p) {:.unnumlist}

`(A MAN HIT THE MAN)`
!!!(p) {:.unnumlist}

`(THE BIG MAN HIT THE MAN)`
!!!(p) {:.unnumlist}

`(A BIG MAN HIT THE MAN)`
!!!(p) {:.unnumlist}

`(THE LITTLE MAN HIT THE MAN)`
!!!(p) {:.unnumlist}

`(THE .
# < CLOSURE 27423236 >) .
# < CLOSURE 27423343 >)`
!!!(p) {:.unnumlist}

[ ](#){:#l4725}`> (enumerate (pipe-elt ss 200)) ⇒`
!!!(p) {:.unnumlist}

`(THE ADIABATIC GREEN BLUE MAN HIT THE MAN)`
!!!(p) {:.unnumlist}

While we were able to represent the infinite set of sentences and enumerate instances of it, we still haven’t solved all the problems.
For one, this enumeration will never get to a sentence that does not have “hit the man” as the verb phrase.
We will see longer and longer lists of adjectives, but no other change.
Another problem is that left-recursive rules will still cause infinite loops.
For example, if the expansion for `Adj*` had been `(Adj* -> (Adj* Adj) ())` instead of `(Adj* -> () (Adj Adj*))`, then the enumeration would never terminate, because pipes need to generate a first element.

We have used delays and pipes for two main purposes: to put off until later computations that may not be needed at all, and to have an explicit representation of large or infinite sets.
It should be mentioned that the language Prolog has a different solution to the first problem (but not the second).
As we shall see in [chapter 11](B978008057115750011X.xhtml), Prolog generates solutions one at a time, automatically keeping track of possible backtrack points.
Where pipes allow us to represent an infinite number of alternatives in the data, Prolog allows us to represent those alternatives in the program itself.

**Exercise 9.1 [h]** When given a function `f` and a pipe `p`, `mappend-pipe` returns a new pipe that will eventually enumerate all of `(f (first p))`, then all of `(f (second p))`, and so on.
This is deemed “unfair” if `(f (first p))` has an infinite number of elements.
Define a function that will fairly interleave elements, so that all of them are eventually enumerated.
Show that the function works by changing `generate-all` to work with it.

## [ ](#){:#st0025}9.4 Indexing Data
{:#s0025}
{:.h1hd}

Lisp makes it very easy to use lists as the universal data structure.
A list can represent a set or an ordered sequence, and a list with sublists can represent a tree or graph.
For rapid prototyping, it is often easiest to represent data in lists, but for efficiency this is not always the best idea.
To find an element in a list of length *n* will take *n*/2 steps on average.
This is true for a simple list, an association list, or a property list.
If *n* can be large, it is worth looking at other data structures, such as hash tables, vectors, property lists, and trees.

Picking the right data structure and algorithm is as important in Lisp as it is in any other programming language.
Even though Lisp offers a wide variety of data structures, it is often worthwhile to spend some effort on building just the right data structure for frequently used data.
For example, Lisp’s hash tables are very general and thus can be inefficient.
You may want to build your own hash tables if, for example, you never need to delete elements, thus making open hashing an attractive possibility.
We will see an example of efficient indexing in [section 9.6](#s0035) ([Page 297](B9780080571157500091.xhtml#p297)).

## [ ](#){:#st0030}9.5 Instrumentation: Deciding What to Optimize
{:#s0030}
{:.h1hd}

Because Lisp is such a good rapid-prototyping language, we can expect to get a working implementation quickly.
Before we go about trying to improve the efficiency of the implementation, it is a good idea to see what parts are used most often.
Improving little-used features is a waste of time.

The minimal support we need is to count the number of calls to selected functions, and then print out the totals.
This is called *profiling* the functions.[3](#fn0020){:#xfn0020} For each function to be profiled, we change the definition so that it increments a counter and then calls the original function.

Most Lisp systems have some built-in profiling mechanism.
If your system has one, by all means use it.
The code in this section is provided for those who lack such a feature, and as an example of how functions can be manipulated.
The following is a simple profiling facility.
For each profiled function, it keeps a count of the number of times it is called under the `profile-count` property of the function’s name.

[ ](#){:#l0200}`(defun profile1 (fn-name)`
!!!(p) {:.unnumlist}

 `“Make the function count how often it is called”`
!!!(p) {:.unnumlist}

 `;; First save away the old, unprofiled function`
!!!(p) {:.unnumlist}

 `;; Then make the name be a new function that increments`
!!!(p) {:.unnumlist}

 `;; a counter and then calls the original function`
!!!(p) {:.unnumlist}

  `(let ((fn (symbol-function fn-name)))`
!!!(p) {:.unnumlist}

     `(setf (get fn-name ‘unprofiled-fn) fn)`
!!!(p) {:.unnumlist}

   `(setf (get fn-name ‘profile-count) 0)`
!!!(p) {:.unnumlist}

   `(setf (symbol-function fn-name)`
!!!(p) {:.unnumlist}

        `(profiled-fn fn-name fn))`
!!!(p) {:.unnumlist}

   `fn-name))`
!!!(p) {:.unnumlist}

[ ](#){:#l8000}`(defun unprofile1 (fn-name)`
!!!(p) {:.unnumlist}

 `“Make the function stop counting how often it is called.”`
!!!(p) {:.unnumlist}

 `(setf (symbol-function fn-name) (get fn-name ‘unprofiled-fn))`
!!!(p) {:.unnumlist}

 `fn-name)`
!!!(p) {:.unnumlist}

[ ](#){:#l8005}`(defun profiled-fn (fn-name fn)`
!!!(p) {:.unnumlist}

 `“Return a function that increments the count.”`
!!!(p) {:.unnumlist}

 `#’(lambda (&rest args)`
!!!(p) {:.unnumlist}

   `(incf (get fn-name ‘profile-count))`
!!!(p) {:.unnumlist}

   `(apply fn args)))`
!!!(p) {:.unnumlist}

[ ](#){:#l8010}`(defun profile-count (fn-name) (get fn-name ‘profile-count))`
!!!(p) {:.unnumlist}

 `(defun profile-report (fn-names &optional (key #’profile-count))`
!!!(p) {:.unnumlist}

 `“Report profiling statistics on given functions.”`
!!!(p) {:.unnumlist}

       `(loop for name in (sort fn-names #’> :key key) do`
!!!(p) {:.unnumlist}

`          (format t “~& ~ 7D ~ A” (profile-count name) name)))`
!!!(p) {:.unnumlist}

That’s all we need for the bare-bones functionality.
However, there are a few ways we could improve this.
First, it would be nice to have macros that, like `trace` and `untrace`, allow the user to profile multiple functions at once and keep track of what has been profiled.
Second, it can be helpful to see the length of time spent in each function, as well as the number of calls.

Also, it is important to avoid profiling a function twice, since that would double the number of calls reported without alerting the user of any trouble.
Suppose we entered the following sequence of commands:

[ ](#){:#l0205}`(defun f (x) (g x))`
!!!(p) {:.unnumlist}

`(profile1 ’f)`
!!!(p) {:.unnumlist}

`(profile1 ’f)`
!!!(p) {:.unnumlist}

Then the definition of `f` would be roughly:

[ ](#){:#l8800}`(lambda (&rest args)`
!!!(p) {:.unnumlist}

   `(incf (get ‘f ‘profile-count))`
!!!(p) {:.unnumlist}

   `(apply #’(lambda (&rest args)`
!!!(p) {:.unnumlist}

      `(incf (get ‘f ‘profile-count))`
!!!(p) {:.unnumlist}

      `(apply #’(lambda (x) (g x))`
!!!(p) {:.unnumlist}

            `args))`
!!!(p) {:.unnumlist}

        `args))`
!!!(p) {:.unnumlist}

The result is that any call to `f` will eventually call the original `f`, but only after incrementing the count twice.

Another consideration is what happens when a profiled function is redefined by the user.
The only way we could ensure that a redefined function would continue profiling would be to change the definition of the macro defun to look for functions that should be profiled.
Changing system functions like defun is a risky prospect, and in *Common Lisp the Language*, 2d edition, it is explicitly disallowed.
Instead, we’ll do the next best thing: ensure that the next call to `profile` will reprofile any functions that have been redefined.
We do this by keeping track of both the original unprofiled function and the profiled function.
We also keep a list of all functions that are currently profiled.

In addition, we will count the amount of time spent in each function.
However, the user is cautioned not to trust the timing figures too much.
First, they include the overhead cost of the profiling facility.
This can be significant, particularly because the facility conses, and thus can force garbage collections that would not otherwise have been done.
Second, the resolution of the system clock may not be fine enough to make accurate timings.
For functions that take about 1/10 of a second or more, the figures will be reliable, but for quick functions they may not be.

Here is the basic code for `profile` and `unprofile:`

[ ](#){:#l0210}`(defvar *profiled-functions* nil`
!!!(p) {:.unnumlist}

 `“Function names that are currently profiled”)`
!!!(p) {:.unnumlist}

[ ](#){:#l8805}`(defmacro profile (&rest fn-names)`
!!!(p) {:.unnumlist}

 `“Profile fn-names.
With no args, list profiled functions.”`
!!!(p) {:.unnumlist}

 `‘(mapcar #’profile1`
!!!(p) {:.unnumlist}

       `(setf *profiled-functions*`
!!!(p) {:.unnumlist}

      `(union *profiled-functions* fn-names))))`
!!!(p) {:.unnumlist}

[ ](#){:#l8810}`(defmacro unprofile (&rest fn-names)`
!!!(p) {:.unnumlist}

 `“Stop profiling fn-names.
With no args, stop all profiling.”`
!!!(p) {:.unnumlist}

 `‘(progn`
!!!(p) {:.unnumlist}

   `(mapcar #’unprofile1`
!!!(p) {:.unnumlist}

         `,(if fn-names fn-names ‘*profiled-functions*))`
!!!(p) {:.unnumlist}

   `(setf *profiled-functions*`
!!!(p) {:.unnumlist}

         `,(if (null fn-names)`
!!!(p) {:.unnumlist}

      `nil`
!!!(p) {:.unnumlist}

         `‘(set-difference *profiled-functions*`
!!!(p) {:.unnumlist}

            `’,fn-names)))))`
!!!(p) {:.unnumlist}

The idiom ‘ ’,`fn-names` deserves comment, since it is common but can be confusing at first.
It may be easier to understand when written in the equivalent form ‘`(quote , fn-names)`.
As always, the backquote builds a structure with both constant and evaluated components.
In this case, the `quote` is constant and the variable `fn-names` is evaluated.
In MacLisp, the function `kwote` was defined to serve this purpose:

[ ](#){:#l0215}`(defun kwote (x) (list ’quote x))`
!!!(p) {:.unnumlist}

Now we need to change `profile1` and `unprofile1` to do the additional bookkeeping: For `profile1`, there are two cases.
If the user does a `profile1` on the same function name twice in a row, then on the second time we will notice that the current function is the same as the functioned stored under the `profiled-fn` property, so nothing more needs to be done.
Otherwise, we create the profiled function, store it as the current definition of the name under the `profiled-fn` property, save the unprofiled function, and initialize the counts.

[ ](#){:#l0220}`(defun profile1 (fn-name)`
!!!(p) {:.unnumlist}

 `“Make the function count how often it is called”`
!!!(p) {:.unnumlist}

 `;; First save away the old, unprofiled function`
!!!(p) {:.unnumlist}

 `;; Then make the name be a new function that increments`
!!!(p) {:.unnumlist}

 `;; a counter and then calls the original function`
!!!(p) {:.unnumlist}

 `(let ((fn (symbol-function fn-name)))`
!!!(p) {:.unnumlist}

   `(unless (eq fn (get fn-name ‘profiled-fn))`
!!!(p) {:.unnumlist}

       `(let ((new-fn (profiled-fn fn-name fn)))`
!!!(p) {:.unnumlist}

         `(setf (symbol-function fn-name) new-fn`
!!!(p) {:.unnumlist}

               `(get fn-name ‘profiled-fn) new-fn`
!!!(p) {:.unnumlist}

               `(get fn-name ‘unprofiled-fn) fn`
!!!(p) {:.unnumlist}

               `(get fn-name ‘profile-time) 0`
!!!(p) {:.unnumlist}

               `(get fn-name ‘profile-count) 0))))`
!!!(p) {:.unnumlist}

    `fn-name)`
!!!(p) {:.unnumlist}

[ ](#){:#l8400}`(defun unprofile1 (fn-name)`
!!!(p) {:.unnumlist}

 `“Make the function stop counting how often it is called.”`
!!!(p) {:.unnumlist}

 `(setf (get fn-name ‘profile-time) 0)`
!!!(p) {:.unnumlist}

 `(setf (get fn-name ‘profile-count) 0)`
!!!(p) {:.unnumlist}

 `(when (eq (symbol-function fn-name) (get fn-name ‘profiled-fn))`
!!!(p) {:.unnumlist}

   `;; normal case: restore unprofiled version`
!!!(p) {:.unnumlist}

   `(setf (symbol-function fn-name)`
!!!(p) {:.unnumlist}

        `(get fn-name ‘unprofiled-fn)))`
!!!(p) {:.unnumlist}

 `fn-name)`
!!!(p) {:.unnumlist}

Now we look into the question of timing.
There is a built-in Common Lisp function, `get-internal-real-time`, that returns the elapsed time since the Lisp session started.
Because this can quickly become a bignum, some implementations provide another timing function that wraps around rather than increasing forever, but which may have a higher resolution than `get-internal-real-time`.
For example, on TI Explorer Lisp Machines, `get-internal-real-time` measures 1/60-second intervals, while `time:microsecond-time` measures 1/1,000,000-second intervals, but the value returned wraps around to zero every hour or so.
The function `time:microsecond-time-difference` is used to compare two of these numbers with compensation for wraparound, as long as no more than one wraparound has occurred.

In the code below, I use the conditional read macro characters `#+`and `#-` to define the right behavior on both Explorer and non-Explorer machines.
We have seeen that `#` is a special character to the reader that takes different action depending on the following character.
For example, `#'fn` is read as `(function fn)`.
The character sequence `#+`is defined so that `#+`*feature expression* reads as *expression* if the *feature* is defined in the current implementation, and as nothing at all if it is not.
The sequence `#-` acts in just the opposite way.
For example, on a TI Explorer, we would get the following:

[ ](#){:#l0225}`>‘(hi #+TI t #+Symbolics s #-Explorer e #-Mac m) ⇒ (HI T M)`
!!!(p) {:.unnumlist}

The conditional read macro characters are used in the following definitions:

[ ](#){:#l0230}`(defun get-fast-time ()`
!!!(p) {:.unnumlist}

 `“Return the elapsed time.
This may wrap around;`
!!!(p) {:.unnumlist}

 `use FAST-TIME-DIFFERENCE to compare.”`
!!!(p) {:.unnumlist}

 `#+Explorer (time:microsecond-time) ; do this on an Explorer`
!!!(p) {:.unnumlist}

 `#-Explorer (get-internal-real-time)) ; do this on a non-Explorer`
!!!(p) {:.unnumlist}

[ ](#){:#l8405}`(defun fast-time-difference (end start)`
!!!(p) {:.unnumlist}

 `“Subtract two time points.”`
!!!(p) {:.unnumlist}

 `#+Explorer (time:microsecond-time-difference end start)`
!!!(p) {:.unnumlist}

 `#-Explorer (− end start))`
!!!(p) {:.unnumlist}

[ ](#){:#l8410}`(defun fast-time->seconds (time)`
!!!(p) {:.unnumlist}

 `“Convert a fast-time interval into seconds.”`
!!!(p) {:.unnumlist}

 `#+Explorer (/ time 1000000.0)`
!!!(p) {:.unnumlist}

 `#-Explorer (/ time internal-time-units-per-second))`
!!!(p) {:.unnumlist}

The next step is to update `profiled-fn` to keep track of the timing data.
The simplest way to do this would be to set a variable, say `start`, to the time when a function is entered, run the function, and then increment the function’s time by the difference between the current time and `start`.
The problem with this approach is that every function in the call stack gets credit for the time of each called function.
Suppose the function f calls itself recursively five times, with each call and return taking place a second apart, so that the whole computation takes nine seconds.
Then f will be charged nine seconds for the outer call, seven seconds for the next call, and so on, for a total of 25 seconds, even though in reality it only took nine seconds for all of them together.

A better algorithm would be to charge each function only for the time since the last call or return.
Then `f` would only be charged the nine seconds.
The variable `*profile-call-stack*` is used to hold a stack of function name/entry time pairs.
This stack is manipulated by `profile-enter` and `profile-exit` to get the right timings.

The functions that are used on each call to a profiled function are declared `inline`.
In most cases, a call to a function compiles into machine instructions that set up the argument list and branch to the location of the function’s definition.
With an `inline` function, the body of the function is compiled in line at the place of the function call.
Thus, there is no overhead for setting up the argument list and branching to the definition.
An `inline` declaration can appear anywhere any other declaration can appear.
In this case, the function `proclaim` is used to register a global declaration.
Inline declarations are discussed in more depth on [Page 317](B9780080571157500108.xhtml#p317).

[ ](#){:#l0235}`(proclaim ‘(inline profile-enter profile-exit inc-profile-time))`
!!!(p) {:.unnumlist}

[ ](#){:#l5100}`(defun profiled-fn (fn-name fn)`
!!!(p) {:.unnumlist}

 `“Return a function that increments the count, and times.”`
!!!(p) {:.unnumlist}

 `#’(lambda (&rest args)`
!!!(p) {:.unnumlist}

     `(profile-enter fn-name)`
!!!(p) {:.unnumlist}

     `(multiple-value-progl`
!!!(p) {:.unnumlist}

        `(apply fn args)`
!!!(p) {:.unnumlist}

        `(profile-exit fn-name))))`
!!!(p) {:.unnumlist}

[ ](#){:#l5110}`(defvar *profile-call-stack* nil)`
!!!(p) {:.unnumlist}

[ ](#){:#l5115}`(defun profile-enter (fn-name)`
!!!(p) {:.unnumlist}

 `(incf (get fn-name ‘profile-count))`
!!!(p) {:.unnumlist}

 `(unless (null *profile-call-stack*)`
!!!(p) {:.unnumlist}

   `;; Time charged against the calling function:`
!!!(p) {:.unnumlist}

   `(inc-profile-time (first *profile-call-stack*)`
!!!(p) {:.unnumlist}

               `(car (first *profile-call-stack*))))`
!!!(p) {:.unnumlist}

[ ](#){:#l5151} `;; Put a new entry on the stack`
!!!(p) {:.unnumlist}

 `(push (cons fn-name (get-fast-time))`
!!!(p) {:.unnumlist}

       `*profile-call-stack*))`
!!!(p) {:.unnumlist}

[ ](#){:#l5160}`(defun profile-exit (fn-name)`
!!!(p) {:.unnumlist}

 `;; Time charged against the current function:`
!!!(p) {:.unnumlist}

 `(inc-profile-time (pop *profile-call-stack*)`
!!!(p) {:.unnumlist}

                     `fn-name)`
!!!(p) {:.unnumlist}

 `;; Change the top entry to reflect current time`
!!!(p) {:.unnumlist}

 `(unless (null *profile-call-stack*)`
!!!(p) {:.unnumlist}

   `(setf (cdr (first *profile-call-stack*))`
!!!(p) {:.unnumlist}

       `(get-fast-time))))`
!!!(p) {:.unnumlist}

[ ](#){:#l5775}`(defun inc-profile-time (entry fn-name)`
!!!(p) {:.unnumlist}

 `(incf (get fn-name ‘profile-time)`
!!!(p) {:.unnumlist}

            `(fast-time-difference (get-fast-time) (cdr entry))))`
!!!(p) {:.unnumlist}

Finally, we need to update `profile-report` to print the timing data as well as the counts.
Note that the default `fn-names` is a copy of the global list.
That is because we pass `fn-names` to `sort`, which is a destructive function.
We don’t want the global list to be modified as a result of this sort.

[ ](#){:#l0240}`(defun profile-report (&optional`
!!!(p) {:.unnumlist}

`                      (fn-names (copy-list *profiled-functions*))`
!!!(p) {:.unnumlist}

`                      (key #’profile-count))`
!!!(p) {:.unnumlist}

`  “Report profiling statistics on given functions.”`
!!!(p) {:.unnumlist}

`  (let ((total-time (reduce #’ + (mapcar #’profile-time fn-names))))`
!!!(p) {:.unnumlist}

`    (unless (null key)`
!!!(p) {:.unnumlist}

`      (setf fn-names (sort fn-names #’> :key key)))`
!!!(p) {:.unnumlist}

`    (format t “~&Total elapsed time: ~d seconds.”`
!!!(p) {:.unnumlist}

`            (fast-time-> seconds total-time))`
!!!(p) {:.unnumlist}

`    (format t Count Secs Time% Name")`
!!!(p) {:.unnumlist}

`    (loop for name in fn-names do`
!!!(p) {:.unnumlist}

`         (format t “~&~7D ~6,2F ~3d% ~A”`
!!!(p) {:.unnumlist}

`                (profile-count name)`
!!!(p) {:.unnumlist}

`                (fast-time-> seconds (profile-time name))`
!!!(p) {:.unnumlist}

`                (round (/ (profile-time name) total-time) .01)`
!!!(p) {:.unnumlist}

`                name))))`
!!!(p) {:.unnumlist}

`(defun profile-time (fn-name) (get fn-name ‘profile-time))`
!!!(p) {:.unnumlist}

These functions can be used by calling `profile`, then doing some representative computation, then calling `profile-report`, and finally `unprofile`.
It can be convenient to provide a single macro for doing all of these at once:

[ ](#){:#l0245}`(defmacro with-profiling (fn-names &rest body)`
!!!(p) {:.unnumlist}

` ‘(progn`
!!!(p) {:.unnumlist}

`    (unprofile .
,fn-names)`
!!!(p) {:.unnumlist}

`    (profile .
,fn-names)`
!!!(p) {:.unnumlist}

`    (setf *profile-call-stack* nil)`
!!!(p) {:.unnumlist}

`    (unwind-protect`
!!!(p) {:.unnumlist}

`        (progn .
,body)`
!!!(p) {:.unnumlist}

`      (profile-report ‘,fn-names)`
!!!(p) {:.unnumlist}

`      (unprofile .
,fn-names))))`
!!!(p) {:.unnumlist}

Note the use of `unwind-protect` to produce the report and call `unprofile` even if the computation is aborted.
`unwind-protect` is a special form that takes any number of arguments.
It evaluates the first argument, and if all goes well it then evaluates the other arguments and returns the first one, just like `progl`.
But if an error occurs during the evaluation of the first argument and computation is aborted, then the subsequent arguments (called cleanup forms) are evaluated anyway.

## [ ](#){:#st0035}9.6 A Case Study in Efficiency: The SIMPLIFY Program
{:#s0035}
{:.h1hd}

Suppose we wanted to speed up the `simplify` program of [chapter 8](B978008057115750008X.xhtml).
This section shows how a combination of general techniques–memoizing, indexing, and compiling–can be used to speed up the program by a factor of 130.
[Chapter 15](B9780080571157500157.xhtml) will show another approach: replace the algorithm with an entirely different one.

The first step to a faster program is defining a *benchmark*, a test suite representing a typical work load.
The following is a short list of test problems (and their answers) that are typical of the `simplify` task.

[ ](#){:#l0250}`(defvar *test-data* (mapcar #’infix-> prefix`
!!!(p) {:.unnumlist}

` ‘((d (a * x ^ 2 + b * x + c) / d x)`
!!!(p) {:.unnumlist}

`   (d ((a * x ^ 2 + b * x + c) / x) / d x)`
!!!(p) {:.unnumlist}

`   (d((a*x ^ 3 + b * x ^ 2 + c * x + d)/x ^ 5)/dx)`
!!!(p) {:.unnumlist}

`   ((sin (x + x)) * (sin (2 * x)) + (cos (d (x ^ 2) / d x)) ^ 1)`
!!!(p) {:.unnumlist}

`   (d (3 * x + (cos x) / x) / d x))))`
!!!(p) {:.unnumlist}

`(defvar *answers* (mapcar #’simplify *test-data*))`
!!!(p) {:.unnumlist}

The function `test-it` runs through the test data, making sure that each answer is correct and optionally printing profiling data.

[ ](#){:#l0255}`(defun test-it (&optional (with-profiling t))`
!!!(p) {:.unnumlist}

`  “Time a test run.
and make sure the answers are correct.”`
!!!(p) {:.unnumlist}

`  (let ((answers`
!!!(p) {:.unnumlist}

`         (if with-profiling`
!!!(p) {:.unnumlist}

`             (with-profiling (simplify simplify-exp pat-match`
!!!(p) {:.unnumlist}

`                              match-variable variable-p)`
!!!(p) {:.unnumlist}

`               (mapcar #’simplify *test-data*))`
!!!(p) {:.unnumlist}

`             (time (mapcar #’simplify *test-data*)))))`
!!!(p) {:.unnumlist}

`    (mapc #’assert-equal answers *answers*)`
!!!(p) {:.unnumlist}

`    t))`
!!!(p) {:.unnumlist}

`(defun assert-equal (x y)`
!!!(p) {:.unnumlist}

`  “If x is not equal to y, complain.”`
!!!(p) {:.unnumlist}

`  (assert (equal x y) (x y)`
!!!(p) {:.unnumlist}

`          “Expected ~a to be equal to ~a” x y))`
!!!(p) {:.unnumlist}

Here are the results of (`test-it`) with and without profiling:

[ ](#){:#l0260}`> (test-it nil)`
!!!(p) {:.unnumlist}

`Evaluation of (MAPCAR #’SIMPLIFY *TEST-DATA*) took 6.612 seconds.`
!!!(p) {:.unnumlist}

`> (test-it t)`
!!!(p) {:.unnumlist}

`Total elapsed time: 22.819614 seconds`
!!!(p) {:.unnumlist}

[ ](#){:#t10010}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `Count` | `Secs` | `Time%` | `Name` |
| `51690` | `11.57` | `51%` | `PAT-MATCH` |
| `37908` | `8.75` | `38%` | `VARIABLE-P` |
| `1393` | `0.32` | `1%` | `MATCH-VARIABLE` |
| `906` | `0.20` | `1%` | `SIMPLIFY` |
| `274` | `1.98` | `9%` | `SIMPLIFY-EXP` |

![t10010](images/B9780080571157500091/t10010.png)

Running the test takes 6.6 seconds normally, although the time triples when the profiling overhead is added in.
It should be clear that to speed things up, we have to either speed up or cut down on the number of calls to `pat-match` or `variable-p`, since together they account for 89% of the calls (and 89% of the time as well).
We will look at three methods for achieving both those goals.

#### [ ](#){:#st0040}Memoization
{:#s0045}
{:.h3hd}

Consider the rule that transforms (`x + x`) into (`2 * x`).
Once this is done, we have to simplify the result, which involves resimplifying the components.
If `x` were some complex expression, this could be time-consuming, and it will certainly be wasteful, because `x` is already simplified and cannot change.
We have seen this type of problem before, and the solution is memoization: make `simplify` remember the work it has done, rather than repeating the work.
We can just say:

[ ](#){:#l0265}`(memoize ‘simplify :test #’equal)`
!!!(p) {:.unnumlist}

Two questions are unclear: what kind of hash table to use, and whether we should clear the hash table between problems.
The simplifier was timed for all four combinations of `eq` or `equal` hash tables and resetting or nonresetting between problems.
The fastest result was `equal` hashing and nonresetting.
Note that with `eq` hashing, the resetting version was faster, presumably because it couldn’t take advantage of the common subexpressions between examples (since they aren’t `eq`).

[ ](#){:#t0025}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| hashing | resetting | time |
| none | – | 6.6 |
| equal | yes | 3.8 |
| equal | no | 3.0 |
| eq | yes | 7.0 |
| eq | no | 10.2 |

This approach makes the function `simplify` remember the work it has done, in a hash table.
If the overhead of hash table maintenance becomes too large, there is an alternative: make the data remember what simplify has done.
This approach was taken in MACSYMA !!!(span) {:.smallcaps} : it represented operators as lists rather than as atoms.
Thus, instead of `(* 2 x)`, MACSYMA !!!(span) {:.smallcaps} would use `((*) 2 x)`.
The simplification function would destructively insert a marker into the operator list.
Thus, the result of simplifying 2*x* would be `((* simp) 2 x)`.
Then, when the simplifier was called recursively on this expression, it would notice the `simp` marker and return the expression as is.

The idea of associating memoization information with the data instead of with the function will be more efficient unless there are many functions that all want to place their marks on the same data.
The data-oriented approach has two drawbacks: it doesn’t identify structures that are `equal` but not `eq`, and, because it requires explicitly altering the data, it requires every other operation that manipulates the data to know about the marker s.
The beauty of the hash table approach is that it is transparent; no code needs to know that memoization is taking place.

#### [ ](#){:#st0045}Indexing
{:#s0050}
{:.h3hd}

We currently go through the entire list of rules one at a time, checking each rule.
This is inefficient because most of the rules could be trivially ruled out–if only they were indexed properly.
The simplest indexing scheme would be to have a separate list of rules indexed under each operator.
Instead of having `simplify-exp` check each member of `*simplification-rules*`, it could look only at the smaller list of rules for the appropriate operator.
Here’s how:

[ ](#){:#l0270}`(defun simplify-exp (exp)`
!!!(p) {:.unnumlist}

`  “Simplify using a rule.
or by doing arithmetic.`
!!!(p) {:.unnumlist}

`  or by using the simp function supplied for this operator.`
!!!(p) {:.unnumlist}

`  This version indexes simplification rules under the operator.”`
!!!(p) {:.unnumlist}

`  (cond ((simplify-by-fn exp))`
!!!(p) {:.unnumlist}

`        ((rule-based-translator exp (rules-for (exp-op exp)) ;***`
!!!(p) {:.unnumlist}

`           :rule-if #’exp-lhs :rule-then #’exp-rhs`
!!!(p) {:.unnumlist}

`           :action #’(lambda (bindings response)`
!!!(p) {:.unnumlist}

`                      (simplify (sublis bindings response)))))`
!!!(p) {:.unnumlist}

`        ((evaluable exp) (eval exp))`
!!!(p) {:.unnumlist}

`        (t exp)))`
!!!(p) {:.unnumlist}

`(defvar *rules-for* (make-hash-table :test #’eq))`
!!!(p) {:.unnumlist}

`(defun main-op (rule) (exp-op (exp-lhs rule)))`
!!!(p) {:.unnumlist}

`(defun index-rules (rules)`
!!!(p) {:.unnumlist}

`  “Index all the rules under the main op.”`
!!!(p) {:.unnumlist}

`  (clrhash *rules-for*)`
!!!(p) {:.unnumlist}

`  (dolist (rule rules)`
!!!(p) {:.unnumlist}

`    ;; nconc instead of push to preserve the order of rules`
!!!(p) {:.unnumlist}

`    (setf (gethash (main-op rule) *rules-for*)`
!!!(p) {:.unnumlist}

`          (nconc (gethash (main-op rule) *rules-for*)`
!!!(p) {:.unnumlist}

`                 (list rule)))))`
!!!(p) {:.unnumlist}

`(defun rules-for (op) (gethash op *rules-for*))`
!!!(p) {:.unnumlist}

`(index-rules *simplification-rules*)`
!!!(p) {:.unnumlist}

Timing the memoized, indexed version gets us to .98 seconds, down from 6.6 seconds for the original code and 3 seconds for the memoized code.
If this hadn’t helped, we could have considered more sophisticated indexing schemes.
Instead, we move on to consider other means of gaining efficiency.

**Exercise 9.2 [m]** The list of rules for each operator is stored in a hash table with the operator as key.
An alternative would be to store the rules on the property list of each operator, assuming operators must be symbols.
Implement this alternative, and time it against the hash table approach.
Remember that you need some way of clearing the old rules–trivial with a hash table, but not automatic with property lists.

#### [ ](#){:#st0050}Compilation
{:#s0055}
{:.h3hd}

You can look at `simplify-exp` as an interpreter for the simplification rule language.
One proven technique for improving efficiency is to replace the interpreter with a compiler.
For example, the rule `(x + x = 2 * x)` could be compiled into something like:

[ ](#){:#l0275}`(lambda (exp)`
!!!(p) {:.unnumlist}

`  (if (and (eq (exp-op exp) ‘+) (equal (exp-lhs exp) (exp-rhs exp)))`
!!!(p) {:.unnumlist}

`      (make-exp :op ‘* :lhs 2 :rhs (exp-rhs exp))))`
!!!(p) {:.unnumlist}

This eliminates the need for consing up and passing around variable bindings, and should be faster than the general matching procedure.
When used in conjunction with indexing, the individual rules can be simpler, because we already know we have the right operator.
For example, with the above rule indexed under “+”, it could now be compiled as:

[ ](#){:#l0280}`(lambda (exp)`
!!!(p) {:.unnumlist}

`  (if (equal (exp-lhs exp) (exp-rhs exp))`
!!!(p) {:.unnumlist}

`      (make-exp :op ‘* :lhs 2 :rhs (exp-lhs exp))))`
!!!(p) {:.unnumlist}

It is important to note that when these functions return nil, it means that they have failed to simplify the expression, and we have to consider another means of simplification.

Another possibility is to compile a set of rules all at the same time, so that the indexing is in effect part of the compiled code.
As an example, I show here a small set of rules and a possible compilation of the rule set.
The generated function assumes that `x` is not an atom.
This is appropriate because we are replacing `simplify-exp`, not `simplify`.
Also, we will return nil to indicate that `x` is already simplified.
I have chosen a slightly different format for the code; the main difference is the let to introduce variable names for subexpressions.
This is useful especially for deeply nested patterns.
The other difference is that I explicitly build up the answer with a call to `list`, rather than `make-exp`.
This is normally considered bad style, but since this is code generated by a compiler, I wanted it to be as efficient as possible.
If the representation of the exp data type changed, we could simply change the compiler; a much easier task than hunting down all the references spread throughout a human- written program.
The comments following were not generated by the compiler.

[ ](#){:#l0285}`(x * 1 = x)`
!!!(p) {:.unnumlist}

`(1 * x = x)`
!!!(p) {:.unnumlist}

`(x * 0 = 0)`
!!!(p) {:.unnumlist}

`(0 * x = 0)`
!!!(p) {:.unnumlist}

`(x * x = x ^ 2)`
!!!(p) {:.unnumlist}

`(lambda (x)`
!!!(p) {:.unnumlist}

`  (let ((xl (exp-lhs x))`
!!!(p) {:.unnumlist}

`        (xr (exp-rhs x)))`
!!!(p) {:.unnumlist}

`    (or (if (eql xr ‘1)    ; (x*1 = X)`
!!!(p) {:.unnumlist}

`            xl)`
!!!(p) {:.unnumlist}

`        (if (eql xl ‘1)    ; (1*x = X)`
!!!(p) {:.unnumlist}

`            xr)`
!!!(p) {:.unnumlist}

`        (if (eql xr ‘0)    ; (x*0 = 0)`
!!!(p) {:.unnumlist}

`            ‘0)`
!!!(p) {:.unnumlist}

`        (if (eql xl ‘0)    ; (0*x = 0)`
!!!(p) {:.unnumlist}

`            ‘0)`
!!!(p) {:.unnumlist}

`        (if (equal xr xl)  ; (x*x = x ^ 2)`
!!!(p) {:.unnumlist}

`            (list '^ xl ‘2)))))`
!!!(p) {:.unnumlist}

I chose this format for the code because I imagined (and later *show*) that it would be fairly easy to write the compiler for it.

#### [ ](#){:#st0055}The Single-Rule Compiler
{:#s0060}
{:.h3hd}

Here I show the complete single-rule compiler, to be followed by the indexed-rule-set compiler.
The single-rule compiler works like this:

[ ](#){:#l0290}`> (compile-rule ‘(= (+ x x) (* 2 x)))`
!!!(p) {:.unnumlist}

`(LAMBDA (X)`
!!!(p) {:.unnumlist}

`  (IF (OP?
X ‘+)`
!!!(p) {:.unnumlist}

`    (LET ((XL (EXP-LHS X))`
!!!(p) {:.unnumlist}

`          (XR (EXP-RHS X)))`
!!!(p) {:.unnumlist}

`     (IF (EQUAL XR XL)`
!!!(p) {:.unnumlist}

`         (SIMPLIFY-EXP (LIST ‘* ‘2 XL))))))`
!!!(p) {:.unnumlist}

Given a rule, it generates code that first tests the pattern and then builds the right- hand side of the rule if the pattern matches.
As the code is generated, correspondences are built between variables in the pattern, like `x`, and variables in the generated code, like `xl`.
These are kept in the association list `*bindings*`.
The matching can be broken down into four cases: variables that haven’t been seen before, variables that have been seen before, atoms, and lists.
For example, the first time we run across `x` in the rule above, no test is generated, since anything can match `x`.
But the entry `(x.xl)` is added to the `*bindings*` list to mark the equivalence.
When the second `x` is encountered, the test `(equal xr xl)` is generated.

Organizing the compiler is a little tricky, because we have to do three things at once: return the generated code, keep track of the `*bindings*`, and keep track of what to do “next”–that is, when a test succeeds, we need to generate more code, either to test further, or to build the result.
This code needs to know about the bindings, so it can’t be done *before* the first part of the test, but it also needs to know where it should be placed in the overall code, so it would be messy to do it *after* the first part of the test.
The answer is to pass in a function that will tell us what code to generate later.
This way, it gets done at the right time, and ends up in the right place as well.
Such a function is often called a *continuation*, because it tells us where to continue Computing.
In our compiler, the variable `consequent` is a continuation function.

The compiler is called `compile-rule`.
It takes a rule as an argument and returns a lambda expression that implements the rule.

[ ](#){:#l0295}`(defvar *bindings* nil`
!!!(p) {:.unnumlist}

`  “A list of bindings used by the rule compiler.”)`
!!!(p) {:.unnumlist}

`(defun compile-rule (rule)`
!!!(p) {:.unnumlist}

`  “Compile a single rule.”`
!!!(p) {:.unnumlist}

`  (let ((*bindings* nil))`
!!!(p) {:.unnumlist}

`    ‘(lambda (x)`
!!!(p) {:.unnumlist}

`      ,(compile-exp ‘x (exp-lhs rule) ; x is the lambda parameter`
!!!(p) {:.unnumlist}

`                    (delay (build-exp (exp-rhs rule)`
!!!(p) {:.unnumlist}

`                                               *bindings*))))))`
!!!(p) {:.unnumlist}

All the work is done by `compile-exp`, which takes three arguments: a variable that will represent the input in the generated code, a pattern that the input should be matched against, and a continuation for generating the code if the test passes.
There are five cases: (1) If the pattern is a variable in the list of bindings, then we generate an equality test.
(2) If the pattern is a variable that we have not seen before, then we add it to the binding list, generate no test (because anything matches a variable) and then generate the consequent code.
(3) If the pattern is an atom, then the match succeeds only if the input is `eql` to that atom.
(4) If the pattern is a conditional like `(?is n numberp)`, then we generate the test `(numberp n)`.
Other such patterns could be included here but have not been, since they have not been used.
Finally, (5) if the pattern is a list, we check that it has the right operator and arguments.

[ ](#){:#l0300}`(defun compile-exp (var pattern consequent)`
!!!(p) {:.unnumlist}

`  “Compile code that tests the expression, and does consequent`
!!!(p) {:.unnumlist}

`  if it matches.
Assumes bindings in *bindings*.”`
!!!(p) {:.unnumlist}

`  (cond ((get-binding pattern *bindings*)`
!!!(p) {:.unnumlist}

`         ;; Test a previously bound variable`
!!!(p) {:.unnumlist}

`         ‘(if (equal .var .(lookup pattern *bindings*))`
!!!(p) {:.unnumlist}

 `             ,(force consequent)))`
!!!(p) {:.unnumlist}

`        ((variable-p pattern)`
!!!(p) {:.unnumlist}

`         ;; Add a new bindings; do type checking if needed.`
!!!(p) {:.unnumlist}

`         (push (cons pattern var) *bindings*)`
!!!(p) {:.unnumlist}

`         (force consequent))`
!!!(p) {:.unnumlist}

`        ((atom pattern)`
!!!(p) {:.unnumlist}

`         ;; Match a 1iteral atom`
!!!(p) {:.unnumlist}

`         ‘(if (eql ,var ‘.pattern)`
!!!(p) {:.unnumlist}

`              ,(force consequent)))`
!!!(p) {:.unnumlist}

`        ((starts-with pattern ‘?is)`
!!!(p) {:.unnumlist}

`         (push (cons (second pattern) var) *bindings*)`
!!!(p) {:.unnumlist}

`         ‘(if (,(third pattern) ,var)`
!!!(p) {:.unnumlist}

`              ,(force consequent)))`
!!!(p) {:.unnumlist}

`         ;; So.
far, only the ?is pattern is covered, because`
!!!(p) {:.unnumlist}

`         ;; it is the only one used in simplification rules.`
!!!(p) {:.unnumlist}

`         ;; Other patterns could be compiled by adding code here.`
!!!(p) {:.unnumlist}

`         ;; Or we could switch to a data-driven approach.`
!!!(p) {:.unnumlist}

`         (t ;; Check the operator and arguments`
!!!(p) {:.unnumlist}

`          ‘(if (op?
,var ‘,(exp-op pattern))`
!!!(p) {:.unnumlist}

`              ,(compile-args var pattern consequent)))))`
!!!(p) {:.unnumlist}

The function `compile-args` is used to check the arguments to a pattern.
It generates a `let` form binding one or two new variables (for a unary or binary expression), and then calls `compile-exp` to generate code that actually makes the tests.
It just passes along the continuation, `consequent`, to `compile-exp`.

[ ](#){:#l0305}`(defun compile-args (var pattern consequent)`
!!!(p) {:.unnumlist}

`  “Compile code that checks the arg or args, and does consequent`
!!!(p) {:.unnumlist}

`  if the arg(s) match.”`
!!!(p) {:.unnumlist}

`  ;; First make up variable names for the arg(s).`
!!!(p) {:.unnumlist}

`  (let ((L (symbol var ‘L))`
!!!(p) {:.unnumlist}

`        (R (symbol var ‘R)))`
!!!(p) {:.unnumlist}

`    (if (exp-rhs pattern)`
!!!(p) {:.unnumlist}

`        ;; two arg case`
!!!(p) {:.unnumlist}

`        ‘(let ((,L (exp-lhs ,var))`
!!!(p) {:.unnumlist}

`               (,R (exp-rhs ,var)))`
!!!(p) {:.unnumlist}

`           ,(compile-exp L (exp-lhs pattern)`
!!!(p) {:.unnumlist}

`                         (delay`
!!!(p) {:.unnumlist}

`                           (compile-exp R (exp-rhs pattern)`
!!!(p) {:.unnumlist}

`                                        consequent))))`
!!!(p) {:.unnumlist}

`        ;; one arg case`
!!!(p) {:.unnumlist}

`        ‘(let ((,L (exp-lhs ,var)))`
!!!(p) {:.unnumlist}

`           ,(compile-exp L (exp-lhs pattern) consequent)))))`
!!!(p) {:.unnumlist}

The remaining functions are simpler.
`build-exp` generates code to build the right- hand side of a `rule, op?` tests if its first argument is an expression with a given operator, and `symbol` constructs a new symbol.
Also given is `new-symbol`, although it is not used in this program.

[ ](#){:#l0310}`(defun build-exp (exp bindings)`
!!!(p) {:.unnumlist}

`  “Compile code that will build the exp, given the bindings.”`
!!!(p) {:.unnumlist}

`  (cond ((assoc exp bindings) (rest (assoc exp bindings)))`
!!!(p) {:.unnumlist}

`        ((variable-p exp)`
!!!(p) {:.unnumlist}

`         (error “Variable ~ a occurred on right-hand side,~`
!!!(p) {:.unnumlist}

`                but not left.” exp))`
!!!(p) {:.unnumlist}

`        ((atom exp) “,exp)`
!!!(p) {:.unnumlist}

`        (t (let ((new-exp (mapcar #’(lambda (x)`
!!!(p) {:.unnumlist}

`                                     (build-exp x bindings))`
!!!(p) {:.unnumlist}

`                                   exp)))`
!!!(p) {:.unnumlist}

`             ‘(simplify-exp (list .,new-exp))))))`
!!!(p) {:.unnumlist}

`(defun op?
(exp op)`
!!!(p) {:.unnumlist}

`  “Does the exp have the given op as its operator?”`
!!!(p) {:.unnumlist}

`  (and (exp-p exp) (eq (exp-op exp) op)))`
!!!(p) {:.unnumlist}

`(defun symbol (&rest args)`
!!!(p) {:.unnumlist}

`  “Concatenate symbols or strings to form an interned symbol”`
!!!(p) {:.unnumlist}

`  (intern (format nil “~{~a~}” args)))`
!!!(p) {:.unnumlist}

`(defun new-symbol (&rest args)`
!!!(p) {:.unnumlist}

`  “Concatenate symbols or strings to form an uninterned symbol”`
!!!(p) {:.unnumlist}

`  (make-symbol (format nil “~{~a~}” args)))`
!!!(p) {:.unnumlist}

Here are some examples of the compiler:

[ ](#){:#l0315}`> (compile-rule ’(= (log (^ e x)) x))`
!!!(p) {:.unnumlist}

`(LAMBDA (X)`
!!!(p) {:.unnumlist}

`  (IF (OP?
X ‘LOG)`
!!!(p) {:.unnumlist}

`    (LET ((XL (EXP-LHS X)))`
!!!(p) {:.unnumlist}

`      (IF (OP?
XL ’^`
!!!(p) {:.unnumlist}

`          (LET ((XLL (EXP-LHS XL))`
!!!(p) {:.unnumlist}

`                (XLR (EXP-RHS XL)))`
!!!(p) {:.unnumlist}

`           (IF (EQL XLL ‘E)`
!!!(p) {:.unnumlist}

`                XLR))))))`
!!!(p) {:.unnumlist}

`> (compile-rule (simp-rule ’(n * (m * x) = (n * m) * x)))`
!!!(p) {:.unnumlist}

`(LAMBDA (X)`
!!!(p) {:.unnumlist}

`  (IF (OP?
X ‘*)`
!!!(p) {:.unnumlist}

`    (LET ((XL (EXP-LHS X))`
!!!(p) {:.unnumlist}

`          (XR (EXP-RHS X)))`
!!!(p) {:.unnumlist}

`      (IF (NUMBERP XL)`
!!!(p) {:.unnumlist}

`          (IF (OP?
XR ’*)`
!!!(p) {:.unnumlist}

`            (LET ((XRL (EXP-LHS XR))`
!!!(p) {:.unnumlist}

`                  (XRR (EXP-RHS XR)))`
!!!(p) {:.unnumlist}

`              (IF (NUMBERP XRL)`
!!!(p) {:.unnumlist}

`                (SIMPLIFY-EXP`
!!!(p) {:.unnumlist}

`                  (LIST ’*`
!!!(p) {:.unnumlist}

`                        (SIMPLIFY-EXP (LIST ’* XL XRL))`
!!!(p) {:.unnumlist}

`                        XRR)))))))))`
!!!(p) {:.unnumlist}

#### [ ](#){:#st0060}The Rule-Set Compiler
{:#s0065}
{:.h3hd}

The next step is to combine the code generated by this single-rule compiler to generate more compact code for sets of rules.
We’ll divide up the complete set of rules into subsets based on the main operator (as we did with the `rules-for` function), and generate one big function for each operator.
We need to preserve the order of the rules, so only certain optimizations are possible, but if we make the assumption that no function has side effects (a safe assumption in this application), we can still do pretty well.
We’ll use the `simp-fn` facility to install the one big function for each operator.

The function `compile-rule-set` takes an operator, finds all the rules for that operator, and compiles each rule individually.
(It uses`compile-indexed-rule` rather than `compile-rule`, because it assumes we have already done the indexing for the main operator.) After each rule has been compiled, they are combined with `combine-rules`, which merges similar parts of rules and concatenates the different parts.
The result is wrapped in a `lambda` expression and compiled as the final simplification function for the operator.

[ ](#){:#l0320}`(defun compile-rule-set (op)`
!!!(p) {:.unnumlist}

`  “Compile all rules indexed under a given main op,`
!!!(p) {:.unnumlist}

`  and make them into the simp-fn for that op.”`
!!!(p) {:.unnumlist}

`  (set-simp-fn op`
!!!(p) {:.unnumlist}

`    (compile nil`
!!!(p) {:.unnumlist}

`      ‘(lambda (x)`
!!!(p) {:.unnumlist}

`        ,(reduce #’combine-rules`
!!!(p) {:.unnumlist}

`                 (mapcar #’compile-indexed-rule`
!!!(p) {:.unnumlist}

`                        (rules-for op)))))))`
!!!(p) {:.unnumlist}

[ ](#){:#l0325}`(defun compile-indexed-rule (rule) .`
!!!(p) {:.unnumlist}

`  “Compile one rule into lambda-less code,`
!!!(p) {:.unnumlist}

`  assuming indexing of main op.”`
!!!(p) {:.unnumlist}

`  (let ((*bindings* nil))`
!!!(p) {:.unnumlist}

`    (compile-args`
!!!(p) {:.unnumlist}

`      ‘x (exp-lhs rule)`
!!!(p) {:.unnumlist}

`      (delay (build-exp (exp-rhs rule) *bindings*)))))`
!!!(p) {:.unnumlist}

Here are two examples of what `compile-indexed-rule` generates:

[ ](#){:#l0330}`> (compile-indexed-rule ‘(= (log 1) 0))`
!!!(p) {:.unnumlist}

` (LET ((XL (EXP-LHS X)))`
!!!(p) {:.unnumlist}

`  (IF (EQL XL ‘1)`
!!!(p) {:.unnumlist}

`      ‘0))`
!!!(p) {:.unnumlist}

[ ](#){:#l0335}`> (compile-indexed-rule ‘(= (log (^ e x)) x))`
!!!(p) {:.unnumlist}

` (LET ((XL (EXP-LHS X)))`
!!!(p) {:.unnumlist}

`  (IF (OP?
XL '^)`
!!!(p) {:.unnumlist}

`      (LET ((XLL (EXP-LHS XL))`
!!!(p) {:.unnumlist}

`            (XLR (EXP-RHS XL)))`
!!!(p) {:.unnumlist}

`        (IF (EQL XLL ‘E)`
!!!(p) {:.unnumlist}

`             XLR))))`
!!!(p) {:.unnumlist}

The next step is to combine several of these rules into one.
The function `combine-rules` takes two rules and merges them together as much as possible.

[ ](#){:#l0340}`(defun combine-rules (a b)`
!!!(p) {:.unnumlist}

`  “Combine the code for two rules into one, maintaining order.”`
!!!(p) {:.unnumlist}

`  ;; In the default case, we generate the code (or a b),`
!!!(p) {:.unnumlist}

`  ;; but we try to be cleverer and share common code,`
!!!(p) {:.unnumlist}

`  ;; on the assumption that there are no side-effects.`
!!!(p) {:.unnumlist}

`  (cond ((and (listp a) (listp b)`
!!!(p) {:.unnumlist}

`              (= (length a) (length b) 3)`
!!!(p) {:.unnumlist}

`              (equal (first a) (first b))`
!!!(p) {:.unnumlist}

`              (equal (second a) (second b)))`
!!!(p) {:.unnumlist}

`        ;; a = (f x y), b = (f x z) => (f x (combine-rules y z))`
!!!(p) {:.unnumlist}

`        ;; This can apply when f=IF or f=LET`
!!!(p) {:.unnumlist}

`        (list (first a) (second a)`
!!!(p) {:.unnumlist}

`              (combine-rules (third a) (third b))))`
!!!(p) {:.unnumlist}

`       ((matching-ifs a b)`
!!!(p) {:.unnumlist}

`        (if ,(second a)`
!!!(p) {:.unnumlist}

`            ,(combine-rules (third a) (third b))`
!!!(p) {:.unnumlist}

`            ,(combine-rules (fourth a) (fourth b))))`
!!!(p) {:.unnumlist}

`       ((starts-with a ‘or)`
!!!(p) {:.unnumlist}

`        ;;  a = (or … (if p y)), b = (if p z) ⇒`
!!!(p) {:.unnumlist}

`        ;;       (or … (if p (combine-rules y z)))`
!!!(p) {:.unnumlist}

`        ;; else`
!!!(p) {:.unnumlist}

`        ;;  a = (or …) b = > (or … b)`
!!!(p) {:.unnumlist}

`        (if (matching-ifs (lastl a) b)`
!!!(p) {:.unnumlist}

`            (append (butlast a)`
!!!(p) {:.unnumlist}

`                    (list (combine-rules (lastl a) b)))`
!!!(p) {:.unnumlist}

`            (append a (list b))))`
!!!(p) {:.unnumlist}

`        (t ; ; a.
b = > (or a b)`
!!!(p) {:.unnumlist}

`          ‘(or ,a ,b))))`
!!!(p) {:.unnumlist}

[ ](#){:#l0345}`(defun matching-ifs (a b)`
!!!(p) {:.unnumlist}

`  “Are a and b if statements with the same predicate?”`
!!!(p) {:.unnumlist}

`  (and (starts-with a ‘if) (starts-with b ‘if)`
!!!(p) {:.unnumlist}

`       (equal (second a) (second b))))`
!!!(p) {:.unnumlist}

[ ](#){:#l0350}`(defun lastl (list)`
!!!(p) {:.unnumlist}

`  “Return the last element (not last cons cell) of list”`
!!!(p) {:.unnumlist}

`  (first (last list)))`
!!!(p) {:.unnumlist}

Here is what `combine-rules` does with the two rules generated above:

[ ](#){:#l0355}`> (combine-rules`
!!!(p) {:.unnumlist}

`    ‘(let ((xl (exp-lhs x))) (if (eql xl ’1) ’0))`
!!!(p) {:.unnumlist}

`    ‘(let ((xl (exp-lhs x)))`
!!!(p) {:.unnumlist}

`       (if (op?
xl ’^)`
!!!(p) {:.unnumlist}

`           (let ((xl1 (exp-lhs xl))`
!!!(p) {:.unnumlist}

`                (xlr (exp-rhs xl)))`
!!!(p) {:.unnumlist}

`             (if (eql xll ’e) xlr)))))`
!!!(p) {:.unnumlist}

[ ](#){:#l0360}`(LET ((XL (EXP-LHS X)))`
!!!(p) {:.unnumlist}

`  (OR (IF (EQL XL ‘1) ‘0)`
!!!(p) {:.unnumlist}

`      (IF (OP?
XL ’^)`
!!!(p) {:.unnumlist}

`          (LET ((XLL (EXP-LHS XL))`
!!!(p) {:.unnumlist}

`                (XLR (EXP-RHS XL)))`
!!!(p) {:.unnumlist}

`            (IF (EQL XLL ‘E) XLR)))))`
!!!(p) {:.unnumlist}

Now we run the compiler by calling `compile-all-rules-indexed` and show the combined compiled simplification function for log.
The comments were entered by hand to show what simplification rules are compiled where.

[ ](#){:#l0365}`(defun compile-all-rules-indexed (rules)`
!!!(p) {:.unnumlist}

`  “Compile a separate fn for each operator, and store it`
!!!(p) {:.unnumlist}

`  as the simp-fn of the operator.”`
!!!(p) {:.unnumlist}

`  (index-rules rules)`
!!!(p) {:.unnumlist}

`  (let ((all-ops (delete-duplicates (mapcar #’main-op rules))))`
!!!(p) {:.unnumlist}

`    (mapc #’compile-rule-set ail-ops)))`
!!!(p) {:.unnumlist}

[ ](#){:#l0370}`> (compile-all-rules-indexed *simplification-rules*)`
!!!(p) {:.unnumlist}

`(SIN COS LOG ^ * / - + D)`
!!!(p) {:.unnumlist}

`> (simp-fn ‘log)`
!!!(p) {:.unnumlist}

`(LAMBDA (X)`
!!!(p) {:.unnumlist}

`  (LET ((XL (EXP-LHS X)))`
!!!(p) {:.unnumlist}

`    (OR (IF (EQL XL ‘1)`
!!!(p) {:.unnumlist}

`            ‘0)                    ;*log 1 = 0*`
!!!(p) {:.unnumlist}

`        (IF (EQL XL ‘0)`
!!!(p) {:.unnumlist}

`            ‘UNDEFINED)            ;*log 0 = undefined*`
!!!(p) {:.unnumlist}

`        (IF (EQL XL ‘E)`
!!!(p) {:.unnumlist}

`            ‘1)                    ;*log e = 1*`
!!!(p) {:.unnumlist}

`        (IF (OP?
XL ’^)`
!!!(p) {:.unnumlist}

`            (LET ((XLL (EXP-LHS XL))`
!!!(p) {:.unnumlist}

`                  (XLR (EXP-RHS XL)))`
!!!(p) {:.unnumlist}

`             (IF (EQL XLL ‘E)`
!!!(p) {:.unnumlist}

`                  XLR))))))       ;*log ex = x*`
!!!(p) {:.unnumlist}

If we want to bypass the rule-based simplifier altogether, we can change `simplify-exp` once again to eliminate the check for rules:

[ ](#){:#l0375}`(defun simplify-exp (exp)`
!!!(p) {:.unnumlist}

`  “Simplify by doing arithmetic, or by using the simp function`
!!!(p) {:.unnumlist}

`  supplied for this operator.
Do not use rules of any kind.”`
!!!(p) {:.unnumlist}

`  (cond ((simplify-by-fn exp))`
!!!(p) {:.unnumlist}

`        ((evaluable exp) (eval exp))`
!!!(p) {:.unnumlist}

`        (t exp)))`
!!!(p) {:.unnumlist}

At last, we are in a position to run the benchmark test on the new compiled code; the function `test-it` runs in about .15 seconds with memoization and .05 without.
Why would memoization, which helped before, now hurt us?
Probably because there is a lot of overhead in accessing the hash table, and that overhead is only worth it when there is a lot of other computation to do.

We’ve seen a great improvement since the original code, as the following table summarizes.
Overall, the various efficiency improvements have resulted in a 130- fold speed-up–we can do now in a minute what used to take two hours.
Of course, one must keep in mind that the statistics are only good for this one particular set of test data on this one machine.
It is an open question what performance you will get on other problems and on other machines.

The following table summarizes the execution time and number of function calls on the test data:

[ ](#){:#t0030}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| | original | memo | memo + index | memo + comp | comp |
| run time (secs) | 6.6 | 3.0 | .98 | .15 | .05 |
| speed-up | – | 2 | 7 | 44 | 130 |
| calls |
| pat-match | 51690 | 20003 | 5159 | 0 | 0 |
| variable-p | 37908 | 14694 | 4798 | 0 | 0 |
| match-variable | 1393 | 551 | 551 | 0 | 0 |
| simplify | 906 | 408 | 408 | 545 | 906 |
| simplify-exp | 274 | 118 | 118 | 118 | 274 |

![t0030](images/B9780080571157500091/t0030.png)

## [ ](#){:#st0065}9.7 History and References
{:#s0070}
{:.h1hd}

The idea of memoization was introduced by Donald Michie 1968.
He proposed using a list of values rather than a hash table, so the savings was not as great.
In mathematics, the field of dynamic programming is really just the study of how to compute values in the proper order so that partial results will already be cached away when needed.

A large part of academic computer science covers compilation; [Aho and Ullman 1972](B9780080571157500285.xhtml#bb0015) is just one example.
The technique of compiling embedded languages (such as the language of pattern-matching rules) is one that has achieved much more attention in the Lisp community than in the rest of computer science.
See [Emanuelson and Haraldsson 1980](B9780080571157500285.xhtml#bb0365), for an example.

Choosing the right data structure, indexing it properly, and defining algorithms to operate on it is another important branch of computer science; [Sedgewick 1988](B9780080571157500285.xhtml#bb1065) is one example, but there are many worthy texts.

Delaying computation by packaging it up in a `lambda` expression is an idea that goes back to Algol’s use of *thunks*–a mechanism to implement call-by-name parameters, essentially by passing functions of no arguments.
The name *thunk* comes from the fact that these functions can be compiled: the system does not have to think about them at run time, because the compiler has already thunk about them.
Peter [Ingerman 1961](B9780080571157500285.xhtml#bb0570) describes thunks in detail.
[Abelson and Sussman 1985](B9780080571157500285.xhtml#bb0010) cover delays nicely.
The idea of eliminating unneeded computation is so attractive that entire languages have built around the concept of *lazy evaluation*–don’t evaluate an expression until its value is needed.
See [Hughes 1985](B9780080571157500285.xhtml#bb0565) or [Field and Harrison 1988](B9780080571157500285.xhtml#bb0400).

## [ ](#){:#st0070}9.8 Exercises
{:#s0075}
{:.h1hd}

**Exercise 9.3 [d]** In this chapter we presented a compiler for `simplify`.
It is not too much harder to extend this compiler to handle the full power of `pat-match`.
Instead of looking at expressions only, allow trees with variables in any position.
Extend and generalize the definitions of `compile-rule` and `compile-rule-set` so that they can be used as a general tool for any application program that uses `pat-match` and/or `rule-based-translator`.
Make sure that the compiler is data-driven, so that the programmer who adds a new kind of pattern to `pat-match` can also instruct the compiler how to deal with it.
One hard part will be accounting for segment variables.
It is worth spending a considerable amount of effort at compile time to make this efficient at run time.

**Exercise 9.4 [m]** Define the time to compute (fib n) without memoization as *Tn*.
Write a formula to express *Tn*.
Given that *T*25 ≈ 1.1 seconds, predict *T*100.

**Exercise 9.5 [m]** Consider a version of the game of Nim played as follows: there is a pile of *n* tokens.
Two players alternate removing tokens from the pile; on each turn a player must take either one, two, or three tokens.
Whoever takes the last token wins.
Write a program that, given *n*, returns the number of tokens to take to insure a win, if possible.
Analyze the execution times for your program, with and without memoization.

**Exercise 9.6 [m]** A more complicated Nim-like game is known as Grundy’s game.
The game starts with a single pile of *n* tokens.
Each player must choose one pile and split it into two uneven piles.
The first player to be unable to move loses.
Write a program to play Grundy’s game, and see how memoization helps.

**Exercise 9.7 [h]** This exercise describes a more challenging one-person game.
In this game the player rolls a six-sided die eight times.
The player forms four two-digit decimal numbers such that the total of the four numbers is as high as possible, but not higher than 170.
A total of 171 or more gets scored as zero.

The game would be deterministic and completely boring if not for the requirement that after each roll the player must immediately place the digit in either the ones or tens column of one of the four numbers.

Here is a sample game.
The player first rolls a 3 and places it in the ones column of the first number, then rolls a 4 and places it in the tens column, and so on.
On the last roll the player rolls a 6 and ends up with a total of 180.
Since this is over the limit of 170, the player’s final score is 0.

[ ](#){:#t0035}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| roll | 3 | 4 | 6 | 6 | 3 | 5 | 3 | 6 |
| lst num. | -3 | 43 | 43 | 43 | 43 | 43 | 43 | 43 |
| 2nd num. | – | – | -6 | -6 | 36 | 36 | 36 | 36 |
| 3rd num. | – | – | – | -6 | -6 | -6 | 36 | 36 |
| 4th num. | – | – | – | – | – | -5 | -5 | 65 |
| total | 03 | 43 | 49 | 55 | 85 | 90 | 120 | 0 |

![t0035](images/B9780080571157500091/t0035.png)

Write a function that allows you to play a game or a series of games.
The function should take as argument a function representing a strategy for playing the game.

**Exercise 9.8 [h]** Define a good strategy for the dice game described above.
(Hint: my strategy scores an average of 143.7.)

**Exercise 9.9 [m]** One problem with playing games involving random numbers is the possibility that a player can cheat by figuring out what `random` is going to do next.
Read the definition of the function `random` and describe how a player could cheat.
Then describe a countermeasure.

**Exercise 9.10 [m]** On [Page 292](B9780080571157500091.xhtml#p292) we saw the use of the read-time conditionals, #+and #-, where #+is the read-time equivalent of when, and #- is the read-time equivalent of unless.
Unfortunately, there is no read-time equivalent of case.
Implement one.

**Exercise 9.11 [h]** Write a compiler for ELIZA !!!(span) {:.smallcaps} that compiles all the rules at once into a single function.
How much more efficient is the compiled version?

**Exercise 9.12 [d]** Write some rules to simplify Lisp code.
Some of the algebraic simplification rules will still be valid, but new ones will be needed to simplify nonalgebraic functions and special forms.
(Since `nil` is a valid expression in this domain, you will have to deal with the semipredicate problem.) Here are some example rules (using prefix notation):

[ ](#){:#l0380}`(= (+ x 0) x)`
!!!(p) {:.unnumlist}

`(= ‘nil nil) (`
!!!(p) {:.unnumlist}

`(= (car (cons x y)) x)`
!!!(p) {:.unnumlist}

`(= (cdr (cons x y)) y)`
!!!(p) {:.unnumlist}

`(= (if t x y) x)`
!!!(p) {:.unnumlist}

`(= (if nil x y) y)`
!!!(p) {:.unnumlist}

`(= (length nil) 0)`
!!!(p) {:.unnumlist}

`(= (expt y (?if x numberp)) (expt (expt y (/ x 2)) 2))`
!!!(p) {:.unnumlist}

**Exercise 9.13 [m]** Consider the following two versions of the sieve of Eratosthenes algorithm.
The second explicitly binds a local variable.
Is this worth it?

[ ](#){:#l0385}`(defun sieve (pipe)`
!!!(p) {:.unnumlist}

`  (make-pipe (head pipe)`
!!!(p) {:.unnumlist}

`             (filter #’(lambda (x)(/= (mod x (headpipe)) 0))`
!!!(p) {:.unnumlist}

`                    (sieve (tail pipe)))))`
!!!(p) {:.unnumlist}

`(defun sieve (pipe)`
!!!(p) {:.unnumlist}

`  (let ((first-num (head pipe)))`
!!!(p) {:.unnumlist}

`    (make-pipe first-num`
!!!(p) {:.unnumlist}

`               (filter #’(lambda (x) (/= (mod x first-num) 0))`
!!!(p) {:.unnumlist}

`                      (sieve (tail pipe))))))`
!!!(p) {:.unnumlist}

## [ ](#){:#st0075}9.9 Answers
{:#s0080}
{:.h1hd}

**Answer 9.4** Let *Fn* denote (`fib n`).
Then the time to compute *Fn*, *Tn*, is a small constant for *n* ≤ 1, and is roughly equal to *Tn-1* plus *Tn-2* for larger *n*.
Thus, *Tn* is roughly proportional to *Fn*:

Tn=FnTiFi

![si1_e](images/B9780080571157500091/si1_e.gif)

We could use some small value of *Ti* to calculate *T*100 if we knew *F*100.
Fortunately, we can use the equation:

Fn∝ϕn

![si2_e](images/B9780080571157500091/si2_e.gif)

Where φ = (1 + √(5))/2 ≈ 1.618.
This equation was derived by de Moivre in 1718 (see Knuth, Donald E.
*Fundamental Algorithms*, pp.
78–83), but the number *φ* has a long interesting history.
Euclid called it the “extreme and mean ratio,” because the ratio of *A* to *B* is the ratio of *A* + *B* to *A* if *A*/*B* is *φ*.
In the Renaissance it was called the “divine proportion,” and in the last century it has been known as the “golden ratio,” because a rectangle with sides in this ratio can be divided into two smaller rectangles that both have the same ratio between sides.
It is said to be a pleasing proportion when employed in paintings and architecture.
Putting history aside, given *T*25 ≈ 1.1 *sec* we can now calculate:

T100≈ϕ1001.1secϕ25≈5×1015sec

![si3_e](images/B9780080571157500091/si3_e.gif)

which is roughly 150 million years.
We can also see that the timing data in the table fits the equation fairly well.
However, we would expect some additional time for larger numbers because it takes longer to add and garbage collect bignums than fixnums.

**Answer 9.5** First we’ll define the notion of a forced win.
This occurs either when there are three or fewer tokens left or when you can make a move that gives your opponent a possible loss.
A possible loss is any position that is not a forced win.
If you play perfectly, then a possible loss for your opponent will in fact be a win for you, since there are no ties.
See the functions `win` and `loss` below.
Now your strategy should be to win the game outright if there are three or fewer tokens, or otherwise to choose the largest number resulting in a possible loss for your opponent.
If there is no such move available to you, take only one, on the grounds that your opponent is more likely to make a mistake with a larger pile to contend with.
This strategy is embodied in the function `nim` below.

[ ](#){:#l0390}`(defun win (n)`
!!!(p) {:.unnumlist}

`  “Is a pile of n tokens a win for the player to move?”`
!!!(p) {:.unnumlist}

`  (or (<= n 3)`
!!!(p) {:.unnumlist}

`      (loss (− n 1))`
!!!(p) {:.unnumlist}

`      (loss (− n 2))`
!!!(p) {:.unnumlist}

`      (loss (− n 3))))`
!!!(p) {:.unnumlist}

[ ](#){:#l0395}`(defun loss (n) (not (win n)))`
!!!(p) {:.unnumlist}

[ ](#){:#l0400}`(defun nim (n)`
!!!(p) {:.unnumlist}

`  “Play Nim: a player must take 1–3; taking the last one wins.`
!!!(p) {:.unnumlist}

`  (con ((<= n 3) n); an immediate win`
!!!(p) {:.unnumlist}

`      ((loss (- n 3)) 3); an eventual win`
!!!(p) {:.unnumlist}

`      ((loss (- n 2)) 2); an eventual win`
!!!(p) {:.unnumlist}

`      ((loss (- n 1)) 1); an eventual win`
!!!(p) {:.unnumlist}

`      (t 1))); a loss; the 1 is arbitrary`
!!!(p) {:.unnumlist}

`(memoize ‘loss)`
!!!(p) {:.unnumlist}

From this we are able to produce a table of execution times (in seconds), with and without memoization.
Only `loss` need be memoized.
(Why?) Do you have a good explanation of the times for the unmemoized version?
What happens if you change the order of the loss clauses in `win` and/or `nim?`

**Answer 9.6** We start by defining a function, `moves`, which generates all possible moves from a given position.
This is done by considering each pile of *n* tokens within a set of piles *s*.
Any pile bigger than two tokens can be split.
We take care to elimina te duplicate positions by sorting each set of piles, and then removing the duplicates.

[ ](#){:#l0405}`(defun moves (s)`
!!!(p) {:.unnumlist}

`  “Return a list of all possible moves in Grundy’s game”`
!!!(p) {:.unnumlist}

`  ;; S is a list of integers giving the sizes of the piles`
!!!(p) {:.unnumlist}

`  (remove-duplicates`
!!!(p) {:.unnumlist}

`    (loop for n in s append (make-moves n s))`
!!!(p) {:.unnumlist}

`    :test #’equal))`
!!!(p) {:.unnumlist}

[ ](#){:#l0410}`(defun make-moves (n s)`
!!!(p) {:.unnumlist}

`  (when (> = n 2)`
!!!(p) {:.unnumlist}

`    (let ((s/n (remove n s :count 1)))`
!!!(p) {:.unnumlist}

`      (loop for i from 1 to (− (ceiling n 2) 1)`
!!!(p) {:.unnumlist}

`            collect (sort* (list* i (− ni) s/n)`
!!!(p) {:.unnumlist}

`                           #’>>))))`
!!!(p) {:.unnumlist}

[ ](#){:#l0415}`(defun sort* (seq pred &key key)`
!!!(p) {:.unnumlist}

`  “Sort without altering the sequence”`
!!!(p) {:.unnumlist}

`  (sort (copy-seq seq) pred :key key))`
!!!(p) {:.unnumlist}

This time a loss is defined as a position from which you have no moves, or one from which your opponent can force a win no matter what you do.
A winning position is one that is not a loss, and the strategy is to pick a move that is a loss for your opponent, or if you can’t, just to play anything (here we arbitrarily pick the first move generated).

[ ](#){:#l0420}`(defun loss (s)`
!!!(p) {:.unnumlist}

`  (let ((choices (moves s)))`
!!!(p) {:.unnumlist}

`    (or (null choices)`
!!!(p) {:.unnumlist}

`        (every #’win choices))))`
!!!(p) {:.unnumlist}

[ ](#){:#l0425}`(defun win (s) (not (loss s)))`
!!!(p) {:.unnumlist}

[ ](#){:#l0430}`(defun grundy (s)`
!!!(p) {:.unnumlist}

`  (let ((choices (moves s)))`
!!!(p) {:.unnumlist}

`    (or (find-if #’loss choices)`
!!!(p) {:.unnumlist}

`        (first choices))))`
!!!(p) {:.unnumlist}

**Answer 9.7** The answer assumes that a strategy function takes four arguments: the current die roll, the score so far, the number of remaining positions in the tens column, and the number of remaining positions in the ones column.
The strategy function should return 1 or 10.

[ ](#){:#l0435}`(defun play-games (&optional (n-games 10) (player ‘make-move))`
!!!(p) {:.unnumlist}

`  “A driver for a simple dice game.
In this game the player`
!!!(p) {:.unnumlist}

`  rolls a six-sided die eight times.
The player forms four`
!!!(p) {:.unnumlist}

`  two-digit decimal numbers such that the total of the four`
!!!(p) {:.unnumlist}

`  numbers is as high as possible, but not higher than 170.`
!!!(p) {:.unnumlist}

`  A total of 171 or more gets scored as zero.
After each die`
!!!(p) {:.unnumlist}

`  is rolled, the player must decide where to put it.`
!!!(p) {:.unnumlist}

`  This function returns the player’s average score.”`
!!!(p) {:.unnumlist}

`  (/ (loop repeat n-games summing (play-game player 0 4 4))`
!!!(p) {:.unnumlist}

`     (float n-games)))`
!!!(p) {:.unnumlist}

[ ](#){:#l0440}`(defun play-game (player &optional (total 0) (tens 4) (ones 4))`
!!!(p) {:.unnumlist}

`  (cond ((or (> total 170) (< tens 0) (< ones 0)) 0)`
!!!(p) {:.unnumlist}

`        ((and (= tens 0) (= ones 0)) total)`
!!!(p) {:.unnumlist}

`        (t (let ((die (roll-die)))`
!!!(p) {:.unnumlist}

`            (case (funcall player die total tens ones)`
!!!(p) {:.unnumlist}

`             (1 (play-game player (+ total die)`
!!!(p) {:.unnumlist}

`                           tens (− ones 1)))`
!!!(p) {:.unnumlist}

`             (10 (play-game player (+ total (* 10 die))`
!!!(p) {:.unnumlist}

`                           (− tens 1) ones))`
!!!(p) {:.unnumlist}

`             (t 0))))))`
!!!(p) {:.unnumlist}

[ ](#){:#l0445}`(defun roll-die () (+ 1 (random 6)))`
!!!(p) {:.unnumlist}

So, the expression `(play-games 5 #’make-move)` would play five games with a strategy called `make-move`.
This returns only the average score of the games; if you want to see each move as it is played, use this function:

[ ](#){:#l0450}`(defun show (player)`
!!!(p) {:.unnumlist}

`  “Return a player that prints out each move it makes.”`
!!!(p) {:.unnumlist}

`  #’(lambda (die total tens ones)`
!!!(p) {:.unnumlist}

`      (when (= total 0) (fresh-line))`
!!!(p) {:.unnumlist}

`      (let ((move (funcall player die total tens ones)))`
!!!(p) {:.unnumlist}

`        (incf total (* die move))`
!!!(p) {:.unnumlist}

`        (format t “~2d-> ~ 3d | ~ @[*~]” (* move die) total (> total 170))`
!!!(p) {:.unnumlist}

`         move)))`
!!!(p) {:.unnumlist}

and call `(play-games 5 (show #’make-moves))`.

**Answer 9.9** The expression `(random 6 (make-random-state))` returns the next number that `roll-die` will return.
To guard against this, we can make `roll-die` use a random state that is not accessible through a global variable:

[ ](#){:#l0455}`(let ((state (make-random-state t)))`
!!!(p) {:.unnumlist}

`  (defun roll-die () (+ 1 (random 6 state))))`
!!!(p) {:.unnumlist}

**Answer 9.10** Because this has to do with read-time evaluation, it must be implemented as a macro or read macro.
Here’s one way to do it:

[ ](#){:#l0460}`(defmacro read-time-case (first-case &rest other-cases)`
!!!(p) {:.unnumlist}

`  “Do the first case, where normally cases are`
!!!(p) {:.unnumlist}

`  specified with #+or possibly #- marks.”`
!!!(p) {:.unnumlist}

`  (declare (ignore other-cases))`
!!!(p) {:.unnumlist}

`  first-case)`
!!!(p) {:.unnumlist}

A fanciful example, resurrecting a number of obsolete Lisps, follows:

[ ](#){:#l0465}`(defun get-fast-time ()`
!!!(p) {:.unnumlist}

`  (read-time-case`
!!!(p) {:.unnumlist}

[ ](#){:#t00110}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| `#+Explorer` | `(time :microsecond-time)` |
| `#+Franz` | `(sys:time)` |
| `#+(or PSL UCI)` | `(time)` |
| `#+YKT` | `(currenttime)` |
| `#+MTS` | `(status 39)` |
| `#+Interlisp` | `(clock 1)` |
| `#+Lispl.5` | `(tempus-fugit)` |
| `;; otherwise` | |
| | `(get-internal-real-time)))` |

**Answer 9.13** Yes.
Computing (`head pipe`) may be a trivial computation, but it will be done many times.
Binding the local variable makes sure that it is only done once.
In general, things that you expect to be done multiple times should be moved out of delayed functions, while things that may not be done at all should be moved inside a delay.

----------------------

[1](#xfn0010){:#np0010} One could say that the FORTRAN compiler was "broken." This underscores the problem of defining the efficiency of a language-do we judge by the most popular compiler, by the best compiler available, or by the best compiler imaginable?
!!!(p) {:.ftnote1}

[2](#xfn0015){:#np0015} In KCL, the symbol `lambda-closure` is used, and in Allegro, it is `excl:.
1exical-closure`
!!!(p) {:.ftnote1}

[3](#xfn0020){:#np0020} The terms *metering* and *monitoring* are sometimes used instead of profiling.
!!!(p) {:.ftnote1}

