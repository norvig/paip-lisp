
# *Paradigms of Artificial Intelligence Programming* 

![PAIP](https://norvig.com/paip-cover.gif)

This is an open-source repository for the book *Paradigms of Artificial
Intelligence Programming: Case Studies in Common Lisp* by Peter Norvig (1992), and the code contained therein.  The copyright has reverted to the author, who has shared it here under MIT license.

# The Book

The book is available in these formats:  [PAIP-part1.pdf](https://github.com/norvig/paip-lisp/blob/master/PAIP-part1.pdf) (Chapters 1-14) and [PAIP-part2.pdf](https://github.com/norvig/paip-lisp/blob/master/PAIP-part2.pdf) (15-25);
[PAIP.txt](https://github.com/norvig/paip-lisp/blob/master/PAIP.txt) (containing many errors);
[PAIP-safari.epub](https://github.com/norvig/paip-lisp/blob/master/PAIP-safari.epub) (much cleaner); and `chapter?.md` markdown files:

- **Paradigms of Artificial Intelligence Programming**
  * [Front matter](docs/frontmatter.md)
  * [Preface](docs/preface.md)
- **Part I:  Introduction to Common Lisp**
  * 1  [Introduction to Lisp](docs/chapter1.md)
  * 2  [A Simple Lisp Program](docs/chapter2.md)
  * 3 [Overview of Lisp](docs/chapter3.md)
- **Part II: Early AI Programs**
  * 4  [GPS:  The General problem Solver](docs/chapter4.md)
  * 5  [Eliza:  Dialog with a Machine](docs/chapter5.md)
  * 6  [Building Software Tools](docs/chapter6.md)
  * 7 [Student:  Solving Algebra Word Problems](docs/chapter7.md)
  * 8 [Symbolic Mathematics:  A Simplification Program](docs/chapter8.md)
- **Part III:  Tools and Techniques**
  * 9  [Efficiency Issues](docs/chapter9.md)
  * 10  [Low-Level Efficiency Issues](docs/chapter10.md)
  * 11  [Logic Programming](docs/chapter11.md)
  * 12  [Compiling Logic programs](docs/chapter12.md)
  * 13  [Object-Oriented Programming](docs/chapter13.md)
  * 14  [Knowledge Representation and Reasoning](docs/chapter14.md)
- **Part IV:  Advanced AI Programs**
  * 15  [Symbolic Mathematics with Canonical Forms](docs/chapter15.md)
  * 16  [Expert Systems](docs/chapter16.md)
  * 17  [Line-Diagram Labeling by Constraint Satisfaction](docs/chapter17.md)
  * 18  [Search and the Game of Othello](docs/chapter18.md)
  * 19  [Introduction to Natural Language](docs/chapter19.md)
  * 20  [Unification Grammars](docs/chapter20.md)
  * 21  [A Grammar of English](docs/chapter21.md)
- **Part V:  The Rest of Lisp**
  * 22  [Scheme:  An Uncommon Lisp](docs/chapter22.md)
  * 23  [Compiling Lisp](docs/chapter23.md)
  * 24  [ANSI Common Lisp](docs/chapter24.md)
  * 25  [Troubleshooting](docs/chapter25.md)
  
As seen on [TV](https://norvig.com/paip-tv.html). See also: [errata](https://norvig.com/paip-errata.html), [comments](https://norvig.com/paip-comments.html),  [retrospective](https://norvig.com/Lisp-retro.html).

# The Lisp Files

The [Lisp code files](https://github.com/norvig/paip-lisp/tree/master/lisp) are listed here:

<p>
<table border=1>
<tr><td><b><u>CH</u></b> <td><b><u>Filename</u></b><td> <b><u>Description</u></b>
<tr><td>-  <td><a href="lisp/examples.lisp">examples.lisp</a><td>	A list of example inputs taken from the book
<tr><td>-  <td><a href="lisp/tutor.lisp">tutor.lisp</a><td>		An interpreter for running the examples
<tr><td>-  <td><a href="lisp/auxfns.lisp">auxfns.lisp</a><td>		Auxiliary functions; load this before anything else
<tr><td>1  <td><a href="lisp/intro.lisp">intro.lisp</a><td>		A few simple definitions
<tr><td>2  <td><a href="lisp/simple.lisp">simple.lisp</a><td>		Random sentence generator (two versions)
<tr><td>3  <td><a href="lisp/overview.lisp">overview.lisp</a><td>	14 versions of LENGTH and other examples
<tr><td>4  <td><a href="lisp/gps1.lisp">gps1.lisp</a><td>		Simple version of General Problem Solver
<tr><td>4  <td><a href="lisp/gps.lisp">gps.lisp</a><td>		Final version of General Problem Solver
<tr><td>5  <td><a href="lisp/eliza1.lisp">eliza1.lisp</a><td>		Basic version of Eliza program
<tr><td>5  <td><a href="lisp/eliza.lisp">eliza.lisp</a><td>		Eliza with more rules; different reader
<tr><td>6  <td><a href="lisp/patmatch.lisp">patmatch.lisp</a><td>	Pattern Matching Utility
<tr><td>6  <td><a href="lisp/eliza-pm.lisp">eliza-pm.lisp</a><td>	Version of Eliza using utilities
<tr><td>6  <td><a href="lisp/search.lisp">search.lisp</a><td>		Search Utility
<tr><td>6  <td><a href="lisp/gps-srch.lisp">gps-srch.lisp</a><td>	Version of GPS using the search utility 
<tr><td>7  <td><a href="lisp/student.lisp">student.lisp</a><td>		The Student Program
<tr><td>8  <td><a href="lisp/macsyma.lisp">macsyma.lisp</a><td>		The Macsyma Program
<tr><td>8  <td><a href="lisp/macsymar.lisp">macsymar.lisp</a><td>	Simplification and integration rules for Macsyma
<tr><td>9-10	<td> &nbsp; <td>		(functions from these chapters are in <a href="auxfns.lisp">auxfns.lisp</a>)
<tr><td>11 <td><a href="lisp/unify.lisp">unify.lisp</a><td>		Unification functions
<tr><td>11 <td><a href="lisp/prolog1.lisp">prolog1.lisp</a><td>		First version of Prolog interpreter
<tr><td>11 <td><a href="lisp/prolog.lisp">prolog.lisp</a><td>		Final version of Prolog interpreter
<tr><td>12 <td><a href="lisp/prologc1.lisp">prologc1.lisp</a><td>	First version of Prolog compiler
<tr><td>12 <td><a href="lisp/prologc2.lisp">prologc2.lisp</a><td>	Second version of Prolog compiler
<tr><td>12 <td><a href="lisp/prologc.lisp">prologc.lisp</a><td>		Final version of Prolog compiler
<tr><td>12 <td><a href="lisp/prologcp.lisp">prologcp.lisp</a><td>	Primitives for Prolog compiler
<tr><td>13 <td><a href="lisp/clos.lisp">clos.lisp</a><td>		Some object-oriented and CLOS code
<tr><td>14 <td><a href="lisp/krep1.lisp">krep1.lisp</a><td>		Knowledge Representation code: first version 
<tr><td>14 <td><a href="lisp/krep2.lisp">krep2.lisp</a><td>		Knowledge Representation code with conjunctions
<tr><td>14 <td><a href="lisp/krep.lisp">krep.lisp</a><td>		Final KR code: worlds and attached functions
<tr><td>15 <td><a href="lisp/cmacsyma.lisp">cmacsyma.lisp</a><td>	Efficient Macsyma with canonical form
<tr><td>16 <td><a href="lisp/mycin.lisp">mycin.lisp</a><td>		The Emycin expert system shell
<tr><td>16 <td><a href="lisp/mycin-r.lisp">mycin-r.lisp</a><td>		Some rules for a medical application of emycin
<tr><td>17 <td><a href="lisp/waltz.lisp">waltz.lisp</a><td>		A Line-Labeling program using the Waltz algorithm
<tr><td>18 <td><a href="lisp/othello.lisp">othello.lisp</a><td>		The Othello playing program and some strategies
<tr><td>18 <td><a href="lisp/othello2.lisp">othello2.lisp</a><td>	Additional strategies for Othello
<tr><td>18 <td><a href="lisp/edge-tab.lisp">edge-tab.lisp</a><td>	Edge table for Iago strategy
<tr><td>19 <td><a href="lisp/syntax1.lisp">syntax1.lisp</a><td>		Syntactic Parser
<tr><td>19 <td><a href="lisp/syntax2.lisp">syntax2.lisp</a><td>		Syntactic Parser with semantics
<tr><td>19 <td><a href="lisp/syntax3.lisp">syntax3.lisp</a><td>		Syntactic Parser with semantics and preferences
<tr><td>20 <td><a href="lisp/unifgram.lisp">unifgram.lisp</a><td>	Unification Parser
<tr><td>21 <td><a href="lisp/grammar.lisp">grammar.lisp</a><td>		Comprehensive grammar of English
<tr><td>21 <td><a href="lisp/lexicon.lisp">lexicon.lisp</a><td>		Sample Lexicon of English
<tr><td>22 <td><a href="lisp/interp1.lisp">interp1.lisp</a><td>		Scheme interpreter, including version with macros
<tr><td>22 <td><a href="lisp/interp2.lisp">interp2.lisp</a><td>		A tail recursive Scheme interpreter
<tr><td>22 <td><a href="lisp/interp3.lisp">interp3.lisp</a><td>		A Scheme interpreter that handles call/cc
<tr><td>23 <td><a href="lisp/compile1.lisp">compile1.lisp</a><td>	Simple Scheme compiler
<tr><td>23 <td><a href="lisp/compile2.lisp">compile2.lisp</a><td>	Compiler with tail recursion and primitives
<tr><td>23 <td><a href="lisp/compile3.lisp">compile3.lisp</a><td>	Compiler with peephole optimizer
<tr><td>23 <td><a href="lisp/compopt.lisp">compopt.lisp</a><td>		Peephole optimizers for compile3.lisp
</table>

      

# Running the Code

There is no single "application" to run. Rather, there is a collection of source code files,
duplicating the code in the book. You can read and/or run whatever you like. Lisp is an interactive language,
and you will need to interact with the code to get benefit from it. Some hints:

* You will need a Common Lisp interpreter/compiler/environment. Here's a [discussion](https://www.reddit.com/r/lisp/comments/752wxe/what_is_the_best_common_lisp_interpreter_out_there/) of the options.
* You will always need `(load "auxfns.lisp")`.
* You will need `(requires "`*file*`")`, for the various
instances of *file* that you want to use. (If `requires` does not work properly on
your system you may have to alter its definition, in 
`auxfns.lisp`.  
* The function `do-examples`, which takes as an argument either `:all`
or a chapter number or a list of chapter numbers, can be used to see examples
of the use of various functions.  For example, `(do-examples 1)` shows
the examples from chapter 1. Access this by doing `(requires "examples")`.




      
