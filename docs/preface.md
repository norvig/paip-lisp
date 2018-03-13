# Preface {docsify-ignore}
<a id='page-vii'></a>

> **paradigm** *n* **1** an example or pattern; *esp* an outstandingly clear or typical example. 
>
> -*Longman's Dictionary of the English Language*, 1984 

This book is concerned with three related topics: the field of artificial intelligence, or AI; the skill 
of computer programming; and the programming language Common Lisp. Careful readers of 
this book can expect to come away with an appreciation of the major questions and techniques 
of AI, an understanding of some important AI programs, and an ability to read, modify, and 
create programs using Common Lisp. The examples in this book are designed to be clear 
examples of good programming style—paradigms of programming. They are also paradigms 
of AI research—historically significant programs that use widely applicable techniques to solve 
important problems. 

Just as a liberal arts education includes a course in "the great books" of a culture, so this book 
is, at one level, a course in "the great programs" that define the AI culture.[TK fn1] 

At another level, this book is a highly technical compendium of the knowledge you will need 
to progress from being an intermediate Lisp programmer to being an expert. Parts I and II are 
designed to help the novice get up to speed, but the complete beginner may have a hard time 
even with this material. Fortunately, there are at least five good texts available for the beginner; 
see [page xiii](preface#page-xiii) for my recommendations. 

[TK fn1] This does not imply that the programs chosen are the best of all AI programs—just that 
they are representative. 

<a id='page-viii'></a>

All too often, the teaching of computer programming consists of explaining the 
syntax of the chosen language, showing the student a 10-line program, and then 
asking the student to write programs. In this book, we take the approach that the 
best way to learn to write is to read (and conversely, a good way to improve reading 
skills is to write). After the briefest of introductions to Lisp, we start right off with 
complex programs and ask the reader to understand and make small modifications 
to these programs. 

The premise of this book is that you can only write something useful and interesting 
when you both understand what makes good writing and have something 
interesting to say. This holds for writing programs as well as for writing prose. As 
Kernighan and Plauger put it on the cover of *Software Tools in Pascal*: 

> Good programming is not learned from generalities, but by seeing how significant 
programs can be made clean, easy to read, easy to maintain and modify, 
human-engineered, efficient, and reliable, by the application of common sense 
and good programming practices. Careful study and imitation of good programs 
leads to better writing. 

The proud craftsman is often tempted to display only the finished work, without 
any indication of the false starts and mistakes that are an unfortunate but unavoidable 
part of the creative process. Unfortunately, this reluctance to unveil the process is 
a barrier to learning; a student of mathematics who sees a beautiful 10-line proof in 
a textbook can marvel at its conciseness but does not learn how to construct such a 
proof. This book attempts to show the complete programming process, "warts and 
all." Each chapter starts with a simple version of a program, one that works on some 
examples but fails on others. Each chapter shows how these failures can be analyzed 
to build increasingly sophisticated versions of the basic program. Thus, the reader 
can not only appreciate the final result but also see how to learn from mistakes and 
refine an initially incomplete design. Furthermore, the reader who finds a particular 
chapter is becoming too difficult can skip to the next chapter, having gained some 
appreciation of the problem area, and without being overwhelmed by the details. 

This book presents a body of knowledge loosely known as "AI programming 
techniques," but it must be recognized that there are no clear-cut boundaries on this 
body of knowledge. To be sure, no one can be a good AI programmer without first 
being a good programmer. Thus, this book presents topics (especially in parts III 
and V) that are not AI per se, but are essential background for any AI practitioner. 

## Why Lisp? Why Common Lisp? 

Lisp is one of the oldest programming languages still in widespread use today. There 
have been many versions of Lisp, each sharing basic features but differing in detail. 
In this book we use the version called Common Lisp, which is the most widely 
accepted standard. Lisp has been chosen for three reasons. 

<a id='page-ix'></a>

First, Lisp is the most popular language for AI programming, particularly in the 
United States. If you're going to learn a language, it might as well be one with a 
growing literature, rather than a dead tongue. 

Second, Lisp makes it easy to capture relevant generalizations in defining new 
objects. In particular. Lisp makes it easy to define new languages especially targeted 
to the problem at hand. This is especially handy in AI applications, which often 
manipulate complex information that is most easily represented in some novel form. 
Lisp is one of the few languages that allows full flexibility in defining and manipulating 
programs as well as data. All programming languages, by definition, provide 
a means of defining programs, but many other languages limit the ways in which a 
program can be used, or limit the range of programs that can be defined, or require 
the programmer to explicitly state irrelevant details. 

Third, Lisp makes it very easy to develop a working program fast. Lisp programs 
are concise and are uncluttered by low-level detail. Common Lisp offers an unusually 
large number of useful predefined objects, including over 700 functions. The programming 
environment (such as debugging tools, incremental compilers, integrated 
editors, and interfaces to window systems) that surround Lisp systems are usually 
very good. And the dynamic, interactive nature of Lisp makes it easy to experiment 
and change a program while it is being developed. 

It must be mentioned that in Europe and Japan, Prolog has been as popular as 
Lisp for AI work. Prolog shares most of Lisp's advantages in terms of flexibility and 
conciseness. Recently, Lisp has gained popularity worldwide, and Prolog is becoming 
more well known in the United States. As a result, the average AI worker today is 
likely to be bilingual. This book presents the key ideas behind Prolog in chapters 11 
and 12, and uses these ideas in subsequent chapters, particularly 20 and 21. 

The dialect of Lisp known as Scheme is also gaining in popularity, but primarily 
for teaching and experimenting with programming language design and techniques, 
and not so much for writing large AI programs. Scheme is presented in chapters 22 
and 23. Other dialects of Lisp such as Franz Lisp, MacLisp, InterLisp, ZetaLisp, 
and Standard Lisp are now considered obsolete. The only new dialect of Lisp to be 
proposed recently is EuLisp, the European Lisp. A few dialects of Lisp live on as 
embedded extension languages. For example, the Gnu Emacs text editor uses elisp, 
and the AutoCad computer-aided design package uses AutoLisp, a derivative of Xlisp. 
In the future, it is likely that Scheme will become a popular extension language, since 
it is small but powerful and has an officially sanctioned standard definition. 

There is a myth that Lisp (and Prolog) are "special-purpose" languages, while 
languages like Pascal and C are "general purpose." Actually, just the reverse is 
true. Pascal and C are special-purpose languages for manipulating the registers and 
memory of a von Neumann-style computer. The majority of their syntax is devoted 
to arithmetic and Boolean expressions, and while they provide some facilities for 
forming data structures, they have poor mechanisms for procedural abstraction 
or control abstraction. In addition, they are designed for the state-oriented style 
<a id='page-x'></a>
of programming: computing a result by changing the value of variables through 
assignment statements. 

Lisp, on the other hand, has no special syntax for arithmetic. Addition and 
multiplication are no more or less basic than list operations like appending, or string 
operations like converting to upper case. But Lisp provides all you will need for 
programming in general: defining data structures, functions, and the means for 
combining them. 

The assignment-dominated, state-oriented style of programming is possible in 
Lisp, but in addition object-oriented, rule-based, and functional styles are all supported 
within Lisp. This flexibility derives from two key features of Lisp: First, Lisp 
has a powerful *macro* facility, which can be used to extend the basic language. When 
new styles of programming were invented, other languages died out; Lisp simply 
incorporated the new styles by defining some new macros. The macro facility is 
possible because Lisp programs are composed of a simple data structure: the list. 
In the early days, when Lisp was interpreted, most manipulation of programs was 
done through this data structure. Nowadays, Lisp is more often compiled than interpreted, 
and programmers rely more on Lisp's second great flexible feature: the 
*function*. Of course, other languages have functions, but Lisp is rare in allowing the 
creation of new functions while a program is running. 

Lisp's flexibility allows it to adapt as programming styles change, but more importantly. 
Lisp can adapt to your particular programming problem. In other languages 
you fit your problem to the language; with Lisp you extend the language to fit your 
problem. 

Because of its flexibility. Lisp has been succesful as a high-level language for rapid 
prototyping in areas such as AI, graphics, and user interfaces. Lisp has also been 
the dominant language for exploratory programming, where the problems are so 
complex that no clear solution is available at the start of the project. Much of AI falls 
under this heading. 

The size of Common Lisp can be either an advantage or a disadvantage, depending 
on your outlook. In David Touretzky's (1989) fine book for beginning programmers, 
the emphasis is on simplicity. He chooses to write some programs slightly less 
concisely, rather than introduce an esoteric new feature (he cites `pushnew` as an 
example). That approach is entirely appropriate for beginners, but this book goes 
well past the level of beginner. This means exposing the reader to new features of 
the language whenever they are appropriate. Most of the time, new features are 
described as they are introduced, but sometimes explaining the details of a low-
level function would detract from the explanation of the workings of a program. 
In accepting the privilege of being treated as an "adult," the reader also accepts a 
responsibility—to look up unfamiliar terms in an appropriate reference source. 

<a id='page-xi'></a>

## Outline of the Book 

This book is organized into five parts. 

**Part I** introduces the Common Lisp programming language. 

Chapter 1 gives a quick introduction by way of small examples that demonstrate 
the novel features of Lisp. It can be safely skipped or skimmed by the experienced 
programmer. 

Chapter 2 is a more extended example showing how the Lisp primitives can be 
put together to form a program. It should be studied carefully by the novice, and 
even the experienced programmer will want to look through it to get a feel for my 
programming style. 

Chapter 3 provides an overview of the Lisp primitives. It can be skimmed on first 
reading and used as a reference whenever an unfamiliar function is mentioned in 
the text. 

Part I has been kept intentionally brief, so that there is more room for presenting 
actual AI programs. Unfortunately, that means that another text or reference book 
(or online help) may be needed to clarify some of the more esoteric features of the 
language. My recommendations for texts are on [page xiii](preface#page-xiii). 

The reader may also want to refer to chapter 25, which offers some debugging 
and troubleshooting hints. 

**Part II** covers four early AI programs that all use rule-based pattern-matching 
techniques. By starting with relatively simple versions of the programs and then 
improving them and moving on to more complex programs, the reader is able to 
gradually acquire increasingly advanced programming skills. 

Chapter 4 presents a reconstruction of GPS, the General Problem Solver. The 
implementation follows the STRIPS approach. 

Chapter 5 describes ELIZA, a program that mimics human dialogue. This is 
followed by a chapter that generalizes some of the techniques used in GPS and ELIZA 
and makes them available as tools for use in subsequent programs. 

Chapter 7 covers STUDENT, a program that solves high-school-level algebra word 
problems. 

Chapter 8 develops a small subset of the MACSYMA program for doing symbolic 
algebra, including differential and integral calculus. It may be skipped by those who 
shy away from heavy mathematics. 

**Part III** detours from AI for a moment to present some general tools for more 
efficient programming. The reader who masters the material in this part can be 
considered an advanced Lisp programmer. 

Chapter 9 is a detailed study of efficiency techniques, concentrating on caching, 
indexing, compilation, and delaying computation. Chapter 10 covers lower-level efficiency 
issues such as using declarations, avoiding garbage generation, and choosing 
the right data structure. 

<a id='page-xii'></a>

Chapter 11 presents the Prolog language. The aim is two-fold: to show how to 
write an interpreter for another language, and to introduce the important features 
of Prolog, so that they can be used where appropriate. Chapter 12 shows how a 
compiler for Prolog can be 20 to 200 times faster than the interpreter. 

Chapter 13 introduces object-oriented programming in general, then explores the 
Common Lisp Object System (CLOS). 

Chapter 14 discusses the advantages and limitations of both logic-oriented and 
object-oriented programming, and develops a knowledge representation formalism 
using all the techniques of part III. 

**Part IV** covers some advanced AI programs. 

Chapter 15 uses the techniques of part III to come up with a much more efficient 
implementation of MACSYMA. It uses the idea of a canonical form, and replaces the 
very general rewrite rule approach with a series of more specific functions. 

Chapter 16 covers the EMYCIN expert system shell, a backward chaining rule-based 
system based on certainty factors. The MYCIN medical expert system is also 
covered briefly. 

Chapter 17 covers the Waltz line-labeling algorithm for polyhedra (using Huffman-Clowes 
labels). Different approaches to constraint propagation and backtracking 
are discussed. 

Chapter 18 presents a program that plays an excellent game of Othello. The 
technique used, alpha-beta searching, is appropriate to a wide variety of two-person 
games. 

Chapter 19 is an introduction to natural language processing. It covers context-free 
grammar, top-down and bottom-up parsing, chart parsing, and some semantic 
interpretation and preferences. 

Chapter 20 extends the linguistic coverage of the previous chapter and introduces 
logic grammars, using the Prolog compiler developed in chapter 11. 

Chapter 21 is a fairly comprehensive grammar of English using the logic grammar 
formalism. The problems of going from a simple idea to a realistic, comprehensive 
program are discussed. 

**Part V** includes material that is peripheral to AI but important for any serious 
Lisp programmer. 

Chapter 22 presents the Scheme dialect of Lisp. A simple Scheme interpreter is 
developed, then a properly tail-recursive interpreter, then an interpreter that explicitly 
manipulates continuations and supports `call/cc`. Chapter 23 presents a Scheme 
compiler. 

Chapter 24 presents the features that are unique to American National Standards 
Institute (ANSI) Common Lisp. This includes the `loop` macro, as well as error 
handling, pretty printing, series and sequences, and the package facility. 

Chapter 25 is a guide to troubleshooting and debugging Lisp programs. 

<a id='page-xiii'></a>

The bibliography lists over 200 sources, and there is a comprehensive index. In 
addition, the appendix provides a directory of publicly available Lisp programs. 

## How to Use This Book 

The intended audience for this book is broad: anyone who wants to become an advanced 
Lisp programmer, and anyone who wants to be an advanced AI practitioner. 
There are several recommended paths through the book: 

* *In an Introductory AI Course:* Concentrate on parts I and II, and at least one 
example from part IV. 
* *In an Advanced AI Programming Course:* Concentrate on parts I, II and IV, skipping 
chapters that are of less interest and adding as much of part III as time permits. 
* *In an Advanced Programming Languages Course:* Concentrate on parts I and V, 
with selections from part III. Cover chapters 11 and 13 if similar material is not 
presented with another text. 
* *For the Professional Lisp Programmer:* Read as much of the book as possible, and 
refer back to it often. Part III and chapter 25 are particularly important. 

## Supplementary Texts and Reference Books 

The definitive reference source is Steele's *Common Lisp the Language.* From 1984 
to 1990, this unambiguously defined the language Common Lisp. However, in 
1990 the picture became more complicated by the publication of *Common Lisp the 
Language,* 2d edition. This book, also by Steele, contains the recommendations of 
ANSI subcommittee X3J13, whose charter is to define a standard for Lisp. These 
recommendations include many minor changes and clarifications, as well as brand 
new material on object-oriented programming, error condition handling, and the 
loop macro. The new material doubles the size of the book from 465 to 1029 pages. 

Until the ANSI recommendations are formally accepted, Common Lisp users 
are in the unfortunate situation of having two distinct and incompatible standards: 
"original" Common Lisp and ANSI Common Lisp. Most of the code in this book is 
compliant with both standards. The most significant use of an ANSI function is the 
`loop` macro. The ANSI `map-into`, `complement`, and `reduce` functions are also used, 
although rarely. Definitions for all these functions are included, so even those using 
an "original" Common Lisp system can still run all the code in the book. 

While *Common Lisp the Language* is the definitive standard, it is sometimes terse 
and can be difficult for a beginner. *Common Lisp: the Reference,* published by Franz 
Inc., offers complete coverage of the language with many helpful examples. *Common 
LISPcraft,* by Robert Wilensky, and *Artificial Intelligence Programming,* by Charniak 
<a id='page-xiv'></a>
et al., also include brief summaries of the Common Lisp functions. They are not 
as comprehensive, but that can be a blessing, because it can lead the reader more 
directly to the functions that are important (at least in the eyes of the author). 

It is a good idea to read this book with a computer at hand, to try out the examples 
and experiment with examples of your own. A computer is also handy because Lisp 
is self-documenting, through the functions `apropos`, `describe`, and `documentation`. 
Many implementations also provide more extensive documentation through some 
kind of 'help' command or menu. 

The five introductory Lisp textbooks I recommend are listed below. The first is 
more elementary than the others. 

* *Common Lisp: A Gentle Introduction to Symbolic Computation* by David Touretzky. 
Most appropriate for beginners, including those who are not computer 
scientists. 

* *A Programmer's Guide to Common Lisp* by Deborah G. Tatar. Appropriate for 
those with experience in another programming language, but none in Lisp. 

* *Common LISPcraft* by Robert Wilensky. More comprehensive and faster paced, 
but still useful as an introduction as well as a reference. 

* *Common Lisp* by Wade L. Hennessey. Somewhat hit-and-miss in terms of the 
topics it covers, but with an enlightened discussion of implementation and 
efficiency issues that do not appear in the other texts. 

* *LISP* (3d edition) by Patrick H. Winston and Bertold Horn. Covers the most 
ground in terms of programming advice, but not as comprehensive as a reference. 
May be difficult for beginners. Includes some AI examples. 

While it may be distracting for the beginner to be continually looking at some 
reference source, the alternative—to have this book explain every new function in 
complete detail as it is introduced—would be even more distracting. It would interrupt 
the description of the AI programs, which is what this book is all about. 

There are a few texts that show how to write AI programs and tools, but none 
that go into the depth of this book. Nevertheless, the expert AI programmer will 
want to be familiar with all the following texts, listed in rough order of increasing 
sophistication: 

* *LISP* (3d edition). (See above.) 

* *Programming Paradigms in Lisp* by Rajeev Sangal. Presents the different styles 
of programming that Lisp accommodates, illustrating them with some useful 
AI tools. 

<a id='page-xv'></a>

* *Programming for Artificial Intelligence* by Wolfgang Kreutzer and Bruce McKenzie. 
Covers some of the basics of rule-based and pattern-matching systems well, 
but covers Lisp, Prolog, and Smalltalk, and thus has no time left for details in 
any of the languages. 

* *Artificial Intelligence Programming* (2d edition) by Eugene Charniak, Christopher 
Riesbeck, Drew McDermott, and James Meehan. Contains 150 pages of 
Lisp overview, followed by an advanced discussion of AI tools, but no actual 
AI programs. 

* *AI in Practice: Examples in Pop-11* by Allan Ramsey and Rosalind Barrett. Advanced, 
high-quality implementations of five AI programs, unfortunately using 
a language that has not gained popularity. 

The current text combines the virtues of the last two entries: it presents both actual 
AI programs and the tools necessary to build them. Furthermore, the presentation is 
in an incremental fashion, with simple versions presented first for clarity, followed 
by more sophisticated versions for completeness. 

## A Note on Exercises 

Sample exercises are provided throughout. Readers can test their level of understanding 
by faithfully doing the exercises. The exercises are graded on the scale [s], 
[m], [h], [d], which can be interpreted either as a level of difficulty or as an expected 
time it will take to do the exercise: 

| Code | Difficulty | Time to Do  |
|------|------------|-------------|
| [s]  | Simple     | Seconds     |
| [m]  | Medium     | Minutes     |
| [h]  | Hard       | Hours       |
| [d]  | Difficult  | Days        |

The time to do the exercise is measured from the point that the concepts have 
been well understood. If the reader is unclear on the underlying concepts, it might 
take hours of review to understand a [m] problem. Answers to the exercises can be 
found in a separate section at the end of each chapter. 

## Acknowledgments 

A great many people contributed to this book. First of all I would like to thank my 
students at USC and Berkeley, as well as James Martin's students at Colorado and 
Michael Pazzani's students at Irvine, who course-tested earlier versions of this book. 
Useful suggestions, corrections, and additions were made by: 

<a id='page-xvi'></a>

Nina Amenta (Berkeley), Ray S. Babcock and John Paxton (Montana State), 
Bryan A. Bentz (BBN), Mary P. Boelk (Johnson Controls), Michael Braverman (Berkeley), 
R. Chandrasekar and M. Sasikumar (National Centre for Software Technology, 
Bombay), Mike Clancy (Berkeley), Michael Covington (Georgia), Bruce D'Ambrosio 
(Oregon State), Piew Datta (Irvine), Shawn Dettrey (USC), J. A. Durieux (AI Engineering 
BV, Amsterdam), Joseph Faletti (ETS), Paul Fuqua (Texas Instruments), 
Robert Goldman (Tulane), Marty Hall (Johns Hopkins), Marti Hearst (Berkeley), Jim 
Hendler (Maryland), Phil Laird (NASA), Raymond Lang (Tulane), David D. Loeffler 
(MCC), George Luger (New Mexico), Rob MacLachlan (CMU), Barry Margolin 
(Thinking Machines), James Mayfield (UMBC), Sanjay Manchandi (Arizona), Robert 
McCartney (Connecticut), James Meehan (DEC), Andrew L. Ressler, Robert S. Rist 
(University of Technology, Sydney), Paul Snively (Apple), Peter Van Roy (Berkeley), 
David Gumby Wallace (Cygnus), and Jeff Wu (Colorado). 

Sam Dooley and Eric Wefald both wrote Othello-playing programs without which 
I would not have written chapter 18. Eric also showed me Aristotle's quotes on means-ends 
analysis. Tragically, Eric died in August 1989. He is sorely missed by his friends 
and colleagues. Richard Fateman made suggestions for chapter 8, convinced me to 
write chapter 15, and, with help from Peter Klier, wrote a substantial program from 
which I adapted some code for that chapter. Charley Cox (Franz Inc.), Jamie Zawinski 
(Lucid Inc.), and Paul Fuqua (Texas Instruments) explained the inner workings of 
their respective companies' compilers. Mike Harrison, Paul Hilfinger, Marc Luria, 
Ethan Munson, and Stephan Slade helped with LATEX. Narciso Jarimillo tested all the 
code and separated it into the files that are available to the reader (see [page 897](appendix.md#page-897)). 

During the writing of this book I was supported by a grant from the Defense 
Advanced Research Projects Agency (DoD), Arpa Order No. 4871, monitored by 
Space and Naval Warfare Systems Command under Contract N00039-84-C-0089. 
Special thanks to DARPA and to Robert Wilensky and the rest of my colleagues and 
students at Berkeley for providing a stimulating environment for research, programming, 
and writing. 

Finally, thanks to Mike Morgan and Yonie Overton for overseeing the production 
of the book and encouraging me to finish on time. 

