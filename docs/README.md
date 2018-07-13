
# *Paradigms of Artificial Intelligence Programming* 

![PAIP](https://norvig.com/paip-cover.gif)

# Table of Contents

- Preface
  * Why Lisp?  Why Common Lisp?
  * Outline of the Book
  * How to use This Book
  * Supplementary Texts and Reference Books
  * A Note on Exercises
  * Acknowledgments
- **Part I  Introduction to Common Lisp**
- **1  Introduction to Lisp**
  * 1.1  Symbolic Computation
  * 1.2  Variables
  * 1.3  Special Forms
  * 1.4  Lists
  * 1.5  Defining New Functions
  * 1.6  Using Functions
  * 1.7  Higher-Order Functions
  * 1.8  Other Data Types
  * 1.9  Summary:  The Lisp Evaluation Rule
  * 1.10  What Makes Lisp Different?
  * 1.11  Exercises
  * 1.12  Answers
- **2  A Simple Lisp Program**
  * 2.1  A Grammar for a Subset of English
  * 2.2  A Straightforward Solution
  * 2.3  A Rule-Based Solution
  * 2.4  Two paths to Follow
  * 2.5  Changing the Grammar without Changing the Program
  * 2.6  Using the Same Data for Several Programs
  * 2.7  Exercises
  * 2.8  Answers
- **3  Overview of Lisp**
  * 3.1  A Guide to Lisp Style
  * 3.2  Special Forms
      * Special Forms for Definitions
      * Special Forms for Conditionals
      * Special Forms for Dealing with Variables and Places
      * Functions and Special Forms for Repetition
      * Repetition through Recursion
      * Other Special Forms
      * Macros
      * Backquote Notation
  * 3.3  Functions on Lists
  * 3.4  Equality and Internal Representation
  * 3.5  Functions on Sequences
  * 3.6  Functions for Maintaining Tables
  * 3.7  Functions on Trees
  * 3.8  Functions on Numbers
  * 3.9  Functions on Sets
  * 3.10  Destructive Functions
  * 3.11 Overview of Data types
  * 3.12  Input/Output
  * 3.13  Debugging tools
  * 3.14  Antibugging Tools
      * Timing Tools
  * 3.15  Evaluation
  * 3.16  Closures
  * 3.17  Special Variables
  * 3.18  Multiple Values
  * 3.19  More about Parameters
  * 3.20  The Rest of Lisp
  * 3.21  Exercises
  * 3.22  Answers
- **Part II  Early AI Programs**
- **4  GPS:  The General problem Solver**
  * 4.1  Stage 1:  Description
  * 4.2  Stage 2:  Specification
  * 4.3  Stage 3:  Implementation
  * 4.4  Stage 4:  Test
  * 4.5  Stage 5:  Analysis, or &quot;We Lied about the G&quot;
  * 4.6  The Running Around the Block Problem
  * 4.7  The Clobbered Sibling Goal Problem
  * 4.8  The Leaping before You Look Problem
  * 4.9  The recursive Subgoal problem
  * 4.10  The Lack of Intermediate Information Problem
  * 4.11  GPS Version 2:  A More General problem Solver
  * 4.12  The New Domain problem:  Monkey and Bananas
  * 4.13  The Maze Searching Domain
  * 4.14  The Blocks World Domain
      * The Sussman Anomaly
  * 4.15  Stage 5 Repeated:  Analysis of Version 2
  * 4.16  The Not Looking after You Don&#39;t Leap Problem
  * 4.17  The Lack of Descriptive Power Problem
  * 4.18  The Perfect Information Problem
  * 4.19  The Interacting Goals Problem
  * 4.20  The End of GPS
  * 4.21  History and References
  * 4.22 Exercises
  * 4.23  Answers
- **5  Eliza:  Dialog with a Machine**
  * 5.1  Describing and Specifying Eliza
  * 5.2  Pattern Matching
  * 5.3  Segment Pattern Matching
  * 5.4  The Eliza Program:  A Rule-Based Translator
  * 5.5  History and References
  * 5.6  Exercises
  * 5.7  Answers
- **6  Building Software Tools**
  * 6.1  An Interactive Interpreter Tool
  * 6.2  A Pattern-Matching Tool
  * 6.3  A Rule-Based Translator Tool
  * 6.4  A Set of Searching Tools
      * Searching Trees
      * Guiding the Search
      * Search Paths
      * Guessing versus Guaranteeing a Good Solution
      * Searching Graphs
  * 6.5  GPS as Search
  * 6.6  History and References
  * 6.7  Exercises
  * 6.8  Answers
- **7  Student:  Solving Algebra Word Problems**
  * 7.1  Translating English into Equations
  * 7.2  Solving Algebraic Equations
  * 7.3  Examples
  * 7.4  History and References
  * 7.5  Exercises
  * 7.6  Answers
- **8  Symbolic Mathematics:  A Simplification Program**
  * 8.1  Converting Infix to Prefix Notation
  * 8.2  Simplification Rules
  * 8.3  Associativity and Commutativity
  * 8.4  Logs, Trig, and Differentiation
  * 8.5  Limits of Rule-Based Approaches
  * 8.6  Integration
  * 8.7  History and References
  * 8.8. Exercises
- **Part III  Tools and Techniques**
- **9  Efficiency Issues**
  * 9.1  Caching Results of Previous Computations:   Memoization
  * 9.2  Compiling One Language into Another
  * 9.3  Delaying Computation
  * 9.4  Indexing Data
  * 9.5  Instrumentation:  Deciding What to Optimize
  * 9.6  A Case Study in Efficiency:  The SIMPLIFY Program
      * Memoization
      * Indexing
      * Compilation
      * The Single-Rule Compiler
      * The Rule-Set Compiler
  * 9.7  History and References
  * 9.8  Exercises
  * 9.9  Answers
- **10  Low-Level Efficiency Issues**
  * 10.1  use Declarations
  * 10.2  Avoid Generic Functions
  * 10.3  Avoid Complex Argument Lists
  * 10.4  Avoid Unnecessary Consing
      * Avoid Consing:  Unique Lists
      * Avoid Consing:  Multiple Values
      * Avoid Consing:  Resources
  * 10.5  Use the Right Data Structures
      * The Right Data Structure:  Variables
      * The Right Data Structure:  Queues
      * The Right Data Structure:  Tables
  * 10.6  Exercises
  * 10.7  Answers
- **11  Logic Programming**
  * 11.1  Idea 1:  A Uniform Data Base
  * 11.2  Idea 2:  Unification of Logic Variables
      * Programming with Prolog
  * 11.3  Idea 3:  Automatic Backtracking
      * Approaches to Backtracking
      * Anonymous Variables
  * 11.4  The Zebra Puzzle
  * 11.5  The Synergy of Backtracking and Unification
  * 11.6  Destructive Unification
  * 11.7  Prolog in Prolog
  * 11.8  Prolog Compared to Lisp
  * 11.9  History and References
  * 11.10  Exercises
  * 11.11  Answers
- **12  Compiling Logic programs**
  * 12.1  A prolog Compiler
  * 12.2  Fixing the Errors in the Compiler
  * 12.3  Improving the Compiler
  * 12.4  Improving the Compilation of Unification
  * 12.5  Further Improvements to Unification
  * 12.6  The User Interface to the Compiler
  * 12.7  Benchmarking the Compiler
  * 12.8  Adding More Primitives
  * 12.9  The Cut
  * 12.10  &quot;Real&quot; Prolog
  * 12.11 History and References
  * 12.12  Exercises
  * 12.13  Answers
- **13  Object-Oriented Programming**
  * 13.1  Object-Oriented Programming
  * 13.2  Objects
  * 13.3  Generic Functions
  * 13.4  Classes
  * 13.5  Delegation
  * 13.6  Inheritance
  * 13.7  CLOS:  The Common Lisp Object System
  * 13.8  A CLOS Example:  Searching Tools
      * Best-First Search
  * 13.9  Is CLOS Object-Oriented?
  * 13.10  Advantages of Object-Oriented programming
  * 13.11  History and References
  * 13.12  Exercises
- **14  Knowledge Representation and Reasoning**
  * 14.1  A Taxonomy of Representation Languages
  * 14.2  Predicate Calculus and its Problems
  * 14.3  A Logical Language: Prolog
  * 14.4  Problems with Prolog&#39;s Expressiveness
  * 14.5  Problems with Predicate Calculus&#39;s Expressiveness
  * 14.6  Problems with Completeness
  * 14.7  Problems with Efficiency:  Indexing
  * 14.8  A Solution to the Indexing Problem
  * 14.9  A Solution to the Completeness Problem
  * 14.10  Solutions to the Expressiveness Problems
      * Higher-Order Predications
      * Improvements
      * A Frame Language
      * Possible Worlds:  Truth, Negation, and Disjunction
      * Unification, Equality, Types, and Skolem Constants
  * 14.11  History and References
  * 14.12  Exercises
  * 14.13  Answers
- **Part IV  Advanced AI Programs**
- **15  Symbolic Mathematics with Canonical Forms**
  * 15.1  A Canonical Form for Polynomials
  * 15.2  Differentiating Polynomials
  * 15.3  Converting between Infix and Prefix
  * 15.4  Benchmarking the Polynomial Simplifier
  * 15.5  A Canonical Form for Rational Expressions
  * 15.6  Extending Rational Expressions
  * 15.7  History and References
  * 15.8  Exercises
  * 15.9  Answers
- **16  Expert Systems**
  * 16.1  Dealing with Uncertainty
  * 16.2  Caching Derived Facts
  * 16.3  Asking Questions
  * 16.4  Contexts Instead of Variables
  * 16.5  Backward-Chaining Revisited
  * 16.6  Interacting with the Expert
  * 16.7  Interacting with the Client
  * 16.8  MYCIN, A Medical Expert System
  * 16.9  Alternatives to Certainty Factors
  * 16.10  History and References
  * 16.11  Exercises
  * 16.12  Answers
- **17  Line-Diagram Labeling by Constraint Satisfaction**
  * 17.1  The Line-Labeling Problem
  * 17.2  Combining Constraints and Searching
  * 17.3  Labeling Diagrams
  * 17.4  Checking Diagrams for Errors
  * 17.5  History and References
  * 17.6  Exercises
- **18  Search and the Game of Othello**
  * 18.1  The Rules of the Game
  * 18.2  Representation Choices
  * 18.3  Evaluating Positions
  * 18.4  Searching Ahead:  Minimax
  * 18.5  Smarter Searching:  Alpha-Beta Search
  * 18.6  An Analysis of Some Games
  * 18.7  The Tournament Version of Othello
  * 18.8  Playing a Series of Games
  * 18.9  More Efficient Searching
  * 18.10  It Pays to Precycle
  * 18.11  Killer Moves
  * 18.12  Championship Programs:  Iago and Bill
      * Mobility
      * Edge Stability
      * Combining the Factors
  * 18.13  Other Techniques
      * Interative Deepening
      * Forward Pruning
      * Nonspeculative Forward Pruning
      * Aspiration Search
      * Think-Ahead
      * Hashing and Opening Book Moves
      * The End Game
      * Metareasoning
      * Learning
  * 18.14  History and References
  * 18.15  Exercises
  * 18.16  Answers
- **19  Introduction to Natural Language**
  * 19.1  Parsing with a Phrase-Structure Grammar
  * 19.2  Extending the Grammar and Recognizing Ambiguity
  * 19.3  More Efficient parsing
  * 19.4  The Unknown-Word Problem
  * 19.5  Parsing into a Semantic Representation
  * 19.6  Parsing with Preferences
  * 19.7  The Problem with Context-Free Phrase-Structure Rules
  * 19.8  History and References
  * 19.9  Exercises
  * 19.10  Answers
- **20  Unification Grammars**
  * 20.1  Parsing as Deduction
  * 20.2  Definite Clause Grammars
  * 20.3  A Simple Grammar In DCG Format
  * 20.4  A DCG Grammar with Quantifiers
  * 20.5  Preserving Quantifier Scope Ambiguity
  * 20.6  Long-Distance Dependencies
  * 20.7  Augmenting DCG Rules
  * 20.8  History and References
  * 20.9  Exercises
  * 20.10  Answers
- **21  A Grammar of English**
  * 21.1  Noun Phrases
  * 21.2  Modifiers
  * 21.3  Noun Modifiers
  * 21.4  Determiners
  * 21.5  Verb Phrases
  * 21.6  Adverbs
  * 21.7  Clauses
  * 21.8  Sentences
  * 21.9  XPs
  * 21.10  Word Categories
  * 21.11  The Lexicon
      * Verbs
      * Auxiliary Verbs
      * Nouns
      * Pronouns
      * Names
      * Adjectives
      * Adverbs
      * Articles
      * Cardinal and Ordinal Numbers
      * Prepositions
  * 21.12  Supporting the Lexicon
  * 21.13  Other Primitives
  * 21.14  Examples
  * 21.15  History and References
  * 21.16  Exercises
- **Part V  The Rest of Lisp**
- **22  Scheme:  An Uncommon Lisp**
  * 22.1  A Scheme Interpreter
  * 22.2  Syntactic Extension with Macros
  * 22.3  A Properly Tail-Recursive Interpreter
  * 22.4  Throw, Catch, and Call/cc
  * 22.5  An interpreter Supporting Call/cc
  * 22.6  History and References
  * 22.7  Exercises
  * 22.8  Answers
- **23  Compiling Lisp**
  * 23.1  A Properly Tail-Recursive Lisp Compiler
  * 23.2  Introducing Call/cc
  * 23.3  The Abstract Machine
  * 23.4  A Peephole Optimizer
  * 23.5  Languages with Different Lexical Conventions
  * 23.6  History and References
  * 23.7  Exercises
  * 23.8  Answers
- **24  ANSI Common Lisp**
  * 24.1  Packages
  * The Seven Name Spaces
  * 24.2  Conditions and Error Handling
      * Signaling Errors
      * Handling Errors
  * 24.3  Pretty Printing
  * 24.4  Series
  * 24.5  The Loop Macro
      * Anatomy of a Loop
      * Iteration Control (26.6)
      * End-Test Control (26.7)
      * Value Accumulation (26.8)
      * Variable Initialization (26.9)
      * Conditional Execution (26.10)
      * Unconditional Execution (26.11)
      * Miscellaneous Features (26.12)
  * 24.6  Sequence Functions
      * Once-only:  A Lesson in Macrology
      * Avoid Overusing Macros
      * MAP-INTO
      * REDUCE with :key
  * 24.7  Exercises
  * 24.8  Answers
- **25  Troubleshooting**
  * 25.1  Nothing Happens
  * 25.2  Change to Variable Has No Effect
  * 25.3  Change to Function Has No Effect
  * 25.4  Values Change &quot;by Themselves&quot;
  * 25.5  Built-In Functions Don&#39;t Find Elements
  * 25.6  Multiple Values Are Lost
  * 25.7  Declarations Are Ignored
  * 25.8  My Lisp Does the Wrong Thing
  * 25.9  How to Find the Function You Want
  * 25.10  Syntax of LOOP
  * 25.11  Syntax of COND
  * 25.12  Syntax of CASE
  * 25.13  Syntax of LET and LET*
  * 25.14  Problems with Macros
  * 25.15  A Style Guide to Lisp
      * When to Define a Function
      * When to Define a Special Variable
      * When to Bind a Lexical Variable
      * How to Choose a Name
      * Deciding on the Order of Parameters
  * 25.16  Dealing with Files, Packages, and Systems
  * 25.17  Portability Problems
  * 25.18  Exercises
  * 25.19  Answers
- Appendix
- Bibliography
- Index
