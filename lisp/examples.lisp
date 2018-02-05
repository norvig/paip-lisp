;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991, 1996 Peter Norvig

(requires "tutor")

(defexamples 1 "Introduction to Lisp"
  "This chapter is for people with little or no experince in Lisp."
  "Intermediate or advanced readers can skim or skip this chapter."
  ""
  "Lisp expressions are in prefix notation: the operator first."
  ((+ 2 2) => 4 @ 4)
  ((+ 1 2 3 4 5 6 7 8 9 10) => 55 @ 5)
  "This is Lisp for (900 + 900 + 90 + 9) - (5000 + 500 + 50 + 5)"
  ((- (+ 9000 900 90 9) (+ 5000 500 50 5)) => 4444)
  (:section "1.1 Symbolic Computation")
  "This is an example of computation on lists:"
  ((append '(Pat Kim) '(Robin Sandy)) => (PAT KIM ROBIN SANDY) @ 6)
  "The quote mark instructs Lisp to treat the list as data."
  ('(pat Kim) => (PAT KIM))
  "Let's look at some more list processing functions"
  (:section "1.4 Lists")
  ((setf p '(John Q Public)) @ 10)
  ((first p))
  ((rest p))
  ((second p))
  ((third p))
  ((fourth p))
  ((length p))
  "It is also possible to build up new lists"
  (p @ 11)
  ((cons 'Mr p))
  ((cons (first p) (rest p)))
  ((setf town (list 'Anytown 'USA)))
  ((list p 'of town 'may 'have 'already 'won!))
  ((append p '(of) town '(may have already won)))
  (p)
  (:section "1.5 Defining New Functions")
  "The special form DEFUN stands for 'define function.'"
  "It is used here to define a new function called last-name:"
  ((requires "intro"))
  ((last-name p) => PUBLIC @ 13)
  ((last-name '(Rex Morgan MD)) => MD)
  ((last-name '(Spot)) => SPOT)
  ((last-name '(Aristotle)) => ARISTOTLE)
  "We can also define the function first-name."
  "Even though the definition is trivial (it is the same as FIRST),"
  "it is good practice to define first-name explicitly."
  (p)
  ((first-name p) => JOHN)
  ((first-name '(Wilma Flintstone)) => WILMA)
  ((setf names '((John Q Public) (Malcolm X)
              (Admiral Grace Murray Hopper) (Spot) 
              (Aristotle) (A A Milne) (Z Z Top)
              (Sir Larry Olivier) (Miss Scarlet))) @ 14)
  ((first-name (first names)) => JOHN)
  (:section "1.6 Using Functions")
  "Consider the following expression, which can be used to test LAST-NAME:"
  ((mapcar #'last-name names))
  "The #' notation maps the name of a function to the function itself."
  ((mapcar #'- '(1 2 3 4)) @ 15)
  ((mapcar #'+ '(1 2 3 4) '(10 20 30 40)))
  "Now that we understand mapcar, let's use it to test FIRST-NAME:"
  ((mapcar #'first-name names))
  "Suppose we wanted a version of FIRST-NAME that ignored titles like Miss:"
  ((defparameter *titles*
     '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General)
     "A list of titles that can appear at the start of a name."))
  ((defun first-name (name)
     "Select the first name from a name represented as a list."
     (if (member (first name) *titles*)
	 (first-name (rest name))
       (first name))) @ 16)
  ((mapcar #'first-name names))
  ((first-name '(Madam Major General Paula Jones)) => PAULA)
  "We can see how this works by tracing the execution of first-name:"
  ((trace first-name))
  ((first-name '(John Q Public)) => JOHN @ 17)
  ((first-name '(Madam Major General Paula Jones)) => PAULA)
  ((untrace first-name))
  (:section "1.7 Higher-Order Functions")
  ((apply #'+ '(1 2 3 4)) => 10)
  ((apply #'append '((1 2 3) (a b c))))
  "Now we define a new function, self-and-double, and apply it to arguments."
  ((defun self-and-double (x) (list x (+ x x))))
  ((self-and-double 3) => (3 6))
  ((apply #'self-and-double '(3)) => (3 6))
  "Now let's return to the mapping functions:"
  ((mapcar #'self-and-double '(1 10 300)))
  ((mappend #'self-and-double '(1 10 300)))
  "FUNCALL is similar to APPLY; it too takes a function as its"
  "first argument and applies the function to a list of arguments,"
  "but in the case of FUNCALL, the arguments are listed separately:"
  ((funcall #'+ 2 3) => 5 @ 20)
  ((apply #'+ '(2 3)) => 5)
  )

(defexamples 2 "A Simple Lisp Program"
  "This chapter shows how to combine the basic functions and"
  "special forms of Lisp into a complete program"
  "The program generates random English sentences."
  (:section "2.2 A Straightforward Solution")
  "We can test the program by generating a few random sentences."
  "(Note that since these are random, you won't get the same ones"
  "as in the book.)"
  ((requires "simple"))
  ((sentence) @ 36)
  ((sentence) @ 36)
  ((sentence) @ 36)
  ((noun-phrase))
  ((verb-phrase))
  ((trace sentence noun-phrase verb-phrase article noun verb) @ 37)
  ((sentence))
  ((untrace))
  (:section "2.3 A Rule-Based Solution")
  "An alternative implementation concentrates on making it easy"
  "to write grammar rules."
  ((generate 'sentence) @ 41)
  ((generate 'sentence) @ 41)
  ((generate 'noun-phrase) @ 41)
  ((generate 'verb-phrase) @ 41)
  "One advantage of this approach is its easier to change grammars."
  ((setf *grammar* *bigger-grammar*) @ 43)
  ((generate 'sentence))
  ((generate 'sentence))
  "Another advantage is that the same data (grammar) can be used"
  "for more than one purpose.  Consider generate-tree:"
  ((generate-tree 'sentence) @ 45))


(defexamples 3 "Overview of Lisp"
  "This chapter briefly covers the most important special forms and"
  "functions in Lisp."
  (:section "3.2 Special Forms")
  "Start with functions and special forms for repetition:"
  "First, functions like MAPCAR can apply to any number of lists:" 
  ((mapcar #'- '(1 2 3)) => (-1 -2 -3) @ 61)
  ((mapcar #'+ '(1 2) '(10 20) '(100 200)) => (111 222))
  "Second, many of the functions accept keywords:"
  ((remove 1 '(1 2 3 2 1 0 -1)) => (2 3 2 0 -1) @ 61)
  ((remove 1 '(1 2 3 2 1 0 -1) :key #'abs) => (2 3 2 0) @ 61)
  ((remove 1 '(1 2 3 2 1 0 -1) :test #'<) => (1 1 0 -1) @ 61)
  ((remove 1 '(1 2 3 2 1 0 -1) :start 4) => (1 2 3 2 0 -1) @ 61)
  "Third, some have corresponding -IF or -IF-NOT versions:"
  ((remove-if #'oddp '(1 2 3 2 1 0 -1)) => (2 2 0))
  ((remove-if-not #'oddp '(1 2 3 2 1 0 -1)) => (1 3 1 -1))
  "The forms TRACE and UNTRACE are used to control debugging info:"
  ((requires "overview"))
  ((trace length9) @ 65)
  ((length9 '(1 b c)) => 3)
  ((untrace length9))
  ((length9 '(1 b c)) => 3)
  (:section "3.7 Functions on Trees")
  ((setf tree '((a b) ((c)) (d e))) @ 76)
  ((tree-equal tree (copy-tree tree)) => t)
  ((same-shape-tree tree '((1 2) ((3)) (4 5))) => t)
  ((same-shape-tree tree '((1 2) (3) (4 5))) => nil)
  "There are two functions for substituting a new expression into a tree:"
  ((subst 'new 'old '(old ((very old)))) => (NEW ((VERY NEW))))
  ((sublis '((old . new)) '(old ((very old)))) => (NEW ((VERY NEW))))
  ((subst 'new 'old 'old) => NEW)
  "Here is an example:"
  ((english->french '(hello my friend - how are you today?))
   => (bonjour mon ami - comment va tu today?) @ 77)
  (:section "3.10 Destructive Functions")
  "Consider the following:"
  ((setq x '(a b c)) @ 80)
  ((setq y '(1 2 3)))
  ((nconc x y) => (a b c 1 2 3))
  (x => (a b c 1 2 3))
  (y => (1 2 3))
  "NCONC computes the same result as APPEND, but it alters the first argument."
  "It is called a 'destructive' function."
  "There is quite a conceptual load on the programmer who uses NCONC."
  "The advantage of NCONC is that it doesn't use any storage."
  ""
  (:section "3.11 Overview of Data Types")
  "The function TYPE-OF returns the type of its argument."
  ((type-of 123) => fixnum @ 82)
  ((typep 123 'fixnum) => t)
  ((typep 123 'integer) => t)
  ((typep 123.0 'integer) => nil)
  ((subtypep 'fixnum 'integer) => t)
  (:section "3.12 Input/Output")
  "FORMAT is the main function for formatted output:"
  ((format t "hello, world") @ 84)
  ((format t "~&~a plus ~s is ~f" "two" "two" 4))
  ((let ((numbers '( 1 2 3 4 5)))
     (format t "~&~{~r~^ plus ~} is ~@r"
	     numbers (apply #'+ numbers))))
  (:section "3.13 Debugging tools")
  ((documentation 'first 'function) @ 87)
  ((documentation 'pi 'variable))
  (:section "3.14 Antibugging Tools")
  ((defun f (n) (dotimes (i n) nil)) @ 90)
  ((time (f 10000)))
  ((compile 'f))
  ((time (f 10000)))
  (:section "3.15 Evaluation")
  "The following five forms are equivalent:"
  ((+ 1 2 3 4) => 10 @ 91)
  ((funcall #'+ 1 2 3 4) => 10 @ 91)
  ((apply #'+ '(1 2 3 4)) => 10 @ 91)
  ((apply #'+ 1 2 '(3 4)) => 10 @ 91)
  ((eval '(+ 1 2 3 4)) => 10 @ 91)
  (:section "3.16 Closures")
  "In the general case, a function consists of the body of the function"
  "coupled with any free lexical variables that the function references."
  "Consider the example:"
  ((mapcar (adder 3) '(1 3 10)) => (4 6 13) @ 92)
  ((mapcar (adder 10) '(1 3 10)) => (11 13 20) @ 92)
  "In the following, two calls to BANK-ACCOUNT create two different closures,"
  "each with a separate value for the lexical variable BALANCE."
  ((setf my-account (bank-account 500.00)) @ 92)
  ((setf your-account (bank-account 250.00)) @ 93)
  ((funcall my-account 'withdraw 75.00) => 425.0)
  ((funcall your-account 'deposit 250.00) => 500.0)
  ((funcall your-account 'withdraw 100.00) => 400.0)
  ((funcall my-account 'withdraw 25.00) => 400.0)
  "This style of programming is covered in more detail in chapter 13."
  )

(defexamples 4 "GPS: The General Problem Solver" 
  "The General problem Solver, developed in 1957 by Alan Newell and Herbert"
  "Simon, embodied a grandiose vision: a single computer program that could"
  "solve ANY problem.  GPS caused quite a stir ..."
  (:section "4.4 Stage 4: test")
  ((requires "gps1"))
  "Here are some examples of using GPS"
  "The first example works with a complex chain of steps."
  ((gps '(son-at-home car-needs-battery have-money have-phone-book)
       '(son-at-school)
       *school-ops*) => SOLVED @ 118)
  "The next example fails because there is no way to make the car work,"
  "because we can't contact the shop to get the battery fixed."
  ((gps '(son-at-home car-needs-battery have-money)
       '(son-at-school)
       *school-ops*) => NIL)
  "The third example is easy, because the car is currently working."
  ((gps '(son-at-home car-works)
       '(son-at-school)
       *school-ops*) => SOLVED)

  (:section "4.7 The Clobbered Sibling Goal Problem")
  "In the next example, GPS incorrectly reports success, when in fact it has"
  "spent the money on the battery, and thus should fail."
  ((gps '(son-at-home have-money car-works)
       '(have-money son-at-school)
       *school-ops*) => SOLVED @ 120)
  "The bug is that when (EVERY #'ACHIEVE GOALS) returns true, it means all the"
  "goals were achieved in turn, but they might not still be all true."

  (:section "4.8 The Leaping before You Look Problem") 
  "What happens if we move the HAVE-MONEY goal to the end?"
  ((gps '(son-at-home car-needs-battery have-money have-phone-book)
       '(have-money son-at-school)
       *school-ops*) => SOLVED @ 121)
  "GPS returns nil, but only after executing all the actions."
  "I call this the 'leaping before you look' problem, because if you asked"
  "the program to solve for the two goals (JUMP-OFF-CLIFF LAND-SAFELY) it"
  "would happily jump first, only to discover that it had no operator to land"
  "safely.  This is less than prudent behavior."

  (:section "4.9 The Recursive Subgoal Problem")
  "We won't show the problem (because it gets into an infinite loop),"
  "but we will add the new operator to the *school-ops*; we'll use it later."
  ((push (make-op :action 'ask-phone-number
               :preconds '(in-communication-with-shop)
               :add-list '(know-phone-number))
	 *school-ops*) @ 122)

  (:section "4.11 GPS Version 2: A More General problem Solver")
  "At this point we are ready to put together a new version of GPS with"
  "solutions for the 'running around the block,' 'prerequisite clobbers"
  "sibling goal,' 'leaping before you look,' and 'recursive subgoal' problems."
  "The most important change is that, instead of printing a message when each"
  "operator is applied, we will instead have GPS return the resulting state."
  ((requires "gps"))
  "We use the list of operators that includes the 'asking the shop their"
  "phone number' operator." 
  ((push (make-op :action 'ask-phone-number
               :preconds '(in-communication-with-shop)
               :add-list '(know-phone-number))
      *school-ops*))
  ((use *school-ops*) => 7 @ 130)
 "First we make sure the new version works on some of the examples that"
 "version 1 worked on:"
  ((gps '(son-at-home car-needs-battery have-money have-phone-book)
	'(son-at-school)) =>
	((START)
	 (EXECUTING LOOK-UP-NUMBER) 
	 (EXECUTING TELEPHONE-SHOP)
	 (EXECUTING TELL-SHOP-PROBLEM)
	 (EXECUTING GIVE-SHOP-MONEY)
	 (EXECUTING SHOP-INSTALLS-BATTERY)
	 (EXECUTING DRIVE-SON-TO-SCHOOL)) @ 131)
  "We can see what is going on here by turning on debugging temporarily:"
  ((debug :gps))
  ((gps '(son-at-home car-needs-battery have-money have-phone-book)
	'(son-at-school)) =>
	((START)
	 (EXECUTING LOOK-UP-NUMBER) 
	 (EXECUTING TELEPHONE-SHOP)
	 (EXECUTING TELL-SHOP-PROBLEM)
	 (EXECUTING GIVE-SHOP-MONEY)
	 (EXECUTING SHOP-INSTALLS-BATTERY)
	 (EXECUTING DRIVE-SON-TO-SCHOOL)) @ 131)
  ((undebug))
  "Here is another old example:"
  ((gps '(son-at-home car-works)
       '(son-at-school)) =>
       ((START)
	(EXECUTING DRIVE-SON-TO-SCHOOL)) @ 132)
  "Now we see that version 2 can handle the three cases version 1 got wrong."
  "In each case the program avoids an infinite loop, and also avoids leaping"
  "before it looks."
  ((gps '(son-at-home car-needs-battery have-money have-phone-book)
       '(have-money son-at-school)) => NIL)
  ((gps '(son-at-home car-needs-battery have-money have-phone-book)
       '(son-at-school have-money)) => NIL)
  ((gps '(son-at-home car-needs-battery have-money)
       '(son-at-school)) => NIL)
  "Finally, we see the new GPS also works on trivial problems:"
  ((gps '(son-at-home) '(son-at-home)) => ((START)))

  (:section "4.12 The New Domain Problem: Monkey and Bananas")
  "To show that GPS is at all general, we have to make it work in different"
  "domains.  We start with a 'classic' AI problem: Monkey and Bananas"
  ((use *banana-ops*) => 6 @ 133)
  "We pose the problem of becoming not-hungry, given an initial state."
  "GPS can find a solution to this problem:"
  ((GPS '(at-door on-floor has-ball hungry chair-at-door)
	'(not-hungry)) =>
	((START)
	 (EXECUTING PUSH-CHAIR-FROM-DOOR-TO-MIDDLE-ROOM)
	 (EXECUTING CLIMB-ON-CHAIR)
	 (EXECUTING DROP-BALL)
	 (EXECUTING GRASP-BANANAS)
	 (EXECUTING EAT-BANANAS)) @ 133)
  "Notice we did not need to make any changes at all to the GPS program."
  "We just used a different set of operators."

  (:section "4.13 The Maze Searching Domain")
  "Next we will consider another 'classic' problem, maze searching."
  "We will assume a particular maze, diagrammed on page 134."
  ((use *maze-ops*) => 48 @ 134)
  ((gps '((at 1)) '((at 25))) @ 135)

  "We can define FIND-PATH to use the results of a GPS search:"
  ((find-path 1 25) @ 136 => 
   (1 2 3 4 9 8 7 12 11 16 17 22 23 24 19 20 25))
  ((find-path 1 1) => (1))
  ((equal (find-path 1 25) (reverse (find-path 25 1))) => T)

  (:section "4.14 The Blocks World Domain")
  "Another domain that has attracted more than its share of attention in AI"
  "circles is the blocks world domain."
  ((use (make-block-ops '(a b))) => 4 @ 137)
  "The simplest possible problem is stacking one block on another."
  ((gps '((a on table) (b on table) (space on a) (space on b)
         (space on table))
       '((a on b) (b on table))) =>
       ((START)
	(EXECUTING (MOVE A FROM TABLE TO B))))
  "Here is a slightly more complex problem: inverting a stack of two blocks."
  "This time we show the debugging output:"
  ((debug :gps) @ 138)
  ((gps '((a on b) (b on table) (space on a) (space on table))
       '((b on a))) =>
       ((START)
	(EXECUTING (MOVE A FROM B TO TABLE))
	(EXECUTING (MOVE B FROM TABLE TO A))))
  ((undebug))
  "Now we move on to the three block world."
  ((use (make-block-ops '(a b c))) => 18)
  "We try some problems:"
  ((gps '((a on b) (b on c) (c on table) (space on a) (space on table))
       '((b on a) (c on b))) =>
       ((START)
	(EXECUTING (MOVE A FROM B TO TABLE))
	(EXECUTING (MOVE B FROM C TO A))
	(EXECUTING (MOVE C FROM TABLE TO B))))
  ((gps '((c on a) (a on table) (b on table)
         (space on c) (space on b) (space on table))
       '((c on table) (a on b))) =>
       ((START)
	(EXECUTING (MOVE C FROM A TO TABLE))
	(EXECUTING (MOVE A FROM TABLE TO B))) @ 141)
  ((gps '((a on b) (b on c) (c on table) (space on a) (space on table))
	'((b on a) (c on b))) @ 141 =>
	((START)
	 (EXECUTING (MOVE A FROM B TO TABLE))
	 (EXECUTING (MOVE B FROM C TO A))
	 (EXECUTING (MOVE C FROM TABLE TO B))))

  ((gps '((a on b) (b on c) (c on table) (space on a) (space on table))
	'((c on b) (b on a))) =>
	((START)
	 (EXECUTING (MOVE A FROM B TO TABLE))
	 (EXECUTING (MOVE B FROM C TO A))
	 (EXECUTING (MOVE C FROM TABLE TO B))))
  "The Sussman Anomaly"
  ((setf start '((c on a) (a on table) (b on table) (space on c)
                (space on b) (space on table))) @ 142)
  ((gps start '((a on b) (b on c))) => NIL)
  ((gps start '((b on c) (a on b))) => NIL)

  (:section "4.16 The Not Looking after You Don't Leap Problem")
  ((use (push (op 'taxi-son-to-school
               :preconds '(son-at-home have-money)
               :add-list '(son-at-school)
               :del-list '(son-at-home have-money))
           *school-ops*)) @ 143)
  ((debug :gps))
  ((gps '(son-at-home have-money car-works)
       '(son-at-school have-money)) => NIL)
  ((undebug))
  )

(defexamples 5 "Eliza: Dialog with a Machine"
  "ELIZA was one of the first programs to feature English output as well as input."
  "The program was named after the heroine of Pygmalion, who was taught to"
  "speak proper English by a dedicated teacher."
  (:section "5.2 Pattern Matching")
  ((requires "eliza1"))
  "The hard part is the notion of pattern matching and transformation."
  "All symbols beginning with ? are variables for the pattern matcher."
  "First we see how to substitute variable/value pairs into expressions:"
  ((sublis '((?X . vacation)) '(what would it mean to you if you got a ?X ?))
   => (what would it mean to you if you got a VACATION ?) @ 156)
  "Now a version of pat-match that works with such pairs:"
  ((pat-match '(I need a ?x) '(I need a vacation))  @ 158)
  "Showing how to plug it in:"
  ((sublis (pat-match '(I need a ?x) '(I need a vacation)) 
	   '(what would it mean to you if you got a ?X ?))
   => (what would it mean to you if you got a VACATION ?) @ 159)
  ((pat-match '(I need a ?x) '(I really need a vacation)) => nil)
  ((pat-match '(this is easy) '(this is easy)) => ((t . t)))
  ((pat-match '(?x is ?x) '((2 + 2) is 4)) => nil)
  ((pat-match '(?x is ?x) '((2 + 2) is (2 + 2))) => ((?x 2 + 2)))
  ((pat-match '(?P need . ?X) '(I need a long vacation))
   => ((?X a long vacation) (?P . I)))

  (:section "5.3 Segment Pattern Matching")
  "We show how to have a variable that will match more than one element."
  "We call these segment variables, and denote them (?* name)."
  ((pat-match '((?* ?p) need (?* ?x))
	      '(Mr Hulot and I need a vacation)) @ 160)
  (:section "5.4 The Eliza Program: A Rule-Based Translator")
  ((requires "eliza"))
  "We can't show you an interactive ELIZA session, because the replies are"
  "random, and thus change every time.  You can experiment on your own by"
  "evaluating (ELIZA) and typing in your end of the conversation.
  Type (good bye) when you are done."
  )

(defexamples 6 "Building Software Tools"
  "In chapters 4 and 5 we were concerned with buildinng two particular"
  "programs, GPS and ELIZA.  In this chapter, we will reexamine those"
  "two programs to discover some common patterns.  Those patterns will be"
  "abstracted out to form reusable software tools."
  (:section "6.2 A Pattern-Matching tool")
  ((requires "patmatch"))
  ((pat-match '(x = (?is ?n numberp)) '(x = 34)) => ((?n . 34)) @ 179)
  ((pat-match '(x = (?is ?n numberp)) '(x = x)) => NIL)
  ((pat-match '(?x (?or < = >) ?y) '(3 < 4)) => ((?Y . 4) (?X . 3)))
  ((pat-match '(x = (?and (?is ?n numberp) (?is ?n oddp))) '(x = 3)) 
   => ((?N . 3)))
  ((pat-match '(?x /= (?not ?x)) '(3 /= 4)) => ((?X . 3)) @ 180)
  ((pat-match '(?x > ?y (?if (> ?x ?y))) '(4 > 3)) => ((?Y . 3) (?X . 4)))
  ((pat-match '(a (?* ?x) d) '(a b c d)) => ((?X B C)) @ 185)
  ((pat-match '(a (?* ?x) (?* ?y) d) '(a b c d)) => ((?Y B C) (?X)))
  ((pat-match '(a (?* ?x) (?* ?y) ?x ?y) '(a b c d (b c) (d))) 
   => ((?Y D) (?X B C)) @ 186)
  ((pat-match '(?x ?op ?y is ?z (?if (eql (funcall ?op ?x ?y) ?z))) 
	      '(3 + 4 is 7))
   => ((?Z . 7) (?Y . 4) (?OP . +) (?X . 3)))
  ((pat-match '(?x ?op ?y (?if (funcall ?op ?x ?y))) '(3 > 4)) => NIL)
  ((pat-match-abbrev '?x* '(?* ?x)) => (?* ?X) @ 187)
  ((pat-match-abbrev '?y* '(?* ?y)) => (?* ?Y))
  ((setf axyd (expand-pat-match-abbrev '(a ?x* ?y* d)))
   => (A (?* ?X) (?* ?Y) D))
  ((pat-match axyd '(a b c d)) => ((?Y B C) (?X)))
  ((pat-match '(((?* ?x) (?* ?y)) ?x ?y) '((a b c d) (a b) (c d))) 
   => NIL)
  ((requires "eliza-pm"))

  (:section "6.4 A Set of Searching Tools")
  ((requires "search"))
  ((debug :search) @ 192)
  "We can search through the binary tree, looking for, say, 12 as the goal."
  "With breadth-first search this would yield an infinite loop, so we won't"
  "do it.  Breadth-first search works better:"
  ((breadth-first-search 1 (is 12) 'binary-tree) => 12 @ 193)
  ((depth-first-search 1 (is 12) (finite-binary-tree 15)) => 12 @ 193)
  "Guiding the Search"
  "Best-first search takes an additional argument which estimates how close"
  "we are to the goal.  We call this the cost function."
  ((best-first-search 1 (is 12) #'binary-tree (diff 12)) => 12 @ 195)
  ((best-first-search 1 (is 12) #'binary-tree (price-is-right 12)) => 12)
  "The function beam-search is just like best-first-search, except that after"
  "we sort the states, we then take only the first beam-width states."
  ((beam-search 1 (is 12) #'binary-tree (price-is-right 12) 2) => 12)
  "As a concrete example of a problem that can be solved by search,"
  "consider planning a flight across North America in a plane whose range is"
  "limited to 1000 kilometers.  Here we plan a trip from SF to Boston."
  ((path-state (trip (city 'san-francisco) (city 'boston)))
   => (BOSTON 71.05 42.21) @ 199)
  ((path-state (trip (city 'boston) (city 'san-francisco)))
   => (SAN-FRANCISCO 122.26 37.47))
  ((undebug :search))
  ((show-city-path (trip (city 'san-francisco) (city 'boston) 1)) @ 201)
  ((show-city-path (trip (city 'boston) (city 'san-francisco) 1)))
  ((show-city-path (trip (city 'boston) (city 'san-francisco) 3)) @ 202)
  ((iter-wide-search 1 (is 12) (finite-binary-tree 15) (diff 12))  => 12 @ 205)
  ((tree-search '(1) (is 6) #'next2 #'prepend) => 6 @ 208)
  ((graph-search '(1) (is 6) #'next2 #'prepend) => 6)
  ((path-states
    (a*-search (list (make-path :state 1)) (is 6) 
               #'next2 #'(lambda (x y) 1) (diff 6))) => (6 5 3 1) @ 210)
  (:section "6.5 GPS as Search")
  ((requires "gps-srch"))
  ((setf start '((c on a) (a on table) (b on table) (space on c)
                 (space on b) (space on table))) @ 213)
  ((use (make-block-ops '(a b c))) => 18)
  ((search-gps start '((a on b) (b on c)))
   => ((START)
       (EXECUTING (MOVE C FROM A TO TABLE))
       (EXECUTING (MOVE B FROM TABLE TO C))
       (EXECUTING (MOVE A FROM TABLE TO B))) @ 213)
  ((search-gps start '((b on c) (a on b)))
   => ((START)
       (EXECUTING (MOVE C FROM A TO TABLE))
       (EXECUTING (MOVE B FROM TABLE TO C))
       (EXECUTING (MOVE A FROM TABLE TO B))))
  )

(defexamples 7 "STUDENT: Solving Algebra Word Problems"
  "STUDENT was another early language understanding program, written by Daniel"
  "Bobrow in 1964.  It was designed to read and solve the kind of word"
  "problems found in high school algebra books."
  (:section "7.1 Translating English into Equations")
  ((requires "student"))
  ((translate-to-expression '(if z is 3 |,| what is twice z))
   => ((= z 3) (= what (* 2 z))) @ 222)
  (:section "7.2 Solving Algebra Equations")
  ((trace isolate solve) @ 229)
  ((solve-equations '((= (+ 3 4) (* (- 5 (+ 2 x)) 7))
                   (= (+ (* 3 x) y) 12))) => nil)
  ((untrace isolate solve))
  (:section "7.3 Examples")
  ((student '(If the number of customers Tom gets is twice the square of
           20 % of the number of advertisements he runs |,| 
           and the number of advertisements is 45 |,|
           then what is the number of customers Tom gets ?)) => nil @ 231)
  ((student '(The daily cost of living for a group is the overhead cost plus 
           the running cost for each person times the number of people in 
           the group |.|  This cost for one group equals $ 100 |,|
           and the number of people in the group is 40 |.|
           If the overhead cost is 10 times the running cost |,|
           find the overhead and running cost for each person |.|)))
  ((student '(Fran's age divided by Robin's height is one half Kelly's IQ |.|
           Kelly's IQ minus 80 is Robin's height |.|
           If Robin is 4 feet tall |,| how old is Fran ?)))
  ((student '(Fran's age divided by Robin's height is one half Kelly's IQ |.|
           Kelly's IQ minus 80 is Robin's height |.|
           If Robin is 0 feet tall |,| how old is Fran ?)))
  )

(defexamples 8 "Symbolic Mathematics: A Simplification Program"
  "'Symbolic mathematics' is to numerical mathematics as algebra is to"
  "arithmetic: it deals with variables and expressions, not just numbers."
  "This chapter develops a program that simplifies algebraic expressions."
  "We then show that differentiation and even integration can be seen as"
  "special cases of 'simplification.'  (Note that we replace calls to the"
  "interactive function SIMPLIFIER with calls to the function SIMP.)"
  (:section "8.2 Simplification Rules")
  ((requires "macsymar"))
  ((simp '(2 + 2)) => 4 @ 245)
  ((simp '(5 * 20 + 30 + 7)) => 137 )
  ((simp '(5 * x - (4 + 1) * x)) => 0 )
  ((simp '(y / z * (5 * x - (4 + 1) * x))) => 0 )
  ((simp '((4 - 3) * x + (y / y - 1) * z)) => X )
  ((simp '(1 * f(x) + 0)) => (F X) )

  (:section "8.3 Associativity and Commutativity")
  ((simp '(3 * 2 * x)) => (6 * X) @ 247)
  ((simp '(2 * x * x * 3)) => (6 * (X ^ 2)) )
  ((simp '(2 * x * 3 * y * 4 * z * 5 * 6)) => (720 * (X * (Y * Z))) )
  ((simp '(3 + x + 4 + x)) => ((2 * X) + 7) )
  ((simp '(2 * x * 3 * x * 4 * (1 / x) * 5 * 6)) => (720 * X))

  (:section "8.4 Logs, Trig, and Differentiation")
  ((simp '(d (x + x) / d x)) => 2 @ 250)
  ((simp '(d (a * x ^ 2 + b * x + c) / d x)) => ((2 * (A * X)) + B) )
  "For the next one, note we had an error in the first printing of the book;"
  "the sign was reversed on the (d (u / v) ...) rule."
  ((simp '(d ((a * x ^ 2 + b * x + c) / x) / d x)) 
   => (((X * ((2 * (A * X)) + B)) - ((A * (X ^ 2)) + ((B * X) + C))) /
       (X ^ 2)))
  ((simp '(log ((d (x + x) / d x) / 2))) => 0 )
  ((simp '(log(x + x) - log x)) => (LOG 2))
  ((simp '(x ^ cos pi)) => (1 / X) )
  "These next two examples were also affected by the (d (u / v) ...) rule."
  ((simp '(d (3 * x + (cos x) / x) / d x)) 
   => ((((X * (- (SIN X))) - (COS X)) / (X ^ 2)) + 3))
  ((simp '(d ((cos x) / x) / d x)) 
   => (((X * (- (SIN X))) - (COS X)) / (X ^ 2)))
  ((simp '(d (3 * x ^ 2 + 2 * x + 1) / d x)) => ((6 * X) + 2))
  ((simp '(sin(x + x) ^ 2 + cos(d x ^ 2 / d x) ^ 2)) => 1 )
  ((simp '(sin(x + x) * sin(d x ^ 2 / d x) +
	      cos(2 * x) * cos(x * d 2 * y / d y))) => 1 )

  (:section "8.5 Limits of Rule-Based Approaches")
  "In this section we return to some examples that pose problems."
  "For the following, we would prefer (2 * (x + y))"
  ((simp '(x + y + y + x)) => (X + (Y + (Y + X))))
  "For the following, we would prefer (7 * X) and (Y + (8 * X)), respectively:"
  ((simp '(3 * x + 4 * x)) => ((3 * X) + (4 * X)))
  ((simp '(3 * x + y + x + 4 * x)) => ((3 * X) + (Y + (X + (4 * X)))) )
  "In chapter 15, we develop a new version of the program that handles this problem."

  (:section "8.6 Integration")
  ((set-simp-fn 'Int #'(lambda (exp) 
			(integrate (exp-lhs exp) (exp-rhs exp)))) @ 258)
  ((simp '(Int x * sin(x ^ 2) d x)) => (1/2 * (- (COS (X ^ 2)))) )
  ((simp '(Int ((3 * x ^ 3) - 1 / (3 * x ^ 3)) d x)) 
   => ((3 * ((X ^ 4) / 4)) - (1/3 * ((X ^ -2) / -2))) )
  ((simp '(Int (3 * x + 2) ^ -2/3 d x)) => (((3 * X) + 2) ^ 1/3) )
  ((simp '(Int sin(x) ^ 2 * cos(x) d x)) => (((SIN X) ^ 3) / 3) )
  ((simp '(Int sin(x) / (1 + cos(x)) d x)) => (-1 * (LOG ((COS X) + 1))) )
  ((simp '(Int (2 * x + 1) / (x ^ 2 + x - 1) d x)) 
   => (LOG ((X ^ 2) + (X - 1))) )
  ((simp '(Int 8 * x ^ 2 / (x ^ 3 + 2) ^ 3 d x)) 
   => (8 * ((1/3 * (((X ^ 3) + 2) ^ -2)) / -2)) )
  ((set-simp-fn 'Int 
	       #'(lambda (exp)
		   (unfactorize
		    (factorize
		     (integrate (exp-lhs exp) (exp-rhs exp)))))) @ 259)
  ((simp '(Int 8 * x ^ 2 / (x ^ 3 + 2) ^ 3 d x)) 
   => (-4/3 * (((X ^ 3) + 2) ^ -2)) )
  )

(defexamples 9 "Efficiency Issues"
  "One of the reasons Lisp has enjoyed a long history is because it is an"
  "ideal language for what is called rapid-prototyping or rapid development."
  "Most real AI programs deal with large amounts of data.  Thus, efficiency"
  "is important.  This chapter shows some ways to make programs efficient."
  (:section "9.1 Caching Results of Previous Computations: Memoization")
  ((defun fib (n) (if (<= n 1) 1 (+ (fib (- n 1)) (fib (- n 2))))) @ 269)
  ((setf memo-fib (memo #'fib)) @ 270)
  ((trace fib))
  ((funcall memo-fib 3) => 3 @ 270)
  ((funcall memo-fib 3) => 3)
  ((untrace fib))
  ((memoize 'fib) @ 272)
  ((trace fib))
  ((fib 5) => 8)
  ((fib 5) => 8)
  ((fib 6) => 13)
  ((untrace fib))
)

(defexamples 10 "Low-Level Efficiency Issues"
  "The efficiency techniques of the previous chapter all involved fairly"
  "significant changes to an algorithm.  But what happens when you are already"
  "using the best imaginable algorithms, and performance is still a problem?"
  (:section "10.1 Use Declarations")
  "Compare these functions with and without declarations:"
  ((defun f (x y)
     (declare (fixnum x y) (optimize (safety 0) (speed 3)))
     (the fixnum (+ x y))) @ 318)
  ((defun g (x y) (+ x y)))
  "Here is the disassembled code for f and g:"
  ((disassemble 'f))
  ((disassemble 'g) @ 319)
)

(defexamples 11 "Logic Programming"
  "The idea behind logic programming is that the programmer should state the"
  "relationships that describe a problem and its solution."
  "In this chapter we develop an interpreter for the Prolog language."

  (:section "11.1 Idea 1: A Uniform Data Base")
  ((requires "prolog1"))
  "First let's make sure we're dealing with a brand new database."
  ((clear-db))
  "Facts are entered into the data base with the <- macro"
  ((<- (likes Kim Robin)) @ 350)
  ((<- (likes Sandy Lee)))
  ((<- (likes Sandy Kim)))
  ((<- (likes Robin cats)))
  "We can also enter rules, which state contingent facts."
  ((<- (likes Sandy ?x) (likes ?x cats)) @ 351)
  ((<- (likes Kim ?x) (likes ?x Lee) (likes ?x Kim)))

  (:section "11.2 Idea 2: Unification of Logic Variables")
  ((requires "unify"))
  ((pat-match '(?x + ?y) '(2 + 1)) => ((?y . 1) (?x . 2)) @ 352)
  ((unify '(?x + 1) '(2 + ?y)) => ((?y . 1) (?x . 2)))
  ((unify '(f ?x) '(f ?y)) => ((?x . ?y)))
  ((unify '(?a + ?a = 0) '(?x + ?y = ?y)) => ((?y . 0) (?x . ?y) (?a . ?x)))
  ((unifier '(?a + ?a = 0) '(?x + ?y = ?y)) => (0 + 0 = 0))
  "Let's try UNIFY on some (more) examples:"
  ((unify '(?x ?y a) '(?y ?x ?x)) => ((?y . a) (?x . ?y)) @ 357)
  ((unify '?x '(f ?x)) => nil)
  ((unify 'a 'a) => ((t . t)))
  "Here are some examples of UNIFIER:"
  ((unifier '(?x ?y a) '(?y ?x ?x)) => (a a a))
  ((unifier '((?a * ?x ^ 2) + (?b * ?x) + ?c) 
	    '(?z + (4 * 5) + 3))
   => ((?a * 5 ^ 2) + (4 * 5) + 3))

  "Programming with Prolog"
  "First we define the MEMBER relation in Prolog:"
  ((<- (member ?item (?item . ?rest))) @ 358)
  ((<- (member ?item (?x . ?rest)) (member ?item ?rest)))
  "Now we can make some queries:"
  ((?- (member 2 (1 2 3))))
  ((?- (member 2 (1 2 3 2 1))))
  ((?- (member ?x (1 2 3))))
  "Let's add one more rule to the Sandy and the cats facts:"
  ((<- (likes ?x ?x)) @ 363)
  "Now we can ask some queries:"
  ((?- (likes Sandy ?who)) @ 365)
  ((?- (likes ?who Sandy)))
  ((?- (likes Robin Lee)))
  ((?- (likes ?x ?y) (likes ?y ?x)) @ 366)

  (:section "11.3 Idea 3: Automatic Backtracking")
  "Now we load the version that does automatic backtracking one step at a time"
  "as opposed to the previous version, which collects all answers at once."
  "Since we don't want to involve you, the user, in typing input to move on"
  "to the next step, we supply the input (a ; or a .) as in the book."
  "Unfortunately, it is not specified in Common Lisp whether read-char echoes"
  "the character it reads, so you may or may not see the ; and . characters."
  ((requires "prolog"))
  "Let's add the definition of the relation LENGTH:"
  ((<- (length () 0)) @ 370)
  ((<- (length (?x . ?y) (1+ ?n)) (length ?y ?n)))
  "Here are some queries:"
  ((?- (length (a b c d) ?n)) :input ";")
  ((?- (length ?list (1+ (1+ 0)))) :input ";")
  ((?- (length ?list ?n)) :input ";;.")
  ((?- (length ?l (1+ (1+ 0))) (member a ?l)) :input ";;")
  "(We won't try the example that leads to an infinite loop.)"
  (:section "11.4 The Zebra Puzzle")
  "First we define the NEXTO and IRIGHT (to the immediate right) relations:"
  ((<- (nextto ?x ?y ?list) (iright ?x ?y ?list)) @ 374)
  ((<- (nextto ?x ?y ?list) (iright ?y ?x ?list)))
  ((<- (iright ?left ?right (?left ?right . ?rest))))
  ((<- (iright ?left ?right (?x . ?rest)) 
       (iright ?left ?right ?rest)))
  ((<- (= ?x ?x)))
  "Now we define the zebra puzzle:"
  ((<- (zebra ?h ?w ?z)
       ;; Each house is of the form:
       ;; (house nationality pet cigarette drink house-color)
       (= ?h ((house norwegian ? ? ? ?)	;1,10
	      ? 
	      (house ? ? ? milk ?) ? ?)) ; 9
       (member (house englishman ? ? ? red) ?h)	; 2
       (member (house spaniard dog ? ? ?) ?h) ; 3
       (member (house ? ? ? coffee green) ?h) ; 4
       (member (house ukrainian ? ? tea ?) ?h) ; 5
       (iright (house ? ? ? ? ivory)	; 6
	       (house ? ? ? ? green) ?h)
       (member (house ? snails winston ? ?) ?h)	; 7
       (member (house ? ? kools ? yellow) ?h) ; 8
       (nextto (house ? ? chesterfield ? ?) ;11
	       (house ? fox ? ? ?) ?h)
       (nextto (house ? ? kools ? ?)	;12
	       (house ? horse ? ? ?) ?h)
       (member (house ? ? luckystrike oj ?) ?h)	;13
       (member (house japanese ? parliaments ? ?) ?h) ;14
       (nextto (house norwegian ? ? ? ?) ;15
	       (house ? ? ? ? blue) ?h)
       (member (house ?w ? ? water ?) ?h) ;Q1
       (member (house ?z zebra ? ? ?) ?h))) ;Q2
  "If you want to test this out, run the following query:"
  "   ((?- (zebra ?houses ?water-drinker ?zebra-owner)))"
  "It is not included as an example because it takes a minute or so to run."
  )


(defexamples 12 "Compiling Logic Programs"
  "This chapter presents a compiler that translates from Prolog to Lisp."
  "Unfortunatley, there's not much to see in terms of examples."
  "But we load the files for you, in case you want to play with them."
  ((requires "prologc1" "prologc2" "prologcp"))
  ((prolog-compile 'likes) @ 389)
  ((prolog-compile 'member))
  )

(defexamples 13 "Object Oriented Programming"
  "It is only natural that a wide range of programming styles have been"
  "introduced to attack the wide range of problems in this book."
  "One style not yet covered is 'object-oriented programming'."
  "Peter Wegner (1987) proposes the following formula as a definition:"
  "Object-orientation = Objects + Classes + Inheritance"

  (:section "13.2 Objects")
  "Now we're ready to get started."
  ((requires "clos"))
  ((setf acct (new-account "J. Random Customer" 1000.00)) @ 438)
  ((send acct 'withdraw 500.00) => 500.0)
  ((send acct 'deposit 123.45) => 623.45)
  ((send acct 'name) => "J. Random Customer")
  ((send acct 'balance) => 623.45)

  (:section "13.4 Classes")
  "Now we define the class ACCOUNT with the define-class macro."
  ((define-class account (name &optional (balance 0.00))
        ((interest-rate .06))
     (withdraw (amt) (if (<= amt balance)
                       (decf balance amt)
                       'insufficient-funds))
     (deposit  (amt) (incf balance amt))
     (balance  ()    balance)
     (name     ()    name)
     (interest ()    (incf balance (* interest-rate balance)))) @ 440)
  "Here are the generic functions defined by this macro:"
  ((setf acct2 (account "A. User" 2000.00)))
  ((deposit acct2 42.00) => 2042.0)
  ((interest acct2) => 2164.52)
  ((balance acct2) => 2164.52 @ 441)
  ((balance acct) => 623.45)

  (:section "13.5 Delegation")
  ((define-class password-account (password acct) ()
     (change-password (pass new-pass)
                      (if (equal pass password)
                        (setf password new-pass)
                        'wrong-password))
     (otherwise (pass &rest args)
                (if (equal pass password)
                  (apply message acct args)
                  'wrong-password))))
  "Now we see how the class PASSWORD-ACCOUNT can be used to provide protection"
  "for an existing account:"
  ((setf acct3 (password-account "secret" acct2)) @ 441)
  ((balance acct3 "secret") => 2164.52)
  ((withdraw acct3 "guess" 2000.00) => WRONG-PASSWORD)
  ((withdraw acct3 "secret" 2000.00) => 164.52)

  (:section "13.7 CLOS: The Common Lisp Object System")
  "Because some Lisp implementations can't convert a structure class into"
  "a CLOS class, nor convert a regular function into a generic function,"
  "we use the names account*, name*, balance*, interest-rate*.  If you were"
  "doing a real application, not just some examples, you would choose one"
  "implementation and get to use the regular names."
  ; ?????? some problems here
  ((defclass account* ()
     ((name :initarg :name :reader name*)
      (balance :initarg :balance :initform 0.00 :accessor balance*)
      (interest-rate :allocation :class :initform .06 
                     :reader interest-rate*))) @ 445)
  ((setf a1 (make-instance 'account* :balance 5000.00
                          :name "Fred")) @ 446)
  ((name* a1) => "Fred")
  ((balance* a1) => 5000.0)
  ((interest-rate* a1) => 0.06)
  ((defmethod withdraw* ((acct account*) amt)
     (if (< amt (balance* acct))
       (decf (balance* acct) amt)
       'insufficient-funds)) @ 446)
  ((defclass limited-account (account*)
     ((limit :initarg :limit :reader limit))))
  ((defmethod withdraw* ((acct limited-account) amt)
     (if (> amt (limit acct))
       'over-limit
       (call-next-method))))
  ((setf a2 (make-instance 'limited-account
                          :name "A. Thrifty Spender"
                          :balance 500.00 :limit 100.00)) @ 447)
  ((name* a2) => "A. Thrifty Spender")
  ((withdraw* a2 200.00) => OVER-LIMIT)
  ((withdraw* a2 20.00) => 480.0)

  (:section "13.8 A CLOS Example: Searching Tools")
  ((defclass problem ()
     ((states :initarg :states :accessor problem-states))) @ 449)
  ((defmethod searcher ((prob problem))
  "Find a state that solves the search problem."
  (cond ((no-states-p prob) fail)
        ((goal-p prob) (current-state prob))
        (t (let ((current (pop-state prob)))
             (setf (problem-states prob)
                   (problem-combiner
                     prob
                     (problem-successors prob current)
                     (problem-states prob))))
           (searcher prob)))))
  ((defmethod current-state ((prob problem))
    "The current state is the first of the possible states."
    (first (problem-states prob))))

  ((defmethod pop-state ((prob problem))
  "Remove and return the current state."
  (pop (problem-states prob))))

  ((defmethod no-states-p ((prob problem))
  "Are there any more unexplored states?"
  (null (problem-states prob))))

  ((defmethod searcher :before ((prob problem))
     (dbg 'search "~&;; Search: ~a" (problem-states prob))) @ 450)

  ((defclass eql-problem (problem)
     ((goal :initarg :goal :reader problem-goal))))

  ((defmethod goal-p ((prob eql-problem))
  (eql (current-state prob) (problem-goal prob))))

  ((defclass dfs-problem (problem) ()
     (:documentation "Depth-first search problem.")))

  ((defclass bfs-problem (problem) ()
     (:documentation "Breadth-first search problem.")))

  ((defmethod problem-combiner ((prob dfs-problem) new old)
     "Depth-first search looks at new states first."
     (append new old)))

  ((defmethod problem-combiner ((prob bfs-problem) new old)
     "Depth-first search looks at old states first."
     (append old new)))

  ((defclass binary-tree-problem (problem) ()) @ 451)

  ((defmethod problem-successors ((prob binary-tree-problem) state)
     (let ((n (* 2 state)))
       (list n (+ n 1)))))

  ((defclass binary-tree-eql-bfs-problem
     (binary-tree-problem eql-problem bfs-problem) ()))

  ((setf p1 (make-instance 'binary-tree-eql-bfs-problem 
                          :states '(1) :goal 12)))
  ((searcher p1) => 12)

  ((defclass best-problem (problem) ()
     (:documentation "A Best-first search problem.")) @ 452)

  ((defmethod problem-combiner ((prob best-problem) new old)
     "Best-first search sorts new and old according to cost-fn."
     (sort (append new old) #'<
           :key #'(lambda (state) (cost-fn prob state)))))

  ((defmethod cost-fn ((prob eql-problem) state)
     (abs (- state (problem-goal prob)))))

  ((defclass beam-problem (problem)
     ((beam-width :initarg :beam-width :initform nil
                  :reader problem-beam-width))))

  ((defmethod problem-combiner :around ((prob beam-problem) new old)
     (let ((combined (call-next-method)))
       (subseq combined 0 (min (problem-beam-width prob) 
                               (length combined))))))

  ((defclass binary-tree-eql-best-beam-problem
     (binary-tree-problem eql-problem best-problem beam-problem) 
     ()))

  ((setf p3 (make-instance 'binary-tree-eql-best-beam-problem 
                          :states '(1) :goal 12 :beam-width 3)))

  ((searcher p3) => 12)

  ((defclass trip-problem (binary-tree-eql-best-beam-problem) 
     ((beam-width :initform 1))) @ 453)

  ((defmethod cost-fn ((prob trip-problem) city)
     (air-distance (problem-goal prob) city)))

  ((defmethod problem-successors ((prob trip-problem) city)
     (neighbors city)))

  ((setf p4 (make-instance 'trip-problem 
                          :states (list (city 'new-york)) 
                          :goal (city 'san-francisco))))

  ((searcher p4) =>
   (SAN-FRANCISCO 122.26 37.47))

  (:section "13.9 Is CLOS Object-oriented?")
  ((defmethod conc ((x null) y) y) @ 454)

  ((defmethod conc (x (y null)) x))

  ((defmethod conc ((x list) (y list))
     (cons (first x) (conc (rest x) y))))

  ((defmethod conc ((x vector) (y vector))
     (let ((vect (make-array (+ (length x) (length y)))))
       (replace vect x)
       (replace vect y :start1 (length x)))))

  ((conc nil '(a b c)) => (A B C) @ 455)
  ((conc '(a b c) nil) => (A B C))
  ((conc '(a b c) '(d e f)) => (A B C D E F))
  ((conc '#(a b c) '#(d e f)) => #(A B C D E F))
  )

(defexamples 14 "Knowledge Representation and Reasoning"
  "In this chapter we explore means of indexing facts so that they can be"
  "retrieved and reasoned with efficiently."
  "Section 14.1 to 14.7 discuss problems with logical reasoning systems"
  "such as Prolog."
  (:section "14.8 A Solution to the Indexing Problem")
  "Here we show how to index facts in a kind of table that makes it easy to"
  "add, delete, and retrieve entries.  We will develop an extension of the"
  "trie or discrimination tree data structure built in section 10.5 (page 344)."
  ((requires "krep1"))
  "Now we define a function to test the indexing routine.  Compare the output"
  "with figure 14.1 on page 474."
  ((test-index) @ 478)
  "Here is an example of fetching from the index"
  ((fetch '(p ? c)) @ 480 =>
   (((P B C) (P A C))
    ((P A ?X))))
  "We can make a change to rename variables before indexing facts."
  ((defun index (key)
     "Store key in a dtree node.  Key must be (predicate . args);
  it is stored in the predicate's dtree."
     (dtree-index key (rename-variables key) ; store unique vars
		  (get-dtree (predicate key)))) @ 481)
  "We have to reindex:"
  ((test-index))
  "We are now ready to test the retrieval mechanism:"
  ((fetch '(p ?x c)) @ 481)
  ((retrieve '(p ?x c)) @ 481)
  ((retrieve-matches '(p ?x c)) =>
   ((P A C) (P A C) (P B C)))
  ((retrieve-matches '(p ?x (?fn c))) =>
   ((P A (?FN C)) (P A (F C)) (P B (F C))))
  ((query-bind (?x ?fn) '(p ?x (?fn c))
	       (format t "~&P holds between ~a and ~a of c." ?x ?fn)) @ 482)

  (:section "14.10 Solutions to the Expressiveness Problems")
  "In this section we introduce a frame-like language, using the primitives"
  "sub, rel, ind, val, and and."
  ((requires "krep"))
  "We add some facts about dogs and bears, both as individuals and species:"
  ((add-fact '(sub dog animal)) @ 488)
  ((add-fact '(sub bear animal)))
  ((add-fact '(ind Fido dog)))
  ((add-fact '(ind Yogi bear)))
  ((add-fact '(val color Yogi brown)))
  ((add-fact '(val color Fido golden)))
  ((add-fact '(val latin-name bear ursidae)))
  ((add-fact '(val latin-name dog canis-familiaris)))
  "Now retrieve-fact is used to answer three questions: What kinds of animals"
  "are there?"
  ((retrieve-fact '(sub ?kind animal)) =>
   (((?KIND . DOG))
    ((?KIND . BEAR))))
  "What are the Latin names of each kind of animal?"
  ((retrieve-fact '(and (sub ?kind animal)
                       (val latin-name ?kind ?latin))) =>
   (((?LATIN . CANIS-FAMILIARIS) (?KIND . DOG))
    ((?LATIN . URSIDAE) (?KIND . BEAR))))
  "What are the colors of each individual bear?"
  ((retrieve-fact '(and (ind ?x bear) (val color ?x ?c))) @ 489 =>
   (((?C . BROWN) (?X . YOGI))))
  ((test-bears) @ 492)
  )

(defexamples 15 "Symbolic Mathematics with Canonical Forms"
  "This chapter uses a canonical representation for polynomials"
  "to achieve a more efficient program than the rules-based one in Chapter 8."
  (:section "15.1 A Canonical Form for Polynomials")
  ((requires "cmacsyma"))
  "We represent polynomials as vectors, with the variable in element 0,"
  "and the coefficients starting in element 1 and going up from there."
  "Here is the representation of 5x^3 + 10x^2 + 20x + 30"
  ('#(x 30 20 10 5) @ 511)
  "Here are some examples (without the interactive loop):"
  ((canon '(3 + x + 4 - x)) => 7 @ 521)
  ((canon '(x + y + y + x)) => ((2 * x) + (2 * y)))
  ((canon '(3 * x + 4 * x)) => (7 * x))
  ((canon '(3 * x + y + x + 4 * x)) => ((8 * x) + y))
  ((canon '((x + 1) ^ 10)) =>
   ((x ^ 10) + (10 * (x ^ 9)) + (45 * (x ^ 8)) + (120 * (x ^ 7))
    + (210 * (x ^ 6)) + (252 * (x ^ 5)) + (210 * (x ^ 4))
    + (120 * (x ^ 3)) + (45 * (x ^ 2)) + (10 * x) + 1))
  ((canon '((x + 1) ^ 10 - (x - 1) ^ 10)) =>
   ((20 * (x ^ 8)) + (240 * (x ^ 7)) + (504 * (x ^ 5))
    + (240 * (x ^ 3)) + (20 * x)))
  ((canon '(d (3 * x ^ 2 + 2 * x + 1) / d x)) @ 522 =>
   ((6 * x) + 2))
  ((canon '(d (z + 3 * x + 3 * z * x ^ 2 + z ^ 2 * x ^ 3) / d z)) =>
   (((2 * z) * (x ^ 3)) + (3 * (x ^ 2)) + 1)))


(defexamples 16 "Expert Systems"
  "In this chapter we develop an expert system shell, and give it a few rules"
  "about infectious disease, thus duplicating some of the Mycin system."
  ((requires "mycin-r"))
  "Because this is an interactive system, we can't show the interaction here."
  "You can try it yourself by evaluating (mycin)"
  )

(defexamples 17 "Line Diagram Labelling by Constraint Satisfaction"
  "In this chapter we look at the line-diagram labeling problem: Given a list"
  "of lines and the vertexes at which they intersect, how can we determine"
  "what the lines represent?"
  ((requires "waltz"))
  (:section "17.2 Combining Constraints and Searching")
  "First let's test that we can find the possible labelings for a vertex class:"
  ((possible-labelings 'Y) @ 574 =>
   ((+ + +) (- - -) (L R -) (- L R) (R - L)))
  "Notice how matrix-transpose works:"
  ((matrix-transpose (possible-labelings 'Y)) =>
   ((+ - L - R)
    (+ - R L -)
    (+ - - R L)))
  ((defdiagram cube
     (a Y b c d)
     (b W g e a)
     (c W e f a)
     (d W f g a)
     (e L c b)
     (f L d c)
     (g L b d)) @ 575)
  (:section "17.3 Labelling Diagrams")
  "We are now ready to try labelling diagrams.  First the cube:"
  ((print-labelings (diagram 'cube)) @ 577)
  "The cube should have given four solutions."
  "We can get down to one solution by grounding line GD:"
  ((print-labelings (ground (diagram 'cube) 'g 'd)) @ 580)
  "For the more complex cube on a plate, we get similar results;"
  "Four interpretations, which turn to one after grounding line KM:"
  ((defdiagram cube-on-plate
     (a Y b c d)
     (b W g e a)
     (c W e f a)
     (d W f g a)
     (e L c b)
     (f Y d c i)
     (g Y b d h)
     (h W l g j)
     (i W f m j)
     (j Y h i k)
     (k W m l j)
     (l L h k)
     (m L k i)) @ 581)
  ((print-labelings (ground (diagram 'cube-on-plate) 'k 'm)) @ 582)
  "It is interesting to try the algorithm on an 'impossible' diagram."
  "It turns out the algorithm correctly finds no interpretation for this"
  "well-known illusion:"
  ((defdiagram poiuyt
     (a L b g)
     (b L j a)
     (c L d l)
     (d L h c)
     (e L f i)
     (f L k e)
     (g L a l)
     (h L l d)
     (i L e k)
     (j L k b)
     (k W j i f)
     (l W h g c)) @ 583)
  ((print-labelings (diagram 'poiuyt)) @ 583)
  "Now we try a more complex diagram:"
  ((defdiagram tower
     (a Y b c d)    (n L q o) 
     (b W g e a)    (o W y j n)
     (c W e f a)    (p L r i)
     (d W f g a)    (q W n s w)
     (e L c b)      (r W s p x)
     (f Y d c i)    (s L r q)
     (g Y b d h)    (t W w x z)
     (h W l g j)    (u W x y z)
     (i W f m p)    (v W y w z)
     (j Y h o k)    (w Y t v q)
     (k W m l j)    (x Y r u t)
     (l L h k)      (y Y v u o)
     (m L k i)      (z Y t u v)) @ 584)
  ((print-labelings (ground (diagram 'tower) 'l 'k)) @ 584))
  
(defexamples 18 "Search and the Game of Othello"
  "In this chapter we will develop a simplified Othello-playing program."
  "It will not be a champion, but is much better than beginning players."
  (:section "18.2 Representation Choices")
  ((requires "othello"))
  "First, we see that our choices for representing the board seem to work:"
  ((print-board (initial-board)) @ 604)
  "Now we can compare the weighted squares and count difference strategies"
  "by playing two games, alternating who goes first.  The NIL as third argument"
  "means don't print the board after each move."
  ((othello (maximizer #'weighted-squares) 
         (maximizer #'count-difference) nil) @ 610)
  ((othello (maximizer #'count-difference)
            (maximizer #'weighted-squares) nil))

  (:section "18.4 Searching Ahead: Minimax")
  "We can test the minimax strategy, and see that searching ahead 3 ply is"
  "indeed better than looking at only 1 ply.  We can follow the whole game"
  ((othello (minimax-searcher 3 #'count-difference)
            (maximizer #'count-difference)) @ 614 => 53)

  (:section "18.5 Smarter Searching: Alpha-Beta Search")
  "The following should produce the same result, only faster:"
  ((othello (alpha-beta-searcher 3 #'count-difference)
	 (maximizer #'count-difference) nil) => 53)

  (:section "18.8 Playing a Series of Games")
  "A single game is not enough to establish that one strategy is better than"
  "another.  The function RANDOM-OTHELLO-SERIES allows two strategies to"
  "compete in a series of games."
  ((requires "othello2"))
  ((random-othello-series 
    (alpha-beta-searcher 2 #'weighted-squares)
    (alpha-beta-searcher 2 #'modified-weighted-squares)
    5) @ 628)
  "Here is a comparison of five strategies that search only 1 ply."
  "To save time, we run 2 pairs of games each, not 5 pairs."
  ((round-robin
   (list (maximizer #'count-difference)
         (maximizer #'mobility)
         (maximizer #'weighted-squares)
         (maximizer #'modified-weighted-squares)
         #'random-strategy)
   2 10
   '(count-difference mobility weighted modified-weighted random)) @ 629)
  "Now we compare alpha-beta searchers at 3 ply for 1 pair of games each."
  "In the book it was 4 ply for 5 pairs each, but that takes too long."
  ((round-robin
   (list (alpha-beta-searcher 3 #'count-difference)
         (alpha-beta-searcher 3 #'weighted-squares)
         (alpha-beta-searcher 3 #'modified-weighted-squares)
         #'random-strategy)
   1 10 
   '(count-difference weighted modified-weighted random)))
  )

(defexamples 19 "Introduction to Natural Language"
  "This chapter is a brief introduction to natural language processing."
  (:section "19.1 Parsing with a Phrase-Structure Grammar")
  "We start with the grammar defined on page 39 for the GENERATE program."
  "I include 'noun' and 'verb' as nouns in the grammar *grammar3*"
  ((requires "syntax1"))
  (*grammar3* @ 657)
  ((use *grammar3*))
  ((parser '(the table)) => ((NP (ART THE) (NOUN TABLE))))
  ((parser '(the ball hit the table)) =>
   ((SENTENCE (NP (ART THE) (NOUN BALL))
	      (VP (VERB HIT)
		  (NP (ART THE) (NOUN TABLE))))))
  ((parser '(the noun took the verb)) =>
   ((SENTENCE (NP (ART THE) (NOUN NOUN))
	      (VP (VERB TOOK)
		  (NP (ART THE) (NOUN VERB))))))
  "The range of sentences we can parse is quite limited."
  "The following grammar includes a wider variety."
  (*grammar4* @ 661)
  ((use *grammar4*))
  ((parser '(The man hit the table with the ball)) =>
   ((S (NP (D THE) (N MAN))
       (VP (VP (V HIT) (NP (D THE) (N TABLE)))
	   (PP (P WITH) (NP (D THE) (N BALL)))))
    (S (NP (D THE) (N MAN))
       (VP (V HIT)
	   (NP (NP (D THE) (N TABLE))
	       (PP (P WITH) (NP (D THE) (N BALL))))))))
  "Here we see a phrase that is ambiguous between a sentence and a noun phrase:"
  ((parser '(the orange saw)) @ 662 =>
   ((S (NP (D THE) (N ORANGE)) (VP (V SAW)))
    (NP (D THE) (A+ (A ORANGE)) (N SAW))))

  (:section "19.4 The Unknown-Word Problem")
  "As it stands, the parser cannot deal with unknown words."
  "One way of treating unknown words is to allow them to be any of the"
  "'open-class' categories--nouns, verbs, adjectives, and names."
  ((parser '(John liked Mary)) @ 664 =>
   ((S (NP (NAME JOHN))
       (VP (V LIKED) (NP (NAME MARY))))))
  ((parser '(Dana liked Dale)) @ 665 =>
   ((S (NP (NAME DANA))
       (VP (V LIKED) (NP (NAME DALE))))))
  "We see the parser works as well with words it knows (John and Mary)"
  "as with new words (Dana and Dale), which it can recognize as names"
  "because of their position in the sentence."
  ((parser '(the rab zaggled the woogly quax)) =>
   ((S (NP (D THE) (N RAB))
       (VP (V ZAGGLED) (NP (D THE) (A+ (A WOOGLY)) (N QUAX))))))
  ((parser '(the slithy toves gymbled)) =>
   ((S (NP (D THE) (N SLITHY)) (VP (V TOVES) (NP (NAME GYMBLED))))
    (S (NP (D THE) (A+ (A SLITHY)) (N TOVES)) (VP (V GYMBLED)))
    (NP (D THE) (A+ (A SLITHY) (A+ (A TOVES))) (N GYMBLED))))
  ((parser '(the slithy toves gymbled on the wabe)) =>
   ((S (NP (D THE) (N SLITHY))
       (VP (VP (V TOVES) (NP (NAME GYMBLED)))
	   (PP (P ON) (NP (D THE) (N WABE)))))
    (S (NP (D THE) (N SLITHY))
       (VP (V TOVES) (NP (NP (NAME GYMBLED))
			 (PP (P ON) (NP (D THE) (N WABE))))))
    (S (NP (D THE) (A+ (A SLITHY)) (N TOVES))
       (VP (VP (V GYMBLED)) (PP (P ON) (NP (D THE) (N WABE)))))
    (NP (NP (D THE) (A+ (A SLITHY) (A+ (A TOVES))) (N GYMBLED))
	(PP (P ON) (NP (D THE) (N WABE))))))
  (:section "19.5 Parsing into a Semantic Representation")
  ((requires "syntax2"))
  "Syntactic parse trees of a sentence may be interesting, but by themselves"
  "they're not very useful.  We use sentences to communicate ideas, not to"
  "display grammatical structures."
  ""
  "Imagine a compact disc player for which you can punch buttons like"
  "'play 1 to 5 without 3'.  We will define such a language."
  "The meaning of a sentence in the language is the list of tracks played."
  (*grammar5* @ 667)
  ((use *grammar5*))
  ((meanings '(1 to 5 without 3)) @ 669 => ((1 2 4 5)))
  ((meanings '(1 to 4 and 7 to 9)) => ((1 2 3 4 7 8 9)))
  ((meanings '(1 to 6 without 3 and 4)) => ((1 2 4 5 6) (1 2 5 6)))
  "The example '1 to 6 without 3 and 4' is ambiguous."
  "The syntactic ambiguity leads to a semantic ambiguity."
  "We can define a new grammar that eliminates some ambiguities:"
  (*grammar6* @ 669)
  ((use *grammar6*))
  "With this new grammar, we can get single interpretations out of most inputs"
  ((meanings '(1 to 6 without 3 and 4)) => ((1 2 5 6)))
  ((meanings '(1 and 3 to 7 and 9 without 5 and 6)) => ((1 3 4 7 9)))
  ((meanings '(1 and 3 to 7 and 9 without 5 and 2)) => ((1 3 4 6 7 9 2)))
  ((meanings '(1 9 8 to 2 0 1)) => ((198 199 200 201)))
  ((meanings '(1 2 3)) => (123 (123)))

  (:section "19.6 Parsing with Preferences")
  ((requires "syntax3"))
  "We need some compromise between the permissive grammar, which generated"
  "all possible parses, and the restrictive grammar, which eliminates too"
  "many parses.  To get the 'best' interpretation we will need not only a"
  "new grammar, we will also need to modify the program to compare the"
  "relative worth of candidate interpretations."
  (*grammar7* @ 673)
  ((use *grammar7*))
  "We will need a way to show off the prefernce rankings:"
  ((all-parses '(1 to 6 without 3 and 4)) @ 675)
  ((all-parses '(1 and 3 to 7 and 9 without 5 and 6)))
  ((all-parses '(1 and 3 to 7 and 9 without 5 and 2)) @ 676)
  "In each case, the preference rules are able to assign higher scores to"
  "more reasonable interpretations.  What we really want is to pick the best."
  "Here we see some examples:"
  ((meaning '(1 to 5 without 3 and 4)) => (1 2 5))
  ((meaning '(1 to 5 without 3 and 6)) => (1 2 4 5 6))
  ((meaning '(1 to 5 without 3 and 6 shuffled)))
  ((meaning '([ 1 to 5 without [ 3 and 6 ] ] reversed)) => (5 4 2 1))
  ((meaning '(1 to 5 to 9)) => NIL)
  )


(defexamples 20 "Unification Grammars"
  "Prolog was invented as a formalism to describe the grammar of French."
  "It is still useful to view a grammar as a set of logic programming clauses."
  "This chapter describes how that can be done."
  ((requires "unifgram"))
  (:section "20.3 A Simple Grammar in DCG Format")
  "Here is the trivial grammar from page 688 in DCG format:"
  ((clear-db))
  ((rule (S (?pred ?subj)) -->
	 (NP ?agr ?subj)
	 (VP ?agr ?pred)) @ 692)
  ((rule (NP ?agr (?det ?n)) -->
	 (Det ?agr ?det)
	 (N ?agr ?n)))
  ((rule (NP 3sg (the male))          --> (:word he)) @ 693)
  ((rule (NP ~3sg (some objects))     --> (:word they)))
  ((rule (VP 3sg sleep)               --> (:word sleeps)))
  ((rule (VP ~3sg sleep)              --> (:word sleep)))
  ((rule (Det ?any the)               --> (:word the)))
  ((rule (N 3sg (young male human))   --> (:word boy)))
  ((rule (N 3sg (young female human)) --> (:word girl)))
  "We can parse some of the sentences from page 689 (but in DCG format)."
  "Parsing:"
  ((?- (S ?sem (He sleeps) ())) :input ".")
  "Generating:"
  ((?- (S (sleep (the male)) ?words  ())) :input ".")
  "Enumerating:"
  ((?- (S ?sem ?words ())) :input ";;;;")
  "If we want the interpretation of 'Terry kisses Jean' to be"
  "(kiss Terry Jean) not ((lambda (x) (kiss x Jean)) Terry), then we need"
  "a way to unify semantic components together.  Here's one way:"
  ((clear-db))
  ((rule (S ?pred) -->
	 (NP ?agr ?subj)
	 (VP ?agr ?subj ?pred)) @ 694)
  ((rule (VP ?agr ?subj ?pred) -->
	 (Verb/tr ?agr ?subj ?pred ?obj)
	 (NP ?any-agr ?obj)))
  ((rule (VP ?agr ?subj ?pred) -->
	 (Verb/intr ?agr ?subj ?pred)))

  ((rule (Verb/tr ~3sg ?x (kiss ?x ?y) ?y) --> (:word kiss)))
  ((rule (Verb/tr 3sg ?x (kiss ?x ?y) ?y) --> (:word kisses)))
  ((rule (Verb/tr ?any  ?x (kiss ?x ?y) ?y) --> (:word kissed)))

  ((rule (Verb/intr ~3sg ?x (sleep ?x)) --> (:word sleep)))
  ((rule (Verb/intr 3sg ?x (sleep ?x)) --> (:word sleeps)))
  ((rule (Verb/intr ?any  ?x (sleep ?x)) --> (:word slept)))

  "Here are the rules for noun phrases and nouns"
  ((rule (NP ?agr ?sem) -->
	 (Name ?agr ?sem)))
  ((rule (NP ?agr (?det-sem ?noun-sem)) -->
	 (Det ?agr ?det-sem)
	 (Noun ?agr ?noun-sem)))

  ((rule (Name 3sg Terry) --> (:word Terry)))
  ((rule (Name 3sg Jean)  --> (:word Jean)))

  ((rule (Noun 3sg (young male human))           --> (:word boy)) @ 695)
  ((rule (Noun 3sg (young female human))         --> (:word girl)))
  ((rule (Noun ~3sg (group (young male human)))   --> (:word boys)))
  ((rule (Noun ~3sg (group (young female human))) --> (:word girls)))

  ((rule (Det ?any the)  --> (:word the)))
  ((rule (Det 3sg a) --> (:word a)))

  "This grammar and lexicon generates more sentences, although it is still"
  "rather limited.  Here are some examples:"

  ((?- (S ?sem (The boys kiss a girl) ())) @ 695 :input ";.")
  ((?- (S ?sem (The girls kissed the girls) ())) :input ";.")
  ((?- (S ?sem (Terry kissed the girl) ())) :input ";.")
  ((?- (S ?sem (The girls kisses the boys) ())) :input ";.")
  ((?- (S ?sem (Terry kissed a girls) ())) :input ";.")
  ((?- (S ?sem (Terry sleeps Jean) ())) :input ";.")

  (:section "20.4 A DCG Grammar with Quantifiers")
  ((clear-db))
  ((rule (Det ?any ?x ?p ?q (the ?x (and ?p ?q)))    --> (:word the)) @ 697)
  ((rule (Det 3sg  ?x ?p ?q (exists ?x (and ?p ?q))) --> (:word a)))
  ((rule (Det 3sg  ?x ?p ?q (all    ?x (-> ?p ?q)))  --> (:word every)))

  ((rule (Noun 3sg ?x (picture ?x)) --> (:word picture)) @ 698)
  ((rule (Noun 3sg ?x (story ?x)) --> (:word story)))
  ((rule (Noun 3sg ?x (and (young ?x) (male ?x) (human ?x))) -->
	 (:word boy)))

  ((rule (NP ?agr ?x ?pred ?pred) -->
	 (Name ?agr ?name)))

  ((rule (NP ?agr ?x ?pred ?np) -->
	 (Det ?agr ?x ?noun&rel ?pred ?np)
	 (Noun ?agr ?x ?noun)
	 (rel-clause ?agr ?x ?noun ?noun&rel)))

  ((rule (rel-clause ?agr ?x ?np ?np) --> ))
  ((rule (rel-clause ?agr ?x ?np (and ?np ?rel)) -->
	 (:word that)
	 (VP ?agr ?x ?rel)))

  ((rule (Verb/tr ~3sg ?x ?y (paint ?x ?y)) --> (:word paint)) @ 699)
  ((rule (Verb/tr 3sg  ?x ?y (paint ?x ?y)) --> (:word paints)))
  ((rule (Verb/tr ?any ?x ?y (paint ?x ?y)) --> (:word painted)))

  ((rule (Verb/intr ~3sg ?x (sleep ?x)) --> (:word sleep)))
  ((rule (Verb/intr 3sg  ?x (sleep ?x)) --> (:word sleeps)))
  ((rule (Verb/intr ?any ?x (sleep ?x)) --> (:word slept)))

  ((rule (Verb/intr 3sg  ?x (sells ?x)) --> (:word sells)))
  ((rule (Verb/intr 3sg  ?x (stinks ?x)) --> (:word stinks)))

  ((rule (VP ?agr ?x ?vp) -->
	 (Verb/tr ?agr ?x ?obj ?verb)
	 (NP ?any-agr ?obj ?verb ?vp)))

  ((rule (VP ?agr ?x ?vp) -->
	 (Verb/intr ?agr ?x ?vp)))

  ((rule (S ?np) -->
	 (NP ?agr ?x ?vp ?np)
	 (VP ?agr ?x ?vp)))

  "Now we define a function to show the output from a query."
  "In the book, you just saw the output of such a function."
  ((defun do-s (words)
     (top-level-prove `((S ?sem ,words ())))))

  ((do-s '(Every picture paints a story)) :input "." @ 699)
  ((do-s '(Every boy that paints a picture sleeps)) :input ".")
  ((do-s '(Every boy that sleeps paints a picture)) :input ".")
  ((do-s '(Every boy that paints a picture that sells paints a picture 
		 that stinks)) :input "." @ 700)

  (:section "20.5 Preserving Quantifier Scope Ambiguity")
  ((clear-db))
  ((rule (S (and ?np ?vp)) -->
	 (NP ?agr ?x ?np)
	 (VP ?agr ?x ?vp)) @ 701)

  ((rule (VP ?agr ?x (and ?verb ?obj)) -->
	 (Verb/tr ?agr ?x ?o ?verb)
	 (NP ?any-agr ?o ?obj)))

  ((rule (VP ?agr ?x ?verb) -->
	 (Verb/intr ?agr ?x ?verb)))

  ((rule (NP ?agr ?name t) -->
	 (Name ?agr ?name)))

  ((rule (NP ?agr ?x ?det) -->
	 (Det ?agr ?x (and ?noun ?rel) ?det)
	 (Noun ?agr ?x ?noun)
	 (rel-clause ?agr ?x ?rel)))

  ((rule (rel-clause ?agr ?x t) --> ))
  ((rule (rel-clause ?agr ?x ?rel) -->
	 (:word that)
	 (VP ?agr ?x ?rel)))

  ((rule (Name 3sg Terry)                     --> (:word Terry)))
  ((rule (Name 3sg Jean)                      --> (:word Jean)))
  ((rule (Det 3sg  ?x ?restr (all ?x ?restr)) --> (:word every)))
  ((rule (Noun 3sg ?x (man ?x))               --> (:word man)))
  ((rule (Verb/tr 3sg ?x ?y (love ?x ?y))     --> (:word loves)))
  ((rule (Verb/intr 3sg ?x (lives ?x))        --> (:word lives)))
  ((rule (Det 3sg  ?x ?res (exists ?x ?res))  --> (:word a)))
  ((rule (Noun 3sg ?x (woman ?x))             --> (:word woman)))

  "Here is an example of the new representation:"
  ((do-s '(every man loves a woman)) :input "." @ 701)
  )

(defexamples 21 "A Grammar of English"
  ((if (boundp 'clear-db) (clear-db)) @ 715)
  ((requires "grammar" "lexicon"))
  ((prolog-compile-symbols))
  (:section "21.10 Word Categories")
  ((?- (word sees verb ?infl ?senses)) :input ".")
  ((try S John promised Kim to persuade Lee to sleep) :input ";;;.")
  (:section "21.14 Examples")
  ((try S When did John promise Kim to persuade Lee to sleep) 
   @ 746 :input ";;;.")
  ((try S Kim would not have been looking for Lee) @ 747 :input ";;;.")
  ((try s It should not surprise you that Kim does not like Lee) :input ";;;.")
  )

(defexamples 22 "Scheme: An Uncommon Lisp"
  "This chapter presents the Scheme dialect of Lisp and an interpreter for it."
  "Understanding the interpreter can give you a better appreciation of Lisp."
  (:section "22.1 A Scheme Interpreter")
  ((requires "interp1"))
  "We're ready to try out the interpreter.  Note we provide an argument"
  "to avoid going into a read-eval-print loop with SCHEME.  This is a new"
  "functionality, no in the book, added to make these examples easier."
  ((scheme '(+ 2 2)) @ 760 => 4 )
  ((scheme '((if (= 1 2) * +) 3 4)) => 7) 
  ((scheme '((if (= 1 1) * +) 3 4)) => 12 @ 761) 
  ((scheme '(set! fact (lambda (n) (if (= n 0) 1
				     (* n (fact (- n 1))))))))
  ((scheme '(fact 5)) => 120) 
  ((scheme '(set! table (lambda (f start end)
			  (if (<= start end)
			      (begin
			       (write (list start (f start)))
			       (newline)
			       (table f (+ start 1) end)))))))

  ((scheme '(table fact 1 10)) => NIL )
  ((scheme '(table (lambda (x) (* x x x)) 5 10)) => NIL)

  (:section "22.2 Syntactic Extension with Macros")
  "Scheme has a number of special forms that were not listed above."
  "These can be implemented by macros (although macros are not officially"
  "part of Scheme).  We can test out the macro facility:"
  ((scheme-macro-expand '(and p q)) => (IF P (AND Q)) @ 765)
  ((scheme-macro-expand '(and q)) => Q)
  ((scheme-macro-expand '(let ((x 1) (y 2)) (+ x y))) =>
   ((LAMBDA (X Y) (+ X Y)) 1 2))
  ((scheme-macro-expand
    '(letrec 
      ((even? (lambda (x) (or (= x 0) (odd? (- x 1)))))
       (odd?  (lambda (x) (even? (- x 1)))))
      (even? z))))
  "Now let's look at uses of the macros DEFINE and LET*"
  ((scheme '(define (reverse l)
	      (if (null? l) nil
		(append (reverse (cdr l)) (list (car l)))))) => REVERSE)
  ((scheme '(reverse '(a b c d))) => (D C B A))
  ((scheme '(let* ((x 5) (y (+ x x)))
	      (if (or (= x 0) (and (< 0 y) (< y 20)))
		  (list x y)
		(+ y x)))) => (5 10))


  (:section "22.4 Throw, Catch, and Call/cc")
  ((requires "interp3"))
  "Non-local flow of control is provided in Scheme with a very general and"
  "powerful procedure, CALL-WITH-CURRENT-CONTINUATION, which is often"
  "abbreviated CALL/CC.  Here are some examples:"
  ((scheme '(+ 1 (call/cc (lambda (cc) (+ 20 300))))) @ 770 => 321)
  "The above example ignores CC and computes (+ 1 (+ 20 300))"
  "The next example does make use of CC:"
  ((scheme '(+ 1 (call/cc (lambda (cc) (+ 20 (cc 300)))))) => 301)
  "The above passes 300 to CC, thus bypassing the addition of 20."
  "It effectively throws 300 out to the catch point established by call/cc."
  )

(defexamples 23 "Compiling Lisp"
  "Compilers are simple to write and useful to know about."
  "In this chapter we develop a simple compiler for Scheme."
  ""
  ((requires "compile1"))
  "Now we are ready to show the simple compiler at work:"
  ((comp-show '(if (= x y) (f (g x)) (h x y (h 1 2)))) @ 791)
  "Here are some places where a compiler could do better than an interpreter"
  "(although our compiler currently does not):"
  ((comp-show '(begin "doc" (write x) y)) @ 792)
  "We should not have to push 'doc' on the stack just to pop it off."
  "Here's another example:"
  ((comp-show '(begin (+ (* a x) (f x)) x)))
  "Here's an example using local variables:"
  ((comp-show '((lambda (x) ((lambda (y z) (f x y z)) 3 x)) 4)) @ 794)
  (:section "23.1 A Properly Tail-Recursive Compiler")
  "Notice the two new instructions, CALLJ and SAVE"
  ((requires "compile2"))
  "First we see how nested function calls work:"
  ((comp-show '(f (g x))) @ 796)
  "In the next example we see that unneeded constants and variables in BEGIN"
  "expressions are ignored:"
  ((comp-show '(begin "doc" x (f x) y)) @ 797)
  ((comp-show '(begin (+ (* a x) (f x)) x)))
  "Here are some examples of IF expressions:"
  ((comp-show '(if p (+ x y) (* x y))) @ 801)
  "If we put the same code inside a BEGIN we get something quite different:"
  ((comp-show '(begin (if p (+ x y) (* x y)) z)) @ 802)
  "Here are some more examples of the compiler at work:"
  ((comp-show '(if (null? (car l)) (f (+ (* a x) b))
                  (g (/ x 2)))) @ 806)
  ((comp-show '(define (last1 l)
                (if (null? (cdr l)) (car l)
                    (last1 (cdr l))))) @ 807)
  ((comp-show '(define (length l)
                (if (null? l) 0 (+ 1 (length (cdr l)))))) @ 808)
  "Of course, it is possible to write LENGTH in tail-recursive fashion:"
  ((comp-show '(define (length l)
                (letrec ((len (lambda (l n)
                                (if (null? l) n
                                    (len (rest l) (+ n 1))))))
                  (len l 0)))))
  (:section "23.4 A Peephole Optimizer")
  "In this section we investigate a simple technique that will generate"
  "slightly better code in cases where the compiler is less than perfect."
  ((requires "compile3"  "compopt"))
  ((comp-show '(begin (if (if t 1 (f x)) (set! x 2)) x)) @ 818)
  )
