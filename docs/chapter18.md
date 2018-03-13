# Chapter 18 {docsify-ignore}
<a id='page-596'></a>

Search and the 
Game of Othello 

In the beginner's mind there are 
endless possibilities; 
in the expert's there are few. 

-Suzuki Roshi, Zen Master 

G
G
ame playing has been the target of much early work in AI for three reasons. First, 
the rules of most games are formalized, and they can be implemented in a computer 
program rather easily. Second, in many games the interface requirements are trivial. 
The computer need only print out its moves and read in the opponent's moves. This is true for 
games like chess and checkers, but not for ping-pong and basketball, where vision and motor 
skills are crucial. Third, playing a good game of chess is considered by many an intellectual 
achievement. Newell, Shaw, and Simon say, "Chess is the intellectual game par excellence " and 
Donald Michie called chess the "Drosophila melanogaster of machine intelligence," meaning that 
chess is a relatively simple yet interesting domain that can lead to advances in AI, just as study 
of the fruit fly served to advance biology. 

<a id='page-597'></a>
Today there is less emphasis on game playing in AI. It has been realized that 
techniques that work well in the limited domain of a board game do not necessarily 
lead to intelligent behavior in other domains. Also, as it turns out, the techniques 
that allow computers to play well are not the same as the techniques that good 
human players use. Humans are capable of recognizing abstract patterns learned 
from previous games, and formulating plans of attack and defense. While some 
computer programs try to emulate this approach, the more succesful programs 
work by rapidly searching thousands of possible sequences of moves, making fairly 
superficial evaluations of the worth of each sequence. 

While much previous work on game playing has concentrated on chess and 
checkers, this chapter demonstrates a program to play the game of Othello.^ Othello 
is a variation on the nineteenth-century game Reversi. It is an easy game to program 
because the rules are simpler than chess. Othello is also a rewarding game to 
program, because a simple search technique can yield an excellent player. There 
are two reasons for this. First, the number of legal moves per turn is low, so the 
search is not too explosive. Second, a single Othello move can flip a dozen or more 
opponent pieces. This makes it difficult for human players to visualize the long-range 
consequences of a move. Search-based programs are not confused, and thus do well 
relative to humans. 

The very name "Othello" derives from the fact that the game is so unpredictable, 
like the Moor of Venice. The name may also be an allusion to the line, "Your daughter 
and the Moor are now making the beast with two backs,"^ since the game pieces 
do indeed have two backs, one white and one black. In any case, the association 
between the game and the play carries over to the name of several programs: Cassio, 
lago, and Bill. The last two will be discussed in this chapter. They are equal to or 
better than even champion human players. We will be able to develop a simplified 
version that is not quite a champion but is much better than beginning players. 

18.1 The Rules of the Game 
Othello is played on a 8-by-8 board, which is initially set up with four pieces in the 
center, as shown in figure 18.1. The two players, black and white, alternate turns, 
with black playing first. On each turn, a player places a single piece of his own color 
on the board. No piece can be moved once it is placed, but subsequent moves may 
flip a piece from one color to another. Each piece must be placed so that it brackets 
one or more opponent pieces. That is, when black plays a piece there must be a 
line (horizontal, vertical, or diagonal) that goes through the piece just played, then 
through one or more white pieces, and then to another black piece. The intervening 

^Othello is a registered trademark of CBS Inc. Gameboard design © 1974 CBS Inc. 

^Othelh [I. i. 117] WiUiam Shakespeare. 

<a id='page-598'></a>

white pieces are flipped over to black. If there are bracketed white pieces in more 
than one direction, they are all flipped. Figure 18.2 (a) indicates the legal moves for 
black with small dots. Figure 18.2 (b) shows the position after black moves to square 
b4. Players alternate turns, except that a player who has no legal moves must pass. 
When neither player has any moves, the game is over, and the player with the most 
pieces on the board wins. This usually happens because there are no empty squares 
left, but it occasionally happens earlier in the game. 

f g h 

O'o ' 

Figure 18.1: The Othello Board 

. f g . f g h 

o o 

o o '' ' ' ' 
'' ' 

O o o o o o o

'o ' ' ' 

o 

(b) 

Figure 18.2: Legal Othello Moves 

<a id='page-599'></a>
18.2 Representation Choices 
In developing an Othello program, we will want to test out various strategies, playing 
those strategies against each other and against human players. We may also want 
our program to allow two humans to play a game. Therefore, our main function, 
othel 10, will be a monitoring function that takes as arguments two strategies. It 
uses these strategies to get each player's moves, and then applies these moves to a 
representation of the game board, perhaps printing out the board as it goes. 

The first choice to make is how to represent the board and the pieces on it. The 
board is an 8-by-8 square, and each square can be filled by a black or white piece or 
can be empty. Thus, an obvious representation choice is to make the board an 8-by-8 
array, where each element of the array is the symbol bl ack, whi te, or ni 1. 

Notice what is happening here: we are following the usual Lisp convention of 
implementing an enumerated type (the type of pieces that can fill a square) as a set 
of symbols. This is an appropriate representation because it supports the primary 
operation on elements of an enumerated type: test for equality using eq. It also 
supports input and output quite handily. 

In many other languages (such as C or Pascal), enumerated types are implemented 
as integers. In Pascal one could declare: 

type piece = (black, white, empty); 

to define pi ece as a set of three elements that is treated as a subtype of the integers. 
The language does not allow for direct input and output of such types, but equality 
can be checked. An advantage of this approach is that an element can be packed into 
a small space. In the Othello domain, we anticipate that efficiency will be important, 
because one way to pick a good move is to look at a large number of possible sequences 
of moves, and choose a sequence that leads toward a favorable result. Thus, we are 
willing to look hard at alternative representations to find an efficient one. It takes 
only two bits to represent one of the three possible types, while it takes many more 
(perhaps 32) to represent a symbol. Thus, we may save space by representing pieces 
as small integers rather than symbols. 

Next, we consider the board. The two-dimensional array seems like such an 
obvious choice that it is hard to imagine a better representation. We could consider 
an 8-element list of 8-element lists, but this would just waste space (for the cons 
cells) and time (in accessing the later elements of the lists). However, we will have to 
implement two other abstract data types that we have not yet considered: the square 
and the direction. We will need, for example, to represent the square that a player 
chooses to move into. This will be a pair of integers, such as 4,5. We could represent 
this as a two-element list, or more compactly as a cons cell, but this still means that 
we may have to generate garbage (create a cons cell) every time we want to refer 
to a new square. Similarly, we need to be able to scan in a given direction from a 

<a id='page-600'></a>

square, looking for pieces to flip. Directions will be represented as a pair of integers, 
such as +1,-1. One clever possibility is to use complex numbers for both squares and 
directions, with the real component mapped to the horizontal axis and the imaginary 
component mapped to the vertical axis. Then moving in a given direction from a 
square is accomplished by simply adding the direction to the square. But in most 
implementations, creating new complex numbers will also generate garbage. 

Another possibiUty is to represent squares (and directions) as two distinct integers, 
and have the routines that manipulate them accept two arguments instead of 
one. This would be efficient, but it is losing an important abstraction: that squares 
(and directions) are conceptually single objects. 

A way out of this dilemma is to represent the board as a one-dimensional vector. 
Squares are represented as integers in the range 0 to 63. In most implementations, 
small integers (fixnums) are represented as immediate data that can be manipulated 
without generating garbage. Directions can also be implemented as integers, representing 
the numerical difference between adjacent squares along that direction. To 
get a feel for this, take a look at the board: 

0 1 2 3 4 5 6 7 
8 9 10 11 12 13 14 15 
16 17 18 19 20 21 22 23 
24 25 26 27 28 29 30 31 
32 33 34 35 36 37 38 39 
40 41 42 43 44 45 46 47 
48 49 50 51 52 53 54 55 
56 57 58 59 60 61 62 63 

You can see that the direction +1 corresponds to movement to the right, +7 corresponds 
to diagonal movement downward and to the left, +8 is downward, and +9 is 
diagonally downward and to the right. The negations of these numbers (-1, -7, -8, -9) 
represent the opposite directions. 

There is one complication with this scheme: we need to know when we hit the 
edge of the board. Starting at square 0, we can move in direction +1 seven times to 
arrive at the right edge of the board, but we aren't allowed to move in that direction 
yet again to arrive at square 8. It is possible to check for the edge of the board by 
considering quotients and remainders modulo 8, but it is somewhat complicated and 
expensive to do so. 

A simpler solution is to represent the edge of the board explicitly, by using a 100element 
vector instead of a 64-element vector. The outlying elements are filled with a 
marker indicating that they are outside the board proper. This representation wastes 
some space but makes edge detection much simpler. It also has the minor advantage 
that legal squares are represented by numbers in the range 11-88, which makes them 
easier to understand while debugging. Here's the new 100-element board: 

<a id='page-601'></a>
0 1 2 3 4 5 6 7 8 9 
10 11 12 13 14 15 16 17 18 19 
20 21 22 23 24 25 26 27 28 29 
30 31 32 33 34 35 36 37 38 39 
40 41 42 43 44 45 46 47 48 49 
50 51 52 53 54 55 56 57 58 59 
60 61 62 63 64 65 66 67 68 69 
70 71 72 73 74 75 76 77 78 79 
80 81 82 83 84 85 86 87 88 89 
90 91 92 93 94 95 96 97 98 99 

The horizontal direction is now &plusmn;1, vertical is &plusmn;10, and the diagonals are &plusmn;9 and 
&plusmn;11. We'll tentatively adopt this latest representation, but leave open the possibility 
of changing to another format. With this much decided, we are ready to begin. 
Figure 18.3 is the glossary for the complete program. A glossary for a second version 
of the program is on [page 623](chapter18.md#page-623). 

What follows is the code for directions and pieces. We explicitly define the type 
piece to be a number from empty to outer (0 to 3), and define the function name-of 
to map from a piece number to a character: a dot for empty, @ for black, 0 for white, 
and a question mark (which should never be printed) for outer. 

(defconstant all-directions '(-11 -10 -9-119 10 ID) 

(defconstant empty 0 "An empty square") 
(defconstant black 1 "A black piece") 
(defconstant white 2 "A white piece") 
(defconstant outer 3 "Marks squares outside the 8x8 board") 

(deftype piece () '(integer .empty .outer)) 

(defun name-of (piece) (char ".@0?" piece)) 

(defun opponent (player) (if (eql player black) white black)) 

And here is the code for the board. Note that we introduce the function bref, 
for "board reference" rather than using the built-in function aref. This facilitates 
possible changes to the representation of boards. Also, even though there is no 
contiguous range of numbers that represents the legal squares, we can define the 
constant a 11 - squa res to be a list of the 64 legal squares, computed as those numbers 
from 11 to 88 whose value mod 10 is between 1 and 8. 

(deftype board () '(simple-array piece (100))) 

(defun bref (board square) (aref board square)) 
(defsetf bref (board square) (val) 
'(setf (aref .board .square) .val)) 

<a id='page-602'></a>

Othello 

empty 
black 
white 
outer 
all-directions 
all-squares 
winning-value 
losing-value 

piece 

board 

get-move 
make-move 
human 
random-strategy 
maximi ze-di fference 

maximizer 
weighted-squares 
modified-weighted-squares 
mi .imax 
minimax-searcher 
alpha-beta 
alpha-beta-searcher 

bref 
copy-board 
initial-board 
print-board 
count-difference 
name-of 
opponent 
valid-p 
legal-p 

make-flips 
would-flip? 
find-bracketing-piece 
any-legal-move? 
next-to-play 
legal-moves 
final-value 
neighbors 
switch-strategies 

random-elt 

Top-Level Function 

Play a game of Othello. Return the score. 

Constants 

0 represents an empty square. 
1 represents a black piece. 
2 represents a white piece. 
3 represents a piece outside the 8x8 board. 
A list of integers representing the eight directions. 
A list of all legal squares. 
The best possible evaluation. 
The worst possible evaluation. 

Data Types 

An integer from empty to outer. 
A vector of 100 pieces. 

Major Functions 

Call the player's strategy function to get a move. 

Update board to reflect move by player. 
A strategy that prompts a human player. 
Make any legal move. 

A strategy that maximizes the difference in pieces. 
Return a strategy that maximizes some measure. 
Sum of the weights of player's squares minus opponent's. 
Like above, but treating corners better. 
Find the best move according to EVAL.FN, searching PLY levels. 
Return a strategy that uses mi.i max to search. 
Find the best move according to EVAL-FN, searching PLY levels. 
Return a strategy that uses al pha- beta to search. 

Auxiliary Functions 

Reference to a position on the board. 
Make a new board. 
Return a board, empty except for four pieces in the middle. 
Print a board, along with some statistics. 
Count player's pieces minus opponent's pieces. 
A character used to print a piece. 
The opponent of black is white, and vice-versa. 
A syntactically vahd square. 
A legal move on the board. 
Make any flips in the given direction. 
Would this move result in any flips in this direction? 
Return the square number of the bracketing piece. 
Does player have any legal moves in this position? 
Compute the player to move next, or NIL if nobody can move. 
Returns a list of legal moves for player. 
Is this a win, loss, or draw for player? 
Return a list of all squares adjacent to a square. 
Play one strategy for a while, then switch. 

Previously Defined Functions 

Choose a random element from a sequence, (pg. 36) 

Figure 18.3: Glossary for the Othello Program 

<a id='page-603'></a>

(defun copy-board (board) 
(copy-seq board)) 

(defconstant all-squares 
(loop for i from 11 to 88 when (<= 1 (mod i 10) 8) collect i)) 

(defun initial-board () 

"Return a board, empty except for four pieces in the middle." 
Boards are 100-element vectors, with elements 11-88 used, 
and the others marked with the sentinel OUTER. Initially 
the 4 center squares are taken, the others empty, 

(let ((board (make-array 100 :element-type 'piece 
:initial-element outer))) 
(dolist (square all-squares) 
(setf (bref board square) empty)) 
(setf (bref board 44) white (bref board 45) black 
(bref board 54) black (bref board 55) white) 
board)) 

(defun print-board (board) 
"Print a board, along with some statistics." 
(format t "~2& 1 2 3 4 5 6 7 8 [~c=~2a ~c=''2a (~@d)]" 

(name-of black) (count black board) 
(name-of white) (count white board) 
(count-difference black board)) 

(loop for row from 1 to 8 do 
(format t "-& ~d " (* 10 row)) 
(loop for col from 1 to 8 

for piece = (bref board (+ col (* 10 row))) 
do (format t ""c " (name-of piece)))) 
(format t "~2&")) 

(defun count-difference (player board) 
"Count player's pieces minus opponent's pieces." 
(- (count player board) 

(count (opponent player) board))) 

Now let's take a look at the initial board, as it is printed by pri nt - boa rd, and by a raw 
wri te (I added the line breaks to make it easier to read): 

<a id='page-604'></a>

> (write (initial-board ) > (print-board (initial-board) ) 
rarray t) 
#(3 33333333 3 1234567 8 C@=2 0=2 (-^0)1 
300000000 3 10 
300000000 3 20 
300000000 3 30 
300021000 3 40...0@.. . 
300012000 3 50 . ..@0.. . 
300000000 3 60 
300000000 3 70 
300000000 3 80 
33333333 3 3) 
#<ART-2B-100 -72570734> NIL 

Notice that pri nt - boa rd provides some additional information: the number of pieces 
that each player controls, and the difference between these two counts. 

The next step is to handle moves properly: given a board and a square to move 
to, update the board to reflect the effects of the player moving to that square. This 
means flipping some of the opponent's pieces. One design decision is whether the 
procedure that makes moves, make-move, will be responsible for checking for error 
conditions. My choice is that make - move assumes it will be passed a legal move. That 
way, a strategy can use the function to explore sequences of moves that are known to 
be valid without slowing make - move down. Of course, separate procedures will have 
to insure that a move is legal. Here we introduce two terms: a valid move is one that 
is syntactically correct: an integer from 11 to 88 that is not off the board. A legal move 
is a valid move into an empty square that will flip at least one opponent. Here's the 
code: 

(defun valid-p (move) 

"Valid moves are numbers in the range 11-88 that end in 1-8." 

(and (integerp move) (<= 11 move 88) (<= 1 (mod move 10) 8))) 

(defun legal-p (move player board) 

"A Legal move must be into an empty square, and it must 

flip at least one opponent piece." 

(and (eql (bref board move) empty) 
(some #'(lambda (dir) (would-flip? move player board dir)) 

all-directions))) 

(defun make-move (move player board) 

"Update board to reflect move by player" 

First make the move, then make any flips 
(setf (bref board move) player) 
(dolist (dir all-directions) 

(make-flips move player board dir)) 
board) 

<a id='page-605'></a>
Now all we need is to make-fl ips. To do that, we search in all directions for a 
bracketing piece: a piece belonging to the player who is making the move, which 
sandwiches a string of opponent pieces. If there are no opponent pieces in that 
direction, or if an empty or outer piece is hit before the player's piece, then no flips 
are made. Note that would-f 1 ip? is a semipredicate that returns false if no flips 
would be made in the given direction, and returns the square of the bracketing piece 
if there is one. 

(defun make-flips (move player board dir) 
"Make any flips in the given direction." 
(let ((bracketer (would-flip? move player board dir))) 

(when bracketer 
(loop for c from (+ move dir) by dir until (eql c bracketer) 
do (setf (bref board c) player))))) 

(defun would-flip? (move player board dir) 
"Would this move result in any flips in this direction? 
If so. return the square number of the bracketing piece." 

A flip occurs if, starting at the adjacent square, c. there 
is a string of at least one opponent pieces, bracketed by 
one of player's pieces 

(let ((c (+ move dir))) 
(and (eql (bref board c) (opponent player)) 
(find-bracketing-piece (+ c dir) player board dir)))) 

(defun find-bracketing-piece (square player board dir) 
"Return the square number of the bracketing piece." 
(cond ((eql (bref board square) player) square) 

((eql (bref board square) (opponent player)) 
(find-bracketing-piece (+ square dir) player board dir)) 
(t nil))) 

Finally we can write the function that actually monitors a game. But first we are 
faced with one more important choice: how will we represent a player? We have 
already distinguished between black and white's pieces, but we have not decided 
how to ask black or white for their moves. I choose to represent player's strategies 
as functions. Each function takes two arguments: the color to move (black or white) 
and the current board. The function should return a legal move number. 

(defun Othello (bl-strategy wh-strategy &optional (print t)) 
"Play a game of Othello. Return the score, where a positive 
difference means black (the first player) wins." 
(let ((board (initial-board))) 

(loop for player = black 
then (next-to-play board player print) 
for strategy = (if (eql player black) 

<a id='page-606'></a>

bl-strategy 

wh-strategy) 
until (null player) 
do (get-move strategy player board print)) 

(when print 
(format t "~&The game is over. Final result:") 
(print-board board)) 

(count-difference black board))) 

We need to be able to determine who plays next at any point. The rules say that 
players alternate turns, but if one player has no legal moves, the other can move 
again. When neither has a legal move, the game is over. This usually happens 
because there are no empty squares left, but it sometimes happens earlier in the 
game. The player with more pieces at the end of the game wins. If neither player has 
more, the game is a draw. 

(defun next-to-play (board previous-player print) 
"Compute the player to move next, or NIL if nobody can move." 
(let ((opp (opponent previous-player))) 

(cond ((any-legal-move? opp board) opp) 
((any-legal-move? previous-player board) 
(when print 
(format t ""&^C has no moves and must pass." 
(name-of opp))) 
previous-player) 
(t nil)))) 

(defun any-legal-move? (player board) 
"Does player have any legal moves in this position?" 
(some #'(lambda (move) (legal-p move player board)) 

all-squares)) 

Note that the argument print (of Othello, next-to-play, and below, get-move) 
determines if information about the progress of the game will be printed. For an 
interactive game, pri nt should be true, but it is also possible to play a "batch" game 
with pri nt set to false. 

In get - move below, the player's strategy function is called to determine his move. 
Illegal moves are detected, and proper moves are reported when pri nt is true. The 
strategy function is passed a number representing the player to move (black or white) 
and a copy of the board. If we passed the real game board, the function could cheat 
by changing the pieces on the board! 

<a id='page-607'></a>

(defun get-move (strategy player board print) 
"Call the player's strategy function to get a move. 
Keep calling until a legal move is made." 
(when print (print-board board)) 
(let ((move (funcall strategy player (copy-board board)))) 

(cond 
((and (valid-p move) (legal-p move player board)) 
(when print 
(format t "'^&'. moves to ~d." (name-of player) move)) 
(make-move move player board)) 
(t (warn "Illegal move: ~d" move) 
(get-move strategy player board print))))) 

Here we define two simple strategies: 

(defun human (player board) 
"A human player for the game of Othello" 
(declare (ignore board)) 
(format t "~&~c to move: " (name-of player)) 
(read)) 

(defun random-strategy (player board) 
"Make any legal move." 
(random-elt (legal-moves player board))) 

(defun legal-moves (player board) 
"Returns a list of legal moves for player" 
(loop for move in all-squares 

when (legal-p move player board) collect move)) 

We are now in a position to play the game. The expression 
(othel 1 0 # * human #'human) will let two people play against each other. Alternately, 
(othel lo #'random-strategy #'human) will allow us to match our wits against a 
particularly poor strategy. The rest of this chapter shows how to develop a better 
strategy. 

18.3 Evaluating Positions 
The random-move strategy is, of course, a poor one. We would like to make a good 
move rather than a random move, but so far we don't know what makes a good 
move. The only positions we are able to evaluate for sure are final positions: when 
the game is over, we know that the player with the most pieces wins. This suggests a 
strategy: choose the move that maximizes count-di f f erence, the piece differential. 

<a id='page-608'></a>

The function maxi mize-di ff erence does just that. It calls maxi mi zer, a higher-order 

function that chooses the best move according to an arbitrary evaluation function. 

(defun maximize-difference (player board) 
"A strategy that maximizes the difference in pieces." 
(funcall (maximizer #'count-difference) player board)) 

(defun maximizer (eval-fn) 
"Return a strategy that will consider every legal move, 
apply EVAL-FN to each resulting board, and choose 
the move for which EVAL-FN returns the best score. 
FN takes two arguments: the player-to-move and board" 
#*(lambda (player board) 

(let* ((moves (legal-moves player board)) 
(scores (mapcar #'(lambda (move) 

(funcall 
eval-fn 
player 
(make-move move player 

(copy-board board)))) 
moves)) 
(best (apply #*max scores))) 
(elt moves (position best scores))))) 

&#9635; Exercise 18.1 Playsomegameswithmaximize -differenceagainst random-strategy 
and human. How good is maximize-difference? 
Those who complete the exercise will quickly see that the maximi ze-di ff erence 
player does better than random, and may even beat human players in their first game 
or two. But most humans are able to improve, learning to take advantage of the 
overly greedy play of maximi ze-di ff erence. Humans learn that the edge squares, 
for example, are valuable because the player dominating the edges can surround the 
opponent, while it is difficult to recapture an edge. This is especially true of corner 
squares, which can never be recaptured. 
Using this knowledge, a clever player can temporarily sacrifice pieces to obtain 
edge and corner squares in the short run, and win back pieces in the long run. 
We can approximate some of this reasoning with the weighted-squa res evaluation 
function. Like count-difference, it adds up all the player's pieces and subtracts 
the opponents, but each piece is weighted according to the square it occupies. Edge 
squares are weighted highly, corner squares higher still, and squares adjacent to the 
corners and edges have negative weights, because occupying these squares often 
gives the opponent a means of capturing the desirable square. Figure 18.4 shows 
the standard nomenclature for edge squares: X, A, B, and C. In general, X and C 

<a id='page-609'></a>
squares are to be avoided, because taking them gives the opponent a chance to take 
the corner. The wei ghted-squares evaluation function reflects this. 

a b c d e f g h 

1 c A . . A C 

2 X X

c c 

3 A A 
4 . . 
5 .

. 
6 A A 

7 C X X C 

8 c A . . A c 

Figure 18.4: Names for Edge Squares 

(defparameter ^weights* 

'#(0 0 0 0 0 0 0 0 0 0 
0 120 -20 20 5 5 20 -20 120 0 
0 -20 -40 -5 -5 -5 -5 -40 -20 0 
0 20 -5 15 3 3 15 -5 20 0 
5-5333 3 -5 5 0 
5-5333 3 -5 5 0 
20 -5 15 3 3 15 -5 20 0 
-20 -40 -5 -5 -5 -5 -40 -20 0 
120 -20 20 5 5 20 -20 120 0 

0 00000 0 0 0)) 

(defun weighted-squares (player board) 
"Sum of the weights of player's squares minus opponent's." 
(let ((opp (opponent player))) 

(loop for i in all-squares 
when (eql (bref board i) player) 
sum (aref *weights* i) 
when (eql (bref board i) opp) 
sum (- (aref ^weights* i))))) 

&#9635; Exercise 18.2 Compare strategies by evaluating the two forms below. What happens? 
Is this a good test to determine which strategy is better? 

<a id='page-610'></a>

(Othello (maximizer #'weighted-squares) 
(maximizer #*count-difference) nil) 

(Othello (maximizer #'count-difference) 
(maximizer #'weighted-squares) nil) 

18.4 Searching Ahead: Minimax 
Even the weighted-squares strategy is no match for an experienced player. There 
are two ways we could improve the strategy. First, we could modify the evaluation 
function to take more information into account. But even without changing the 
evaluation function, we can improve the strategy by searching ahead. Instead of 
choosing the move that leads immediately to the highest score, we can also consider 
the opponent's possible replies, our replies to those replies, and so on. By searching 
through several levels of moves, we can steer away from potential disaster and find 
good moves that were not immediately apparent. 

Another way to look at the maxi mi zer function is as a search function that searches 
only one level, or ply, deep: 

The top of the tree is the current board position, and the squares below that indicate 
possible moves. The maxi mi zer function evaluates each of these and picks the best 
move, which is underlined in the diagram. 

Now let's see how a 3-ply search might go. The first step is to apply maxi mi zer to 
the positions just above the bottom of the tree. Suppose we get the following values: 

<a id='page-611'></a>

Each position is shown as having two possible legal moves, which is unreahstic 
but makes the diagram fit on the page. In a real game, five to ten legal moves per 
position is typical. The values at the leaves of the tree were computed by applying 
the evaluation function, while the values one level up were computed by maxi mi zer. 
The result is that we know what our best move is for any of the four positions just 
above the bottom of the tree. 

Going up a level, it is the opponent's turn to move. We can assume the opponent 
will choose the move that results in the minimal value to us, which would be the 
maximal value to the opponent. Thus, the opponent's choices would be the 10- and 
9-valued positions, avoiding the 20- and 23-valued positions. 

<a id='page-612'></a>

Now it is our turn to move again, so we apply maxi mi zer once again to get the final 
value of the top-level position: 

If the opponent plays as expected, we will always follow the left branch of the tree 
and end up at the position with value 10. If the opponent plays otherwise, we will 
end up at a position with a better value. 

This kind of search is traditionally called a minimax search, because of the alternate 
application of the maxi mi zer and a hypothetical mi ni mi zer function. Notice that only 
the leaf positions in the tree are looked at by the evaluation function. The value of all 
other positions is determined by minimizing and maximizing. 

We are almost ready to code the minimax algorithm, but first we have to make 
a few design decisions. First, we could write two functions, mi nimax and maxi mi n, 
which correspond to the two players' analyses. However, it is easier to write a single 
function that maximizes the value of a position for a particular player. In other words, 
by adding the player as a parameter, we avoid having to write two otherwise identical 
functions. 

Second, we have to decide if we are going to write a general minimax searcher 
or an Othello-specific searcher. I decided on the latter for efficiency reasons, and 
because there are some Othello-specific complications that need to be accounted for. 
First, it is possible that a player will not have any legal moves. In that case, we want 
to continue the search with the opponent to move. If the opponent has no moves 
either, then the game is over, and the value of the position can be determined with 
finality by counting the pieces. 

Third, we need to decide the interaction between the normal evaluation function 
and this final evaluation that occurs when the game is over. We could insist that 

<a id='page-613'></a>
each evaluation function determine when the game is over and do the proper computation. 
But that overburdens the evaluation functions and may lead to wasteful 
checking for the end of game. Instead, I implemented a separate f i nal - val ue evaluation 
function, which returns 0 for a draw, a large positive number for a win, and 
a large negative number for a loss. Because fixnum arithmetic is most efficient, the 
constants most-positive-fixnum and most-negative-fixnum are used. The evaluation 
functions must be careful to return numbers that are within this range. All 
the evaluation functions in this chapter will be within range if fixnums are 20 bits 
or more. 

In a tournament, it is not only important who wins and loses, but also by how 
much. If we were trying to maximize the margin of victory, then f i na1 - va1 ue would 
be changed to include a small factor for the final difference. 

(defconstant winning-value most-positive-fixnum) 
(defconstant losing-value most-negative-fixnum) 

(defun final-value (player board) 
"Is this a win. loss, or draw for player?" 
(case (Signum (count-difference player board)) 

(-1 losing-value) 
( 0 0) 
(+1 winning-value))) 

Fourth, and finally, we need to decide on the parameters for the minimax function. 
Like the other evaluation functions, it needs the player to move and the current board 
as parameters. It also needs an indication of how many ply to search, and the static 
evaluation function to apply to the leaf positions. Thus, minimax will be a function 
of four arguments. What will it return? It needs to return the best move, but it also 
needs to return the value of that move, according to the static evaluation function. 
We use multiple values for this. 

(defun minimax (player board ply eval-fn) 
"Find the best move, for PLAYER, according to EVAL-FN. 
searching PLY levels deep and backing up values." 
(if (= ply 0) 

(funcall eval-fn player board) 
(let ((moves (legal-moves player board))) 
(if (null moves) 
(if (any-legal-move? (opponent player) board) 
(- (minimax (opponent player) board 
(- ply 1) eval-fn)) 
(final-value player board)) 
(let ((best-move nil) 
(best-val nil)) 
(dolist (move moves) 

<a id='page-614'></a>

(let* ((board2 (make-move move player 
(copy-board board))) 

(val (- (minimax 
(opponent player) board2 
(- ply 1) eval-fn)))) 

(when (or (null best-val) 

(> val best-val)) 
(setf best-val val) 
(setf best-move move)))) 

(values best-val best-move)))))) 

The mi . i max function cannot be used as a strategy function as is, because it takes too 
many arguments and returns too many values. The functional minimax-searcher 
returns an appropriate strategy. Remember that a strategy is a fimction of two 
arguments: the player and the board, get-move is responsible for passing the right 
arguments to the function, so the strategy need not worry about where the arguments 
come from. 

(defun minimax-searcher (ply eval-fn) 
"A strategy that searches PLY levels and then uses EVAL-FN." 
#*(lambda (player board) 

(multiple-value-bind (value move) 

(minimax player board ply eval-fn) 
(declare (ignore value)) 
move))) 

We can test the minimax strategy, and see that searching ahead 3 ply is indeed better 
than looking at only 1 ply. I show only the final result, which demonstrates that it is 
indeed an advantage to be able to look ahead: 

> (Othello (minimax-searcher 3 #*count-difference) 
(maximizer #'count-difference)) 

The game is over. Final result: 

12 3 4 5 6 7 8 [@=53 0=0 (+53)] 

20@@@@@@@@ 

30@@@@@@@@ 
40@@@@@@@@ 
50@@@@@@@@ 

60 . . @@ @@ @ @ 
70 . . . @ @ @ @ @ 
80 . . . . @ @ . . 
<a id='page-615'></a>

18.5 Smarter Searching: Alpha-Beta Search 
The problem with a full minimax search is that it considers too many positions. It 
looks at every line of play, including many improbable ones. Fortunately, there is a 
way to find the optimal line of play without looking at every possible position. Let's 
go back to our familiar search tree: 

Here we have marked certain positions with question marks. The idea is that the 
whole search tree evaluates to 10 regardless of the value of the positions labeled ?i. 
Consider the position labeled ?i. It does not matter what this position evaluates to, 
because the opponent will always choose to play toward the 10-position, to avoid the 
possibility of the 15. Thus, we can cut off the search at this point and not consider 
the ?-position. This kind of cutoff has historically been called a beta cutoff. 

Now consider the position labeled ?4. It does not matter what this position 
evaluates to, because we will always prefer to choose the 10 position at the left 
branch, rather than giving the opponent a chance to play to the 9-position. This is an 
alpha cutoff. Notice that it cuts off a whole subtree of positions below it (labeled ?2 
and ?3). 

In general, we keep track of two parameters that bound the true value of the 
current position. The lower bound is a value we know we can achieve by choosing a 
certain line of play. The idea is that we need not even consider moves that will lead 
to a value lower than this. The lower bound has traditionally been called alpha, but 
we will name it achi evabl e. The upper bound represents a value the opponent can 
achieve by choosing a certain line of play. It has been called beta, but we will call it 
cutoff. Again, the idea is that we need not consider moves with a higher value than 
this (because then the opponent would avoid the move that is so good for us). The 

<a id='page-616'></a>

alpha-beta algorithm is just minimax, but with some needless evaluations pruned by 
these two parameters. 

In deeper trees with higher branching factors, many more evaluations can be 
pruned. In general, a tree of depth d and branching factor b requires b^ evaluations 
for full minimax, and as few as 6^/^ evaluations with alpha-beta minimax. 

To implement alpha-beta search, we add two more parameters to the function 
minimax and rename it alpha-beta, achievable is the best score the player can 
achieve; it is what we want to maximize. The cutoff is a value that, when exceeded, 
will make the opponent choose another branch of the tree, thus making the rest of 
the current level of the tree irrelevant. The test unti 1 (>= achi evabl e cutoff) in 
the penultimate line of minimax does the cutoff; all the other changes just involve 
passing the parameters around properly. 

(defun alpha-beta (player board achievable cutoff ply eval-fn) 
"Find the best move, for PLAYER, according to EVAL-FN, 
searching PLY levels deep and backing up values, 
using cutoffs whenever possible." 
(if (= ply 0) 

(funcall eval-fn player board) 
(let ((moves (legal-moves player board))) 
(if (null moves) 
(if (any-legal-move? (opponent player) board) 

(- (alpha-beta (opponent player) board 
(- cutoff) (- achievable) 
(- ply 1) eval-fn)) 

(final-value player board)) 
(let ((best-move (first moves))) 
(loop for move in moves do 
(let* ((boardZ (make-move move player 
(copy-board board))) 

(val (- (alpha-beta 
(opponent player) board2 
(- cutoff) (- achievable) 
(- ply 1) eval-fn)))) 

(when (> val achievable) 
(setf achievable val) 
(setf best-move move))) 

until (>= achievable cutoff)) 
(values achievable best-move)))))) 

(defun alpha-beta-searcher (depth eval-fn) 
"A strategy that searches to DEPTH and then uses EVAL-FN." 
#.(lambda (player board) 

(multiple-value-bind (value move) 
(alpha-beta player board losing-value winning-value 
depth eval-fn) 

<a id='page-617'></a>
(declare (ignore value)) 
move))) 

It must be stressed that a 1 pha- beta computes the exact same result as the full-search 
version of mi . i max. The only advantage of the cutoffs is making the search go faster 
by considering fewer positions. 

18.6 An Analysis of Some Games 
Now is a good time to stop and analyze where we have gone. We've demonstrated a 
program that can play a legal game of Othello, and some strategies that may or may 
not play a good game. First, we'll look at some individual games to see the mistakes 
made by some strategies, and then we'll generate some statistics for series of games. 

Is the weighted-squares measure a good one? We can compare it to a strategy of 
maximizing the number of pieces. Such a strategy would of course be perfect if it 
could look ahead to the end of the game, but the speed of our computers limits us 
to searching only a few ply, even with cutoffs. Consider the following game, where 
black is maximizing the difference in the number of pieces, and white is maximizing 
the weighted sum of squares. Both search to a depth of 4 ply: 

> (Othello (alpha-beta-searcher 4 #'count-difference) 
(alpha-beta-searcher 4 #*weighted-squares)) 

Black is able to increase the piece difference dramatically as the game progresses. 
After 17 moves, white is down to only one piece: 

12 3 4 5 6 7 8 [@=20 0=1 (+19)] 
10 0 @ 

20 . @ . . . @ @ . 
30 @ @ @ @ @ @ . . 
40 . @ . @ @ . . . 
50 @ @ @@ @ @ . . 
60 . @ 
70 
80 
Although behind by 19 points, white is actually in a good position, because the piece 
in the corner is safe and threatens many of black's pieces. White is able to maintain 
good position while being numerically far behind black, as shown in these positions 
later in the game: 

<a id='page-618'></a>

12 3 4 5 6 7 8 [e=32 0=15 (+17)] 
10 0 0 0 0 @ @ 0 0 
20 @ @0 @ @ @ @ @ 
30 @ @ 0 0 @ 0 @ @ 
40 0 0 @ @ @ @ @ @ 
50 @0 @ @ @ @ 
60 @ @0 @ @ 0 
70 @ . . @ @ . 
80 
1 2 3 4 5 6 7 8 [@=34 0=19 (+15)] 
10 0 0 0 0 @ @ 0 0 
20 @ @0 @ @ @ @ @ 
30 @ @ 0 0 @ 0 @ @ 
40 0 @ 0 @ @ @ @ @ 
50 0 @ 0 @ @ @ @ . 
60 0 @ 0 @ @ @ 
70 0 @ @ @ @ . 
80 0 @ 0 . 

After some give-and-take, white gains the advantage for good by capturing eight 
pieces on a move to square 85 on the third-to-last move of the game: 

1 2 3 4 5 6 7 8 [@=31 0=30 (+1)] 
10 0 0 0 0 @ @ 0 0 
20 @ @ 0 0 @ @ @ 0 
30 @ @ 0 0 0@ @ 0 
40 0 @ 0 0 0@ @ 0 
50 0 @ 0 @ 0 @ @ 0 

60 0 @ 0 @ @ @@ 0 

70 0 @ @ @ @ @0 0 

80 0 @ @ @ . . ' 0 

0 moves to 85. 

1 2 3 4 5 6 7 8 [@=23 0=39 (-16)] 
10 0 0 0 0 @ @ 0 0 
20 @ @0 0 @ @ @ 0 
30 @ @0 0 0@ @ 0 
40 0 @ 0 0 0@ @ 0 
50 0 @ 0 @0 @ @ 0 
60 0 @ 0 @ 0 @ 0 0 
70 0 @ @ 0 0 0 0 0 
80 0 0 0 0 0 . ' 0 

@ moves to 86. 

<a id='page-619'></a>
12 3 4 5 6 7 8 [@=26 0=37 (-11)] 
10 0000@@00 
20@@00@@@0 
30@@000@@0 
40 0@000@@0 
50 0@0@0@@0 
60 0@0@0@00 
70 0@@0@@00 
80 00000@.0 

0 moves to 87. 
The game is over. Final result: 

1 2 3 4 5 6 7 8 [@=24 0=40 (-16)] 
10 0000@@00 
20@@00@@@0 
30@@000@@0 
40 0@000@@0 
50 0@0@0@@0 
60 0@0@0@00 
70 0@@0@000 
80 00000000 

-16 

White ends up winning by 16 pieces. Black's strategy was too greedy: black was 
willing to give up position (all four corners and all but four of the edge squares) for 
temporary gains in material. 

Increasing the depth of search does not compensate for a faulty evaluation function. 
In the following game, black's search depth is increased to 6 ply, while white's 
is kept at 4. The same things happen, although black's doom takes a bit longer to 
unfold. 

> (Othello (alpha-beta-searcher 6 #'count-difference) 
(alpha-beta-searcher 4 #'weighted-squares)) 

Black slowly builds up an advantage: 

12 3 4 5 6 7 8 [@=21 0=8 (+13)] 

10 . . @ @ @ @ @ 
20 . @ . @ 0 @ . 
30 0@@0@00 
40 . @. @ 0 @ 0 
50. @ @ @ @ @ . 
60 . @ . @ . 0 . 
70 
80 
<a id='page-620'></a>

But at this point white has clear access to the upper left corner, and through that 
corner threatens to take the whole top edge. Still, black maintains a material edge as 
the game goes on: 

12 3 4 5 6 7 8 [@=34 0=11 (+23)] 

10 0 . @ @ @ @@. 
20 . 0 0 @ @ @ . . 
30 0@00@@@@ 
40@@@@0@@ . 
50@@@@@0@. 
60@@@@@@00 
70 @ . . @ . . @ 0 
80 
But eventually white's weighted-squares strategy takes the lead: 

12 3 4 5 6 7 8 [@=23 0=27 (-4)] 
10 00 0 00000 
20 @ @ 0 @ @ @ . . 
30 0@00@@@@ 
40 0@0@0@@ . 
50 0@0@@0@ . 
60 000@@@00 

70 0 . 0 @ . . @ 0 
800 
and is able to hold on to win: 

12 3 4 5 6 7 8 [@=24 0=40 (-16)] 
10 00000000 
20@@0@00@@ 
30 0@00@@@@ 
40 0@00@@@0 
50 00@@0@00 
60 000@0@@0 
70 0000@@00 
80 00000@@0 

-16 

This shows that brute-force searching is not a panacea. While it is helpful to be able 
to search deeper, greater gains can be made by making the evaluation function more 
accurate. There are many problems with the weighted-squares evaluation function. 
Consider again this position from the first game above: 

<a id='page-621'></a>

12 3 4 5 6 7 8 [@=20 0=1 (+19)] 
10 0 @ 

20 . @ . . . @@ . 
30 @ @ @ @@ @ . . 
40 . @ . @ @ . . . 
50 @ @@ @ @@ . . 
60 . @ 
70 
80 
Here white, playing the weighted-squares strategy, chose to play 66. This is probably 
a mistake, as 13 would extend white's dominance of the top edge, and allow white to 
play again (since black would have no legal moves). Unfortunately, white rejects this 
move, primarily because square 12 is weighted as -20. Thus, there is a disincentive 
to taking this square. But 12 is weighted -20 because it is a bad idea to take such a 
square when the corner is empty - the opponent will then have a chance to capture 
the corner, regaining the 12 square as well. Thus, we want squares like 12 to have a 
negative score when the corner is empty, but not when it is already occupied. The 
modi f i ed - wei ghted - squa res evaluation function does just that. 

(defun modified-weighted-squares (player board) 
"Like WEIGHTED-SQUARES, but don't take off for moving 
near an occupied corner." 
(let ((w (weighted-squares player board))) 

(dolist (corner '(11 18 81 88)) 
(when (not (eql (bref board corner) empty)) 
(dolist (c (neighbors corner)) 
(when (not (eql (bref board c) empty)) 
(incf w (* (-5 (aref *weights* c)) 
(if (eql (bref board c) player) 
+1 -1))))))) 
w)) 

(let ((neighbor-table (make-array 100 linitial-element nil))) 
;; Initialize the neighbor table 
(dolist (square all-squares) 

(dolist (dir all-directions) 
(if (valid-p (+ square dir)) 
(push (+ square dir) 
(aref neighbor-table square))))) 

(defun neighbors (square) 
"Return a list of all squares adjacent to a square." 
(aref neighbor-table square))) 

<a id='page-622'></a>

18.7 The Tournament Version of Othello 
While the othel 1 o function serves as a perfectly good moderator for casual play, 
there are two points that need to be fixed for tournament-level play. First, tournament 
games are played under a strict time limit: a player who takes over 30 minutes total 
to make all the moves forfeits the game. Second, the standard notation for Othello 
games uses square names in the range al to h8, rather than in the 11 to 88 range that 
we have used so far. al is the upper left corner, a8 is the lower left corner, and h8 is 
the lower right corner. We can write routines to translate between this notation and 
the one we were using by creating a table of square names. 

(let ((square-names 

(cross-product #'symbol 
'(? a b c d e f g h ?) 
'(712345678 ?)))) 

(defun h8->88 (str) 
"Convert from alphanumeric to numeric square notation." 
(or (position (string str) square-names rtest #'string-equal) 

str)) 

(defun 88->h8 (num) 
"Convert from numeric to alphanumeric square notation." 
(if (valid-p num) 

(elt square-names num) 
num))) 

(defun cross-product (fn xlist ylist) 
"Return a list of all (fn . y) values." 
(mappend #*(lambda (y) 

(mapcar #'(lambda (x) (funcall fn . y)) 
xlist)) 
ylist)) 

Note that these routines return their input unchanged when it is not one of the 
expected values. This is to allow commands other than moving to a particular 
square. For example, we will add a feature that recognizes res i gn as a move. 

The h uma. player needs to be changed slightly to read moves in this format. While 
we're at it, we'll also print the list of possible moves: 

(defun human (player board) 
"A human player for the game of Othello" 
(format t "~&~c to move "a: " (name-of player) 

(mapcar #*88->h8 (legal-moves player board))) 
(h8->88 (read))) 

<a id='page-623'></a>
Top-Level Functions 

Othello-series Play a series of . games. 
random-Othello-series Play a series of games, starting from a random position. 
round-robin Play a tournament among strategies. 

Special Variables 

*clock* A copy of the game clock (tournament version only). 
*board* A copy of the game board (tournament version only). 
*move-number* Number of moves made (tournament version only). 
*ply-boards* A vector of boards; used as a resource to avoid consing. 

Data Structures 

node Holds a board and its evaluation. 

Main Functions 

alpha-beta2 Sorts moves by static evaluation. 
alpha-beta-searcher2 Strategy using a1 pha- beta2. 
alpha-beta3 Uses the killer heuristic. 
alpha-beta-searcher3 Strategy using a1 pha- beta3. 
lago-eval Evaluation function based on Rosenbloom's program. 
lago Strategy using lago-eval. 

Auxiliary Functions 

h8->88 Convert from alphanumeric to numeric square notation. 
88->h8 Convert from numeric to alphanumeric square notation. 
time-string Convert internal time units to a mm.ss string. 
switch-strategies Play one strategy for a while, then another. 
mobil ity A strategy that counts the number of legal moves. 
legal-nodes A list of legal moves sorted by their evaluation. 
negate-node Set the value of a node to its negative. 
put-first Put the killer move first, if it is legal. 

Previously Defined Fimctions 

cross-product Apply fn to all pairs of arguments, (pg. 47) 
symbol Build a symbol by concatenating components. 

Figure 18.5: Glossary for the Tournament Version of Othello 

The othel 10 function needn't worry about notation, but it does need to monitor the 
time. We make up a new data structure, the clock, which is an array of integers 
saying how much time (in internal units) each player has left. For example, (aref 
cl ock bl ack) is the amount of time black has left to make all his moves. In Pascal, 
we would declare the clock array as arrayCbl ack. .white], but in Common Lisp all 
arrays are zero-based, so we need an array of three elements to allow the subscript 
black, which is 2. 

The clock is passed to get - move and print - boa rd but is otherwise unused. I could 
have complicated the main game loop by adding tests for forfeits because of expired 
time and, as we shall see later, resignation by either player. However, I felt that would 
add a great deal of complexity for rarely used options. Instead, I wrap the whole game 
loop, along with the computation of the final score, in a catch special form. Then, if 

<a id='page-624'></a>

get-move encounters a forfeit or resignation, it can throw an appropriate final score: 
64 or -64, depending on which player forfeits. 

(defvar *move-number* 1 "The number of the move to be played") 

(defun Othello (bl-strategy wh-strategy 

&optional (print t) (minutes 30)) 
"Play a game of Othello. Return the score, where a positive 
difference means black, the first player, wins." 
(let ((board (initial-board)) 

(clock (make-array (+ 1 (max black white)) 
:initial-element 
(* minutes 60 

internal-time-units-per-second)))) 
(catch 'game-over 

(loop for *move-number* from 1 
for player = black then (next-to-play board player print) 
for strategy = (if (eql player black) 

bl-strategy 

wh-strategy) 
until (null player) 
do (get-move strategy player board print clock)) 

(when print 
(format t "~&The game is over. Final result:") 
(print-board board clock)) 

(count-difference black board)))) 

Strategies now have to comply with the time-limit rule, so they may want to look at 
the time remaining. Rather than passing the clock in as an argument to the strategy,I 
decided to store the clock in the special variable *cl ock*. The new version of othel 10 
also keeps track of the *move-number*. This also could have been passed to the 
strategy functions as a parameter. But adding these extra arguments would require 
changes to all the strategies we have developed so far. By storing the information in 
special variables, strategies that want to can look at the clock or the move number, 
but other strategies don't have to know about them. 

We still have the security problem-we don't want a strategy to be able to set the 
opponent's remaining time to zero and thereby win the game. Thus, we use *cl ock* 
only as a copy of the "real" game clock. The function repl ace copies the real clock 
into *cl ock*, and also copies the real board into *board*. 

(defvar *clock* (make-array 3) "A copy of the game clock") 
(defvar *board* (initial-board) "A copy of the game board") 

<a id='page-625'></a>

(defun get-move (strategy player board print clock) 
"Call the player's strategy function to get a move. 
Keep calling until a legal move is made." 

Note we don't pass the strategy function the REAL board. 
;; If we did, it could cheat by changing the pieces on the board, 
(when print (print-board board clock)) 
(replace *clock* clock) 
(let* ((to (get-internal-real-time)) 

(move (funcall strategy player (replace *board* board))) 

(tl (get-internal-real-time))) 
(decf (elt clock player) (- tl tO)) 
(cond 

((< (elt clock player) 0) 
(format t ""&^c has no time left and forfeits." 
(name-of player)) 
(THROW 'game-over (if (eql player black) -64 64))) 
((eq move 'resign) 
(THROW 'game-over (if (eql player black) -64 64))) 
((and (valid-p move) (legal-p move player board)) 
(when print 
(format t "^&'O moves to ~a. " 
(name-of player) (88->h8 move))) 
(make-move move player board)) 
(t (warn "Illegal move: ~a" (88->h8 move)) 
(get-move strategy player board print clock))))) 

Finally, the function print - boa rd needs to print the time remaining for each player; 
this requires an auxiliary function to get the number of minutes and seconds from an 
internal-format time interval. Note that we make the arguments optional, so that in 
debugging one can say just (print- board) to see the current situation. Also note the 
esoteric format option: " ~2 / Od" prints a decimal number using at least two places, 
padding on the left with zeros. 

(defun print-board (&optional (board *board*) clock) 
"Print a board, along with some statistics." 
First print the header and the current score 

(format t "~2& a b c d e f g h [~c=~2a ~c=~2a ("d)]" 
(name-of black) (count black board) 
(name-of white) (count white board) 
(count-difference black board)) 

Print the board itself 

(loop for row from 1 to 8 do 
(format t "~& ~d " row) 
(loop for col from 1 to 8 

for piece = (bref board (+ col (* 10 row))) 

do (format t "~c " (name-of piece)))) 

<a id='page-626'></a>

;: Finally print the time remaining for each player 
(when clock 

(format t " ["'c='"a ~c=~a]~2&" 
(name-of black) (time-string (elt clock black)) 
(name-of white) (time-string (elt clock white))))) 

(defun time-string (time) 
"Return a string representing this internal time in minisecs. 
(multiple-value-bind (min sec) 

(floor (round time internal-time-units-per-second) 60) 
(format nil ""Zdrz/Od" min sec))) 

18.8 Playing a Series of Games 
A single game is not enough to establish that one strategy is better than another. The 
following function allows two strategies to compete in a series of games: 

(defun Othello -series (strategyl strategy2 n-pairs) 
"Play a series of 2*n-pairs games, swapping sides." 
(let ((scores (loop repeat n-pairs 

collect (Othello strategyl strategy2 nil) 

collect (- (Othello strategy2 strategyl nil))))) 
Return the number of wins, (1/2 for a tie), 
the total of thepoint differences, and the 
scores themselves, all from strategyl's point of view, 

(values (+ (count-if #'plusp scores) 

(/ (count-if #*zerop scores) 2)) 
(apply #'+ scores) 
scores))) 

Let's see what happens when we use it to pit the two weighted-squares functions 
against each other in a series of ten games: 

> (othello-series 
(alpha-beta-searcher 2 #*modified-weighted-squares) 
(alpha-beta-searcher 2 #'weighted-squares) 5) 

0 
60 
(-28 40 -28 40 -28 40 -28 40 -28 40) 

Something is suspicious here - the same scores are being repeated. A little thought 
reveals why: neither strategy has a random component, so the exact same game 
was played five times with one strategy going first, and another game was played 

<a id='page-627'></a>

five times when the other strategy goes first! A more accurate appraisal of the two 

strategies' relative worth would be gained by starting each game from some random 

position and playing from there. 

Think for a minute how you would design to run a series of games starting from a 
random position. One possibility would be to change the function othel 1 o to accept 
an optional argument indicating the initial state of the board. Then othel 1 o- seri es 
could be changed to somehow generate a random board and pass it to othel 1 o. While 
this approach is feasible, it means changing two existing working functions, as well 
as writing another function, generate - random-board. But we couldn't generate just 
any random board: it would have to be a legal board, so it would have to call othel 1 o 
and somehow get it to stop before the game was over. 

An alternative is to leave both Othello and othello-series alone and build 
another function on top of it, one that works by passing in two new strategies: 
strategies that make a random move for the first few moves and then revert to 
the normal specified behavior. This is a better solution because it uses existing 
functions rather than modifying them, and because it requires no new functions 
besides switch-strategies, which could prove useful for other purposes, and 
random-othel lo-seri es, which does nothing more than call othel lo-seri es with 
the proper arguments. 

(defun random-Othello-series (strategyl strategy2 

n-pairs &optional (n-random 10)) 
"Play a series of 2*n games, starting from a random position." 
(othello-series 

(switch-strategies #'random-strategy n-random strategyl) 
(switch-strategies #*random-strategy n-random strategy2) 
n-pairs)) 

(defun switch-strategies (strategyl m strategy2) 
"Make a new strategy that plays strategyl for m moves, 
then plays according to strategy2." 
#'(lambda (player board) 

(funcall (if (<= *move-number* m) strategyl strategy2) 
player board))) 

There is a problem with this kind of series: it may be that one of the strategies just 

happens to get better random positions. A fairer test would be to play two games 

from each random position, one with the each strategy playing first. One way to 

do that is to alter othel 1 o-seri es so that it saves the random state before playing 

the first game of a pair, and then restores the saved random state before playing the 

second game. That way the same random position will be duplicated. 

<a id='page-628'></a>

(defun Othello-series (strategyl strategy2 n-pairs) 
"Play a series of 2*n-pairs games, swapping sides." 
(let ((scores 

(loop repeat n-pairs 
for random-state = (make-random-state) 
collect (Othello strategyl strategy2 nil) 
do (setf *random-state* random-state) 
collect (- (Othello strategy2 strategyl nil))))) 

Return the number of wins (1/2 for a tie), 
the total of the point differences, and the 
scores themselves, all from strategyl's point of view, 

(values (+ (count-if #*plusp scores) 

(/ (count-if #*zerop scores) 2)) 
(apply #'+ scores) 
scores))) 

Now we are in a position to do a more meaningful test. In the following, the weighted-
squares strategy wins 4 out of 10 games against the modified strategy, losing by a 
total of 76 pieces, with the actual scores indicated. 

> (random-Othello-series 
(alpha-beta-searcher 2 #'weighted-squares) 
(alpha-beta-searcher 2#'modified-weighted-squares) 
5) 

4 
-76 
(-8 -40 22 -30 10 -10 12 -18 4 -18) 

The random- othel lo-series function is useful for comparing two strategies. When 
there are more than two strategies to be compared at the same time, the following 
function can be useful: 

(defun round-robin (strategies n-pairs &optional 

(n-random 10) (names strategies)) 
"Play a tournament among the strategies. 
N-PAIRS = games each strategy plays as each color against 
each opponent. So with . strategies, a total of 
N*(N-1)*N-PAIRS games are played." 
(let* ((N (length strategies)) 

(totals (make-array . .-initial-element 0)) 
(scores (make-array (list . .) 
:i ni ti al-element 0))) 
Play the games 
(dotimes (IN) 
(loop for j from (+ i 1) to (- . 1) do 
(let* ((wins (random-Othello-series 

<a id='page-629'></a>
(elt strategies i) 
(elt strategies j) 
n-pairs n-random)) 

(losses (- (* 2 n-pairs) wins))) 
(incf (aref scores i j) wins) 
(incf (aref scores j i) losses) 
(incf (aref totals i) wins) 
(incf (aref totals j) losses)))) 

Print the results 

(dotimes (i N) 
(format t "~ra~20T ~4f: " (elt names i) (elt totals i)) 
(dotimes (j N) 

(format t "~4f " (if (= i j) 
(aref scores i j))))))) 

Here is a comparison of five strategies that search only 1 ply: 

(defun mobility (player board) 
"The number of moves a player has." 
(length (legal-moves player board))) 

> (round-robin 

(list (maximizer #'count-difference) 
(maximizer #'mobility) 
(maximizer #*weighted-squares) 
(maximizer #'modified-weighted-squares) 
#'random-strategy) 

5 10 
'(count-difference mobility weighted modified-weighted random)) 

COUNT-DIFFERENCE 12.5: --3.0 
2.5 0.0 7.0 
MOBILITY 20.5: 7.0 --1.5 
5.0 7.0 
WEIGHTED 28.0: 7.5 8.5 --3.0 
9.0 
MODIFIED-WEIGHTED 31.5: 10.0 5.0 7.0 --9.5 
RANDOM 7.5: 3.0 3.0 1.0 0.5 --


The parameter .-pai rs is 5, meaning that each strategy plays five games as black 
and five as white against each of the other four strategies, for a total of 40 games 
for each strategy and 100 games overall. The first line of output says that the count-
difference strategy won 12.5 of its 40 games, including 3 against the mobility strategy, 

2.5 against the weighted strategy, none against the modified weighted, and 7 against 
the random strategy. The fact that the random strategy manages to win 7.5 out of 40 
games indicates that the other strategies are not amazingly strong. Now we see what 
happens when the search depth is increased to 4 ply (this will take a while to run): 
<a id='page-630'></a>

> (round-robi. 

(list (alpha-beta-searcher 4 #*count-difference) 
(alpha-beta-searcher 4 #'weighted-squares) 
(alpha-beta-searcher 4 #'modified-weighted-squares) 
#'random-strategy) 

5 10 

'(count-difference weighted modified-weighted random)) 

COUNT-DIFFERENCE 12.0: --
2.0 0.0 10.0 
WEIGHTED 23.5: 8.0 .. . 5.5 10.0 
MODIFIED-WEIGHTED 24.5: 10.0 4.5 .. . 10.0 
RANDOM 0.0: 0.0 0.0 0.0 

Here the random strategy does not win any games - an indication that the other 
strategies are doing something right. Notice that the modified weighted-squares 
has only a slight advantage over the weighted-squares, and in fact it lost their head-
to-head series, four games to five, with one draw. So it is not clear which strategy 
is better. 

The output does not break down wins by black or white, nor does it report the 
numerical scores. I felt that that would clutter up the output too much, but you're 
welcome to add this information. It turns out that white wins 23 (and draws 1) of 
the 40 games played between 4-ply searching strategies. Usually, Othello is a fairly 
balanced game, because black has the advantage of moving first but white usually 
gets to play last. It is clear that these strategies do not play well in the operung game, 
but for the last four ply they play perfectly. This may explain white's slight edge, or 
it may be a statistical aberration. 

18.9 More Efficient Searching 
The alpha-beta cutoffs work when we have established a good move and another 
move proves to be not as good. Thus, we will be able to make cutoffs earlier if we 
ensure that good moves are considered first. Our current algorithm loops through 
the list of 1 egal -moves, but 1 egal -moves makes no attempt to order the moves in any 
way. We will call this the random-ordering strategy (even though the ordering is not 
random at all-square 11 is always considered first, then 12, etc.). 

One way to try to generate good moves first is to search highly weighted squares 
first. Since 1 egal -moves considers squares in the order defined by all -squares, all 
we have to do is redefine the list al 1 -squares^: 

^Remember, when a constant is redefined, it may be necessary to recompile any functions 
that use the constant. 

<a id='page-631'></a>

(defconstant all-squares 
(sort (loop for i from 11 to 88 
when (<= 1 (mod i 10) 8) collect i) 
#*> :key #'(lambda (sq) (elt *weights* sq)))) 

Now the corner squares will automatically be considered first, followed by the other 
highly weighted squares. We call this the static-ordering strategy, because the ordering 
is not random, but it does not change depending on the situation. 

A more informed way to try to generate good moves first is to sort the moves 
according to the evaluation function. This means making more evaluations. Previously, 
only the boards at the leaves of the search tree were evaluated. Now we need 
to evaluate every board. In order to avoid evaluating a board more than once, we 
make up a structure called a node, which holds a board, the square that was taken to 
result in that board, and the evaluation value of that board. The search is the same 
except that nodes are passed around instead of boards, and the nodes are sorted by 
their value. 

(defstruct (node) square board value) 

(defun alpha-beta-searcher2 (depth eval-fn) 
"Return a strategy that does A-B search with sorted moves." 
#'(lambda (player board) 

(multiple-value-bind (value node) 
(alpha-beta2 
player (make-node :board board 
.-value (funcall eval-fn player board)) 

losing-value winning-value depth eval-fn) 
(declare (ignore value)) 
(node-square node)))) 

(defun alpha-beta2 (player node achievable cutoff ply eval-fn) 
"A-B search, sorting moves by eval-fn" 
;; Returns two values: achievable-value and move-to-make 
(if (= ply 0) 

(values (node-value node) node) 
(let* ((board (node-board node)) 
(nodes (legal-nodes player board eval-fn))) 
(if (null nodes) 
(if (any-legal-move? (opponent player) board) 

(values (- (alpha-beta2 (opponent player) 
(negate-value node) 
(- cutoff) (- achievable) 
(- ply 1) eval-fn)) 

nil) 
(values (final-value player board) nil)) 
(let ((best-node (first nodes))) 
(loop for move in nodes 

<a id='page-632'></a>

for val = (- (alpha-betaZ 
(opponent player) 
(negate-value move) 
(- cutoff) (- achievable) 
(- ply 1) eval-fn)) 

do (when (> val achievable) 
(setf achievable val) 
(setf best-node move)) 

until (>= achievable cutoff)) 
(values achievable best-node)))))) 

(defun negate-value (node) 
"Set the value of a node to its negative." 
(setf (node-value node) (- (node-value node))) 
node) 

(defun legal-nodes (player board eval-fn) 
"Return a list of legal moves, each one packed into a node." 
(let ((moves (legal-moves player board))) 

(sort (map-into 
moves 
#*(lambda (move) 

(let ((new-board (make-move move player 
(copy-board board)))) 

(make-node 
: squa re move .-board new-board 
:value (funcall eval-fn player new-board)))) 

moves) 
#'> :key #'node-value))) 

(Note the use of the function map -i nto. This is part of ANSI Common Lisp, but if it 
is not a part of your implementation, a definition is provided on [page 857](chapter24.md#page-857).) 

The following table compares the performance of the random-ordering strategy, 
the sorted-ordering strategy and the static-ordering strategy in the course of a single 
game. All strategies search 6 ply deep. The table measures the number of boards 
investigated, the number of those boards that were evaluated (in all cases the evaluation 
function was modi f i ed - wei ghted - squa res) and the time in seconds to compute 
a move. 

<a id='page-633'></a>
random order sorted order static order 
boards evals sees boards evals sees boards evals sees 
13912 10269 69 5556 5557 22 2365 1599 19 
9015 6751 56 6571 6572 25 3081 2188 18 
9820 7191 46 11556 11557 45 5797 3990 31 
4195 3213 20 5302 5303 17 2708 2019 15 
10890 7336 60 10709 10710 38 3743 2401 23 
13325 9679 63 6431 6432 24 4222 2802 24 
13163 9968 58 9014 9015 32 6657 4922 31 
16642 12588 70 9742 9743 33 10421 7488 51 
18016 13366 80 11002 11003 37 9508 7136 41 
23295 17908 104 15290 15291 48 26435 20282 111 
34120 25895 143 22994 22995 75 20775 16280 78 
56117 43230 224 46883 46884 150 48415 36229 203 
53573 41266 209 62252 62253 191 37803 28902 148 
43943 33184 175 31039 31040 97 33180 24753 133 
51124 39806 193 45709 45710 135 19297 15064 69 
24743 18777 105 20003 20004 65 15627 11737 66 
1.0 1.0 1.0 .81 1.07 .62 .63 .63 .63 

The last two lines of the table give the averages and the averages normalized to the 
random-ordering strategy's performance. The sorted-ordering strategy takes only 
62% of the time of the random-ordering strategy, and the static-ordering takes 63 %. 
These times are not to be trusted too much, because a large-scale garbage collection 
was taking place during the latter part of the game, and it may have thrown off the 
times. The board and evaluation count may be better indicators, and they both show 
the static-ordering strategy doing the best. 

We have to be careful how we evaluate these results. Earlier I said that alpha-beta 
search makes more cutoffs when it is presented first with better moves. The actual 
truth is that it makes more cutoffs when presented first with moves that the evaluation 
function thinks are better. In this case the evaluation function and the static-ordering 
strategy are in strong agreement on what are the best moves, so it is not surprising 
that static ordering does so well. As we develop evaluation functions that vary from 
the weighted-squares approach, we will have to run experiments again to see if the 
static-ordering is still the best. 

18.10 It Pays to Precycle 
The progressive city of Berkeley, California, has a strong recycling program to reclaim 
glass, paper, and aluminum that would otherwise be discarded as garbage. In 1989, 

<a id='page-634'></a>

Berkeley instituted a novel program of precycling: consumers are encouraged to avoid 
buying products that come in environmentally wasteful packages. 

Your Lisp system also has a recycling program: the Lisp garbage collector automatically 
recycles any unused storage. However, there is a cost to this program, and 
you the consumer can get better performance by precycling your data. Don't buy 
wasteful data structures when simpler ones can be used or reused. You, the Lisp 
programmer, may not be able to save the rain forests or the ozone layer, but you can 
save valuable processor time. 

We saw before that the search routines look at tens of thousands of boards per 
move. Currently, each board position is created anew by copy-board and discarded 
soon thereafter. We could avoid generating all this garbage by reusing the same board 
at each ply. We'd still need to keep the board from the previous ply for use when 
the search backs up. Thus, a vector of boards is needed. In the following we assume 
that we will never search deeper than 40 ply. This is a safe assumption, as even the 
fastest Othello programs can only search about 15 ply before running out of time. 

(defvar *ply-boards* 
(apply #*vector (loop repeat 40 collect (initial-board)))) 

Now that we have sharply limited the number of boards needed, we may want to 
reevaluate the implementation of boards. Instead of having the board as a vector of 
pieces (to save space), we may want to implement boards as vectors of bytes or full 
words. In some implementations, accessing elements of such vectors is faster. (In 
other implementations, there is no difference.) 

An implementation using the vector of boards will be done in the next section. 
Note that there is another alternative: use only one board, and update it by making 
and retracting moves. This is a good alternative in a game like chess, where a move 
only alters two squares. In Othello, many squares can be altered by a move, so 
copying the whole board over and making the move is not so bad. 

It should be mentioned that it is worth looking into the problem of copying a 
position from one board to another. The function repl ace copies one sequence (or 
part of it) into another, but it is a generic function that may be slow. In particular, if 
each element of a board is only 2 bits, then it may be much faster to use displaced 
arrays to copy 32 bits at a time. The advisability of this approach depends on the 
implementation, and so it is not explored further here. 

18.11 Killer Moves 
In section 18.9, we considered the possibility of searching moves in a different 
order, in an attempt to search the better moves first, thereby getting more alpha-beta 
pruning. In this section, we consider the killer heunstic, which states that a move that 

<a id='page-635'></a>
has proven to be a good one in one line of play is also likely to be a good one in another 
line of play. To use chess as perhaps a more familiar example, suppose I consider 
one move, and it leads to the opponent replying by capturing my queen. This is a 
killer move, one that I would like to avoid. Therefore, when I consider other possible 
moves, I want to immediately consider the possibility of the opponent making that 
queen-capturing move. 

The function alpha-beta3 adds the parameter ki 11 er, which is the best move 
found so far at the current level. After we determine the legal -moves, we use 
put-first to put the killer move first, if it is in fact a legal move. When it comes 
time to search the next level, we keep track of the best move in kiHer2. This 
requires keeping track of the value of the best move in ki 11 er 2- va1. Everything else 
is unchanged, except that we get a new board by recycling the *pl y-boards* vector 
rather than by allocating fresh ones. 

(defun alpha-betaS (player board achievable cutoff ply eval-fn 

killer) 
"A-. search, putting killer move first." 
(if (= ply 0) 

(funcall eval-fn player board) 
(let ((moves (put-first killer (legal-moves player board)))) 
(if (null moves) 
(if (any-legal-move? (opponent player) board) 

(- (alpha-betaS (opponent player) board 
(- cutoff) (- achievable) 
(- ply 1) eval-fn nil)) 

(final-value player board)) 

(let ((best-move (first moves)) 
(new-board (aref *ply-boards* ply)) 
(killer2 nil) 
(killer2-val winning-value)) 

(loop for move in moves 
do (multiple-value-bind (val reply) 

(alpha-betaS 
(opponent player) 
(make-move move player 

(replace new-board board)) 
(- cutoff) (- achievable) 
(- ply 1) eval-fn killer2) 

(setf val (- val)) 

(when (> val achievable) 
(setf achievable val) 
(setf best-move move)) 

(when (and reply (< val killer2-val)) 
(setf killer2 reply) 
(setf killer2-val val))) 

until (>= achievable cutoff)) 

<a id='page-636'></a>

(values achievable best-move)))))) 

(defun alpha-beta-searcher3 (depth eval-fn) 
"Return a strategy that does A-B search with killer moves." 
#'(lambda (player board) 

(multiple-value-bind (value move) 
(alpha-betaS player board losing-value winning-value 

depth eval-fn nil) 
(declare (ignore value)) 
move))) 

(defun put-first (killer moves) 
"Move the killer move to the front of moves, 
if the killer move is in fact a legal move." 
(if (member killer moves) 

(cons killer (delete killer moves)) 
moves)) 

Another experiment on a single game reveals that adding the killer heuristic to static-
ordering search (again at 6-ply) cuts the number of boards and evaluations, and the 
total time, all by about 20%. To summarize, alpha-beta search at 6 ply with random 
ordering takes 105 seconds per move (in our experiment), adding static-ordering cuts 
it to 66 seconds, and adding killer moves to that cuts it again to 52 seconds. This 
doesn't include the savings that alpha-beta cutoffs give over full minimax search. At 
6 ply with a branching factor of 7, full minimax would take about nine times longer 
than static ordering with killers. The savings increase with increased depth. At 
7 ply and a branching factor of 10, a small experiment shows that static-ordering 
with killers looks at only 28,000 boards in about 150 seconds. Full minimax would 
evaluate 10 million boards and take 350 times longer. The times for full minimax are 
estimates based on the number of boards per second, not on an actual experiment. 

The algorithm in this section just keeps track of one killer move. It is of course 
possible to keep track of more than one. The Othello program Bill (Lee and Mahajan 
1990b) merges the idea of killer moves with legal move generation: it keeps a list of 
possible moves at each level, sorted by their value. The legal move generator then 
goes down this list in sorted order. 

It should be stressed once again that all this work on alpha-beta cutoffs, ordering, 
and killer moves has not made any change at all in the moves that are selected. We 
still end up choosing the same move that would be made by a full minimax search to 
the given depth, we are just doing it faster, without looking at possibilities that we 
can prove are not as good. 

<a id='page-637'></a>
18.12 Championship Programs: lago and Bill 
As mentioned in the introduction, the unpredictability of Othello makes it a difficult 
game for humans to master, and thus programs that search deeply can do comparatively 
well. In fact, in 1981 the reigning champion, Jonathan Cerf, proclaimed "In 
my opinion the top programs ... are now equal (if not superior) to the best human 
players." In discussing Rosenbloom's lago program (1982), Cerf went on to say "I 
understand Paul Rosenbloom is interested in arranging a match against me. Unfortunately 
my schedule is very full, and I'm going to see that it remains that way for the 
foreseeable future." 

In 1989, another program. Bill (Lee and Mahajan 1990) beat the highest rated 
American Othello player, Brian Rose, by a score of 56-8. Bill's evaluation function is 
fast enough to search 6-8 ply under tournament conditions, yet it is so accurate that 
it beats its creator, Kai-Fu Lee, searching only 1 ply. (However, Lee is only a novice 
Othello player; his real interest is in speech recognition; see Waibel and Lee 1991.) 
There are other programs that also play at a high level, but they have not been written 
up in the AI literature as lago and Bill have. 

In this section we present an evaluation function based on lago's, although it also 
contains elements of Bill, and of an evaluation function written by Eric Wef aid in 1989. 
The evaluation function makes use of two main features: mobility and edge stability. 

Mobility 

Both lago and Bill make heavy use of the concept of mobility. Mobility is a measure of 
the ability to make moves; basically, the more moves one can make, the better. This 
is not quite true, because there is no advantage in being able to make bad moves, 
but it is a useful heuristic. We define current mobility as the number of legal moves 
available to a player, and potential mobility as the number of blank squares that are 
adjacent to opponent's pieces. These include the legal moves. A better measure of 
mobility would try to count only good moves. The following function computes both 
current and potential mobility for a player: 

(defun mobility (player board) 
"Current mobility is the number of legal moves. 
Potential mobility is the number of blank squares 
adjacent to an opponent that are not legal moves. 
Returns current and potential mobility for player." 
(let ((opp (opponent player)) 

(current 0) ; player's current mobility 
(potential 0)) ; player's potential mobility 
(dolist (square all-squares) 
(when (eql (bref board square) empty) 
(cond ((legal-p square player board) 

<a id='page-638'></a>

(incf current)) 
((some #.(lambda (sq) (eql (bref board sq) opp)) 
(neighbors square)) 
(incf potential))))) 
(values current (+ current potential)))) 

Edge Stability 

Success at Othello often hinges around edge play, and both lago and Bill evaluate 
the edges carefully. Edge analysis is made easier by the fact that the edges are fairly 
independent of the interior of the board: once a piece is placed on the edge, no 
interior moves can flip it. This independence allows a simplifying assumption: to 
evaluate a position's edge strength, evaluate each of the four edges independently, 
without consideration of the interior of the board. The evaluation can be made more 
accurate by considering the X-squares to be part of the edge. 

Even evaluating a single edge is a time-consuming task, so Bill and lago compile 
away the evaluation by building a table of all possible edge positions. An "edge" 
according to Bill is ten squares: the eight actual edge squares and the two X-squares. 
Since each square can be black, white, or empty, there are 3^^ or 59,049 possible edge 
positions - a large but manageable number. 

The value of each edge position is determined by a process of succesive approximation. 
Just as in a minimax search, we will need a static edge evaluation function 
to determine the value of a edge position without search. This static edge evaluation 
function is appHed to every possible edge position, and the results are stored in a 
59,049 element vector. The static evaluation is just a weighted sum of the occupied 
squares, with different weights given depending on if the piece is stable or unstable. 

Each edge position's evaluation can be improved by a process of search. lago 
uses a single ply search: given a position, consider all moves that could be made 
(including no move at all). Some moves will be clearly legal, because they flip pieces 
on the edge, but other moves will only be legal if there are pieces in the interior of 
the board to flip. Since we are only considering the edge, we don't know for sure if 
these moves are legal. They will be assigned probabilities of legality. The updated 
evaluation of a position is determined by the values and probabilities of each move. 
This is done by sorting the moves by value and then summing the product of the 
value times the probability that the move can be made. This process of iterative 
approximation is repeated five times for each position. At that point, Rosenbloom 
reports, the values have nearly converged. 

In effect, this extends the depth of the normal alpha-beta search by including an 
edge-only search in the evaluation function. Since each edge position with . pieces 
is evaluated as a function of the positions with . -h 1 pieces, the search is complete-it 
is an implicit 10-ply search. 

<a id='page-639'></a>
Calculating edge stability is a bit more complicated than the other features. The 
first step is to define a variable, *eclge - table*, which will hold the evaluation of each 
edge position, and a constant, edge-and-x-1 i sts, which is a list of the squares on 
each of the four edges. Each edge has ten squares because the X-squares are included. 

(defvar *edge-table* (make-array (expt 3 10)) 
"Array of values to player-to-move for edge positions.") 

(defconstant edge-and-x-1ists 

'((22 11 12 13 14 15 16 17 18 27) 
(72 81 82 83 84 85 86 87 88 77) 
(22 11 21 31 41 51 61 71 81 72) 
(27 18 28 38 48 58 68 78 88 77)) 

"The four edges (with their X-squares).") 

Now for each edge we can compute an index into the edge table by building a 10-digit 
base-3 number, where each digit is 1 if the corresponding edge square is occupied by 
the player, 2 if by the opponent, and 0 if empty. The function edge- i ndex computes 
this, and edge - stabi 1 i ty sums the values of the four edge indexes. 

(defun edge-index (player board squares) 
"The index counts 1 for player; 2 for opponent, 
on each square---summed as a base 3 number." 
(let ((index 0)) 

(dolist (sq squares) 
(setq index (+ (* index 3) 

(cond ((eql (bref board sq) empty) 0) 
((eql (bref board sq) player) 1) 
(t 2))))) 

index)) 

(defun edge-stability (player board) 
"Total edge evaluation for player to move on board." 
(loop for edge-list in edge-and-x-1ists 

sum (aref *edge-table* 
(edge-index player board edge-list)))) 

The function edge - stabi 1 i ty is all we will need in lago's evaluation function, but we 

still need to generate the edge table. Since this needs to be done only once, we don't 

have to worry about efficiency. In particular, rather than invent a new data structure 

to represent edges, we will continue to use complete boards, even though they will 

be mostly empty. The computations for the edge table will be made on the top edge, 

from the point of view of black, with black to play. But the same table can be used for 

white, or for one of the other edges, because of the way the edge index is computed. 

Each position in the table is first initialized to a static value computed by a kind 
of weighted-squares metric, but with different weights depending on if a piece is in 

<a id='page-640'></a>

danger of being captured. After that, each position is updated by considering the 
possible moves that can be made from the position, and the values of each of these 
moves. 

(defconstant top-edge (first edge-and-x-lists)) 

(defun init-edge-table () 
"Initialize *edge-table*. starting from the empty board." 
Initialize the static values 
(loop for n-pieces from 0 to 10 do 
(map-edge-n-pieces 
#*(lambda (board index) 
(setf (aref *edge-table* index) 
(static-edge-stability black board))) 
black (initial-board) n-pieces top-edge 0)) 
Now iterate five times trying to improve: 

(dotimes (i 5) 
;; Do the indexes with most pieces first 
(loop for n-pieces from 9 downto 1 do 

(map-edge-n-pieces 
#'(lambda (board index) 
(setf (aref *edge-table* index) 
(possible-edge-moves-value 
black board index))) 
black (initial-board) n-pieces top-edge 0)))) 

The function map-edge-n-pieces iterates through all edge positions with a total of 
. pieces (of either color), applying a function to each such position. It also keeps a 
running count of the edge index as it goes. The function should accept two arguments: 
the board and the index. Note that a single board can be used for all the positions 
because squares are reset after they are used. The function has three cases: if the 
number of squares remaining is less than n, then it will be impossible to place . pieces 
on those squares, so we give up. If there are no more squares then . must also be 
zero, so this is a valid position, and the function f . is called. Otherwise we first try 
leaving the current square blank, then try filling it with player's piece, and then with 
the opponent's piece, in each case calling map-edge-.-pi eces recursively. 

(defun map-edge-n-pieces (fn player board . squares index) 
"Call fn on all edges with . pieces." 
;; Index counts 1 for player; 2 for opponent 
(cond 

((< (length squares) n) nil) 
((null squares) (funcall fn board index)) 
(t (let ((index3 (* 3 index)) 

(sq (first squares))) 
(map-edge-n-pieces fn player board . (rest squares) indexS) 

<a id='page-641'></a>
(when (and (> . 0) (eql (bref board sq) empty)) 
(setf (bref board sq) player) 
(map-edge-n-pieces fn player board (- . 1) (rest squares) 

(+ 1 index3)) 
(setf (bref board sq) (opponent player)) 
(map-edge-n-pieces fn player board (- . 1) (rest squares) 

(+ 2 indexS)) 
(setf (bref board sq) empty)))))) 

The function possible-edge-moves-value searches through all possible moves to 
determine an edge value that is more accurate than a static evaluation. It loops 
through every empty square on the edge, calling possible-edge-move to return a 
(probability value) pair. Since it is also possible for a player not to make any move at 
all on an edge, the pair (1.0 current-value) is also included. 

(defun possible-edge-moves-value (player board index) 
"Consider all possible edge moves. 
Combine their values into a single number." 
(combine-edge-moves 

(cons 
(list 1.0 (aref *edge-table* index)) ;; no move 
(loop for sq in top-edge ;; possible moves 

when (eql (bref board sq) empty) 
collect (possible-edge-move player board sq))) 
player)) 

The value of each position is determined by making the move on the board, then 
looking up in the table the value of the resulting position for the opponent, and 
negating it (since we are interested in the value to us, not to our opponent). 

(defun possible-edge-move (player board sq) 
"Return a (prob val) pair for a possible edge move." 
(let ((new-board (replace (aref *ply-boards* player) board))) 

(make-move sq player new-board) 
(list (edge-move-probability player board sq) 
(- (aref *edge-table* 
(edge-index (opponent player) 
new-board top-edge)))))) 

The possible moves are combined with combi ne-edge-moves, which sorts the moves 
best-first. (Since ini t-edge-tabl e started from black's perspective, black tries to 
maximize and white tries to minimize scores.) We then go down the moves, increasing 
the total value by the value of each move times the probability of the move, and 
decreasing the remaining probability by the probability of the move. Since there will 

<a id='page-642'></a>

always be a least one move (pass) with probability 1.0, this is guaranteed to converge. 
In the end we round off the total value, so that we can do the run-time calculations 
with fixnums. 

(defun combine-edge-moves (possibilities player) 
"Combine the best moves." 
(let ((prob 1.0) 

(val 0.0) 
(fn (if (eql player black) #'> #'<))) 

(loop for pair in (sort possibilities fn :key #'second) 
while (>= prob 0.0) 
do (incf val (* prob (first pair) (second pair))) 

(decf prob (* prob (first pair)))) 
(round val))) 

We still need to compute the probability that each possible edge move is legal. These 
probabiUties should reflect things such as the fact that it is easy to capture a corner 
if the opponent is in the adjacent X-square, and very difficult otherwise. First we 
define some functions to recognize corner and X-squares and relate them to their 
neighbors: 

(let ((corner/xsqs '((11 . 22) (18 . 27) (81. 72) (88 . 77)))) 
(defun corner-p (sq) (assoc sq corner/xsqs)) 
(defun x-square-p (sq) (rassoc sq corner/xsqs)) 
(defun x-square-for (corner) (cdr (assoc corner corner/xsqs))) 
(defun corner-for (xsq) (car (rassoc xsq corner/xsqs)))) 

Now we consider the probabilities. There are four cases. First, since we don't 
know anything about the interior of the board, we assume each player has a 50% 
chance of being able to play in an X-square. Second, if we can show that a move 
is legal (because it flips opponent pieces on the edge) then it has 100% probability. 
Third, for the corner squares, we assign a 90% chance if the opponent occupies the 
X-square, 10% if it is empty, and only .1 % if we occupy it. Otherwise, the probability 
is determined by the two neighboring squares: if a square is next to one or more 
opponents it is more likely we can move there; if it is next to our pieces it is less likely. 
If it is legal for the opponent to move into the square, then the chances are cut in half 
(although we may still be able to move there, since we move first). 

(defun edge-move-probability (player board square) 
"What's the probability that player can move to this square?" 
(cond 

((x-square-p square) .5) ;; X-squares 
((legal-p square player board) 1.0) immediate capture 
((corner-p square) :; move to corner depends on X-square 

<a id='page-643'></a>
(let ((x-sq (x-square-for square))) 

(cond 
((eql (bref board x-sq) empty) .1) 
((eql (bref board x-sq) player) 0.001) 
(t .9)))) 

(t (/ (aref 

'#2A((.l .4 .7) 
(.05 .3 *) 
(.01 * *)) 

(count-edge-neighbors player board square) 
(count-edge-neighbors (opponent player) board square)) 
(if (legal-p square (opponent player) board) 2 1))))) 

(defun count-edge-neighbors (player board square) 
"Count the neighbors of this square occupied by player." 
(count-if #'(lambda (inc) 

(eql (bref board (+ square inc)) player)) 
'(+1 -1))) 

Now we return to the problem of determining the static value of an edge position. 
This is computed by a weighted-squares metric, but the weights depend on the 
stability of each piece. A piece is called stable if it cannot be captured, unstable if 
it is in immediate danger of being captured, and semistable otherwise. A table of 
weights follows for each edge square and stability. Note that corner squares are 
always stable, and X-squares we will call semistable if the adjacent corner is taken, 
and unstable otherwise. 

(defparameter *static-edge-table* 

'#2A(;stab semi un 
( * 0 -2000) X 
( 700 ' *) corner 
(1200 200 -25) C 
(1000 200 75) A 
(1000 200 50) . 
(1000 200 50) . 
(1000 200 75) A 
(1200 200 -25) C 
( 700 ' *) corner 
( * 0 -2000) . X 

)) 

<a id='page-644'></a>

The static evaluation then just sums each piece's value according to this table: 

(defun static-edge-stability (player board) 
"Compute this edge's static stability" 
(loop for sq in top-edge 

for i from 0 

sum (cond 
((eql (bref board sq) empty) 0) 
((eql (bref board sq) player) 

(aref *static-edge-table* i 
(piece-stability board sq))) 
(t (- (aref *static-edge-table* i 
(piece-stability board sq))))))) 

The computation of stability is fairly complex. It centers around finding the two 
"pieces," pi and p2, which lay on either side of the piece in question and which are 
not of the same color as the piece. These "pieces" may be empty, or they may be off 
the board. A piece is unstable if one of the two is empty and the other is the opponent; 
it is semistable if there are opponents on both sides and at least one empty square to 
play on, or if it is surrounded by empty pieces. Finally, if either pi or p2 is nil then 
the piece is stable, since it must be connected by a solid wall of pieces to the corner. 

(let ((stable 0) (semi-stable 1) (unstable 2)) 

(defun piece-stability (board sq) 

(cond 
((corner-p sq) stable) 
((x-square-p sq) 

(if (eql (bref board (corner-for sq)) empty) 
unstable semi-stable)) 

(t (let* ((player (bref board sq)) 
(opp (opponent player)) 
(pi (find player board :test-not #*eql 

istart sq :end 19)) 

(p2 (find player board :test-not #'eql 
:start 11 :end sq 
:from-end t))) 

(cond 
unstable pieces can be captured immediately 
by playing in the empty square 

((or (and (eql pi empty) (eql p2 opp)) 
(and (eql p2 empty) (eql pi opp))) 

unstable) 
;; semi-stable pieces might be captured 
((and (eql pi opp) (eql p2 opp) 

<a id='page-645'></a>
(find empty board :start 11 :end 19)) 
semi-stable) 
((and (eql pi empty) (eql p2 empty)) 
semi-stable) 
Stable pieces can never be captured 
(t stable))))))) 

The edge table can now be built by a call to i ni t-edge-tabl e. After the table is built 
once, it is a good idea to save it so that we won't need to repeat the initialization. We 
could write simple routines to dump the table into a file and read it back in, but it is 
faster and easier to use existing tools that already do this job quite well: comp i 1 e-f i 1 e 
and 1 oad. All we have to do is create and compile a file containing the single line: 

(setf *edge-table* *#.*edge-table*) 

The #. read macro evaluates the following expression at read time. Thus, the 
compiler will see and compile the current edge table. It will be able to store this more 
compactly and 1 oad it back in more quickly than if we printed the contents of the 
vector in decimal (or any other base). 

Combining the Factors 

Now we have a measure of the three factors: current mobility, potential mobility, and 
edge stability. All that remains is to find a good way to combine them into a single 
evaluation metric. The combination function used by Rosenbloom (1982) is a linear 
combination of the three factors, but each factor's coefficient is dependent on the 
move number. Rosenbloom's features are normalized to the range [-1000,1000]; we 
normalize to the range [-1,1] by doing a division after multiplying by the coefficient. 
That allows us to use fixnuums for the coefficients. Since our three factors are 
not calculated in quite the same way as Rosenbloom's, it is not surprising that his 
coefficients are not the best for our program. The edge coefficient was doubled and 
the potential coefficient cut by a factor of five. 

(defun lago-eval (player board) 
"Combine edge-stability, current mobility and 
potential mobility to arrive at an evaluation." 

The three factors are multiplied by coefficients 
that vary by move number: 
(let ((c-edg(+ 312000 (* 6240 *move-number*))) 

(c-cur (if (< *move-number* 25) 
(+ 50000 (* 2000 *move-number*)) 
(+ 75000 (* 1000 *move-number*)))) 

(c-pot 20000)) 

<a id='page-646'></a>

(multiple-value-bind (p-cur p-pot) 
(mobility player board) 
(multiple-value-bind (o-cur o-pot) 

(mobility (opponent player) board) 
;; Combine the three factors into one sum: 
(+ (round (* c-edg (edge-stability player board)) 32000) 

(round (* c-cur (- p-cur o-cur)) (+ p-cur o-cur 2)) 
(round (* c-pot (- p-pot o-pot)) (+ p-pot o-pot 2))))))) 

Finally, we are ready to code the lago function. Given a search depth, lago returns a 
strategy that will do alpha-beta search to that depth using the lago-eval evaluation 
function. This version of lago was able to defeat the modified weighted-squares 
strategy in 8 of 10 games at 3 ply, and 9 of 10 at 4 ply. On an Explorer II, 4-ply search 
takes about 20 seconds per move. At 5 ply, many moves take over a minute, so the 
program runs the risk of forfeiting. At 3 ply, the program takes only a few seconds 
per move, but it still was able to defeat the author in five straight games, by scores 
of 50-14, 64-0, 51-13, 49-15 and 36-28. Despite these successes, it is likely that the 
evaluation function could be improved greatly with a little tuning of the parameters. 

(defun lago (depth) 
"Use an approximation of lago's evaluation function." 
(alpha-beta-searcher3 depth #'iago-eval)) 

18.13 Other Techniques 
There are many other variations that can be tried to speed up the search and improve 
play. Unfortunately, choosing among the techniques is a bit of a black art. You will 
have to experiment to find the combination that is best for each domain and each 
evaluation function. Most of the following techniques were incorporated, or at least 
considered and rejected, in Bill. 

Iterative Deepening 

We have seen that the average branching factor for Othello is about 10. This means 
that searching to depth . -f 1 takes roughly 10 times longer than search to depth 

n. Thus, we should be willing to go to a lot of overhead before we search one level 
deeper, to assure two things: that search will be done efficiently, and that we won't 
forfeit due to running out of time. A by-now familiar technique, iterative deepening 
(see chapters 6 and 14), serves both these goals. 
<a id='page-647'></a>
Iterative deepening is used as follov/s. The strategy determines how much of the 
remaining time to allocate to each move. A simple strategy could allocate a constant 
amount of time for each move, and a more sophisticated strategy could allocate more 
time for moves at crucial points in the game. Once the time allocation is determined 
for a move, the strategy starts an iterative deepening alpha-beta search. There are 
two complications: First, the search at . ply keeps track of the best moves, so that 
the search at . -h 1 ply will have better ordering information. In many cases it will be 
faster to do both the . and n + 1 ply searches with the ordering information than to 
do only the . -i-1 ply search without it. Second, we can monitor how much time has 
been taken searching each ply, and cut off the search when searching one more ply 
would exceed the allocated time limit. Thus, iterative-deepening search degrades 
gracefully as time limits are imposed. It will give a reasonable answer even with a 
short time allotment, and it will rarely exceed the allotted time. 

Forward Pruning 

One way to cut the number of positions searched is to replace the legal move generator 
with a plausible move generator: in other words, only consider good moves, and never 
even look at moves that seem clearly bad. This technique is called forward pruning. 
It has fallen on disfavor because of the difficulty in determining which moves are 
plausible. For most games, the factors that would go into a plausible move generator 
would be duplicated in the static evaluation function anyway, so forward pruning 
would require more effort without much gain. Worse, forward pruning could rule 
out a brilliant sacrifice - a move that looks bad initially but eventually leads to a gain. 

For some games, forward pruning is a necessity. The game of Go, for example, is 
played on a 19 by 19 board, so the first player has 361 legal moves, and a 6-ply search 
would involve over 2 quadrillion positions. However, many good Go programs can 
be viewed as not doing forward pruning but doing abstraction. There might be 30 
empty squares in one portion of the board, and the program would treat a move to 
any of these squares equivalently. 

Bill uses forward pruning in a limited way to rule out certain moves adjacent to 
the corners. It does this not to save time but because the evaluation function might 
lead to such a move being selected, even though it is in fact a poor move. In other 
words, forward pruning is used to correct a bug in the evaluation function cheaply. 

Nonspeculative Forward Pruning 

This technique makes use of the observation that there are limits in the amount the 
evaluation function can change from one position to the next. For example, if we 
are using the count difference as the evaluation function, then the most a move can 
change the evaluation is +37 (one for placing a piece in the corner, and six captures 
in each of the three directions). The smallest change is 0 (if the player is forced to 

<a id='page-648'></a>

pass). Thus, if there are 2 ply left in the search, and the backed-up value of position 
A has been established as 38 points better than the static value of position B, then it 
is useless to expand position B. This assumes that we are evaluating every position, 
perhaps to do sorted ordering or iterative deepening. It also assumes that no position 
in the search tree is a final position, because then the evaluation could change by 
more than 37 points. In conclusion, it seems that nonspeculative forward pruning is 
not very useful for Othello, although it may play a role in other games. 

Aspiration Search 

Alpha-beta search is initated with the achievable and cutoff boundaries set to 
. os i ng-val ue and wi nni ng-val ue, respectively. In other words, the search assumes 
nothing: the final position may be anything from a loss to a win. But suppose we are 
in a situation somewhere in the mid-game where we are winning by a small margin 
(say the static evaluation for the current position is 50). In most cases, a single move 
will not change the evaluation by very much. Therefore, if we invoked the alpha-
beta search with a window defined by boundaries of, say, 0 and 100, two things can 
happen: if the actual backed-up evaluation for this position is in fact in the range 0 
to 100, then the search will find it, and it will be found quickly, because the reduced 
window will cause more pruning. If the actual value is not in the range, then the 
value returned will reflect that, and we can search again using a larger window. This 
is called aspiration search, because we aspire to find a value within a given window. 
If the window is chosen well, then often we will succeed and will have saved some 
search time. 

Pearl (1984) suggests an alternative called zero-window search. At each level, the 
first possible move, which we'll call m, is searched using a reasonably wide window 
to determine its exact value, which we'll call v. Then the remaining possible moves 
are searched using . as both the lower and upper bounds of the window. Thus, the 
result of the search will tell if each subsequent move is better or worse than m, but 
won't tell how much better or worse. There are three outcomes for zero-window 
search. If no move turns out to be better than m, then stick with m. If a single move is 
better, then use it. If several moves are better than m, then they have to be searched 
again using a wider window to determine which is best. 

There is always a trade-off between time spent searching and information gained. 
Zero-window search makes an attractive trade-off: we gain some search time by 
losing information about the value of the best move. We are still guaranteed of 
finding the best move, we just don't know its exact value. 

Bill's zero-window search takes only 63% of the time taken by full alpha-beta 
search. It is effective because Bill's move-ordering techniques ensure that the first 
move is often best. With random move ordering, zero-window search would not be 
effective. 

<a id='page-649'></a>
Think-Ahead 

A program that makes its move and then waits for the opponent's reply is wasting 
half the time available to it. A better use of time is to compute, or think-ahead while 
the opponent is moving. Think-ahead is one factor that helps Bill defeat lago. While 
many programs have done think-ahead by choosing the most likely move by the 
opponent and then starting an iterative-deepening search assuming that move. Bill's 
algorithm is somewhat more complex. It can consider more than one move by the 
opponent, depending on how much time is available. 

Hashing and Opening Book Moves 

We have been treating the search space as a tree, but in general it is a directed acyclic 
graph (dag): there may be more than one way to reach a particular position, but there 
won't be any loops, because every move adds a new piece. This raises the question 
we explored briefly in section 6.4: should we treat the search space as a tree or a 
graph? By treating it as a graph we eliminate duplicate evaluations, but we have the 
overhead of storing all the previous positions, and of checking to see if a new position 
has been seen before. The decision must be based on the proportion of duplicate 
positions that are actually encountered in play. One compromise solution is to store 
in a hash table a partial encoding of each position, encoded as, say, a single fixnum 
(one word) instead of the seven or so words needed to represent a full board. Along 
with the encoding of each position, store the move to try first. Then, for each new 
position, look in the hash table, and if there is a hit, try the corresponding move first. 
The move may not even be legal, if there is an accidental hash collision, but there is 
a good chance that the move will be the right one, and the overhead is low. 

One place where it is clearly worthwhile to store information about previous 
positions is in the opening game. Since there are fewer choices in the opening, it is a 
good idea to compile an opening "book" of moves and to play by it as long as possible, 
until the opponent makes a move that departs from the book. Book moves can be 
gleaned from the literature, although not very much has been written about Othello 
(as compared to openings in chess). However, there is a danger in following expert 
advice: the positions that an expert thinks are advantageous may not be the same as 
the positions from which our program can play well. It may be better to compile the 
book by playing the program against itself and determining which positions work 
out best. 

The End Game 

It is also a good idea to try to save up time in the midgame and then make an all-out 
effort to search the complete game tree to completion as soon as feasible. Bill can 
search to completion from about 14 ply out. Once the search is done, of course, the 

<a id='page-650'></a>

most promising lines of play should be saved so that it won't be necessary to solve 
the game tree again. 

Metareasontng 

If it weren't for the clock, Othello would be a trivial game: just search the complete 
game tree all the way to the end, and then choose the best move. The clock imposes 
a complication: we have to make all our moves before we run out of time. The 
algorithms we have seen so far manage the clock by allocating a certain amount of 
time to each move, such that the total time is guaranteed (or at least very likely) to 
be less than the allotted time. This is a very crude policy. A finer-grained way of 
managing time is to consider computation itself as a possible move. That is, at every 
tick of the clock, we need to decide if it is better to stop and play the best move we 
have computed so far or to continue and try to compute a better move. It will be 
better to compute more only in the case where we eventually choose a better move; 
it will be better to stop and play only in the case where we would otherwise forfeit 
due to time constraints, or be forced to make poor choices later in the game. An 
algorithm that includes computation as a possible move is called a metareasoning 
system, because it reasons about how much to reason. 

Russell and Wefald (1989) present an approach based on this view. In addition to 
an evaluation function, they assume a variance function, which gives an estimate of 
how much a given position's true value is likely to vary from its static value. At each 
step, their algorithm compares the value and variance of the best move computed so 
far and the second best move. If the best move is clearly better than the second best 
(taking variance into account), then there is no point computing any more. Also, if the 
top two moves have similar values but both have very low variance, then computing 
will not help much; we can just choose one of the two at random. 

For example, if the board is in a symmetric position, then there may be two 
symmetric moves that will have identical value. By searching each move's subtree 
more carefully, we soon arrive at a low variance for both moves, and then we can 
choose either one, without searching further. Of course, we could also add special-
case code to check for symmetry, but the metareasoning approach will work for 
nonsymmetric cases as well as symmetric ones. If there is a situation where two 
moves both lead to a clear win, it won't waste time choosing between them. 

The only situation where it makes sense to continue computing is when there 
are two moves with high variance, so that it is uncertain if the true value of one 
exceeds the other. The metareasoning algorithm is predicated on devoting time to 
just this case. 

<a id='page-651'></a>
Learning 

From the earhest days of computer game playing, it was realized that a championship 
program would need to learn to improve itself. Samuel (1959) describes a program 
that plays checkers and learns to improve its evaluation function. The evaluation 
function is a linear combination of features, such as the number of pieces for each 
player, the number of kings, the number of possible forks, and so on. Learning is 
done by a hill-climbing search procedure: change one of the coefficients for one of 
the features at random, and then see if the changed evaluation function is better than 
the original one. 

Without some guidance, this hill-climbing search would be very slow. First, the 
space is very large - Samuel used 38 different features, and although he restricted 
the coefficients to be a power of two between 0 and 20, that still leaves 21^^ possible 
evaluation functions. Second, the obvious way of determining the relative worth of 
two evaluation functions - playing a series of games between them and seeing which 
wins more of ten - is quite time-consuming. 

Fortunately, there is a faster way of evaluating an evaluation function. We can 
apply the evaluation function to a position and compare this static value with the 
backed-up value determined by an alpha-beta search. If the evaluation function is 
accurate, the static value should correlate well with the backed-up value. If it does not 
correlate well, the evaluation function should be changed in such a way that it does. 
This approach still requires the trial-and-error of hill-climbing, but it will converge 
much faster if we can gain information from every position, rather than just from 
every game. 

In the past few years there has been increased interest in learning by a process 
of guided search. Neural nets are one example of this. They have been discussed 
elsewhere. Another example is genetic learning algorithms. These algorithms start 
with several candidate solutions. In our case, each candidate would consist of a set 
of coefficients for an evaluation function. On each generation, the genetic algorithm 
sees how well each candidate does. The worst candidates are eliminated, and the 
best ones "mate" and "reproduce" - two candidates are combined in some way to 
yield a new one. If the new offspring has inherited both its parents' good points, then 
it will prosper; if it has inherited both its parents' bad points, then it will quickly die 
out. Either way, the idea is that natural selection will eventually yield a high-quality 
solution. To increase the chances of this, it is a good idea to allow for mutations: 
random changes in the genetic makeup of one of the candidates. 

18.14 History and References 
Lee and Mahajan (1986,1990) present the current top Othello program. Bill. Their 
description outlines all the techniques used but does not go into enough detail to allow 

<a id='page-652'></a>

the reader to reconstruct the program. Bill is based in large part on Rosenbloom's 
lago program. Rosenbloom's article (1982) is more thorough. The presentation in 
this chapter is based largely on this article, although it also contains some ideas from 
Bill and from other sources. 

The journal Othello Quarterly is the definitive source for reports on both human 
and computer Othello games and strategies. 

The most popular game for computer implementation is chess. Shannon (1950a,b) 
speculated that a computer might play chess. In a way, this was one of the boldest 
steps in the history of AI. Today, writing a chess program is a challenging but feasible 
project for an undergraduate. But in 1950, even suggesting that such a program 
might be possible was a revolutionary step that changed the way people viewed 
these arithmetic calculating devices. Shannon introduced the ideas of a game tree 
search, minimaxing, and evaluation functions - ideas that remain intact to this day. 
Marsland (1990) provides a good short introduction to computer chess, and David 
Levy has two books on the subject (1976,1988). It was Levy, an international chess 
master, who in 1968 accepted a bet from John McCarthy, Donald Michie, and others 
that a computer chess program would not beat him in the next ten years. Levy won 
the bet. Levy's Heuristic Programming (1990) and Computer Games (1988) cover a variety 
of computer game playing programs. The studies by DeGroot (1965,1966) give a 
fascinating insight into the psychology of chess masters. 

Knuth and Moore (1975) analyze the alpha-beta algorithm, and Pearl's book 
Heuristics (1984) covers all kinds of heuristic search, games included. 

Samuel (1959) is the classic work on learning evaluation function parameters. It 
is based on the game of checkers. Lee and Mahajan (1990) present an alternative 
learning mechanism, using Bayesian classification to learn an evaluation function 
that optimally distinguishes winning positions from losing positions. Genetic algorithms 
are discussed by L. Davis (1987,1991) and Goldberg (1989). 

18-15 Exercises 

&#9635; Exercise 18.3 [s] How many different Othello positions are there? Would it be 
feasible to store the complete game tree and thus have a perfect player? 

&#9635; Exercise 18.4 [m] At the beginning of this chapter, we implemented pieces as an 
enumerated type. There is no built-in facility in Common Lisp for doing this, so 
we had to introduce a series of defconstant forms. Define a macro for defining 
enumerated types. What else should be provided besides the constants? 

&#9635; Exercise 18.5 [h] Add fixnum and speed declarations to the lago evaluation func


<a id='page-653'></a>
tion and the alpha-beta code. How much does this speed up lago? What other 
efficiency measures can you take? 

&#9635; Exercise 18.6 [h] Implement an iterative deepening search that allocates time for 
each move and checks between each iteration if the time is exceeded. 

&#9635; Exercise 18.7 [h] Implement zero-window search, as described in section 18.13. 

&#9635; Exercise 18.8 [d] Read the references on Bill (Lee and Mahajan 1990, and 1986 if 
you can get it), and reimplement Bill's evaluation function as best you can, using the 
table-based approach. It will also be helpful to read Rosenbloom 1982. 

&#9635; Exercise 18.9 [d] Improve the evaluation function by tuning the parameters, using 
one of the techniques described in section 18.13. 

&#9635; Exercise 18.10 [h] Write move-generation and evaluation functions for another 
game, such as chess or checkers. 

18.16 Answers 
Answer 18.2 The wei ghted-squa res strategy wins the first game by 20 pieces, 
but when count-di ff erence plays first, it captures all the pieces on its fifth move. 
These two games alone are not enough to determine the best strategy; the function 
othel 1 o-seri es on [page 626](chapter18.md#page-626) shows a better comparison. 

Answer 18.3 3^ = 3,433,683,820,292,512,484,657,849,089,281. No. 

<a id='page-654'></a>

Answer 18.4 Besides the constants, we provide a def type for the type itself, and 
conversion routines between integers and symbols: 

(defmacro define-enumerated-type (type &rest elements) 
"Represent an enumerated type with integers 0-n. " 
'(progn 

(deftype .type () '(integer 0 ,( - (length elements) 1))) 
(defun .(symbol type *->symbol) (.type) 
(elt '.elements .type)) 
(defun .(symbol 'symbol-> type) (symbol) 
(position symbol '.elements)) 

.(loop for element in elements 
for i from 0 
collect '(defconstant .element .i)))) 

Here's how the macro would be used to define the piece data type, and the code 
produced: 

> (macroexpand 
'(define-enumerated-type piece 
empty black white outer)) 

(PROGN 
(DEFTYPE PIECE () '(INTEGER 0 3)) 
(DEFUN PIECE->SYMBOL (PIECE) 

(ELT '(EMPTY BLACK WHITE OUTER) PIECE)) 
(DEFUN SYMBOL->PIECE (SYMBOL) 

(POSITION SYMBOL '(EMPTY BLACK WHITE OUTER))) 
(DEFCONSTANT EMPTY 0) 
(DEFCONSTANT BLACK 1) 
(DEFCONSTANT WHITE 2) 
(DEFCONSTANT OUTER 3)) 

A more general facility would, like defstruct, provide for several options. For 
example, it might allow for a documentation string for the type and each constant, 
and for a : cone-name, so the constants could have names like pi ece-empty instead 
of empty. This would avoid conflicts with other types that wanted to use the same 
names. The user might also want the ability to start the values at some number other 
than zero, or to assign specific values to some of the symbols. 

