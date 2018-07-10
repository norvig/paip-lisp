# Chapter 18
## Search and the Game of Othello

> In the beginner’s mind there are endless possibilities; in the expert’s there are few.

> –Suzuki Roshi, Zen Master

**G**ame playing has been the target of much early work in AI for three reasons.
First, the rules of most games are formalized, and they can be implemented in a computer program rather easily.
Second, in many games the interface requirements are trivial.
The computer need only print out its moves and read in the opponent’s moves.
This is true for games like chess and checkers, but not for ping-pong and basketball, where vision and motor skills are crucial.
Third, playing a good game of chess is considered by many an intellectual achievement.
Newell, Shaw, and Simon say, “Chess is the intellectual game *par excellence*,” and Donald Michie called chess the “*Drosophila melanogaster* of machine intelligence,” meaning that chess is a relatively simple yet interesting domain that can lead to advances in AI, just as study of the fruit fly served to advance biology.

Today there is less emphasis on game playing in AI.
It has been realized that techniques that work well in the limited domain of a board game do not necessarily lead to intelligent behavior in other domains.
Also, as it turns out, the techniques that allow computers to play well are not the same as the techniques that good human players use.
Humans are capable of recognizing abstract patterns learned from previous games, and formulating plans of attack and defense.
While some computer programs try to emulate this approach, the more succesful programs work by rapidly searching thousands of possible sequences of moves, making fairly superficial evaluations of the worth of each sequence.

While much previous work on game playing has concentrated on chess and checkers, this chapter demonstrates a program to play the game of Othello.[1](#fn0015){:#xfn0015} Othello is a variation on the nineteenth-century game Reversi.
It is an easy game to program because the rules are simpler than chess.
Othello is also a rewarding game to program, because a simple search technique can yield an excellent player.
There are two reasons for this.
First, the number of legal moves per turn is low, so the search is not too explosive.
Second, a single Othello move can flip a dozen or more opponent pieces.
This makes it difficult for human players to visualize the long-range consequences of a move.
Search-based programs are not confused, and thus do well relative to humans.

The very name “Othello” derives from the fact that the game is so unpredictable, like the Moor of Venice.
The name may also be an allusion to the line, “Your daughter and the Moor are now making the beast with two backs,”[2](#fn0020){:#xfn0020} since the game pieces do indeed have two backs, one white and one black.
In any case, the association between the game and the play carries over to the name of several programs: Cassio, Iago, and Bill.
The last two will be discussed in this chapter.
They are equal to or better than even champion human players.
We will be able to develop a simplified version that is not quite a champion but is much better than beginning players.

## [ ](#){:#st0010}18.1 The Rules of the Game
{:#s0010}
{:.h1hd}

Othello is played on a 8-by-8 board, which is initially set up with four pieces in the center, as shown in [figure 18.1](#f0010).
The two players, black and white, alternate turns, with black playing first.
On each turn, a player places a single piece of his own color on the board.
No piece can be moved once it is placed, but subsequent moves may flip a piece from one color to another.
Each piece must be placed so that it *brackets* one or more opponent pieces.
That is, when black plays a piece there must be a line (horizontal, vertical, or diagonal) that goes through the piece just played, then through one or more white pieces, and then to another black piece.
The intervening white pieces are flipped over to black.
If there are bracketed white pieces in more than one direction, they are all flipped.
[Figure 18.2 (a)](#f0015) indicates the legal moves for black with small dots.
[Figure 18.2 (b)](#f0015) shows the position after black moves to square b4.
Players alternate turns, except that a player who has no legal moves must pass.
When neither player has any moves, the game is over, and the player with the most pieces on the board wins.
This usually happens because there are no empty squares left, but it occasionally happens earlier in the game.

![f18-01-9780080571157](images/B9780080571157500182/f18-01-9780080571157.jpg)     
Figure 18.1
!!!(span) {:.fignum}
The Othello Board
![f18-02-9780080571157](images/B9780080571157500182/f18-02-9780080571157.jpg)     
Figure 18.2
!!!(span) {:.fignum}
Legal Othello Moves

## [ ](#){:#st0015}18.2 Representation Choices
{:#s0015}
{:.h1hd}

In developing an Othello program, we will want to test out various strategies, playing those strategies against each other and against human players.
We may also want our program to allow two humans to play a game.
Therefore, our main function, `othello`, will be a monitoring function that takes as arguments two strategies.
It uses these strategies to get each player’s moves, and then applies these moves to a representation of the game board, perhaps printing out the board as it goes.

The first choice to make is how to represent the board and the pieces on it.
The board is an 8-by-8 square, and each square can be filled by a black or white piece or can be empty.
Thus, an obvious representation choice is to make the board an 8-by-8 array, where each element of the array is the symbol `black, white,` or `nil`.

Notice what is happening here: we are following the usual Lisp convention of implementing an *enumerated type* (the type of pieces that can fill a square) as a set of symbols.
This is an appropriate representation because it supports the primary operation on elements of an enumerated type: test for equality using eq.
It also supports input and output quite handily.

In many other languages (such as C or Pascal), enumerated types are implemented as integers.
In Pascal one could declare:

[ ](#){:#l9010}`type piece = (black, white, empty);`
!!!(p) {:.unnumlist}

to define `piece` as a set of three elements that is treated as a subtype of the integers.
The language does not allow for direct input and output of such types, but equality can be checked.
An advantage of this approach is that an element can be packed into a small space.
In the Othello domain, we anticipate that efficiency will be important, because one way to pick a good move is to look at a large number of possible sequences of moves, and choose a sequence that leads toward a favorable result.
Thus, we are willing to look hard at alternative representations to find an efficient one.
It takes only two bits to represent one of the three possible types, while it takes many more (perhaps 32) to represent a symbol.
Thus, we may save space by representing pieces as small integers rather than symbols.

Next, we consider the board.
The two-dimensional array seems like such an obvious choice that it is hard to imagine a better representation.
We could consider an 8-element list of 8-element lists, but this would just waste space (for the cons cells) and time (in accessing the later elements of the lists).
However, we will have to implement two other abstract data types that we have not yet considered: the square and the direction.
We will need, for example, to represent the square that a player chooses to move into.
This will be a pair of integers, such as 4,5.
We could represent this as a two-element list, or more compactly as a cons cell, but this still means that we may have to generate garbage (create a cons cell) every time we want to refer to a new square.
Similarly, we need to be able to scan in a given direction from a square, looking for pieces to flip.
Directions will be represented as a pair of integers, such as +1,-1.
One clever possibility is to use complex numbers for both squares and directions, with the real component mapped to the horizontal axis and the imaginary component mapped to the vertical axis.
Then moving in a given direction from a square is accomplished by simply adding the direction to the square.
But in most implementations, creating new complex numbers will also generate garbage.

Another possibility is to represent squares (and directions) as two distinct integers, and have the routines that manipulate them accept two arguments instead of one.
This would be efficient, but it is losing an important abstraction: that squares (and directions) are conceptually single objects.

A way out of this dilemma is to represent the board as a one-dimensional vector.
Squares are represented as integers in the range 0 to 63.
In most implementations, small integers (fixnums) are represented as immediate data that can be manipulated without generating garbage.
Directions can also be implemented as integers, representing the numerical difference between adjacent squares along that direction.
To get a feel for this, take a look at the board:

[ ](#){:#l0010}` 0  1  2  3  4  5  6  7`
!!!(p) {:.unnumlist}

` 8  9 10 11 12 13 14 15`
!!!(p) {:.unnumlist}

`16 17 18 19 20 21 22 23`
!!!(p) {:.unnumlist}

`24 25 26 27 28 29 30 31`
!!!(p) {:.unnumlist}

`32 33 34 35 36 37 38 39`
!!!(p) {:.unnumlist}

`40 41 42 43 44 45 46 47`
!!!(p) {:.unnumlist}

`48 49 50 51 52 53 54 55`
!!!(p) {:.unnumlist}

`56 57 58 59 60 61 62 63`
!!!(p) {:.unnumlist}

You can see that the direction +1 corresponds to movement to the right, +7 corresponds to diagonal movement downward and to the left, +8 is downward, and +9 is diagonally downward and to the right.
The negations of these numbers (-1, -7, -8, -9) represent the opposite directions.

There is one complication with this scheme: we need to know when we hit the edge of the board.
Starting at square 0, we can move in direction +1 seven times to arrive at the right edge of the board, but we aren’t allowed to move in that direction yet again to arrive at square 8.
It is possible to check for the edge of the board by considering quotients and remainders modulo 8, but it is somewhat complicated and expensive to do so.

A simpler solution is to represent the edge of the board explicitly, by using a 100-element vector instead of a 64-element vector.
The outlying elements are filled with a marker indicating that they are outside the board proper.
This representation wastes some space but makes edge detection much simpler.
It also has the minor advantage that legal squares are represented by numbers in the range 11-88, which makes them easier to understand while debugging.
Here’s the new 100-element board:

[ ](#){:#l0015}` 0  1  2  3  4  5  6  7  8  9`
!!!(p) {:.unnumlist}

`10 11 12 13 14 15 16 17 18 19`
!!!(p) {:.unnumlist}

`20 21 22 23 24 25 26 27 28 29`
!!!(p) {:.unnumlist}

`30 31 32 33 34 35 36 37 38 39`
!!!(p) {:.unnumlist}

`40 41 42 43 44 45 46 47 48 49`
!!!(p) {:.unnumlist}

`50 51 52 53 54 55 56 57 58 59`
!!!(p) {:.unnumlist}

`60 61 62 63 64 65 66 67 68 69`
!!!(p) {:.unnumlist}

`70 71 72 73 74 75 76 77 78 79`
!!!(p) {:.unnumlist}

`80 81 82 83 84 85 86 87 88 89`
!!!(p) {:.unnumlist}

`90 91 92 93 94 95 96 97 98 99`
!!!(p) {:.unnumlist}

The horizontal direction is now ±1, vertical is ±10, and the diagonals are ±9 and ±11.
We’ll tentatively adopt this latest representation, but leave open the possibility of changing to another format.
With this much decided, we are ready to begin.
[Figure 18.3](#f0020) is the glossary for the complete program.
A glossary for a second version of the program is on [page 623](#p623).

![f18-03-9780080571157](images/B9780080571157500182/f18-03-9780080571157.jpg)     
Figure 18.3
!!!(span) {:.fignum}
Glossary for the Othello Program
What follows is the code for directions and pieces.
We explicitly define the type `piece` to be a number from `empty` to `outer` (0 to 3), and define the function `name-of` to map from a piece number to a character: a dot for empty, `@` for black, 0 for white, and a question mark (which should never be printed) for `outer`.

[ ](#){:#l0020}`(defconstant all-directions ’(-11 -10 -9 -1 1 9 10 11))`
!!!(p) {:.unnumlist}

`(defconstant empty 0 "An empty square")`
!!!(p) {:.unnumlist}

`(defconstant black 1 "A black piece")`
!!!(p) {:.unnumlist}

`(defconstant white 2 "A white piece")`
!!!(p) {:.unnumlist}

`(defconstant outer 3 "Marks squares outside the 8×8 board")`
!!!(p) {:.unnumlist}

`(deftype piece () ‘(integer ,empty ,outer))`
!!!(p) {:.unnumlist}

`(defun name-of (piece) (char ".@0?" piece))`
!!!(p) {:.unnumlist}

`(defun opponent (player) (if (eql player black) white black))`
!!!(p) {:.unnumlist}

And here is the code for the board.
Note that we introduce the function `bref`, for “board reference” rather than using the built-in function `aref`.
This facilitates possible changes to the representation of boards.
Also, even though there is no contiguous range of numbers that represents the legal squares, we can define the constant `all-squares` to be a list of the 64 legal squares, computed as those numbers from 11 to 88 whose value mod 10 is between 1 and 8.

[ ](#){:#l0025}`(deftype board () ’(simple-array piece (100)))`
!!!(p) {:.unnumlist}

`(defun bref (board square) (aref board square))`
!!!(p) {:.unnumlist}

`(defsetf bref (board square) (val)`
!!!(p) {:.unnumlist}

`  ‘(setf (aref ,board ,square) ,val))`
!!!(p) {:.unnumlist}

`(defun copy-board (board)`
!!!(p) {:.unnumlist}

`  (copy-seq board))`
!!!(p) {:.unnumlist}

`(defconstant all-squares`
!!!(p) {:.unnumlist}

`  (loop for i from 11 to 88 when (<= 1 (mod i 10) 8) collect i))`
!!!(p) {:.unnumlist}

`(defun initial-board ()`
!!!(p) {:.unnumlist}

`  "Return a board, empty except for four pieces in the middle."`
!!!(p) {:.unnumlist}

`  ;; Boards are 100-element vectors, with elements 11-88 used,`
!!!(p) {:.unnumlist}

`  ;; and the others marked with the sentinel OUTER.
Initially`
!!!(p) {:.unnumlist}

`  ;; the 4 center squares are taken, the others empty.`
!!!(p) {:.unnumlist}

`  (let ((board (make-array 100 :element-type ’piece`
!!!(p) {:.unnumlist}

`                    :initial-element outer)))`
!!!(p) {:.unnumlist}

`    (dolist (square all-squares)`
!!!(p) {:.unnumlist}

`      (setf (bref board square) empty))`
!!!(p) {:.unnumlist}

`    (setf (bref board 44) white (bref board 45) black`
!!!(p) {:.unnumlist}

`         (bref board 54) black (bref board 55) white)`
!!!(p) {:.unnumlist}

`    board))`
!!!(p) {:.unnumlist}

`(defun print-board (board)`
!!!(p) {:.unnumlist}

`  "Print a board, along with some statistics."`
!!!(p) {:.unnumlist}

`  (format t "~2&   1 2 3 4 5 6 7 8  [~c =~2a ~c =~2a (~@d)]"`
!!!(p) {:.unnumlist}

`         (name-of black) (count black board)`
!!!(p) {:.unnumlist}

`         (name-of white) (count white board)`
!!!(p) {:.unnumlist}

`         (count-difference black board))`
!!!(p) {:.unnumlist}

`  (loop for row from 1 to 8 do`
!!!(p) {:.unnumlist}

`       (format t "~&~d" (* 10 row))`
!!!(p) {:.unnumlist}

`       (loop for col from 1 to 8`
!!!(p) {:.unnumlist}

`           for piece = (bref board (+ col (* 10 row)))`
!!!(p) {:.unnumlist}

`           do (format t "~c" (name-of piece))))`
!!!(p) {:.unnumlist}

`  (format t "~2&"))`
!!!(p) {:.unnumlist}

`(defun count-difference (player board)`
!!!(p) {:.unnumlist}

`  "Count player’s pieces minus opponent’s pieces."`
!!!(p) {:.unnumlist}

`  (- (count player board)`
!!!(p) {:.unnumlist}

`   (count (opponent player) board)))`
!!!(p) {:.unnumlist}

Now let’s take a look at the initial board, as it is printed by `print-board`, and by a raw `write` (I added the line breaks to make it easier to read):

[ ](#){:#l0030}`> (write (initial-board)  > (print-board (initial-board))`
!!!(p) {:.unnumlist}

`     :array t)`
!!!(p) {:.unnumlist}

`#(3 3 3 3 3 3 3 3 3 3         1 2 3 4 5 6 7 8[@=2 0=2 (+0)]`
!!!(p) {:.unnumlist}

`  3 0 0 0 0 0 0 0 0 3      10 .
.
.
.
.
.
.
.`
!!!(p) {:.unnumlist}

`  3 0 0 0 0 0 0 0 0 3      20 .
.
.
.
.
.
.
.`
!!!(p) {:.unnumlist}

`  3 0 0 0 0 0 0 0 0 3      30 .
.
.
.
.
.
.
.`
!!!(p) {:.unnumlist}

`  3 0 0 0 2 1 0 0 0 3      40 .
.
.
0 @ .
.
.`
!!!(p) {:.unnumlist}

`  3 0 0 0 1 2 0 0 0 3      50 .
.
.
@ 0 .
.
.`
!!!(p) {:.unnumlist}

`  3 0 0 0 0 0 0 0 0 3      60 .
.
.
.
.
.
.
.`
!!!(p) {:.unnumlist}

`  3 0 0 0 0 0 0 0 0 3      70 .
.
.
.
.
.
.
.`
!!!(p) {:.unnumlist}

`  3 0 0 0 0 0 0 0 0 3      80 .
.
.
.
.
.
.
.`
!!!(p) {:.unnumlist}

`  3 3 3 3 3 3 3 3 3 3)`
!!!(p) {:.unnumlist}

`#<ART-2B-100 -72570734>  NIL`
!!!(p) {:.unnumlist}

Notice that `print-board` provides some additional information: the number of pieces that each player controls, and the difference between these two counts.

The next step is to handle moves properly: given a board and a square to move to, update the board to reflect the effects of the player moving to that square.
This means flipping some of the opponent’s pieces.
One design decision is whether the procedure that makes moves, `make-move`, will be responsible for checking for error conditions.
My choice is that `make-move` assumes it will be passed a legal move.
That way, a strategy can use the function to explore sequences of moves that are known to be valid without slowing `make-move` down.
Of course, separate procedures will have to insure that a move is legal.
Here we introduce two terms: a *valid* move is one that is syntactically correct: an integer from 11 to 88 that is not off the board.
A *legal* move is a valid move into an empty square that will flip at least one opponent.
Here’s the code:

[ ](#){:#l0035}`(defun valid-p (move)`
!!!(p) {:.unnumlist}

`  "Valid moves are numbers in the range 11-88 that end in 1-8."`
!!!(p) {:.unnumlist}

`  (and (integerp move) (<= 11 move 88) (<= 1 (mod move 10) 8)))`
!!!(p) {:.unnumlist}

`(defun legal-p (move player board)`
!!!(p) {:.unnumlist}

`  "A Legal move must be into an empty square, and it must`
!!!(p) {:.unnumlist}

`  flip at least one opponent piece."`
!!!(p) {:.unnumlist}

`  (and (eql (bref board move) empty)`
!!!(p) {:.unnumlist}

`     (some #’(lambda (dir) (would-flip?
move player board dir))`
!!!(p) {:.unnumlist}

`           all-directions)))`
!!!(p) {:.unnumlist}

`(defun make-move (move player board)`
!!!(p) {:.unnumlist}

`  "Update board to reflect move by player"`
!!!(p) {:.unnumlist}

`  ;; First make the move, then make any flips`
!!!(p) {:.unnumlist}

`  (setf (bref board move) player)`
!!!(p) {:.unnumlist}

`  (dolist (dir all-directions)`
!!!(p) {:.unnumlist}

`    (make-flips move player board dir))`
!!!(p) {:.unnumlist}

`  board)`
!!!(p) {:.unnumlist}

Now all we need is to `make-flips`.
To do that, we search in all directions for a *bracketing* piece: a piece belonging to the player who is making the move, which sandwiches a string of opponent pieces.
If there are no opponent pieces in that direction, or if an empty or outer piece is hit before the player’s piece, then no flips are made.
Note that `would-flip?` is a semipredicate that returns false if no flips would be made in the given direction, and returns the square of the bracketing piece if there is one.

[ ](#){:#l0040}`(defun make-flips (move player board dir)`
!!!(p) {:.unnumlist}

`  "Make any flips in the given direction."`
!!!(p) {:.unnumlist}

`  (let ((bracketer (would-flip?
move player board dir)))`
!!!(p) {:.unnumlist}

`    (when bracketer`
!!!(p) {:.unnumlist}

`      (loop for c from (+ move dir) by dir until (eql c bracketer)`
!!!(p) {:.unnumlist}

`          do (setf (bref board c) player)))))`
!!!(p) {:.unnumlist}

`(defun would-flip?
(move player board dir)`
!!!(p) {:.unnumlist}

`  "Would this move result in any flips in this direction?`
!!!(p) {:.unnumlist}

`  If so, return the square number of the bracketing piece."`
!!!(p) {:.unnumlist}

`  ;; A flip occurs if, starting at the adjacent square, c, there`
!!!(p) {:.unnumlist}

`  ;; is a string of at least one opponent pieces, bracketed by`
!!!(p) {:.unnumlist}

`  ;; one of player’s pieces`
!!!(p) {:.unnumlist}

`  (let ((c (+ move dir)))`
!!!(p) {:.unnumlist}

`    (and (eql (bref board c) (opponent player))`
!!!(p) {:.unnumlist}

`        (find-bracketing-piece (+ c dir) player board dir))))`
!!!(p) {:.unnumlist}

`(defun find-bracketing-piece (square player board dir)`
!!!(p) {:.unnumlist}

`  "Return the square number of the bracketing piece."`
!!!(p) {:.unnumlist}

`  (cond ((eql (bref board square) player) square)`
!!!(p) {:.unnumlist}

`       ((eql (bref board square) (opponent player))`
!!!(p) {:.unnumlist}

`        (find-bracketing-piece (+ square dir) player board dir))`
!!!(p) {:.unnumlist}

`       (t nil)))`
!!!(p) {:.unnumlist}

Finally we can write the function that actually monitors a game.
But first we are faced with one more important choice: how will we represent a player?
We have already distinguished between black and white’s pieces, but we have not decided how to ask black or white for their moves.
I choose to represent player’s strategies as functions.
Each function takes two arguments: the color to move (black or white) and the current board.
The function should return a legal move number.

[ ](#){:#l0045}`(defun othello (bl-strategy wh-strategy &optional (print t))`
!!!(p) {:.unnumlist}

`  "Play a game of Othello.
Return the score, where a positive`
!!!(p) {:.unnumlist}

`  difference means black (the first player) wins."`
!!!(p) {:.unnumlist}

`  (let ((board (initial-board)))`
!!!(p) {:.unnumlist}

`    (loop for player = black`
!!!(p) {:.unnumlist}

`          then (next-to-play board player print)`
!!!(p) {:.unnumlist}

`        for strategy = (if (eql player black)`
!!!(p) {:.unnumlist}

`             bl-strategy`
!!!(p) {:.unnumlist}

`             wh-strategy)`
!!!(p) {:.unnumlist}

`      until (null player)`
!!!(p) {:.unnumlist}

`      do (get-move strategy player board print))`
!!!(p) {:.unnumlist}

`  (when print`
!!!(p) {:.unnumlist}

`    (format t "~&The game is over.
Final result:")`
!!!(p) {:.unnumlist}

`    (print-board board))`
!!!(p) {:.unnumlist}

`  (count-difference black board)))`
!!!(p) {:.unnumlist}

We need to be able to determine who plays next at any point.
The rules say that players alternate turns, but if one player has no legal moves, the other can move again.
When neither has a legal move, the game is over.
This usually happens because there are no empty squares left, but it sometimes happens earlier in the game.
The player with more pieces at the end of the game wins.
If neither player has more, the game is a draw.

[ ](#){:#l0050}`(defun next-to-play (board previous-player print)`
!!!(p) {:.unnumlist}

`  "Compute the player to move next, or NIL if nobody can move."`
!!!(p) {:.unnumlist}

`  (let ((opp (opponent previous-player)))`
!!!(p) {:.unnumlist}

`    (cond ((any-legal-move?
opp board) opp)`
!!!(p) {:.unnumlist}

`        ((any-legal-move?
previous-player board)`
!!!(p) {:.unnumlist}

`         (when print`
!!!(p) {:.unnumlist}

`           (format t "~&~c has no moves and must pass."`
!!!(p) {:.unnumlist}

`                (name-of opp)))`
!!!(p) {:.unnumlist}

`         previous-player)`
!!!(p) {:.unnumlist}

`        (t nil))))`
!!!(p) {:.unnumlist}

`(defun any-legal-move?
(player board)`
!!!(p) {:.unnumlist}

`  "Does player have any legal moves in this position?"`
!!!(p) {:.unnumlist}

`  (some #’(lambda (move) (legal-p move player board))`
!!!(p) {:.unnumlist}

`      all-squares))`
!!!(p) {:.unnumlist}

Note that the argument `print` (of `othello`, `next-to-play`, and below, `get-move`) determines if information about the progress of the game will be printed.
For an interactive game, `print` should be true, but it is also possible to play a “batch” game with `print` set to false.

In `get-move` below, the player’s strategy function is called to determine his move.
Illegal moves are detected, and proper moves are reported when `print` is true.
The strategy function is passed a number representing the player to move (black or white) and a copy of the board.
If we passed the *real* game board, the function could cheat by changing the pieces on the board!

[ ](#){:#l0055}`(defun get-move (strategy player board print)`
!!!(p) {:.unnumlist}

`  "Call the player’s strategy function to get a move.`
!!!(p) {:.unnumlist}

`  Keep calling until a legal move is made."`
!!!(p) {:.unnumlist}

`  (when print (print-board board))`
!!!(p) {:.unnumlist}

`  (let ((move (funcall strategy player (copy-board board))))`
!!!(p) {:.unnumlist}

`    (cond`
!!!(p) {:.unnumlist}

`      ((and (valid-p move) (legal-p move player board))`
!!!(p) {:.unnumlist}

`       (when print`
!!!(p) {:.unnumlist}

`         (format t "~&~c moves to ~d." (name-of player) move))`
!!!(p) {:.unnumlist}

`       (make-move move player board))`
!!!(p) {:.unnumlist}

`      (t (warn "Illegal move: ~d" move)`
!!!(p) {:.unnumlist}

`        (get-move strategy player board print)))))`
!!!(p) {:.unnumlist}

Here we define two simple strategies:

[ ](#){:#l0060}`(defun human (player board)`
!!!(p) {:.unnumlist}

`  "A human player for the game of Othello"`
!!!(p) {:.unnumlist}

`  (declare (ignore board))`
!!!(p) {:.unnumlist}

`  (format t "~&~c to move: " (name-of player))`
!!!(p) {:.unnumlist}

`  (read))`
!!!(p) {:.unnumlist}

`(defun random-strategy (player board)`
!!!(p) {:.unnumlist}

`  "Make any legal move."`
!!!(p) {:.unnumlist}

`  (random-elt (legal-moves player board)))`
!!!(p) {:.unnumlist}

`(defun legal-moves (player board)`
!!!(p) {:.unnumlist}

`  "Returns a list of legal moves for player"`
!!!(p) {:.unnumlist}

`  (loop for move in all-squares`
!!!(p) {:.unnumlist}

`    when (legal-p move player board) collect move))`
!!!(p) {:.unnumlist}

We are now in a position to play the game.
The expression

`(othello #’human #’human)` will let two people play against each other.
Alternately, `(othello #’ random-strategy #’human)` will allow us to match our wits against a particularly poor strategy.
The rest of this chapter shows how to develop a better strategy.

## [ ](#){:#st0020}18.3 Evaluating Positions
{:#s0020}
{:.h1hd}

The random-move strategy is, of course, a poor one.
We would like to make a good move rather than a random move, but so far we don’t know what makes a good move.
The only positions we are able to evaluate for sure are final positions: when the game is over, we know that the player with the most pieces wins.
This suggests a strategy: choose the move that maximizes `count-difference`, the piece differential.
The function `maximize-difference` does just that.
It calls `maximizer`, a higher-order function that chooses the best move according to an arbitrary evaluation function.

[ ](#){:#l0065}`(defun maximize-difference (player board)`
!!!(p) {:.unnumlist}

`  "A strategy that maximizes the difference in pieces."`
!!!(p) {:.unnumlist}

`  (funcall (maximizer #’count-difference) player board))`
!!!(p) {:.unnumlist}

`(defun maximizer (eval-fn)`
!!!(p) {:.unnumlist}

`  "Return a strategy that will consider every legal move,`
!!!(p) {:.unnumlist}

`  apply EVAL-FN to each resulting board, and choose`
!!!(p) {:.unnumlist}

`  the move for which EVAL-FN returns the best score.`
!!!(p) {:.unnumlist}

`  FN takes two arguments: the player-to-move and board"`
!!!(p) {:.unnumlist}

`  #’(lambda (player board)`
!!!(p) {:.unnumlist}

`      (let* ((moves (legal-moves player board))`
!!!(p) {:.unnumlist}

`          (scores (mapcar #’(lambda (move)`
!!!(p) {:.unnumlist}

`                (funcall`
!!!(p) {:.unnumlist}

`                  eval-fn`
!!!(p) {:.unnumlist}

`                  player`
!!!(p) {:.unnumlist}

`                  (make-move move player`
!!!(p) {:.unnumlist}

`                      (copy-board board))))`
!!!(p) {:.unnumlist}

`              moves))`
!!!(p) {:.unnumlist}

`         (best (apply #’max scores)))`
!!!(p) {:.unnumlist}

`      (elt moves (position best scores)))))`
!!!(p) {:.unnumlist}

**Exercise 18.1** Play some games with `maximize-difference` against `random-strategy` and `human`.
How good is `maximize-difference`?

Those who complete the exercise will quickly see that the `maximize-difference` player does better than random, and may even beat human players in their first game or two.
But most humans are able to improve, learning to take advantage of the overly greedy play of `maximize-difference`.
Humans learn that the edge squares, for example, are valuable because the player dominating the edges can surround the opponent, while it is difficult to recapture an edge.
This is especially true of corner squares, which can never be recaptured.

Using this knowledge, a clever player can temporarily sacrifice pieces to obtain edge and corner squares in the short run, and win back pieces in the long run.
We can approximate some of this reasoning with the `weighted-squares` evaluation function.
Like `count-difference`, it adds up all the player’s pieces and subtracts the opponents, but each piece is weighted according to the square it occupies.
Edge squares are weighted highly, corner squares higher still, and squares adjacent to the corners and edges have negative weights, because occupying these squares often gives the opponent a means of capturing the desirable square.
[Figure 18.4](#f0025) shows the standard nomenclature for edge squares: X, A, B, and C.
In general, X and C squares are to be avoided, because taking them gives the opponent a chance to take the corner.
The `weighted-squares` evaluation function reflects this.

![f18-04-9780080571157](images/B9780080571157500182/f18-04-9780080571157.jpg)     
Figure 18.4
!!!(span) {:.fignum}
Names for Edge Squares
[ ](#){:#l0070}`(defparameter *weights*`
!!!(p) {:.unnumlist}

`  '#(0   0   0  0  0  0  0   0   0 0`
!!!(p) {:.unnumlist}

`     0 120 -20 20  5  5 20 -20 120 0`
!!!(p) {:.unnumlist}

`     0 -20 -40 -5 -5 -5 -5 -40 -20 0`
!!!(p) {:.unnumlist}

`     0  20  -5 15  3  3 15  -5  20 0`
!!!(p) {:.unnumlist}

`     0   5  -5  3  3  3  3  -5   5 0`
!!!(p) {:.unnumlist}

`     0   5  -5  3  3  3  3  -5   5 0`
!!!(p) {:.unnumlist}

`     0  20  -5 15  3  3 15  -5  20 0`
!!!(p) {:.unnumlist}

`     0 -20 -40 -5 -5 -5 -5 -40 -20 0`
!!!(p) {:.unnumlist}

`     0 120 -20 20  5  5 20 -20 120 0`
!!!(p) {:.unnumlist}

`     0   0   0  0  0  0  0   0   0 0))`
!!!(p) {:.unnumlist}

`(defun weighted-squares (player board)`
!!!(p) {:.unnumlist}

`  "Sum of the weights of player’s squares minus opponent’s.`
!!!(p) {:.unnumlist}

`  (let ((opp (opponent player)))`
!!!(p) {:.unnumlist}

`    (loop for i in all-squares`
!!!(p) {:.unnumlist}

`        when (eql (bref board i) player)`
!!!(p) {:.unnumlist}

`        sum (aref *weights* i)`
!!!(p) {:.unnumlist}

`        when (eql (bref board i) opp)`
!!!(p) {:.unnumlist}

`        sum (- (aref *weights* i)))))`
!!!(p) {:.unnumlist}

**Exercise 18.2** Compare strategies by evaluating the two forms below.
What happens?
Is this a good test to determine which strategy is better?

[ ](#){:#l0075}`(othello (maximizer #’weighted-squares)`
!!!(p) {:.unnumlist}

`         (maximizer #’count-difference) nil)`
!!!(p) {:.unnumlist}

`(othello (maximizer #’count-difference)`
!!!(p) {:.unnumlist}

`         (maximizer #’weighted-squares) nil)`
!!!(p) {:.unnumlist}

## [ ](#){:#st0025}18.4 Searching Ahead: Minimax
{:#s0025}
{:.h1hd}

Even the weighted-squares strategy is no match for an experienced player.
There are two ways we could improve the strategy.
First, we could modify the evaluation function to take more information into account.
But even without changing the evaluation function, we can improve the strategy by searching ahead.
Instead of choosing the move that leads immediately to the highest score, we can also consider the opponent’s possible replies, our replies to those replies, and so on.
By searching through several levels of moves, we can steer away from potential disaster and find good moves that were not immediately apparent.

Another way to look at the `maximizer` function is as a search function that searches only one level, or *ply*, deep:[ ](#){:#p1225}

![u18-01-9780080571157](images/B9780080571157500182/u18-01-9780080571157.jpg)     

The top of the tree is the current board position, and the squares below that indicate possible moves.
The `maximizer` function evaluates each of these and picks the best move, which is underlined in the diagram.

Now let’s see how a 3-ply search might go.
The first step is to apply `maximizer` to the positions just above the bottom of the tree.
Suppose we get the following values:[ ](#){:#p1240}

![u18-02-9780080571157](images/B9780080571157500182/u18-02-9780080571157.jpg)     

Each position is shown as having two possible legal moves, which is unrealistic but makes the diagram fit on the page.
In a real game, five to ten legal moves per position is typical.
The values at the leaves of the tree were computed by applying the evaluation function, while the values one level up were computed by `maximizer`.
The result is that we know what our best move is for any of the four positions just above the bottom of the tree.

Going up a level, it is the opponent’s turn to move.
We can assume the opponent will choose the move that results in the minimal value to us, which would be the maximal value to the opponent.
Thus, the opponent’s choices would be the 10- and 9-valued positions, avoiding the 20- and 23-valued positions.[ ](#){:#p1255}

![u18-03-9780080571157](images/B9780080571157500182/u18-03-9780080571157.jpg)     

Now it is our turn to move again, so we apply `maximizer` once again to get the final value of the top-level position:[ ](#){:#p1265}

![u18-04-9780080571157](images/B9780080571157500182/u18-04-9780080571157.jpg)     

If the opponent plays as expected, we will always follow the left branch of the tree and end up at the position with value 10.
If the opponent plays otherwise, we will end up at a position with a better value.

This kind of search is traditionally called a *minimax* search, because of the alternate application of the `maximizer` and a hypothetical `minimizer` function.
Notice that only the leaf positions in the tree are looked at by the evaluation function.
The value of all other positions is determined by minimizing and maximizing.

We are almost ready to code the minimax algorithm, but first we have to make a few design decisions.
First, we could write two functions, `minimax` and `maximin`, which correspond to the two players’ analyses.
However, it is easier to write a single function that maximizes the value of a position for a particular player.
In other words, by adding the player as a parameter, we avoid having to write two otherwise identical functions.

Second, we have to decide if we are going to write a general minimax searcher or an Othello-specific searcher.
I decided on the latter for efficiency reasons, and because there are some Othello-specific complications that need to be accounted for.
First, it is possible that a player will not have any legal moves.
In that case, we want to continue the search with the opponent to move.
If the opponent has no moves either, then the game is over, and the value of the position can be determined with finality by counting the pieces.

Third, we need to decide the interaction between the normal evaluation function and this final evaluation that occurs when the game is over.
We could insist that each evaluation function determine when the game is over and do the proper computation.
But that overburdens the evaluation functions and may lead to wasteful checking for the end of game.
Instead, I implemented a separate `final-value` evaluation function, which returns 0 for a draw, a large positive number for a win, and a large negative number for a loss.
Because fixnum arithmetic is most efficient, the constants `most-positive-fixnum` and `most-negative-fixnum` are used.
The evaluation functions must be careful to return numbers that are within this range.
All the evaluation functions in this chapter will be within range if fixnums are 20 bits or more.

In a tournament, it is not only important who wins and loses, but also by how much.
If we were trying to maximize the margin of victory, then `final-value` would be changed to include a small factor for the final difference.

[ ](#){:#l0080}`(defconstant winning-value most-positive-fixnum)`
!!!(p) {:.unnumlist}

`(defconstant losing-value most-negative-fixnum)`
!!!(p) {:.unnumlist}

`(defun final-value (player board)`
!!!(p) {:.unnumlist}

`  "Is this a win, loss, or draw for player?"`
!!!(p) {:.unnumlist}

`  (case (signum (count-difference player board))`
!!!(p) {:.unnumlist}

`    (-1 losing-value)`
!!!(p) {:.unnumlist}

`    ( 0 0)`
!!!(p) {:.unnumlist}

`    (+1 winning-value)))`
!!!(p) {:.unnumlist}

Fourth, and finally, we need to decide on the parameters for the minimax function.
Like the other evaluation functions, it needs the player to move and the current board as parameters.
It also needs an indication of how many ply to search, and the static evaluation function to apply to the leaf positions.
Thus, minimax will be a function of four arguments.
What will it return?
It needs to return the best move, but it also needs to return the value of that move, according to the static evaluation function.
We use multiple values for this.

[ ](#){:#l0085}`(defun minimax (player board ply eval-fn)`
!!!(p) {:.unnumlist}

`  "Find the best move, for PLAYER, according to EVAL-FN,`
!!!(p) {:.unnumlist}

`  searching PLY levels deep and backing up values."`
!!!(p) {:.unnumlist}

`  (if (= ply 0)`
!!!(p) {:.unnumlist}

`      (funcall eval-fn player board)`
!!!(p) {:.unnumlist}

`      (let ((moves (legal-moves player board)))`
!!!(p) {:.unnumlist}

`        (if (null moves)`
!!!(p) {:.unnumlist}

`         (if (any-legal-move?
(opponent player) board)`
!!!(p) {:.unnumlist}

`            (- (minimax (opponent player) board`
!!!(p) {:.unnumlist}

`                 (- ply 1) eval-fn))`
!!!(p) {:.unnumlist}

`            (final-value player board))`
!!!(p) {:.unnumlist}

`         (let ((best-move nil)`
!!!(p) {:.unnumlist}

`               (best-val nil))`
!!!(p) {:.unnumlist}

`           (dolist (move moves)`
!!!(p) {:.unnumlist}

`      (let* ((board2 (make-move move player`
!!!(p) {:.unnumlist}

`                    (copy-board board)))`
!!!(p) {:.unnumlist}

`          (val (- (minimax`
!!!(p) {:.unnumlist}

`               (opponent player) board2`
!!!(p) {:.unnumlist}

`               (- ply 1) eval-fn))))`
!!!(p) {:.unnumlist}

`       (when (or (null best-val)`
!!!(p) {:.unnumlist}

`             (> val best-val))`
!!!(p) {:.unnumlist}

`         (setf best-val val)`
!!!(p) {:.unnumlist}

`         (setf best-move move))))`
!!!(p) {:.unnumlist}

`    (values best-val best-move))))))`
!!!(p) {:.unnumlist}

The `minimax` function cannot be used as a strategy function as is, because it takes too many arguments and returns too many values.
The functional `minimax-searcher` returns an appropriate strategy.
Remember that a strategy is a function of two arguments: the player and the board.
`get-move` is responsible for passing the right arguments to the function, so the strategy need not worry about where the arguments come from.

[ ](#){:#l0090}`(defun minimax-searcher (ply eval-fn)`
!!!(p) {:.unnumlist}

`  "A strategy that searches PLY levels and then uses EVAL-FN."`
!!!(p) {:.unnumlist}

`  #’(lambda (player board)`
!!!(p) {:.unnumlist}

`      (multiple-value-bind (value move)`
!!!(p) {:.unnumlist}

`         (minimax player board ply eval-fn)`
!!!(p) {:.unnumlist}

`        (declare (ignore value))`
!!!(p) {:.unnumlist}

`        move)))`
!!!(p) {:.unnumlist}

We can test the minimax strategy, and see that searching ahead 3 ply is indeed better than looking at only 1 ply.
I show only the final result, which demonstrates that it is indeed an advantage to be able to look ahead:

[ ](#){:#l0095}`> (othello (minimax-searcher 3 #’count-difference)`
!!!(p) {:.unnumlist}

`         (maximizer #’count-difference))`
!!!(p) {:.unnumlist}

`...`
!!!(p) {:.unnumlist}

`The game is over.
Final result:`
!!!(p) {:.unnumlist}

`   1 2 3 4 5 6 7 8   [@=53 0=0 (+53)]`
!!!(p) {:.unnumlist}

`10 @ @ @ @ @ @ @ @`
!!!(p) {:.unnumlist}

`20 @ @ @ @ @ @ @ @`
!!!(p) {:.unnumlist}

`30 @ @ @ @ @ @ @ @`
!!!(p) {:.unnumlist}

`40 @ @ @ @ @ @ @ @`
!!!(p) {:.unnumlist}

`50 @ @ @ @ @ @ @ @`
!!!(p) {:.unnumlist}

`60 .
.
@ @ @ @ @ @`
!!!(p) {:.unnumlist}

`70 .
.
.
@ @ @ @ @`
!!!(p) {:.unnumlist}

`80 .
.
.
.
@ @ .
.`
!!!(p) {:.unnumlist}

## [ ](#){:#st0030}18.5 Smarter Searching: Alpha-Beta Search
{:#s0030}
{:.h1hd}

The problem with a full minimax search is that it considers too many positions.
It looks at every line of play, including many improbable ones.
Fortunately, there is a way to find the optimal line of play without looking at every possible position.
Let’s go back to our familiar search tree:[ ](#){:#p1575}

![u18-05-9780080571157](images/B9780080571157500182/u18-05-9780080571157.jpg)     

Here we have marked certain positions with question marks.
The idea is that the whole search tree evaluates to 10 regardless of the value of the positions labeled ?*i*.
Consider the position labeled ?1.
It does not matter what this position evaluates to, because the opponent will always choose to play toward the 10-position, to avoid the possibility of the 15.
Thus, we can cut off the search at this point and not consider the ?-position.
This kind of cutoff has historically been called a *beta* cutoff.

Now consider the position labeled ?4.
It does not matter what this position evaluates to, because we will always prefer to choose the 10 position at the left branch, rather than giving the opponent a chance to play to the 9-position.
This is an *alpha* cutoff.
Notice that it cuts off a whole subtree of positions below it (labeled ?2 and ?3).

In general, we keep track of two parameters that bound the true value of the current position.
The lower bound is a value we know we can achieve by choosing a certain line of play.
The idea is that we need not even consider moves that will lead to a value lower than this.
The lower bound has traditionally been called *alpha,* but we will name it `achievable`.
The upper bound represents a value the opponent can achieve by choosing a certain line of play.
It has been called *beta*, but we will call it `cutoff`.
Again, the idea is that we need not consider moves with a higher value than this (because then the opponent would avoid the move that is so good for us).
The alpha-beta algorithm is just minimax, but with some needless evaluations pruned by these two parameters.

In deeper trees with higher branching factors, many more evaluations can be pruned.
In general, a tree of depth *d* and branching factor *b* requires *bd* evaluations for full minimax, and as few as *b**d*/2 evaluations with alpha-beta minimax.

To implement alpha-beta search, we add two more parameters to the function `minimax` and rename it `alpha-beta`.
`achievable` is the best score the player can achieve; it is what we want to maximize.
The `cutoff` is a value that, when exceeded, will make the opponent choose another branch of the tree, thus making the rest of the current level of the tree irrelevant.
The test `until (>= achievable cutoff)` in the penultimate line of `minimax` does the cutoff; all the other changes just involve passing the parameters around properly.

[ ](#){:#l0100}`(defun alpha-beta (player board achievable cutoff ply eval-fn)`
!!!(p) {:.unnumlist}

`  "Find the best move, for PLAYER, according to EVAL-FN,`
!!!(p) {:.unnumlist}

`  searching PLY levels deep and backing up values,`
!!!(p) {:.unnumlist}

`  using cutoffs whenever possible."`
!!!(p) {:.unnumlist}

`  (if (= ply 0)`
!!!(p) {:.unnumlist}

`    (funcall eval-fn player board)`
!!!(p) {:.unnumlist}

`    (let ((moves (legal-moves player board)))`
!!!(p) {:.unnumlist}

`      (if (null moves)`
!!!(p) {:.unnumlist}

`        (if (any-legal-move?
(opponent player) board)`
!!!(p) {:.unnumlist}

`          (- (alpha-beta (opponent player) board`
!!!(p) {:.unnumlist}

`                  (- cutoff) (- achievable)`
!!!(p) {:.unnumlist}

`                  (- ply 1) eval-fn))`
!!!(p) {:.unnumlist}

`          (final-value player board))`
!!!(p) {:.unnumlist}

`      (let ((best-move (first moves)))`
!!!(p) {:.unnumlist}

`        (loop for move in moves do`
!!!(p) {:.unnumlist}

`         (let* ((board2 (make-move move player`
!!!(p) {:.unnumlist}

`                     (copy-board board)))`
!!!(p) {:.unnumlist}

`            (val (- (alpha-beta`
!!!(p) {:.unnumlist}

`                  (opponent player) board2`
!!!(p) {:.unnumlist}

`                  (- cutoff) (- achievable)`
!!!(p) {:.unnumlist}

`                  (- ply 1) eval-fn))))`
!!!(p) {:.unnumlist}

`           (when (> val achievable)`
!!!(p) {:.unnumlist}

`             (setf achievable val)`
!!!(p) {:.unnumlist}

`             (setf best-move move)))`
!!!(p) {:.unnumlist}

`         until (>= achievable cutoff))`
!!!(p) {:.unnumlist}

`      (values achievable best-move))))))`
!!!(p) {:.unnumlist}

`(defun alpha-beta-searcher (depth eval-fn)`
!!!(p) {:.unnumlist}

`  "A strategy that searches to DEPTH and then uses EVAL-FN."`
!!!(p) {:.unnumlist}

`  #’(lambda (player board)`
!!!(p) {:.unnumlist}

`    (multiple-value-bind (value move)`
!!!(p) {:.unnumlist}

`      (alpha-beta player board losing-value winning-value`
!!!(p) {:.unnumlist}

`                  depth eval-fn)`
!!!(p) {:.unnumlist}

`        (declare (ignore value))`
!!!(p) {:.unnumlist}

`        move)))`
!!!(p) {:.unnumlist}

It must be stressed that `alpha-beta` computes the exact same result as the full-search version of `minimax`.
The only advantage of the cutoffs is making the search go faster by considering fewer positions.

## [ ](#){:#st0035}18.6 An Analysis of Some Games
{:#s0035}
{:.h1hd}

Now is a good time to stop and analyze where we have gone.
We’ve demonstrated a program that can play a *legal* game of Othello, and some strategies that may or may not play a *good* game.
First, we’ll look at some individual games to see the mistakes made by some strategies, and then we’ll generate some statistics for series of games.

Is the weighted-squares measure a good one?
We can compare it to a strategy of maximizing the number of pieces.
Such a strategy would of course be perfect if it could look ahead to the end of the game, but the speed of our computers limits us to searching only a few ply, even with cutoffs.
Consider the following game, where black is maximizing the difference in the number of pieces, and white is maximizing the weighted sum of squares.
Both search to a depth of 4 ply:

[ ](#){:#l5105}`> (othello (alpha-beta-searcher 4 #’count-difference)`
!!!(p) {:.unnumlist}

`           (alpha-beta-searcher 4 #’weighted-squares))`
!!!(p) {:.unnumlist}

Black is able to increase the piece difference dramatically as the game progresses.
After 17 moves, white is down to only one piece:

[ ](#){:#l0105}`     1 2 3 4 5 6 7 8  [@=20 0=1 (+19)]`
!!!(p) {:.unnumlist}

`  10 0 @ .
.
.
.
.
.`
!!!(p) {:.unnumlist}

`  20 .
@ .
.
.
@ @ .`
!!!(p) {:.unnumlist}

`  30 @ @ @ @ @ @ .
.`
!!!(p) {:.unnumlist}

`  40 .
@ .
@ @ .
.
.`
!!!(p) {:.unnumlist}

`  50 @ @ @ @ @ @ .
.`
!!!(p) {:.unnumlist}

`  60 .
.
@ .
.
.
.
.`
!!!(p) {:.unnumlist}

`  70 .
.
.
.
.
.
.
.`
!!!(p) {:.unnumlist}

`  80 .
.
.
.
.
.
.
.`
!!!(p) {:.unnumlist}

Although behind by 19 points, white is actually in a good position, because the piece in the corner is safe and threatens many of black’s pieces.
White is able to maintain good position while being numerically far behind black, as shown in these positions later in the game:

[ ](#){:#l0110}`     1 2 3 4 5 6 7 8  [@=32 0=15 (+17)]`
!!!(p) {:.unnumlist}

`  10 0 0 0 0 @ @ 0 0`
!!!(p) {:.unnumlist}

`  20 @ @ 0 @ @ @ @ @`
!!!(p) {:.unnumlist}

`  30 @ @ 0 0 @ 0 @ @`
!!!(p) {:.unnumlist}

`  40 0 0 @ @ @ @ @ @`
!!!(p) {:.unnumlist}

`  50 @ 0 @ @ @ @ .
.`
!!!(p) {:.unnumlist}

`  60 @ @ 0 @ @ 0 .
.`
!!!(p) {:.unnumlist}

`  70 @ .
.
@ @ .
.
.`
!!!(p) {:.unnumlist}

`  80 .
.
.
.
.
.
.
.`
!!!(p) {:.unnumlist}

`     1 2 3 4 5 6 7 8  [@=34 0=19 (+15)]`
!!!(p) {:.unnumlist}

`  10 0 0 0 0 @ @ 0 0`
!!!(p) {:.unnumlist}

`  20 @ @ 0 @ @ @ @ @`
!!!(p) {:.unnumlist}

`  30 @ @ 0 0 @ 0 @ @`
!!!(p) {:.unnumlist}

`  40 0 @ 0 @ @ @ @ @`
!!!(p) {:.unnumlist}

`  50 0 @ 0 @ @ @ @ .`
!!!(p) {:.unnumlist}

`  60 0 @ 0 @ @ @ .
.`
!!!(p) {:.unnumlist}

`  70 0 @ @ @ @ .
.
.`
!!!(p) {:.unnumlist}

`  80 0 @ 0 .
.
.
.
.`
!!!(p) {:.unnumlist}

After some give-and-take, white gains the advantage for good by capturing eight pieces on a move to square 85 on the third-to-last move of the game:

[ ](#){:#l0115}`     1 2 3 4 5 6 7 8  [@=31 0=30 (+1)]`
!!!(p) {:.unnumlist}

`  10 0 0 0 0 @ @ 0 0`
!!!(p) {:.unnumlist}

`  20 @ @ 0 0 @ @ @ 0`
!!!(p) {:.unnumlist}

`  30 @ @ 0 0 0 @ @ 0`
!!!(p) {:.unnumlist}

`  40 0 @ 0 0 0 @ @ 0`
!!!(p) {:.unnumlist}

`  50 0 @ 0 @ 0 @ @ 0`
!!!(p) {:.unnumlist}

`  60 0 @ 0 @ @ @ @ 0`
!!!(p) {:.unnumlist}

`  70 0 @ @ @ @ @ 0 0`
!!!(p) {:.unnumlist}

`  80 0 @ @ @ .
.
.0`
!!!(p) {:.unnumlist}

`0 moves to 85.`
!!!(p) {:.unnumlist}

`     1 2 3 4 5 6 7 8  [@=23 0=39 (-16)]`
!!!(p) {:.unnumlist}

`  10 0 0 0 0 @ @ 0 0`
!!!(p) {:.unnumlist}

`  20 @ @ 0 0 @ @ @ 0`
!!!(p) {:.unnumlist}

`  30 @ @ 0 0 0 @ @ 0`
!!!(p) {:.unnumlist}

`  40 0 @ 0 0 0 @ @ 0`
!!!(p) {:.unnumlist}

`  50 0 @ 0 @ 0 @ @ 0`
!!!(p) {:.unnumlist}

`  60 0 @ 0 @ 0 @ 0 0`
!!!(p) {:.unnumlist}

`  70 0 @ @ 0 0 0 0 0`
!!!(p) {:.unnumlist}

`  80 0 0 0 0 0 .
.
0`
!!!(p) {:.unnumlist}

`@ moves to 86.`
!!!(p) {:.unnumlist}

`     1 2 3 4 5 6 7 8  [@=26 0=37 (-11)]`
!!!(p) {:.unnumlist}

`  10 0 0 0 0 @ @ 0 0`
!!!(p) {:.unnumlist}

`  20 @ @ 0 0 @ @ @ 0`
!!!(p) {:.unnumlist}

`  30 @ @ 0 0 0 @ @ 0`
!!!(p) {:.unnumlist}

`  40 0 @ 0 0 0 @ @ 0`
!!!(p) {:.unnumlist}

`  50 0 @ 0 @ 0 @ @ 0`
!!!(p) {:.unnumlist}

`  60 0 @ 0 @ 0 @ 0 0`
!!!(p) {:.unnumlist}

`  70 0 @ @ 0 @ @ 0 0`
!!!(p) {:.unnumlist}

`  80 0 0 0 0 0 @ .
0`
!!!(p) {:.unnumlist}

`0 moves to 87.`
!!!(p) {:.unnumlist}

`The game is over.
Final result:`
!!!(p) {:.unnumlist}

`     1 2 3 4 5 6 7 8  [@=24 0=40 (-16)]`
!!!(p) {:.unnumlist}

`  10 0 0 0 0 @ @ 0 0`
!!!(p) {:.unnumlist}

`  20 @ @ 0 0 @ @ @ 0`
!!!(p) {:.unnumlist}

`  30 @ @ 0 0 0 @ @ 0`
!!!(p) {:.unnumlist}

`  40 0 @ 0 0 0 @ @ 0`
!!!(p) {:.unnumlist}

`  50 0 @ 0 @ 0 @ @ 0`
!!!(p) {:.unnumlist}

`  60 0 @ 0 @ 0 @ 0 0`
!!!(p) {:.unnumlist}

`  70 0 @ @ 0 @ 0 0 0`
!!!(p) {:.unnumlist}

`  80 0 0 0 0 0 0 0 0`
!!!(p) {:.unnumlist}

`-16`
!!!(p) {:.unnumlist}

White ends up winning by 16 pieces.
Black’s strategy was too greedy: black was willing to give up position (all four corners and all but four of the edge squares) for temporary gains in material.

Increasing the depth of search does not compensate for a faulty evaluation function.
In the following game, black’s search depth is increased to 6 ply, while white’s is kept at 4.
The same things happen, although black’s doom takes a bit longer to unfold.

[ ](#){:#l0120}`> (othello (alpha-beta-searcher 6 #’count-difference)`
!!!(p) {:.unnumlist}

`           (alpha-beta-searcher 4 #’weighted-squares))`
!!!(p) {:.unnumlist}

Black slowly builds up an advantage:

[ ](#){:#l0125}`     1 2 3 4 5 6 7 8  [@=21 0=8 (+13)]`
!!!(p) {:.unnumlist}

`  10 .
.
@ @ @ @ @ .`
!!!(p) {:.unnumlist}

`  20 .
@ .
@ 0 @ .
.`
!!!(p) {:.unnumlist}

`  30 0 @ @ 0 @ 0 0 .`
!!!(p) {:.unnumlist}

`  40 .
@ .
@ 0 @ 0 .`
!!!(p) {:.unnumlist}

`  50 .
@ @ @ @ @ .
.`
!!!(p) {:.unnumlist}

`  60 .
@ .
@ .
0 .
.`
!!!(p) {:.unnumlist}

`  70 .
.
.
.
.
.
.
.`
!!!(p) {:.unnumlist}

`  80 .
.
.
.
.
.
.
.`
!!!(p) {:.unnumlist}

But at this point white has clear access to the upper left corner, and through that corner threatens to take the whole top edge.
Still, black maintains a material edge as the game goes on:

[ ](#){:#l0130}`     1 2 3 4 5 6 7 8  [@=34 0=11 (+23)]`
!!!(p) {:.unnumlist}

`  10 0 .
@ @ @ @ @ .`
!!!(p) {:.unnumlist}

`  20 .
0 0 @ @ @ .
.`
!!!(p) {:.unnumlist}

`  30 0 @ 0 0 @ @ @ @`
!!!(p) {:.unnumlist}

`  40 @ @ @ @ 0 @ @ .`
!!!(p) {:.unnumlist}

`  50 @ @ @ @ @ 0 @ .`
!!!(p) {:.unnumlist}

`  60 @ @ @ @ @ @ 0 0`
!!!(p) {:.unnumlist}

`  70 @ .
.
@ .
.
@ 0`
!!!(p) {:.unnumlist}

`  80 .
.
.
.
.
.
.
.`
!!!(p) {:.unnumlist}

But eventually white’s weighted-squares strategy takes the lead:

[ ](#){:#l0135}`     1 2 3 4 5 6 7 8  [@=23 0=27 (-4)]`
!!!(p) {:.unnumlist}

`  10 0 0 0 0 0 0 0 0`
!!!(p) {:.unnumlist}

`  20 @ @ 0 @ @ @ .
.`
!!!(p) {:.unnumlist}

`  30 0 @ 0 0 @ @ @ @`
!!!(p) {:.unnumlist}

`  40 0 @ 0 @ 0 @ @ .`
!!!(p) {:.unnumlist}

`  50 0 @ 0 @ @ 0 @ .`
!!!(p) {:.unnumlist}

`  60 0 0 0 @ @ @ 0 0`
!!!(p) {:.unnumlist}

`  70 0 .
0 @ .
.
@ 0`
!!!(p) {:.unnumlist}

`  80 0 .
.
.
.
.
.
.`
!!!(p) {:.unnumlist}

and is able to hold on to win:
!!!(p) {:.unnumlist}

`     1 2 3 4 5 6 7 8  [@=24 0=40 (-16)]`
!!!(p) {:.unnumlist}

`  10 0 0 0 0 0 0 0 0`
!!!(p) {:.unnumlist}

`  20 @ @ 0 @ 0 0 @ @`
!!!(p) {:.unnumlist}

`  30 0 @ 0 0 @ @ @ @`
!!!(p) {:.unnumlist}

`  40 0 @ 0 0 @ @ @ 0`
!!!(p) {:.unnumlist}

`  50 0 0 @ @ 0 @ 0 0`
!!!(p) {:.unnumlist}

`  60 0 0 0 @ 0 @ @ 0`
!!!(p) {:.unnumlist}

`  70 0 0 0 0 @ @ 0 0`
!!!(p) {:.unnumlist}

`  80 0 0 0 0 0 @ @ 0`
!!!(p) {:.unnumlist}

`-16`
!!!(p) {:.unnumlist}

This shows that brute-force searching is not a panacea.
While it is helpful to be able to search deeper, greater gains can be made by making the evaluation function more accurate.
There are many problems with the weighted-squares evaluation function.
Consider again this position from the first game above:

[ ](#){:#l0140}`     1 2 3 4 5 6 7 8  [@=20 0=1 (+19)]`
!!!(p) {:.unnumlist}

`  10 0 @ .
.
.
.
.
.`
!!!(p) {:.unnumlist}

`  20 .
@ .
.
.
@ @ .`
!!!(p) {:.unnumlist}

`  30 @ @ @ @ @ @ .
.`
!!!(p) {:.unnumlist}

`  40 .
@ .
@ @ .
.
.`
!!!(p) {:.unnumlist}

`  50 @ @ @ @ @ @ .
.`
!!!(p) {:.unnumlist}

`  60 .
@ .
.
.
.
.
.`
!!!(p) {:.unnumlist}

`  70 .
.
.
.
.
.
.
.`
!!!(p) {:.unnumlist}

`  80 .
.
.
.
.
.
.
.`
!!!(p) {:.unnumlist}

Here white, playing the weighted-squares strategy, chose to play 66.
This is probably a mistake, as 13 would extend white’s dominance of the top edge, and allow white to play again (since black would have no legal moves).
Unfortunately, white rejects this move, primarily because square 12 is weighted as -20.
Thus, there is a disincentive to taking this square.
But 12 is weighted -20 because it is a bad idea to take such a square when the corner is empty–the opponent will then have a chance to capture the corner, regaining the 12 square as well.
Thus, we want squares like 12 to have a negative score when the corner is empty, but not when it is already occupied.
The `modified-weighted-squares` evaluation function does just that.

[ ](#){:#l0145}`(defun modified-weighted-squares (player board)`
!!!(p) {:.unnumlist}

`  "Like WEIGHTED-SQUARES, but don't take off for moving`
!!!(p) {:.unnumlist}

`  near an occupied corner."`
!!!(p) {:.unnumlist}

`  (let ((w (weighted-squares player board)))`
!!!(p) {:.unnumlist}

`    (dolist (corner '(11 18 81 88))`
!!!(p) {:.unnumlist}

`      (when (not (eql (bref board corner) empty))`
!!!(p) {:.unnumlist}

`        (dolist (c (neighbors corner))`
!!!(p) {:.unnumlist}

`          (when (not (eql (bref board c) empty))`
!!!(p) {:.unnumlist}

`            (incf w (* (- 5 (aref *weights* c))`
!!!(p) {:.unnumlist}

`                  (if (eql (bref board c) player)`
!!!(p) {:.unnumlist}

`                     +1 -1)))))))`
!!!(p) {:.unnumlist}

`    w))`
!!!(p) {:.unnumlist}

`(let ((neighbor-table (make-array 100 : initial-element nil)))`
!!!(p) {:.unnumlist}

`  ;; Initialize the neighbor table`
!!!(p) {:.unnumlist}

`  (dolist (square all-squares)`
!!!(p) {:.unnumlist}

`    (dolist (dir all-directions)`
!!!(p) {:.unnumlist}

`      (if (valid-p (+ square dir))`
!!!(p) {:.unnumlist}

`          (push (+ square dir)`
!!!(p) {:.unnumlist}

`              (aref neighbor-table square)))))`
!!!(p) {:.unnumlist}

`  (defun neighbors (square)`
!!!(p) {:.unnumlist}

`    "Return a list of all squares adjacent to a square."`
!!!(p) {:.unnumlist}

`    (aref neighbor-table square)))`
!!!(p) {:.unnumlist}

## [ ](#){:#st0040}18.7 The Tournament Version of Othello
{:#s0040}
{:.h1hd}

While the `othello` function serves as a perfectly good moderator for casual play, there are two points that need to be fixed for tournament-level play.
First, tournament games are played under a strict time limit: a player who takes over 30 minutes total to make all the moves forfeits the game.
Second, the standard notation for Othello games uses square names in the range al to h8, rather than in the 11 to 88 range that we have used so far.
a1 is the upper left corner, a8 is the lower left corner, and h8 is the lower right corner.
We can write routines to translate between this notation and the one we were using by creating a table of square names.

[ ](#){:#l0150}`(let ((square-names`
!!!(p) {:.unnumlist}

`      (cross-product #’symbol`
!!!(p) {:.unnumlist}

`            ’(?
a b c d e f g h ?)`
!!!(p) {:.unnumlist}

`            ’(?
1 2 3 4 5 6 7 8 ?))))`
!!!(p) {:.unnumlist}

`  (defun h8->88 (str)`
!!!(p) {:.unnumlist}

`    "Convert from alphanumeric to numeric square notation."`
!!!(p) {:.unnumlist}

`    (or (position (string str) square-names :test #’string-equal)`
!!!(p) {:.unnumlist}

`        str))`
!!!(p) {:.unnumlist}

`  (defun 88->h8 (num)`
!!!(p) {:.unnumlist}

`    "Convert from numeric to alphanumeric square notation.”\`
!!!(p) {:.unnumlist}

`    (if (valid-p num)`
!!!(p) {:.unnumlist}

`        (elt square-names num)`
!!!(p) {:.unnumlist}

`        num)))`
!!!(p) {:.unnumlist}

`  (defun cross-product (fn xlist ylist)`
!!!(p) {:.unnumlist}

`    "Return a list of all (fn x y) values."`
!!!(p) {:.unnumlist}

`    (mappend #’(lambda (y)`
!!!(p) {:.unnumlist}

`             (mapcar #’(lambda (x) (funcall fn x y))`
!!!(p) {:.unnumlist}

`                 xlist))`
!!!(p) {:.unnumlist}

`        ylist))`
!!!(p) {:.unnumlist}

Note that these routines return their input unchanged when it is not one of the expected values.
This is to allow commands other than moving to a particular square.
For example, we will add a feature that recognizes `resign` as a move.

The `human` player needs to be changed slightly to read moves in this format.
While we’re at it, we’ll also print the list of possible moves:

[ ](#){:#l0155}`(defun human (player board)`
!!!(p) {:.unnumlist}

`  "A human player for the game of Othello"`
!!!(p) {:.unnumlist}

`  (format t "~&~c to move ~a: " (name-of player)`
!!!(p) {:.unnumlist}

`         (mapcar #’88->h8 (legal-moves player board)))`
!!!(p) {:.unnumlist}

`  (h8->88 (read)))`
!!!(p) {:.unnumlist}

![f18-05-9780080571157](images/B9780080571157500182/f18-05-9780080571157.jpg)     
Figure 18.5
!!!(span) {:.fignum}
Glossary for the Tournament Version of Othello
The `othello` function needn’t worry about notation, but it does need to monitor the time.
We make up a new data structure, the clock, which is an array of integers saying how much time (in internal units) each player has left.
For example, (`aref clock black`) is the amount of time black has left to make all his moves.
In Pascal, we would declare the clock array as `array[black..white]`, but in Common Lisp all arrays are zero-based, so we need an array of three elements to allow the subscript `black`, which is 2.

The clock is passed to `get-move` and `print-board` but is otherwise unused.
I could have complicated the main game loop by adding tests for forfeits because of expired time and, as we shall see later, resignation by either player.
However, I felt that would add a great deal of complexity for rarely used options.
Instead, I wrap the whole game loop, along with the computation of the final score, in a `catch` special form.
Then, if get-move encounters a forfeit or resignation, it can `throw` an appropriate final score: 64 or -64, depending on which player forfeits.

[ ](#){:#l0160}`(defvar *move-number* 1 “The number of the move to be played”)`
!!!(p) {:.unnumlist}

`(defun othello (bl-strategy wh-strategy`
!!!(p) {:.unnumlist}

`         &optional (print t) (minutes 30))`
!!!(p) {:.unnumlist}

`  "Play a game of othello.
Return the score, where a positive`
!!!(p) {:.unnumlist}

`  difference means black, the first player, wins."`
!!!(p) {:.unnumlist}

`  (let ((board (initial-board))`
!!!(p) {:.unnumlist}

`      (clock (make-array (+1 (max black white))`
!!!(p) {:.unnumlist}

`            :initial-element`
!!!(p) {:.unnumlist}

`            (* minutes 60`
!!!(p) {:.unnumlist}

`               internal-time-units-per-second))))`
!!!(p) {:.unnumlist}

`    (catch ’game-over`
!!!(p) {:.unnumlist}

`      (loop for *move-number* from 1`
!!!(p) {:.unnumlist}

`        for player = black then (next-to-play board player print)`
!!!(p) {:.unnumlist}

`        for strategy = (if (eql player black)`
!!!(p) {:.unnumlist}

`             bl-strategy`
!!!(p) {:.unnumlist}

`             wh-strategy)`
!!!(p) {:.unnumlist}

`        until (null player)`
!!!(p) {:.unnumlist}

`        do (get-move strategy player board print clock))`
!!!(p) {:.unnumlist}

`      (when print`
!!!(p) {:.unnumlist}

`           (format t “~&The game is over.
Final result:”)`
!!!(p) {:.unnumlist}

`           (print-board board clock))`
!!!(p) {:.unnumlist}

`      (count-difference black board))))`
!!!(p) {:.unnumlist}

Strategies now have to comply with the time-limit rule, so they may want to look at the time remaining.
Rather than passing the clock in as an argument to the strategy, I decided to store the clock in the special variable `*clock*`.
The new version of `othello` also keeps track of the `*move-number*`.
This also could have been passed to the strategy functions as a parameter.
But adding these extra arguments would require changes to all the strategies we have developed so far.
By storing the information in special variables, strategies that want to can look at the clock or the move number, but other strategies don’t have to know about them.

We still have the security problem–we don’t want a strategy to be able to set the opponent’s remaining time to zero and thereby win the game.
Thus, we use `*clock*` only as a copy of the “real” game clock.
The function `replace` copies the real clock into `*clock*`, and also copies the real board into `*board*`.

[ ](#){:#l0165}`(defvar *clock* (make-array 3) “A copy of the game clock”)`
!!!(p) {:.unnumlist}

`(defvar *board* (initial-board) “A copy of the game board”)`
!!!(p) {:.unnumlist}

`(defun get-move (strategy player board print clock)`
!!!(p) {:.unnumlist}

`  "Call the player’s strategy function to get a move.`
!!!(p) {:.unnumlist}

`  Keep calling until a legal move is made."`
!!!(p) {:.unnumlist}

`  ;; Note we don’t pass the strategy function the REAL board.`
!!!(p) {:.unnumlist}

`  ;; If we did, it could cheat by changing the pieces on the board.`
!!!(p) {:.unnumlist}

`  (when print (print-board board clock))`
!!!(p) {:.unnumlist}

`  (replace *clock* clock)`
!!!(p) {:.unnumlist}

`  (let* ((t0 (get-internal-real-time))`
!!!(p) {:.unnumlist}

`         (move (funcall strategy player (replace *board* board)))`
!!!(p) {:.unnumlist}

`         (t1 (get-internal-real-time)))`
!!!(p) {:.unnumlist}

`    (decf (elt clock player) (- t1 t0))`
!!!(p) {:.unnumlist}

`    (cond`
!!!(p) {:.unnumlist}

`      ((< (elt clock player) 0)`
!!!(p) {:.unnumlist}

`       (format t “~&~c has no time left and forfeits."`
!!!(p) {:.unnumlist}

`           (name-of player))`
!!!(p) {:.unnumlist}

`       (THROW ’game-over (if (eql player black) -64 64)))`
!!!(p) {:.unnumlist}

`      ((eq move ’resign)`
!!!(p) {:.unnumlist}

`       (THROW ’game-over (if (eql player black) -64 64)))`
!!!(p) {:.unnumlist}

`      ((and (valid-p move) (legal-p move player board))`
!!!(p) {:.unnumlist}

`       (when print`
!!!(p) {:.unnumlist}

`         (format t “~&~c moves to ~ a."`
!!!(p) {:.unnumlist}

`             (name-of player) (88->h8 move)))`
!!!(p) {:.unnumlist}

`       (make-move move player board))`
!!!(p) {:.unnumlist}

`      (t (warn “Illegal move: ~ a” (88->h8 move))`
!!!(p) {:.unnumlist}

`         (get-move strategy player board print clock)))))`
!!!(p) {:.unnumlist}

Finally, the function `print-board` needs to print the time remaining for each player; this requires an auxiliary function to get the number of minutes and seconds from an internal-format time interval.
Note that we make the arguments optional, so that in debugging one can say just (`print-board`) to see the current situation.
Also note the esoteric format option: `"~2, '0d"` prints a decimal number using at least two places, padding on the left with zeros.

[ ](#){:#l0170}`(defun print-board (&optional (board *board*) clock)`
!!!(p) {:.unnumlist}

`  "Print a board, along with some statistics."`
!!!(p) {:.unnumlist}

`  ;; First print the header and the current score`
!!!(p) {:.unnumlist}

`  (format t “~2& a b c d e f g h  [~c =~2a ~c =~2a (~@d)]"`
!!!(p) {:.unnumlist}

`         (name-of black) (count black board)`
!!!(p) {:.unnumlist}

`         (name-of white) (count white board)`
!!!(p) {:.unnumlist}

`         (count-difference black board))`
!!!(p) {:.unnumlist}

`  ;; Print the board itself`
!!!(p) {:.unnumlist}

`  (loop for row from 1 to 8 do`
!!!(p) {:.unnumlist}

`        (format t "~&~d" row)`
!!!(p) {:.unnumlist}

`        (loop for col from 1 to 8`
!!!(p) {:.unnumlist}

`            for piece = (bref board (+ col (* 10 row)))`
!!!(p) {:.unnumlist}

`            do (format t "~c " (name-of piece))))`
!!!(p) {:.unnumlist}

`  ;; Finally print the time remaining for each player`
!!!(p) {:.unnumlist}

`  (when clock`
!!!(p) {:.unnumlist}

`    (format t “ [~c =~a ~c =~a]~2&"`
!!!(p) {:.unnumlist}

`         (name-of black) (time-string (elt clock black))`
!!!(p) {:.unnumlist}

`         (name-of white) (time-string (elt clock white)))))`
!!!(p) {:.unnumlist}

`(defun time-string (time)`
!!!(p) {:.unnumlist}

`  "Return a string representing this internal time in min:secs."`
!!!(p) {:.unnumlist}

`  (multiple-value-bind (min sec)`
!!!(p) {:.unnumlist}

`      (floor (round time internal-time-units-per-second) 60)`
!!!(p) {:.unnumlist}

`    (format nil “~2d:~2,’0d” min sec)))`
!!!(p) {:.unnumlist}

## [ ](#){:#st0045}18.8 Playing a Series of Games
{:#s0045}
{:.h1hd}

A single game is not enough to establish that one strategy is better than another.
The following function allows two strategies to compete in a series of games:

[ ](#){:#l0175}`(defun othello-series (strategy1 strategy2 n-pairs)`
!!!(p) {:.unnumlist}

`  "Play a series of 2*n-pairs games, swapping sides."`
!!!(p) {:.unnumlist}

`  (let ((scores (loop repeat n-pairs`
!!!(p) {:.unnumlist}

`            collect (othello strategy1 strategy2 nil)`
!!!(p) {:.unnumlist}

`            collect (- (othello strategy2 strategy1 nil)))))`
!!!(p) {:.unnumlist}

`    ;; Return the number of wins, (1/2 for a tie),`
!!!(p) {:.unnumlist}

`    ;; the total of thepoint differences, and the`
!!!(p) {:.unnumlist}

`    ;; scores themselves.
all from strategy1’s point of view.`
!!!(p) {:.unnumlist}

`    (values (+ (count-if #’plusp scores)`
!!!(p) {:.unnumlist}

`          (/ (count-if #’zerop scores) 2))`
!!!(p) {:.unnumlist}

`        (apply #’+ scores)`
!!!(p) {:.unnumlist}

`        scores)))`
!!!(p) {:.unnumlist}

Let’s see what happens when we use it to pit the two weighted-squares functions against each other in a series of ten games:

[ ](#){:#l0180}`>(othello-series`
!!!(p) {:.unnumlist}

`    (alpha-beta-searcher 2 #’modified-weighted-squares)`
!!!(p) {:.unnumlist}

`    (alpha-beta-searcher 2 #’weighted-squares) 5)`
!!!(p) {:.unnumlist}

`0`
!!!(p) {:.unnumlist}

`60`
!!!(p) {:.unnumlist}

`(-28 40 -28 40 -28 40 -28 40 -28 40)`
!!!(p) {:.unnumlist}

Something is suspicious here–the same scores are being repeated.
A little thought reveals why: neither strategy has a random component, so the exact same game was played five times with one strategy going first, and another game was played five times when the other strategy goes first!
A more accurate appraisal of the two strategies’ relative worth would be gained by starting each game from some random position and playing from there.

Think for a minute how you would design to run a series of games starting from a random position.
One possibility would be to change the function `othello` to accept an optional argument indicating the initial state of the board.
Then `othello-series` could be changed to somehow generate a random board and pass it to `othello`.
While this approach is feasible, it means changing two existing working functions, as well as writing another function, `generate-random-board`.
But we couldn’t generate just any random board: it would have to be a legal board, so it would have to call `othello` and somehow get it to stop before the game was over.

An alternative is to leave both `othello` and `othello-series` alone and build another function on top of it, one that works by passing in two new strategies: strategies that make a random move for the first few moves and then revert to the normal specified behavior.
This is a better solution because it uses existing functions rather than modifying them, and because it requires no new functions besides `switch-strategies`, which could prove useful for other purposes, and `random-othello-series`, which does nothing more than call `othello-series` with the proper arguments.

[ ](#){:#l0185}`(defun random-othello-series (strategy1 strategy2`
!!!(p) {:.unnumlist}

`              n-pairs &optional (n-random 10))`
!!!(p) {:.unnumlist}

`  "Play a series of 2*n games, starting from a random position."`
!!!(p) {:.unnumlist}

`  (othello-series`
!!!(p) {:.unnumlist}

`    (switch-strategies #’random-strategy n-random strategy1)`
!!!(p) {:.unnumlist}

`    (switch-strategies #’random-strategy n-random strategy2)`
!!!(p) {:.unnumlist}

`    n-pairs))`
!!!(p) {:.unnumlist}

`(defun switch-strategies (strategy1 m strategy2)`
!!!(p) {:.unnumlist}

`  "Make a new strategy that plays strategy1 for m moves,`
!!!(p) {:.unnumlist}

`  then plays according to strategy2."`
!!!(p) {:.unnumlist}

`  #’(lambda (player board)`
!!!(p) {:.unnumlist}

`      (funcall (if (<= *move-number* m) strategy1 strategy2)`
!!!(p) {:.unnumlist}

`         player board)))`
!!!(p) {:.unnumlist}

There is a problem with this kind of series: it may be that one of the strategies just happens to get better random positions.
A fairer test would be to play two games from each random position, one with the each strategy playing first.
One way to do that is to alter `othello-series` so that it saves the random state before playing the first game of a pair, and then restores the saved random state before playing the second game.
That way the same random position will be duplicated.

[ ](#){:#l0190}`(defun othello-series (strategy1 strategy2 n-pairs)`
!!!(p) {:.unnumlist}

`  "Play a series of 2*n-pairs games.
swapping sides."`
!!!(p) {:.unnumlist}

`  (let ((scores`
!!!(p) {:.unnumlist}

`        (loop repeat n-pairs`
!!!(p) {:.unnumlist}

`           for random-state = (make-random-state)`
!!!(p) {:.unnumlist}

`           collect (othello strategy1 strategy2 nil)`
!!!(p) {:.unnumlist}

`           do (setf *random-state* random-state)`
!!!(p) {:.unnumlist}

`           collect (- (othello strategy2 strategy1 nil)))))`
!!!(p) {:.unnumlist}

`    ;; Return the number of wins (1/2 for a tie).`
!!!(p) {:.unnumlist}

`    ;; the total of the point differences, and the`
!!!(p) {:.unnumlist}

`    ;; scores themselves.
all from strategy1’s point of view.`
!!!(p) {:.unnumlist}

`    (values (+ (count-if #’plusp scores)`
!!!(p) {:.unnumlist}

`             (/ (count-if #’zerop scores) 2))`
!!!(p) {:.unnumlist}

`         (apply #’+ scores)`
!!!(p) {:.unnumlist}

`         scores)))`
!!!(p) {:.unnumlist}

Now we are in a position to do a more meaningful test.
In the following, the weighted-squares strategy wins 4 out of 10 games against the modified strategy, losing by a total of 76 pieces, with the actual scores indicated.

[ ](#){:#l0195}`> (random-othello-series`
!!!(p) {:.unnumlist}

`    (alpha-beta-searcher 2 #’weighted-squares)`
!!!(p) {:.unnumlist}

`    (alpha-beta-searcher 2#’modified-weighted-squares)`
!!!(p) {:.unnumlist}

`    5)`
!!!(p) {:.unnumlist}

`4`
!!!(p) {:.unnumlist}

− `76`
!!!(p) {:.unnumlist}

`(-8 -40 22 -30 10 -10 12 -18 4 -18)`
!!!(p) {:.unnumlist}

The `random-othello-series` function is useful for comparing two strategies.
When there are more than two strategies to be compared at the same time, the following function can be useful:

[ ](#){:#l0200}`(defun round-robin (strategies n-pairs &optional`
!!!(p) {:.unnumlist}

`            (n-random 10) (names strategies))`
!!!(p) {:.unnumlist}

`  "Play a tournament among the strategies.`
!!!(p) {:.unnumlist}

`  N-PAIRS = games each strategy plays as each col or against`
!!!(p) {:.unnumlist}

`  each opponent.
So with N strategies, a total of`
!!!(p) {:.unnumlist}

`  N*(N-1)*N-PAIRS games are played."`
!!!(p) {:.unnumlist}

`  (let* ((N (length strategies))`
!!!(p) {:.unnumlist}

`        (totals (make-array N :initial-element 0))`
!!!(p) {:.unnumlist}

`        (scores (make-array (list N N)`
!!!(p) {:.unnumlist}

`                : initial-element 0)))`
!!!(p) {:.unnumlist}

`    ;; Play the games`
!!!(p) {:.unnumlist}

`    (dotimes (i N)`
!!!(p) {:.unnumlist}

`      (loop for j from (+i 1) to (- N 1) do`
!!!(p) {:.unnumlist}

`         (let* ((wins (random-othello-series`
!!!(p) {:.unnumlist}

`                (elt strategies i)`
!!!(p) {:.unnumlist}

`                (elt strategies j)`
!!!(p) {:.unnumlist}

`                n-pairs n-random))`
!!!(p) {:.unnumlist}

`            (losses (- (* 2 n-pairs) wins)))`
!!!(p) {:.unnumlist}

`         (incf (aref scores i j) wins)`
!!!(p) {:.unnumlist}

`         (incf (aref scores j i) losses)`
!!!(p) {:.unnumlist}

`         (incf (aref totals i) wins)`
!!!(p) {:.unnumlist}

`         (incf (aref totals j) losses))))`
!!!(p) {:.unnumlist}

`  ;; Print the results`
!!!(p) {:.unnumlist}

`  (dotimes (i N)`
!!!(p) {:.unnumlist}

`    (format t “~&~a~20 T ~ 4f: “ (elt names i) (elt totals i))`
!!!(p) {:.unnumlist}

`    (dotimes (j N)`
!!!(p) {:.unnumlist}

`      (format t “~4f “ (if (= i j) '---`
!!!(p) {:.unnumlist}

`                   (aref scores i j)))))))`
!!!(p) {:.unnumlist}

Here is a comparison of five strategies that search only 1 ply:

[ ](#){:#l0205}`(defun mobility (player board)`
!!!(p) {:.unnumlist}

`  "The number of moves a player has."`
!!!(p) {:.unnumlist}

`  (length (legal-moves player board)))`
!!!(p) {:.unnumlist}

`> (round-robin`
!!!(p) {:.unnumlist}

`  (list (maximizer #’count-difference)`
!!!(p) {:.unnumlist}

`        (maximizer #’mobility)`
!!!(p) {:.unnumlist}

`        (maximizer #’weighted-squares)`
!!!(p) {:.unnumlist}

`        (maximizer #’modified-weighted-squares)`
!!!(p) {:.unnumlist}

`        #’random-strategy)`
!!!(p) {:.unnumlist}

`  5 10`
!!!(p) {:.unnumlist}

`  ’(count-difference mobility weighted modified-weighted random))`
!!!(p) {:.unnumlist}

`COUNT-DIFFERENCE   12.5:  --- 3.0 2.5 0.0 7.0`
!!!(p) {:.unnumlist}

`MOBILITY           20.5:  7.0 --- 1.5 5.0 7.0`
!!!(p) {:.unnumlist}

`WEIGHTED           28.0:  7.5 8.5 --- 3.0 9.0`
!!!(p) {:.unnumlist}

`MODIFIED-WEIGHTED  31.5: 10.0 5.0 7.0 --- 9.5`
!!!(p) {:.unnumlist}

`RANDOM              7.5:  3.0 3.0 1.0 0.5 ---`
!!!(p) {:.unnumlist}

The parameter `n-pairs` is 5, meaning that each strategy plays five games as black and five as white against each of the other four strategies, for a total of 40 games for each strategy and 100 games overall.
The first line of output says that the count-difference strategy won 12.5 of its 40 games, including 3 against the mobility strategy, 2.5 against the weighted strategy, none against the modified weighted, and 7 against the random strategy.
The fact that the random strategy manages to win 7.5 out of 40 games indicates that the other strategies are not amazingly strong.
Now we see what happens when the search depth is increased to 4 ply (this will take a while to run):

[ ](#){:#l0210}`> (round-robin`
!!!(p) {:.unnumlist}

`  (list (alpha-beta-searcher 4 #’count-difference)`
!!!(p) {:.unnumlist}

`        (alpha-beta-searcher 4 #’weighted-squares)`
!!!(p) {:.unnumlist}

`        (alpha-beta-searcher 4 #’modified-weighted-squares)`
!!!(p) {:.unnumlist}

`        #’random-strategy)`
!!!(p) {:.unnumlist}

`  5 10`
!!!(p) {:.unnumlist}

`  ’(count-difference weighted modified-weighted random))`
!!!(p) {:.unnumlist}

`COUNT-DIFFERENCE   12.0:  --- 2.0 0.0 10.0`
!!!(p) {:.unnumlist}

`WEIGHTED           23.5:  8.0 --- 5.5 10.0`
!!!(p) {:.unnumlist}

`MODIFIED-WEIGHTED  24.5: 10.0 4.5 --- 10.0`
!!!(p) {:.unnumlist}

`RANDOM              0.0:  0.0 0.0 0.0  ---`
!!!(p) {:.unnumlist}

Here the random strategy does not win any games–an indication that the other strategies are doing something right.
Notice that the modified weighted-squares has only a slight advantage over the weighted-squares, and in fact it lost their head-to-head series, four games to five, with one draw.
So it is not clear which strategy is better.

The output does not break down wins by black or white, nor does it report the numerical scores.
I felt that that would clutter up the output too much, but you’re welcome to add this information.
It turns out that white wins 23 (and draws 1) of the 40 games played between 4-ply searching strategies.
Usually, Othello is a fairly balanced game, because black has the advantage of moving first but white usually gets to play last.
It is clear that these strategies do not play well in the opening game, but for the last four ply they play perfectly.
This may explain white’s slight edge, or it may be a statistical aberration.

## [ ](#){:#st0050}18.9 More Efficient Searching
{:#s0050}
{:.h1hd}

The alpha-beta cutoffs work when we have established a good move and another move proves to be not as good.
Thus, we will be able to make cutoffs earlier if we ensure that good moves are considered first.
Our current algorithm loops through the list of `legal-moves`, but `legal-moves` makes no attempt to order the moves in any way.
We will call this the *random-ordering* strategy (even though the ordering is not random at all–square 11 is always considered first, then 12, etc.).

One way to try to generate good moves first is to search highly weighted squares first.
Since `legal-moves` considers squares in the order defined by `all-squares`, all we have to do is redefine the list `all-squares`[3](#fn0025){:#xfn0025}:

[ ](#){:#l0215}`(defconstant all-squares`
!!!(p) {:.unnumlist}

`  (sort (loop for i from 11 to 88`
!!!(p) {:.unnumlist}

`         when (<= 1 (mod i 10) 8) collect i)`
!!!(p) {:.unnumlist}

`      #’> :key #’(lambda (sq) (elt *weights* sq))))`
!!!(p) {:.unnumlist}

Now the corner squares will automatically be considered first, followed by the other highly weighted squares.
We call this the s*tatic-ordering* strategy, because the ordering is not random, but it does not change depending on the situation.

A more informed way to try to generate good moves first is to sort the moves according to the evaluation function.
This means making more evaluations.
Previously, only the boards at the leaves of the search tree were evaluated.
Now we need to evaluate every board.
In order to avoid evaluating a board more than once, we make up a structure called a `node`, which holds a board, the square that was taken to result in that board, and the evaluation value of that board.
The search is the same except that nodes are passed around instead of boards, and the nodes are sorted by their value.

[ ](#){:#l0220}`(defstruct (node) square board value)`
!!!(p) {:.unnumlist}

`(defun alpha-beta-searcher2 (depth eval-fn)`
!!!(p) {:.unnumlist}

`  "Return a strategy that does A-B search with sorted moves."`
!!!(p) {:.unnumlist}

`  #’(lambda (player board)`
!!!(p) {:.unnumlist}

`    (multiple-value-bind (value node)`
!!!(p) {:.unnumlist}

`        (alpha-beta2`
!!!(p) {:.unnumlist}

`          player (make-node :board board`
!!!(p) {:.unnumlist}

`                 :value (funcall eval-fn player board))`
!!!(p) {:.unnumlist}

`          losing-value winning-value depth eval-fn)`
!!!(p) {:.unnumlist}

`      (declare (ignore value))`
!!!(p) {:.unnumlist}

`      (node-square node))))`
!!!(p) {:.unnumlist}

`(defun alpha-beta2 (player node achievable cutoff ply eval-fn)`
!!!(p) {:.unnumlist}

`  "A-B search.
sorting moves by eval-fn"`
!!!(p) {:.unnumlist}

`  ;; Returns two values: achievable-value and move-to-make`
!!!(p) {:.unnumlist}

`  (if (= ply 0)`
!!!(p) {:.unnumlist}

`    (values (node-value node) node)`
!!!(p) {:.unnumlist}

`    (let* ((board (node-board node))`
!!!(p) {:.unnumlist}

`           (nodes (legal-nodes player board eval-fn)))`
!!!(p) {:.unnumlist}

`      (if (null nodes)`
!!!(p) {:.unnumlist}

`         (if (any-legal-move?
(opponent player) board)`
!!!(p) {:.unnumlist}

`           (values (- (alpha-beta2 (opponent player)`
!!!(p) {:.unnumlist}

`                   (negate-value node)`
!!!(p) {:.unnumlist}

`                   (- cutoff) (- achievable)`
!!!(p) {:.unnumlist}

`                   (- ply 1) eval-fn))`
!!!(p) {:.unnumlist}

`               nil)`
!!!(p) {:.unnumlist}

`           (values (final-value player board) nil))`
!!!(p) {:.unnumlist}

`      (let ((best-node (first nodes)))`
!!!(p) {:.unnumlist}

`        (loop for move in nodes`
!!!(p) {:.unnumlist}

`             for val = (- (alpha-beta2`
!!!(p) {:.unnumlist}

`                  (opponent player)`
!!!(p) {:.unnumlist}

`                  (negate-value move)`
!!!(p) {:.unnumlist}

`                  (- cutoff) (- achievable)`
!!!(p) {:.unnumlist}

`                  (- ply 1) eval-fn))`
!!!(p) {:.unnumlist}

`             do (when (> val achievable)`
!!!(p) {:.unnumlist}

`               (setf achievable val)`
!!!(p) {:.unnumlist}

`               (setf best-node move))`
!!!(p) {:.unnumlist}

`             until (>= achievable cutoff))`
!!!(p) {:.unnumlist}

`           (values achievable best-node))))))`
!!!(p) {:.unnumlist}

`(defun negate-value (node)`
!!!(p) {:.unnumlist}

`  "Set the value of a node to its negative."`
!!!(p) {:.unnumlist}

`  (setf (node-value node) (- (node-value node)))`
!!!(p) {:.unnumlist}

`  node)`
!!!(p) {:.unnumlist}

`(defun legal-nodes (player board eval-fn)`
!!!(p) {:.unnumlist}

`  "Return a list of legal moves, each one packed into a node."`
!!!(p) {:.unnumlist}

`  (let ((moves (legal-moves player board)))`
!!!(p) {:.unnumlist}

`    (sort (map-into`
!!!(p) {:.unnumlist}

`        moves`
!!!(p) {:.unnumlist}

`        #’(lambda (move)`
!!!(p) {:.unnumlist}

`           (let ((new-board (make-move move player`
!!!(p) {:.unnumlist}

`                    (copy-board board))))`
!!!(p) {:.unnumlist}

`             (make-node`
!!!(p) {:.unnumlist}

`               :square move :board new-board`
!!!(p) {:.unnumlist}

`               :value (funcall eval-fn player new-board))))`
!!!(p) {:.unnumlist}

`        moves)`
!!!(p) {:.unnumlist}

`      #’> :key #’node-value)))`
!!!(p) {:.unnumlist}

(Note the use of the function `map-into`.
This is part of ANSI Common Lisp, but if it is not a part of your implementation, a definition is provided on [page 857](B9780080571157500248.xhtml#p857).)

The following table compares the performance of the random-ordering strategy, the sorted-ordering strategy and the static-ordering strategy in the course of a single game.
All strategies search 6 ply deep.
The table measures the number of boards investigated, the number of those boards that were evaluated (in all cases the evaluation function was `modified-weighted-squares`) and the time in seconds to compute a move.

[ ](#){:#t0010}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| random order | sorted order | static order |
| boards | evals | secs | boards | evals | secs | boards | evals | secs |
| 13912 | 10269 | 69 | 5556 | 5557 | 22 | 2365 | 1599 | 19 |
| 9015 | 6751 | 56 | 6571 | 6572 | 25 | 3081 | 2188 | 18 |
| 9820 | 7191 | 46 | 11556 | 11557 | 45 | 5797 | 3990 | 31 |
| 4195 | 3213 | 20 | 5302 | 5303 | 17 | 2708 | 2019 | 15 |
| 10890 | 7336 | 60 | 10709 | 10710 | 38 | 3743 | 2401 | 23 |
| 13325 | 9679 | 63 | 6431 | 6432 | 24 | 4222 | 2802 | 24 |
| 13163 | 9968 | 58 | 9014 | 9015 | 32 | 6657 | 4922 | 31 |
| 16642 | 12588 | 70 | 9742 | 9743 | 33 | 10421 | 7488 | 51 |
| 18016 | 13366 | 80 | 11002 | 11003 | 37 | 9508 | 7136 | 41 |
| 23295 | 17908 | 104 | 15290 | 15291 | 48 | 26435 | 20282 | 111 |
| 34120 | 25895 | 143 | 22994 | 22995 | 75 | 20775 | 16280 | 78 |
| 56117 | 43230 | 224 | 46883 | 46884 | 150 | 48415 | 36229 | 203 |
| 53573 | 41266 | 209 | 62252 | 62253 | 191 | 37803 | 28902 | 148 |
| 43943 | 33184 | 175 | 31039 | 31040 | 97 | 33180 | 24753 | 133 |
| 51124 | 39806 | 193 | 45709 | 45710 | 135 | 19297 | 15064 | 69 |
| 24743 | 18777 | 105 | 20003 | 20004 | 65 | 15627 | 11737 | 66 |
| 1.0 | 1.0 | 1.0 | .81 | 1.07 | .62 | .63 | .63 | .63 |

![t0010](images/B9780080571157500182/t0010.png)

The last two lines of the table give the averages and the averages normalized to the random-ordering strategy’s performance.
The sorted-ordering strategy takes only 62% of the time of the random-ordering strategy, and the static-ordering takes 63%.
These times are not to be trusted too much, because a large-scale garbage collection was taking place during the latter part of the game, and it may have thrown off the times.
The board and evaluation count may be better indicators, and they both show the static-ordering strategy doing the best.

We have to be careful how we evaluate these results.
Earlier I said that alpha-beta search makes more cutoffs when it is presented first with better moves.
The actual truth is that it makes more cutoffs when presented first with moves that *the evaluation function thinks* are better.
In this case the evaluation function and the static-ordering strategy are in strong agreement on what are the best moves, so it is not surprising that static ordering does so well.
As we develop evaluation functions that vary from the weighted-squares approach, we will have to run experiments again to see if the static-ordering is still the best.

## [ ](#){:#st0055}18.10 It Pays to Precycle
{:#s0055}
{:.h1hd}

The progressive city of Berkeley, California, has a strong recycling program to reclaim glass, paper, and aluminum that would otherwise be discarded as garbage.
In 1989, Berkeley instituted a novel program of *precycling:* consumers are encouraged to avoid buying products that corne in environmentally wasteful packages.

Your Lisp system also has a recycling program: the Lisp garbage collector automatically recycles any unused storage.
However, there is a cost to this program, and you the consumer can get better performance by precycling your data.
Don’t buy wasteful data structures when simpler ones can be used or reused.
You, the Lisp programmer, may not be able to save the rain forests or the ozone layer, but you can save valuable processor time.

We saw before that the search routines look at tens of thousands of boards per move.
Currently, each board position is created anew by `copy-board` and discarded soon thereaf ter.
We could avoid generating all this garbage by reusing the same board at each ply.
We’d still need to keep the board from the previous ply for use when the search backs up.
Thus, a vector of boards is needed.
In the following we assume that we will never search deeper than 40 ply.
This is a safe assumption, as even the fastest Othello programs can only search about 15 ply before running out of time.

[ ](#){:#l0225}`(defvar *ply-boards*`
!!!(p) {:.unnumlist}

`  (apply #’vector (loop repeat 40 collect (initial-board))))`
!!!(p) {:.unnumlist}

Now that we have sharply limited the number of boards needed, we may want to reevaluate the implementation of boards.
Instead of having the board as a vector of pieces (to save space), we may want to implement boards as vectors of bytes or full words.
In some implementations, accessing elements of such vectors is faster.
(In other implementations, there is no difference.)

An implementation using the vector of boards will be done in the next section.
Note that there is another alternative: use only one board, and update it by making and retracting moves.
This is a good alternative in a game like chess, where a move only alters two squares.
In Othello, many squares can be altered by a move, so copying the whole board over and making the move is not so bad.

It should be mentioned that it is worth looking into the problem of copying a position from one board to another.
The function `replace` copies one sequence (or part of it) into another, but it is a generic function that may be slow.
In particular, if each element of a board is only 2 bits, then it may be much faster to use displaced arrays to copy 32 bits at a time.
The advisability of this approach depends on the implementation, and so it is not explored further here.

## [ ](#){:#st0060}18.11 Killer Moves
{:#s0060}
{:.h1hd}

In [section 18.9](#s0050), we considered the possibility of searching moves in a different order, in an attempt to search the better moves first, thereby getting more alpha-beta pruning.
In this section, we consider the *killer heuristic,* which states that a move that has proven to be a good one in one line of play is also likely to be a good one in another line of play.
To use chess as perhaps a more familiar example, suppose I consider one move, and it leads to the opponent replying by capturing my queen.
This is a killer move, one that I would like to avoid.
Therefore, when I consider other possible moves, I want to immediately consider the possibility of the opponent making that queen-capturing move.

The function `alpha-beta3` adds the parameter `killer`, which is the best move found so far at the current level.
After we determine the `legal-moves`, we use `put-first` to put the killer move first, if it is in fact a legal move.
When it cornes time to search the next level, we keep track of the best move in `killer2`.
This requires keeping track of the value of the best move in `killer2-val`.
Everything else is unchanged, except that we get a new board by recycling the `*ply-boards*` vector rather than by allocating fresh ones.

[ ](#){:#l0230}`(defun alpha-beta3 (player board achievable cutoff ply eval-fn`
!!!(p) {:.unnumlist}

`           killer)`
!!!(p) {:.unnumlist}

`  "A-B search, putting killer move first."`
!!!(p) {:.unnumlist}

`  (if (= ply 0)`
!!!(p) {:.unnumlist}

`    (funcall eval-fn player board)`
!!!(p) {:.unnumlist}

`    (let ((moves (put-first killer (legal-moves player board))))`
!!!(p) {:.unnumlist}

`      (if (null moves)`
!!!(p) {:.unnumlist}

`        (if (any-legal-move?
(opponent player) board)`
!!!(p) {:.unnumlist}

`          (- (alpha-beta3 (opponent player) board`
!!!(p) {:.unnumlist}

`                  (- cutoff) (- achievable)`
!!!(p) {:.unnumlist}

`                  (- ply 1) eval-fn nil))`
!!!(p) {:.unnumlist}

`          (final-value player board))`
!!!(p) {:.unnumlist}

`        (let ((best-move (first moves))`
!!!(p) {:.unnumlist}

`            (new-board (aref *ply-boards* ply))`
!!!(p) {:.unnumlist}

`            (killer2 nil)`
!!!(p) {:.unnumlist}

`            (killer2-val winning-value))`
!!!(p) {:.unnumlist}

`          (loop for move in moves`
!!!(p) {:.unnumlist}

`              do (multiple-value-bind (val reply)`
!!!(p) {:.unnumlist}

`                  (alpha-beta3`
!!!(p) {:.unnumlist}

`                    (opponent player)`
!!!(p) {:.unnumlist}

`                    (make-move move player`
!!!(p) {:.unnumlist}

`                      (replace new-board board))`
!!!(p) {:.unnumlist}

`                    (- cutoff) (- achievable)`
!!!(p) {:.unnumlist}

`                    (- ply 1) eval-fn killer2)`
!!!(p) {:.unnumlist}

`                (setf val (- val))`
!!!(p) {:.unnumlist}

`                (when (> val achievable)`
!!!(p) {:.unnumlist}

`                  (setf achievable val)`
!!!(p) {:.unnumlist}

`                  (setf best-move move))`
!!!(p) {:.unnumlist}

`                (when (and reply (< val killer2-val))`
!!!(p) {:.unnumlist}

`                  (setf killer2 reply)`
!!!(p) {:.unnumlist}

`                  (setf killer2-val val)))`
!!!(p) {:.unnumlist}

`              until (>= achievable cutoff))`
!!!(p) {:.unnumlist}

`            (values achievable best-move))))))`
!!!(p) {:.unnumlist}

`(defun alpha-beta-searcher3 (depth eval-fn)`
!!!(p) {:.unnumlist}

`  "Return a strategy that does A-B search with killer moves."`
!!!(p) {:.unnumlist}

`  #’(lambda (player board)`
!!!(p) {:.unnumlist}

`    (multiple-value-bind (value move)`
!!!(p) {:.unnumlist}

`        (alpha-beta3 player board losing-value winning-value`
!!!(p) {:.unnumlist}

`            depth eval-fn nil)`
!!!(p) {:.unnumlist}

`      (declare (ignore value))`
!!!(p) {:.unnumlist}

`      move)))`
!!!(p) {:.unnumlist}

`(defun put-first (killer moves)`
!!!(p) {:.unnumlist}

`  "Move the killer move to the front of moves,`
!!!(p) {:.unnumlist}

`  if the killer move is in fact a legal move."`
!!!(p) {:.unnumlist}

`  (if (member killer moves)`
!!!(p) {:.unnumlist}

`    (cons killer (delete killer moves))`
!!!(p) {:.unnumlist}

`    moves))`
!!!(p) {:.unnumlist}

Another experiment on a single game reveals that adding the killer heuristic to staticordering search (again at 6-ply) cuts the number of boards and evaluations, and the total time, all by about 20%.
To summarize, alpha-beta search at 6 ply with random ordering takes 105 seconds per move (in our experiment), adding static-ordering cuts it to 66 seconds, and adding killer moves to that cuts it again to 52 seconds.
This doesn’t include the savings that alpha-beta cutoffs give over full minimax search.
At 6 ply with a branching factor of 7, full minimax would take about nine times longer than static ordering with killers.
The savings increase with increased depth.
At 7 ply and a branching factor of 10, a small experiment shows that static-ordering with killers looks at only 28,000 boards in about 150 seconds.
Full minimax would evaluate 10 million boards and take 350 times longer.
The times for full minimax are estimates based on the number of boards per second, not on an actual experiment.

The algorithm in this section just keeps track of one killer move.
It is of course possible to keep track of more than one.
The Othello program Bill ([Lee and Mahajan 1990b](B9780080571157500285.xhtml#bb0715)) merges the idea of killer moves with legal move generation: it keeps a list of possible moves at each level, sorted by their value.
The legal move generator then goes down this list in sorted order.

It should be stressed once again that all this work on alpha-beta cutoffs, ordering, and killer moves has not made any change at all in the moves that are selected.
We still end up choosing the same move that would be made by a full minimax search to the given depth, we are just doing it faster, without looking at possibilities that we can prove are not as good.

## [ ](#){:#st0065}18.12 Championship Programs: Iago and Bill
{:#s0065}
{:.h1hd}

As mentioned in the introduction, the unpredictability of Othello makes it a difficult game for humans to master, and thus programs that search deeply can do comparatively well.
In fact, in 1981 the reigning champion, Jonathan Cerf, proclaimed “In my opinion the top programs … are now equal (if not superior) to the best human players.” In discussing Rosenbloom’s Iago program (1982), Cerf went on to say “I understand Paul Rosenbloom is interested in arranging a match against me.
Unfortunately my schedule is very full, and I’m going to see that it remains that way for the foreseeable future.”

In 1989, another program, Bill ([Lee and Mahajan 1990](B9780080571157500285.xhtml#bb0715)) beat the highest rated American Othello player, Brian Rose, by a score of 56-8.
Bill’s evaluation function is fast enough to search 6–8 ply under tournament conditions, yet it is so accurate that it beats its creator, Kai-Fu Lee, searching only 1 ply.
(However, Lee is only a novice Othello player; his real interest is in speech recognition; see [Waibel and Lee 1991](B9780080571157500285.xhtml#bb1285).) There are other programs that also play at a high level, but they have not been written up in the AI literature as Iago and Bill have.

In this section we present an evaluation function based on Iago’s, although it also contains elements of Bill, and of an evaluation function written by Eric Wefald in 1989.
The evaluation function makes use of two main features: *mobilityand edge stability*.

### [ ](#){:#st0070}Mobility
{:#s0070}
{:.h2hd}

Both Iago and Bill make heavy use of the concept of *mobility*.
Mobility is a measure of the ability to make moves; basically, the more moves one can make, the better.
This is not quite true, because there is no advantage in being able to make bad moves, but it is a useful heuristic.
We define *current mobility* as the number of legal moves available to a player, and *potential mobility* as the number of blank squares that are adjacent to opponent’s pieces.
These include the legal moves.
A better measure of mobility would try to count only good moves.
The following function computes both current and potential mobility for a player:

[ ](#){:#l0235}`(defun mobility (player board)`
!!!(p) {:.unnumlist}

`  "Current mobility is the number of legal moves.`
!!!(p) {:.unnumlist}

`  Potential mobility is the number of blank squares`
!!!(p) {:.unnumlist}

`  adjacent to an opponent that are not legal moves.`
!!!(p) {:.unnumlist}

`  Returns current and potential mobility for player."`
!!!(p) {:.unnumlist}

`  (let ((opp (opponent player))`
!!!(p) {:.unnumlist}

`      (current 0) ; player’s current mobility`
!!!(p) {:.unnumlist}

`      (potential 0)) ; player’s potential mobility`
!!!(p) {:.unnumlist}

`  (dolist (square all-squares)`
!!!(p) {:.unnumlist}

`    (when (eql (bref board square) empty)`
!!!(p) {:.unnumlist}

`      (cond ((legal-p square player board)`
!!!(p) {:.unnumlist}

`   (incf current))`
!!!(p) {:.unnumlist}

`    ((some #’(lambda (sq) (eql (bref board sq) opp))`
!!!(p) {:.unnumlist}

`        (neighbors square))`
!!!(p) {:.unnumlist}

`      (incf potential)))))`
!!!(p) {:.unnumlist}

`(values current (+ current potential))))`
!!!(p) {:.unnumlist}

### [ ](#){:#st0075}Edge Stability
{:#s0075}
{:.h2hd}

Success at Othello often hinges around edge play, and both Iago and Bill evaluate the edges carefully.
Edge analysis is made easier by the fact that the edges are fairly independent of the interior of the board: once a piece is placed on the edge, no interior moves can flip it.
This independence allows a simplifying assumption: to evaluate a position’s edge strength, evaluate each of the four edges independently, without consideration of the interior of the board.
The evaluation can be made more accurate by considering the X-squares to be part of the edge.

Even evaluating a single edge is a time-consuming task, so Bill and Iago compile away the evaluation by building a table of all possible edge positions.
An “edge” according to Bill is ten squares: the eight actual edge squares and the two X-squares.
Since each square can be black, white, or empty, there are 310 or 59,049 possible edge positions–a large but manageable number.

The value of each edge position is determined by a process of succesive approximation.
Just as in a minimax search, we will need a static edge evaluation function to determine the value of a edge position without search.
This static edge evaluation function is applied to every possible edge position, and the results are stored in a 59,049 element vector.
The static evaluation is just a weighted sum of the occupied squares, with different weights given depending on if the piece is stable or unstable.

Each edge position’s evaluation can be improved by a process of search.
Iago uses a single ply search: given a position, consider all moves that could be made (including no move at all).
Some moves will be clearly legal, because they flip pieces on the edge, but other moves will only be legal if there are pieces in the interior of the board to flip.
Since we are only considering the edge, we don’t know for sure if these moves are legal.
They will be assigned probabilities of legality.
The updated evaluation of a position is determined by the values and probabilities of each move.
This is done by sorting the moves by value and then summing the product of the value times the probability that the move can be made.
This process of iterative approximation is repeated five times for each position.
At that point, Rosenbloom reports, the values have nearly converged.

In effect, this extends the depth of the normal alpha-beta search by including an edge-only search in the evaluation function.
Since each edge position with *n* pieces is evaluated as a function of the positions with *n* + 1 pieces, the search is complete–it is an implicit 10-ply search.

Calculating edge stability is a bit more complicated than the other features.
The first step is to define a variable, `*edge-table*`, which will hold the evaluation of each edge position, and a constant, `edge-and-x-lists`, which is a list of the squares on each of the four edges.
Each edge has ten squares because the X-squares are included.

[ ](#){:#l0240}`(defvar *edge-table* (make-array (expt 3 10))`
!!!(p) {:.unnumlist}

`  "Array of values to player-to-move for edge positions.”)`
!!!(p) {:.unnumlist}

`(defconstant edge-and-x-lists`
!!!(p) {:.unnumlist}

`  ’((22 11 12 13 14 15 16 17 18 27)`
!!!(p) {:.unnumlist}

`    (72 81 82 83 84 85 86 87 88 77)`
!!!(p) {:.unnumlist}

`    (22 11 21 31 41 51 61 71 81 72)`
!!!(p) {:.unnumlist}

`    (27 18 28 38 48 58 68 78 88 77))`
!!!(p) {:.unnumlist}

`  "The four edges (with their X-squares).”)`
!!!(p) {:.unnumlist}

Now for each edge we can compute an index into the edge table by building a 10-digit base-3 number, where each digit is 1 if the corresponding edge square is occupied by the player, 2 if by the opponent, and 0 if empty.
The function `edge-index` computes this, and `edge-stability` sums the values of the four edge indexes.

[ ](#){:#l0245}`(defun edge-index (player board squares)`
!!!(p) {:.unnumlist}

`  "The index counts 1 for player; 2 for opponent,`
!!!(p) {:.unnumlist}

`  on each square---summed as a base 3 number."`
!!!(p) {:.unnumlist}

`  (let ((index 0))`
!!!(p) {:.unnumlist}

`    (dolist (sq squares)`
!!!(p) {:.unnumlist}

`      (setq index (+ (* index 3)`
!!!(p) {:.unnumlist}

`          (cond ((eql (bref board sq) empty) 0)`
!!!(p) {:.unnumlist}

`              ((eql (bref board sq) player) 1)`
!!!(p) {:.unnumlist}

`              (t 2)))))`
!!!(p) {:.unnumlist}

`    index))`
!!!(p) {:.unnumlist}

`(defun edge-stability (player board)`
!!!(p) {:.unnumlist}

`  "Total edge evaluation for player to move on board."`
!!!(p) {:.unnumlist}

`  (loop for edge-list in edge-and-x-lists`
!!!(p) {:.unnumlist}

`      sum (aref *edge-table*`
!!!(p) {:.unnumlist}

`         (edge-index player board edge-list))))`
!!!(p) {:.unnumlist}

The function `edge-stability` is all we will need in Iago’s evaluation function, but we still need to generate the edge table.
Since this needs to be done only once, we don’t have to worry about efficiency.
In particular, rather than invent a new data structure to represent edges, we will continue to use complete boards, even though they will be mostly empty.
The computations for the edge table will be made on the top edge, from the point of view of black, with black to play.
But the same table can be used for white, or for one of the other edges, because of the way the edge index is computed.

Each position in the table is first initialized to a static value computed by a kind of weighted-squares metric, but with different weights depending on if a piece is in danger of being captured.
After that, each position is updated by considering the possible moves that can be made from the position, and the values of each of these moves.

[ ](#){:#l0250}`(defconstant top-edge (first edge-and-x-lists))`
!!!(p) {:.unnumlist}

`(defun init-edge-table ()`
!!!(p) {:.unnumlist}

`  "Initialize *edge-table*, starting from the empty board."`
!!!(p) {:.unnumlist}

`  ;; Initialize the static values`
!!!(p) {:.unnumlist}

`  (loop for n-pieces from 0 to 10 do`
!!!(p) {:.unnumlist}

`      (map-edge-n-pieces`
!!!(p) {:.unnumlist}

`           #’(lambda (board index)`
!!!(p) {:.unnumlist}

`             (setf (aref *edge-table* index)`
!!!(p) {:.unnumlist}

`               (static-edge-stability black board)))`
!!!(p) {:.unnumlist}

`           black (initial-board) n-pieces top-edge 0))`
!!!(p) {:.unnumlist}

`  ;; Now iterate five times trying to improve:`
!!!(p) {:.unnumlist}

`  (dotimes (i 5)`
!!!(p) {:.unnumlist}

`    ;; Do the indexes with most pieces first`
!!!(p) {:.unnumlist}

`    (loop for n-pieces from 9 downto 1 do`
!!!(p) {:.unnumlist}

`      (map-edge-n-pieces`
!!!(p) {:.unnumlist}

`        #’(lambda (board index)`
!!!(p) {:.unnumlist}

`          (setf (aref *edge-table* index)`
!!!(p) {:.unnumlist}

`            (possible-edge-moves-value`
!!!(p) {:.unnumlist}

`              black board index)))`
!!!(p) {:.unnumlist}

`        black (initial-board) n-pieces top-edge 0))))`
!!!(p) {:.unnumlist}

The function `map-edge-n-pieces` iterates through all edge positions with a total of `n` pieces (of either color), applying a function to each such position.
It also keeps a running count of the edge index as it goes.
The function should accept two arguments: the board and the index.
Note that a single board can be used for all the positions because squares are reset after they are used.
The function has three cases: if the number of squares remaining is less than `n`, then it will be impossible to place `n` pieces on those squares, so we give up.
If there are no more squares then `n` must also be zero, so this is a valid position, and the function `fn` is called.
Otherwise we first try leaving the current square blank, then try filling it with player’s piece, and then with the opponent’s piece, in each case calling `map-edge-n-pieces` recursively.

[ ](#){:#l0255}`(defun map-edge-n-pieces (fn player board n squares index)`
!!!(p) {:.unnumlist}

`  "Call fn on all edges with n pieces."`
!!!(p) {:.unnumlist}

`  ;; Index counts 1 for player; 2 for opponent`
!!!(p) {:.unnumlist}

`  (cond`
!!!(p) {:.unnumlist}

`    ((< (length squares) n) nil)`
!!!(p) {:.unnumlist}

`    ((null squares) (funcall fn board index))`
!!!(p) {:.unnumlist}

`    (t (let ((index3 (* 3 index))`
!!!(p) {:.unnumlist}

`         (sq (first squares)))`
!!!(p) {:.unnumlist}

`      (map-edge-n-pieces fn player board n (rest squares) index3)`
!!!(p) {:.unnumlist}

`    (when (and (> n 0) (eql (bref board sq) empty))`
!!!(p) {:.unnumlist}

`        (setf (bref board sq) player)`
!!!(p) {:.unnumlist}

`        (map-edge-n-pieces fn player board (- n 1) (rest squares)`
!!!(p) {:.unnumlist}

`                (+1 index3))`
!!!(p) {:.unnumlist}

`        (setf (bref board sq) (opponent player))`
!!!(p) {:.unnumlist}

`        (map-edge-n-pieces fn player board (- n 1) (rest squares)`
!!!(p) {:.unnumlist}

`                (+2 index3))`
!!!(p) {:.unnumlist}

`        (setf (bref board sq) empty))))))`
!!!(p) {:.unnumlist}

The function `possible-edge-moves-value` searches through all possible moves to determine an edge value that is more accurate than a static evaluation.
It loops through every empty square on the edge, calling `possible-edge-move` to return a (*probability value*) pair.
Since it is also possible for a player not to make any move at all on an edge, the pair (`1.0`*current-value*) is also included.

[ ](#){:#l0260}`(defun possible-edge-moves-value (player board index)`
!!!(p) {:.unnumlist}

`  "Consider all possible edge moves.`
!!!(p) {:.unnumlist}

`  Combine their values into a single number."`
!!!(p) {:.unnumlist}

`  (combine-edge-moves`
!!!(p) {:.unnumlist}

`    (cons`
!!!(p) {:.unnumlist}

`      (list 1.0 (aref *edge-table* index)) ;; no move`
!!!(p) {:.unnumlist}

`      (loop for sq in top-edge ;; possible moves`
!!!(p) {:.unnumlist}

`        when (eql (bref board sq) empty)`
!!!(p) {:.unnumlist}

`        collect (possible-edge-move player board sq)))`
!!!(p) {:.unnumlist}

`    player))`
!!!(p) {:.unnumlist}

The value of each position is determined by making the move on the board, then looking up in the table the value of the resulting position for the opponent, and negating it (since we are interested in the value to us, not to our opponent).

[ ](#){:#l0265}`(defun possible-edge-move (player board sq)`
!!!(p) {:.unnumlist}

`  "Return a (prob val) pair for a possible edge move."`
!!!(p) {:.unnumlist}

`  (let ((new-board (replace (aref *ply-boards* player) board)))`
!!!(p) {:.unnumlist}

`    (make-move sq player new-board)`
!!!(p) {:.unnumlist}

`    (list (edge-move-probability player board sq)`
!!!(p) {:.unnumlist}

`      (- (aref *edge-table*`
!!!(p) {:.unnumlist}

`        (edge-index (opponent player)`
!!!(p) {:.unnumlist}

`          new-board top-edge))))))`
!!!(p) {:.unnumlist}

The possible moves are combined with `combine-edge-moves`, which sorts the moves best-first.
(Since `init-edge-table` started from black’s perspective, black tries to maximize and white tries to minimize scores.) We then go down the moves, increasing the total value by the value of each move times the probability of the move, and decreasing the remaining probability by the probability of the move.
Since there will always be a least one move (pass) with probability 1.0, this is guaranteed to converge.
In the end we round off the total value, so that we can do the run-time calculations with fixnums.

[ ](#){:#l0270}`(defun combine-edge-moves (possibilities player)`
!!!(p) {:.unnumlist}

`  "Combine the best moves."`
!!!(p) {:.unnumlist}

`  (let ((prob 1.0)`
!!!(p) {:.unnumlist}

`      (val 0.0)`
!!!(p) {:.unnumlist}

`      (fn (if (eql player black) #’> #’<)))`
!!!(p) {:.unnumlist}

`    (loop for pair in (sort possibilities fn :key #’second)`
!!!(p) {:.unnumlist}

`        while (>= prob 0.0)`
!!!(p) {:.unnumlist}

`        do (incf val (* prob (first pair) (second pair)))`
!!!(p) {:.unnumlist}

`          (decf prob (* prob (first pair))))`
!!!(p) {:.unnumlist}

`    (round val)))`
!!!(p) {:.unnumlist}

We still need to compute the probability that each possible edge move is legal.
These probabilities should reflect things such as the fact that it is easy to capture a corner if the opponent is in the adjacent X-square, and very difficult otherwise.
First we define some functions to recognize corner and X-squares and relate them to their neighbors:

[ ](#){:#l0275}`(let ((corner/xsqs ’((11 .
22) (18 .
27) (81.
72) (88 .
77))))`
!!!(p) {:.unnumlist}

`  (defun corner-p (sq) (assoc sq corner/xsqs))`
!!!(p) {:.unnumlist}

`  (defun x-square-p (sq) (rassoc sq corner/xsqs))`
!!!(p) {:.unnumlist}

`  (defun x-square-for (corner) (cdr (assoc corner corner/xsqs)))`
!!!(p) {:.unnumlist}

`  (defun corner-for (xsq) (car (rassoc xsq corner/xsqs))))`
!!!(p) {:.unnumlist}

Now we consider the probabilities.
There are four cases.
First, since we don’t know anything about the interior of the board, we assume each player has a 50% chance of being able to play in an X-square.
Second, if we can show that a move is legal (because it flips opponent pieces on the edge) then it has 100% probability.
Third, for the corner squares, we assign a 90% chance if the opponent occupies the X-square, 10% if it is empty, and only .1 % if we occupy it.
Otherwise, the probability is determined by the two neighboring squares: if a square is next to one or more opponents it is more likely we can move there; if it is next to our pieces it is less likely.
If it is legal for the opponent to move into the square, then the chances are cut in half (although we may still be able to move there, since we move first).

[ ](#){:#l0280}`(defun edge-move-probability (player board square)`
!!!(p) {:.unnumlist}

`  "What’s the probability that player can move to this square?"`
!!!(p) {:.unnumlist}

`  (cond`
!!!(p) {:.unnumlist}

`    ((x-square-p square) .5) ;; X-squares`
!!!(p) {:.unnumlist}

`    ((legal-p square player board) 1.0) ;; immediate capture`
!!!(p) {:.unnumlist}

`    ((corner-p square) ;; move to corner depends on X-square`
!!!(p) {:.unnumlist}

`    (let ((x-sq (x-square-for square)))`
!!!(p) {:.unnumlist}

`      (cond`
!!!(p) {:.unnumlist}

`        ((eql (bref board x-sq) empty) .1)`
!!!(p) {:.unnumlist}

`        ((eql (bref board x-sq) player) 0.001)`
!!!(p) {:.unnumlist}

`        (t .9))))`
!!!(p) {:.unnumlist}

`    (t (/ (aref`
!!!(p) {:.unnumlist}

`          '#2A((.l .4 .7)`
!!!(p) {:.unnumlist}

`            (.05 .3 *)`
!!!(p) {:.unnumlist}

`            (.01 * *))`
!!!(p) {:.unnumlist}

`          (count-edge-neighbors player board square)`
!!!(p) {:.unnumlist}

`          (count-edge-neighbors (opponent player) board square))`
!!!(p) {:.unnumlist}

`        (if (legal-p square (opponent player) board) 2 1)))))`
!!!(p) {:.unnumlist}

`(defun count-edge-neighbors (player board square)`
!!!(p) {:.unnumlist}

`  "Count the neighbors of this square occupied by player."`
!!!(p) {:.unnumlist}

`  (count-if #’(lambda (inc)`
!!!(p) {:.unnumlist}

`            (eql (bref board (+ square inc)) player))`
!!!(p) {:.unnumlist}

`        ’(+1 -1)))`
!!!(p) {:.unnumlist}

Now we return to the problem of determining the static value of an edge position.
This is computed by a weighted-squares metric, but the weights depend on the *stability* of each piece.
A piece is called stable if it cannot be captured, unstable if it is in immediate danger of being captured, and semistable otherwise.
A table of weights follows for each edge square and stability.
Note that corner squares are always stable, and X-squares we will call semistable if the adjacent corner is taken, and unstable otherwise.

[ ](#){:#l0285}`(defparameter *static-edge-table*`
!!!(p) {:.unnumlist}

`  '#2A(;stab semi    un`
!!!(p) {:.unnumlist}

`       (   *   0 -2000) ; X`
!!!(p) {:.unnumlist}

`       ( 700   *     *) ; corner`
!!!(p) {:.unnumlist}

`       (1200 200   -25) ; C`
!!!(p) {:.unnumlist}

`       (1000 200    75) ; A`
!!!(p) {:.unnumlist}

`       (1000 200    50) ; B`
!!!(p) {:.unnumlist}

`       (1000 200    50) ; B`
!!!(p) {:.unnumlist}

`       (1000 200    75) ; A`
!!!(p) {:.unnumlist}

`       (1200 200   -25) ; C`
!!!(p) {:.unnumlist}

`       ( 700   *     *) ; corner`
!!!(p) {:.unnumlist}

`       (   *   0 -2000) ; X`
!!!(p) {:.unnumlist}

`       ))`
!!!(p) {:.unnumlist}

The static evaluation then just sums each piece’s value according to this table:

[ ](#){:#l0290}`(defun static-edge-stability (player board)`
!!!(p) {:.unnumlist}

`  "Compute this edge’s static stability"`
!!!(p) {:.unnumlist}

`  (loop for sq in top-edge`
!!!(p) {:.unnumlist}

`      for i from 0`
!!!(p) {:.unnumlist}

`      sum (cond`
!!!(p) {:.unnumlist}

`          ((eql (bref board sq) empty) 0)`
!!!(p) {:.unnumlist}

`          ((eql (bref board sq) player)`
!!!(p) {:.unnumlist}

`           (aref *static-edge-table* i`
!!!(p) {:.unnumlist}

`              (piece-stability board sq)))`
!!!(p) {:.unnumlist}

`          (t (- (aref *static-edge-table* i`
!!!(p) {:.unnumlist}

`                 (piece-stability board sq)))))))`
!!!(p) {:.unnumlist}

The computation of stability is fairly complex.
It centers around finding the two “pieces,” `p1` and `p2`, which lay on either side of the piece in question and which are not of the same color as the piece.
These “pieces” may be empty, or they may be off the board.
A piece is unstable if one of the two is empty and the other is the opponent; it is semistable if there are opponents on both sides and at least one empty square to play on, or if it is surrounded by empty pieces.
Finally, if either `p1` or `p2` is nil then the piece is stable, since it must be connected by a solid wall of pieces to the corner.

[ ](#){:#l0295}`(let ((stable 0) (semi-stable 1) (unstable 2))`
!!!(p) {:.unnumlist}

`  (defun piece-stability (board sq)`
!!!(p) {:.unnumlist}

`    (cond`
!!!(p) {:.unnumlist}

`      ((corner-p sq) stable)`
!!!(p) {:.unnumlist}

`      ((x-square-p sq)`
!!!(p) {:.unnumlist}

`       (if (eql (bref board (corner-for sq)) empty)`
!!!(p) {:.unnumlist}

`         unstable semi-stable))`
!!!(p) {:.unnumlist}

`      (t (let* ((player (bref board sq))`
!!!(p) {:.unnumlist}

`             (opp (opponent player))`
!!!(p) {:.unnumlist}

`             (p1 (find player board :test-not #’eql`
!!!(p) {:.unnumlist}

`               :start sq :end 19))`
!!!(p) {:.unnumlist}

`             (p2 (find player board :test-not #’eql`
!!!(p) {:.unnumlist}

`               :start 11 :end sq`
!!!(p) {:.unnumlist}

`               :from-end t)))`
!!!(p) {:.unnumlist}

`         (cond`
!!!(p) {:.unnumlist}

`           ;; unstable pieces can be captured immediately`
!!!(p) {:.unnumlist}

`           ;; by playing in the empty square`
!!!(p) {:.unnumlist}

`           ((or (and (eql p1 empty) (eql p2 opp))`
!!!(p) {:.unnumlist}

`             (and (eql p2 empty) (eql p1 opp)))`
!!!(p) {:.unnumlist}

`            unstable)`
!!!(p) {:.unnumlist}

`           ;; semi-stable pieces might be captured`
!!!(p) {:.unnumlist}

`           ((and (eql p1 opp) (eql p2 opp)`
!!!(p) {:.unnumlist}

`             (find empty board :start 11 :end 19))`
!!!(p) {:.unnumlist}

`            semi-stable)`
!!!(p) {:.unnumlist}

`           ((and (eql p1 empty) (eql p2 empty))`
!!!(p) {:.unnumlist}

`            semi-stable)`
!!!(p) {:.unnumlist}

`           ;; Stable pieces can never be captured`
!!!(p) {:.unnumlist}

`           (t stable)))))))`
!!!(p) {:.unnumlist}

The edge table can now be built by a call to `init-edge-lable`.
After the table is built once, it is a good idea to save it so that we won’t need to repeat the initialization.
We could write simple routines to dump the table into a file and read it back in, but it is faster and easier to use existing tools that already do this job quite well: `compile-file` and `load`.
All we have to do is create and compile a file containing the single line:

[ ](#){:#l9015}`(setf *edge-table* '#.*edge-table*)`
!!!(p) {:.unnumlist}

The `#.` read macro evaluates the following expression at read time.
Thus, the compiler will see and compile the current edge table.
It will be able to store this more compactly and `load` it back in more quickly than if we printed the contents of the vector in decimal (or any other base).

### [ ](#){:#st0080}Combining the Factors
{:#s0080}
{:.h2hd}

Now we have a measure of the three factors: current mobility, potential mobility, and edge stability.
All that remains is to find a good way to combine them into a single evaluation metric.
The combination function used by [Rosenbloom (1982)](B9780080571157500285.xhtml#bb1000) is a linear combination of the three factors, but each factor’s coefficient is dependent on the move number.
Rosenbloom’s features are normalized to the range [-1000, 1000]; we normalize to the range [-1, 1] by doing a division after multiplying by the coefficient.
That allows us to use fixnuums for the coefficients.
Since our three factors are not calculated in quite the same way as Rosenbloom’s, it is not surprising that his coefficients are not the best for our program.
The edge coefficient was doubled and the potential coefficient cut by a factor of five.

[ ](#){:#l0300}`(defun Iago-eval (player board)`
!!!(p) {:.unnumlist}

`  "Combine edge-stability, current mobility and`
!!!(p) {:.unnumlist}

`  potential mobility to arrive at an evaluation."`
!!!(p) {:.unnumlist}

`  ;; The three factors are multiplied by coefficients`
!!!(p) {:.unnumlist}

`  ;; that vary by move number:`
!!!(p) {:.unnumlist}

`  (let ((c-edg(+ 312000 (* 6240 *move-number*)))`
!!!(p) {:.unnumlist}

`    (c-cur (if (< *move-number* 25)`
!!!(p) {:.unnumlist}

`      (+ 50000 (* 2000 *move-number*))`
!!!(p) {:.unnumlist}

`      (+ 75000 (* 1000 *move-number*))))`
!!!(p) {:.unnumlist}

`    (c-pot 20000))`
!!!(p) {:.unnumlist}

`  (multiple-value-bind (p-cur p-pot)`
!!!(p) {:.unnumlist}

`      (mobility player board)`
!!!(p) {:.unnumlist}

`    (multiple-value-bind (o-cur o-pot)`
!!!(p) {:.unnumlist}

`        (mobility (opponent player) board)`
!!!(p) {:.unnumlist}

`      ;; Combine the three factors into one sum:`
!!!(p) {:.unnumlist}

`      (+ (round (* c-edg (edge-stability player board)) 32000)`
!!!(p) {:.unnumlist}

`          (round (* c-cur (- p-cur o-cur)) (+ p-cur o-cur 2))`
!!!(p) {:.unnumlist}

`          (round (* c-pot (- p-pot o-pot)) (+ p-pot o-pot 2)))))))`
!!!(p) {:.unnumlist}

Finally, we are ready to code the `Iago` function.
Given a search depth, `Iago` returns a strategy that will do alpha-beta search to that depth using the `Iago-eval` evaluation function.
This version of Iago was able to defeat the modified weighted-squares strategy in 8 of 10 games at 3 ply, and 9 of 10 at 4 ply.
On an Explorer II, 4-ply search takes about 20 seconds per move.
At 5 ply, many moves take over a minute, so the program runs the risk of forfeiting.
At 3 ply, the program takes only a few seconds per move, but it still was able to defeat the author in five straight games, by scores of 50-14, 64-0, 51-13, 49-15 and 36-28.
Despite these successes, it is likely that the evaluation function could be improved greatly with a little tuning of the parameters.

[ ](#){:#l0305}`(defun Iago (depth)`
!!!(p) {:.unnumlist}

`  "Use an approximation of Iago’s evaluation function."`
!!!(p) {:.unnumlist}

`  (alpha-beta-searcher3 depth #’iago-eval))`
!!!(p) {:.unnumlist}

## [ ](#){:#st0085}18.13 Other Techniques
{:#s0085}
{:.h1hd}

There are many other variations that can be tried to speed up the search and improve play.
Unfortunately, choosing among the techniques is a bit of a black art.
You will have to experiment to find the combination that is best for each domain and each evaluation function.
Most of the following techniques were incorporated, or at least considered and rejected, in Bill.

### [ ](#){:#st0090}Iterative Deepening
{:#s0090}
{:.h2hd}

We have seen that the average branching factor for Othello is about 10.
This means that searching to depth *n* + 1 takes roughly 10 times longer than search to depth *n*.
Thus, we should be willing to go to a lot of overhead before we search one level deeper, to assure two things: that search will be done efficiently, and that we won’t forfeit due to running out of time.
A by-now familiar technique, iterative deepening (see [chapters 6](B9780080571157500066.xhtml) and [14](B9780080571157500145.xhtml)), serves both these goals.

Iterative deepening is used as follows.
The strategy determines how much of the remaining time to allocate to each move.
A simple strategy could allocate a constant amount of time for each move, and a more sophisticated strategy could allocate more time for moves at crucial points in the game.
Once the time allocation is determined for a move, the strategy starts an iterative deepening alpha-beta search.
There are two complications: First, the search at *n* ply keeps track of the best moves, so that the search at *n* + 1 ply will have better ordering information.
In many cases it will be faster to do both the *n* and *n* + 1 ply searches with the ordering information than to do only the *n* + 1 ply search without it.
Second, we can monitor how much time has been taken searching each ply, and cut off the search when searching one more ply would exceed the allocated time limit.
Thus, iterative-deepening search degrades gracefully as time limits are imposed.
It will give a reasonable answer even with a short time allotment, and it will rarely exceed the allotted time.

### [ ](#){:#st0095}Forward Pruning
{:#s0095}
{:.h2hd}

One way to cut the number of positions searched is to replace the legal move generator with a *plausible* move generator: in other words, only consider good moves, and never even look at moves that seem clearly bad.
This technique is called *forward pruning*.
It has fallen on disfavor because of the difficulty in determining which moves are plausible.
For most games, the factors that would go into a plausible move generator would be duplicated in the static evaluation function anyway, so forward pruning would require more effort without much gain.
Worse, forward pruning could rule out a brilliant sacrifice–a move that looks bad initially but eventually leads to a gain.

For some games, forward pruning is a necessity.
The game of Go, for example, is played on a 19 by 19 board, so the first player has 361 legal moves, and a 6-ply search would involve over 2 quadrillion positions.
However, many good Go programs can be viewed as not doing forward pruning but doing abstraction.
There might be 30 empty squares in one portion of the board, and the program would treat a move to any of these squares equivalently.

Bill uses forward pruning in a limited way to rule out certain moves adjacent to the corners.
It does this not to save time but because the evaluation function might lead to such a move being selected, even though it is in fact a poor move.
In other words, forward pruning is used to correct a bug in the evaluation function cheaply.

### [ ](#){:#st0100}Nonspeculative Forward Pruning
{:#s0100}
{:.h2hd}

This technique makes use of the observation that there are limits in the amount the evaluation function can change from one position to the next.
For example, if we are using the count difference as the evaluation function, then the most a move can change the evaluation is +37 (one for placing a piece in the corner, and six captures in each of the three directions).
The smallest change is 0 (if the player is forced to pass).
Thus, if there are 2 ply left in the search, and the backed-up value of position *A* has been established as 38 points better than the static value of position *B*, then it is useless to expand position *B*.
This assumes that we are evaluating every position, perhaps to do sorted ordering or iterative deepening.
It also assumes that no position in the search tree is a final position, because then the evaluation could change by more than 37 points.
In conclusion, it seems that nonspeculative forward pruning is not very useful for Othello, although it may play a rôle in other games.

### [ ](#){:#st0105}Aspiration Search
{:#s0105}
{:.h2hd}

Alpha-beta search is initated with the `achievable` and `cutoff` boundaries set to `losing-value` and `winning-value`, respectively.
In other words, the search assumes nothing: the final position may be anything from a loss to a win.
But suppose we are in a situation somewhere in the mid-game where we are winning by a small margin (say the static evaluation for the current position is 50).
In most cases, a single move will not change the evaluation by very much.
Therefore, if we invoked the alpha-beta search with a window defined by boundaries of, say, 0 and 100, two things can happen: if the actual backed-up evaluation for this position is in fact in the range 0 to 100, then the search will find it, and it will be found quickly, because the reduced window will cause more pruning.
If the actual value is not in the range, then the value returned will reflect that, and we can search again using a larger window.
This is called aspiration search, because we aspire to find a value within a given window.
If the window is chosen well, then often we will succeed and will have saved some search time.

[Pearl (1984)](B9780080571157500285.xhtml#bb0930) suggests an alternative called zero-window search.
At each level, the first possible move, which we’ll call *m*, is searched using a reasonably wide window to determine its exact value, which we’ll call *v*.
Then the remaining possible moves are searched using *v* as both the lower and upper bounds of the window.
Thus, the result of the search will tell if each subsequent move is better or worse than *m*, but won’t tell how much better or worse.
There are three outcomes for zero-window search.
If no move turns out to be better than *m*, then stick with *m*.
If a single move is better, then use it.
If several moves are better than *m*, then they have to be searched again using a wider window to determine which is best.

There is always a trade-off between time spent searching and information gained.
Zero-window search makes an attractive trade-off: we gain some search time by losing information about the value of the best move.
We are still guaranteed of finding the best move, we just don’t know its exact value.

Bill’s zero-window search takes only 63% of the time taken by full alpha-beta search.
It is effective because Bill’s move-ordering techniques ensure that the first move is often best.
With random move ordering, zero-window search would not be effective.

### [ ](#){:#st0110}Think-Ahead
{:#s0110}
{:.h2hd}

A program that makes its move and then waits for the opponent’s reply is wasting half the time available to it.
A better use of time is to compute, or *think-ahead* while the opponent is moving.
Think-ahead is one factor that helps Bill defeat Iago.
While many programs have done think-ahead by choosing the most likely move by the opponent and then starting an iterative-deepening search assuming that move, Bill’s algorithm is somewhat more complex.
It can consider more than one move by the opponent, depending on how much time is available.

### [ ](#){:#st0115}Hashing and Opening Book Moves
{:#s0115}
{:.h2hd}

We have been treating the search space as a tree, but in general it is a directed acyclic graph (dag): there may be more than one way to reach a particular position, but there won’t be any loops, because every move adds a new piece.
This raises the question we explored briefly in [section 6.4](B9780080571157500066.xhtml#s0025): should we treat the search space as a tree or a graph?
By treating it as a graph we eliminate duplicate evaluations, but we have the overhead of storing all the previous positions, and of checking to see if a new position has been seen before.
The decision must be based on the proportion of duplicate positions that are actually encountered in play.
One compromise solution is to store in a hash table a partial encoding of each position, encoded as, say, a single fixnum (one word) instead of the seven or so words needed to represent a full board.
Along with the encoding of each position, store the move to try first.
Then, for each new position, look in the hash table, and if there is a hit, try the corresponding move first.
The move may not even be legal, if there is an accidental hash collision, but there is a good chance that the move will be the right one, and the overhead is low.

One place where it is clearly worthwhile to store information about previous positions is in the opening game.
Since there are fewer choices in the opening, it is a good idea to compile an opening “book” of moves and to play by it as long as possible, until the opponent makes a move that departs from the book.
Book moves can be gleaned from the literature, although not very much has been written about Othello (as compared to openings in chess).
However, there is a danger in following expert advice: the positions that an expert thinks are advantageous may not be the same as the positions from which our program can play well.
It may be better to compile the book by playing the program against itself and determining which positions work out best.

### [ ](#){:#st0120}The End Game
{:#s0120}
{:.h2hd}

It is also a good idea to try to save up time in the midgame and then make an all-out effort to search the complete game tree to completion as soon as feasible.
Bill can search to completion from about 14 ply out.
Once the search is done, of course, the most promising lines of play should be saved so that it won’t be necessary to solve the game tree again.

### [ ](#){:#st0125}Metareasoning
{:#s0125}
{:.h2hd}

If it weren’t for the clock, Othello would be a trivial game: just search the complete game tree all the way to the end, and then choose the best move.
The clock imposes a complication: we have to make all our moves before we run out of time.
The algorithms we have seen so far manage the clock by allocating a certain amount of time to each move, such that the total time is guaranteed (or at least very likely) to be less than the allotted time.
This is a very crude policy.
A finer-grained way of managing time is to consider computation itself as a possible move.
That is, at every tick of the clock, we need to decide if it is better to stop and play the best move we have computed so far or to continue and try to compute a better move.
It will be better to compute more only in the case where we eventually choose a better move; it will be better to stop and play only in the case where we would otherwise forfeit due to time constraints, or be forced to make poor choices later in the game.
An algorithm that includes computation as a possible move is called a metareasoning system, because it reasons about how much to reason.

[Russell and Wefald (1989)](B9780080571157500285.xhtml#bb1025) present an approach based on this view.
In addition to an evaluation function, they assume a variance function, which gives an estimate of how much a given position’s true value is likely to vary from its static value.
At each step, their algorithm compares the value and variance of the best move computed so far and the second best move.
If the best move is clearly better than the second best (taking variance into account), then there is no point Computing any more.
Also, if the top two moves have similar values but both have very low variance, then Computing will not help much; we can just choose one of the two at random.

For example, if the board is in a symmetric position, then there may be two symmetric moves that will have identical value.
By searching each move’s subtree more carefully, we soon arrive at a low variance for both moves, and then we can choose either one, without searching further.
Of course, we could also add special-case code to check for symmetry, but the metareasoning approach will work for nonsymmetric cases as well as symmetric ones.
If there is a situation where two moves both lead to a clear win, it won’t waste time choosing between them.

The only situation where it makes sense to continue Computing is when there are two moves with high variance, so that it is uncertain if the true value of one exceeds the other.
The metareasoning algorithm is predicated on devoting time to just this case.

### [ ](#){:#st0130}Learning
{:#s0130}
{:.h2hd}

From the earliest days of computer game playing, it was realized that a championship program would need to learn to improve itself.
[Samuel (1959)](B9780080571157500285.xhtml#bb1040) describes a program that plays checkers and learns to improve its evaluation function.
The evaluation function is a linear combination of features, such as the number of pieces for each player, the number of kings, the number of possible forks, and so on.
Learning is done by a hill-climbing search procedure: change one of the coefficients for one of the features at random, and then see if the changed evaluation function is better than the original one.

Without some guidance, this hill-climbing search would be very slow.
First, the space is very large–Samuel used 38 different features, and although he restricted the coefficients to be a power of two between 0 and 20, that still leaves 2138 possible evaluation functions.
Second, the obvious way of determining the relative worth of two evaluation functions–playing a series of games between them and seeing which wins more often–is quite time-consuming.

Fortunately, there is a faster way of evaluating an evaluation function.
We can apply the evaluation function to a position and compare this static value with the backed-up value determined by an alpha-beta search.
If the evaluation function is accurate, the static value should correlate well with the backed-up value.
If it does not correlate well, the evaluation function should be changed in such a way that it does.
This approach still requires the trial-and-error of hill-climbing, but it will converge much faster if we can gain information from every position, rather than just from every game.

In the past few years there has been increased interest in learning by a process of guided search.
*Neural nets* are one example of this.
They have been discussed elsewhere.
Another example is *genetic learning* algorithms.
These algorithms start with several candidate solutions.
In our case, each candidate would consist of a set of coefficients for an evaluation function.
On each generation, the genetic algorithm sees how well each candidate does.
The worst candidates are eliminated, and the best ones “mate” and “reproduce”–two candidates are combined in some way to yield a new one.
If the new offspring has inherited both its parents’ good points, then it will prosper; if it has inherited both its parents’ bad points, then it will quickly die out.
Either way, the idea is that natural selection will eventually yield a high-quality solution.
To increase the chances of this, it is a good idea to allow for mutations: random changes in the genetic makeup of one of the candidates.

## [ ](#){:#st0135}18.14 History and References
{:#s0135}
{:.h1hd}

[Lee and Mahajan (1986,](B9780080571157500285.xhtml#bb0710)[1990)](B9780080571157500285.xhtml#bb0715)present the current top Othello program, Bill.
Their description outlines all the techniques used but does not go into enough detail to allow the reader to reconstruct the program.
Bill is based in large part on Rosenbloom’s Iago program.
Rosenbloom’s article (1982) is more thorough.
The presentation in this chapter is based largely on this article, although it also contains some ideas from Bill and from other sources.

The journal *Othello Quarterly* is the definitive source for reports on both human and computer Othello games and strategies.

The most popular game for computer implementation is chess.
[Shannon (1950a,](B9780080571157500285.xhtml#bb1070)[b)](B9780080571157500285.xhtml#bb1075) speculated that a computer might play chess.
In a way, this was one of the boldest steps in the history of AI.
Today, writing a chess program is a challenging but feasible project for an undergraduate.
But in 1950, even suggesting that such a program might be possible was a revolutionary step that changed the way people viewed these arithmetic calculating devices.
Shannon introduced the ideas of a game tree search, minimaxing, and evaluation functions–ideas that remain intact to this day.
[Marsland (1990)](B9780080571157500285.xhtml#bb0770) provides a good short introduction to computer chess, and David Levy has two books on the subject (1976, 1988).
It was Levy, an international chess master, who in 1968 accepted a bet from John McCarthy, Donald Michie, and others that a computer chess program would not beat him in the next ten years.
Levy won the bet.
Levy’s *Heuristic Programming* (1990) and *Computer Games* (1988) cover a variety of computer game playing programs.
The studies by [DeGroot (1965,](B9780080571157500285.xhtml#bb0305)[1966)](B9780080571157500285.xhtml#bb0310) give a fascinating insight into the psychology of chess masters.

[Knuth and Moore (1975)](B9780080571157500285.xhtml#bb0630) analyze the alpha-beta algorithm, and Pearl’s book *Heuristics* (1984) covers all kinds of heuristic search, games included.

[Samuel (1959)](B9780080571157500285.xhtml#bb1040) is the classic work on learning evaluation function parameters.
It is based on the game of checkers.
[Lee and Mahajan (1990)](B9780080571157500285.xhtml#bb0715) present an alternative learning mechanism, using Bayesian classification to learn an evaluation function that optimally distinguishes winning positions from losing positions.
Genetic algorithms are discussed by L.
[Davis (1987,](B9780080571157500285.xhtml#bb0280)[1991)](B9780080571157500285.xhtml#bb0285) and [Goldberg (1989)](B9780080571157500285.xhtml#bb0480).

## [ ](#){:#st0140}18.15 Exercises
{:#s0140}
{:.h1hd}

**Exercise 18.3 [s]** How many different Othello positions are there?
Would it be feasible to store the complete game tree and thus have a perfect player?

**Exercise 18.4 [m]** At the beginning of this chapter, we implemented pieces as an enumerated type.
There is no built-in facility in Common Lisp for doing this, so we had to introduce a series of `defconstant` forms.
Define a macro for defining enumerated types.
What else should be provided besides the constants?

**Exercise 18.5 [h]** Add fixnum and speed declarations to the Iago evaluation function and the alpha-beta code.
How much does this speed up Iago?
What other efficiency measures can you take?

**Exercise 18.6 [h]** Implement an iterative deepening search that allocates time for each move and checks between each iteration if the time is exceeded.

**Exercise 18.7 [h]** Implement zero-window search, as described in [section 18.13](#s0085).

**Exercise 18.8 [d]** Read the references on Bill ([Lee and Mahajan 1990](B9780080571157500285.xhtml#bb0715), and [1986](B9780080571157500285.xhtml#bb0710) if you can get it), and reimplement Bill’s evaluation function as best you can, using the table-based approach.
It will also be helpful to read [Rosenbloom 1982](B9780080571157500285.xhtml#bb1000).

**Exercise 18.9 [d]** Improve the evaluation function by tuning the parameters, using one of the techniques described in [section 18.13](#s0085).

**Exercise 18.10 [h]** Write move-generation and evaluation functions for another game, such as chess or checkers.

## [ ](#){:#st0145}18.16 Answers
{:#s0145}
{:.h1hd}

**Answer 18.2** The `weighted-squares` strategy wins the first game by 20 pieces, but when `count-difference` plays first, it captures all the pieces on its fifth move.
These two games alone are not enough to determine the best strategy; the function `othello-series` on [page 626](#p626) shows a better comparison.

**Answer 18.3** 364 = 3, 433, 683, 820, 292, 512, 484, 657, 849, 089, 281.
No.

**Answer 18.4** Besides the constants, we provide a `deftype` for the type itself, and conversion routines between integers and symbols:

[ ](#){:#l0310}`(defmacro define-enumerated-type (type &rest elements)`
!!!(p) {:.unnumlist}

`  "Represent an enumerated type with integers 0-n."`
!!!(p) {:.unnumlist}

`  ’(progn`
!!!(p) {:.unnumlist}

`    (deftype ,type () ’(integer 0 , (- (length elements) 1)))`
!!!(p) {:.unnumlist}

`    (defun ,(symbol type ’->symbol) (,type)`
!!!(p) {:.unnumlist}

`      (elt ’,elements ,type))`
!!!(p) {:.unnumlist}

`    (defun ,(symbol ’symbol-> type) (symbol)`
!!!(p) {:.unnumlist}

`      (position symbol ’,elements))`
!!!(p) {:.unnumlist}

`    ,@(loop for element in elements`
!!!(p) {:.unnumlist}

`        for i from 0`
!!!(p) {:.unnumlist}

`        collect ’(defconstant ,element ,i))))`
!!!(p) {:.unnumlist}

Here’s how the macro would be used to define the piece data type, and the code produced:

[ ](#){:#l0315}`> (macroexpand`
!!!(p) {:.unnumlist}

`    '(define-enumerated-type piece`
!!!(p) {:.unnumlist}

`      empty black white outer))`
!!!(p) {:.unnumlist}

`(PROGN`
!!!(p) {:.unnumlist}

`  (DEFTYPE PIECE () '(INTEGER 0 3))`
!!!(p) {:.unnumlist}

`  (DEFUN PIECE->SYMBOL (PIECE)`
!!!(p) {:.unnumlist}

`    (ELT '(EMPTY BLACK WHITE OUTER) PIECE))`
!!!(p) {:.unnumlist}

`  (DEFUN SYMBOL->PIECE (SYMBOL)`
!!!(p) {:.unnumlist}

`    (POSITION SYMBOL '(EMPTY BLACK WHITE OUTER)))`
!!!(p) {:.unnumlist}

`  (DEFCONSTANT EMPTY 0)`
!!!(p) {:.unnumlist}

`  (DEFCONSTANT BLACK 1)`
!!!(p) {:.unnumlist}

`  (DEFCONSTANT WHITE 2)`
!!!(p) {:.unnumlist}

`  (DEFCONSTANT OUTER 3))`
!!!(p) {:.unnumlist}

A more general facility would, like `defstruct`, provide for several options.
For example, it might allow for a documentation string for the type and each constant, and for a : `conc-name`, so the constants could have names like `piece-empty` instead of `empty`.
This would avoid conflicts with other types that wanted to use the same names.
The user might also want the ability to start the values at some number other than zero, or to assign specific values to some of the symbols.

----------------------

[1](#xfn0015){:#np0015} Othello is a registered trademark of CBS Inc.
Gameboard design © 1974 CBS Inc.
!!!(p) {:.ftnote1}

[2](#xfn0020){:#np0020}*Othello,* [I.
i.
117] William Shakespeare.
!!!(p) {:.ftnote1}

[3](#xfn0025){:#np0025} Remember, when a constant is redefined, it may be necessary to recompile any functions that use the constant.
!!!(p) {:.ftnote1}

