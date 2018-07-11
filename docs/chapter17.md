# Chapter 17
## Line-Diagram Labeling by Constraint Satisfaction

> It is wrong to think of Waltz's work only as a statement of the epistemology of line drawings of polyhedra.
Instead I think it is an elegant case study of a paradigm we can expect to see again and again.

> —Patrick Winston

> The Psychology of Computer Vision (1975)

This book touches only the areas of AI that deal with abstract reasoning.
There is another side of AI, the field of *robotics,* that deals with interfacing abstract reasoning with the real world through sensors and motors.
A robot receives input from cameras, microphones, sonar, and touch-sensitive devices, and produces “ouput” by moving its appendages or generating sounds.
The real world is a messier place than the abstract worlds we have been covering.
A robot must deal with noisy data, faulty components, and other agents and events in the world that can affect changes in the environment.

Computer vision is the subfield of robotics that deals with interpreting visual information.
Low-level vision takes its input directly from a camera and detects lines, regions and textures.
We will not be concerned with this.
High-level vision uses the findings of the low-level component to build a three-dimensional model of the objects depicted in the scene.
This chapter covers one small aspect of high-level vision.

## [ ](#){:#st0010}17.1 The Line-Labeling Problem
{:#s0010}
{:.h1hd}

In this chapter we look at the line-diagram labeling problem: Given a list of lines and the vertexes at which they intersect, how can we determine what the lines represent?
For example, given the nine lines in [figure 17.1](#f0010), how can we interpret the diagram as a cube?

![f17-01-9780080571157](images/B9780080571157500170/f17-01-9780080571157.jpg)     
Figure 17.1
!!!(span) {:.fignum}
A Cube
Before we can arrive at an interpretation, we have to agree on what the candidates are.
After all, [figure 17.1](#f0010) could be just a hexagon with three lines in the middle.
For the purposes of this chapter, we will consider only diagrams that depict one or more *polyhedra—*three-dimensional solid figures whose surfaces are flat faces bounded by straight lines.
In addition, we will only allow *trihedral* vertexes.
That is, each vertex must be formed by the intersection of three faces, as in the corner of a cube, where the top, front, and side of the cube come together.
A third restriction on diagrams is that no so-called *accidental* vertexes are allowed.
For example, [figure 17.1](#f0010) might be a picture of three different cubes hanging in space, which just happen to line up so that the edge of one is aligned with the edge of another from our viewpoint.
We will assume that this is not the case.

Given a diagram that fits these three restrictions, our goal is to identify each line, placing it in one of three classes:

[ ](#){:#l0010}1. A convex line separates two visible faces of a polyhedron such that a line from one face to the other would lie inside the polyhedron.
It will be marked with a plus sign:+.
!!!(p) {:.numlist}

2. A concave line separates two faces of two polyhedra such that a line between the two spaces would pass through empty space.
It will be marked with a minus sign:−.
!!!(p) {:.numlist}

3. A boundary line denotes the same physical situation as a convex line, but the diagram is oriented in such a way that only one of the two faces of the polyhedron is visible.
Thus, the line marks the boundary between the polyhedron and the background.
It will be marked with an arrow:→.
Traveling along the line from the tail to the point of the arrow, the polyhedron is on the right, and the background is on the left.
!!!(p) {:.numlist}

[Figure 17.2](#f0015) shows a labeling of the cube using these conventions.
Vertex A is the near corner of the cube, and the three lines coming out of it are all convex lines.
Lines GD and DF are concave lines, indicating the junction between the cube and the surface on which it is resting.
The remaining lines are boundary lines, indicating that there is no physical connection between the cube and the background there, but that there are other sides of the cube that cannot be seen.

![f17-02-9780080571157](images/B9780080571157500170/f17-02-9780080571157.jpg)     
Figure 17.2
!!!(span) {:.fignum}
A Line-labeled Cube
The line-labeling technique developed in this chapter is based on a simple idea.
First we enumerate all the possible vertexes, and all the possible labelings for each vertex.
It turns out there are only four different vertex types in the trihedral polygon world.
We call them L, Y, W, and T vertexes, because of their shape.
The Y and W vertexes are also known as forks and arrows, respectively.
The vertexes are listed in [figure 17.3](#f0020).
Each vertex imposes some constraints on the lines that compose it.
For example, in a W vertex, the middle line can be labeled with a + or −, but not with an arrow.

![f17-03-9780080571157](images/B9780080571157500170/f17-03-9780080571157.jpg)     
Figure 17.3
!!!(span) {:.fignum}
The Possible Vertexes and Labels
Each line connects two vertexes, so it must satisfy both constraints.
This suggests a simple algorithm for labeling a diagram based on constraint propagation: First, label each vertex with all the possible labelings for the vertex type.
An L vertex has six possibilities, Y has five, T has four, and W has three.
Next, pick a vertex, V.
Consider a neighboring vertex, N (that is, N and V are connected by a line).
N will also have a set of possible labelings.
If N and V agree on the possible labelings for the line between them, then we have gained nothing.
But if the intersection of the two possibility sets is smaller than V's possibility set, then we have found a constraint on the diagram.
We adjust N and V's possible labelings accordingly.
Every time we add a constraint at a vertex, we repeat the whole process for all the neighboring vertexes, to give the constraint a chance to propagate as far as possible.
When every vertex has been visited at least once and there are no more constraints to propagate, then we are done.

[Figure 17.4](#f0025) illustrâtes this process.
On the left we start with a cube.
All vertexes have all possible labelings, except that we know line GD is concave (−), indicating that the cube is resting on a surface.
This constrains vertex D in such a way that line DA must be convex (+).
In the middle picture the constraint on vertex D has propagated to vertex A, and in the right-hand picture it propagates to vertex B.
Soon, the whole cube will be uniquely labeled.

![f17-04-9780080571157](images/B9780080571157500170/f17-04-9780080571157.jpg)     
Figure 17.4
!!!(span) {:.fignum}
Propagating Constraints
Many diagrams will be labeled uniquely by this constraint propagation process.
Some diagrams, however, are ambiguous.
They will still have multiple labelings after constraint propagation has finished.
In this case, we can search for a solution.
Simply choose an ambiguous vertex, choose one of the possible labelings for that vertex, and repeat the constraint propagation/search process.
Keep going until the diagram is either unambiguous or inconsistent.

That completes the sketch of the line-labeling algorithm.
We are now ready to implement a labeling program.
It's glossary is in [figure 17.5](#f0030).

![f17-05-9780080571157](images/B9780080571157500170/f17-05-9780080571157.jpg)     
Figure 17.5
!!!(span) {:.fignum}
Glossary for the Line-Labeling Program
The two main data structures are the di agram and the vertex.
It would have been possible to implement a data type for `lines`, but it is not necessary: lines are defined implicitly by the two vertexes at their end points.

A diagram is completely specified by its list of vertexes, so the structure di agram needs only one slot.
A vertex, on the other hand, is a more complex structure.
Each vertex has an identifying name (usually a single letter), a vertex type (L, Y, W, or T), a list of neighboring vertexes, and a list of possible labelings.
A labeling is a list of line labels.
For example, a Y vertex will initially have a list of five possible labelings.
If it is discovered that the vertex is the interior of a concave corner, then it will have the single labeling ( - - - ).
We give type information on the slots of vertex because it is a complicated data type.
The syntax of defstruct is such that you cannot specify a : type without first specifying a default value.
We chose L as the default value for the type slot at random, but note that it would have been an error to give `nil` as the default value, because `nil` is not of the right type.

[ ](#){:#l0015}`(defstruct diagram "A diagram is a list of vertexes." vertexes)`
!!!(p) {:.unnumlist}

`(defstruct (vertex (:print-function print-vertex))`
!!!(p) {:.unnumlist}

` (name   nil :type atom)`
!!!(p) {:.unnumlist}

` (type    'L :type (member L Y W T))`
!!!(p) {:.unnumlist}

` (neighbors nil :type list) ; of vertex`
!!!(p) {:.unnumlist}

` (labelings nil :type list)) ; of lists of (member + − L R)))))`
!!!(p) {:.unnumlist}

An ambiguous vertex will have several labelings, while an unambiguous vertex has exactly one, and a vertex with no labelings indicates an impossible diagram.
Initially we don't know which vertexes are what, so they all start with several possible labelings.
Note that a labeling is a list, not a set: the order of the labels is significant and matches the order of the neighboring vertexes.
The function possible-labelings gives a list of all possible labelings for each vertex type.
We use R and L instead of arrows as labels, because the orientation of the arrows is significant.
An R means that as you travel from the vertex to its neighbor, the polyhedron is on the right and the background object is on the left.
Thus, an R is equivalent to an arrow pointing away from the vertex.
The L is just the reverse.

[ ](#){:#l0020}`(defun ambiguous-vertex-p (vertex)`
!!!(p) {:.unnumlist}

` "A vertex is ambiguous if it has more than one labeling."`
!!!(p) {:.unnumlist}

` (> (number-of-labelings vertex) 1))`
!!!(p) {:.unnumlist}

`(defun number-of-labelings (vertex)`
!!!(p) {:.unnumlist}

` (length (vertex-labelings vertex)))`
!!!(p) {:.unnumlist}

`(defun impossible-vertex-p (vertex)`
!!!(p) {:.unnumlist}

` "A vertex is impossible if it has no labeling."`
!!!(p) {:.unnumlist}

` (null (vertex-labelings vertex)))`
!!!(p) {:.unnumlist}

`(defun impossible-diagram-p (diagram)`
!!!(p) {:.unnumlist}

` "An impossible diagram is one with an impossible vertex."`
!!!(p) {:.unnumlist}

` (some #'impossible-vertex-p (diagram-vertexes diagram)))`
!!!(p) {:.unnumlist}

`(defun possible-labelings (vertex-type)\`
!!!(p) {:.unnumlist}

` "The list of possible labelings for a given vertex type."`
!!!(p) {:.unnumlist}

` ;; In these labelings, R means an arrow pointing away from`
!!!(p) {:.unnumlist}

` ;; the vertex, L means an arrow pointing towards it.`
!!!(p) {:.unnumlist}

`  (case vertex-type`
!!!(p) {:.unnumlist}

`  ((L) '((R L) (L R) (+ R) (L +) (- L) (R -)))`
!!!(p) {:.unnumlist}

`  ((Y) '((+ + +) ( ) (L R -) (− L R) (R - L)))`
!!!(p) {:.unnumlist}

`  ((T) '((R L +) (R L -) (R L L) (R L R)))`
!!!(p) {:.unnumlist}

`  ((W) '((L R +) (− − +) (+ + −)))))`
!!!(p) {:.unnumlist}

## [ ](#){:#st0015}17.2 Combining Constraints and Searching
{:#s0015}
{:.h1hd}

The main function `print-labelings` takes a diagram as input, reduces the number of labelings on each vertex by constraint propagation, and then searches for all consistent interpretations.
Output is printed before and after each step.

[ ](#){:#l0025}`(defun print-labelings (diagram)`
!!!(p) {:.unnumlist}

` "Label the diagram by propagating constraints and then`
!!!(p) {:.unnumlist}

` searching for solutions if necessary.
Print results."`
!!!(p) {:.unnumlist}

` (show-diagram diagram "~&The initial diagram is:")`
!!!(p) {:.unnumlist}

` (every #'propagate-constraints (diagram-vertexes diagram))`
!!!(p) {:.unnumlist}

` (show-diagram diagram`
!!!(p) {:.unnumlist}

`       "~2&After constraint propagation the diagram is:")`
!!!(p) {:.unnumlist}

` (let* ((solutions (if (impossible-diagram-p diagram)`
!!!(p) {:.unnumlist}

`         nil`
!!!(p) {:.unnumlist}

`         (search-solutions diagram)))`
!!!(p) {:.unnumlist}

`    (n (length solutions)))`
!!!(p) {:.unnumlist}

`  (unless (= n 1)`
!!!(p) {:.unnumlist}

`   (format t "~2&There are ~ r solution~:p:" n)`
!!!(p) {:.unnumlist}

`   (mapc #'show-diagram solutions)))`
!!!(p) {:.unnumlist}

` (values))`
!!!(p) {:.unnumlist}

The function `propagate-constraints` takes a vertex and considers the constraints imposed by neighboring vertexes to get a list of all the `consistent-labelings` for the vertex.
If the number of consistent labelings is less than the number before we started, then the neighbors' constraints have had an effect on this vertex, so we propagate the new-found constraints on this vertex back to each neighbor.
The function returns nil and thus immediately stops the propagation if there is an impossible vertex.
Otherwise, propagation continues until there are no more changes to the labelings.

The whole propagation algorithm is started by a call to `every in print−labelings,` which propagates constraints from each vertex in the diagram.
But it is not obvious that this is all that is required.
After propagating from each vertex once, couldn't there be another vertex that needs relabeling?
The only vertex that could possibly need relabeling would be one that had a neighbor changed since its last update.
But any such vertex would have been visited by `propagate-constraint,` since we propagate to all neighbors.
Thus, a single pass through the vertexes, compounded with recursive calls, will find and apply all possible constraints.

The next question worth asking is if the algorithm is guaranteed to terminate.
Clearly, it is, because `propagate-constraints` can only produce recursive calls when it removes a labeling.
But since there are a finite number of labelings initially (no more than six per vertex), there must be a finite number of calls to `propagate-constraints.`

[ ](#){:#l0030}`(defun propagate-constraints (vertex)`
!!!(p) {:.unnumlist}

` "Reduce the labelings on vertex by considering neighbors.`
!!!(p) {:.unnumlist}

` If we can reduce, propagate the constraints to each neighbor.`
!!!(p) {:.unnumlist}

` ;; Return nil only when the constraints lead to an impossibility`
!!!(p) {:.unnumlist}

` (let ((old-num (number-of-labelings vertex)))`
!!!(p) {:.unnumlist}

`  (setf (vertex-labelings vertex) (consistent-labelings vertex))`
!!!(p) {:.unnumlist}

`  (unless (impossible-vertex-p vertex)`
!!!(p) {:.unnumlist}

`   (when (< (number-of-labelings vertex) old-num)`
!!!(p) {:.unnumlist}

`    (every #'propagate-constraints (vertex-neighbors vertex)))`
!!!(p) {:.unnumlist}

`   t)))`
!!!(p) {:.unnumlist}

The function `consistent-labelings` is passed a vertex.
It gets all the labels for this vertex from the neighboring vertexes, collecting them in `neighbor-labels`.
It then checks all the labels on the current vertex, keeping only the ones that are consistent with all the neighbors' constraints.
The auxiliary function `labels-for` finds the labels for a particular neighbor at a vertex, and reverse-1 abel accounts for the fact that L and R labels are interpreted with respect to the vertex they point at.

[ ](#){:#l0035}`(defun consistent-labelings (vertex)`
!!!(p) {:.unnumlist}

` "Return the set of labelings that are consistent with neighbors."`
!!!(p) {:.unnumlist}

` (let ((neighbor-labels`
!!!(p) {:.unnumlist}

`     (mapcar #'(lambda (neighbor) (labels-for neighbor vertex))`
!!!(p) {:.unnumlist}

`       (vertex-neighbors vertex))))`
!!!(p) {:.unnumlist}

`  :: Eliminate labelings that don't have` all `lines consistent`
!!!(p) {:.unnumlist}

`  :: with the corresponding line's label from the neighbor.`
!!!(p) {:.unnumlist}

`  :: Account for the L-R mismatch with reverse-label.`
!!!(p) {:.unnumlist}

`  (find-all-if`
!!!(p) {:.unnumlist}

`   #'(lambda (labeling)`
!!!(p) {:.unnumlist}

`     (every #'member (mapcar #'reverse-label labeling)`
!!!(p) {:.unnumlist}

`       neighbor-labels))`
!!!(p) {:.unnumlist}

`   (vertex-labelings vertex))))`
!!!(p) {:.unnumlist}

Constraint propagation is often sufficient to yield a unique interpretation.
But sometimes the diagram is still underconstrained, and we will have to search for solutions.
The function `search-solutions` first checks to see if the diagram is ambiguous, by seeing if it has an ambiguous vertex, v.
If the diagram is unambiguous, then it is a solution, and we return it (in a list, `since search-solutions` is designed to return a list of all solutions).
Otherwise, for each of the possible labelings for the ambiguous vertex, we create a brand new copy of the diagram and set v's labeling in the copy to one of the possible labelings.
In effect, we are guessing that a labeling is a correct one.
We call `propagate-constraints;` if it falls, then we have guessed wrong, so there are no solutions with this labeling.
But if it succeeds, then we call `search-solutions` recursively to give us the list of solutions generated by this labeling.

[ ](#){:#l0040}`(defun search-solutions (diagram)`
!!!(p) {:.unnumlist}

` "Try` all `labelings for one ambiguous vertex.
and pro pagate."`
!!!(p) {:.unnumlist}

` :: If there is no ambiguous vertex, return the diagram.`
!!!(p) {:.unnumlist}

` :: If there is one.
make copies of the diagram trying each of`
!!!(p) {:.unnumlist}

` :: the possible labelings.
Propagate constraints and append`
!!!(p) {:.unnumlist}

` ::` all `the solutions together.`
!!!(p) {:.unnumlist}

` (let ((v (find-if #'ambiguous-vertex-p`
!!!(p) {:.unnumlist}

`            (diagram-vertexes diagram))))`
!!!(p) {:.unnumlist}

`  (if (null v)`
!!!(p) {:.unnumlist}

`     (list diagram)`
!!!(p) {:.unnumlist}

`     (mapcan`
!!!(p) {:.unnumlist}

`      #'(lambda (v-labeling)`
!!!(p) {:.unnumlist}

` (let* ((diagram2 (make-copy-diagram diagram))`
!!!(p) {:.unnumlist}

`   (v2 (find-vertex (vertex-name v) diagram2)))`
!!!(p) {:.unnumlist}

` (setf (vertex-labelings v2) (list v-labeling))`
!!!(p) {:.unnumlist}

` (if (propagate-constraints v2)`
!!!(p) {:.unnumlist}

`   (search-solutions diagram2)`
!!!(p) {:.unnumlist}

`   nil)))`
!!!(p) {:.unnumlist}

`(vertex-labelings v)))))`
!!!(p) {:.unnumlist}

That's all there is to the algorithm; all that remains are some auxiliary functions.
Here are three of them:

[ ](#){:#l0045}`(defun labels-for (vertex from)`
!!!(p) {:.unnumlist}

` "Return all the labels for the line going to vertex."`
!!!(p) {:.unnumlist}

` (let ((pos (position from (vertex-neighbors vertex))))`
!!!(p) {:.unnumlist}

`  (mapcar #'(lambda (labeling) (nth pos labeling))`
!!!(p) {:.unnumlist}

`      (vertex-labelings vertex))))`
!!!(p) {:.unnumlist}

[ ](#){:#l0050}`(defun reverse-label (label)`
!!!(p) {:.unnumlist}

` "Account for the fact that one vertex's right is another's left."`
!!!(p) {:.unnumlist}

` (case label (L 'R) (R 'L) (otherwise label)))`
!!!(p) {:.unnumlist}

[ ](#){:#l0055}`(defun find-vertex (name diagram)`
!!!(p) {:.unnumlist}

` "Find the vertex in the given diagram with the given name."`
!!!(p) {:.unnumlist}

` (find name (diagram-vertexes diagram) :key #'vertex-name))`
!!!(p) {:.unnumlist}

Here are the printing functions.
`print-vertex` prints a vertex in short form.
It obeys the `print` convention of returning the first argument.
The functions `show-vertex` and `show-diagram` print more de tailed forms.
They obey the convention f or `describe`-like functions of returning no values at all.

[ ](#){:#l0060}`(defun print-vertex (vertex stream depth)`
!!!(p) {:.unnumlist}

` "Print a vertex in the short form."`
!!!(p) {:.unnumlist}

` (d`e`clar`e `(ignore depth))`
!!!(p) {:.unnumlist}

` (format stream "~a/~d" (vertex-name vertex)`
!!!(p) {:.unnumlist}

`      (number-of-labelings vertex))`
!!!(p) {:.unnumlist}

` vertex)`
!!!(p) {:.unnumlist}

[ ](#){:#l0065}`(defun show-vertex (vertex &optional (stream t))`
!!!(p) {:.unnumlist}

` "Print a vertex in a long form, on a new line."`
!!!(p) {:.unnumlist}

` (format stream "~&~a ~d:" vertex (vertex-type vertex))`
!!!(p) {:.unnumlist}

` (mapc #'(lambda (neighbor labels)`
!!!(p) {:.unnumlist}

`      (format stream " ~a~a=[~{~a~}]" (vertex-name vertex)`
!!!(p) {:.unnumlist}

`          (vertex-name neighbor) labels))`
!!!(p) {:.unnumlist}

`   (vertex-neighbors vertex)`
!!!(p) {:.unnumlist}

`   (matrix-transpose (vertex-labelings vertex)))`
!!!(p) {:.unnumlist}

` (values))`
!!!(p) {:.unnumlist}

[ ](#){:#l0070}`(defun show-diagram (diagram &optional (title "~2&Diagram:")`
!!!(p) {:.unnumlist}

`               (stream t))`
!!!(p) {:.unnumlist}

` "Print a diagram in a long form.
Include a title."`
!!!(p) {:.unnumlist}

` (format stream title)`
!!!(p) {:.unnumlist}

` (mapc #'show-vertex (diagram-vertexes diagram))`
!!!(p) {:.unnumlist}

` (let ((n (reduce #'* (mapcar #'number-of-labelings`
!!!(p) {:.unnumlist}

`                (diagram-vertexes diagram)))))`
!!!(p) {:.unnumlist}

` (when (> n 1)`
!!!(p) {:.unnumlist}

`  (format stream "~&For ~:d interpretation ~:p." n))`
!!!(p) {:.unnumlist}

` (values)))`
!!!(p) {:.unnumlist}

`Note` that `matrix-transpose` is called by `show-vertex` to turn the matrix of labelings on its side.
It works like this:

[ ](#){:#l0075}`(possible-labelings 'Y)`
!!!(p) {:.unnumlist}

`((+ + +)`
!!!(p) {:.unnumlist}

` (- - -)`
!!!(p) {:.unnumlist}

` (L R -)`
!!!(p) {:.unnumlist}

` (− L R)`
!!!(p) {:.unnumlist}

` (R - L))`
!!!(p) {:.unnumlist}

`(matrix-transpose (possible-labelings 'Y))`
!!!(p) {:.unnumlist}

`((+ − L - R)`
!!!(p) {:.unnumlist}

` (+ − R L -)`
!!!(p) {:.unnumlist}

` (+ − − R L))`
!!!(p) {:.unnumlist}

The implementation of `matrix-transpose` is surprisingly concise.
It is an old Lisp trick, and well worth understanding:

[ ](#){:#l0080}`(defun matrix-transpose (matrix)`
!!!(p) {:.unnumlist}

` "Turn a matrix on its side."`
!!!(p) {:.unnumlist}

` (if matrix (apply #'mapcar #'list matrix)))`
!!!(p) {:.unnumlist}

The remaining code has to do with creating diagrams.
We need some handy way of specifying diagrams.
One way would be with a line-recognizing program operating on digitized input from a camera or bitmap display.
Another possibility is an interactive drawing program using a mouse and bitmap display.
But since there is not yet a Common Lisp standard for interacting with such devices, we will have to settle for a textual description.
The macro `defdiagram` defines and names a diagram.
The name is followed by a list of vertex descriptions.
Each description is a list consisting of the name of a vertex, the vertex type (Y, A, L, or T), and the names of the neighboring vertexes.
Here again is the `defdiagram` description for the cube shown in [figure 17.6](#f0035).

![f17-06-9780080571157](images/B9780080571157500170/f17-06-9780080571157.jpg)     
Figure 17.6
!!!(span) {:.fignum}
A Cube
[ ](#){:#l0085}`(defdiagram cube`
!!!(p) {:.unnumlist}

` (a Y b c d)`
!!!(p) {:.unnumlist}

` (b W g e a)`
!!!(p) {:.unnumlist}

` (c W e f a)`
!!!(p) {:.unnumlist}

` (d W f g a)`
!!!(p) {:.unnumlist}

` (e L c b)`
!!!(p) {:.unnumlist}

` (f L d c)`
!!!(p) {:.unnumlist}

` (g L b d))`
!!!(p) {:.unnumlist}

The macro `defdiagram` calls `construct-diagram` to do the real work.
It would be feasible to have `defdiagram` expand into a `defvar,` making the names be special variables.
But then it would be the user`'s` responsibility to make copies of such a variable before passing it to a destructive function.
Instead, I use `put-diagram` and `diagram` to put and get diagrams in a table, `diagram` retrieves the named diagram and makes a copy of it.
Thus, the user cannot corrupt the original diagrams stored in the table.
Another possibility would be to have `defdiagram` expand into a function definition for `name` that returns a copy of the diagram.
I chose to keep the diagram name space separate from the function name space, since names like `cube` make sense in both spaces.

[ ](#){:#l0090}`(defmacro defdiagram (name &rest vertex-descriptors)`
!!!(p) {:.unnumlist}

` "Define a diagram.
A copy can be gotten by (diagram name)."`
!!!(p) {:.unnumlist}

` '(put-diagram '.name (construct-diagram '.vertex-descriptors)))`
!!!(p) {:.unnumlist}

`(let ((diagrams (make-hash-table)))`
!!!(p) {:.unnumlist}

[ ](#){:#l0095}`(defun diagram (name)`
!!!(p) {:.unnumlist}

` "Get a fresh copy of the diagram with this name."`
!!!(p) {:.unnumlist}

` (make-copy-diagram (gethash name diagrams)))`
!!!(p) {:.unnumlist}

[ ](#){:#l0100}`(defun put-diagram (name diagram)`
!!!(p) {:.unnumlist}

` "Store a diagram under a name."`
!!!(p) {:.unnumlist}

` (setf (gethash name diagrams) diagram)`
!!!(p) {:.unnumlist}

` name))`
!!!(p) {:.unnumlist}

The function `construct-diagram` translates each vertex description, using `construct-vertex`, and then fills in the neighbors of each vertex.

[ ](#){:#l0105}`(defun construct-diagram (vertex-descriptors)`
!!!(p) {:.unnumlist}

` "Build a new diagram from a set of vertex descriptor."`
!!!(p) {:.unnumlist}

` (let ((diagram (make-diagram)))`
!!!(p) {:.unnumlist}

`  :: Put in the vertexes`
!!!(p) {:.unnumlist}

`  (setf (diagram-vertexes diagram)`
!!!(p) {:.unnumlist}

`     (mapcar #'construct-vertex vertex-descriptors))`
!!!(p) {:.unnumlist}

`  :: Put in the neighbors for each vertex`
!!!(p) {:.unnumlist}

`  (dolist (v-d vertex-descriptors)`
!!!(p) {:.unnumlist}

`   (setf (vertex-neighbors (find-vertex (first v-d) diagram))`
!!!(p) {:.unnumlist}

`      (mapcar #'(lambda (neighbor)`
!!!(p) {:.unnumlist}

`          (find-vertex neighbor diagram))`
!!!(p) {:.unnumlist}

`         (v-d-neighbors v-d))))`
!!!(p) {:.unnumlist}

`  diagram))`
!!!(p) {:.unnumlist}

[ ](#){:#l0110}`(defun construct-vertex (vertex-descriptor)`
!!!(p) {:.unnumlist}

` "Build the vertex corresponding to the descriptor."`
!!!(p) {:.unnumlist}

` :: Descriptors are like: (x L y z)`
!!!(p) {:.unnumlist}

` (make-vertex`
!!!(p) {:.unnumlist}

`  :name (first vertex-descriptor)`
!!!(p) {:.unnumlist}

`  :type (second vertex-descriptor)`
!!!(p) {:.unnumlist}

`  :labelings (possible-labelings (second vertex-descriptor))))`
!!!(p) {:.unnumlist}

`(defun v-d-neighbors (vertex-descriptor)`
!!!(p) {:.unnumlist}

` "The neighboring vertex names in a vertex descriptor."`
!!!(p) {:.unnumlist}

` (rest (rest vertex-descriptor)))`
!!!(p) {:.unnumlist}

The `defstruct` for `diagram` automatically creates the function `copy-diagram,` but it just copies each field, without copying the contents of each field.
Thus we need `make-copy-diagram` to create a copy that shares no structure with the original.

[ ](#){:#l0115}`(defun make-copy-diagram (diagram)`
!!!(p) {:.unnumlist}

` "Make a copy of a diagram, preserving connectivity."`
!!!(p) {:.unnumlist}

` (let* ((new (make-diagram`
!!!(p) {:.unnumlist}

`      :vertexes (mapcar #'copy-vertex`
!!!(p) {:.unnumlist}

`               (diagram-vertexes diagram)))))`
!!!(p) {:.unnumlist}

`  :: Put in the neighbors for each vertex`
!!!(p) {:.unnumlist}

`  (dolist (v (diagram-vertexes new))`
!!!(p) {:.unnumlist}

`   (setf (vertex-neighbors v)`
!!!(p) {:.unnumlist}

`       (mapcar #'(lambda (neighbor)`
!!!(p) {:.unnumlist}

`            (find-vertex (vertex-name neighbor) new))`
!!!(p) {:.unnumlist}

`          (vertex-neighbors v))))`
!!!(p) {:.unnumlist}

`  new))`
!!!(p) {:.unnumlist}

## [ ](#){:#st0020}17.3 Labeling Diagrams
{:#s0020}
{:.h1hd}

We are now ready to try labeling diagrams.
First the cube:

[ ](#){:#l0120}`> (print-labelings (diagram 'cube))`
!!!(p) {:.unnumlist}

`The initial diagram is:`
!!!(p) {:.unnumlist}

` A/5 Y: AB=[+−L-R] AC=[+−RL-] AD=[+--RL]`
!!!(p) {:.unnumlist}

` B/3 W: BG=[L−+] BE=[R−+] BA=[++−]`
!!!(p) {:.unnumlist}

` C/3 W: CE=[L−+] CF=[R−+] CA=[++−]`
!!!(p) {:.unnumlist}

` D/3 W: DF=[L−+] DG=[R−+] DA=[++−]`
!!!(p) {:.unnumlist}

` E/6 L: EC=[RL+L-R] EB=[LRR+L-]`
!!!(p) {:.unnumlist}

` F/6 L: FD=[RL+L-R] FC=[LRR+L-]`
!!!(p) {:.unnumlist}

` G/6 L: GB=[RL+L-R] GD=[LRR+L-]`
!!!(p) {:.unnumlist}

`For 29,160 interpr`e`tations.`
!!!(p) {:.unnumlist}

[ ](#){:#l0125}`After constraint propagation the diagram is:`
!!!(p) {:.unnumlist}

` A/1 Y: AB=[+] AC=[+] AD=[+]`
!!!(p) {:.unnumlist}

` B/2 W: BG=[L-] BE=[R-] BA=[++]`
!!!(p) {:.unnumlist}

` C/2 W: CE=[L-] CF=[R-] CA=[++]`
!!!(p) {:.unnumlist}

` D/2 W: DF=[L-] DG=[R-] DA=[++]`
!!!(p) {:.unnumlist}

` E/3 L: EC=[R-R] EB=[LL-]`
!!!(p) {:.unnumlist}

` F/3 L: FD=[R-R] FC=[LL-]`
!!!(p) {:.unnumlist}

` G/3 L: GB=[R-R] GD=[LL-]`
!!!(p) {:.unnumlist}

`For 216 interpr`e`tations.`
!!!(p) {:.unnumlist}

[ ](#){:#l0130}`There are four solutions:`
!!!(p) {:.unnumlist}

`Diagram:`
!!!(p) {:.unnumlist}

` A/1 Y: AB=[+] AC=[+] AD=[+]`
!!!(p) {:.unnumlist}

` B/1 W: BG=[L] BE=[R] BA=[+]`
!!!(p) {:.unnumlist}

` C/l W: CE=[L] CF=[R] CA=[+]`
!!!(p) {:.unnumlist}

` D/1 W: DF=[L] DG=[R] DA=[+]`
!!!(p) {:.unnumlist}

` E/l L: EC=[R] EB=[L]`
!!!(p) {:.unnumlist}

` F/1 L: FD=[R] FC=[L]`
!!!(p) {:.unnumlist}

` G/1 L: GB=[R] GD=[L]`
!!!(p) {:.unnumlist}

[ ](#){:#l0135}` Diagram:`
!!!(p) {:.unnumlist}

` A/1 Y: AD=[+] AC=[+] AD=[+]`
!!!(p) {:.unnumlist}

` B/1 W: BG=[L] BE=[R] BA=[+]`
!!!(p) {:.unnumlist}

` C/l W: CE=[L] CF=[R] CA=[+]`
!!!(p) {:.unnumlist}

` D/1 W: DF=[-] DG=[-] DA=[+]`
!!!(p) {:.unnumlist}

` E/l L: EC=[R] EB=[L]`
!!!(p) {:.unnumlist}

` F/1 L: FD=[-] FC=[L]`
!!!(p) {:.unnumlist}

` G/1 L: GB=[R] GD=[-]`
!!!(p) {:.unnumlist}

[ ](#){:#l0140}`Diagram:`
!!!(p) {:.unnumlist}

` A/1 Y: AB=[+] AC=[+] AD=[+]`
!!!(p) {:.unnumlist}

` B/1 W: BG=[L] BE=[R] BA=[+]`
!!!(p) {:.unnumlist}

` C/l W: CE=[-] CF=[-] CA=[+]`
!!!(p) {:.unnumlist}

` D/1 W: DF=[L] DG=[R] DA=[+]`
!!!(p) {:.unnumlist}

` E/l L: EC=[-] EB=[L]`
!!!(p) {:.unnumlist}

` F/1 L: FD=[R] FC=[-]`
!!!(p) {:.unnumlist}

` G/1 L: GB=[R] GD=[L]`
!!!(p) {:.unnumlist}

[ ](#){:#l0145}`Diagram:`
!!!(p) {:.unnumlist}

` A/1 Y: AB=[+] AC=[+] AD=[+]`
!!!(p) {:.unnumlist}

` B/1 W: BG=[-] BE=[-] BA=[+]`
!!!(p) {:.unnumlist}

` C/1 W: CE=[L] CF=[R] CA=[+]`
!!!(p) {:.unnumlist}

` D/1 W: DF=[L] DG=[R] DA=[+]`
!!!(p) {:.unnumlist}

` E/1 L: EC=[R] EB=[-]`
!!!(p) {:.unnumlist}

` F/1 L: FD=[R] FC=[L]`
!!!(p) {:.unnumlist}

` G/1 L: GB=[-] GD=[L]`
!!!(p) {:.unnumlist}

The four interpretations correspond, respectively, to the cases where the cube is free floating, attached to the floor (GD and DF = −), attached to a wall on the right (EC and CF = −), or attached to a wall on the left (BG and BE = −).
These are shown in [figure 17.7](#f0040).
It would be nice if we could supply information about where the cube is attached, and see if we can get a unique interpretation.
The function ground takes a diagram and modifies it by making one or more lines be grounded lines-lines that have a concave (−) label, corresponding to a junction with the ground.

![f17-07-9780080571157](images/B9780080571157500170/f17-07-9780080571157.jpg)     
Figure 17.7
!!!(span) {:.fignum}
Four Interpretations of the Cube
[ ](#){:#l0150}`(defun ground (diagram vertex-a vertex-b)`
!!!(p) {:.unnumlist}

` "Attach the line between the two vertexes to the ground.`
!!!(p) {:.unnumlist}

` That is, label the line with a -"`
!!!(p) {:.unnumlist}

` (let* ((A (find-vertex vertex-a diagram))`
!!!(p) {:.unnumlist}

`    (B (find-vertex vertex-b diagram))`
!!!(p) {:.unnumlist}

`    (i (position B (vertex-neighbors A))))`
!!!(p) {:.unnumlist}

`  (assert (not (null i)))`
!!!(p) {:.unnumlist}

`  (setf (vertex-labelings A)`
!!!(p) {:.unnumlist}

`    (find-all-if #'(lambda (1) (eq (nth i 1) '-))`
!!!(p) {:.unnumlist}

`        (vertex-labelings A)))`
!!!(p) {:.unnumlist}

`  diagram))`
!!!(p) {:.unnumlist}

We can see how this works on the cube:

[ ](#){:#l0155}`> (print-labelings (ground (diagram 'cube) 'g 'd))`
!!!(p) {:.unnumlist}

`The initial diagram is:`
!!!(p) {:.unnumlist}

` A/5 Y: AB=[+−L-R] AC=[+−RL-] AD=[+−−RL]`
!!!(p) {:.unnumlist}

` B/3 W: BG=[L−+] BE=[R−+] BA=[++−]`
!!!(p) {:.unnumlist}

` C/3 W: CE=[L−+] CF=[R−+] CA=[++−]`
!!!(p) {:.unnumlist}

` D/3 W: DF=[L−+] DG=[R−+] DA=[++−]`
!!!(p) {:.unnumlist}

` E/6 L: EC=[RL+L-R] EB[LRR+L-]`
!!!(p) {:.unnumlist}

` F/6 L: FD=[RL+L-R] FC=[LRR+L-]`
!!!(p) {:.unnumlist}

` G/1 L: GB=[R] GD=[−]`
!!!(p) {:.unnumlist}

`For 4,860 interpr`e`tations.`
!!!(p) {:.unnumlist}

[ ](#){:#l0160}`After constraint propagation the diagram is:`
!!!(p) {:.unnumlist}

` A/1 Y: AB=[+] AC=[+] AD=[+]`
!!!(p) {:.unnumlist}

` B/l W: BG=[L] BE=[R] BA=[+]`
!!!(p) {:.unnumlist}

` C/l W: CE=[L] CF=[R] CA=[C +]`
!!!(p) {:.unnumlist}

` D/l W: DF=[−] DG=[−] DA=[+]`
!!!(p) {:.unnumlist}

` E/l L: EC=[R] EB=[L]`
!!!(p) {:.unnumlist}

` F/1 L: FD=[-] FC=[L]`
!!!(p) {:.unnumlist}

` G/1 L: GB=[R] GD=[−]`
!!!(p) {:.unnumlist}

Note that the user only had to specify one of the two ground lines, GD.
The program found that DF is also grounded.
Similarly, in programming `ground-line`, we only had to update one of the vertexes.
The rest is done by constraint propagation.

The next example yields the same four interpretations, in the same order (free floating, attached at bottom, attached at right, and attached at left) when interpreted ungrounded.
The grounded version yields the unique solution shown in the following output and in [figure 17.9](#f0050).

![f17-08-9780080571157](images/B9780080571157500170/f17-08-9780080571157.jpg)     
Figure 17.8
!!!(span) {:.fignum}
Cube on a Plate
![f17-09-9780080571157](images/B9780080571157500170/f17-09-9780080571157.jpg)     
Figure 17.9
!!!(span) {:.fignum}
Labeled Cube on a Plate
[ ](#){:#l0165}`(defdiagram cube-on-plate`
!!!(p) {:.unnumlist}

` (a Y b c d)`
!!!(p) {:.unnumlist}

` (b W g e a)`
!!!(p) {:.unnumlist}

` (c W e f a)`
!!!(p) {:.unnumlist}

` (d W f g a)`
!!!(p) {:.unnumlist}

` (e L c b)`
!!!(p) {:.unnumlist}

` (f Y d c i)`
!!!(p) {:.unnumlist}

` (g Y b d h)`
!!!(p) {:.unnumlist}

` (h W l g j)`
!!!(p) {:.unnumlist}

` (i W f m j)`
!!!(p) {:.unnumlist}

` (j Y h i k)`
!!!(p) {:.unnumlist}

` (k W m l j)`
!!!(p) {:.unnumlist}

` (l L h k)`
!!!(p) {:.unnumlist}

` (m L k i))`
!!!(p) {:.unnumlist}

`> (print-labelings (ground (diagram 'cube-on-plate) 'k 'm))`
!!!(p) {:.unnumlist}

`The initial diagram is:`
!!!(p) {:.unnumlist}

` A/5 Y: AB=[+−L-R] AC=[+−RL-] AD=[+−−RL]`
!!!(p) {:.unnumlist}

` B/3 W: BG=[L−+] BE=[R−+] BA=[++−]`
!!!(p) {:.unnumlist}

` C/3 W: CE=[L−+] CF=[R−+] CA=[++−]`
!!!(p) {:.unnumlist}

` D/3 W: DF=[L−+] DG=[R−+] DA=[++−]`
!!!(p) {:.unnumlist}

` E/6 L: EC=[RL+L-R] EB=[LRR+L-]`
!!!(p) {:.unnumlist}

` F/5 Y: FD=C+−L-R] FC=[+−RL-] FI=[+−−RL]`
!!!(p) {:.unnumlist}

` G/5 Y: GB=[+−L-R] GD=[+−RL-] GH=[+−−RL]`
!!!(p) {:.unnumlist}

` H/3 W: HL=[L−+] HG=[R−+] HJ=[++−]`
!!!(p) {:.unnumlist}

` I/3 W: IF=[L−+] IM=[R−+] IJ=[++−]`
!!!(p) {:.unnumlist}

` J/5 Y: JH=[+−L-R] JI=[+−RL-] JK=[+−−RL]`
!!!(p) {:.unnumlist}

` K/1 W: KM=[−] KL=[−] KJ=[+]`
!!!(p) {:.unnumlist}

` L/6 L: LH=[RL+L-R] LK=[LRR+L-]`
!!!(p) {:.unnumlist}

` M/6 L: MK=[RL+L-R] MI=[LRR+L-]`
!!!(p) {:.unnumlist}

`For 32.805.000 interpr`e`tations.`
!!!(p) {:.unnumlist}

[ ](#){:#l0170}`After constraint propagation the diagram is`
!!!(p) {:.unnumlist}

` A/1 Y: AB=[+] AC=[+] AD=[+]`
!!!(p) {:.unnumlist}

` B/2 W: BG=[L-] BE=[R-] BA=[++]`
!!!(p) {:.unnumlist}

` C/2 W: CE=[L-] CF=[R-] CA=[++]`
!!!(p) {:.unnumlist}

` D/2 W: DF=[L-] DG=[R-] DA=[++]`
!!!(p) {:.unnumlist}

` E/1 L: EC=[R] EB=[L]`
!!!(p) {:.unnumlist}

` F/1 Y: FD=[-] FC=[L] FI=[R]`
!!!(p) {:.unnumlist}

` G/1 Y: GB=[R] GD=[-] GH=[L]`
!!!(p) {:.unnumlist}

` H/1 W: HL=[L] HG=[R] HJ=[+]`
!!!(p) {:.unnumlist}

` I/1 W: IF=[L] IM=[R] IJ=[+]`
!!!(p) {:.unnumlist}

` J/1 Y: JH=[+] JI=[+] JK=[+]`
!!!(p) {:.unnumlist}

` K/1 W: KM=[-] KL=[-] KJ=[+]`
!!!(p) {:.unnumlist}

` L/1 L: LH=[R] LK=[-]`
!!!(p) {:.unnumlist}

` M/1 L: MK=[-] MI=[L]`
!!!(p) {:.unnumlist}

It is interesting to try the algorithm on an “impossible” diagram.
It turns out the algorithm correctly finds no interpretation for this well-known illusion:

[ ](#){:#l0175}`(defdiagram poiuyt`
!!!(p) {:.unnumlist}

` (a L b g)`
!!!(p) {:.unnumlist}

` (b L j a)`
!!!(p) {:.unnumlist}

` (c L d l)`
!!!(p) {:.unnumlist}

` (d L h c)`
!!!(p) {:.unnumlist}

` (e L f i)`
!!!(p) {:.unnumlist}

` (f L k e)`
!!!(p) {:.unnumlist}

` (g L a l)`
!!!(p) {:.unnumlist}

` (h L l d)`
!!!(p) {:.unnumlist}

` (i L e k)`
!!!(p) {:.unnumlist}

` (j L k b)`
!!!(p) {:.unnumlist}

` (k W j i f)`
!!!(p) {:.unnumlist}

` (l W h g c))`
!!!(p) {:.unnumlist}

`> (print-1 abel ings (diagram 'poiuyt))`
!!!(p) {:.unnumlist}

[ ](#){:#l0180}`The initial diagram is:`
!!!(p) {:.unnumlist}

` A/6 L: AB=[RL+L-R] AG=[LRR+L-]`
!!!(p) {:.unnumlist}

` B/6 L: BJ=[RL+L-R] BA=[LRR+L-]`
!!!(p) {:.unnumlist}

` C/6 L: CD=[RL+L-R] CL=[LRR+L-]`
!!!(p) {:.unnumlist}

` D/6 L: DH=[RL+L-R] DC=[LRR+L-]`
!!!(p) {:.unnumlist}

` E/6 L: EF=[RL+L-R] EI=[LRR+L-]`
!!!(p) {:.unnumlist}

` F/6 L: FK=[RL+L-R] FE=[LRR+L-]`
!!!(p) {:.unnumlist}

` G/6 L: GA=[RL+L-R] GL=[LRR+L-]`
!!!(p) {:.unnumlist}

` H/6 L: HL=[RL+L-R] HD=[LRR+L-]`
!!!(p) {:.unnumlist}

` I/6 L: IE=[RL+L-R] IK=[LRR+L-]`
!!!(p) {:.unnumlist}

` J/6 L: JK=[RL+L-R] JB=[LRR+L-]`
!!!(p) {:.unnumlist}

` K/3 W: KJ=[L−+] KI=[R−+] KF=[++−]`
!!!(p) {:.unnumlist}

` L/3 W: LH=[L−+] LG=[R−+] LC=[++−]`
!!!(p) {:.unnumlist}

`For 544,195.584 interpr`e`tations.`
!!!(p) {:.unnumlist}

[ ](#){:#l0185}`After constraint propagation the diagram is:`
!!!(p) {:.unnumlist}

` A/5 L: AB=[RL+−R] AG=[LRRL-]`
!!!(p) {:.unnumlist}

` B/5 L: BJ=[RLL-R] BA=[LR+L-]`
!!!(p) {:.unnumlist}

` C/2 L: CD=[LR] CL=[+−]`
!!!(p) {:.unnumlist}

` D/3 L: DH=[RL-] DC=[LRL]`
!!!(p) {:.unnumlist}

` E/3 L: EF=[RLR] EI=[LR-]`
!!!(p) {:.unnumlist}

` F/2 L: FK=[+−] FE=[RL]`
!!!(p) {:.unnumlist}

` G/4 L: GA=[RL-R] GL=[L+L-]`
!!!(p) {:.unnumlist}

` H/4 L: HL=[R+−R] HD=[LRL-]`
!!!(p) {:.unnumlist}

` I/4 L: IE=[RL-R] IK=[L+L-]`
!!!(p) {:.unnumlist}

` J/4 L: JK=[R+−R] JB=[LRL-]`
!!!(p) {:.unnumlist}

` K/3 W: KJ=[L−+] KI=[R−+] KF=[++−]`
!!!(p) {:.unnumlist}

` L/3 W: LH=[L−+] LG=[R−+] LC=[++−]`
!!!(p) {:.unnumlist}

`For 2,073,600 interpr`e`tations.`
!!!(p) {:.unnumlist}

`There are z`e`ro solutions:`
!!!(p) {:.unnumlist}

Now we try a more complex diagram:

[ ](#){:#l0190}`(defdiagram tower`
!!!(p) {:.unnumlist}

` (a Y b c d)  (n L q o)`
!!!(p) {:.unnumlist}

` (b W g e a)  (o W y j n)`
!!!(p) {:.unnumlist}

` (c W e f a)  (P L r i)`
!!!(p) {:.unnumlist}

` (d W f g a)  (q W n s w)`
!!!(p) {:.unnumlist}

` (e L c b)   (r W s p x)`
!!!(p) {:.unnumlist}

` (f Y d c i)  (s L r q)`
!!!(p) {:.unnumlist}

` (g Y b d h)  (t W w x z)`
!!!(p) {:.unnumlist}

` (h W l g J)  (u W x y z)`
!!!(p) {:.unnumlist}

` (i W f m p)  (v W y w z)`
!!!(p) {:.unnumlist}

` (j Y h o k)  (w Y t v q)`
!!!(p) {:.unnumlist}

` (k W m l j)  (x Y r u t)`
!!!(p) {:.unnumlist}

` (l L h k)   (y Y v u o)`
!!!(p) {:.unnumlist}

` (m L k i)   (z Y t u v))`
!!!(p) {:.unnumlist}

`> (print-labelings (ground (diagram 'tower) 'l 'k))`
!!!(p) {:.unnumlist}

`The initial diagram is:`
!!!(p) {:.unnumlist}

` A/5 Y: AB=[+−L-R] AC=[+−RL-] AD=[+−−RL]`
!!!(p) {:.unnumlist}

` B/3 W: BG=[L−+] BE=[R−+] BA=[++−]`
!!!(p) {:.unnumlist}

` C/3 W: CE=[L−+] CF=[R−+] CA=[++−]`
!!!(p) {:.unnumlist}

` D/3 W: DF=[L−+] DG=[R−+] DA=[++−]`
!!!(p) {:.unnumlist}

` E/6 L: EC[RL+L-R] EB=[LRR+L-]`
!!!(p) {:.unnumlist}

` F/5 Y: FD=[+−L-R] FC=[+−RL-] FI=[+−−RL]`
!!!(p) {:.unnumlist}

` G/5 Y: GB=[+−L-R] GD=[+−RL-] GH=[+−−RL]`
!!!(p) {:.unnumlist}

` H/3 W: HL=[L−+] HG=[R−+] HJ=[++−]`
!!!(p) {:.unnumlist}

` I/3 W: IF=[L−+] IM=[R−+] IP=[++−]`
!!!(p) {:.unnumlist}

` J/5 Y: JH=[+−L-R] JO=[+−RL-] JK=[+−−RL]`
!!!(p) {:.unnumlist}

` K/3 W: KM=[L−+] KL=[R−+] KJ=[++−]`
!!!(p) {:.unnumlist}

` L/1 L: LH=[R] LK=[−]`
!!!(p) {:.unnumlist}

` M/6 L: MK=[RL+L-R] MI=[LRR+L-]`
!!!(p) {:.unnumlist}

` N/6 L: NQ=[RL+L-R] NO=[LRR+L-]`
!!!(p) {:.unnumlist}

` O/3 W: OY=[L−+] OJ=[R−+] ON=[++−]`
!!!(p) {:.unnumlist}

` P/6 L: PR=[RL+L-R] PI=[LRR+L-]`
!!!(p) {:.unnumlist}

` Q/3 W: QN=[L−+] QS=[R−+] QW=[++−]`
!!!(p) {:.unnumlist}

` R/3 W: RS=[L−+] RP=[R−+] RX=[++−]`
!!!(p) {:.unnumlist}

` S/6 L: SR=[RL+L-R] SQ=[LRR+L-]`
!!!(p) {:.unnumlist}

` T/3 W:` TW=[L−+] `TX=[R−+] TZ=[++−]`
!!!(p) {:.unnumlist}

` U/3 W: UX=[L−+] UY=[R−+] UZ=[++−]`
!!!(p) {:.unnumlist}

` V/3 W: VY=[L−+] VW=[R−+] VZ=[++−]`
!!!(p) {:.unnumlist}

` W/5 Y: WT=[+−L-R] WV=[+−RL-] WQ=[+−−RL]`
!!!(p) {:.unnumlist}

` X/5 Y: XR=[+−L-R] XU=[+−RL-] XT=[+−−RL]`
!!!(p) {:.unnumlist}

` Y/5 Y: YV=[+−L-R] YU=[+−RL-] YO=[+−−RL]`
!!!(p) {:.unnumlist}

` Z/5 Y: ZT=[+−L-R] ZU=[+−RL-] ZV=[+−−RL]`
!!!(p) {:.unnumlist}

`For 1,614,252,037,500,000 interpretations.`
!!!(p) {:.unnumlist}

After constraint propagation the diagram is:

[ ](#){:#l0195}` A/1 Y: AB=[+] AC=[+] AD=[+]`
!!!(p) {:.unnumlist}

` B/l W: BG=[L] BE=[R] BA=[+]`
!!!(p) {:.unnumlist}

` C/1 W: CE=[L] CF=[R] CA=[+]`
!!!(p) {:.unnumlist}

` D/l W: DF=[−] DG=[−] DA=[+]`
!!!(p) {:.unnumlist}

` E/1 L: EC=[R] EB=[L]`
!!!(p) {:.unnumlist}

` F/1 Y: FD=[−] FC=[L] FI=[R]`
!!!(p) {:.unnumlist}

` G/1 Y: GB=[R] GD=[−]GH=[L]`
!!!(p) {:.unnumlist}

` H/1 W: HL=[L] HG=[R] HJ=[+]`
!!!(p) {:.unnumlist}

` I/1 W: IF=[L] IM=[R] IP=[+]`
!!!(p) {:.unnumlist}

` J/l Y: JH=[+] JO=[+] JK=[+]`
!!!(p) {:.unnumlist}

` K/l W: KM=[−] KL=[−] KJ=[+]`
!!!(p) {:.unnumlist}

` L/l L: LH=[R] LK=[−]`
!!!(p) {:.unnumlist}

` M/1 L: MK=[−] MI=[L]`
!!!(p) {:.unnumlist}

` N/l L: NQ=[R] NO[−]`
!!!(p) {:.unnumlist}

` O/l W: OY=[+] OJ=[+] ON=[−]`
!!!(p) {:.unnumlist}

` P/l L: PR=[L] PI=[+]`
!!!(p) {:.unnumlist}

` Q/1 W: QN=[L] QS=[R] QW=[+]`
!!!(p) {:.unnumlist}

` R/1 W: RS=[L] RP=[R] RX=[+]`
!!!(p) {:.unnumlist}

` S/1 L: SR=[R] SQ=[L]`
!!!(p) {:.unnumlist}

` T/1 W: TW=[+] TX=[+] TZ=[−]`
!!!(p) {:.unnumlist}

` U/1 W: UX=[+] UY=[+] UZ=[−]`
!!!(p) {:.unnumlist}

` V/l W: VY=[+] VW=[+] VZ=[−]`
!!!(p) {:.unnumlist}

` W/l Y: WT=[+] WV=[+] WQ=[+]`
!!!(p) {:.unnumlist}

` X/1 Y: XR=[+] XU=[+] XT=[+]`
!!!(p) {:.unnumlist}

` Y/1 Y: YV=[+] YU=[+] YO=[+]`
!!!(p) {:.unnumlist}

` Z/l Y: ZT=[−] ZU=[−] ZV=[−]`
!!!(p) {:.unnumlist}

We see that the algorithm was able to arrive at a single interpretation.
Moreover, even though there were a large number of possibilities—over a quadrillion—the computation is quite fast.
Most of the time is spent printing, so to get a good measurement, we define a function to find solutions without printing anything:

[ ](#){:#l0200}`(defun find-labelings (diagram)`
!!!(p) {:.unnumlist}

` "Return a list of all consistent labelings of the diagram."`
!!!(p) {:.unnumlist}

` (every #'propagate-constraints (diagram-vertexes diagram))`
!!!(p) {:.unnumlist}

` (search-solutions diagram))`
!!!(p) {:.unnumlist}

When we time the application of `find-labelings` to the grounded tower and the poiuyt, we find the tower takes 0.11 seconds, and the poiuyt 21 seconds.
This is over 180 times longer, even though the poiuyt has only half as many vertexes and only about half a million interpretations, compared to the tower's quadrillion.
The poiuyt takes a long time to process because there are few local constraints, so violations are discovered only by considering several widely separated parts of the figure all at the same time.
It is interesting that the same fact that makes the processing of the poiuyt take longer is also responsible for its interest as an illusion.

## [ ](#){:#st0025}17.4 Checking Diagrams for Errors
{:#s0025}
{:.h1hd}

This section considers one more example, and considers what to do when there are apparent errors in the input.
The example is taken from Charniak and McDermott's *Introduction to Artificial Intelligence*, page 138, and shown in [figure 17.12](#f0065).

![f17-10-9780080571157](images/B9780080571157500170/f17-10-9780080571157.jpg)     
Figure 17.10
!!!(span) {:.fignum}
An Impossible Figure (A Poiuyt)
![f17-11-9780080571157](images/B9780080571157500170/f17-11-9780080571157.jpg)     
Figure 17.11
!!!(span) {:.fignum}
A Tower
![f17-12-9780080571157](images/B9780080571157500170/f17-12-9780080571157.jpg)     
Figure 17.12
!!!(span) {:.fignum}
Diagram of an arch
[ ](#){:#l0205}`(defdiagram arch`
!!!(p) {:.unnumlist}

` (a W e b c)  (p L o q)`
!!!(p) {:.unnumlist}

` (b L d a)   (q T P i r)`
!!!(p) {:.unnumlist}

` (c Y a d g)  (r T j s q)`
!!!(p) {:.unnumlist}

` (d Y c b m)  (s L r t)`
!!!(p) {:.unnumlist}

` (e L a f)   (t W v s k)`
!!!(p) {:.unnumlist}

` (f T e g n)  (u L t l)`
!!!(p) {:.unnumlist}

` (g W h f c)  (v L t l)`
!!!(p) {:.unnumlist}

` (h T g i o)  (w W x l y)`
!!!(p) {:.unnumlist}

` (i T h j q)  (x L w z)`
!!!(p) {:.unnumlist}

` (j T i k r)  (y Y w 2 z)`
!!!(p) {:.unnumlist}

` (k T J l t)  (z W 3 x y)`
!!!(p) {:.unnumlist}

` (l T k m v)  (l T n o w)`
!!!(p) {:.unnumlist}

` (m L l d)   (2 W v 3 y)`
!!!(p) {:.unnumlist}

` (n L f 1)   (3 L z 2)`
!!!(p) {:.unnumlist}

` (o W P 1 h)  (4 T u l v))`
!!!(p) {:.unnumlist}

Unfortunately, running this example results in no consistent interpretations after constraint propagation.
This seems wrong.
Worse, when we try to ground the diagram on the line XZ and call `print-labelings` on that, we get the following error:

[ ](#){:#l0210}`>>>ERROR: The first argument to NTH was of the wrong type.`
!!!(p) {:.unnumlist}

`The function expected a fixnum >= z`e`ro.`
!!!(p) {:.unnumlist}

`While in the function LABELS-FOR`⇐ `CONSISTENT-LABELINGS`
!!!(p) {:.unnumlist}

`Debugger entered while in the following function:`
!!!(p) {:.unnumlist}

`LABELS-FOR (P.C.
= 23)`
!!!(p) {:.unnumlist}

` Arg 0 (VERTEX): U/6`
!!!(p) {:.unnumlist}

` Arg 1 (FROM): 4/4`
!!!(p) {:.unnumlist}

What has gone wrong?
A good guess is that the diagram is somehow inconsistent— somewhere an error was made in transcribing the diagram.
It could be that the diagram is in fact impossible, like the poiuyt.
But that is unlikely, as it is easy for us to provide an intuitive interpretation.
We need to debug the diagram, and it would also be a good idea to handle the error more gracefully.

One property of the diagram that is easy to check for is that every line should be mentioned twice.
If there is a line between vertexes A and B, there should be two entries in the vertex descriptors of the following form:

[ ](#){:#l0215}`(A ?
… B …)`
!!!(p) {:.unnumlist}

`(B ?
… A …)`
!!!(p) {:.unnumlist}

Here the symbol “?” means we aren't concerned about the type of the vertexes, only with the presence of the line in two places.
The following code makes this check when a diagram is defined.
It also checks that each vertex is one of the four legal types, and has the right number of neighbors.

[ ](#){:#l0220}`(defmacro defdiagram (name &rest vertex-descriptors)`
!!!(p) {:.unnumlist}

` "Define a diagram.
A copy can be gotten by (diagram name)."`
!!!(p) {:.unnumlist}

` '(put-diagram '.name (construct-diagram`
!!!(p) {:.unnumlist}

`          (check-diagram ',vertex-descriptors))))`
!!!(p) {:.unnumlist}

`(defun check-diagram (vertex-descriptors)`
!!!(p) {:.unnumlist}

` "Check if the diagram description appears consistent."`
!!!(p) {:.unnumlist}

` (let ((errors 0))`
!!!(p) {:.unnumlist}

`  (dolist (v-d vertex-descriptors)`
!!!(p) {:.unnumlist}

`   :: v-d is like: (a Y b c d)`
!!!(p) {:.unnumlist}

`   (let ((A (first v-d))`
!!!(p) {:.unnumlist}

`          (v-type (second v-d)))`
!!!(p) {:.unnumlist}

`    :: Check that the number of neighbors is right for`
!!!(p) {:.unnumlist}

`    :: the vertex type (and that the vertex type is l`e`gal)`
!!!(p) {:.unnumlist}

`    (when (/= (length (v-d-neighbors v-d))`
!!!(p) {:.unnumlist}

`       (case v-type ((W Y T) 3) ((L) 2) (t − 1)))`
!!!(p) {:.unnumlist}

`     (warn "Ill`e`gal type/neighbor combo: ~a" v-d)`
!!!(p) {:.unnumlist}

`     (incf errors))`
!!!(p) {:.unnumlist}

`    :: Check that each neighbor B is connected to`
!!!(p) {:.unnumlist}

`    :: this vertex.
A, exactly once`
!!!(p) {:.unnumlist}

`     (dolist (B (v-d-neighbors v-d))`
!!!(p) {:.unnumlist}

`      (when (/= 1 (count-if`
!!!(p) {:.unnumlist}

`        #'(lambda (v-d2)`
!!!(p) {:.unnumlist}

`         (and (eql (first v-d2) B)`
!!!(p) {:.unnumlist}

`          (member A (v-d-neighbors v-d2))))`
!!!(p) {:.unnumlist}

`       vertex-descri ptors))`
!!!(p) {:.unnumlist}

`    (warn "Inconsistent vertex: ~a-~a" A B)`
!!!(p) {:.unnumlist}

`    (incf errors)))))`
!!!(p) {:.unnumlist}

`   (when (> errors 0)`
!!!(p) {:.unnumlist}

`    (error "Inconsistent diagram.
~d total error~:p."`
!!!(p) {:.unnumlist}

`      errors)))`
!!!(p) {:.unnumlist}

`  vertex-descriptors)`
!!!(p) {:.unnumlist}

Now let's try the arch again:

[ ](#){:#l0225}`(defdiagram arch`
!!!(p) {:.unnumlist}

` (a W e b c)  (p L o q)`
!!!(p) {:.unnumlist}

` (b L d a)   (q T p i r)`
!!!(p) {:.unnumlist}

` (c Y a d g)  (r T j s q)`
!!!(p) {:.unnumlist}

` (d Y c b m)  (s L r t)`
!!!(p) {:.unnumlist}

` (e L a f)   (t W v s k)`
!!!(p) {:.unnumlist}

` (f T e g n)  (u L t l)`
!!!(p) {:.unnumlist}

` (g W h f c)  (v L 2 4)`
!!!(p) {:.unnumlist}

` (h T g i o)  (w W x l y)`
!!!(p) {:.unnumlist}

` (i T h j q)  (x L w z)`
!!!(p) {:.unnumlist}

` (j T i k r)  (y Y w 2 z)`
!!!(p) {:.unnumlist}

` (k T j l t)  (z W 3 x y)`
!!!(p) {:.unnumlist}

` (l T k m v)  (1 T n o w)`
!!!(p) {:.unnumlist}

` (m L l d)   (2 W v 3 y)`
!!!(p) {:.unnumlist}

` (n L f 1)   (3 L z 2)`
!!!(p) {:.unnumlist}

` (o W P 1 h)  (4 T u l v))`
!!!(p) {:.unnumlist}

`Warning: Inconsistent vertex: T-V`
!!!(p) {:.unnumlist}

`Warning: Inconsistent vertex: U-T`
!!!(p) {:.unnumlist}

`Warning: Inconsistent vertex: U-L`
!!!(p) {:.unnumlist}

`Warning: Inconsistent vertex: L-V`
!!!(p) {:.unnumlist}

`Warning: Inconsistent vertex: 4-U`
!!!(p) {:.unnumlist}

`Warning: Inconsistent vertex: 4-L`
!!!(p) {:.unnumlist}

`»ERR0R: Inconsistent diagram.
6 total errors.`
!!!(p) {:.unnumlist}

The `defdiagram` was transcribed from a hand-labeled diagram, and it appears that the transcription has fallen prey to one of the oldest problems in mathematical notation: confusing a “u” with a “v.” The other problem was in seeing the line U-L as a single line, when in fact it is broken up into two segments, U-4 and 4-L.
Repairing these bugs gives the diagram:

[ ](#){:#l0230}`(defdiagram arch`
!!!(p) {:.unnumlist}

` (a W e b c)  (P L o q)`
!!!(p) {:.unnumlist}

` (b L d a)   (q T P i r)`
!!!(p) {:.unnumlist}

` (c Y a d g)  (r T j s q)`
!!!(p) {:.unnumlist}

` (d Y c b m)  (s L r t)`
!!!(p) {:.unnumlist}

` (e L a f)   (t W u s k)    *;t-u not t-v*`
!!!(p) {:.unnumlist}

` (f T e g n)  (u L t 4)     *;u-4 not u-l*`
!!!(p) {:.unnumlist}

` (g W h f c)  (v L 2 4)`
!!!(p) {:.unnumlist}

` (h T g i o)  (w W x l y)`
!!!(p) {:.unnumlist}

` (i T h j q)  (x L w z)`
!!!(p) {:.unnumlist}

` (j T i k r)  (y Y w 2 z)`
!!!(p) {:.unnumlist}

` (k T J l t)  (z W 3 x y)`
!!!(p) {:.unnumlist}

` (l T k m 4)  (1 T n o w)     *;l-4 not l-v*`
!!!(p) {:.unnumlist}

` (m L l d)   (2 W v 3 y)`
!!!(p) {:.unnumlist}

` (n L f 1)   (3 L z 2)`
!!!(p) {:.unnumlist}

` (o W P 1 h)  (4 T u l v))`
!!!(p) {:.unnumlist}

This time there arenoerrorsdetected by `check-diagram,` butrunning `print-labelings` again still does not give a solution.
`To` get more information about which constraints are applied, `I` modified `propagate-constraints` to print out some information:

[ ](#){:#l0235}`(defun propagate-constraints (vertex)`
!!!(p) {:.unnumlist}

` "Reduce the number of labelings on vertex by considering neighbors.`
!!!(p) {:.unnumlist}

` If we can reduce, propagate the new constraint to each neighbor."`
!!!(p) {:.unnumlist}

` :: Return nil only when the constraints lead to an impossibility`
!!!(p) {:.unnumlist}

` (let ((old-num (number-of-labelings vertex)))`
!!!(p) {:.unnumlist}

`  (setf (vertex-labelings vertex) (consistent-labelings vertex))`
!!!(p) {:.unnumlist}

`  (unless (impossible-vertex-p vertex)`
!!!(p) {:.unnumlist}

`   (when (< (number-of-labelings vertex) old-num)`
!!!(p) {:.unnumlist}

`    (format t "~&; ~a: ~14a ~a" vertex ;***`
!!!(p) {:.unnumlist}

`        (vertex-neighbors vertex) ;***`
!!!(p) {:.unnumlist}

`        (vertex-labelings vertex)) ;***`
!!!(p) {:.unnumlist}

`    (every #'propagate-constraints (vertex-neighbors vertex)))`
!!!(p) {:.unnumlist}

`   vertex)))`
!!!(p) {:.unnumlist}

Running the problem again gives the following trace:

[ ](#){:#l0240}`> (print-labelings (ground (diagram 'arch) 'x 'z))`
!!!(p) {:.unnumlist}

`The initial diagram is:`
!!!(p) {:.unnumlist}

` A/3 W: AE=[L−+] AB-CR−+] AC=[++−]`
!!!(p) {:.unnumlist}

` P/6 L: P0=[RL+L-R] PQ=[LRR+L-]`
!!!(p) {:.unnumlist}

` B/6 L: BD=[RL+L-R] BA=[LRR+L-]`
!!!(p) {:.unnumlist}

` Q/4 T: QP=[RRRR] QI=[LLLL] QR=[+−LR]`
!!!(p) {:.unnumlist}

` C/5 Y: CA=[+−L-R] CD=[+−RL-] CG=[+−−RL]`
!!!(p) {:.unnumlist}

` R/4 T: RJ=[RRRR] RS=[LLLL] RQ=[+−LR]`
!!!(p) {:.unnumlist}

` D/5 Y: DC=[+−L-R] DB=[+−RL-] DM=[+−−RL]`
!!!(p) {:.unnumlist}

` S/6 L: SR=[RL+L-R] ST=[LRR+L-]`
!!!(p) {:.unnumlist}

` S/6 L: EA=[RL+L-R] EF=[LRR+L-]`
!!!(p) {:.unnumlist}

` T/3 W: TU=[L-+] TS=[R-+] TK=[++-]`
!!!(p) {:.unnumlist}

` F/4 T: FE=[RRRR] FG=[LLLL] FN=[+-LR]`
!!!(p) {:.unnumlist}

` U/6 L: UT=[RL+L-R] U4=[LRR+L-]`
!!!(p) {:.unnumlist}

` G/3 W: GH=[L-+] GF=[R-+] GC=[++-]`
!!!(p) {:.unnumlist}

` V/6 L: V2=[RL+L-R] V4=[LRR+L-]`
!!!(p) {:.unnumlist}

` H/4 T: HG=[RRRR] HI=[LLLL] Ho=[+-LR]`
!!!(p) {:.unnumlist}

` W/3 W: WX=[L-+] W1=[R-+] WY=[++-]`
!!!(p) {:.unnumlist}

` I/4 T: IH=[RRRR] IJ=[LLLL] IQ=[+-LR]`
!!!(p) {:.unnumlist}

` X/1 L: XW=[R] XZ=[-]`
!!!(p) {:.unnumlist}

` J/4 T: JI=[RRRR] JK=[LLLL] JR=[+-LR]`
!!!(p) {:.unnumlist}

` Y/5 Y: YW=[+-L-R] Y2=[+-RL-] YZ=[+--RL]`
!!!(p) {:.unnumlist}

` K/4 T: KJ=[RRRR] KL=[LLLL] KT=[+-LR]`
!!!(p) {:.unnumlist}

` Z/3 W: Z3=[L-+] ZX=[R-+] ZY=[++-]`
!!!(p) {:.unnumlist}

` L/4 T: LK=[RRRR] LM=[LLLL] L4=[+-LR]`
!!!(p) {:.unnumlist}

` 1/4 T: 1N=[RRRR] 10=[LLLL] 1 W=[+-LR]`
!!!(p) {:.unnumlist}

` M/6 L: ML=[RL+L-R] MD=[LRR+L-]`
!!!(p) {:.unnumlist}

` 2/3 W: 2 V=[L-+] 23=[R-+] 2Y=[++-]`
!!!(p) {:.unnumlist}

` N/6 L: NF=[RL+L-R] N1=[LRR+L-]`
!!!(p) {:.unnumlist}

` 3/6 L: 3Z=[RL+L-R] 32=[LRR+L-]`
!!!(p) {:.unnumlist}

` 0/3 W: 0P=[L-+] 01=[R-+] 0H=[++-]`
!!!(p) {:.unnumlist}

` 4/4 T: 4U=[RRRR] 4 L=[LLLL] 4 V=[+-LR]`
!!!(p) {:.unnumlist}

`For 2,888, 816, 545.234, 944,000 interpretations`
!!!(p) {:.unnumlist}

`: P/2: (0/3 Q/4)    ((R L) (- L))`
!!!(p) {:.unnumlist}

`: 0/1: (P/2 1/4 H/4)  ((L R +))`
!!!(p) {:.unnumlist}

`: P/1: (0/1 Q/4)    ((R L))`
!!!(p) {:.unnumlist}

`: 1/3: (N/6 0/1 W/3)  ((R L +) (R L -) (R L L))`
!!!(p) {:.unnumlist}

`: N/2: (F/4 1/3)    ((R L) (- L))`
!!!(p) {:.unnumlist}

`: F/2: (E/6 G/3 N/2)  ((R L -) (R L L))`
!!!(p) {:.unnumlist}

`: E/2: (A/3 F/2)   ((R L) (- L))`
!!!(p) {:.unnumlist}

`: A/2: (E/2 B/6 C/5)  ((L R +) (- - +))`
!!!(p) {:.unnumlist}

`: B/3: (D/5 A/2)   ((R L) (- L) (R -))`
!!!(p) {:.unnumlist}

`: D/3: (C/5 B/3 M/6)  ((- - -) (- L R) (R - L))`
!!!(p) {:.unnumlist}

`: W/1: (X/l 1/3 Y/5)  ((L R +))`
!!!(p) {:.unnumlist}

`: 1/1: (N/2 0/1 W/l)  ((R L L))`
!!!(p) {:.unnumlist}

`: Y/1: (W/l 2/3 Z/3)  ((+ + +))`
!!!(p) {:.unnumlist}

`: 2/2: (V/6 3/6 Y/1)  ((L R +) (- - +))`
!!!(p) {:.unnumlist}

`: V/3: (2/2 4/4)   ((R L) (- L) (R -))`
!!!(p) {:.unnumlist}

`: 4/2: (U/6 L/4 V/3)  ((R L -) (R L R))`
!!!(p) {:.unnumlist}

`: U/2: (T/3 4/2)   ((R L) (- L))`
!!!(p) {:.unnumlist}

`: T/2: (U/2 S/6 K/4)  ((L R +) (- - +))`
!!!(p) {:.unnumlist}

`: S/2: (R/4 T/2)   ((R L) (R -))`
!!!(p) {:.unnumlist}

`: K/1: (J/4 L/4 T/2)  ((R L +))`
!!!(p) {:.unnumlist}

`: J/1: (1/4 K/1 R/4)  ((R L L))`
!!!(p) {:.unnumlist}

`: I/1: (H/4 J/1 Q/4)  ((R L R))`
!!!(p) {:.unnumlist}

`: L/1: (K/l M/6 4/2)  ((R L R))`
!!!(p) {:.unnumlist}

`: M/2: (L/1 D/3)   ((R L) (R -))`
!!!(p) {:.unnumlist}

`: 3/3: (Z/3 2/2)   ((R L) (− L) (R -))`
!!!(p) {:.unnumlist}

`: Z/1 : (3/3 X/1 Y/1)  ((− − +))`
!!!(p) {:.unnumlist}

`: 3/1: (Z/l 2/2)  ((− L))`
!!!(p) {:.unnumlist}

`: 2/1: (V/3 3/1 Y/1)  ((L R +))`
!!!(p) {:.unnumlist}

`: V/2: (2/1 4/2)   ((R L) (R -))`
!!!(p) {:.unnumlist}

`After constraint propagation the diagram is:`
!!!(p) {:.unnumlist}

` A/0 W:`
!!!(p) {:.unnumlist}

` P/l L: P0=[R] PQ=CL]`
!!!(p) {:.unnumlist}

` B/0 L:`
!!!(p) {:.unnumlist}

` Q/4 T: QP=[RRRR] QI=[LLLL] QR=[+−LR]`
!!!(p) {:.unnumlist}

` C/0 Y:`
!!!(p) {:.unnumlist}

` R/4 T: RJ=[RRRR] RS=[LLLL] RQ=[+−LR]`
!!!(p) {:.unnumlist}

` D/0 Y:`
!!!(p) {:.unnumlist}

` S/2 L: SR=[RR] ST=[L-]`
!!!(p) {:.unnumlist}

` E/2 L: EA=[R-] EF=[LL]`
!!!(p) {:.unnumlist}

` T/2 W: TU=[L-] TS=CR-] TK=[++]`
!!!(p) {:.unnumlist}

` F/2 T: FE=[RR] FG=[LL] FN=[− L]`
!!!(p) {:.unnumlist}

` U/2 L: UT=[R-] U4=[LL]`
!!!(p) {:.unnumlist}

` G/0 W:`
!!!(p) {:.unnumlist}

` V/2 L: V2=[RR] V4=[L-]`
!!!(p) {:.unnumlist}

` H/0 T:`
!!!(p) {:.unnumlist}

` W/l W: WX=[L] W1=[R] WY=[+]`
!!!(p) {:.unnumlist}

` I/1 T: IH=[R] IJ=[L] IQ=[R]`
!!!(p) {:.unnumlist}

` X/1 L: XW=[R] XZ=[−]`
!!!(p) {:.unnumlist}

` J/1 T: JI=[R] JK=[L] JR=[L]`
!!!(p) {:.unnumlist}

` Y/1 Y: YW=[+] Y2=[+] YZ=[+]`
!!!(p) {:.unnumlist}

` K/1 T: KJ=[R] KL=[L] KT=[+]`
!!!(p) {:.unnumlist}

` Z/1 W: Z3=[−] ZX=[−] ZY=[+]`
!!!(p) {:.unnumlist}

` L/1 T: LK=[R] LM=[L] L4=[R]`
!!!(p) {:.unnumlist}

` 1/1 T: 1 N=[R] 10=[L] 1 W=[L]`
!!!(p) {:.unnumlist}

` M/2 L: ML=[RR] MD=[L-]`
!!!(p) {:.unnumlist}

` 2/1 W: 2 V=[L] 23=[R] 2Y=[+]`
!!!(p) {:.unnumlist}

` N/2 L: NF=[R-] N1=[LL]`
!!!(p) {:.unnumlist}

` 3/1 L: 3Z=[−] 32=[L]`
!!!(p) {:.unnumlist}

` 0/1 W: 0P=[L] 01=[R] 0H=[+]`
!!!(p) {:.unnumlist}

` 4/2 T: 4U=[RR] 4 L=[LL] 4 V=[− R]`
!!!(p) {:.unnumlist}

From the diagram after constraint propagation we can see that the vertexes A,B,C,D,G, and H have no interpretations, so they are a good place to look first for an error.
From the trace generated by `propagate-constraints` (the lines beginning with a semicolon), we see that constraint propagation started at P and after seven propagations reached some of the suspect vertexes:

[ ](#){:#l0245}`: A/2: (E/2 B/6 C/5)  ((L R +) (− − + ))`
!!!(p) {:.unnumlist}

`: B/3: (D/5 A/2)    ((R L) (− L) (R -))`
!!!(p) {:.unnumlist}

`: D/3: (C/5 B/3 M/6)  ((- - -) (− L R) (R - L))`
!!!(p) {:.unnumlist}

A and B look acceptable, but look at the entry for vertex D.
It shows three interpretations, and it shows that the neighbors are C, B, and M.
Note that line DC, the first entry in each of the interpretations, must be either -, - or R.
But this is an error, because the “correct” interpretation has DC as a + line.
Looking more closely, we notice that D is in fact a W-type vertex, not a Y vertex as written in the definition.
We should have:

[ ](#){:#l0250}`(defdiagram arch`
!!!(p) {:.unnumlist}

` (a W e b c)  (p L o q)`
!!!(p) {:.unnumlist}

` (b L d a)   (q T p i r)`
!!!(p) {:.unnumlist}

` (c Y a d g)  (r T j s q)`
!!!(p) {:.unnumlist}

` (d W b m c)  (s L r t)     ;*d is a W, not Y*`
!!!(p) {:.unnumlist}

` (e L a f)   (t W u s k)`
!!!(p) {:.unnumlist}

` (f T e g n)  (u L t 4)`
!!!(p) {:.unnumlist}

` (g W h f c)  (v L 2 4)`
!!!(p) {:.unnumlist}

` (h T g i o)  (w W x 1 y)`
!!!(p) {:.unnumlist}

` (i T h j q)  (x L w z)`
!!!(p) {:.unnumlist}

` (j T i k r)  (y Y w 2 z)`
!!!(p) {:.unnumlist}

` (k T J l t)  (z W 3 x y)`
!!!(p) {:.unnumlist}

` (1 T k m 4)  (1 T n o w)`
!!!(p) {:.unnumlist}

` (m L l d)   (2 W v 3 y)`
!!!(p) {:.unnumlist}

` (n L f 1)   (3 L z 2)`
!!!(p) {:.unnumlist}

` (o W P 1 h)  (4 T u l v))`
!!!(p) {:.unnumlist}

By running the problem again and inspecting the trace output, we soon discover the real root of the problem: the most natural interpretation of the diagram is beyond the scope of the program!
There are many interpretations that involve blocks floating in air, but if we ground lines OP, TU and XZ, we run into trouble.
Remember, we said that we were considering trihedral vertexes only.
But vertex 1 would be a quad-hedral vertex, formed by the intersection of four planes: the top and back of the base, and the bottom and left-hand side of the left pillar.
The intuitively correct labeling for the diagram would have O1 be a concave (−) line and Al be an occluding line, but our repertoire of labelings for T vertexes does not allow this.
Hence, the diagram cannot be labeled consistently.

Let's go back and consider the error that came up in the first version of the diagram.
Even though the error no longer occurs on this diagram, we want to make sure that it won't show up in another case.
Here's the error:

[ ](#){:#l0255}`>>>ERROR: The first argument to NTH was of the wrong type.`
!!!(p) {:.unnumlist}

`The function expected a fixnum >= z`e`ro.`
!!!(p) {:.unnumlist}

`While in the function LABELS-FOR`⇐ `CONSISTENT-LABELINGS`
!!!(p) {:.unnumlist}

`Debugger entered while in the following function:`
!!!(p) {:.unnumlist}

`LABELS-FOR (P.C.
= 23)`
!!!(p) {:.unnumlist}

`   Arg 0 (VERTEX): U/6`
!!!(p) {:.unnumlist}

`   Arg 1 (FROM): 4/4`
!!!(p) {:.unnumlist}

Looking at the definition of `labels-for`, we see that it is looking for the from vertex, which in this case is 4, among the neighbors of U.
It was not found, so pos became nil, and the function nth complained that it was not given an integer as an argument.
So this error, if we had pursued it earlier, would have pointed out that 4 was not listed as a neighbor of U, when it should have been.
Of course, we found that out by other means.
In any case, there is no bug here to fix—as long as a diagram is guaranteed to be consistent, the `labels-for` bug will not appear again.

This section has made two points: First, write code that checks the input as thoroughly as possible.
Second, even when input checking is done, it is still up to the user to understand the limitations of the program.

## [ ](#){:#st0030}17.5 History and References
{:#s0030}
{:.h1hd}

[Guzman (1968)](B9780080571157500285.xhtml#bb0500) was one of the first to consider the problem of interpreting line diagrams.
He classified vertexes, and defined some heuristics for combining information from adjacent vertexes.
[Huffman (1971)](B9780080571157500285.xhtml#bb0560) and [Clowes (1971)](B9780080571157500285.xhtml#bb0215) independently came up with more formai and complete analyses, and David [Waltz (1975)](B9780080571157500285.xhtml#bb1300) extended the analysis to handle shadows, and introduced the constraint propagation algorithm to eut down on the need for search.
The algorithm is sometimes called "Waltz filtering" in his honor.
With shadows and nontrihedral angles, there are thousands of vertex labelings instead of 18, but there are also more constraints, so the constraint propagation actually does better than it does in our limited world.
Waltz's approach and the Huf f man-Clowes labels are covered in most introductory AI books, including Rich and Knight 1990, [Charniak and McDermott 1985](B9780080571157500285.xhtml#bb0175), and [Winston 1984](B9780080571157500285.xhtml#bb1405).
Waltz's original paper appears in *The Psychology of Computer Vision* ([Winston 1975](B9780080571157500285.xhtml#bb1400)), an influential volume collecting early work done at MIT.
He also contributed a summary article on Waltz filtering ([Waltz 1990](B9780080571157500285.xhtml#bb1305)).

Many introductory AI texts give vision short coverage, but [Charniak and McDermott (1985)](B9780080571157500285.xhtml#bb0175) and [Tanimoto (1990)](B9780080571157500285.xhtml#bb1220) provide good overviews of the field.
[Zucker (1990)](B9780080571157500285.xhtml#bb1450) provides an overview of low-level vision.

[Ramsey and Barrett (1987)](B9780080571157500285.xhtml#bb0975) give an implementation of a line-recognition program.
It would make a good project to connect their program to the one presented in this chapter, and thereby go all the way from pixels to 3-D descriptions.

## [ ](#){:#st0035}17.6 Exercises
{:#s0035}
{:.h1hd}

This chapter has solved the problem of line-labeling for polyhedra made of trihedral vertexes.
The following exercises extend this solution.

**Exercise 17.1 [h]** Use the line-labeling to produce a face labeling.
Write a function that takes a labeled diagram as input and produces a list of the faces (planes) that comprise the diagram.

**Exercise 17.2 [h]** Use the face labeling to produce a polyhedron labeling.
Write a function that takes a list of faces and a diagram and produces a list of polyhedra (blocks) that comprise the diagram.

**Exercise 17.3 [d]** Extend the system to include quad-hedral vertexes and/or shadows.
There is no conceptual difficulty in this, but it is a very demanding task to find all the possible vertex types and labelings for them.
Consult [Waltz 1975](B9780080571157500285.xhtml#bb1300).

**Exercise 17.4 [d]** Implement a program to recognize lines from pixels.

**Exercise 17.5 [d]** If you have access to a workstation with a graphical interface, implement a program to allow a user to draw diagrams with a mouse.
Have the program generate output in the form expected by `construct-diagram`

