# Chapter 17
## Line-Diagram Labeling by Constraint Satisfaction

> It is wrong to think of Waltz's work only as a statement of the epistemology of line drawings of polyhedra.
Instead I think it is an elegant case study of a paradigm we can expect to see again and again.

> -Patrick Winston

> The Psychology of Computer Vision (1975)

This book touches only the areas of AI that deal with abstract reasoning.
There is another side of AI, the field of *robotics,* that deals with interfacing abstract reasoning with the real world through sensors and motors.
A robot receives input from cameras, microphones, sonar, and touch-sensitive devices, and produces "ouput" by moving its appendages or generating sounds.
The real world is a messier place than the abstract worlds we have been covering.
A robot must deal with noisy data, faulty components, and other agents and events in the world that can affect changes in the environment.

Computer vision is the subfield of robotics that deals with interpreting visual information.
Low-level vision takes its input directly from a camera and detects lines, regions and textures.
We will not be concerned with this.
High-level vision uses the findings of the low-level component to build a three-dimensional model of the objects depicted in the scene.
This chapter covers one small aspect of high-level vision.

## 17.1 The Line-Labeling Problem
{:#s0010}
{:.h1hd}

In this chapter we look at the line-diagram labeling problem: Given a list of lines and the vertexes at which they intersect, how can we determine what the lines represent?
For example, given the nine lines in [figure 17.1](#f0010), how can we interpret the diagram as a cube?

| []()                                   |
|----------------------------------------|
| ![f17-01](images/chapter17/f17-01.jpg) |
| Figure 17.1: A Cube                    |

Before we can arrive at an interpretation, we have to agree on what the candidates are.
After all, [figure 17.1](#f0010) could be just a hexagon with three lines in the middle.
For the purposes of this chapter, we will consider only diagrams that depict one or more *polyhedra-*three-dimensional solid figures whose surfaces are flat faces bounded by straight lines.
In addition, we will only allow *trihedral* vertexes.
That is, each vertex must be formed by the intersection of three faces, as in the corner of a cube, where the top, front, and side of the cube come together.
A third restriction on diagrams is that no so-called *accidental* vertexes are allowed.
For example, [figure 17.1](#f0010) might be a picture of three different cubes hanging in space, which just happen to line up so that the edge of one is aligned with the edge of another from our viewpoint.
We will assume that this is not the case.

Given a diagram that fits these three restrictions, our goal is to identify each line, placing it in one of three classes:

1.  A convex line separates two visible faces of a polyhedron such that a line from one face to the other would lie inside the polyhedron.
It will be marked with a plus sign:+.
!!!(p) {:.numlist}

2.  A concave line separates two faces of two polyhedra such that a line between the two spaces would pass through empty space.
It will be marked with a minus sign:-.
!!!(p) {:.numlist}

3.  A boundary line denotes the same physical situation as a convex line, but the diagram is oriented in such a way that only one of the two faces of the polyhedron is visible.
Thus, the line marks the boundary between the polyhedron and the background.
It will be marked with an arrow:&rarr;.
Traveling along the line from the tail to the point of the arrow, the polyhedron is on the right, and the background is on the left.
!!!(p) {:.numlist}

[Figure 17.2](#f0015) shows a labeling of the cube using these conventions.
Vertex A is the near corner of the cube, and the three lines coming out of it are all convex lines.
Lines GD and DF are concave lines, indicating the junction between the cube and the surface on which it is resting.
The remaining lines are boundary lines, indicating that there is no physical connection between the cube and the background there, but that there are other sides of the cube that cannot be seen.

| []()                                   |
|----------------------------------------|
| ![f17-02](images/chapter17/f17-02.jpg) |
| Figure 17.2: A Line-labeled Cube       |

The line-labeling technique developed in this chapter is based on a simple idea.
First we enumerate all the possible vertexes, and all the possible labelings for each vertex.
It turns out there are only four different vertex types in the trihedral polygon world.
We call them L, Y, W, and T vertexes, because of their shape.
The Y and W vertexes are also known as forks and arrows, respectively.
The vertexes are listed in [figure 17.3](#f0020).
Each vertex imposes some constraints on the lines that compose it.
For example, in a W vertex, the middle line can be labeled with a + or -, but not with an arrow.

| []()                                          |
|-----------------------------------------------|
| ![f17-03](images/chapter17/f17-03.jpg)        |
| Figure 17.3: The Possible Vertexes and Labels |

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

[Figure 17.4](#f0025) illustrates this process.
On the left we start with a cube.
All vertexes have all possible labelings, except that we know line GD is concave (-), indicating that the cube is resting on a surface.
This constrains vertex D in such a way that line DA must be convex (+).
In the middle picture the constraint on vertex D has propagated to vertex A, and in the right-hand picture it propagates to vertex B.
Soon, the whole cube will be uniquely labeled.

| []()                                   |
|----------------------------------------|
| ![f17-04](images/chapter17/f17-04.jpg) |
| Figure 17.4: Propagating Constraints   |

Many diagrams will be labeled uniquely by this constraint propagation process.
Some diagrams, however, are ambiguous.
They will still have multiple labelings after constraint propagation has finished.
In this case, we can search for a solution.
Simply choose an ambiguous vertex, choose one of the possible labelings for that vertex, and repeat the constraint propagation/search process.
Keep going until the diagram is either unambiguous or inconsistent.

That completes the sketch of the line-labeling algorithm.
We are now ready to implement a labeling program.
It's glossary is in [figure 17.5](#f0030).

| []()                                                |
|-----------------------------------------------------|
| ![f17-05](images/chapter17/f17-05.jpg)              |
| Figure 17.5: Glossary for the Line-Labeling Program |

*(ed: should be a markdown table)*

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

```lisp
(defstruct diagram "A diagram is a list of vertexes." vertexes)
(defstruct (vertex (:print-function print-vertex))
  (name      nil :type atom)
  (type        'L :type (member L Y W T))
  (neighbors nil :type list)  ; of vertex
  (labelings nil :type list)) ; of lists of (member + - L R)))))
```

An ambiguous vertex will have several labelings, while an unambiguous vertex has exactly one, and a vertex with no labelings indicates an impossible diagram.
Initially we don't know which vertexes are what, so they all start with several possible labelings.
Note that a labeling is a list, not a set: the order of the labels is significant and matches the order of the neighboring vertexes.
The function possible-labelings gives a list of all possible labelings for each vertex type.
We use R and L instead of arrows as labels, because the orientation of the arrows is significant.
An R means that as you travel from the vertex to its neighbor, the polyhedron is on the right and the background object is on the left.
Thus, an R is equivalent to an arrow pointing away from the vertex.
The L is just the reverse.

```lisp
(defun ambiguous-vertex-p (vertex)
  "A vertex is ambiguous if it has more than one labeling."
  (> (number-of-labelings vertex) 1))
(defun number-of-labelings (vertex)
  (length (vertex-labelings vertex)))
(defun impossible-vertex-p (vertex)
  "A vertex is impossible if it has no labeling."
  (null (vertex-labelings vertex)))
(defun impossible-diagram-p (diagram)
  "An impossible diagram is one with an impossible vertex."
  (some #'impossible-vertex-p (diagram-vertexes diagram)))
(defun possible-labelings (vertex-type)\
  "The list of possible labelings for a given vertex type."
  ;; In these labelings, R means an arrow pointing away from
  ;; the vertex, L means an arrow pointing towards it.
    (case vertex-type
    ((L) '((R L) (L R) (+ R) (L +) (- L) (R -)))
    ((Y) '((+ + +) ( ) (L R -) (- L R) (R - L)))
    ((T) '((R L +) (R L -) (R L L) (R L R)))
    ((W) '((L R +) (- - +) (+ + -)))))
```

## 17.2 Combining Constraints and Searching
{:#s0015}
{:.h1hd}

The main function `print-labelings` takes a diagram as input, reduces the number of labelings on each vertex by constraint propagation, and then searches for all consistent interpretations.
Output is printed before and after each step.

```lisp
(defun print-labelings (diagram)
  "Label the diagram by propagating constraints and then
```

`  searching for solutions if necessary.
Print results."`

```lisp
  (show-diagram diagram "~&The initial diagram is:")
  (every #'propagate-constraints (diagram-vertexes diagram))
  (show-diagram diagram
              "~2&After constraint propagation the diagram is:")
  (let* ((solutions (if (impossible-diagram-p diagram)
                  nil
                  (search-solutions diagram)))
        (n (length solutions)))
    (unless (= n 1)
      (format t "~2&There are ~  r solution~:p:" n)
      (mapc #'show-diagram solutions)))
  (values))
```

The function `propagate-constraints` takes a vertex and considers the constraints imposed by neighboring vertexes to get a list of all the `consistent-labelings` for the vertex.
If the number of consistent labelings is less than the number before we started, then the neighbors' constraints have had an effect on this vertex, so we propagate the new-found constraints on this vertex back to each neighbor.
The function returns nil and thus immediately stops the propagation if there is an impossible vertex.
Otherwise, propagation continues until there are no more changes to the labelings.

The whole propagation algorithm is started by a call to `every in print-labelings,` which propagates constraints from each vertex in the diagram.
But it is not obvious that this is all that is required.
After propagating from each vertex once, couldn't there be another vertex that needs relabeling?
The only vertex that could possibly need relabeling would be one that had a neighbor changed since its last update.
But any such vertex would have been visited by `propagate-constraint,` since we propagate to all neighbors.
Thus, a single pass through the vertexes, compounded with recursive calls, will find and apply all possible constraints.

The next question worth asking is if the algorithm is guaranteed to terminate.
Clearly, it is, because `propagate-constraints` can only produce recursive calls when it removes a labeling.
But since there are a finite number of labelings initially (no more than six per vertex), there must be a finite number of calls to `propagate-constraints.`

```lisp
(defun propagate-constraints (vertex)
  "Reduce the labelings on vertex by considering neighbors.
  If we can reduce, propagate the constraints to each neighbor.
  ;; Return nil only when the constraints lead to an impossibility
  (let ((old-num (number-of-labelings vertex)))
    (setf (vertex-labelings vertex) (consistent-labelings vertex))
    (unless (impossible-vertex-p vertex)
      (when (< (number-of-labelings vertex) old-num)
        (every #'propagate-constraints (vertex-neighbors vertex)))
      t)))
```

The function `consistent-labelings` is passed a vertex.
It gets all the labels for this vertex from the neighboring vertexes, collecting them in `neighbor-labels`.
It then checks all the labels on the current vertex, keeping only the ones that are consistent with all the neighbors' constraints.
The auxiliary function `labels-for` finds the labels for a particular neighbor at a vertex, and reverse-1 abel accounts for the fact that L and R labels are interpreted with respect to the vertex they point at.

```lisp
(defun consistent-labelings (vertex)
  "Return the set of labelings that are consistent with neighbors."
  (let ((neighbor-labels
          (mapcar #'(lambda (neighbor) (labels-for neighbor vertex))
              (vertex-neighbors vertex))))
```

`    :: Eliminate labelings that don't have` all `lines consistent`

```lisp
    :: with the corresponding line's label from the neighbor.
    :: Account for the L-R mismatch with reverse-label.
    (find-all-if
      #'(lambda (labeling)
          (every #'member (mapcar #'reverse-label labeling)
              neighbor-labels))
      (vertex-labelings vertex))))
```

Constraint propagation is often sufficient to yield a unique interpretation.
But sometimes the diagram is still underconstrained, and we will have to search for solutions.
The function `search-solutions` first checks to see if the diagram is ambiguous, by seeing if it has an ambiguous vertex, v.
If the diagram is unambiguous, then it is a solution, and we return it (in a list, `since search-solutions` is designed to return a list of all solutions).
Otherwise, for each of the possible labelings for the ambiguous vertex, we create a brand new copy of the diagram and set v's labeling in the copy to one of the possible labelings.
In effect, we are guessing that a labeling is a correct one.
We call `propagate-constraints;` if it falls, then we have guessed wrong, so there are no solutions with this labeling.
But if it succeeds, then we call `search-solutions` recursively to give us the list of solutions generated by this labeling.

```lisp
(defun search-solutions (diagram)
```

`  "Try` all `labelings for one ambiguous vertex.
and pro pagate."`

```lisp
  :: If there is no ambiguous vertex, return the diagram.
```

`  :: If there is one.
make copies of the diagram trying each of`

`  :: the possible labelings.
Propagate constraints and append`

`  ::` all `the solutions together.`

```lisp
  (let ((v (find-if #'ambiguous-vertex-p
                        (diagram-vertexes diagram))))
    (if (null v)
          (list diagram)
          (mapcan
            #'(lambda (v-labeling)
  (let* ((diagram2 (make-copy-diagram diagram))
      (v2 (find-vertex (vertex-name v) diagram2)))
  (setf (vertex-labelings v2) (list v-labeling))
  (if (propagate-constraints v2)
      (search-solutions diagram2)
      nil)))
(vertex-labelings v)))))
```

That's all there is to the algorithm; all that remains are some auxiliary functions.
Here are three of them:

```lisp
(defun labels-for (vertex from)
  "Return all the labels for the line going to vertex."
  (let ((pos (position from (vertex-neighbors vertex))))
    (mapcar #'(lambda (labeling) (nth pos labeling))
            (vertex-labelings vertex))))
(defun reverse-label (label)
  "Account for the fact that one vertex's right is another's left."
  (case label (L 'R) (R 'L) (otherwise label)))
(defun find-vertex (name diagram)
  "Find the vertex in the given diagram with the given name."
  (find name (diagram-vertexes diagram) :key #'vertex-name))
```

Here are the printing functions.
`print-vertex` prints a vertex in short form.
It obeys the `print` convention of returning the first argument.
The functions `show-vertex` and `show-diagram` print more de tailed forms.
They obey the convention f or `describe`-like functions of returning no values at all.

```lisp
(defun print-vertex (vertex stream depth)
  "Print a vertex in the short form."
```

`  (d`e`clar`e `(ignore depth))`

```lisp
  (format stream "~a/~d" (vertex-name vertex)
            (number-of-labelings vertex))
  vertex)
(defun show-vertex (vertex &optional (stream t))
  "Print a vertex in a long form, on a new line."
  (format stream "~&~a ~d:" vertex (vertex-type vertex))
  (mapc #'(lambda (neighbor labels)
            (format stream " ~a~a=[~{~a~}]" (vertex-name vertex)
                    (vertex-name neighbor) labels))
      (vertex-neighbors vertex)
      (matrix-transpose (vertex-labelings vertex)))
  (values))
(defun show-diagram (diagram &optional (title "~2&Diagram:")
                              (stream t))
```

`  "Print a diagram in a long form.
Include a title."`

```lisp
  (format stream title)
  (mapc #'show-vertex (diagram-vertexes diagram))
  (let ((n (reduce #'* (mapcar #'number-of-labelings
                                (diagram-vertexes diagram)))))
  (when (> n 1)
    (format stream "~&For ~:d interpretation  ~:p." n))
  (values)))
```

`Note` that `matrix-transpose` is called by `show-vertex` to turn the matrix of labelings on its side.
It works like this:

```lisp
(possible-labelings 'Y)
((+ + +)
  (- - -)
  (L R -)
  (- L R)
  (R - L))
(matrix-transpose (possible-labelings 'Y))
((+ - L - R)
  (+ - R L -)
  (+ - - R L))
```

The implementation of `matrix-transpose` is surprisingly concise.
It is an old Lisp trick, and well worth understanding:

```lisp
(defun matrix-transpose (matrix)
  "Turn a matrix on its side."
  (if matrix (apply #'mapcar #'list matrix)))
```

The remaining code has to do with creating diagrams.
We need some handy way of specifying diagrams.
One way would be with a line-recognizing program operating on digitized input from a camera or bitmap display.
Another possibility is an interactive drawing program using a mouse and bitmap display.
But since there is not yet a Common Lisp standard for interacting with such devices, we will have to settle for a textual description.
The macro `defdiagram` defines and names a diagram.
The name is followed by a list of vertex descriptions.
Each description is a list consisting of the name of a vertex, the vertex type (Y, A, L, or T), and the names of the neighboring vertexes.
Here again is the `defdiagram` description for the cube shown in [figure 17.6](#f0035).

| []()                                   |
|----------------------------------------|
| ![f17-06](images/chapter17/f17-06.jpg) |
| Figure 17.6: A Cube                    |

```lisp
(defdiagram cube
  (a Y b c d)
  (b W g e a)
  (c W e f a)
  (d W f g a)
  (e L c b)
  (f L d c)
  (g L b d))
```

The macro `defdiagram` calls `construct-diagram` to do the real work.
It would be feasible to have `defdiagram` expand into a `defvar,` making the names be special variables.
But then it would be the user`'s` responsibility to make copies of such a variable before passing it to a destructive function.
Instead, I use `put-diagram` and `diagram` to put and get diagrams in a table, `diagram` retrieves the named diagram and makes a copy of it.
Thus, the user cannot corrupt the original diagrams stored in the table.
Another possibility would be to have `defdiagram` expand into a function definition for `name` that returns a copy of the diagram.
I chose to keep the diagram name space separate from the function name space, since names like `cube` make sense in both spaces.

```lisp
(defmacro defdiagram (name &rest vertex-descriptors)
```

`  "Define a diagram.
A copy can be gotten by (diagram name)."`

```lisp
  '(put-diagram '.name (construct-diagram '.vertex-descriptors)))
(let ((diagrams (make-hash-table)))
(defun diagram (name)
  "Get a fresh copy of the diagram with this name."
  (make-copy-diagram (gethash name diagrams)))
(defun put-diagram (name diagram)
  "Store a diagram under a name."
  (setf (gethash name diagrams) diagram)
  name))
```

The function `construct-diagram` translates each vertex description, using `construct-vertex`, and then fills in the neighbors of each vertex.

```lisp
(defun construct-diagram (vertex-descriptors)
  "Build a new diagram from a set of vertex descriptor."
  (let ((diagram (make-diagram)))
    :: Put in the vertexes
    (setf (diagram-vertexes diagram)
          (mapcar #'construct-vertex vertex-descriptors))
    :: Put in the neighbors for each vertex
    (dolist (v-d vertex-descriptors)
      (setf (vertex-neighbors (find-vertex (first v-d) diagram))
            (mapcar #'(lambda (neighbor)
                    (find-vertex neighbor diagram))
                  (v-d-neighbors v-d))))
    diagram))
(defun construct-vertex (vertex-descriptor)
  "Build the vertex corresponding to the descriptor."
  :: Descriptors are like: (x L y z)
  (make-vertex
    :name (first vertex-descriptor)
    :type (second vertex-descriptor)
    :labelings (possible-labelings (second vertex-descriptor))))
(defun v-d-neighbors (vertex-descriptor)
  "The neighboring vertex names in a vertex descriptor."
  (rest (rest vertex-descriptor)))
```

The `defstruct` for `diagram` automatically creates the function `copy-diagram,` but it just copies each field, without copying the contents of each field.
Thus we need `make-copy-diagram` to create a copy that shares no structure with the original.

```lisp
(defun make-copy-diagram (diagram)
  "Make a copy of a diagram, preserving connectivity."
  (let* ((new (make-diagram
            :vertexes (mapcar #'copy-vertex
                              (diagram-vertexes diagram)))))
    :: Put in the neighbors for each vertex
    (dolist (v (diagram-vertexes new))
      (setf (vertex-neighbors v)
              (mapcar #'(lambda (neighbor)
                        (find-vertex (vertex-name neighbor) new))
                    (vertex-neighbors v))))
    new))
```

## 17.3 Labeling Diagrams
{:#s0020}
{:.h1hd}

We are now ready to try labeling diagrams.
First the cube:

```lisp
> (print-labelings (diagram 'cube))
The initial diagram is:
  A/5 Y: AB=[+-L-R] AC=[+-RL-] AD=[+--RL]
  B/3 W: BG=[L-+] BE=[R-+] BA=[++-]
  C/3 W: CE=[L-+] CF=[R-+] CA=[++-]
  D/3 W: DF=[L-+] DG=[R-+] DA=[++-]
  E/6 L: EC=[RL+L-R] EB=[LRR+L-]
  F/6 L: FD=[RL+L-R] FC=[LRR+L-]
  G/6 L: GB=[RL+L-R] GD=[LRR+L-]
```

`For 29,160 interpr`e`tations.`

```lisp
After constraint propagation the diagram is:
  A/1 Y: AB=[+] AC=[+] AD=[+]
  B/2 W: BG=[L-] BE=[R-] BA=[++]
  C/2 W: CE=[L-] CF=[R-] CA=[++]
  D/2 W: DF=[L-] DG=[R-] DA=[++]
  E/3 L: EC=[R-R] EB=[LL-]
  F/3 L: FD=[R-R] FC=[LL-]
  G/3 L: GB=[R-R] GD=[LL-]
```

`For 216 interpr`e`tations.`

```lisp
There are four solutions:
Diagram:
  A/1 Y: AB=[+] AC=[+] AD=[+]
  B/1 W: BG=[L] BE=[R] BA=[+]
  C/l W: CE=[L] CF=[R] CA=[+]
  D/1 W: DF=[L] DG=[R] DA=[+]
  E/l L: EC=[R] EB=[L]
  F/1 L: FD=[R] FC=[L]
  G/1 L: GB=[R] GD=[L]
  Diagram:
  A/1 Y: AD=[+] AC=[+] AD=[+]
  B/1 W: BG=[L] BE=[R] BA=[+]
  C/l W: CE=[L] CF=[R] CA=[+]
  D/1 W: DF=[-] DG=[-] DA=[+]
  E/l L: EC=[R] EB=[L]
  F/1 L: FD=[-] FC=[L]
  G/1 L: GB=[R] GD=[-]
Diagram:
  A/1 Y: AB=[+] AC=[+] AD=[+]
  B/1 W: BG=[L] BE=[R] BA=[+]
  C/l W: CE=[-] CF=[-] CA=[+]
  D/1 W: DF=[L] DG=[R] DA=[+]
  E/l L: EC=[-] EB=[L]
  F/1 L: FD=[R] FC=[-]
  G/1 L: GB=[R] GD=[L]
Diagram:
  A/1 Y: AB=[+] AC=[+] AD=[+]
  B/1 W: BG=[-] BE=[-] BA=[+]
  C/1 W: CE=[L] CF=[R] CA=[+]
  D/1 W: DF=[L] DG=[R] DA=[+]
  E/1 L: EC=[R] EB=[-]
  F/1 L: FD=[R] FC=[L]
  G/1 L: GB=[-] GD=[L]
```

The four interpretations correspond, respectively, to the cases where the cube is free floating, attached to the floor (GD and DF = -), attached to a wall on the right (EC and CF = -), or attached to a wall on the left (BG and BE = -).
These are shown in [figure 17.7](#f0040).
It would be nice if we could supply information about where the cube is attached, and see if we can get a unique interpretation.
The function ground takes a diagram and modifies it by making one or more lines be grounded lines-lines that have a concave (-) label, corresponding to a junction with the ground.

| []()                                          |
|-----------------------------------------------|
| ![f17-07](images/chapter17/f17-07.jpg)        |
| Figure 17.7: Four Interpretations of the Cube |

```lisp
(defun ground (diagram vertex-a vertex-b)
  "Attach the line between the two vertexes to the ground.
  That is, label the line with a -"
  (let* ((A (find-vertex vertex-a diagram))
        (B (find-vertex vertex-b diagram))
        (i (position B (vertex-neighbors A))))
    (assert (not (null i)))
    (setf (vertex-labelings A)
        (find-all-if #'(lambda (1) (eq (nth i 1) '-))
                (vertex-labelings A)))
    diagram))
```

We can see how this works on the cube:

```lisp
> (print-labelings (ground (diagram 'cube) 'g 'd))
The initial diagram is:
  A/5 Y: AB=[+-L-R] AC=[+-RL-] AD=[+--RL]
  B/3 W: BG=[L-+] BE=[R-+] BA=[++-]
  C/3 W: CE=[L-+] CF=[R-+] CA=[++-]
  D/3 W: DF=[L-+] DG=[R-+] DA=[++-]
  E/6 L: EC=[RL+L-R] EB[LRR+L-]
  F/6 L: FD=[RL+L-R] FC=[LRR+L-]
  G/1 L: GB=[R] GD=[-]
```

`For 4,860 interpr`e`tations.`

```lisp
After constraint propagation the diagram is:
  A/1 Y: AB=[+] AC=[+] AD=[+]
  B/l W: BG=[L] BE=[R] BA=[+]
  C/l W: CE=[L] CF=[R] CA=[C  +]
  D/l W: DF=[-] DG=[-] DA=[+]
  E/l L: EC=[R] EB=[L]
  F/1 L: FD=[-] FC=[L]
  G/1 L: GB=[R] GD=[-]
```

Note that the user only had to specify one of the two ground lines, GD.
The program found that DF is also grounded.
Similarly, in programming `ground-line`, we only had to update one of the vertexes.
The rest is done by constraint propagation.

The next example yields the same four interpretations, in the same order (free floating, attached at bottom, attached at right, and attached at left) when interpreted ungrounded.
The grounded version yields the unique solution shown in the following output and in [figure 17.9](#f0050).

| []()                                   |
|----------------------------------------|
| ![f17-08](images/chapter17/f17-08.jpg) |
| Figure 17.8: Cube on a Plate           |

| []()                                   |
|----------------------------------------|
| ![f17-09](images/chapter17/f17-09.jpg) |
| Figure 17.9: Labeled Cube on a Plate   |

```lisp
(defdiagram cube-on-plate
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
  (m L k i))
> (print-labelings (ground (diagram 'cube-on-plate) 'k 'm))
The initial diagram is:
  A/5 Y: AB=[+-L-R] AC=[+-RL-] AD=[+--RL]
  B/3 W: BG=[L-+] BE=[R-+] BA=[++-]
  C/3 W: CE=[L-+] CF=[R-+] CA=[++-]
  D/3 W: DF=[L-+] DG=[R-+] DA=[++-]
  E/6 L: EC=[RL+L-R] EB=[LRR+L-]
  F/5 Y: FD=C+-L-R] FC=[+-RL-] FI=[+--RL]
  G/5 Y: GB=[+-L-R] GD=[+-RL-] GH=[+--RL]
  H/3 W: HL=[L-+] HG=[R-+] HJ=[++-]
  I/3 W: IF=[L-+] IM=[R-+] IJ=[++-]
  J/5 Y: JH=[+-L-R] JI=[+-RL-] JK=[+--RL]
  K/1 W: KM=[-] KL=[-] KJ=[+]
  L/6 L: LH=[RL+L-R] LK=[LRR+L-]
  M/6 L: MK=[RL+L-R] MI=[LRR+L-]
```

`For 32.805.000 interpr`e`tations.`

```lisp
After constraint propagation the diagram is
  A/1 Y: AB=[+] AC=[+] AD=[+]
  B/2 W: BG=[L-] BE=[R-] BA=[++]
  C/2 W: CE=[L-] CF=[R-] CA=[++]
  D/2 W: DF=[L-] DG=[R-] DA=[++]
  E/1 L: EC=[R] EB=[L]
  F/1 Y: FD=[-] FC=[L] FI=[R]
  G/1 Y: GB=[R] GD=[-] GH=[L]
  H/1 W: HL=[L] HG=[R] HJ=[+]
  I/1 W: IF=[L] IM=[R] IJ=[+]
  J/1 Y: JH=[+] JI=[+] JK=[+]
  K/1 W: KM=[-] KL=[-] KJ=[+]
  L/1 L: LH=[R] LK=[-]
  M/1 L: MK=[-] MI=[L]
```

It is interesting to try the algorithm on an "impossible" diagram.
It turns out the algorithm correctly finds no interpretation for this well-known illusion:

```lisp
(defdiagram poiuyt
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
  (l W h g c))
> (print-1 abel ings (diagram 'poiuyt))
The initial diagram is:
  A/6 L: AB=[RL+L-R] AG=[LRR+L-]
  B/6 L: BJ=[RL+L-R] BA=[LRR+L-]
  C/6 L: CD=[RL+L-R] CL=[LRR+L-]
  D/6 L: DH=[RL+L-R] DC=[LRR+L-]
  E/6 L: EF=[RL+L-R] EI=[LRR+L-]
  F/6 L: FK=[RL+L-R] FE=[LRR+L-]
  G/6 L: GA=[RL+L-R] GL=[LRR+L-]
  H/6 L: HL=[RL+L-R] HD=[LRR+L-]
  I/6 L: IE=[RL+L-R] IK=[LRR+L-]
  J/6 L: JK=[RL+L-R] JB=[LRR+L-]
  K/3 W: KJ=[L-+] KI=[R-+] KF=[++-]
  L/3 W: LH=[L-+] LG=[R-+] LC=[++-]
```

`For 544,195.584 interpr`e`tations.`

```lisp
After constraint propagation the diagram is:
  A/5 L: AB=[RL+-R] AG=[LRRL-]
  B/5 L: BJ=[RLL-R] BA=[LR+L-]
  C/2 L: CD=[LR] CL=[+-]
  D/3 L: DH=[RL-] DC=[LRL]
  E/3 L: EF=[RLR] EI=[LR-]
  F/2 L: FK=[+-] FE=[RL]
  G/4 L: GA=[RL-R] GL=[L+L-]
  H/4 L: HL=[R+-R] HD=[LRL-]
  I/4 L: IE=[RL-R] IK=[L+L-]
  J/4 L: JK=[R+-R] JB=[LRL-]
  K/3 W: KJ=[L-+] KI=[R-+] KF=[++-]
  L/3 W: LH=[L-+] LG=[R-+] LC=[++-]
```

`For 2,073,600 interpr`e`tations.`

`There are z`e`ro solutions:`

Now we try a more complex diagram:

```lisp
(defdiagram tower
  (a Y b c d)    (n L q o)
  (b W g e a)    (o W y j n)
  (c W e f a)    (P L r i)
  (d W f g a)    (q W n s w)
  (e L c b)      (r W s p x)
  (f Y d c i)    (s L r q)
  (g Y b d h)    (t W w x z)
  (h W l g J)    (u W x y z)
  (i W f m p)    (v W y w z)
  (j Y h o k)    (w Y t v q)
  (k W m l j)    (x Y r u t)
  (l L h k)      (y Y v u o)
  (m L k i)      (z Y t u v))
> (print-labelings (ground (diagram 'tower) 'l 'k))
The initial diagram is:
  A/5 Y: AB=[+-L-R] AC=[+-RL-] AD=[+--RL]
  B/3 W: BG=[L-+] BE=[R-+] BA=[++-]
  C/3 W: CE=[L-+] CF=[R-+] CA=[++-]
  D/3 W: DF=[L-+] DG=[R-+] DA=[++-]
  E/6 L: EC[RL+L-R] EB=[LRR+L-]
  F/5 Y: FD=[+-L-R] FC=[+-RL-] FI=[+--RL]
  G/5 Y: GB=[+-L-R] GD=[+-RL-] GH=[+--RL]
  H/3 W: HL=[L-+] HG=[R-+] HJ=[++-]
  I/3 W: IF=[L-+] IM=[R-+] IP=[++-]
  J/5 Y: JH=[+-L-R] JO=[+-RL-] JK=[+--RL]
  K/3 W: KM=[L-+] KL=[R-+] KJ=[++-]
  L/1 L: LH=[R] LK=[-]
  M/6 L: MK=[RL+L-R] MI=[LRR+L-]
  N/6 L: NQ=[RL+L-R] NO=[LRR+L-]
  O/3 W: OY=[L-+] OJ=[R-+] ON=[++-]
  P/6 L: PR=[RL+L-R] PI=[LRR+L-]
  Q/3 W: QN=[L-+] QS=[R-+] QW=[++-]
  R/3 W: RS=[L-+] RP=[R-+] RX=[++-]
  S/6 L: SR=[RL+L-R] SQ=[LRR+L-]
```

`  T/3 W:` TW=[L-+] `TX=[R-+] TZ=[++-]`

```lisp
  U/3 W: UX=[L-+] UY=[R-+] UZ=[++-]
  V/3 W: VY=[L-+] VW=[R-+] VZ=[++-]
  W/5 Y: WT=[+-L-R] WV=[+-RL-] WQ=[+--RL]
  X/5 Y: XR=[+-L-R] XU=[+-RL-] XT=[+--RL]
  Y/5 Y: YV=[+-L-R] YU=[+-RL-] YO=[+--RL]
  Z/5 Y: ZT=[+-L-R] ZU=[+-RL-] ZV=[+--RL]
For 1,614,252,037,500,000 interpretations.
```

After constraint propagation the diagram is:

```lisp
  A/1 Y: AB=[+] AC=[+] AD=[+]
  B/l W: BG=[L] BE=[R] BA=[+]
  C/1 W: CE=[L] CF=[R] CA=[+]
  D/l W: DF=[-] DG=[-] DA=[+]
  E/1 L: EC=[R] EB=[L]
  F/1 Y: FD=[-] FC=[L] FI=[R]
  G/1 Y: GB=[R] GD=[-]GH=[L]
  H/1 W: HL=[L] HG=[R] HJ=[+]
  I/1 W: IF=[L] IM=[R] IP=[+]
  J/l Y: JH=[+] JO=[+] JK=[+]
  K/l W: KM=[-] KL=[-] KJ=[+]
  L/l L: LH=[R] LK=[-]
  M/1 L: MK=[-] MI=[L]
  N/l L: NQ=[R] NO[-]
  O/l W: OY=[+] OJ=[+] ON=[-]
  P/l L: PR=[L] PI=[+]
  Q/1 W: QN=[L] QS=[R] QW=[+]
  R/1 W: RS=[L] RP=[R] RX=[+]
  S/1 L: SR=[R] SQ=[L]
  T/1 W: TW=[+] TX=[+] TZ=[-]
  U/1 W: UX=[+] UY=[+] UZ=[-]
  V/l W: VY=[+] VW=[+] VZ=[-]
  W/l Y: WT=[+] WV=[+] WQ=[+]
  X/1 Y: XR=[+] XU=[+] XT=[+]
  Y/1 Y: YV=[+] YU=[+] YO=[+]
  Z/l Y: ZT=[-] ZU=[-] ZV=[-]
```

We see that the algorithm was able to arrive at a single interpretation.
Moreover, even though there were a large number of possibilities-over a quadrillion-the computation is quite fast.
Most of the time is spent printing, so to get a good measurement, we define a function to find solutions without printing anything:

```lisp
(defun find-labelings (diagram)
  "Return a list of all consistent labelings of the diagram."
  (every #'propagate-constraints (diagram-vertexes diagram))
  (search-solutions diagram))
```

When we time the application of `find-labelings` to the grounded tower and the poiuyt, we find the tower takes 0.11 seconds, and the poiuyt 21 seconds.
This is over 180 times longer, even though the poiuyt has only half as many vertexes and only about half a million interpretations, compared to the tower's quadrillion.
The poiuyt takes a long time to process because there are few local constraints, so violations are discovered only by considering several widely separated parts of the figure all at the same time.
It is interesting that the same fact that makes the processing of the poiuyt take longer is also responsible for its interest as an illusion.

## 17.4 Checking Diagrams for Errors
{:#s0025}
{:.h1hd}

This section considers one more example, and considers what to do when there are apparent errors in the input.
The example is taken from Charniak and McDermott's *Introduction to Artificial Intelligence*, page 138, and shown in [figure 17.12](#f0065).

| []()                                          |
|-----------------------------------------------|
| ![f17-10](images/chapter17/f17-10.jpg)        |
| Figure 17.10: An Impossible Figure (A Poiuyt) |

| []()                                   |
|----------------------------------------|
| ![f17-11](images/chapter17/f17-11.jpg) |
| Figure 17.11: A Tower                  |

| []()                                   |
| ---------------------------------------|
| ![f17-12](images/chapter17/f17-12.jpg) |
| Figure 17.12: Diagram of an arch       |

```lisp
(defdiagram arch
  (a W e b c)    (p L o q)
  (b L d a)      (q T P i r)
  (c Y a d g)    (r T j s q)
  (d Y c b m)    (s L r t)
  (e L a f)      (t W v s k)
  (f T e g n)    (u L t l)
  (g W h f c)    (v L t l)
  (h T g i o)    (w W x l y)
  (i T h j q)    (x L w z)
  (j T i k r)    (y Y w 2 z)
  (k T J l t)    (z W 3 x y)
  (l T k m v)    (l T n o w)
  (m L l d)      (2 W v 3 y)
  (n L f 1)      (3 L z 2)
  (o W P 1 h)    (4 T u l v))
```

Unfortunately, running this example results in no consistent interpretations after constraint propagation.
This seems wrong.
Worse, when we try to ground the diagram on the line XZ and call `print-labelings` on that, we get the following error:

```lisp
>>>ERROR: The first argument to NTH was of the wrong type.
```

`The function expected a fixnum >= z`e`ro.`

`While in the function LABELS-FOR`<= `CONSISTENT-LABELINGS`

```lisp
Debugger entered while in the following function:
```

`LABELS-FOR (P.C.
= 23)`

```lisp
  Arg 0 (VERTEX): U/6
  Arg 1 (FROM): 4/4
```

What has gone wrong?
A good guess is that the diagram is somehow inconsistent- somewhere an error was made in transcribing the diagram.
It could be that the diagram is in fact impossible, like the poiuyt.
But that is unlikely, as it is easy for us to provide an intuitive interpretation.
We need to debug the diagram, and it would also be a good idea to handle the error more gracefully.

One property of the diagram that is easy to check for is that every line should be mentioned twice.
If there is a line between vertexes A and B, there should be two entries in the vertex descriptors of the following form:

```lisp
(A ? ... B ...)
(B ? ... A ...)
```

Here the symbol "?" means we aren't concerned about the type of the vertexes, only with the presence of the line in two places.
The following code makes this check when a diagram is defined.
It also checks that each vertex is one of the four legal types, and has the right number of neighbors.

```lisp
(defmacro defdiagram (name &rest vertex-descriptors)
```

`  "Define a diagram.
A copy can be gotten by (diagram name)."`

```lisp
  '(put-diagram '.name (construct-diagram
                    (check-diagram ',vertex-descriptors))))
(defun check-diagram (vertex-descriptors)
  "Check if the diagram description appears consistent."
  (let ((errors 0))
    (dolist (v-d vertex-descriptors)
      :: v-d is like: (a Y b c d)
      (let ((A (first v-d))
                    (v-type (second v-d)))
        :: Check that the number of neighbors is right for
```

`        :: the vertex type (and that the vertex type is l`e`gal)`

```lisp
        (when (/= (length (v-d-neighbors v-d))
              (case v-type ((W Y T) 3) ((L) 2) (t -  1)))
```

`          (warn "Ill`e`gal type/neighbor combo: ~a" v-d)`

```lisp
          (incf errors))
        :: Check that each neighbor B is connected to
```

`        :: this vertex.
A, exactly once`

```lisp
          (dolist (B (v-d-neighbors v-d))
            (when (/= 1 (count-if
                #'(lambda (v-d2)
                  (and (eql (first v-d2) B)
                    (member A (v-d-neighbors v-d2))))
              vertex-descri ptors))
        (warn "Inconsistent vertex: ~a-~a" A B)
        (incf errors)))))
      (when (> errors 0)
```

`        (error "Inconsistent diagram.
~d total error~:p."`

```lisp
            errors)))
    vertex-descriptors)
```

Now let's try the arch again:

```lisp
(defdiagram arch
  (a W e b c)    (p L o q)
  (b L d a)      (q T p i r)
  (c Y a d g)    (r T j s q)
  (d Y c b m)    (s L r t)
  (e L a f)      (t W v s k)
  (f T e g n)    (u L t l)
  (g W h f c)    (v L 2 4)
  (h T g i o)    (w W x l y)
  (i T h j q)    (x L w z)
  (j T i k r)    (y Y w 2 z)
  (k T j l t)    (z W 3 x y)
  (l T k m v)    (1 T n o w)
  (m L l d)      (2 W v 3 y)
  (n L f 1)      (3 L z 2)
  (o W P 1 h)    (4 T u l v))
Warning: Inconsistent vertex: T-V
Warning: Inconsistent vertex: U-T
Warning: Inconsistent vertex: U-L
Warning: Inconsistent vertex: L-V
Warning: Inconsistent vertex: 4-U
Warning: Inconsistent vertex: 4-L
```

`>>ERROR: Inconsistent diagram.
6 total errors.`

The `defdiagram` was transcribed from a hand-labeled diagram, and it appears that the transcription has fallen prey to one of the oldest problems in mathematical notation: confusing a "u" with a "v." The other problem was in seeing the line U-L as a single line, when in fact it is broken up into two segments, U-4 and 4-L.
Repairing these bugs gives the diagram:

```lisp
(defdiagram arch
  (a W e b c)    (P L o q)
  (b L d a)      (q T P i r)
  (c Y a d g)    (r T j s q)
  (d Y c b m)    (s L r t)
  (e L a f)      (t W u s k)        *;t-u not t-v*
  (f T e g n)    (u L t 4)          *;u-4 not u-l*
  (g W h f c)    (v L 2 4)
  (h T g i o)    (w W x l y)
  (i T h j q)    (x L w z)
  (j T i k r)    (y Y w 2 z)
  (k T J l t)    (z W 3 x y)
  (l T k m 4)    (1 T n o w)          *;l-4 not l-v*
  (m L l d)      (2 W v 3 y)
  (n L f 1)      (3 L z 2)
  (o W P 1 h)    (4 T u l v))
```

This time there arenoerrorsdetected by `check-diagram,` butrunning `print-labelings` again still does not give a solution.
`To` get more information about which constraints are applied, `I` modified `propagate-constraints` to print out some information:

```lisp
(defun propagate-constraints (vertex)
  "Reduce the number of labelings on vertex by considering neighbors.
  If we can reduce, propagate the new constraint to each neighbor."
  :: Return nil only when the constraints lead to an impossibility
  (let ((old-num (number-of-labelings vertex)))
    (setf (vertex-labelings vertex) (consistent-labelings vertex))
    (unless (impossible-vertex-p vertex)
      (when (< (number-of-labelings vertex) old-num)
        (format t "~&; ~a: ~14a ~a" vertex ;***
                (vertex-neighbors vertex) ;***
                (vertex-labelings vertex)) ;***
        (every #'propagate-constraints (vertex-neighbors vertex)))
      vertex)))
```

Running the problem again gives the following trace:

```lisp
> (print-labelings (ground (diagram 'arch) 'x 'z))
The initial diagram is:
  A/3 W: AE=[L-+] AB-CR-+] AC=[++-]
  P/6 L: P0=[RL+L-R] PQ=[LRR+L-]
  B/6 L: BD=[RL+L-R] BA=[LRR+L-]
  Q/4 T: QP=[RRRR] QI=[LLLL] QR=[+-LR]
  C/5 Y: CA=[+-L-R] CD=[+-RL-] CG=[+--RL]
  R/4 T: RJ=[RRRR] RS=[LLLL] RQ=[+-LR]
  D/5 Y: DC=[+-L-R] DB=[+-RL-] DM=[+--RL]
  S/6 L: SR=[RL+L-R] ST=[LRR+L-]
  S/6 L: EA=[RL+L-R] EF=[LRR+L-]
  T/3 W: TU=[L-+] TS=[R-+] TK=[++-]
  F/4 T: FE=[RRRR] FG=[LLLL] FN=[+-LR]
  U/6 L: UT=[RL+L-R] U4=[LRR+L-]
  G/3 W: GH=[L-+] GF=[R-+] GC=[++-]
  V/6 L: V2=[RL+L-R] V4=[LRR+L-]
  H/4 T: HG=[RRRR] HI=[LLLL] Ho=[+-LR]
  W/3 W: WX=[L-+] W1=[R-+] WY=[++-]
  I/4 T: IH=[RRRR] IJ=[LLLL] IQ=[+-LR]
  X/1 L: XW=[R] XZ=[-]
  J/4 T: JI=[RRRR] JK=[LLLL] JR=[+-LR]
  Y/5 Y: YW=[+-L-R] Y2=[+-RL-] YZ=[+--RL]
  K/4 T: KJ=[RRRR] KL=[LLLL] KT=[+-LR]
  Z/3 W: Z3=[L-+] ZX=[R-+] ZY=[++-]
  L/4 T: LK=[RRRR] LM=[LLLL] L4=[+-LR]
  1/4 T: 1N=[RRRR] 10=[LLLL] 1 W=[+-LR]
  M/6 L: ML=[RL+L-R] MD=[LRR+L-]
  2/3 W: 2 V=[L-+] 23=[R-+] 2Y=[++-]
  N/6 L: NF=[RL+L-R] N1=[LRR+L-]
  3/6 L: 3Z=[RL+L-R] 32=[LRR+L-]
  0/3 W: 0P=[L-+] 01=[R-+] 0H=[++-]
  4/4 T: 4U=[RRRR] 4 L=[LLLL] 4 V=[+-LR]
For 2,888, 816, 545.234, 944,000 interpretations
: P/2: (0/3 Q/4)        ((R L) (- L))
: 0/1: (P/2 1/4 H/4)    ((L R +))
: P/1: (0/1 Q/4)        ((R L))
: 1/3: (N/6 0/1 W/3)    ((R L +) (R L -) (R L L))
: N/2: (F/4 1/3)        ((R L) (- L))
: F/2: (E/6 G/3 N/2)    ((R L -) (R L L))
: E/2: (A/3 F/2)      ((R L) (- L))
: A/2: (E/2 B/6 C/5)    ((L R +) (- - +))
: B/3: (D/5 A/2)      ((R L) (- L) (R -))
: D/3: (C/5 B/3 M/6)    ((- - -) (- L R) (R - L))
: W/1: (X/l 1/3 Y/5)    ((L R +))
: 1/1: (N/2 0/1 W/l)    ((R L L))
: Y/1: (W/l 2/3 Z/3)    ((+ + +))
: 2/2: (V/6 3/6 Y/1)    ((L R +) (- - +))
: V/3: (2/2 4/4)      ((R L) (- L) (R -))
: 4/2: (U/6 L/4 V/3)    ((R L -) (R L R))
: U/2: (T/3 4/2)      ((R L) (- L))
: T/2: (U/2 S/6 K/4)    ((L R +) (- - +))
: S/2: (R/4 T/2)      ((R L) (R -))
: K/1: (J/4 L/4 T/2)    ((R L +))
: J/1: (1/4 K/1 R/4)    ((R L L))
: I/1: (H/4 J/1 Q/4)    ((R L R))
: L/1: (K/l M/6 4/2)    ((R L R))
: M/2: (L/1 D/3)      ((R L) (R -))
: 3/3: (Z/3 2/2)      ((R L) (- L) (R -))
: Z/1 : (3/3 X/1 Y/1)    ((- - +))
: 3/1: (Z/l 2/2)    ((- L))
: 2/1: (V/3 3/1 Y/1)    ((L R +))
: V/2: (2/1 4/2)      ((R L) (R -))
After constraint propagation the diagram is:
  A/0 W:
  P/l L: P0=[R] PQ=CL]
  B/0 L:
  Q/4 T: QP=[RRRR] QI=[LLLL] QR=[+-LR]
  C/0 Y:
  R/4 T: RJ=[RRRR] RS=[LLLL] RQ=[+-LR]
  D/0 Y:
  S/2 L: SR=[RR] ST=[L-]
  E/2 L: EA=[R-] EF=[LL]
  T/2 W: TU=[L-] TS=CR-] TK=[++]
  F/2 T: FE=[RR] FG=[LL] FN=[-  L]
  U/2 L: UT=[R-] U4=[LL]
  G/0 W:
  V/2 L: V2=[RR] V4=[L-]
  H/0 T:
  W/l W: WX=[L] W1=[R] WY=[+]
  I/1 T: IH=[R] IJ=[L] IQ=[R]
  X/1 L: XW=[R] XZ=[-]
  J/1 T: JI=[R] JK=[L] JR=[L]
  Y/1 Y: YW=[+] Y2=[+] YZ=[+]
  K/1 T: KJ=[R] KL=[L] KT=[+]
  Z/1 W: Z3=[-] ZX=[-] ZY=[+]
  L/1 T: LK=[R] LM=[L] L4=[R]
  1/1 T: 1 N=[R] 10=[L] 1 W=[L]
  M/2 L: ML=[RR] MD=[L-]
  2/1 W: 2 V=[L] 23=[R] 2Y=[+]
  N/2 L: NF=[R-] N1=[LL]
  3/1 L: 3Z=[-] 32=[L]
  0/1 W: 0P=[L] 01=[R] 0H=[+]
  4/2 T: 4U=[RR] 4 L=[LL] 4 V=[-  R]
```

From the diagram after constraint propagation we can see that the vertexes A,B,C,D,G, and H have no interpretations, so they are a good place to look first for an error.
From the trace generated by `propagate-constraints` (the lines beginning with a semicolon), we see that constraint propagation started at P and after seven propagations reached some of the suspect vertexes:

```lisp
: A/2: (E/2 B/6 C/5)    ((L R +) (- - + ))
: B/3: (D/5 A/2)        ((R L) (- L) (R -))
: D/3: (C/5 B/3 M/6)    ((- - -) (- L R) (R - L))
```

A and B look acceptable, but look at the entry for vertex D.
It shows three interpretations, and it shows that the neighbors are C, B, and M.
Note that line DC, the first entry in each of the interpretations, must be either -, - or R.
But this is an error, because the "correct" interpretation has DC as a + line.
Looking more closely, we notice that D is in fact a W-type vertex, not a Y vertex as written in the definition.
We should have:

```lisp
(defdiagram arch
  (a W e b c)    (p L o q)
  (b L d a)      (q T p i r)
  (c Y a d g)    (r T j s q)
  (d W b m c)    (s L r t)          ;*d is a W, not Y*
  (e L a f)      (t W u s k)
  (f T e g n)    (u L t 4)
  (g W h f c)    (v L 2 4)
  (h T g i o)    (w W x 1 y)
  (i T h j q)    (x L w z)
  (j T i k r)    (y Y w 2 z)
  (k T J l t)    (z W 3 x y)
  (1 T k m 4)    (1 T n o w)
  (m L l d)      (2 W v 3 y)
  (n L f 1)      (3 L z 2)
  (o W P 1 h)    (4 T u l v))
```

By running the problem again and inspecting the trace output, we soon discover the real root of the problem: the most natural interpretation of the diagram is beyond the scope of the program!
There are many interpretations that involve blocks floating in air, but if we ground lines OP, TU and XZ, we run into trouble.
Remember, we said that we were considering trihedral vertexes only.
But vertex 1 would be a quad-hedral vertex, formed by the intersection of four planes: the top and back of the base, and the bottom and left-hand side of the left pillar.
The intuitively correct labeling for the diagram would have O1 be a concave (-) line and Al be an occluding line, but our repertoire of labelings for T vertexes does not allow this.
Hence, the diagram cannot be labeled consistently.

Let's go back and consider the error that came up in the first version of the diagram.
Even though the error no longer occurs on this diagram, we want to make sure that it won't show up in another case.
Here's the error:

```lisp
>>>ERROR: The first argument to NTH was of the wrong type.
```

`The function expected a fixnum >= z`e`ro.`

`While in the function LABELS-FOR`<= `CONSISTENT-LABELINGS`

```lisp
Debugger entered while in the following function:
```

`LABELS-FOR (P.C.
= 23)`

```lisp
      Arg 0 (VERTEX): U/6
      Arg 1 (FROM): 4/4
```

Looking at the definition of `labels-for`, we see that it is looking for the from vertex, which in this case is 4, among the neighbors of U.
It was not found, so pos became nil, and the function nth complained that it was not given an integer as an argument.
So this error, if we had pursued it earlier, would have pointed out that 4 was not listed as a neighbor of U, when it should have been.
Of course, we found that out by other means.
In any case, there is no bug here to fix-as long as a diagram is guaranteed to be consistent, the `labels-for` bug will not appear again.

This section has made two points: First, write code that checks the input as thoroughly as possible.
Second, even when input checking is done, it is still up to the user to understand the limitations of the program.

## 17.5 History and References
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

## 17.6 Exercises
{:#s0035}
{:.h1hd}

This chapter has solved the problem of line-labeling for polyhedra made of trihedral vertexes.
The following exercises extend this solution.

**Exercise  17.1 [h]** Use the line-labeling to produce a face labeling.
Write a function that takes a labeled diagram as input and produces a list of the faces (planes) that comprise the diagram.

**Exercise  17.2 [h]** Use the face labeling to produce a polyhedron labeling.
Write a function that takes a list of faces and a diagram and produces a list of polyhedra (blocks) that comprise the diagram.

**Exercise  17.3 [d]** Extend the system to include quad-hedral vertexes and/or shadows.
There is no conceptual difficulty in this, but it is a very demanding task to find all the possible vertex types and labelings for them.
Consult [Waltz 1975](B9780080571157500285.xhtml#bb1300).

**Exercise  17.4 [d]** Implement a program to recognize lines from pixels.

**Exercise  17.5 [d]** If you have access to a workstation with a graphical interface, implement a program to allow a user to draw diagrams with a mouse.
Have the program generate output in the form expected by `construct-diagram`

