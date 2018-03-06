# Chapter 17 {docsify-ignore}
<a id='page-564'></a>

Line-Diagram 
Labeling by Constraint 
Satisfaction 

It is wrong to think of Waltz's work only as a 
statement of the epistemology of line drawings of 
polyhedra. Instead I think it is an elegant case study 
of a paradigm we can expect to see again and again. 

- Patrick Winston 

The Psychology of Computer Vision (1975) 

I I 1 his book touches only the areas of AI that deal with abstract reasoning. There is another 

I side of AI, the field of robotics, that deals with interfacing abstract reasoning with the real 

JL world through sensors and motors. A robot receives input from cameras, microphones, 
sonar, and touch-sensitive devices, and produces "ouput" by moving its appendages or generating 
sounds. The real world is a messier place than the abstract worlds we have been covering. 
A robot must deal with noisy data, faulty components, and other agents and events in the world 
that can affect changes in the environment. 

<a id='page-565'></a>

Computer vision is the subfield of robotics that deals with interpreting visual 
information. Low-level vision takes its input directly from a camera and detects 
lines, regions and textures. We will not be concerned with this. High-level vision 
uses the findings of the low-level component to build a three-dimensional model of 
the objects depicted in the scene. This chapter covers one small aspect of high-level 
vision. 

17.1 The Line-Labeling Problem 
In this chapter we look at the line-diagram labeling problem: Given a list of lines and 
the Vertexes at which they intersect, how can we determine what the lines represent? 
For example, given the nine lines in figure 17.1, how can we interpret the diagram as 
a cube? 

Figure 17.1: A Cube 

Before we can arrive at an interpretation, we have to agree on what the candidates 
are. After all, figure 17.1 could be just a hexagon with three lines in the middle. For 
the purposes of this chapter, we will consider only diagrams that depict one or more 
po/y/zedra - three-dimensional solid figures whose surfaces are flat faces bounded by 
straight lines. In addition, we will only allow trihedral VERTEXES. That is, each vertex 
must be formed by the intersection of three faces, as in the corner of a cube, where 
the top, front, and side of the cube come together. A third restriction on diagrams is 
that no so-called accidental Vertexes are allowed. For example, figure 17.1 might be 
a picture of three different cubes hanging in space, which just happen to line up so 
that the edge of one is aligned with the edge of another from our viewpoint. We will 
assume that this is not the case. 

<a id='page-566'></a>

Given a diagram that fits these three restrictions, our goal is to identify each line, 
placing it in one of three classes: 

1. A convex line separates two visible faces of a polyhedron such that a line from 
one face to the other would lie inside the polyhedron. It will be marked with a 
plus sign: -h. 
2. A concave line separates two faces of two polyhedra such that a line between 
the two spaces would pass through empty space. It will be marked with a 
minus sign: -. 
3. A boundary line denotes the same physical situation as a convex line, but the 
diagram is oriented in such a way that only one of the two faces of the polyhedron 
is visible. Thus, the line marks the boundary between the polyhedron 
and the background. It will be marked with an arrow: - Traveling along the 
line from the tail to the point of the arrow, the polyhedron is on the right, and 
the background is on the left. 
Figure 17.2 shows a labeling of the cube using these conventions. Vertex A is 
the near corner of the cube, and the three lines coming out of it are all convex lines. 
Lines GD and DF are concave lines, indicating the junction between the cube and 
the surface on which it is resting. The remaining lines are boundary lines, indicating 
that there is no physical connection between the cube and the background there, but 
that there are other sides of the cube that cannot be seen. 

Figure 17.2: A Line-labeled Cube 

The line-labeling technique developed in this chapter is based on a simple idea. 
First we enumerate all the possible Vertexes, and all the possible labelings for each 

<a id='page-567'></a>

vertex. It turns out there are only four different vertex types in the trihedral polygon 
world. We call them L, Y, W, and . Vertexes, because of their shape. The Y and W 
Vertexes are also known as forks and arrows, respectively. The Vertexes are listed in 
figure 17.3. Each vertex imposes some constraints on the lines that compose it. For 
example, in a W vertex, the middle line can be labeled with a + or - but not with 
an arrow. 

NX ./ \y 

\ 
\ 
1 . 2 
1 

3 

3 

3 
V 2 

\ V 

(V L 1 2) (V Y 1 2 3) (V . 1 2 3) (VWL 23) 

Figure 17.3: The Possible Vertexes and Labels 

Each line connects two Vertexes, so it must satisfy both constraints. This suggests 
a simple algorithm for labeling a diagram based on constraint propagation: First, 
label each vertex with all the possible labelings for the vertex type. An L vertex has 
six possibilities, Y has five, . has four, and W has three. Next, pick a vertex, V. 
Consider a neighboring vertex, . (that is, . and V are connected by a line). . will 
also have a set of possible labelings. If . and V agree on the possible labelings for the 
line between them, then we have gained nothing. But if the intersection of the two 
possibility sets is smaller than V's possibility set, then we have found a constraint on 

<a id='page-568'></a>

the diagram. We adjust . and V's possible labehngs accordingly. Every time we add 
a constraint at a vertex, we repeat the whole process for all the neighboring Vertexes, 
to give the constraint a chance to propagate as far as possible. When every vertex 
has been visited at least once and there are no more constraints to propagate, then 
we are done. 

Figure 17.4 illustrates this process. On the left we start with a cube. All Vertexes 
have all possible labelings, except that we know line GD is concave (-), indicating that 
the cube is resting on a surface. This constrains vertex D in such a way that line DA 
must be convex (+). In the middle picture the constraint on vertex D has propagated 
to vertex A, and in the right-hand picture it propagates to vertex B. Soon, the whole 
cube will be uniquely labeled. 

Figure 17.4: Propagating Constraints 

Many diagrams will be labeled uniquely by this constraint propagation process. 
Some diagrams, however, are ambiguous. They will still have multiple labelings 
after constraint propagation has finished. In this case, we can search for a solution. 
Simply choose an ambiguous vertex, choose one of the possible labelings for that 
vertex, and repeat the constraint propagation/search process. Keep going until the 
diagram is either unambiguous or inconsistent. 

That completes the sketch of the line-labeling algorithm. We are now ready to 
implement a labeling program. It's glossary is in figure 17.5. 

The two main data structures are the di agram and the vertex. It would have been 
possible to implement a data type for 1 i nes, but it is not necessary: lines are defined 
implicitly by the two Vertexes at their end points. 

A diagram is completely specified by its list of Vertexes, so the structure di agram 
needs only one slot. A vertex, on the other hand, is a more complex structure. Each 
vertex has an identifying name (usually a single letter), a vertex type (L, Y, W, or T), a 

<a id='page-569'></a>

Top-Level Functions 
print-labelings Label the diagram by propagating constraints and then searching. 

Data Types 
diagram A diagram is a list of VERTEXES. 
vertex A vertex has a name, type, and list of neighbors and labelings. 

Major Functions 
find-labelings Do the same constraint propagation, but don't print anything. 
propagate-constraints Reduce the number of labelings on vertex by considering neighbors. 
consistent-labelings Return the set of labelings that are consistent with neighbors. 
search-solutions Try all labelings for one ambiguous vertex, and propagate. 
defdiagram (macro) Define a diagram. 
diagram Retrieve a diagram stored by name. 
ground Attach the line between the two VERTEXES to the ground. 

Auxiliary Functions 
labels-for Return all the labels for the line going to vertex. 
reverse -label Reverse left and right on arrow labels. 
ambiguous-vertex -p A vertex is ambiguous if it has more than one labeling. 
number-of-labelings Number of labels on a vertex. 
find-vertex Find the vertex with the given name. 
matrix-transpose Turn a matrix on its side. 
possible-labelings The list of possible labelings for a given vertex type. 
print-vertex Print a vertex in the short form. 
show-vertex Print a vertex in a long form, on a new line. 
show-diagram Print a diagram in a long form. Include a title. 
construct-diagram Build a new diagram from a set of vertex descriptions. 
construct-vertex Build a new vertex from a vertex description. 
make-copy-diagram Make a copy of a diagram, preserving connectivity. 
check-diagram Check if the description appears consistent. 

Figure 17.5: Glossary for the Line-Labeling Program 

list of neighboring Vertexes, and a list of possible labelings. A labeling is a list of line 
labels. For example, a Y vertex will initially have a list of five possible labelings. If it 
is discovered that the vertex is the interior of a concave corner, then it will have the 
single labeling ( -- -). We give type information on the slots of vertex because it 
is a compUcated data type. The syntax of defstruct is such that you cannot specify 

a : type without first specifying a default value. We chose L as the default value for 
the type slot at random, but note that it would have been an error to give nil as the 
default value, because ni1 is not of the right type. 
(defstruct diagram "A diagram is a list of Vertexes." Vertexes) 

(defstruct (vertex (:print-function print-vertex)) 
(name nil :type atom) 
(type 'L :type (member L Y W T)) 
(neighbors nil :type list) ; of vertex 
(labelings nil :type list)) ; of lists of (member + -L R))))) 

<a id='page-570'></a>

An ambiguous vertex will have several labelings, while an unambiguous vertex has 
exactly one, and a vertex with no labelings indicates an impossible diagram. Initially 
we don't know which Vertexes are what, so they all start with several possible labelings. 
Note that a labeling is a list, not a set: the order of the labels is significant and 
matches the order of the neighboring Vertexes. The function possi bl e-1 abel i ngs 
gives a list of all possible labelings for each vertex type. We use R and L instead of 
arrows as labels, because the orientation of the arrows is significant. An R means 
that as you travel from the vertex to its neighbor, the polyhedron is on the right and 
the background object is on the left. Thus, an R is equivalent to an arrow pointing 
away from the vertex. The L is just the reverse. 

(defun ambiguous-vertex-p (vertex) 
"A vertex is ambiguous if it has more than one labeling." 
(> (number-of-labelings vertex) D) 

(defun number-of-labelings (vertex) 
(length (vertex-labelings vertex))) 

(defun impossible-vertex-p (vertex) 
"A vertex is impossible if it has no labeling." 
(null (vertex-labelings vertex))) 

(defun impossible-diagram-p (diagram) 
"An impossible diagram is one with an impossible vertex." 
(some #*impossible-vertex-p (diagram-Vertexes diagram))) 

(defun possible-labelings (vertex-type) 
"The list of possible labelings for a given vertex type." 
;; In these labelings, R means an arrow pointing away from 

the vertex, L means an arrow pointing towards it. 

(case vertex-type 
((L) '((R L) (L R) (+ R) (L +) (- L) (R -))) 
((Y) '((+ + +) ( ) (L R -) (- L R) (R - L))) 
((T) '((R L -H) (R L -) (R L L) (R L R))) 
((W) '((L R +) (-- +) (+ + -))))) 

17.2 Combining Constraints and Searching 
The main function print-1 abel ings takes a diagram as input, reduces the number 
of labelings on each vertex by constraint propagation, and then searches for all 
consistent interpretations. Output is printed before and after each step. 

<a id='page-571'></a>

(defun print-labelings (diagram) 
"Label the diagram by propagating constraints and then 
searching for solutions if necessary. Print results." 
(show-diagram diagram "~&The initial diagram is:") 
(every #*propagate-constraints (diagram-vertexes diagram)) 
(show-diagram diagram 

"~2&After constraint propagation the diagram is:") 

(let* ((solutions (if (impossible-diagram-p diagram) 
nil 
(search-solutions diagram))) 

(n (length solutions))) 

(unless (= . 1) 
(format t "~2&There are ~r solution~:p:" n) 
(mapc #'show-diagram solutions))) 

(values)) 
The function propagate-constraints takes a vertex and considers the constraints 
imposed by neighboring Vertexes to get a list of all the cons i stent -1 abel i ngs for the 
vertex. If the number of consistent labelings is less than the number before we started, 
then the neighbors' constraints have had an effect on this vertex, so we propagate the 
new-found constraints on this vertex back to each neighbor. The function returns 
nil and thus immediately stops the propagation if there is an impossible vertex. 
Otherwise, propagation continues until there are no more changes to the labelings. 
The whole propagation algorithm is started by a call to every in pri nt -1 abel i ngs, 
which propagates constraints from each vertex in the diagram. But it is not obvious 
that this is all that is required. After propagating from each vertex once, couldn't 
there be another vertex that needs relabeling? The only vertex that could possibly 
need relabeling would be one that had a neighbor changed since its last update. 
But any such vertex would have been visited by propagate-constraint, since we 
propagate to all neighbors. Thus, a single pass through the Vertexes, compounded 
with recursive calls, will find and apply all possible constraints. 
The next question worth asking is if the algorithm is guaranteed to terminate. 
Clearly, it is, because propagate-constra i nts can only produce recursive calls when 
it removes a labeling. But since there are a finite number of labelings initially (no more 
than six per vertex), there must be a finite number of calls topropagate-constraints. 
(defun propagate-constraints (vertex) 
"Reduce the labelings on vertex by considering neighbors. 
If we can reduce, propagate the constraints to each neighbor." 
Return nil only when the constraints lead to an impossibility 
(let ((old-num (number-of-labelings vertex))) 
(setf (vertex-labelings vertex) (consistent-labelings vertex)) 
(unless (impossible-vertex-p vertex) 
(when (< (number-of-labelings vertex) old-num) 
(every #'propagate-constraints (vertex-neighbors vertex))) 
t))) 

<a id='page-572'></a>

The function consi stent -1 abel i ngs is passed a vertex. It gets all the labels for this 
vertex from the neighboring Vertexes, collecting them in nei ghbor-1 abel s. It then 
checks all the labels on the current vertex, keeping only the ones that are consistent 
with all the neighbors' constraints. The auxiliary function labels-for finds the 
labels for a particular neighbor at a vertex, and reverse -1 abel accounts for the fact 
that L and R labels are interpreted with respect to the vertex they point at. 

(defun consistent-labelings (vertex) 
"Return the set of labelings that are consistent with neighbors." 
(let ((neighbor-labels 

(mapcar #'(lambda (neighbor) (labels-for neighbor vertex)) 
(vertex-neighbors vertex)))) 
Eliminate labelings that don't have all lines consistent 
;; with the corresponding line's label from the neighbor. 
Account for the L-R mismatch with reverse -label, 
(find-all-if 
#'(lambda (labeling) 
(every #'member (mapcar #'reverse-1abel labeling) 
neighbor-labels)) 
(vertex-labelings vertex)))) 

Constraint propagation is often sufficient to yield a unique interpretation. But sometimes 
the diagram is still underconstrained, and we will have to search for solutions. 
The function search-sol utions first checks to see if the diagram is ambiguous, by 
seeing if it has an ambiguous vertex, v. If the diagram is unambiguous, then it is a 
solution, and we return it (in a hst, since sea rch - sol ut i ons is designed to return a 
list of all solutions). Otherwise, for each of the possible labelings for the ambiguous 
vertex, we create a brand new copy of the diagram and set v's labeling in the copy to 
one of the possible labelings. In effect, we are guessing that a labeling is a correct one. 
We call propagate - const ra i nts; if it fails, then we have guessed wrong, so there are 
no solutions with this labeling. But if it succeeds, then we call sea rch-sol utions 
recursively to give us the list of solutions generated by this labeling. 

(defun search-solutions (diagram) 
"Try all labelings for one ambiguous vertex, and propagate." 

If there is no ambiguous vertex, return the diagram. 
;; If there is one, make copies of the diagram trying each of 
;; the possible labelings. Propagate constraints and append 
;; all the solutions together, 
(let ((v (find-if #'ambiguous-vertex-p 

(diagram-Vertexes diagram)))) 

(if (null V) 
(list diagram) 
(mapcan 

#'(lambda (v-labeling) 

<a id='page-573'></a>

(let* ((diagram2 (make-copy-diagram diagram)) 

(v2 (find-vertex (vertex-name v) diagram2))) 
(setf (vertex-labelings v2) (list v-labeling)) 
(if (propagate-constraints v2) 

(search-solutions diagram2) 
nil))) 
(vertex-labelings v))))) 

That's all there is to the algorithm; all that remains are some auxiliary functions. 
Here are three of them: 

(defun labels-for (vertex from) 
"Return all the labels for the line going to vertex." 
(let ((pos (position from (vertex-neighbors vertex)))) 

(mapcar #*(lambda (labeling) (nth pos labeling)) 
(vertex-labelings vertex)))) 

(defun reverse-label (label) 
"Account for the fact that one vertex's right is another's left." 
(case label (L 'R) (R 'D (otherwise label))) 

(defun find-vertex (name diagram) 
"Find the vertex in the given diagram with the given name." 
(find name (diagram-vertexes diagram) :key #'vertex-name)) 

Here are the printing functions, print - vertex prints a vertex in short form. It obeys 
the .r i .t convention of returning the first argument. The functions s how - ve r t ex and 
show-di agramprintmoredetailedforms. They obey theconventionfordescri be-like 
functions of returning no values at all. 

(defun print-vertex (vertex stream depth) 
"Print a vertex in the short form." 
(declare (ignore depth)) 
(format stream "~a/~d" (vertex-name vertex) 

(number-of-labelings vertex)) 
vertex) 

(defun show-vertex (vertex &optional (stream t)) 
"Print a vertex in a long form, on a new line." 
(format stream "~& "a "d:" vertex (vertex-type vertex)) 
(mapc #'(lambda (neighbor labels) 

(format stream " '"a~a=[''{~a''}]" (vertex-name vertex) 

(vertex-name neighbor) labels)) 
(vertex-neighbors vertex) 
(matrix-transpose (vertex-labelings vertex))) 

(values)) 

<a id='page-574'></a>

(defun show-diagram (diagram &optional (title "~2&Diagram:") 

(stream t)) 
"Print a diagram in a long form. Include a title. " 
(format stream title) 
(mapc #*show-vertex (diagram-vertexes diagram)) 
(let ((n (reduce #'* (mapcar #'number-of-labelings 

(diagram-vertexes diagram))))) 
(when (> . 1) 
(format stream "~&For "RD interpretation~:p." n)) 
(values))) 

Note that matri x-transpose is called by show-vertex to turn the matrix of labelings 
on its side. It works like this: 

> (possible-labelings *Y) 

ii+ + +) 

( ) 

(L R -) 
(- L R) 
(R - D) 

> (matrix-transpose (possible-labelings .)) 

((+ - L - R) 
(-^' - R L -) 
(-H -- R D) 

The implementation of matrix-transpose is surprisingly concise. It is an old Lisp 
trick, and well worth understanding: 

(defun matrix-transpose (matrix) 
"Turn a matrix on its side." 
(if matrix (apply #'mapcar #'list matrix))) 

The remaining code has to do with creating diagrams. We need some handy way of 
specifying diagrams. One way would be with a line-recognizing program operating 
on digitized input from a camera or bitmap display. Another possibility is an interactive 
drawing program using a mouse and bitmap display. But since there is not yet a 
Common Lisp standard for interacting with such devices, we will have to settle for a 
textual description. The macro def di agram defines and names a diagram. The name 
is followed by a list of vertex descriptions. Each description is a list consisting of 
the name of a vertex, the vertex type (Y, A, L, or T), and the names of the neighboring 
Vertexes. Here again is the def di agram description for the cube shown in figure 17.6. 

<a id='page-575'></a>

(defdiagram cube 
(a Y b c d) 

(bW e a) 
(c W f a) 
(dW g a) 
(eL b) 
(f L c) 
(gL d)) 

Figure 17.6: A Cube 

Tiie macro def diagram calls construct -diagram to do the real work. It would 
be feasible to have defdi agram expand into a defvar, making the names be special 
variables. But then it would be the user's responsibility to make copies of such a 
variable before passing it to a destructive function. Instead, I use put-di agram and 
di agram to put and get diagrams in a table, di agram retrieves the named diagram 
and makes a copy of it. Thus, the user cannot corrupt the original diagrams stored in 
the table. Another possibility would be to have def di agram expand into a function 
definition for name that returns a copy of the diagram. I chose to keep the diagram 
name space separate from the function name space, since names like cube make 
sense in both spaces. 

(defmacro defdiagram (name &rest vertex-descriptors) 
"Define a diagram. A copy can be gotten by (diagram name)." 
'(put-diagram '.name (construct-diagram '.vertex-descriptors))) 

(let ((diagrams (make-hash-table))) 

<a id='page-576'></a>

(defun diagram (name) 
"Get a fresh copy of the diagram with this name." 
(make-copy-diagram (gethash name diagrams))) 

(defun put-diagram (name diagram) 
"Store a diagram under a name." 
(setf (gethash name diagrams) diagram) 
name)) 

The function construct-di agram translates each vertex description, using 
construct - vertex, and then fills in the neighbors of each vertex. 

(defun construct-diagram (vertex-descriptors) 
"Build a new diagram from a set of vertex descriptor." 
(let ((diagram (make-diagram))) 

Put in the Vertexes 
(setf (diagram-vertexes diagram) 

(mapcar #'construct-vertex vertex-descriptors)) 
;; Put in the neighbors for each vertex 
(dolist (v-d vertex-descriptors) 

(setf (vertex-neighbors (find-vertex (first v-d) diagram)) 
(mapcar #'(lambda (neighbor) 
(find-vertex neighbor diagram)) 
(v-d-neighbors v-d)))) 
diagram)) 

(defun construct-vertex (vertex-descriptor) 
"Build the vertex corresponding to the descriptor." 
;; Descriptors are like: (x L y z) 
(make-vertex 

:name (first vertex-descriptor) 
:type (second vertex-descriptor) 
:labelings (possible-labelings (second vertex-descriptor)))) 

(defun v-d-neighbors (vertex-descriptor) 
"The neighboring vertex names in a vertex descriptor." 
(rest (rest vertex-descriptor))) 

The defstruct for di agram automatically creates the function copy-di agram, but it 
just copies each field, without copying the contents of each field. Thus we need 
make - copy -di ag ram to create a copy that shares no structure with the original. 

<a id='page-577'></a>

(defun make-copy-diagram (diagram) 
"Make a copy of a diagram, preserving connectivity." 
(let* ((new (make-diagram 

'.Vertexes (mapcar #*copy-vertex 
(diagram-vertexes diagram))))) 
Put in the neighbors for each vertex 
(dolist (v (diagram-vertexes new)) 
(setf (vertex-neighbors v) 
(mapcar #*(lambda (neighbor) 
(find-vertex (vertex-name neighbor) new)) 
(vertex-neighbors v)))) 
new)) 

17.3 Labeling Diagrams 
We are now ready to try labeling diagrams. First the cube: 

> (print-labelings (diagram 'cube)) 

The initial diagram is: 
A/5 Y: AB=C+-L-R] AC=[+-RL-] AD=[+--RL] 
B/3 W: BG=[L-+] BE=[R-+] BA=[++-] 
C/3 W: CE=[L-+] CF=[R-+] CA=C++-] 
D/3 W: DF=[L-+] DG=[R-+] DA=C++-] 
E/6 L: EC=[RL+L-R] EB=[LRR+L-] 
F/6 L: FD=[RL+L-R] FC=CLRR+L-] 
G/6 L: GB=[RL+L-R] GD=[LRR+L-] 

For 29,160 interpretations. 

After constraint propagation the diagram is: 
A/1 Y: AB=[+] AC=[+] AD=[+] 
B/2 W: BG=CL-] BE=[R-] BA=C++] 
C/2 W: CE=[L-] CF=[R-] CA=C++] 
D/2 W: DF=[L-] DG=[R-] DA=C++] 
E/3 L: EC=[R-R] EB=[LL-] 
F/3 L: FD=[R-R] FC=CLL-] 
G/3 L: GB=[R-R] GD=[LL-] 

For 216 interpretations. 

There are four solutions: 

<a id='page-578'></a>

Diagram: 
A/1 Y: AB=[+] AC=C+] AD=C+] 
B/1 W: BG=[L] BE=[R] BA=[+] 
C/1 W: CE=CL] CF=CR] CA=[+] 
D/1 W: DF=CL] DG=[R] DA=[+] 
E/1 L: EC=[R] EB=[L] 
F/1 L: FD=CR] FC=CL] 
G/1 L: GB=[R] GD=[L] 

Diagram: 
A/1 Y: AB=[+] AC=[+] AD=[+] 
B/1 W: BG=[L] BE=[R] BA=[+] 
C/1 W: CE=[L] CF=[R] CA=C+] 
D/1 W: DF=C-] DG=[-] DA=C+] 
E/1 L: EC=CR] EB=CL] 
F/1 L: FD=C-] FC=[L] 
G/1 L: GB=CR] GD=[-] 

Diagram: 
A/1 Y: AB=C+] AC=C+] AD=C+] 
B/1 W: BG=[L] BE=[R] BA=C+] 
C/1 W: CE=[-] CF=C-] CA=C+] 
D/1 W: DF=CL] DG=CR] DA=[-H] 
E/1 L: EC=C-] EB=[L] 
F/1 L: FD=[R] FC=[-] 
G/1 L: GB=[R] GD=[L] 

Diagram: 
A/1 Y: AB=[+] AC=[+] AD=C+] 
B/1 W: BGK-] BE=C-] BA=C+] 
C/1 W: CE=[L] CF=[R] CA=[+] 
D/1 W: DF=[L] DG=CR] DA=C+] 
E/1 L: EC=CR] EB=C-] 
F/1 L: FD=[R] FC=CL] 
G/1 L: GB=C-] GD=[L] 

The four interpretations correspond, respectively, to the cases where the cube is free 
floating, attached to the floor (GD and DF = -), attached to a wall on the right (EC 
and CF = -), or attached to a wall on the left (BG and BE = -). These are shown in 
figure 17.7. It would be nice if we could supply information about where the cube is 
attached, and see if we can get a unique interpretation. The function ground takes a 
diagram and modifies it by making one or more lines be grounded lines - lines that 
have a concave (-) label, corresponding to a junction with the ground. 

<a id='page-579'></a>

Figure 17.7: Four Interpretations of the Cube 

(defun ground (diagram vertex-a vertex-b) 
"Attach the line between the two Vertexes to the ground. 
That is. label the line with a -" 
(let* ((A (find-vertex vertex-a diagram)) 

(B (find-vertex vertex-b diagram)) 

(i (position . (vertex-neighbors A)))) 
(assert (not (null i))) 
(setf (vertex-labelings A) 

(find-all-if #'(lambda (1) (eq (nth i 1) '-)) 
(vertex-labelings A))) 
diagram)) 

<a id='page-580'></a>

We can see how this works on the cube: 

Figure 17.8: Cube on a Plate 

> (print-labelings (ground (diagram 'cube) 'g *d)) 

The initial diagram is: 
A/5 Y: AB=[+-L-R] AC=C+-RL-] AD=[+--RL] 
B/3 W: BG=[L-+] BE=[R-+] BA=C++-] 
C/3 W: CE=CL-+] CF=CR-+] CA=C-H-] 
D/3 W: DF=[L-+] DG=[R-+] DA=[-M-] 
E/6 L: EC=[RL+L-R] EB=[LRR+L-] 
F/6 L: FD=[RL+L-R] FCKLRR+L-] 
G/1 L: GB=[R] GD=[-] 
For 4,860 interpretations. 

After constraint propagation the diagram is: 
A/1 Y: AB=C+] AC=C+] AD=C+] 
B/1 W: BG=[L] BE=[R] BA=[+] 
C/1 W: CE=[L] CF=CR] CA=[+] 
D/1 W: DF=[-] DG=C-] DA=C+] 
E/1 L: EC=[R] EB=CL] 
F/1 L: FD=[-] FC=CL] 
G/1 L: GB=CR] GD=C-] 

<a id='page-581'></a>
Note that the user only had to specify one of the two ground lines, GD. The program 
found that DF is also grounded. Similarly, in programming ground-1 ine, we only 
had to update one of the Vertexes. The rest is done by constraint propagation. 

The next example yields the same four interpretations, in the same order (free 
floating, attached at bottom, attached at right, and attached at left) when interpreted 
ungrounded. The grounded version yields the unique solution shown in the following 
output and in figure 17.9. 

Figure 17.9: Labeled Cube on a Plate 

(defdiagram cube-on-plate 
(a Y b c d) 
(b W e a) 
(c W f a) 
(d W g a) 

(eL b) 
(f Y c i) 
(gY d h) 

(h W 1 g j) 
(i W f m j) 
(j Y hi k) 
(k Wm 1 j) 
(1 L h k) 

<a id='page-582'></a>

(m L k i)) 

> (print-labelings (ground (diagram 'cube-on-plate) 'k 'm)) 

The initial diagram is: 
A/5 Y: AB=C+-L-R] AC=C+-RL-] A[)=[+--RL] 
B/3 W: BG=CL-+] BE=[R-+] BA=C++-] 
C/3 W: CE=CL-+] CF=CR-+] CA=C++-] 
D/3 W: DF=CL-+] DG=CR-+] DA=C++-] 
E/6 L: EC=CRL+L-R] EB=CLRR+L-] 
F/5 Y: FD=C+-L-R] FC=C+-RL-] FI=[+--RL] 
G/5 Y: GB=C+-L-R] GD=C+-RL-] GH=[+--RL] 
H/3 W: HL=CL-+] HG=[R-+] HJ=[+-h-] 
1/3 W: IF=CL-+] IM=[R-+] IJ=[++-] 
J/5 Y: JH=C+-L-R] JI=C+-RL-] JK=[+--RL] 
K/1 W: KM=[-] KL=C-] KJ=[+] 
L/6 L: LH=CRL+L-R] LK=CLRR+L-] 
M/6 L: MK=[RL+L-R] MI=CLRR+L-] 

For 32.805.000 interpretations. 

After constraint propagation the diagram is: 
A/1 Y: AB=C+] AC=C+] AD=C+] 
B/1 W: BG=CL] BE=[R] BA=C+] 
C/1 W: CE=[L] CF=CR] CA=[+] 
D/1 W: DF=C-] DG=C-] DA=[+] 
E/1 L: EC=CR] EB=CL] 
F/1 Y: FD=C-] FC=CL] FI=[R] 
G/1 Y: GB=[R] GD=C-] GH=CL] 
H/1 W: HL=[L] HG=CR] HJ=M 
I/l W: IF=CL] IM=[R] IJ=C+] 
J/1 Y: JH=[+] JI=C+] JK=[+] 
K/1 W: KM=[-] KL=[-] KJ=[+] 
L/1 L: LH=CR] LK=C-] 
M/1 L: MK=[-] MI=[L] 

It is interesting to try the algorithm on an "impossible" diagram. It turns out the 
algorithm correctly finds no interpretation for this well-known illusion: 

(defdiagram poiuyt 
(a L b g) 
(b L j a) 
(c L d 1) 
(d L h c) 
(e L f i) 
(f L k e) 
(g L a 1) 
(h L 1 d) 
(i L e k) 
(j L k b) 

<a id='page-583'></a>

Figure 17.10: An Impossible Figure (A Poiuyt) 

(k W j i f) 
(1 W h g c)) 

> (print-labelings (diagram 'poiuyt)) 

The initial diagram is: 
A/6 AB=CRL+L-R] AG=[LRR+L-] 
B/6 BJ=[RL+L-R] BA=[LRR+L-] 
C/6 CD=CRL+L-R] CL=[LRR+L-] 
D/6 DH=[RL+L-R] DC=CLRR+L-] 
E/6 EF=[RL+L-R] EI=[LRR+L-] 
F/6 FK=[RL+L-R] FE=CLRR+L-] 
G/6 GA=[RL+L-R] GL=[LRR+L-] 
H/6 HL=[RL+L-R] HD=CLRR+L-] 
1/6 IE=[RL+L-R] IK=CLRR+L-] 
J/6 JK=[RL+L-R] JB=CLRR+L-] 
K/3 W KJ=[L-+] KI=CR-+] KF=[++-] 
L/3 W LH=[L-+] LG=[R-+] LC=C++-] 

For 544.195.584 interpretations. 

After constraint propagation the diagram is: 
A/5 AB=CRL+-R] AG=[LRRL-] 
B/5 BJ=CRLL-R] BA=[LR+L-] 
C/2 CD=[LR] CL=[+-] 
D/3 DH=[RL-] DC=[LRL3 
E/3 EF=[RLR] EI=[LR-] 
F/2 FK=C+-] FE=[RL] 

<a id='page-584'></a>

G/4 L: GA=[RL-R] GL=[L+L-] 
H/4 L: HL=[R+-R] HD=[LRL-] 
1/4 L: IE=CRL-R] IK=[L+L-] 
J/4 L: JK=[R+-R] JB=CLRL-] 
K/3 W: KJ=[L-+] KI=CR-+] KF=C++-3 
L/3 W: LH=CL-+] LG=[R-+] LC=C++-] 

For 2.073,600 interpretations. 

There are zero solutions: 

Now we try a more complex diagram: 

(defdiagram tower

(a Y b c d) (n L q 0) 
(b W g e a) (0 W y j n) 
(c W e f a) (P L r i) 
(d W f g a) (q W . s w) 
(e L c b) (r W s . .) 
(f Y d c i) (s L r q) 
(g Y b d h) (t W w . .) 
(h W 1 g j) (u W . y .) 
(i W f m p) (V W y w .) 
(j Y h 0 k) (w Y t . q) 
(k W m 1 j) (x Y r u t) 
(1 L h k) (y Y V u o) 
(m L k i) (z Y t U V)) 

> (print-labelings (ground (diagram 'tower) . 'k)) 

The initial diagram is: 
A/5 Y: =[+-L-R3 AC=[+-RL-] AD=C+--RL] 
B/3 W: 
C/3 W: 
D/3 W: 
E/6 L: 
F/5 Y: FD=C+-L-R] FC=C+-RL-] FI=C+--RL] 
G/5 Y: GB=[+-L-R] GD=C+-RL-] GH=C+--RL] 
H/3 W: HL=CL-+] HG=CR-+] HJ=C-h-] 
1/3 W: IF=[L-+] IM=[R-+] IP=C++-] 
J/5 Y: ^--RL] 
K/3 W: KM=[L-+] KL=CR-+] KJ=C++-] 
L/1 L: 
M/6 L: 
N/6 L: 
0/3 W: 
P/6 L: 
0/3 W: QN=[L-+] QS=CR-+] QW=C++-] 
R/3 W: RS=CL-+] RP=[R-+3 RX=C-H-] 
S/
S/S/6
66 L
LL:
:: SR=CRL+L-R] SQ=CLRR+L-] 

<a id='page-585'></a>

T/3 W: TW=CL-+] TX=CR-+] TZ=C++-] 
U/3 W: UX=[L-+] UY=[R-+] UZ=[++-] 
V/3 W: VY=CL-+] VW=CR-+] VZ=[++-] 
W/5 Y: WT=C+-L-R] WV=C+-RL-] WQ=C+--RL] 
X/5 Y: XR=[+-L-R] XU=C+-RL-] XT=[+--RL] 
Y/5 Y: YV=[+-L-R] YU=[+-RL-] YO=C+--RL] 
Z/5 Y: ZT=C+-L-R] ZU=C+-RL-] ZV=[+--RL] 

For 1,614,252,037,500,000 interpretations. 

Figure 17.11: A Tower 

After constraint propagation the diagram is: 
A/1 Y: AB=[+] AC=[+] AI>[+] 
B/1 W: BG=[L] BE=[R] BA=[+] 
C/1W: CE=[L] CF=[R] CA=[+] 
D/1W: DF=[-] DG=[-] DA=[+] 
E/1L: EC=[R]EB=[L] 
F/1YFD=[-] FC=[L] FI=[R] 
G/1Y GB=[R] GD=[-] GH=[L] 
H/1W: HL=[L] HG=[R] HJ=[+] 
I/l W: IF=[L]IM=[R] IP=[+] 
J/lYJH=[+]JO=MJK=[+] 

<a id='page-586'></a>

K/1 W: KM=[-] KL=[-] KJ=[+] 
L/1 L: LH=[R] LK=[-] 
M/1L: MK=[-] MI=[L] 
N/1 L: NQ=[R] NO=[-] 
O/l W:OY=M OJ=M ON=[-] 
P/1 L: PR=[L] PI=[+] 
Q/1 W: QN=[L] QS=[R] QW=[+] 
R/1 W: RS=[L] RP=[R] RX=M 
S/1 L: SR=[R] SQ=[L] 
T/1 W: TW=[+] TX=[+] TZ=[-] 
U/1 W: UX=[+] UY=[+] UZ=[-] 
V/1 W: VY=[+] VW=[+] VZ=[-] 
W/1.: WT=[+] WV=M WQ=[+] 
X/1.: XR=[+] XU=[+] XT=[+] 
./1.: YV=[+] YU=[+] Y0=[+] 
Z/1Y: ZT=[-] ZU=[-] ZV=[-] 

We see that the algorithm was able to arrive at a single interpretation. Moreover, even 
though there were a large number of possibilities - over a quadrillion - the computation 
is quite fast. Most of the time is spent printing, so to get a good measurement, 
we define a function to find solutions without printing anything: 

(defun find-labelings (diagram) 
"Return a list of all consistent labelings of the diagram." 
(every #'propagate-constraints (diagram-vertexes diagram)) 
(search-solutions diagram)) 

When we time the application of find-label ings to the grounded tower and the 
poiuyt, we find the tower takes 0.11 seconds, and the poiuyt 21 seconds. This is over 
180 times longer, even though the poiuyt has only half as many Vertexes and only 
about half a million interpretations, compared to the tower's quadrillion. The poiuyt 
takes a long time to process because there are few local constraints, so violations are 
discovered only by considering several widely separated parts of the figure all at the 
same time. It is interesting that the same fact that makes the processing of the poiuyt 
take longer is also responsible for its interest as an illusion. 

17.4 Checking Diagrams for Errors 
This section considers one more example, and considers what to do when there are 
apparent errors in the input. The example is taken from Charniak and McDermott's 
Introduction to Artificial Intelligence, page 138, and shown in figure 17.12. 

<a id='page-587'></a>
Figure 17.12: Diagram of an arch 

(defdiagram arch 
(a W e b c) (P L 0 q) 
(b L d a) (q . . i .) 
(c Y a d g) (. .3 s q) 
(d Y c b m) (s L . t) 
(e L a f) (t W. s k) 
(f . e g n) (u L t 1) 
(g wh f c) (V L 2 4) 
(h . g i 0) (w W X 1 y) 
(i . h j q) (X Lw z) 
(j . i k r) (y Yw 2 z) 
(k . j 1 t) (z W 3X y) 
(1 . k m v) (1 . . 0 w) 
(m L 1 d) (2 W V3 y) 
(n L f 1) (3 L . 2) 
(0 W . 1 h) (4 . u 1 V)) 

Unfortunately, running this example results in no consistent interpretations after 
constraint propagation. This seems wrong. Worse, when we try to ground the 
diagram on the line XZ and call pri nt -1 abel i ngs on that, we get the following error: 

<a id='page-588'></a>

>>ERROR: The first argument to NTH was of the wrong type. 
The function expected a fixnum >= zero. 
While in the function LABELS-FOR ^ CONSISTENT-LABELINGS 

Debugger entered while in the following function: 

LABELS-FOR (P.C. = 23) 
Arg 0 (VERTEX): U/6 
Arg 1 (FROM): 4/4 

What has gone wrong? A good guess is that the diagram is somehow inconsistent - 
somewhere an error was made in transcribing the diagram. It could be that the 
diagram is in fact impossible, like the poiuyt. But that is unlikely, as it is easy for us 
to provide an intuitive interpretation. We need to debug the diagram, and it would 
also be a good idea to handle the error more gracefully. 

One property of the diagram that is easy to check for is that every line should be 
mentioned twice. If there is a line between Vertexes A and B, there should be two 
entries in the vertex descriptors of the following form: 

(A ? ... . ...) 
(. ? ... A ...) 

Here the symbolmeans we aren't concerned about the type of the Vertexes, only 
with the presence of the line in two places. The following code makes this check 
when a diagram is defined. It also checks that each vertex is one of the four legal 
types, and has the right number of neighbors. 

(defmacro defdiagram (name &rest vertex-descriptors) 
"Define a diagram. A copy can be gotten by (diagram name)." 
'(put-diagram '.name (construct-diagram 

(check-diagram '.vertex-descriptors)))) 

(defun check-diagram (vertex-descriptors) 
"Check if the diagram description appears consistent." 
(let ((errors 0)) 

(dolist (v-d vertex-descriptors) 
v-d is like: (a Y b c d) 
(let ((A (first v-d)) 

(v-type (second v-d))) 
Check that the number of neighbors is right for 
the vertex type (and that the vertex type is legal) 

(when (/= (length (v-d-neighbors v-d)) 

(case v-type ((W Y .) 3) ((L) 2) (t -1))) 
(warn "Illegal type/neighbor combo: '^a" v-d) 
(incf errors)) 

;; Check that each neighbor . is connected to 

<a id='page-589'></a>

this vertex. A. exactly once 
(dolist (B (v-d-neighbors v-d)) 
(when (/= 1 (count-if 
#'(lambda (v-d2) 
(and (eql (first v-d2) B) 
(member A (v-d-neighbors v-d2)))) 

vertex-descriptors)) 
(warn "Inconsistent vertex: "a-^a" A B) 
(incf errors))))) 

(when (> errors 0) 
(error "Inconsistent diagram. ~d total error~:p." 
errors))) 
vertex-descriptors) 

Now let's try the arch again: 

(defdiagram arch 
(a W eb c) (PL 0 q) 
(b L d a) (q. . i r) 
(c Y a d g) (r . j s q) 
(d Y c b m) (s L r t) 
(e L a f) (t W V s k) 
(f . e g n) (u L t 1) 
(g W hf c) (V L 2 4) 
(h . g i 0) (wW X 1 y) 
(i . h j q) (X L w z) 
(j . i k r) (yY w 2 z) 
(k . j 1 t) (.W 3 X y) 
(1 . k m v) (1. . 0 w) 
(m L 1 d) (2W V 3 y) 
(n L f 1) (3 L . 2) 
(0 W .1 h) (4. u 1 V)) 

Warning: Inconsistent vertex: T-'V 
Warning: Inconsistent vertex: U-'T 
Warning: Inconsistent vertex: U--L 
Warning: Inconsistent vertex: L--V 
Warning: Inconsistent vertex: 4

--u 

Warning: Inconsistent vertex: 4-'L 

>ERROR: Inconsistent diagram, 6 total errors. 

The def d i a g ram was transcribed from a hand-labeled diagram, and it appears that the 
transcription has fallen prey to one of the oldest problems in mathematical notation: 
confusing a "u" with a "v." The other problem was in seeing the line U-L as a single 
line, when in fact it is broken up into two segments, U-4 and 4-L. Repairing these 
bugs gives the diagram: 

<a id='page-590'></a>

(defdiagram arch 

(a W e b c) (P L 0 q ) 
( b L d a) (q . . i . ) 
(c Y a d g ) ( . . J s q ) 
(d Y c b m) (s L . t ) 
(e L a f) ( t W u s k) ; t-u not t-v 
(f . e g n) (u L t 4) ; w4 
not u-l 
( g W h f c) (V L 2 4) 
(h . g i 0 ) (w W X 1 y ) 
( i 
(j 
(k 
. h 
. 
. i 
J q ) 
k r) 
1 t ) 
( X L w z) 
( y Y w 2 z) 
(z W 3 X y ) 
(1 . k m 4) (1 . . 0 w) ;1-4 not l-v 
(m L 1 d ) 
(n L f 1) 
( 0 W . 1 h ) 
(2 
(3 
(4 
wV 3 y ) 
L . 2) 
. u 1 V) ) 

This time there are noerrors detected by check-di agram, butrunningprint-label ings 

again still does not give a solution. To get more information about which constraints 

are applied, I modified propagate-constrai nts to print out some information: 

(defun propagate-constraints (vertex) 
"Reduce the number of labelings on vertex by considering neighbors. 
If we can reduce, propagate the new constraint to each neighbor." 

Return nil only when the constraints lead to an impossibility 

(let ((old-num (number-of-labelings vertex))) 
(setf (vertex-labelings vertex) (consistent-labelings vertex)) 
(unless (impossible-vertex-. vertex) 

(when (< (number-of-labelings vertex) old-num) 

(format t "-&; ~a: "Ua ~a" vertex 
(vertex-neighbors vertex) 
(vertex-labelings vertex)) 
(every #'propagate-constraints (vertex-neighbors vertex))) 

vertex))) 

Running the problem again gives the following trace: 

> (print-labelings (ground (diagram 'arch) *x '.)) 

The initial diagram is: 
A/3 W: AE=[L-+] AB=[R-+] AC=[++-] 
P/6 L: PO=[RL+L-R] PQ=CLRR+L-] 
B/6 L: BD=[RL-HL-R] BA=CLRR+L-] 
Q/4 T: QP=[RRRR] QI=[LLLL] QR=[+-LR] 
C/5 Y: CA=C+-L-R] CD=C+-RL-] CG=[+--RL] 
R/4 T: RJ=[RRRR] RS=[LLLL] RQ=[+-LR] 
D/5 Y: DC=C+-L-R] DB=C+-RL-] DM=C+--RL] 

<a id='page-591'></a>
S/6 L: SR=[RL+L-R] ST=CLRR+L-] 
E/6 L: EA=CRL+L-R] EF=[LRR+L-] 
T/3 W: TU=[L-+] TS=[R-+] TK=C++-] 
F/4 T: FE=[RRRR] FG=CLLLL] FN=C+-LR] 
U/6 L: UT=CRL+L-R] U4=CLRR+L-] 
G/3 W: GH=[L-+] GF=CR-+] GCK-H-] 
V/6 L: V2=[RL+L-R] V4=[LRR+L-] 
H/4 T: HG=CRRRR] HI=CLLLL] HO=[+-LR] 
W/3 W: WX=CL-+] W1=[R-+] WY=C++-] 
1/4 T: IH=[RRRR] IJ=CLLLL] IQ=C+-LR] 
X/1 L: XW=[R] XZ=[-] 
J/4 T: JI=CRRRR] JK=CLLLL] JR=[-h-LR] 
Y/5 Y: YW=[+-L-R] Y2=[+-RL-] YZ=C+--RL] 
K/4 T: KJ=[RRRR] KL=[LLLL] KT=C+-LR] 
Z/3 W: Z3=CL-+] ZX=CR-+] ZY=C++-] 
L/4 T: LK=[RRRR] LM=[LLLL] L4=C+-LR] 
1/4 T: 1N=[RRRR] 10=CLLLL] 1W=[+-LR] 
M/6 L: ML=CRL+L-R] MD=[LRR+L-] 
2/3 W: 2V=[L-+] 23=[R-+] 2Y=[++-] 
N/6 L: NF=CRL+L-R] N1=CLRR+L-] 
3/6 L: 3Z=[RL+L-R] 32=CLRR+L-] 
0/3 W: OP=[L-+] 01=CR-+] OH=C++-] 
4/4 T: 4U=[RRRR] 4L=[LLLL] 4V=C+-LR] 

For 2,888 ,816,545,234,944,000 i nterpretati ons. 
P/2 (0/3 0/4) ((R L) (- D) 
0/1 (P/2 1/4 H/4) ((L R +)) 
P/1 (0/1 Q/4) ((R D) 
1/3 (N/6 0/1 W/3) ((R L +) (R L -) (R L D) 
N/2 (F/4 1/3) ((R L) (- D) 
F/2 (E/6 G/3 N/2) ((R L -) (R L D) 
E/2 (A/3 F/2) ((R L) (- D) 
A/2 (E/2 B/6 C/5) ((L R +) (-- +)) 
B/3 (D/5 A/2) ((R L) (- L) (R -)) 
D/3 (C/5 B/3 M/6) ((---) (- L R) (R - D) 
W/1 (X/1 1/3 Y/5) ((L R +)) 
1/1 (N/2 0/1 W/1) ((R L D ) 
Y/1 (W/1 2/3 Z/3) ((+ + +)) 
2/2 (V/6 3/6 Y/1) ((L R +) (-- +)) 
V/3 (2/2 4/4) ((R L) (- L) (R -)) 
4/2 (U/6 L/4 V/3) ((R L -) (R L R)) 
U/2 : (T/3 4/2) ((R L) (- D) 
T/2 (U/2 S/6 K/4) ((L R +) (-- +)) 
S/2 (R/4 T/2) ((R L) (R -)) 
K/1 (J/4 L/4 T/2) ((R L +)) 
J/1 (1/4 K/1 R/4) ((R L D ) 
I/l (H/4 J/1 0/4) ((R L R)) 
L/1 (K/1 M/6 4/2) ((R L R)) 
M/2 (L/1 D/3) ((R L) (R -)) 

<a id='page-592'></a>

3/3: (Z/3 2/2) ((R L) (- L) (R -)) 
1/1: (3/3 X/1 Y/1) (( - - +)) 
3/1: (Z/1 2/2) (( - D) 
2/1: (V/3 3/1 Y/1) ((L R +)) 
V/2: (2/1 4/2) ((R L) (R -)) 

After constraint propagation the diagram is: 
A/0 W: 
P/1 L: PO=[R] PQ=CL] 
B/0 L: 
Q/4 T: QP=[RRRR] QI=[LLLL] QR=[+-LR] 
C/0 Y: 
R/4 T: RJ=[RRRR] RS=[LLLL] RQ=C+-LR] 
D/0 Y: 
S/2 L: SR=CRR] ST=[L-] 
E/2 L: EA=[R-] EF=CLL] 
T/2 W: TU=[L-] TS=CR-] TK=[++] 
F/2 T: FE=CRR] FG=[LL] FN=C-L] 
U/2 L: UT=[R-] U4=[LL] 
G/0 W: 
V/2 L: V2=[RR] V4=CL-] 
H/0 T: 
W/1 W: WX=[L] W1=[R] WY=C+] 

I/l T: IH=[R3 IJ=[L] IQ=[R] 
X/1 L: XW=[R] XZ=[-] 
J/1 T: JI=[R] JK=[L] JR=[L] 
Y/1 Y: YW=C+] Y2=[+] YZ=[+] 
K/1 T: KJ=CR] KL=[L] KT=[+] 
Z/1 W: Z3=C-] ZX=[-] ZY=[H-] 
L/1 T: LK=[R] LM=[L] L4=[R] 
1/1 T: 1N=[R] 10=[L] 1W=[L] 
M/2 L: ML=[RR] MD=CL-] 
2/1 W: 2V=CL] 23=CR] 2Y=[+] 
N/2 L: NF=[R-] N1=[LL] 
3/1 L: 3Z=[-] 32=[L] 
0/1 W: OP=[L] 01=CR] OH=C+] 
4/2 T: 4U=[RR] 4L=[LL] 4V=[-R] 

From the diagram after constraint propagation we can see that the Vertexes A,B,C,D,G, 
and . have no interpretations, so they are a good place to look first for an error. From 
the trace generated by propagate-constraints (the lines beginning with a semicolon), 
we see that constraint propagation started at . and after seven propagations 
reached some of the suspect Vertexes: 

<a id='page-593'></a>

A/2: (E/2 B/6 C/5) ((L R +) (-- +)) 
8/3: (D/5 A/2) ((R L) (- L) (R -)) 
D/3: (C/5 B/3 M/6) (( ) (- L R) (R - D) 

A and . look acceptable, but look at the entry for vertex D. It shows three interpretations, 
and it shows that the neighbors are C, B, and M. Note that line DC, the first 
entry in each of the interpretations, must be either -, - or R. But this is an error, 
because the "correct" interpretation has DC as a + line. Looking more closely, we 
notice that D is in fact a W-type vertex, not a Y vertex as written in the definition. We 
should have: 

(defdiagram arch 
(a W e b c) (p L 0 q) 
(b L d a) (q . . i r) 
(cY a d g) (r . j s q) 
(d W b m c) (s L r t) ;disaW,notY 

(e L a f) (t W u s k) 
(f . e g n) (u L t 4) 
(gW h f c) (V L 2 4) 
(h . g i 0) (w W . 1 y) 
(i . h j q) (x L w z) 
(j . i k r) (y Y w 2 z) 
(k . j 1 t) (z W 3 X y) 
(1 . k m 4) (1 . . 0 w) 
(m L 1 d) (2 WV 3 y) 
(. L f 1) (3 L . 2) 
(0 W . 1 h) (4 . u 1 V)) 

By running the problem again and inspecting the trace output, we soon discover the 
real root of the problem: the most natural interpretation of the diagram is beyond the 
scope of the program! There are many interpretations that involve blocks floating in 
air, but if we ground lines OP, TU and XZ, we run into trouble. Remember, we said 
that we were considering trihedral Vertexes only. But vertex 1 would be a quad-hedral 
vertex, formed by the intersection of four planes: the top and back of the base, and 
the bottom and left-hand side of the left pillar. The intuitively correct labeling for the 
diagram would have Ol be a concave (-) line and Al be an occluding line, but our 
repertoire of labelings for . Vertexes does not allow this. Hence, the diagram cannot 
be labeled consistently. 

Let's go back and consider the error that came up in the first version of the 
diagram. Even though the error no longer occurs on this diagram, we want to make 
sure that it won't show up in another case. Here's the error: 

<a id='page-594'></a>

>>ERROR: The first argument to NTH was of the wrong type. 
The function expected a fixnum >= zero. 
While in the function LABELS-FOR <i= CONSISTENT-LABELINGS 

Debugger entered while in the following function: 

LABELS-FOR (P.C. = 23) 
Arg 0 (VERTEX): U/6 
Arg 1 (FROM): 4/4 

Looking at the definition of 1 abel s - for, we see that it is looking for the from vertex, 
which in this case is 4, among the neighbors of U. It was not found, so pos became nil, 
and the function nth complained that it was not given an integer as an argument. So 
this error, if we had pursued it earlier, would have pointed out that 4 was not listed 
as a neighbor of U, when it should have been. Of course, we found that out by other 
means. In any case, there is no bug here to fix - as long as a diagram is guaranteed to 
be consistent, the 1 abel s - for bug will not appear again. 

This section has made two points: First, write code that checks the input as 
thoroughly as possible. Second, even when input checking is done, it is still up to 
the user to understand the limitations of the program. 

17.5 History and References 
Guzman (1968) was one of the first to consider the problem of interpreting line 
diagrams. He classified Vertexes, and defined some heuristics for combining information 
from adjacent Vertexes. Huffman (1971) and Clowes (1971) independently 
came up with more formal and complete analyses, and David Waltz (1975) extended 
the analysis to handle shadows, and introduced the constraint propagation algorithm 
to cut down on the need for search. The algorithm is sometimes called "Waltz 
filtering" in his honor. With shadows and nontrihedral angles, there are thousands 
of vertex labelings instead of 18, but there are also more constraints, so the constraint 
propagation actually does better than it does in our limited world. Waltz's approach 
and the Huffman-Clowes labels are covered in most introductory AI books, including 
Rich and Knight 1990, Charniak and McDermott 1985, and Winston 1984, Waltz's 
original paper appears in The Psychology of Computer Vision (Winston 1975), an influential 
volume collecting early work done at MIT. He also contributed a summary 
article on Waltz filtering (Waltz 1990). 

Many introductory AI texts give vision short coverage, but Charniak and McDermott 
(1985) and Tanimoto (1990) provide good overviews of the field. Zucker (1990) 
provides an overview of low-level vision. 

Ramsey and Barrett (1987) give an implementation of a line-recognition program. 
It would make a good project to connect their program to the one presented in this 
chapter, and thereby go all the way from pixels to 3-D descriptions. 

<a id='page-595'></a>

17.6 Exercises 
This chapter has solved the problem of line-labeling for polyhedra made of trihedral 
Vertexes. The following exercises extend this solution. 

&#9635; Exercise 17.1 [h] Use the line-labeling to produce a face labeling. Write a function 
that takes a labeled diagram as input and produces a list of the faces (planes) that 
comprise the diagram. 

&#9635; Exercise 17.2 [h] Use the face labeling to produce a polyhedron labeling. Write 
a function that takes a hst of faces and a diagram and produces a list of polyhedra 
(blocks) that comprise the diagram. 

&#9635; Exercise 17.3 [d] Extend the system to include quad-hedral Vertexes and/or shadows. 
There is no conceptual difficulty in this, but it is a very demanding task to find 
all the possible vertex types and labelings for them. Consult Waltz 1975. 

&#9635; Exercise 17.4 [d] Implement a program to recognize lines from pixels. 

&#9635; Exercise 17.5 [d] If you have access to a workstation with a graphical interface, 
implement a program to allow a user to draw diagrams with a mouse. Have the 
program generate output in the form expected by construct-di agram. 

