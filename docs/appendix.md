# Appendix {docsify-ignore}
<a id='page-897'></a>

Obtaining the Code 
in this Book 

FTP: The File Transfer Protocol 

FTP is a ftle transfer protocol that is widely accepted by computers around the world. FTP 
makes it easy to transfer hies between two computers on which you have accounts. But more 
importantly, it also allows a user on one computer to access hies on a computer on which he or 
she does not have an account, as long as both computers are connected to the Internet. This is 
known as anonymous FTP. 

All the code in this book is available for anonymous FTP from the computer mkp. com in files 
in the directory pub/norvi g. The ftle README in that directory gives further instructions on using 
theftles. 

In the session below, the user smith retrieves the ftles from mkp.com. Smith's input is in 
slanted font. The login name must be anonymous, and Smith's own mail address is used as the 
password. The conunand cd pub/norvig changes to that directory, and the command Is lists 
all the ftles. The command mget * retrieves all ftles (the m stands for "multiple"). Normally, 
there would be a prompt before each ftle asking if you do indeed want to copy it, but the prompt 
command disabled this. The command bye ends the FTP session. 

%ftpmkp.com i or ftp 199182.55.2) 
Name (mkp.comismith): anonymous 
331 Guest login ok. send ident as password 
Password: smith@cs.stateu.edu 
230 Guest login ok. access restrictions apply 
ftp> cd pub/norvig 

<a id='page-898'></a>

250 CWD command successful. 
ftp> Is 

f tp> prompt 
Interactive mode off. 
ftp> mget * 

ftp> bye 
% 

Anonymous FTP is a privilege, not a right. The site administrators at mkp. com and 
at other sites below have made their systems available out of a spirit of sharing, but 
there are real costs that must be paid for the cormections, storage, and processing 
that makes this sharing possible. To avoid overloading these systems, do not FTP 
from 7:00 a.m. to 6:00 p.m. local time. This is especially true for sites not in yoiu: 
country. If you are using this book in a class, ask your professor for a particular piece 
of software before you try to FTP it; it would be wasteful if everybody in the class 
transferred the same thing. Use common sense and be considerate: none of us want 
to see sites start to close down because a few are abusing their privileges. 

If you do not have FTP access to the Internet, you can still obtain the nles from 
this book by contacting Morgan Kaufmann at the following: 

Morgan Kaufmann Publishers, Inc. 
340 Pine Street, Sbcth Floor 
San Francisco, CA 94104-3205 
USA 
Telephone 415/392-2665 
Facsimile 415/982-2665 
Internet mkp@mkp.com 
(800) 745-7323 

Make sure to specify which format you want: 

Macintosh diskette ISBN 1-55860-227-5 
DOS 5.25 diskette ISBN 1-55860-228-3 
DOS 3.5 diskette ISBN 1-55860-229-1 

Available Software 

In addition to the program from this book, a good deal of other software is available. 
The tables below list some of the relevant AI/Lisp programs. Each entry lists the 
name of the system, an address, and some comments. The address is either a 
computer from which you can FTP, or a mail address of a contact. Unless it is stated 
that distribution is by email or Roppy or requires a license, then you can FTP from the 
contact's home computer. In some cases the host computer and/or directory have 

<a id='page-899'></a>

been provided in italics in the comments field. However, in most cases it should 
be obvious what files to transfer. First do an 1 s command to see what files and 
directories are available. If there is a file called README, follow its advice: do a get 
README and then look at the file. If you still haven't found what you are looking for, 
be aware that most hosts keep their public software in the directory pub. Do a cd pub 
and then another 1 s, and you should find the desired files. 

If a file ends in the suffix . Z, then you should give the FTP command bi na ry before 
transferring it, and then give the UNIX command uncompress to recover the original 
file. Files with the suffix .tar contain several files that can be unpacked with the 
tar command. If you have problems, consult your local documentation or system 
administrator. 

Knowledge Representation 
System Address Comments 
Babbler rsfl@ra.msstate.edu email; Markov chains/NLP 

BACK peltason@tubvm.cs.tu-berlin.de 3.5''floppy; KL-ONE family 

Belief almond@stat.washington.edu belief networks 
Classic dlm@research.att.com license; KL-ONE family 
FolGetfol fausto@irst.it tape; Weyrauch's FOL system 
Framekit ehn-^^cs.cmu.edu floppy; frames 

FrameWork mkant+@cs.cmu.edu a.gp.cs.cmu.edu:/usr/mkant/Public; irdimes 
Frobs kessler@cs.utah.edu frames 
Knowbel kramer@ai.toronto.edu sorted/temporal logic 
MVL ginsberg@t. stanford.edu multivalued logics 
OPS slisp-group@b.gp.cs.cmu.edu Forgy's OPS-5 language 
PARKA spector@cs.umd.edu frames (designed for connection machine) 
Parmenides pshell@cs.cmu.edu frames 
Rhetorical miller@cs.rochester.edu planning, time logic 
SB-ONE kobsa@cs.uni-sb.de license; in German; KL-ONE family 
SNePS shapiro@cs.buffalo.edu license; semantic net/NLP 
SPI cs.orst.edu Probabilistic inference 
YAK franconi@irst.it KL-ONE family 

<a id='page-900'></a>

Planning and Learning 

System Address Comments 
COBWEB/3 cobweb@ptolemy.arc.nasa.gov email; concept formation 
MATS 
MICRO-xxx 
kautz@research.att.com 
waander@cs.ume.edu 
license; temporal constraints 
case-based reasoning 
Nonlin nonlin-users-request@cs.umd.edu Tate's planner in Common Lisp 
Prodigy prodigy@cs.cmu.edu license; planning and learning 
PROTOS porter@cs.utexas.edu knowledge acquisition 
SNLP 
SOAR 
weld@cs.washington.edu 
soar-requests/@cs.cmu.edu 
nonlinear planner 
license; integrated architecture 
THEO tom.mitchell@cs.cmu.edu frames, learning 
Tileworld pollack@ai.sri.com planning testbed 
TileWorld tileworld@ptolemy.arc.nasa.gov planning testbed 

Mathematics 

System Address Comments 
JACAL jaffer@altdorf.ai.mit.edu algebraic manipulation 
Maxima rascal.ics.utexas.edu version of Macsyma; also proof-checker, nqthm 
MMA f ateman@cs .berkeley.edu peoplesparc.berkeley.edu^ub/mma, *; algebra 
XLispStat umnstat.stat.umn.edu Statistics; also S Bayes 

Compilers and Utilities 

System Address Comments 
AKCL rascal.ics.utexas.edu Austin Koyoto Common Lisp 

CLX, CLUE export.lcs.mit.edu Common Lisp interface to X Windows 
Gambit gambit@cs.brandeis.edu acorn.cs.brandeis.edu:dist/gambit*; Scheme compiler 
ISI Grapher isi.edu Graph displayer; also NLP word lists 

PCL arisia.xerox.com Implementation of CLOS 
Prolog aisunl.ai.uga.edu Prolog-based utilities and NLP programs 
PYTHON ram+@cs.cmu.edu a.gp.cs.cmu.edu: Common Lisp Compiler and tools 
SBProlog arizona.edu Stony Brook Prolog, Icon, Snobol 
Scheme altdorf.ai.mit.edu Scheme utilities and compilers 
Scheme scheme@nexus.yorku.ca Scheme utilities and programs 
SIOD bu.edu users/gjc; small scheme interpreter 
Utilities a.gp.cs.cmu.edu /usr/mkant/Public; profiling, def system, etc. 
XLisp cs.orst.edu Lisp interpreter 
XScheme tut.cis.ohio-state.edu Also mitscheme compiler; sbprolog 

