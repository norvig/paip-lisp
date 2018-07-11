# Appendix
## Obtaining the Code in this Book
### FTP: The File Transfer Protocol

FTP is a file transfer protocol that is widely accepted by computers around the world.
FTP makes it easy to transfer files between two computers on which you have accounts.
But more importantly, it also allows a user on one computer to access files on a computer on which he or she does not have an account, as long as both computers are connected to the Internet.
This is known as *anonymous FTP.*

All the code in this book is available for anonymous FTP from the computer `mkp.com` in files in the directory `pub/norvig`.
The file `README` in that directory gives further instructions on using the files.

In the session below, the user `smith` retrieves the files from `mkp.com`.
Smith's input is in *slanted font.* The login name must be *anonymous*, and Smith's own mail address is used as the password.
The command *cd pub/norvig* changes to that directory, and the command *ls* lists all the files.
The command *mget* * retrieves ail files (the *m* stands for "multiple").
Normally, there would be a prompt before each file asking if you do indeed want to copy it, but the *prompt* command disabled this.
The command *bye* ends the FTP session.

[ ](#){:#l0010}`% *ftp mkp.com* (or *ftp 199.182.55.2*)`
!!!(p) {:.unnumlist}

`Name (mkp.com:smith): *anonymous*`
!!!(p) {:.unnumlist}

`331 Guest login ok, send ident as password`
!!!(p) {:.unnumlist}

`Password: *smith@cs.stateu.edu*`
!!!(p) {:.unnumlist}

`230 Guest login ok, access restrictions apply`
!!!(p) {:.unnumlist}

`ftp>*cd pub/norvig*`
!!!(p) {:.unnumlist}

`250 CWD command successful.`
!!!(p) {:.unnumlist}

`ftp>*ls*`
!!!(p) {:.unnumlist}

`...`
!!!(p) {:.unnumlist}

`ftp>*prompt*`
!!!(p) {:.unnumlist}

`Interactive mode off.`
!!!(p) {:.unnumlist}

`ftp>*mget**`
!!!(p) {:.unnumlist}

`...`
!!!(p) {:.unnumlist}

`ftp> bye`
!!!(p) {:.unnumlist}

`%`
!!!(p) {:.unnumlist}

Anonymous FTP is a privilege, not a right.
The site administrators at `mkp.com` and at other sites below have made their systems available out of a spirit of sharing, but there are real costs that must be paid for the connections, storage, and processing that makes this sharing possible.
To avoid overloading these systems, do not FTP from 7:00 a.m.
to 6:00 p.m.
local time.
This is especially true for sites not in your country.
If you are using this book in a class, ask your professor for a particular piece of software before you try to FTP it; it would be wasteful if everybody in the class transferred the same thing.
Use common sense and be considerate: none of us want to see sites start to close down because a few are abusing their privileges.

If you do not have FTP access to the Internet, you can still obtain the files from this book by contacting Morgan Kaufmann at the following:

[ ](#){:#l0015}Morgan Kaufmann Publishers, Inc.
!!!(p) {:.unnumlist}

340 Pine Street, Sixth Floor
!!!(p) {:.unnumlist}

San Francisco, CA 94104-3205
!!!(p) {:.unnumlist}

USA
!!!(p) {:.unnumlist}

Telephone  415/392-2665
!!!(p) {:.unnumlist}

Facsimile  415/982-2665
!!!(p) {:.unnumlist}

Internet  mkp@mkp.com
!!!(p) {:.unnumlist}

(800) 745-7323
!!!(p) {:.unnumlist}

Make sure to specify which format you want:

[ ](#){:#l0020}Macintosh diskette ISBN 1-55860-227-5
!!!(p) {:.unnumlist}

DOS 5.25 diskette ISBN 1-55860-228-3
!!!(p) {:.unnumlist}

DOS 3.5 diskette ISBN 1-55860-229-1
!!!(p) {:.unnumlist}

### Available Software

In addition to the program from this book, a good deal of other software is available.
The tables below list some of the relevant AI/Lisp programs.
Each entry lists the name of the system, an address, and some comments.
The address is either a computer from which you can FTP, or a mail address of a contact.
Unless it is stated that distribution is by *email* or *Floppy* or requires a *license,* then you can FTP from the contact's home computer.
In some cases the host computer and/or directory have been provided in italics in the comments field.
However, in most cases it should be obvious what files to transfer.
First do an `ls` command to see what files and directories are available.
If there is a file called `README`, follow its advice: do a get `README` and then look at the file.
If you still haven't found what you are looking for, be aware that most hosts keep their public software in the directory pub.
Do a `cd pub` and then another `ls`, and you should find the desired files.

If a file ends in the suffix `.Z`, then you should give the FTP command `binary` before transferring it, and then give the UNIX command `uncompress` to recover the original file.
Files with the suffix `.tar` contain several files that can be unpacked with the `tar` command.
If you have problems, consult your local documentation or system administrator.

**Knowledge Representation**

[ ](#){:#t0010}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| System | Address | Comments |
| Babbler | [rsfl@ra.msstate.edu](mailto:rsfl@ra.msstate.edu) | *email;*Markov chains/NLP |
| BACK | [peltason@tubvm.cs.tu-berlin.de](mailto:peltason@tubvm.cs.tu-berlin.de) | *3.5" floppy;* KL-ONE family |
| Belief | [almond@stat.washington.edu](mailto:almond@stat.washington.edu) | belief networks |
| Classic | [dlm@research.att.com](mailto:dlm@research.att.com) | *license;* KL-ONE family |
| Fol Getfol | [fausto@irst.it](mailto:fausto@irst.it) | *tape;* Weyrauch's FOL system |
| Framekit | [ehn+@cs.cmu.edu](mailto:ehn+@cs.cmu.edu) | *floppy;* frames |
| Framework | [mkant+@cs.cmu.edu](mailto:mkant+@cs.cmu.edu) | *a.gp.cs.cmu.edu:/usr/mkant/Public;* frames |
| Frobs | [kessler@cs.utah.edu](mailto:kessler@cs.utah.edu) | frames |
| Knowbel | [kramer@ai.toronto.edu](mailto:kramer@ai.toronto.edu) | sorted/temporal logic |
| MVL | [ginsberg@t.stanford.edu](mailto:ginsberg@t.stanford.edu) | multivalued logics |
| OPS | [slisp-group@b.gp.cs.cmu.edu](mailto:slisp-group@b.gp.cs.cmu.edu) | Forgy's OPS-5 language |
| PARKA | [spector@cs.umd.edu](mailto:spector@cs.umd.edu) | frames (designed for connection machine) |
| Parmenides | [pshell@cs.cmu.edu](mailto:pshell@cs.cmu.edu) | frames |
| Rhetorical | [miller@cs.rochester.edu](mailto:miller@cs.rochester.edu) | planning, time logic |
| SB-ONE | [kobsa@cs.uni-sb.de](mailto:kobsa@cs.uni-sb.de) | *license;* in German; KL-ONE family |
| SNePS | [shapiro@cs.buffalo.edu](mailto:shapiro@cs.buffalo.edu) | *license;* semantic net/NLP |
| SPI | [cs.orst.edu](mailto:cs.orst.edu) | Probabilistic inference |
| YAK | [franconi@irst.it](mailto:franconi@irst.it) | KL-ONE family |

**Planning and Learning**

[ ](#){:#t0015}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| System | Address | Comments |
| COBWEB/3 | [cobweb@ptolemy.arc.nasa.gov](mailto:cobweb@ptolemy.arc.nasa.gov) | *email;* concept formation |
| MATS | [kautz@research.att.com](mailto:kautz@research.att.com) | *license;* temporal constraints |
| MICRO-xxx | [waander@cs.ume.edu](mailto:waander@cs.ume.edu) | case-based reasoning |
| Nonlin | [nonlin-users-request@cs.umd.edu](mailto:nonlin-users-request@cs.umd.edu) | Tate's planner in Common Lisp |
| Prodigy | [prodigy@cs.cmu.edu](mailto:prodigy@cs.cmu.edu) | *license;* planning and learning |
| PROTOS | [porter@cs.utexas.edu](mailto:porter@cs.utexas.edu) | knowledge acquisition |
| SNLP | [weld@cs.washington.edu](mailto:weld@cs.washington.edu) | nonlinear planner |
| SOAR | [soar-requests/@cs.cmu.edu](mailto:soar-requests/@cs.cmu.edu) | *license*; integrated architecture |
| THEO | [tom.mitchell@cs.cmu.edu](mailto:tom.mitchell@cs.cmu.edu) | frames, learning |
| Tileworld | [pollack@ai.sri.com](mailto:pollack@ai.sri.com) | planning testbed |
| TileWorld | [tileworld@ptolemy.arc.nasa.gov](mailto:tileworld@ptolemy.arc.nasa.gov) | planning testbed |

**Mathematics**

[ ](#){:#t0020}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| System | Address | Comments |
| JACAL | [jaffer@altdorf.ai.mit.edu](mailto:jaffer@altdorf.ai.mit.edu) | algebraic manipulation |
| Maxima | [rascal.ics.utexas.edu](mailto:rascal.ics.utexas.edu) | version of Macsyma; also proof-checker, nqthm |
| MMA | [fateman@cs.berkeley.edu](mailto:fateman@cs.berkeley.edu) | *peoplesparc.berkeley.edu:pub/mma.* *; algebra |
| XLispStat | [umnstat.stat.umn.edu](mailto:umnstat.stat.umn.edu) | Statistics; also S Bayes |

**Compilers and Utilities**

[ ](#){:#t0025}
!!!(table)

| []() | | | | | | | | | |
|---|---|---|---|---|---|---|---|---|---|
| System | Address | Comments |
| AKCL | [rascal.ics.utexas.edu](mailto:rascal.ics.utexas.edu) | Austin Koyoto Common Lisp |
| CLX, CLUE | [export.lcs.mit.edu](mailto:export.lcs.mit.edu) | Common Lisp interface to X Windows |
| Gambit | [gambit@cs.brandeis.edu](mailto:gambit@cs.brandeis.edu) | *acorn.cs.brandeis.edu:dist/gambit**; Scheme compiler |
| ISI Grapher | [isi.edu](mailto:isi.edu) | Graph displayer; also NLP word lists |
| PCL | [arisia.xerox.com](mailto:arisia.xerox.com) | Implementation of CLOS |
| Prolog | [aisun1.ai.uga.edu](mailto:aisun1.ai.uga.edu) | Prolog-based utilities and NLP programs |
| PYTHON | [ram+@cs.cmu.edu](mailto:ram+@cs.cmu.edu) | *a.gp.cs.cmu.edu:* Common Lisp Compiler and tools |
| SBProlog | [arizona.edu](mailto:arizona.edu) | Stony Brook Prolog, Icon, Snobol |
| Scheme | [altdorf.ai.mit.edu](mailto:altdorf.ai.mit.edu) | Scheme utilities and compilers |
| Scheme | [scheme@nexus.yorku.ca](mailto:scheme@nexus.yorku.ca) | Scheme utilities and programs |
| SIOD | [bu.edu](mailto:bu.edu) | *users/gjc;* small scheme interpreter |
| Utilities | [a.gp.cs.cmu.edu](mailto:a.gp.cs.cmu.edu) | */usr/mkant/Public*; profiling, def system, etc. |
| XLisp | [cs.orst.edu](mailto:cs.orst.edu) | Lisp interpreter |
| XScheme | [tut.cis.ohio-state.edu](mailto:tut.cis.ohio-state.edu) | Also mitscheme compiler; sbprolog |



