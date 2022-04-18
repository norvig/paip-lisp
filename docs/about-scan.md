# About this book

This book, "Paradigms of Artificial Intelligence Programming", was first published in 1992.
The rights reverted to the author, Peter Norvig, who decided to share it under the MIT license; it's open source, but not public domain.

There's a collaborative effort to dust it off, fix it up, and bring it online.
We're working on this in public, together, at our Github repo, [https://github.com/norvig/paip-lisp](https://github.com/norvig/paip-lisp) .
Look for updates and newer versions there.
You can send in corrections and issues there, or email them to 
[peter+paip@norvig.com with "attn: PAIP correction" in the subject line.](mailto:peter+paip@norvig.com?subject=attn%3a%20PAIP%20correction)


## About this copy

This is a scanned copy of the 4th printing, 1998.
It's shared for reading, and for improving the Markdown copy in our Github repo.

### How it was made
@pronoiac had the spine / binding removed and fed the pages through a scanner.
Steps and software used:

* scanner gave 600dpi grayscale, as 3.6 gigabytes of png files
* I used [Scantailor Advanced](https://github.com/4lex4/scantailor-advanced) ([in Docker](https://github.com/ryanfb/docker_scantailor)) to deskew the pages and render the pages as 300dpi black and white (1-bit) tiffs - 30 megabytes
* [tiff2pdf](http://www.libtiff.org/man/tiff2pdf.1.html) and [pdfunite](https://manpages.debian.org/testing/poppler-utils/pdfunite.1.en.html): turn those many tiffs into one pdf
* [OCRmyPDF](https://ocrmypdf.readthedocs.io/en/latest/): OCR with Tesseract, add title and author to the pdf, apply lossless JBIG2 compression - 24 megabytes
