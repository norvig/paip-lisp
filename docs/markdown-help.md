
## Markdown help

## Style guide
Try to use Markdown instead of HTML. 
Try to do minimal changes from the text - don't put a paragraph in one line, or remove trailing space separately, as they make diffs hard to follow. 

Example sections in chapters: 

```
# Chapter 1
## Introduction to Lisp
## 3.1 A Guide to Lisp Style 
### Answer 1.2
```

To mark a block of code, use: 

```
  ```lisp
```
That will give syntax highlighting. Leave out `lisp` if it's something else, like assembly. 

`>` to blockquote - to indent. You only need one at the start of a paragraph. 

Use `*italics*` for *italics*, and `**bold**` for **bold**.

## Line breaks
Non-paragraph line breaks can be tricky, like in the quotes at the start of chapters. 
It looks like two trailing spaces do the trick in both Github Flavored Markdown and Docsify: 


> *Cerium quod factum.*  
> (One is certain of only what one builds.) 
> 
> -Giovanni Battista Vico (1668-1744)  
> Italian royal historiographer 


## Special symbols
There are a lot of special symbols, with special ways of calling them; there's [a Wikipedia page with a list.](https://en.wikipedia.org/wiki/List_of_XML_and_HTML_character_entity_references) Here are some of the common ones: 


| symbol   | entity     |
|----------|------------|
| &times;  | `&times;`  |
| &pi;     | `&pi;`     |
| &int;    | `&int;`    |
| &phi;    | `&phi;`    |
| &asymp;  | `&asymp;`  |
| &ouml;   | `&ouml;`   |
| &plusmn; | `&plusmn;` |
| &eacute; | `&eacute;` |
| &rArr;   | `&rArr;`   |
| &lambda; | `&lambda;` |
| 0&#x0338;| `0&#x0338;`|

Note that these will not work in code blocks. 


## Markdown variants

We're largely targeting [Github Flavored Markdown,](https://github.github.com/gfm/) but the online version uses [docsify,](https://docsify.js.org/) which uses [marked.](https://github.com/markedjs/marked) 

If you'd like to test the docsify parsing, you can run a local server; look at `scripts/httpd` - there are Ruby and Python variants. 
