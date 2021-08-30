# The Radical Yasnippet Collection

This repository contains a small collection of *radical* snippets for
[yasnippet](https://github.com/joaotavora/yasnippet).


# Installation

As this collection has only just been created, for the time being this
collection will not available on [MELPA](https://melpa.org/). In the meantime,
you may install it directly from my [personal
archive](https://gustafwaldemarson.com/elpa/). This can be done by adding the
following snippet to your `emacs`.


```emacs-lisp
(require 'package)
(add-to-list 'package-archives
             '("xaldew" . "https://gustafwaldemarson.com/elpa/"))
(add-to-list 'package-unsigned-archives "xaldew")
(package-initialize)
```

# Examples

Below are some examples of the snippets available in this repository, with some
caveats listed below.

## Python

### Non-Type Annotated Function - Google Style

![Google Python Style](./img/fn_google_docstring.gif)

### Non-Type Annotated Function - ReST Style

GIF-TODO.

### Type Annotated Function

IMPL-TODO.

## C/C++

### Documented Function - Doxygen Style

This snippet attempts to grab type-information using the
[Semantic](https://www.gnu.org/software/emacs/manual/html_node/emacs/Semantic.html)
backend, falling back to regexp based extraction should it fail or not be
available.

GIF-TODO.
