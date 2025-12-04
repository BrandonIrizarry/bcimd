# Table of Contents

+ [Introduction](#introduction)
+ [Installation](#installation)
+ [Rationale](#rationale)
+ [Plans](#plans)

<a id="introduction"></a>
# Introduction

This library, subsumed in a single file, consists of extras for Emacs'
excellent [markdown-mode](https://jblevins.org/projects/markdown-mode/).

This library contains:

1. Functions for generating and removing a table of contents.

# Demo

![Generate and remove a Table of Contents](https://github.com/BrandonIrizarry/bcimd/blob/master/output-2025-12-03-19%3A32%3A31.gif)

<a id="installation"></a>
# Installation

Insert anywhere in your load path. `use-package` vc-install should
work too where available, viz.:

```elisp
(use-package bcimd
    :vc (:url "https://github.com/BrandonIrizarry/bcimd" :rev :newest))
```

<a id="rationale"></a>
# Rationale

I'm using [Eleventy](https://www.11ty.dev) to write a blog. However, manually updating
the Table of Contents "infrastructure"—inserting anchor tags, along
with updating the Table of Contents itself—became a hassle, especially
when deciding on exactly how to divide the post into sections. I found
myself becoming stingy over the number of sections a post had, just so
that I could avoid repeated editing. So, I decided that was that, and
automated that step away with some Elisp.

<a id="plans"></a>
# Plans

1. [x] Erase any existing previous table of contents before generating
       a new one.
