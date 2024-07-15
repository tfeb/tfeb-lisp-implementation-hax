# [TFEB.ORG Lisp implementation hax](https://github.com/tfeb/tfeb-lisp-implementation-hax "TFEB.org Lisp toys")
This repo contains some implementation-specific Lisp code, for implementations I use or have used.  Currently not much has been added but I will probably add more over time.

Some of this code may work, some of it may once have worked, some of it may never have worked: *caveat emptor*.

## General
### Requirements
Some of this code may depend on other, portable, things I've written, and in particular on [`require-module`](https://github.com/tfeb/tfeb-lisp-tools#requiring-modules-with-searching-require-module "require-module"). Things may also assume [Quicklisp](https://www.quicklisp.org/ "Quicklisp")  Things in directories which have names like `modules` should be either independently loadable or know how to load their prerequisites with the tools provided by `require-module`, possibly relying on a fallback to `ql:quickload`.  Things in directories which have names like `systems` will be loadable via some system-definition tool although this may not be ASDF.  Things in other directories are probably more for looking at than using.

Probably none of this code is portable CL.

### Zero history
The repo from which the toys are published was invented in 2022, but some of them are much older than that.  Some of that history exists but not in the publication repo.

### Naming conventions
Probably all of this code except for any antiques will use use *domain-structured names*: packages, modules, features and so on have names which start with a reversed DNS domain and then continue to divide further.  The names will generally indicate the implementation (so for instance `org.tfeb.lw.*`).  See [the TFEB.ORG tools documentation](https://github.com/tfeb/tfeb-lisp-tools#naming-conventions "TFEB.ORG tools / Naming conventions") for a little more on this.

---

## Implementation directories
There are, or may be, `README` files for implementation directories.

### [`lw`](lw/): [LispWorks](https://lispworks.com/ "LispWorks")
Code for LispWorks.

---
The TFEB.ORG Lisp implementation hax are copyright 1989-2024 Tim Bradshaw.  See `LICENSE` for the license.