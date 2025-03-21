# [LispWorks hacks](https://lispworks.com/)
## [`tools`](tools/)
Tools for LW.
### LW command line tools
**`lw-comands`** provides some extra commands for the LW toplevel.  These are the commands I use: they may or may not be useful for other people.  The documentation below is rough: a lot of the commands have slightly unclear semantics which I can't always remember.

This module needs ASDF and makes some attempt to make sure ASDF is loaded.

**Package commands.**  These let you change package and maintain a stack of packages so you can push or pop packages.

- `:pkg` with an argument sets the current package to its argument. With no argument it prints the current package, the previous package and the stack.
- `:pkg-` swaps the current package to the previous package.
- `:pkg>` sets the current package to its argument, pushing the former package onto the stack.
- `:pkg<` pops a package from the stack and makes it current.
- `:pkg~` swaps the current entry with the top of the package stack.

**File commands.**  These commands let you compile & load files, remembering a notion of the current file.

- `:ld` will load one or more files, remembering their names.  With no arguments it will load the list of files it has remembered.
- `:cf` will compile one or more files, remembering their names as `:ld` does (this is the same list of files).
- `:cl` compiles and loads files: it just combines `:cf` and `:ld`.

**Doing things to systems.** These commands will let you compile and load systems.  There is a mild assumption that system declarations live in files called `sysdcl.lisp` if not loaded otherwise.  For things like ASDF system declarations you can make symlinks.

- `:sysdcl` loads system declarations.  With no arguments it looks for `sysdcl.lisp` in the current working directory.  With arguments it will look at those directories and load `sysdcl.lisp` from each one.
- `:lss` will list known systems, both LW sysdems and ASDF systems.
- `:cps` will compile one or more systems.  With arguments it compiles systems with those names.  With no arguments it compiles the systems it last compiled.
- `:lds` will load systems, with defaults as for `:cps`.

**Inspecting.**  `:gi` will run the GUI inspector on either its arguments, or with no arguments either `*` or `/` if more than one value was returned by the last thing in the REPL.

**Background and foreground.**  `:bg` will run a form in a new process, `:fg` will just evaluate something.  Both of these implicitly compile a function to do this: this means you get more errors, sooner.  `:fg` exists because it can be useful with `:lrc`, but *caveat emptor*.

**Directory commands.**  These let you change directory and also support directory aliases, autoloaded from files.

*Directory aliases.*  These get loaded from files with names like `.directory-aliases` when they exist: when this file is loaded it will attempt to load such a file from your home directory, and whenever you change directory with one of these commands it will look for such a file in the new directory and load it if found.  The files just contain lots of two-element lists of `(<alias> <translation>)`, where `<alias>` is a symbol, and in practice a keyword is usually best.  Here's part of the one that lives in my home directory:

```lisp
(:src "src/lisp/")
(:play "play/lisp/")
(:work "work/lisp/")
```

The system makes attempts to read these files safely using `with-standard-io-syntax` and turning off `*read-eval*`.  But it's only as safe as the reader: *caveat emptor*.

Given these aliases, all the directory commands accept arguments which can be put together from aliases and directory fragments: `:src "modules"` for instance means 'make a pathname by translating `:src` and then merge it with a pathname whose diretory is "modules".  One implication of this is that aliases should translate to *directory* names, not the file names of directories.  This means in practice that their translations should (if they're not other aliases) end with a `/` on Unixoid systems.

Aliases are translated recursively, and there is loop detection (in fact loops are detected when loading aliases and those are rejected).

The translations of aliases are made with respect to the directory they were loaded from, so relative pathnames work fine.

The nice thing about autoloading aliases is that you can have a bunch of them which sit at the top of some source directory and then let you navigate around it, but only spring into existence at the point you change directory to it.

So, here are the commands.

- `:cd` changes directory.  With no arguments it changes to your home directory.  With an argument like `-` (string or a symbol with that name) it changes to the previous directory.  With an argument like `=` (string or symbol with that name) it prints the current aliases and their translations.
- `:pwd` prints the current working directory.
- `:pushd` changes directory and pushes the old directory onto the stack.
- `:popd` pops the top of the stack and changes to that directory.
- `:dirs` prints the directory stack.  With any argument it will *return* them instead.
- `:dired` points the editor at a directory, with the default being `"."`.

**Prompt commands.**  These manipulate named prompts, stashed in the `*prompts*` variable which is an alist of the form `(<name> . <prompt-string>)`.  See the LW documentation for the syntax of prompt strings.  There are some predefined prompts in `*prompts*` which I find useful:

- `:standard` is the default LW prompt;
- `:short` is shorter in the presence of long package names;
- `:minimal` is even shorter.

`:prompt` then allows you to select a prompt, either by name, directly as a string or by some special things:

- `-` swaps the current and previous prompts;
- `+` sets the prompt to what it thinks it should be, which by default is the standard prompt, and stashes the previous prompt in the last prompt.
- `=` does nothing.

With no arguments, `:prompt` will print what the current state is.

There is no stack of prompts, and this whole mechanism is not really sorted out, but it's enough for me.

**Running these commands from files.**  Quite often I want to say, for instance

```
:cl "my-file"
:pkg> :my-package
:prompt :minimal
```

Every time I start working on something.  The `:lrc` command lets you stash these things in a file and run them.

```
:lrc "setup"
```

will look for a file called `setup.lrc`, and read commands from it.  The commands are wrapped up as lists in the obvious was, so a file to do the above would contain:

```
(:cl "my-file")
(:pkg> :my-package)
(:prompt :minimal)
```

While the file is being read, `*read-eval*` is bound to `nil` but there are no other adjustments made: in particular it is not (and can't be) surrounded by `with-standard-io-syntax`.  You can specify zero or more files to read.  (Yes, `(:lrc "other-file")` works.)

The contents of these files is *only* sequences of the commands specified here: they're not general programs and never will be (why would they be? CL is already that), but just ways of packaging up shorthands.  On the other hand, there is `:fg`.

**Module, package, &c.**  `lw-commands` lives in `:org.tfeb.lw.lw-commands`, provides `:org.tfeb.lw.lw-commands` and pushes `:org.tfeb.lw.lw-commands` onto `*features*`.

Exports:

- `declare-extra-lw-commands` is how the commands get defined – it's exported because I have other utilities which use it but it's not documented;
- `*prompts*` is the alist of promts.

**Implementation note.** `lw-commands` currently uses an ancient, undocumented hack to define new toplevel commands to LW, based on grovelling around in the implementation in 2001-2002.  `system:define-top-loop-command` is now how you are meant to do this, but it does not (yet) have a way of adding documentation strings, so `:?` is less useful.  That's why I'm still using this ancient hack.

## [`modules`](modules/)
Small useful things for LW.

### Advice
**`recording-advice`** teaches `defadvice` how to record what things you have advised.  `*recording-advice*` controls whether advice is recorded and `map-recorded-advice` calls a function on the dspec and name of each bit of recorded advice.  The function is allowed to remove or add more advice.

**`replayable-advice`** allows you to to 'replay', or reapply, advice.  `*replayable-advice*` controls whether advice is noted for replaying, and `map-replayable-advice` calls a function for each bit of replayable advice with three arguments: the dspec, the name, and a function of no arguments which, if called, will replay the advice.  Finally `forget-replayable-advice`, if called with a dspec and name will forget the replayability of that bit of advice, if it has any.

Both of these work by themselves advising the internal function to which `defadvice` expands and are thus very fragile.

### Stack control
**`allowing-stack-extensions`** is a macro which allows you to control how and whether LW will extend the stack, by invoking the appropriate restart.   For instance

```lisp
(allowing-stack-extensions (:limit 100000)
  ...)
```

will allow the stack to be extended while it is smaller than `100000`.  The default value of the limit is `*stack-limit*` which in turn defaults to the current stack length at load time.  There are options to say 'always extend', 'never extend' and 'use `*stack-limit*` dynamically to decide'.

You will need [metatronic macros](https://tfeb.github.io/tfeb-lisp-hax/#metatronic-macros) to use this.

### Protecting variables
`protecting-variables` is a macro which uses information about the lexical environment to make variables read-only.  I would like this to be portable between at least several implementations, and if it becomes so it will move from here (the package will be renamed).  In order to work it needs to be able to map over lexical environments finding variable information, which you can do in LW with `system:map-environment`.

I wrote this because someone asked whether it was possible to do something like it: the only time I can really see it being useful is if you have code which involves macros which might be secretly assigning to things in a way which is hard to see in the source.  It *doesn't* protect you against mutating objects which are the value of variables which is obviously a hard problem in general.  It can't fully protect against assigning to special variables (special variables are not protected by default, because you can always say `(setf (symbol-value ...) ...)`.  It can (if you ask) protect symbol-macros although this tends to cause warnings about unused references which are hard to avoid.

It's not much more than a toy, but it exists.  You will need [collecting](https://tfeb.github.io/tfeb-lisp-hax/#collecting-lists-forwards-and-accumulating-collecting) to use it.

## [`size-by-class`](size-by-class/)
This contains a utility which will let you see memory usage by class and log its changes over time to a file, and a script (written in Racket) which will let you plot this file.  To use this you will need a recent version of [Štar](https://tfeb.github.io/#%C5%A1tar-an-iteration-construct-for-common-lisp), and my `collecting` hack.  To run the plotter you'll need [Racket](https://racket-lang.org/).

The function `size-by-class` will return a list of `class-counter` objects sorted by size usage. These have slots for the name of the class, the number of instances of this class, the number of bytes used by these instances, and the cumulative size for this class and all smaller ones.  You can prune this list using the `min-ratio` argument which will only count classes whose usage is greater than that ratio of the total size.  In this case there will normally be a `class-counter` object whose name is `nil` at the end of the list, whose job is to account for the remaining space.  You can also filter objects using an arbitrary test function, which would let you, for instance, look only at one class or classes you care about.

`log-size-by-class` will run `size-by-class` periodically, dumping its output to a log file.  Its argument is the log file name.  It has a number of keyword arguments:

- `every` is the time in seconds to wait between runs, default `60`;
- `count` is how many times to log with `nil`, the default meaning 'run for ever';
- `min-ratio` is the `min-ratio` argument to `size-by-class`, default `0.05`;
- `append` tells it to append to the log file.

The way to use this function is to run it in a background process:

```lisp
(process-run-function
 "log size"
 ()
 #'log-size-by-class
 "size.ldat"
 :every 10
 :count 100)
```

`plot-sbc.rkt` is a mindless program which will plot the usage from a log file.  If you install it as `plot-sbc`, you can run it as

```
$ plot-sbc size.ldat size.png
```

for instance.  It makes an attempt to deal with the log file changing as it runs (if it gets a read error, it retries after a second).  There is a `-n n` argument which lets you select how many classes to plot.