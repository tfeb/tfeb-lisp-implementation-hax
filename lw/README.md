# [LispWorks hacks](https://lispworks.com/)
## [`tools`](tools/)
Tools for LW.
### LW command line tools
**`lw-comands`** provides some extra commands for the LW toplevel.  These are the commands I use: they may or may not be useful for other people.  The documentation below is rough: a lot of the commands have slightly unclear semantics which I can't always remember.

This module needs both ASDF and [`require-module`](https://tfeb.github.io/tfeb-lisp-tools/#requiring-modules-with-searching-require-module).  It makes some attempt to make sure ASDF is loaded but it just assumes `require-module` is there.  I might remove this dependency as I don't actually use the `:require` command very often any more.

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

**Background.**  `:&` will run a form in a new process.

**Modules.**  `:require` just calls `require-module`.  This is the only place there is a dependency on `require-module`.

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