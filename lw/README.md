# [LispWorks hacks](https://lispworks.com/)
## [`modules`](modules/)
Small useful things.

### Advice
**`recording-advice.lisp`** teaches `defadvice` how to record what things you have advised.  `*recording-advice*` controls whether advice is recorded and `map-recorded-advice` calls a function on the dspec and name of each bit of recorded advice.  The function is allowed to remove or add more advice.

**`replayable-advice.lisp`** allows you to to 'replay', or reapply, advice.  `*replayable-advice*` controls whether advice is noted for replaying, and `map-replayable-advice` calls a function for each bit of replayable advice with three arguments: the dspec, the name, and a function of no arguments which, if called, will replay the advice.  Finally `forget-replayable-advice`, if called with a dspec and name will forget the replayability of that bit of advice, if it has any.

Both of these work by themselves advising the internal function to which `defadvice` expands and are thus very fragile.