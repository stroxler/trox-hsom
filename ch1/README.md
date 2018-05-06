# ch1

Chapter 1 is mostly a basic introduction to haskell syntax. There's
some discussion of Euterpea types, although mostly the Euterpea introduction
starts in Chapter 2.

In `src/Lib.hs`, I did take some notes in a comment block on basic types
in Euterpea, which was helpful. I also walked through a few of the examples,
implementing a few small melodies from simple functions.

You can try these functions out interactively - something I didn't go into
detail on in `ch0` - by running
```
stack ghci
```
to get a haskell interpreter session, then calling `:load src/Lib.hs` to load
up the module, and then calling functions directly e.g.
`play $ minor3rdMelody`.

Convenient tip: once you've loaded a module into ghci once, you can reload
it (and all other modules) by running `:reload`. This is handy if you are
writing code in an editor and trying it out in the interpreter.
