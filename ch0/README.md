# ch0

Setup to use stack for Euterpea:
 - adding dependencies
 - handling stack.yaml updates

We don't go through HSoM in this guide, but the steps are the same.

## Creating a new project, understanding the config files

First, create the project:
```
stack new ch0
```

Note that this project has a `stack.yaml`, a `package.yaml`, and
a `ch0.cabal`. Here's a rough breakdown of uses:
 - `ch0.cabal` is generated. In some templates of stack projects
   (e.g. if we did `stack new ch0 simple` this file would be used, but
   in the default template `package.yaml` replaces it.
 - `stack.yaml`, whose job is mostly to help us resolve dependencies. Most of
   the core haskell ecosystem packages are tracked in Stackage snapshots,
   which you can think of as a mega-project pinning a huge set of compatible
   library versions. Stack's purpose is to wrap our builds and dependency
   resolutions to use a Stackage snapshot by default. But Stackage does not
   have the whole universe, so we use `stack.yaml` to modify behavior:
    - for libraries not tracked by Stackage (there are *many* libraries in
      Hackage that the community does not have time to keep pinned in
      Stackage), we will need to add them in the `extra_deps` section of
      `stack.yaml` so that stack knows what to pull in. We'll need to do this
      for `Euterpea` and several of its dependencies.
    - sometimes we need to override a package pinned in Stackage with an
      earlier or later version; for example we'll see that Euterpea, as of
      the time of writing this (May 2018) requires an older version of
      `PortMidi` than is in Stackage.
 - `package.yaml` is where we describe the project and dependency structure.
   You'll note that our default setup has:
   - some metadata at the top, including the package name and version
     (feel free to edit the author/github info if you plan on sharing, but
     for a homework project you can ignore metadata)
   - a dependencies section and a library section. This describes the library,
     whose code we will put in `src`
   - two more subproject secions, each of which have their own source-dirs
     and dependencies:
     - `executables`, with a subfield `ch0-exe`. This describes the binary
       that we'll build if we run `stack build`. In a simple project it might
       depend only on our library (which is `ch0` here because of the overall
       name of our project) but we could add more dependencies - e.g. a
       commandline flag parser library - if we wanted to
     - `tests`, which describes our test suite. This would typically have
       no dependencies except for our library and any testing frameworks
       we pull in.

You can check that it compiles by running `stack build`, and that it tests
by running `stack test`. You can open an interactive interpreter session
with `stack ghci`.

## Adding Euterpea to the configuration

### Declaring the dependency

In `package.yaml`, under the `depencencies` section (which currently should
list `base`) add `Euterpea`.

Now try running `stack build`. This should produce an error, which will
probably look something like this:
```
Error: While constructing the build plan, the following exceptions were encountered:

In the dependencies for ch0-0.1.0.0:
    Euterpea must match -any, but the stack configuration has no specified version (latest matching version is 2.0.4)
needed since ch0 is a build target.

Some potential ways to resolve this:

  * Recommended action: try adding the following to your extra-deps in /Users/steventroxler/kode/trox/hsom/ch0/stack.yaml:

- Euterpea-2.0.4

  * Set 'allow-newer: true' to ignore all version constraints and build anyway.

  * You may also want to try using the 'stack solver' command.

Plan construction failed.
```

Here, stack is telling us that we declared `Euterpea` as a dependency, but
it's not in Stackage so stack doesn't know how to resolve it for us. The
"Recommended action" line tells us what to do: go to `stack.yaml` and add
`Euterpea-2.0.4` to the `extra-deps` section.

Do this, and run `stack build` again. Once again, we'll see errors:
```

Error: While constructing the build plan, the following exceptions were encountered:

In the dependencies for Euterpea-2.0.4:
    PortMidi-0.1.6.1 from stack configuration does not match ==0.1.5.2 (latest matching version is 0.1.5.2)
    arrows must match >=0.4 && <0.5, but the stack configuration has no specified version (latest matching version is 0.4.4.2)
needed due to ch0-0.1.0.0 -> Euterpea-2.0.4

Some potential ways to resolve this:

  * Recommended action: try adding the following to your extra-deps in /Users/steventroxler/kode/trox/hsom/ch0/stack.yaml:

- PortMidi-0.1.5.2
- arrows-0.4.4.2

  * Set 'allow-newer: true' to ignore all version constraints and build anyway.

  * You may also want to try using the 'stack solver' command.

Plan construction failed.
```

Here, stack is telling us about two types of problems (it couldn't tell us
about these issues until we first fixed `Euterpea`, because these libraries
are `Euterpea` dependencies:
  - the `PortMidi` package is in Stackage, but its version `0.1.6.1` does not
    match Euterpea's dependency requirement. We need to override with an
    older version
  - the `arrows` library, like Euterpea, is not in Stackage at all.
Once again, we can follow the "Recommended action" instructions.

If you keep running `stack build`, you'll get two more errors as we recurse
down the dependencies of `arrows`, and you'll have to add `Stream` and
`lazysmallcheck` to your `stack.yaml`.

Once everything resolves, `stack` will start pulling down all the dependencies.
This can take a minute or two to finish.

## Making sure Euterpea runs

If you are on a mac or linux machine, you'll need to install a MIDI synth
and have it running. I installed SimpleSynth on my mac, and just starting
it up and minimizing it was enough to make Euterpea work properly.

You can test interactively by running `stack ghci` to get an interpreter, and
then running
```
import Euterpea
play $ c 4 qn
```
to play middle C for a quarter-note. If you hear a sound, everything worked!

I like to also add a blues lick to my project for fun, so that I can check
that I'm building a binary which can play music. I modified `Lib.hs`, which
originally has a print message `IO` action, to this:
```
module Lib
    ( someFunc
    ) where

import Euterpea

bluesLick =
  let repeated = ((g 4 en) :=: (bf 4 en))
   in
    ((c 4 qn) :=: ((ef 4 en) :+: (e 4 en))) :+: ((d 4 en) :=: (f 4 en)) :+:
    repeated :+: repeated :+: repeated :+:
    repeated :+: repeated :+: repeated :+:
    repeated :+: repeated :+: ((f 4 en) :=: (a 4 en)) :+:
    ((e 4 qn) :=: (g 4 qn))



someFunc :: IO ()
someFunc = play $ bluesLick
```

Since `app/Main.hs` already imports and runs `someFunc`, this allows us to
check that everything works:
```
# build an executable from app/Main.hs
stack build
# run the executable (you could run it directly, but stack  knows where to find
# the build executable so it's easier to use "stack exec" than inspecting the
# output directory yourself. The actual location is something like
# .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/ch0-exe/ch0-exe)
stack exec ch0-exe
```

## Extra practice

If you are new to `stack`, you might want to try adding `HSoM` as a dependency
in `package.yaml`, and walking through the steps for updating `stack.yaml`
as extra practice.
