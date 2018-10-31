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
    Euterpea must match -any, but the stack configuration has no specified version (latest matching version is 2.0.6)
needed since ch0 is a build target.

Some potential ways to resolve this:

  * Recommended action: try adding the following to your extra-deps in /Users/steventroxler/kode/trox/hsom/ch0/stack.yaml:

- Euterpea-2.0.6

  * Set 'allow-newer: true' to ignore all version constraints and build anyway.

  * You may also want to try using the 'stack solver' command.

Plan construction failed.
```

Here, stack is telling us that we declared `Euterpea` as a dependency, but
it's not in Stackage so stack doesn't know how to resolve it for us. The
"Recommended action" line tells us what to do: go to `stack.yaml` and add
`Euterpea-2.0.6` to the `extra-deps` section.

Do this, and run `stack build` again. We may see more errors of a similar type, 
telling us about dependencies of `Euterpea` that are either not available in
stackage, or have version conflicts. The error messages will guide us through
fixing these issues by modifying `stack.yaml`.

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

## Adding a commandline tool

In addition to getting Euterpea up and running - which requires figuring out
stack and such - I decided to use Ch 1 to learn haskell options parsing.

This is way out-of-scope for basic Haskell School of Music learners, but
if you are interested:
 - Check out the code in `src/Lib.hs` and read until you understand how the
   `Command` datatype and `executeCommand` function are related
 - Read the code in `app/Main.hs` and read through all of the comments

I found learning `optparse-applicative` to be a little time intensive because
it's a very idiomatic Haskell library, using applicatives and monoids quite
a bit. Here are a few resources to get going:
 - [short tutorial from packtpub](https://www.youtube.com/watch?v=UopYjb6eaLo):
   It doesn't go into enough detail to explain everything I'm doing, but it's
   a nice quick getting started guide.
 - [24 days of Haskell tutorial](https://ocharles.org.uk/blog/posts/2012-12-17-24-days-of-hackage-optparse-applicative.html):
   Another quick guide, covers a tiny bit more than the video.
 - [hackage quick guide](https://haskell-lang.org/library/optparse-applicative):
   This is pretty fast-moving, but it has an example of subcommands, which
   was part of what I wanted to learn.
 - [package readme](https://github.com/pcapriotti/optparse-applicative):
   The guides above are all pretty helpful for getting started, because the
   README is a bit overwhelming to newbies. But the README is the most
   complete documentation of the package.

### Using the commandline tool

The commandline parsing tool is help-guided, so you can navigete via `--help`
```
stack build
stack exec ch1-exe -- --help
```
(note the extra `--`, used to separate stack flags from program flags).
You'll see something like this:
```
Usage: ch1-exe COMMAND
  Play a note or a lick

Available options:
  -h,--help                Show this help text

Available commands:
  note                     Play a note
  lick                     Play a lick
```

We can recursively explore the tool with `--help`:
```
stack exec ch1-exe -- lick --help
stack exec ch1-exe -- note --help
```

And we can actually make some music, if we have our midi synth running:
```
stack exec ch1-exe -- note e --octave 3
stack exec ch1-exe -- note g     # octave defaults to 4
stack exec ch1-exe -- note -o 5  # note defaults to c, and -o is short

stack exec ch1-exe -- lick              # play the short lick
stack exec ch1-exe -- lick --extended   # play the long lick
```
