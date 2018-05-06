# Steven Troxler's code, studying Haskell School of Music

## Intro: what is Haskell School of Music / links and info

Dr. Paul Hudak at Yale developed the course and textbook
"Haskell School of Music: From Signals to Symphonies",
along with a (general purpose)  MIDI-backed music library `Euterpea` and a
book-specific example library `HSoM` to teach haskell and music processing.

The book is currently undergoing review for formal publication. As of this
writing (May 2018), there's an older version available for free from the
[Yale CS website](http://haskell.cs.yale.edu/wp-content/uploads/2015/03/HSoM.pdf)

Links:
 - [Euterpea homepage](http://www.euterpea.com/)
 - [Download instructions](http://www.euterpea.com/download-and-installation/)
 - [Euterpea (library on hackage](http://hackage.haskell.org/package/Euterpea)
 - [Euterpea docs](https://hackage.haskell.org/package/Euterpea/docs/)
 - [HSoM (library on hackage](http://hackage.haskell.org/package/HSoM)
 - [HSoM docs](https://hackage.haskell.org/package/HSoM/docs/)

### Haskell

Haskell has a reputation among programmers for being quite difficult.

If you are coming to programming for the first time, learning your first
language is *always* challenging. I would actually argue that Haskell isn't
much harder for beginners than other languages; it's certainly easier to pick
up than C (which is used, for example, in Harvard's intro CS50 course).

For an absolute beginner, many intro haskell resources, such as the freely
available
[Learn You a Haskell for Great Good](http://learnyouahaskell.com/chapters)
should prove useful enough to get you going on Haskell School of Music.

For programmers who are experienced in other languages, Haskell can be
frustrating to learn. It's different enough that for a while you
feel like a beginner again. And getting "over the hump" for knowing how to
make real projects, rather than just toys, can be difficult because
 - The level of type abstraction in haskell libraries is much higher than
   in the libraries of most other ecosystems
 - In day-to-day work (much more so than toy projects) most of what we do
   involves interacting with other systems, so you *very* quickly have to know
   something about `IO`, and often monad transformers as well, to stand up
   actual microservices in Haskell

If you're hoping to learn haskell and also understand how to build real-world
tools in it, I *highly* recommend investing in the book
[Haskell Programming from First Principles](http://haskellbook.com/)
which is an extensively beta-tested book for teaching not only the langauge
but also patterns of haskell development.

## This repository

This repo is a work in progress, and from past experience it's likely to never
be finished. Here are modules thus far:

### ch0: getting Euterpea running on stack

The install instructions for Euterpea rely on a global cabal setup, which is
not the preferred way for modern haskell development to work.

In the Chapter 0 readme, I walk through how to create a `stack` project
(`stack` is the currently dominant user-facing build tool for Haskell) and
add `Euterpea` as a project dependency.

By the end of it, you should be able to make music using modern, production
haskell build tools. It doens't cover any actual theory, it's just about making
the tech fit together.

### ch1: (coming up next)
