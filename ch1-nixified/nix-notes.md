# Nix integration

I decided to try adding nix flakes in-place to an existing
stack project, following the pattern in:
https://docs.haskellstack.org/en/stable/nix_integration/

Unfortunately unless I want to dig around old nixpkgs versions
I'm still stuck doing a major (since 2018) upgrade, but I've
been through a few of those and hopefully it will be smooth.

My first attempt using the latest LTS stackage failed, it seems
like the pinned version of nixpkgs has a compile error in
GHC 9.4.6, so I backed off to the latest stackage release using
9.6.3.

I don't want to commit this change to trunk, instead I'll
start a new folder because I don't really want to reuse existing
code I wrote, plus I don't really want to remove the history of
my stack lineage.

# How to actually run things

To bootstrap the background environment (ghc, stack, and dev tools)
run `nix flake develop`.

This will build the flake attribute `devShells.<system>.default`,
which has all that background tooling.

At that point you can use stack for the Haskell part of the project,
```
stack build
```
