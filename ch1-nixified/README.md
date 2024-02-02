This was an attempt to nixify the old ch1 Euterpea project
with nix flakes.

Unfortunately, it seems that the PortMidi haskell package
is so out of date that it's a challenge finding an environment
that will compile it (I can't figure out how to get recent
versions of clang to not produce warnings as errors on the
C code, so I can't compile the dependencies).

Nontheless, the actual flakeified stack setup here works,
so it's a nice example and I wanted to add it to version
control
