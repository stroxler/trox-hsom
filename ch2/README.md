# ch2

Most of the exercises have tests. There were two cases where I needed
to map a function over `Music`, so I wrote a map for `Music` as part of
my work.

Exercise 2.2, in addition to asking us to write code, asks for a blues melody.
I wrote a two-measure boogie lick, which is not tested but you can try it
out interactively after running `stack ghci`:
```
playMajorBlues C $ majorBluesLick 2
playMajorBlues F $ majorBluesLick 2
```
