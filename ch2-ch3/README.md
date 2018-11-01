# ch2 and ch3

I put the two chapters together because they are pretty similar in spirit.

## Ch2

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

## Ch3

Again, most of the exercises have tests. I decided to create a
`playMode` function to play the modal scales, you can try it out in
`stack ghci`:
```
playMode (C, 4) Ionian
playMode (D, 3) Aeolian
```

I also did not write any test for the "Frere Jacques" exercise, but you can
test the plain melody and the round with:
```
E.play $ areYouSleeping
E.play $ areYouSleepingRound
```
