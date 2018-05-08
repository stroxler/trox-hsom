{-# LANGUAGE NegativeLiterals #-}
module Lib
    ( someFunc
    ) where

import Euterpea


-- Remember: you can see definitions at
-- https://github.com/Euterpea/Euterpea2/blob/master/Euterpea/Music.lhs#L244-L252

-- exercise 2.1

twoFiveOne :: Pitch -> Dur -> Music Pitch
twoFiveOne root duration = twoChord :+: fiveChord :+: oneChord
  where
    finalDuration = 2 * duration
    mp dur pitch = Prim $ Note dur pitch
    twoChord = (mp 1 $ trans -3 root) :=: (mp 1 $ trans 2 root) :=: (mp 1 $ trans 9 root)
    fiveChord = (mp 1 $ trans -1 root) :=: (mp 1 $ trans 2 root) :=: (mp 1 $ trans 7 root)
    oneChord = (mp 2 $ trans 0 root) :=: (mp 2 $ trans 4 root) :=: (mp 2 $ trans 7 root)

-- exercise 2.2
data BluesPitchClass =
   Do | Me | Fa | Fi | So | Te
  deriving (Show, Eq, Ord, Bounded)

type BluesPitch = (BluesPitchClass, Octave)

do_ :: Octave -> Dur -> Music BluesPitch
do_ o d = Prim (Note d (Do, o))
me_ :: Octave -> Dur -> Music BluesPitch
me_ o d = Prim (Note d (Me, o))
fa_ :: Octave -> Dur -> Music BluesPitch
fa_ o d = Prim (Note d (Fa, o))
fi_ :: Octave -> Dur -> Music BluesPitch
fi_ o d = Prim (Note d (Fi, o))
so_ :: Octave -> Dur -> Music BluesPitch
so_ o d = Prim (Note d (So, o))
te_ :: Octave -> Dur -> Music BluesPitch
te_ o d = Prim (Note d (Te, o))

pcFromBlues :: BluesPitchClass -> PitchClass
pcFromBlues Do = C
pcFromBlues Me = Ef
pcFromBlues Fa = F
pcFromBlues Fi = Fs
pcFromBlues So = G
pcFromBlues Te = Bf

pitchFromBlues :: BluesPitch -> Pitch
pitchFromBlues (pc, o) = (pcFromBlues pc, o)

fromBlues :: Music BluesPitch -> Music Pitch
fromBlues (Prim (Note d p)) = Prim $ Note d $ pitchFromBlues p
fromBlues (Prim (Rest d)) = Prim $ Rest d
fromBlues (x :+: y) = fromBlues x :+: fromBlues y
fromBlues (x :=: y) = fromBlues x :=: fromBlues y
fromBlues (Modify control p) = Modify control $ fromBlues p

exampleBluesLick :: Music BluesPitch
exampleBluesLick =
  (do_ 4 en) :+: (me_ 4 en) :+: (fa_ 4 en) :+: (fi_ 4 en) :+:
  (so_ 4 en) :+: (te_ 4 en) :+: ((do_ 5 en) :=: (so_ 4 en))

playBluesExample :: IO ()
playBluesExample = play $ fromBlues $ exampleBluesLick

-- exercise 2.3 and 2.4 are calculation exercises
--
-- 2.3 is brute-force and not so interesting
-- 2.4 can be done as follows
--   trans i (trans j p)
--   trans i (pitch (absPitch p + j))
--   pitch (absPitch (pitch (absPitch p + j)) + i)
--   pitch (absPitch p + j + i)      <-- by exercise 2.3
--   trans (j + i) p

-- exercise 2.5
-- (check the docs for the pitch and absPitch functions, which
--  translate between AbsPitch (basically midi int) and Pitch)

-- Note: I realized *after* I wrote this exercise that
-- there's already a function `trans` which does the same thing; the
-- goal of the exercise was really just to write transM in terms of trans,
-- to work on Pitch from inside a Music context directly rather than by adding
-- (Modify (Transpose x)) to a Music Pitch.
transPitch :: AbsPitch -> Pitch -> Pitch
transPitch nSemitones = pitch . (+ nSemitones) . absPitch

transM :: AbsPitch -> Music Pitch -> Music Pitch
transM ap (Prim (Rest d)) = Prim $ Rest d
transM ap (Prim (Note d p)) = Prim $ Note d (trans ap p)
transM ap (Modify control x) = Modify control $ transM ap x
transM ap (x :+: y) = (transM ap x) :+: (transM ap y)
transM ap (x :=: y) = (transM ap x) :=: (transM ap y)


someFunc :: IO ()
someFunc = play $ Prim $ Note en (C, 4 :: Octave)
