module Lib
    ( someFunc
    ) where

import Euterpea

-- type Octave = Int
-- type Pitch = (PitchClass, Octave)
--
-- note :: Dur -> a -> Music a
-- rest :: Dur -> Music a
--
-- (:+:) :: Music a -> Music a -> Music a   -- sequence
-- (:=:) :: Music a -> Music a -> Music a   -- parallel
-- trans :: Int -> Pitch -> Pitch           -- translate n semitones
--
-- C :: PitchClass
-- c :: Octave -> Dur -> Music Pitch
--

concertA, a440 :: (PitchClass, Octave)
concertA = (A, 4)
a440 = (A, 4)

harmonize :: Dur -> Pitch -> Int -> Music Pitch
harmonize d p offset = (note d p) :=: (note d (trans offset p))

minor3rdMelody :: Pitch -> Pitch -> Pitch -> Music Pitch
minor3rdMelody p1 p2 p3 =
  (h p1) :+: (h p2) :+: (h p3)
    where h p = (harmonize qn p (-3))

playExampleMinor3rdMelody :: IO ()
playExampleMinor3rdMelody = play $ minor3rdMelody (C, 4) (D, 4) (Ef, 4)


pitchList2Run :: Dur -> [Pitch] -> Music Pitch
pitchList2Run _ [] = rest 0
pitchList2Run d (p:ps) = (note d p) :+: (pitchList2Run d ps)

exampleRun :: Music Pitch
exampleRun = pitchList2Run en [(C, 4), (D, 4), (Bf, 3), (B, 3), (C, 4)]


-- Haskell numeric types
-- Int : fixed-width integer
-- Integer : arbirary-precision integer
-- Rational : arbitrary-precision rational
-- Float : single-word floating point
-- Double : double-word floating point
-- Complex : complex; probably implemented in terms of Double but HSoM does not say


someFunc :: IO ()
someFunc = putStrLn $ show a440
