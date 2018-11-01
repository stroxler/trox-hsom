{-# LANGUAGE NegativeLiterals #-}
module Ch2 where

import EuterpeaExports
import qualified Euterpea as E

twoFiveOne :: Pitch -> Dur -> Music Pitch
twoFiveOne p d =
  let
    tr i = Prim $ Note d (E.trans i p)
    do_ = tr 0; re_ = tr 2;
    mi_ = tr 4; fa_ = tr 5;
    hso_ = tr  7; la_ = tr 9;
    lso_ = tr -5; ti_ = tr -1;
  in
    E.line [ E.chord [re_,  fa_,  la_]
           , E.chord [lso_, ti_,  re_]
           , E.chord [do_,  mi_, hso_]
           ]


data MajorBluesPitchClass = MbDo | MbRe | MbMe | MbMi | MbSo | MbLa
  deriving (Eq, Show, Ord)


type MajorBluesPitch = (MajorBluesPitchClass, Octave)


mbdo, mbre, mbme, mbmi, mbso, mbla :: Octave -> Dur -> Music MajorBluesPitch
mbdo o d = Prim $ Note d (MbDo, o)
mbre o d = Prim $ Note d (MbRe, o)
mbme o d = Prim $ Note d (MbMe, o)
mbmi o d = Prim $ Note d (MbMi, o)
mbso o d = Prim $ Note d (MbSo, o)
mbla o d = Prim $ Note d (MbLa, o)


-- MajorBluesPitch may be converted into various actual pitches.
-- The meaning of octive is tied to the octave of the root. E.g.
-- if root is E, then a (Do, 4) gets translated into (E, 4),
-- but a (La, 4) gets translated into a (Cs, 5) because it needs
-- to be in the fourth octave *relative to E*
fromMajorBlues :: PitchClass -> MajorBluesPitch -> Pitch
fromMajorBlues root (mb, o) =
  let
    rootPitch = (root, o)
    tr = flip(E.trans) rootPitch
  in case mb of
    MbDo -> tr 0; MbRe -> tr 2;
    MbMe -> tr 3; MbMi -> tr 4;
    MbSo -> tr 7; MbLa -> tr 9;


mapMusic :: (a -> b) -> Music a -> Music b
mapMusic f (Prim (Note d aa)) = Prim (Note d $ f aa)
mapMusic f (Prim (Rest d)) = Prim (Rest d)
mapMusic f (m0 :+: m1) = (mapMusic f m0) :+: (mapMusic f m1)
mapMusic f (m0 :=: m1) = (mapMusic f m0) :=: (mapMusic f m1)
mapMusic f (Modify c m) = Modify c $ mapMusic f m

fromMajorBluesM :: PitchClass -> Music MajorBluesPitch -> Music Pitch
fromMajorBluesM root = mapMusic (fromMajorBlues root)

playMajorBlues :: PitchClass -> Music MajorBluesPitch -> IO ()
playMajorBlues root = E.play . (fromMajorBluesM root)

majorBluesLick :: Octave -> Music MajorBluesPitch
majorBluesLick o =
  let
    measure =
        mbdo o en :+: mbdo o en :+: mbme o en :+: mbmi o en :+:
        mbso o en :+: mbso o en :+: mbla o en :+: mbso o en
  in
    measure :+: measure


transM :: AbsPitch -> Music Pitch -> Music Pitch
transM i = mapMusic (E.trans i)
