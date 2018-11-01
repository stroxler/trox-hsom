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


-- Note the implication here that Music has a monadic-type structure
-- (but without the parametric type) over Primitive: we can flat map
-- Primitives into Musics.
--
-- It would be *possible* to make a full monad by replacing
-- the Music a <-> Primitive a relationship with a Music prim <-> prim
-- relationship. It probably wouldn't be super useful though, since
-- flatMapMusicPrims seems to give everything I can imagine wanting.
--
-- The only benefit would be `do` notation, but I don't think that
-- would be helpful in most cases.
flatMapMusicPrims :: (Primitive a -> Music b) -> Music a -> Music b
flatMapMusicPrims f (Prim x) = f x
flatMapMusicPrims f (m0 :+: m1) = (flatMapMusicPrims f m0) :+: (flatMapMusicPrims f m1)
flatMapMusicPrims f (m0 :=: m1) = (flatMapMusicPrims f m0) :=: (flatMapMusicPrims f m1)
flatMapMusicPrims f (Modify c m) = Modify c $ flatMapMusicPrims f m

mapMusicPrims :: (Primitive a -> Primitive b) -> Music a -> Music b
mapMusicPrims f = flatMapMusicPrims (Prim . f)

mapPrimitive :: (a -> b) -> Primitive a -> Primitive b
mapPrimitive _ (Rest d) = Rest d   -- Note: Haskell won't let us reuse the Rest :(
mapPrimitive f (Note d p) = Note d (f p)

mapMusic :: (a -> b) -> Music a -> Music b
mapMusic f = mapMusicPrims (mapPrimitive f)


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
