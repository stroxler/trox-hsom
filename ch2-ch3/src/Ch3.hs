{-# LANGUAGE NegativeLiterals #-}
module Ch3 where

import Ch2 (flatMapMusicPrims)

import Data.Foldable (foldl') -- it's not in prelude

import EuterpeaExports
import qualified Euterpea as E


transL :: Int -> [Pitch] -> [Pitch]
transL i = map (E.trans i)

restsL :: [Dur] -> [Music a]
restsL = map (Prim . Rest)


staccatoLiteralPrim :: Primitive a -> Music a
staccatoLiteralPrim r@(Rest _) = Prim r
staccatoLiteralPrim (Note d aa) = Prim (Note (d/2) aa) :+: Prim ( Rest (d/2))

staccatoLiteral :: Music a -> Music a
staccatoLiteral = flatMapMusicPrims staccatoLiteralPrim

staccatoLiteralL :: [Music a] -> [Music a]
staccatoLiteralL = map staccatoLiteral

applyEach :: [a -> b] -> a -> [b]
applyEach fs x = map ($x) fs

applyAll :: [a -> a] -> a -> a
applyAll fs x = foldr ($) x fs

length' :: [a] -> Integer
length' xs = foldl' plusOne 0 xs
  where plusOne n _ = n + 1

fuse :: [Dur] -> [Dur -> Music a] -> [Music a]
fuse [] [] = []
fuse (d:ds) (f:fs) = (f d):(fuse ds fs)
fuse _ _ = error "Called `fuse` on lists with unequal lengths"


foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' _ [] = error "Called `foldl1'` on empty list"
foldl1' f (x:xs) = foldl' f x xs

maxAbsPitch :: [Pitch] -> AbsPitch
maxAbsPitch = (foldl1' max) . (map E.absPitch)


chrom :: Dur -> Pitch -> Pitch -> [Music Pitch]
chrom d p0 p1 =
  let ap0 = E.absPitch p0
      ap1 = E.absPitch p1
      step = if ap0 <= ap1 then 1 else -1
      go ap aps =
        if ap == ap1
        then ap:aps
        else go (ap + step) (ap:aps)
  in
    map ((Prim . Note d) . E.pitch) $ reverse $ go ap0 []


mkScalePitches :: Pitch -> [Int] -> [Pitch]
mkScalePitches root intervals =
  let ap0 = E.absPitch root
      go _ [] aps = aps
      go ap (i:is_) aps =
        let next = ap + i in
        go next is_ (next:aps)
  in
    map E.pitch $ reverse $ go ap0 intervals [ap0]


mkScale :: Dur -> Pitch -> [Int] -> Music Pitch
mkScale d root intervals =
  E.line $ map (Prim . Note d) $ mkScalePitches root intervals


rotate :: [a] -> [a]
rotate [] = []
rotate (x:xs) = xs ++ [x]

data ScaleType =
  Ionian | Dorian | Phrygian |
  Lydian | Mixolydian | Aeolian | Locrian


_ios, _dos, _phs, _lys, _mis, _aes, _los :: [Int]
_ios = [2, 2, 1, 2, 2, 2, 1]
_dos = rotate _ios
_phs = rotate _dos
_lys = rotate _phs
_mis = rotate _lys
_aes = rotate _mis
_los = rotate _aes

intervalsForScaleType :: ScaleType -> [Int]
intervalsForScaleType Ionian     = _ios
intervalsForScaleType Dorian     = _dos
intervalsForScaleType Phrygian   = _phs
intervalsForScaleType Lydian     = _lys
intervalsForScaleType Mixolydian = _mis
intervalsForScaleType Aeolian    = _aes
intervalsForScaleType Locrian    = _los


playMode :: Pitch -> ScaleType -> IO ()
playMode root st = E.play $ mkScale en root $ intervalsForScaleType st


frogEncrypt :: String -> String
frogEncrypt = map (toEnum . (+  1) . fromEnum)

frogDecrypt :: String -> String
frogDecrypt = map (toEnum . (+ -1) . fromEnum)


areYouSleeping :: Music Pitch
areYouSleeping =
  let
    lineA = map (\pc -> E.note qn (pc, 4)) [C, D, E, C]
    lineB = map (\(pc, d) -> E.note d (pc, 4)) [(E, qn), (F, qn), (G, hn)]
    lineC =
      map (\pc -> E.note en (pc, 4)) [G, A, G, F] ++
      map (\pc -> E.note qn (pc, 4)) [E, C]
    lineD = [E.note qn (C, 4), E.note qn (G, 3), E.note hn (C, 4)]
  in
    E.line $ map E.line $ [lineA, lineA, lineB, lineB,
                           lineC, lineC, lineD, lineD]


areYouSleepingRound :: Music Pitch
areYouSleepingRound =
  let
    voiceA = Modify (E.Instrument E.Fiddle) areYouSleeping
    voiceB = E.rest 2 :+: Modify (E.Instrument E.AcousticGuitarNylon) areYouSleeping
    voiceC = E.rest 4 :+: Modify (E.Instrument E.Viola) areYouSleeping
    voiceD = E.rest 6 :+: Modify (E.Instrument E.Banjo) areYouSleeping
  in voiceA :=: voiceB :=: voiceC :=: voiceD
