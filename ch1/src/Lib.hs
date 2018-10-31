{-# LANGUAGE NamedFieldPuns #-}
module Lib
    ( Command (PlayNote, PlayLick)
    , executeCommand
    , hNote
    , hLine
    ) where

import Euterpea
import System.Exit (exitWith, ExitCode (ExitFailure))


data Command =
    PlayNote { noteName :: String
             , octave :: Int }
  | PlayLick { extended :: Bool }
  deriving (Show)

type Note = Octave -> Dur -> Music Pitch

executeCommand :: Command -> IO ()
executeCommand (PlayNote { noteName, octave}) =
  let
    noteOrErr = do
      noteClass <- stringToNote noteName
      return $ noteClass octave qn
  in
    case noteOrErr of
      (Left oops) -> do
        putStrLn $ oops
        exitWith $ ExitFailure 1
      (Right theNote) ->
        play $ theNote
executeCommand (PlayLick { extended }) =
  play $ bluesLick extended


bluesLick :: Bool -> Music Pitch
bluesLick False =
  (c 4 qn) :+: (df 4 en) :+: (d 4 en) :+: (f 4 en) :+: (fs 4 en) :+:
  (g 4 en) :+: ((c 4 qn) :=: (e 4 qn) :=: (bf 4 qn))
bluesLick True =
  let repeated = ((g 4 en) :=: (bf 4 en))
   in
    ((c 4 qn) :=: ((ef 4 en) :+: (e 4 en))) :+: ((d 4 en) :=: (f 4 en)) :+:
    repeated :+: repeated :+: repeated :+:
    repeated :+: repeated :+: repeated :+:
    repeated :+: repeated :+: ((f 4 en) :=: (a 4 en)) :+:
    ((e 4 qn) :=: (g 4 qn))



stringToNote :: String -> Either String  Note
stringToNote stringNote = case stringNote of
  "af" -> Right af
  "a"  -> Right a
  "as" -> Right as
  "bf" -> Right bf
  "b"  -> Right b
  "bs" -> Right bs
  "cf" -> Right cf
  "c"  -> Right c
  "cs" -> Right cs
  "df" -> Right df
  "d"  -> Right d
  "ds" -> Right ds
  "ef" -> Right ef
  "e"  -> Right e
  "es" -> Right es
  "ff" -> Right ff
  "f"  -> Right f
  "fs" -> Right fs
  "gf" -> Right gf
  "g"  -> Right g
  "gs" -> Right gs
  _ -> Left $ "Did not understand the note " ++ show stringNote


-- **************************************************** --
-- ** Above was stuff for the CLI. Below is getting  ** --
-- ** to know some of the Euterpea internals         ** --
-- **************************************************** --


-- play :: (ToMusic1 a, NfData a) => Music a -> IO ()
-- ... don't worry about the NFData bit for now, that's
--     internal to the MIDI stuff. When you're ready to dive
--     in, the code is in Euterpea/IO/MIDI/Play.lhs

-- class ToMusic1 a where
--   toMusic1 :: Music a -> Music1
--     (there are instances for Pitch, AbsPitch, (Pitch, Volume),
--      and (AbsPitch, Volume))
--
-- type AbsPitch = Int
-- type Octave = Int
-- type Volume = Int
-- type Dur = Rational    (1 = 4 quarter notes)
-- type Pitch = (PitchClass, Octave)
-- PitchClasses are, e.g., Cff, Cf, C, Cs, Css, Dff, etc.
--   .. they are ordered from Cff to Bss. Ordering is by pitch,
--      and ties are broken by alphabetical order
--
-- type Music1 = Music Note1
-- type Note1 = (Pitch, [NoteAttribute])
--    ... here NoteAttribute are things like Volume, Fingering, etc
--    ... Note1 takes the place of Pitch/AbsPitch when we use
--        the toMusic1 function
--
-- data Primitive = Note Dur a | Rest Dur
-- data Music a = Prim (Primitive a)
--              | (Music a) :+: (Music a)
--              | (Music a) :=: (Music a)
--              | Modify Control (Music a)
--   ... here :+: and :=: are right-associative
--   ... Control is a variant with a bunch of types of modifications,
--       e.g. Tempo Rational, Transpose AbsPitch,
--            Instrument InstrumentName, Custom String
--       ... the Custom lets us define new modifications. I'm guessing
--           later chapters might cover this
--   ... InstrumentName is a big variant of standard instruments; there's
--       also a CustomInstrument String constructor that lets us
--       create new ones
--
-- Rational is a specific instance of Ratio; both are defined in
-- GHC.Real. The operator to construct one is % (which is *not* modulus,
-- integer division and modulus are done with the `div`, `mod`, and `rem`
-- functions)
--
-- The duration builtins are all just Rational definitions, given the
-- type annotation of Dur. Eg. `qn = 1 % 4`, `dqn = 3 % 8`
--
-- The rest builtins are just `Prim (Rest d)` wrapped around various
-- rests, e.g. `qnr = Prim (Rest (1 % 4))`. Note that rests are
-- polymorphic over all `a` in `Music a`.
--
-- The lower-case notes, e.g. c, ef, f, are all convenience functions
-- with type
--   Octave -> Dur -> Music Pitch
-- e.g. `c o d = Prim ( Note (d p)) where p = (C, o)
--
-- Some common functions:
-- line  :: [Music a] -> Music a
-- chord :: [Music a] -> Music a
-- trans :: Int -> Pitch -> Pitch


-- hNote: harmonize a pitch with a specific duration and interval
hNote :: Int -> Dur -> Pitch -> Music Pitch
hNote interval dd pp =
  let
    pp' = trans interval pp
    mkMusic p = Prim (Note dd p)
  in
    mkMusic pp :=: mkMusic pp'

-- hLine: harmonize a list of pitches with fixed duration and
--        interval. The pitches are played in sequence.
hLine :: Int -> Dur -> [Pitch] -> Music Pitch
hLine interval dd =
  let
    make_line = foldr (:+:) (Prim $ Rest 0)
    make_harmony = fmap (hNote interval dd)
  in
    make_line . make_harmony
