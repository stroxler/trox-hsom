{-# LANGUAGE NamedFieldPuns #-}
module Lib
    ( Command (PlayNote, PlayLick)
    , executeCommand
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
