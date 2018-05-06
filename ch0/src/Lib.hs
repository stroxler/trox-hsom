module Lib
    ( someFunc
    ) where

import Euterpea

bluesLick =
  let repeated = ((g 4 en) :=: (bf 4 en))
   in
    ((c 4 qn) :=: ((ef 4 en) :+: (e 4 en))) :+: ((d 4 en) :=: (f 4 en)) :+:
    repeated :+: repeated :+: repeated :+:
    repeated :+: repeated :+: repeated :+:
    repeated :+: repeated :+: ((f 4 en) :=: (a 4 en)) :+:
    ((e 4 qn) :=: (g 4 qn))



someFunc :: IO ()
someFunc = play $ bluesLick
