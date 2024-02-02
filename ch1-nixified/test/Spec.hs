import Lib

import GHC.Real
import Test.Hspec

import Euterpea (
  qn, en, rest,
  PitchClass (C, E, G, B),
  Music (Prim, (:=:), (:+:)),
  Primitive (Note, Rest),
  )


main :: IO ()
main = hspec $ do
  describe "hNote" $ do
    it "Should work for a positive interval" $ do
      hNote 4 qn (C, 4) `shouldBe`
        (Prim $ Note qn (C, 4)) :=: (Prim $ Note qn (E, 4))
    it "Should work for a negative interval" $ do
      hNote (-5) en (C, 4) `shouldBe`
        (Prim $ Note en (C, 4)) :=: (Prim $ Note en (G, 3))
  describe "hLote" $ do
    it "Should work for a single note" $ do
      hLine 4 en [(C, 4)] `shouldBe`
        ((Prim $ Note en (C, 4)) :=: (Prim $ Note en (E, 4))) :+:
        rest 0
    it "Should work for a two notes" $ do
      hLine 4 qn [(C, 4), (G, 4)] `shouldBe`
        ((Prim $ Note qn (C, 4)) :=: (Prim $ Note qn (E, 4))) :+:
        ((Prim $ Note qn (G, 4)) :=: (Prim $ Note qn (B, 4))) :+:
        rest 0
