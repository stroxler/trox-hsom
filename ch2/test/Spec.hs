import Ch2

import GHC.Real
import Test.Hspec

import EuterpeaExports
import qualified Euterpea as E


main :: IO ()
main = hspec $ do
  describe "twoFiveOne" $
    let
      c = E.c; d = E.d; e = E.e; f = E.f
      g = E.g; a = E.a; b = E.b; as = E.as
    in do
      it "Should work for C" $ do
        twoFiveOne (C, 4) qn `shouldBe`
          E.line [ E.chord [d 4 qn, f 4 qn, a 4 qn]
                 , E.chord [g 3 qn, b 3 qn, d 4 qn]
                 , E.chord [c 4 qn, e 4 qn, g 4 qn]
                 ]
      it "Should work for F" $ do
        twoFiveOne (F, 5) qn `shouldBe`
          E.line [ E.chord [g 5 qn, as 5 qn, d 6 qn]
                 , E.chord [c 5 qn, e  5 qn, g 5 qn]
                 , E.chord [f 5 qn, a  5 qn, c 6 qn]
                 ]
  describe "fromMajorBlues" $ do
    it "Should work in E" $ do
      fromMajorBlues E (MbDo, 4) `shouldBe` (E, 4)
      fromMajorBlues E (MbMi, 4) `shouldBe` (Gs, 4)
      fromMajorBlues E (MbLa, 4) `shouldBe` (Cs, 5)
    it "Should work in A" $ do
      fromMajorBlues A (MbDo, 3) `shouldBe` (A,  3)
      fromMajorBlues A (MbMi, 3) `shouldBe` (Cs, 4)
      fromMajorBlues A (MbLa, 3) `shouldBe` (Fs, 4)
  describe "mapMusic" $ do
    it "Should map over complex objects" $ do
      mapMusic (+8) (
        Modify (E.Transpose 4) $
          Prim (Note qn 5) :+: Prim (Rest qn) :=:
          Prim (Note qn 6)
        ) `shouldBe` (
        Modify (E.Transpose 4) $
          Prim (Note qn 13) :+: Prim (Rest qn) :=:
          Prim (Note qn 14)
        )
  describe "fromMajorBluesM" $ do
    it "should work" $ do
      fromMajorBluesM E (mbdo 4 qn) `shouldBe` Prim (Note qn (E, 4))
  describe "transM" $ do
    it "should transpose correctly" $ do
      transM 10 (Prim $ Note qn (G, 3)) `shouldBe` (Prim $ Note qn (F, 4))
