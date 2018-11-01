module Ch3Spec ( run ) where

import Ch3
import Test.Hspec
import Control.Exception (evaluate)

import EuterpeaExports
import qualified Euterpea as E


run :: IO ()
run = hspec $ do
  describe "transL" $ do
    it "should transpose correctly" $ do
      transL 3 [(C, 4), (D, 4)] `shouldBe` [(Ds, 4), (F, 4)]
  describe "restsL" $ do
    it "should create rests correctly" $ do
      restsL [qn, hn, en] `shouldBe` [E.qnr, E.hnr, E.enr]
  describe "staccatoLiteralPrim" $ do
    it "should just return a rest" $ do
      staccatoLiteralPrim (Rest qn) `shouldBe` (Prim (Rest qn) :: Music Pitch)
    it "should stacatto-literal-ize a Note" $ do
      staccatoLiteralPrim (Note qn 5) `shouldBe` Prim (Note en 5) :+: Prim (Rest en)
  describe "staccatoLiteralL" $ do
    it "should work like StacattoLiteralPrim on single primitives" $ do
      staccatoLiteralL [(Prim (Rest qn))] `shouldBe` [(Prim (Rest qn) :: Music Pitch)]
      staccatoLiteralL [(Prim (Note qn 5))] `shouldBe` [Prim (Note en 5) :+: Prim (Rest en)]
  describe "applyEach" $ do
    it "should work as expected" $ do
      applyEach [("wow" ++) . show, ("bang" ++) . show] '!' `shouldBe` ["wow'!'", "bang'!'"]
  describe "applyAll" $ do
    it "should work as expected" $ do
      applyAll [("wow! " ++), ("bang! " ++)] "pow!" `shouldBe` "wow! bang! pow!"
  describe "length'" $ do
    it "should work as expected" $ do
      length' [C, D, E] `shouldBe` 3
      length' [] `shouldBe` 0
      length' [1, 2, 3, 4, 5, 6] `shouldBe` 6
  describe "fuse" $ do
    it "should throw an error for unequal length args" $ do
      evaluate (fuse [qn] []) `shouldThrow`
        errorCall "Called `fuse` on lists with unequal lengths"
    it "should work on equal-sized lists" $ do
      fuse [qn, hn] [E.c 4, E.d 5] `shouldBe` [E.c 4 qn, E.d 5 hn]
  describe "maxAbsPitch" $ do
    it "should throw an error for empty arg" $ do
      evaluate (maxAbsPitch []) `shouldThrow` errorCall "Called `foldl1'` on empty list"
    it "should work for normal pitch lists" $ do
      maxAbsPitch [(C, 4), (G, 3)] `shouldBe` 60  -- (C, 4) is pitch 60
  describe "chrom" $ do
    it "should work for one-note chromatic runs" $ do
      chrom wn (G, 6) (G, 6) `shouldBe` [Prim (Note wn (G, 6))]
    it "should work for increasing chromatic runs" $ do
      chrom en (B, 5) (Df, 6) `shouldBe` [ Prim (Note en (B,  5))
                                         , Prim (Note en (C,  6))
                                         , Prim (Note en (Cs, 6))
                                         ]
    it "should work for decreasing chromatic runs" $ do
      chrom en (Df, 6) (B, 5) `shouldBe` [ Prim (Note en (Cs, 6))
                                         , Prim (Note en (C,  6))
                                         , Prim (Note en (B,  5))
                                         ]
  describe "mkScalePitches" $ do
    it "should work for a pentatonic scale"
      (let
          actual = mkScalePitches (C, 4) [2, 2, 3, 2]
          expected = [(C, 4), (D, 4), (E, 4), (G, 4), (A, 4)]
       in
         actual `shouldBe` expected)
  describe "mkScale" $ do
    it "should work for a trivial scale"
      (let
          actual = mkScale hn (C, 4) [7]
          expected = E.line [(E.c 4 hn), (E.g 4 hn)]
       in
         actual `shouldBe` expected)
  describe "intervalsForScaleType" $ do
    it "should respect rotations by definition" $
      intervalsForScaleType Phrygian `shouldBe` (rotate $ intervalsForScaleType Dorian)
    it "should be circular with respect to rotations" $
      intervalsForScaleType Ionian `shouldBe` (rotate $ intervalsForScaleType Locrian)
  describe "frogEncrypt and frogDecrypt" $ do
    it "should work on a simple example" $ do
      frogEncrypt "yes" `shouldBe` "zft"
      frogDecrypt "zft" `shouldBe` "yes"
    it "should be inverses of one another" $ do
      (frogDecrypt . frogEncrypt) "a big string" `shouldBe` "a big string"
      (frogEncrypt . frogDecrypt) "a big string" `shouldBe` "a big string"
