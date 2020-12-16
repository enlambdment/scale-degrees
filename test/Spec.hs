module Spec where 

import Lib 
import Euterpea
import qualified Data.List as L 
import qualified Data.Map as M 
import qualified Data.Maybe as MB 
import Control.Applicative
import Test.QuickCheck

instance Arbitrary ScaleMode where 
  arbitrary = elements [MajorMode, MinorMode]

instance Arbitrary PitchClass where 
  arbitrary = elements [Cff .. Bss]

instance Arbitrary PitchClassEquiv where 
  arbitrary = MkPCE <$> arbitrary

instance Arbitrary ScalePitch where 
  arbitrary = 
    (pure ScalePitch) <*> (elements [2 .. 6])
                      <*> (elements [S1 .. S7])
                      <*> (elements [(-2) .. 2])
                      <*> genTypicalKeySig
                      <*> arbitrary

-- Conventionally, only pitch class spellings containing up to one sharp, 
-- or one flat, are typically used as tonics for a key (a.k.a. as key signatures.)
typicalKeySigs :: [PitchClass]
typicalKeySigs = [C,  D,  E,  F,  G,  A,
                  Cf, Df, Ef, Ff, Gf, Af, 
                  Cs, Ds, Es, Fs, Gs, As]

genTypicalKeySig :: Gen PitchClassEquiv
genTypicalKeySig = MkPCE <$> 
  elements typicalKeySigs

genPitch :: Gen Pitch 
genPitch = 
  (pure (,)) <*> arbitrary
             <*> (elements [0..10])

chkIdentity :: (PitchClassEquiv, OctaveEquiv) -> Bool 
chkIdentity q = 
  (pitchEquivalize $ pitchEuterpize q) == q

chkIdentity' :: Pitch -> Bool
chkIdentity' p = 
  (pitchEuterpize $ pitchEquivalize p) == p

-- Checks that for any two pitch class spellings that are supposed to be 
-- enharmonically equivalent, the pitch that you get when pairing both with
-- some choice of octave & then sending back to Euterpea representation is
-- *the same* (exactly the same, not just similar up to some multiple of 12
-- the number of semitones in the octave.)
-- This validates that in any (pitchEquivalize p :: (PitchClassEquiv, OctaveEquiv)),
-- the octave quantity refers to an exact range of absolute pitches in which the tone
-- lies, and does not have different meaning depending on which spelling of the
-- pitch class it is paired with.
chkEnharmonicEquivalence :: PitchClassEquiv
                         -> PitchClassEquiv
                         -> OctaveEquiv
                         -> Property 
chkEnharmonicEquivalence pce pce' octe =
  (M.lookup pce chromaticCircleMap) == (M.lookup pce' chromaticCircleMap) ==>
    (absPitch $ pitchEuterpize (pce, octe)) == 
      (absPitch $ pitchEuterpize (pce', octe))

-- Checks that for any pitch, its "equivalized" representation is a pair
-- of pitch class spelling and octave such that the absolute pitch of the tone
-- bears an exact mathematical relationship with the "equivalized" octave 
-- and the "equivalized" pitch class spelling's modulo-12 integer induced by
-- mapping to the chromatic circle.
chkEquivReprAbsPitch  :: Pitch 
                      -> Bool 
chkEquivReprAbsPitch p = 
  let p'@(pce, octe) = pitchEquivalize p 
  in  toChromaticCircle pce + 
      (12 * (octe + 1)) == absPitch p

-- Double checking the same property as 'chkEquivReprAbsPitch', 
-- but with respect to the convenience function absPitchEq
chkEquivReprAbsPitch2 :: Pitch 
                      -> Bool 
chkEquivReprAbsPitch2 p = 
  let p'  = pitchEquivalize p 
      ap  = absPitch p 
      ap' = absPitchEq p'
  in  ap == ap'

-- Checks that for either of the 2 scale modes currently supported, and  
-- for any of the pitch class spellings typically used to provide the tonic
-- of a key signature, the result of the safe function 'safeScaleDegreeNames'
-- is not Nothing, and contains a list of 7 pitch classes (i.e. the spellings for
-- diatonic pitch classes belonging to the scale.)

chkScaleDegreeNames :: ScaleMode 
                    -> Property 
chkScaleDegreeNames sm =
  forAll genTypicalKeySig
    (\x ->
      let names = safeScaleDegreeNames x sm 
      in  (names /= Nothing) &&
          ((length <$> names) == Just 7))

-- Checks that for any choice of typical tonic pitch class spelling (pce) and choice of
-- mode (sm), the function (\p -> toScaleDegreeRepr p pce sm) takes the lowest lying
-- instance of the tonic pitch class with non-negative abs pitch i.e. the pitch due to 
-- 'pitchEuterpize (pce, (-1))' to 'ScalePitch (-1) S1 0 pce sm', i.e. it should be 
-- reported as the 1st scale degree of octave #(-1) with 0 accidentals, for the tonic
-- and scale mode in question.

chkScaleDegreeReprLowestTonic :: ScaleMode 
                              -> Property 
chkScaleDegreeReprLowestTonic sm = 
  forAll genTypicalKeySig
    (\x -> 
      let lowest_tonic = pitchEuterpize (x, (-1))
      in  (ScalePitch (-1) S1 0 x sm) == 
            toScaleDegreeRepr lowest_tonic x sm)

-- 
chkMkStreamThreeOctaves :: ScaleMode 
                        -> Property 
chkMkStreamThreeOctaves sm = 
  forAll genTypicalKeySig
    (\tonic -> 
      let tonicStream     = mkTonicScaleModeStream tonic sm 
          tonicSubstream  = take 21 tonicStream
          tonicInt        = toChromaticCircle tonic
          stepSizes       = getScaleStepSizes sm 
          offsets         = (scanl (+) 0 $ stepSizes) ++ 
                            (scanl (+) 12 $ stepSizes) ++
                            (scanl (+) 24 $ stepSizes)
          aps             = absPitchEq <$> tonicSubstream
          aps'            = (tonicInt +) <$> offsets 
      in  aps == aps')

chkScaleDegreeRepr  :: ScaleMode 
                    -> Property 
chkScaleDegreeRepr sm =
  forAll (liftA2 (,) genPitch genTypicalKeySig)
    (\(p, tonic) ->
      let sp = toScaleDegreeRepr p tonic sm 
      in  toAbsPitchRepr sp == absPitch p)

main :: IO ()
main = do
  quickCheck chkIdentity
  quickCheck chkIdentity'
  quickCheck chkEnharmonicEquivalence
  quickCheck chkScaleDegreeNames
  quickCheck chkEquivReprAbsPitch
  quickCheck chkEquivReprAbsPitch2
  quickCheck chkScaleDegreeReprLowestTonic
  quickCheck chkMkStreamThreeOctaves
  quickCheck chkScaleDegreeRepr


