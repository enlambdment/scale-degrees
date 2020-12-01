module Spec where 

import Lib 
import Euterpea
import qualified Data.List as L 
import qualified Data.Map as M 
import qualified Data.Maybe as MB 
import qualified Math.NumberTheory.Moduli.Class as NT 
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
  in  (fromIntegral $ NT.getVal $ toChromaticCircle pce :: Int) + 
      (12 * (octe + 1)) == absPitch p

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

-- For a given Euterpea pitch & scale mode, obtain the list of seven diatonic pitches which 
-- spell out the scale starting at that pitch & in that mode.
spellModeOnPitch :: Pitch -> ScaleMode -> [Pitch]
spellModeOnPitch p@(pc, oct) mode = 
        -- get the seven diatonic pitch class spellings
  let   diatonic_pcs    = getPc <$> getScaleDegreeNames (MkPCE pc) mode
        -- get absolute pitches making up the scale
        abs_p           = absPitch p
        octave_aps      = (abs_p +) <$> (stepsToOffsets $ getScaleStepSizes mode)
        -- get all diatonic pitches up to one octave away from (p :: Pitch)
        octs_pitches    = [ (p, o) | p <- diatonic_pcs, o <- [ oct - 1 .. oct + 1 ] ]
        -- get a sublist of just the diatonic pitches whose absolute pitch is in octave_aps
        spelled_octave  = L.filter (\p -> absPitch p `elem` octave_aps) octs_pitches
        spelled_oct_ord = L.sortOn absPitch spelled_octave
  in    spelled_oct_ord

main :: IO ()
main = do
  quickCheck chkIdentity
  quickCheck chkIdentity'
  quickCheck chkEnharmonicEquivalence
  quickCheck chkScaleDegreeNames
  quickCheck chkEquivReprAbsPitch

