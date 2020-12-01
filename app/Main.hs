module Main where

import Lib
import Euterpea

{-
  We will begin expressing the basic concepts in which the principles / rules of 
  counterpoint in the various species are expressed.

  The following concepts are foundational (here, 'scalar' will be used to mean
  'in terms of scale degrees', to distinguish vs. 'in terms of chromatic semitones';
  thus e.g the scalar size between 
    p1 = ScalePitch 3 S4 0 C Minor 
    p2 = ScalePitch 3 S6 0 C Minor
  is 2, even though 3 semitones separate their absolute pitches.)

  a) scalar interval size
  b) scalar interval class (like classifying interval size into one of seven classes)
  c) scalar interval quality (i.e. perfect, diminished, augmented; + major or minor, for 
                                    those pitch classes where applicable)
  d) quality of motion, for a given site with at most 4 pitches & 2 per voice
-}

data IntervalClass = I1 | I2 | I3 | I4 | I5 | I6 | I7 
  deriving (Eq, Show, Enum)

-- Given 2 (ScaleDegree, ScaleOctave) pairs, calculate the absolute scalar interval 
-- between both, as well as the number of octaves separating them (result tuple
-- has 2nd element '0' for simple intervals, '1' for octaves & compound intervals)
scaleDegreeDivMod :: (ScaleDegree, ScaleOctave) 
                  -> (ScaleDegree, ScaleOctave) 
                  -> (Int, Int) 
scaleDegreeDivMod sd_oct sd_oct' = 
  let scaleInt :: (ScaleDegree, Octave) -> Int 
      scaleInt (s, o) = fromEnum s + 7 * o 
  in  (scaleInt sd_oct' - scaleInt sd_oct) `divMod` 7 

getIntervalClass  :: (ScaleDegree, ScaleOctave) 
                  -> (ScaleDegree, ScaleOctave) 
                  -> IntervalClass
getIntervalClass sd_oct sd_oct' = toEnum $ snd (scaleDegreeDivMod sd_oct sd_oct')

getOctaveDiff :: (ScaleDegree, ScaleOctave) 
              -> (ScaleDegree, ScaleOctave) 
              -> Int
getOctaveDiff sd_oct sd_oct' = fst (scaleDegreeDivMod sd_oct sd_oct')

-- This is the quickest way, but feels clumsy.
data IntervalQuality = 
  Dim1 | Prf1        | Aug1 | 
  Dim2 | Min2 | Maj2 | Aug2 | 
  Dim3 | Min3 | Maj3 | Aug3 | 
  Dim4 | Prf4        | Aug4 |
  Dim5 | Prf5        | Aug5 |
  Dim6 | Min6 | Maj6 | Aug6 | 
  Dim7 | Min7 | Maj7 | Aug7 
    deriving (Eq, Show)

-- -- For this part, it's easiest just to leverage the entire
-- -- complement of information that a ScalePitch term carries with it

-- getIntervalQuality  :: (ScaleDegree, Octave, Int) 
--                     -> (ScaleDegree, Octave, Int) 
--                     -> IntervalQuality 
-- -- get interval class between both inputs, which is 
-- -- just a function of the scale degrees & octaves
-- getIntervalQuality (sd, oct, acc) (sd', oct', acc') = 
--   let intervalClass = getIntervalClass (sd, oct) (sd, oct')
-- -- then, based upon interval class, use the ??? as well
-- -- as the accidentals sizes to work out interval quality
--   in  case intervalClass of 

-- This only gives the correct answer if both ScalePitch'es have
-- the same tonic, but not otherwise. 
-- Reason: the scale degrees are defined with respect to the tonic / mode.
getIntervalQuality :: ScalePitch -> ScalePitch -> IntervalQuality
getIntervalQuality  
  sp@(ScalePitch oct degree accs tonic mode) sp'@(ScalePitch oct' degree' accs' tonic' mode') = 
  if (tonic, mode) /= (tonic', mode')
    then 
      error $ "1st scale pitch is given for key of: " 
                ++ (show $ getPc tonic) ++ " " ++ show mode 
              ++ "but 2nd scale pitch is given for key of: " 
                ++ (show $ getPc tonic') ++ " " ++ show mode'
    else 
      let intervalClass     = getIntervalClass (degree, oct) (degree', oct')
          absPitchMod12     = (toAbsPitchRepr sp' - toAbsPitchRepr sp) `mod` 12
          -- accsDelta         = accs' - accs 
      in  case intervalClass of 
            I1          | absPitchMod12 == (-1)   -> Dim1
                        | absPitchMod12 == 0      -> Prf1
                        | absPitchMod12 == 1      -> Aug1
                        | otherwise               -> error $ "no such interval quality for: " ++ show intervalClass

            I2          | absPitchMod12 == 0      -> Dim2
                        | absPitchMod12 == 1      -> Min2 
                        | absPitchMod12 == 2      -> Maj2
                        | absPitchMod12 == 3      -> Aug2
                        | otherwise               -> error $ "no such interval quality for: " ++ show intervalClass

            I3          | absPitchMod12 == 2      -> Dim3 
                        | absPitchMod12 == 3      -> Min3
                        | absPitchMod12 == 4      -> Maj3
                        | absPitchMod12 == 5      -> Aug3
                        | otherwise               -> error $ "no such interval quality for: " ++ show intervalClass

            I4          | absPitchMod12 == 4      -> Dim4 
                        | absPitchMod12 == 5      -> Prf4
                        | absPitchMod12 == 6      -> Aug4 
                        | otherwise               -> error $ "no such interval quality for: " ++ show intervalClass

            I5          | absPitchMod12 == 6      -> Dim5 
                        | absPitchMod12 == 7      -> Prf5
                        | absPitchMod12 == 8      -> Aug5
                        | otherwise               -> error $ "no such interval quality for: " ++ show intervalClass

            I6          | absPitchMod12 == 7      -> Dim6
                        | absPitchMod12 == 8      -> Min6
                        | absPitchMod12 == 9      -> Maj6
                        | absPitchMod12 == 10     -> Aug6
                        | otherwise               -> error $ "no such interval quality for: " ++ show intervalClass

            I7          | absPitchMod12 == 9      -> Dim7
                        | absPitchMod12 == 10     -> Min7
                        | absPitchMod12 == 11     -> Maj7
                        | absPitchMod12 == 12     -> Aug7
                        | otherwise               -> error $ "no such interval quality for: " ++ show intervalClass

-- Convenience function to map a series of ScaleDegree / Octave / accidental triples
-- and an ambient tonic / mode into a "cantus firmus"
mkCantusFirmus  :: [(Octave, ScaleDegree, Int)]
                -> (PitchClassEquiv, ScaleMode) 
                -> [ScalePitch]
mkCantusFirmus triples (pce, sm) =
  let triple_to_sp :: (Octave, ScaleDegree, Int) -> ScalePitch
      triple_to_sp (octave, degree, accs) = ScalePitch octave degree accs pce sm 
  in  triple_to_sp <$> triples

cf1 :: [ScalePitch]
cf1 = mkCantusFirmus 
  [ (3, S1, 0), (3, S5, 0), (3, S4, 0), (3, S5, 0), 
    (3, S6, 0), (3, S7, 0), (4, S1, 0), (3, S4, 0), 
    (3, S5, 0), (3, S4, 0), (3, S3, 0), (3, S2, 0), (3, S1, 0)] 
  (MkPCE E, MinorMode)

lineCFWithDuration :: [ScalePitch] -> Dur -> Music Pitch 
lineCFWithDuration sps drn = line $ (Prim . Note drn . pitch . toAbsPitchRepr) <$> sps

play_cf1 :: IO ()
play_cf1 = play $ line $ ((Prim . Note hn . pitch . toAbsPitchRepr) <$> cf1)

main :: IO ()
main = do
  play $ c 4 qn