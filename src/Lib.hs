{-# LANGUAGE TupleSections              #-}

module Lib where

import Euterpea
import Control.Applicative
import Control.Monad
import Data.List as L
import Data.Map as M 

-- Type alias representing a difference between two
-- consecutive abs pitches in a scale.
type AbsStep    = AbsPitch 

-- Type alias representing a difference of some abs pitch
-- from a base abs pitch.
type AbsOffset  = AbsPitch

-- Lists of abs-pitch (semitone) step sizes for the 
-- major and minor scale modes.
majAbsIntervals, minAbsIntervals :: [AbsStep]
majAbsIntervals = 
  [2, 2, 1, 2, 2, 2]
minAbsIntervals = 
  [2, 1, 2, 2, 1, 2]

-- Starting with just 2 modes for now.
data ScaleMode = MajorMode | MinorMode
 deriving (Eq, Show)

-- Given a scale mode, obtain its list of abs-pitch 
-- (semitone) step sizes.
getScaleStepSizes :: ScaleMode -> [AbsStep]
getScaleStepSizes mode = case mode of 
  MajorMode     -> majAbsIntervals
  MinorMode     -> minAbsIntervals

-- Given a list of abs-pitch (semitone) step sizes,
-- obtain its list of abs-pitch offsets for the 7-tone
-- scale built from some abs pitch.
stepsToOffsets :: [AbsStep] -> [AbsOffset]
stepsToOffsets = (scanl (+) 0)

-- Given a base abs pitch and a list of abs-pitch (semitone)
-- step sizes, build the 7-tone scale of abs pitches built
-- from that base abs pitch & with abs-pitch (semitone) step
-- sizes specified.
mkAbsScale :: AbsPitch 
           -> [AbsStep]
           -> [AbsPitch]
mkAbsScale ap steps = 
  let offsets = stepsToOffsets steps 
  in  (ap +) <$> offsets

data ScaleDegree = 
  S1 | S2 | S3 | S4 | S5 | S6 | S7 
   deriving (Eq, Show, Enum)

type OctaveEquiv  = Int 
type ScaleOctave  = Int 

-- PitchClassEquiv is a newtype on PitchClass, over which we will
-- define operations that create enharmonic equivalence. The newtype
-- is made to inherit the "lexicographic" equality / ordering of its
-- base type, because the Map construction that we leverage below
-- requires for distinct, enharmonically equivalent "spellings" of 
-- a pitchclass to be distinguishable.
newtype PitchClassEquiv = MkPCE {
  getPc :: PitchClass
  } deriving (Show, Eq, Ord)


-- The chromatic circle is obtained by classifying all PitchClasses
-- by enharmonic equivalence relation, into integers mod 12
toChromaticCircle :: PitchClassEquiv -> Int 
toChromaticCircle (MkPCE pc) = 
  (absPitch (pc, -1)) `mod` 12

-- Mapping from each pitch class to its position in the chromatic scale.
-- This will be useful later for generating the series of pitch classes 
-- that a scale founded on some tonic pitch class comprises.
chromaticCircleMap :: M.Map PitchClassEquiv Int 
chromaticCircleMap = M.fromList $ 
  zip allPcs (toChromaticCircle <$> allPcs)
  where allPcs = MkPCE <$> (enumFromTo minBound maxBound :: [PitchClass])

-- chromaticCircleMap is not injective, so this gives back
-- lists of PitchClasses per an int. modulo 12
fromZ12ToPitchClasses :: Int -> [PitchClassEquiv]
fromZ12ToPitchClasses z = 
  let submap = M.filter ((z `mod` 12) ==) chromaticCircleMap
  in  keys submap

-- API functions for working with the chromatic circle.
-- The modular arithmetic is concealed as implementation details.

-- Specify a number of semitones to advance along the chromatic circle.
-- All possible spellings of the resulting pitch class are returned.
chromPlus :: PitchClassEquiv -> Int -> [PitchClassEquiv]
chromPlus pce n = 
  fromZ12ToPitchClasses . (n +) . toChromaticCircle $ pce


-- Given a pitch class equiv, a number of semitones to advance
-- along the chromatic circle, and a number of scale degrees 
-- to advance along a 7-tone scale, returns the specific spelling
-- of the resulting pitch class which is raised from the starting
-- pitch class equiv by the specified number of semitones & scale
-- degrees, if such a spelling is possible.
chromPlusDiatonic :: PitchClassEquiv  -- starting pitch class
                  -> Int              -- semitone step size
                  -> Int              -- number of scale degrees 
                  -> Maybe PitchClassEquiv
chromPlusDiatonic pce dp ds = 
  let pcs = chromPlus pce dp 
      baseChars = ['A'..'G']
      baseChar = head $ show (getPc pce) 

      mBaseChar' :: Maybe Char
      mBaseChar' = liftA2  (!!)  -- :: Maybe Char
        (((pure L.drop) <*> L.elemIndex baseChar baseChars 
                        <*> pure (cycle baseChars)))
        (pure ds)

      mpc' :: [PitchClassEquiv]
      mpc' = L.filter ((mBaseChar' ==) . Just . head . show . getPc)
                      pcs
  in  case mpc' of 
        [pc]           -> Just pc 
        _              -> Nothing

-- Given a pitch class equiv to serve as the tonic & a scale mode, 
-- returns (if possible) the spellings for all 7 distinct pitch classes
-- that make up the resulting scale in the tonic's key & in the specified
-- mode. 
-- When writing tests, make sure that this works for PitchClassEquiv's
-- spelled with up to 1 sharp / flat. 
-- This should work for scales based on those tonics, but may not 
-- work (in fact, is not expected to) in general otherwise.
safeScaleDegreeNames :: PitchClassEquiv               -- a tonic name
                     -> ScaleMode                     -- a scale mode
                     -> Maybe [PitchClassEquiv]       -- the list of names per scale degree,
                                                      -- *if* possible to construct with the 
                                                      -- pitch class names provided by Euterpea
safeScaleDegreeNames pce mode = 
  let mpcs :: [Maybe PitchClassEquiv]
      mpcs = L.map (\(dp, ds) -> chromPlusDiatonic pce dp ds) $
                    zip (scanl (+) (fromIntegral 0) $ getScaleStepSizes mode)
                        [0..]  
  in  L.foldr (liftA2 (:))
              (pure [])
              mpcs

-- For convenience. This is unsafe.
getScaleDegreeNames :: PitchClassEquiv
                    -> ScaleMode
                    -> [PitchClassEquiv]
getScaleDegreeNames pce mode = case safeScaleDegreeNames pce mode of 
  Nothing           ->  error $ "pitch classes unavailable for \
                                \spelling the scale degrees of: "
                                ++ show (getPc pce) ++ " " ++ show mode
  Just pces         ->  pces       

{-
 In the "lexicographic" ordering specified for the Euterpea data type 
 PitchClass, the origin is at C, with PitchClasses ordered first by
 their base char (in the order C, D, E, F, G, A, B), then by the number
 of sharps (up) or flats (down). Ties are broken by placing alternative
 spellings of the "same" pitch class in the order specified by the base
 char, e.g.

  ...   | Ds | Ef | Fff ...

 come in this order, even though they all spell the same pitch class.

 By contrast, the idea of (chromaticCircleMap :: M.Map PitchClassEquiv Z12)
 is that every possible way to spell one of the 12 pitch classes (in the
 standard Western 12-tone temperament in use today) should be mapped to
 the pitch class which it spells, starting from "C" and ending at "B".

  The significance of this is that, in the Euterpea representation,
  the expr's (Cff, 0) and (As, 0) do NOT denote the same pitch,
  even though (conventionally) Cff / As are different spellings of 
  the same pitch class (i.e. enharmonically equivalent):

  > absPitch (Cff, 0)
  10
  > absPitch (As, 0)
  22

  Therefore we have the following
    GOAL: when converting
      Pitch ~ (PitchClass, Octave) -> (PitchClassEquiv, Octave),
    any two input pitches (p, p' :: Pitch), if they spell the same
    absolute pitch (i.e. absPitch p == absPitch p'), then their 
    outputs should indicate the *same*, not different, octaves.

  This is already the case for most pitch classes, but not for 
    Cf, Cff
  for which pitches built using them sound an octave lower than
  those built using their alternative spellings (B / Ass and Bf / As,
  resp.)

  In this way, we should have the following formula satisfied:
    forall (p :: Pitch) :
      pitchEquivalize p is a pair (pce, oct) :: (PitchClassEquiv, OctaveEquiv)
       s.t.:
        absPitch p = (12 * (1 + oct)) + toChromaticCircle pce
-}

pitchEquivalize :: Pitch -> (PitchClassEquiv, OctaveEquiv)
pitchEquivalize (pc, oct) 
  | pc `elem` [Cf, Cff]     = (MkPCE pc, oct - 1)
  | pc `elem` [Bs, Bss]     = (MkPCE pc, oct + 1)
  | otherwise               = (MkPCE pc, oct)

pitchEuterpize :: (PitchClassEquiv, OctaveEquiv) -> Pitch 
pitchEuterpize (MkPCE pc, oct')
  | pc `elem` [Cf, Cff]     = (pc, oct' + 1)
  | pc `elem` [Bs, Bss]     = (pc, oct' - 1)
  | otherwise               = (pc, oct')

-- For convenience, to get the number of semitones separating
-- a (PitchClassEquiv, OctaveEquiv) (equivalized pitch) from 
-- (C, -1), which lies at abs pitch of zero.
absPitchEq :: (PitchClassEquiv, OctaveEquiv) -> AbsPitch 
absPitchEq (pce, octe) = 
  let pitch_class_offset  = toChromaticCircle pce
      octave_offset       = 12 * (octe + 1)
  in  pitch_class_offset + octave_offset

-- Given a tonic pitch-class spelling and scale mode, produces 
-- the infinite list ('stream') of (PitchClassEquiv, OctaveEquiv) 
-- pairs, in order, spelling out the scale in this tonic key, and
-- starting from the lowest tonic instance with non-negative 
-- absolute pitch.
mkTonicScaleModeStream :: PitchClassEquiv 
                       -> ScaleMode 
                       -> [(PitchClassEquiv, OctaveEquiv)]
mkTonicScaleModeStream tonic mode = 
  let pairs         = [(pce, octe) | octe <- [(-1)..],
                                     pce <- L.sortOn toChromaticCircle 
                                                     (getScaleDegreeNames tonic mode)]
      lowest_tonic  = (tonic, (-1))
  in  L.dropWhile (\x -> absPitchEq x < absPitchEq lowest_tonic)
                  pairs 

-- Given an equivalence-class representation :: (PitchClassEquiv, OctaveEquiv),
-- and a list :: [(PitchClassEquiv, OctaveEquiv)],
-- return the index and value of the list element whose 
--  a) base char matches that of the input pitch
--  b) octave part matches that of the input pitch
getPCEBaseChar :: PitchClassEquiv -> Char 
getPCEBaseChar pce = head $ show $ getPc pce 

-- PROBLEM: Right now, this searches a potentially infinite list
-- and can therefore be potentially non-terminating.
-- This is what comes up when I attempt to run certain property
-- tests on toScaleDegreeRepr2, which uses this function.
getClosestIndexPitchEquiv :: (PitchClassEquiv, OctaveEquiv) 
                          -> [(PitchClassEquiv, OctaveEquiv)]
                          -> Maybe (Int, (PitchClassEquiv, OctaveEquiv))
getClosestIndexPitchEquiv (pce, octe) pairs = 
  let pred = (\x -> (getPCEBaseChar (fst x) == getPCEBaseChar pce) &&
                    (               (snd x) ==                octe))
  in  (pure (,)) <*> (L.findIndex pred $ pairs)
                 <*> (find pred $ pairs) 

data ScalePitch = ScalePitch {
  scaleOctave :: ScaleOctave,
  scaleDegree :: ScaleDegree, 
  accidentals :: Int, 
  scaleTonic  :: PitchClassEquiv,
  scaleMode   :: ScaleMode
  } 
    -- deriving (Eq, Show) 
    deriving Eq 

instance Show ScalePitch where 
  show (ScalePitch soct sdeg accs tonic mode) = 
    (case soct of 
      1     -> show soct ++ "st"
      2     -> show soct ++ "nd"
      3     -> show soct ++ "rd"
      _     -> show soct ++ "th") ++ " octave " ++ show sdeg ++ 
    (case signum accs of 
      (-1)  -> L.take (abs accs) $ repeat '-'
      0     -> ""
      1     -> L.take accs $ repeat '+')
        ++ " (" ++ show (getPc tonic) ++ " " ++ show mode ++ ")"

-- Based upon their spellings, figure out how many semitones
-- two PitchClasses with identical base chars differ by.
getAccidentals :: PitchClass -> PitchClass -> Int 
getAccidentals pc pc' = 
  let str  = show pc 
      str' = show pc'
      accInt :: String -> Int 
      accInt accs = case accs of 
        "ff"        -> -2
        "f"         -> -1
        ""          -> 0
        "s"         -> 1
        "ss"        -> 2 
  in  case (str, str') of 
        ((c : accs), (c' : accs'))  
           | c == c'     ->  accInt accs' - accInt accs 
           | otherwise   ->  error "pitch-class base chars don't match"
        _                ->  error "invalid pitch classes"


diatonic_match :: PitchClassEquiv
               -> ScaleMode 
               -> (PitchClassEquiv, OctaveEquiv)
               -> [PitchClassEquiv]
diatonic_match tonic mode peqv = 
  L.filter (\n -> (getPCEBaseChar n) == (getPCEBaseChar $ fst peqv)) 
           (getScaleDegreeNames tonic mode)

neighbors :: PitchClassEquiv
          -> ScaleMode 
          -> (PitchClassEquiv, OctaveEquiv) 
          -> [(PitchClassEquiv, OctaveEquiv)]
neighbors tonic mode peqv = 
  [ (d, oct) | d <- diatonic_match tonic mode peqv, 
               oct <- [(snd peqv - 1)..(snd peqv + 1)] ]

get_nearest_neighbor :: PitchClassEquiv
                     -> ScaleMode
                     -> (PitchClassEquiv, OctaveEquiv)
                     -> (PitchClassEquiv, OctaveEquiv)
get_nearest_neighbor tonic mode peqv = head $ 
  L.sortOn (\x -> abs $ (absPitchEq x) - (absPitchEq peqv)) 
           (neighbors tonic mode peqv)

peqv_sublist, peqv_sorted :: PitchClassEquiv 
                          -> ScaleMode 
                          -> (PitchClassEquiv, OctaveEquiv) 
                          -> [(PitchClassEquiv, OctaveEquiv)]
peqv_sublist tonic mode peqv@(p, o) 
 | o >= 0     = [(pce, octe) | octe <- [(-1)..(o + 1)], 
                               pce <- getScaleDegreeNames tonic mode]
 | otherwise  = [(pce, octe) | octe <- [(o - 1)..0], 
                               pce <- getScaleDegreeNames tonic mode]

peqv_sorted tonic mode peqv = 
  L.sortOn absPitchEq $ peqv_sublist tonic mode peqv

find_origin_vs_match :: Pitch 
                     -> PitchClassEquiv 
                     -> ScaleMode 
                     -> (Maybe Int, Maybe Int)
find_origin_vs_match p tonic mode = 
 let peqv      = pitchEquivalize p 
     match     = diatonic_match tonic mode peqv
     neighbor  = get_nearest_neighbor tonic mode peqv
     subscale  = peqv_sorted tonic mode peqv
     idx_o     = L.findIndex (== (tonic, -1)) subscale
     idx_nghbr = L.findIndex (== neighbor) subscale 
 in  (idx_o, idx_nghbr)


toScaleDegreeRepr :: Pitch 
                  -> PitchClassEquiv 
                  -> ScaleMode 
                  -> ScalePitch 
toScaleDegreeRepr p tonic mode = 
  case (find_origin_vs_match p tonic mode) of 
    (Nothing, _)          -> error "ERROR"
    (_, Nothing)          -> error "ERROR"
    (Just i_o, Just i_n)  -> 
              -- compute scale octave part 
              let di            = i_n - i_o
                  index_div     = di `div` 7 
                  octave_part   = index_div - 1 
              -- compute scale degree part 
                  index_mod     = di `mod` 7 
                  degree_part   = toEnum index_mod :: ScaleDegree
              -- compute accidentals (int) part
                  pc            = fst p
                  match         = head $ diatonic_match tonic mode (pitchEquivalize p) 
                  match_pc      = getPc match 
                  accids_part   = getAccidentals match_pc pc 
              -- insert tonic & mode 
              in  ScalePitch octave_part degree_part accids_part tonic mode

addAccidentals :: PitchClass -> Int -> PitchClass 
addAccidentals pc acc = 
  let base_pc = read $ L.take 1 $ show pc :: PitchClass 
      acc_0   = getAccidentals base_pc pc 
      acc'    = acc + acc_0 
  in  case acc' of 
    -2              -> read (show base_pc ++ "ff") :: PitchClass
    -1              -> read (show base_pc ++ "f") :: PitchClass
    0               -> base_pc 
    1               -> read (show base_pc ++ "s") :: PitchClass
    2               -> read (show base_pc ++ "ss") :: PitchClass
    _               -> error $ "No pitch class available for spelling: "
                               ++ show pc ++ " with adjustment by: " ++ show acc 
                               ++ " semitones"

-- In general, casting back from a scale-degree represented musical pitch
-- into Euterpea representation may be impossible!
-- Reason: the diatonic spelling of the scale degree which a certain musical
-- pitch has (w/r/t some ambient mode / tonic key), adjusted by the accidentals
-- specified by the data, may result in a pitch class spelling that doesn't
-- exist in Euterpea! (because it has too many sharps / flats.)
-- So instead we solve the simpler problem of casting back into an absolute
-- pitch, which we always can use 'pitch' on to cast back into a form that 
-- can be played aloud as MIDI.
-- This also has a bug, though (off-by-one error in the factor multiplying
-- 12, for the octave part of abs pitch sum.)
toAbsPitchRepr :: ScalePitch -> AbsPitch 
toAbsPitchRepr (ScalePitch oct degree accs tonic mode) =
  -- octave part of the absolute pitch sum
  (12 * (oct + 1)) +
  -- scale degree part of the absolute pitch sum
  ((stepsToOffsets $ getScaleStepSizes mode) !! (fromEnum degree)) +
  -- accidentals part of the absolute pitch sum
  accs + 
  -- tonic key part of the absolute pitch sum
  (toChromaticCircle tonic)

-- A convenience function for casting ScalePitch'es back into a playable
-- form without concern for the resulting pitch-class spelling.
pitchify :: ScalePitch -> Pitch 
pitchify = pitch . toAbsPitchRepr

