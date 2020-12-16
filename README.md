# scale-degrees

## Goal 

Extend the `PitchClass` / `Pitch` / etc. data types available via the Haskell library Euterpea with 
additional functionality for calculating, given a pitch and choice of tonic / scale mode, which 
scale degree of the scale the pitch is (including any accidentals, if applicable.)

## Examples 


### Intermediate newtype `PitchClassEquiv` and type alias `OctaveEquiv`

```
> c0 = (C, 0) :: Pitch
> b_1 = (Bs, -1) :: Pitch
> absPitch c0
12
> absPitch b_1
12
```

Observe how both `c0` and `b_1`, values of the Euterpea newtype `Pitch`, have the same absolute pitch
(measured in semitone distance from the lowest nonnegative pitch, i.e. for (C, -1).) This is in spite 
of the fact that the second pair element for each, the `Octave` (`Int`) part, differs. This is by design
and has to do in particular with the Euterpea datatype `PitchClass`, and how its pairings with `Octave` 
get mapped to absolute pitch values.

I sought an alternative representation in which 
* the `PitchClass`es would have a canonical representation as `Int` modulo 12, in line with the typical
  chromatic circle picture whereby e.g. C / B-sharp / D-double-flat all lie at "zero", then C-sharp / 
  B-double-sharp / D-flat at "one", etc. etc. until the pitch classes B / A-double-sharp / C-flat lie at 
  "eleven"
* pairings of this new version of `PitchClass` with some newtype of `Octave` bear the following relationship
  to their absolute pitch: 
    (chromatic-circle representation of `PitchClass`) + (12 * (`Octave` - 1)) == absolute semitone pitch
  where the term (`Octave` - 1) is in order to respect the convention that the lowest-sounding instance 
  having non-negative semitone count, for most Euterpea pitch classes, lies in octave #(-1).

Certain tasks for calculating semitone distances are simplified by using this representation, where the second
pair element `o` of `(p, o) :: (PitchClassEquiv, OctaveEquiv)` indicates (regardless of the pitch class part) that the
pitch in question has semitone value lying within the range `12 * (o + 1) <= semitone value < 12 * (o + 2)`.

### `toScaleDegreeRepr`

Given the following data:

* an input pitch, in Euterpea representation (i.e. as pair `(PitchClass, Octave)`) 
* a tonic pitch class, as `PitchClassEquiv` (i.e. using the alternative pitch-class
  representation for which there is a mapping into the chromatic circle of mod-12 integers defined)
* a scale mode (presently supporting the two principal musical modes of major, `MajorMode`, and 
  natural minor, `MinorMode`, as `ScaleMode` values at this time)

the function `toScaleDegreeRepr` will convert the input `Pitch` into a `ScalePitch`. The algebraic 
data type (ADT) `ScalePitch` is defined so as to convey the following information:

* which 'scale octave' the pitch in question lies in. This is calculated with respect to the tone 
  `(tonic :: PitchClassEquiv, -1 :: OctaveEquiv)` as the 1st scale degree of the (-1)'st scale
  (which is followed by the 0th scale, the 1st scale, etc.)
* what 'scale degree' the pitch in question relates to, either as an exact match or a modification 
  up / down by one or more accidentals. `+` / `-` marks following the scale degree indicate semitones 
  up (sharps) or down (flats), respectively
* what tonic and mode this scale-degree position is being reported with respect to.

Using this representation on the two `Pitch` values defined above, we can see that e.g.:

```
> toScaleDegreeRepr c0 (MkPCE A) MajorMode
-1th octave S3- (A MajorMode)
> toScaleDegreeRepr b_1 (MkPCE A) MajorMode
-1th octave S2+ (A MajorMode)
```

which informs us that both of these pitches lie within the (-1)'st octave of the A major scale (using
the convention that this octave of the scale starts from `(MkPCE A, -1)`.) Furthermore, while (C,0) is
a *lowering* of the 3rd scale degree in A major (namely, C-sharp), the alternative spelling for this pitch
using B-sharp is in fact a *raising* of the 2nd scale degree in this scale (namely, B-natural.)

### Validation

To validate that this representation in terms of scale degrees is reasonable, i.e. expresses pitches with
respect to the most typically encountered key signatures - those with tonic spelled using up to one sharp or flat -
and in a manner that is consistent with the semitone-count calculation for pitches, `toAbsPitchRepr` is defined on 
`ScalePitch` values, and this computation of semitone count for `ScalePitch` form of arbitrary pitches is compared 
with what Euterpea computes using its own `absPitch` function on the original pitches, before being re-cast into this
alternative form: this property test is called `chkScaleDegreeRepr` and currently is confirmed to work for all the 
typical key signatures mentioned above. 

(The implementation as well as validation for functionality e.g. deriving the spellings for all diatonic scale-degree
pitch classes of a given scale, casting into scale-degree representation, etc. had to be limited to these typical key 
signatures because the `PitchClass` datatype which this work is based upon includes only pitch-class spellings containing
up to 2 sharps or flats, whereas a datatype supporting pitch-class spellings for sharpened / flattened pitch classes 
with respect to totally arbitrary key signatures would in fact have to be infinite in principle.)



