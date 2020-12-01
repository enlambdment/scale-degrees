# scale-degrees

TO DO.

1. Break out `src/Lib.hs` into two library modules:
* one implementing PitchClassEquiv, a variety of 
pitch classes where enharmonic equivalence is supported;
* and one implementing ScaleDegree, the scale degree 
representation for pitches

2. Write tests for `src`. 

3. Write the other half of the isomorphism (?) induced by `toScaleDegreeRepr`.

4. Consider refactoring all the unsafe code with case statements including `error` 
branches, into a form that leverages `Maybe` or `Either`.
