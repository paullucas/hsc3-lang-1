* Pitch & record

> import Sound.SC3.Lang.Math.Pitch

The supercollider language pitch model
is organised as a tree with three separate
layers, and is designed to allow separate
processes to manipulate aspects of the
model independently.

The haskell variant implements Pitch as
a labeled data type, with a default value
such that scale degree 5 is the a above
middle c.

> freq (defaultPitch {degree = 5})

The note is given as a degree, with a modal
transposition, indexing a scale interpreted
relative to an equally tempered octave
divided into the indicated number of steps.

The midinote is derived from the note by
adding the inidicated root, octave and
gamut transpositions.

The frequency is derived by a chromatic
transposition of the midinote, with a
harmonic multiplier.

> let {p = defaultPitch
>     ;n = p {stepsPerOctave = 12
>            ,scale = [0,2,4,5,7,9,11]
>            ,degree = 0
>            ,mtranspose = 5}
>     ;m = n {root = 0
>            ,octave = 5
>            ,gtranspose = 0}
>     ;f = m {ctranspose = 0
>            ,harmonic = 1}}
> in (note n,midinote m,freq f)

By editing the values of aspects of
a pitch, processes can cooperate.
Below one process controls the note
by editing the modal transposition,
a second edits the octave.

> let {edit_mtranspose p d = p {mtranspose = mtranspose p + d}
>     ;edit_octave p o = p {octave = octave p + o}
>     ;p = repeat defaultPitch
>     ;q = zipWith edit_mtranspose p [0,2,4,3,5]
>     ;r = zipWith edit_octave q [0,-1,0,1,0]}
> in (map midinote q,map midinote r)
