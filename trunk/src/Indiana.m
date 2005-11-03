BeginPackage["KnotTheory`"]

BraidIndex::usage = "
BraidIndex[K] returns the braid index of the knot K, if known to
KnotTheory`.
"

BraidIndex::about = "
The braid index data known to KnotTheory` is taken from Charles
Livingston's \"Table of Knot Invariants\",
http://www.indiana.edu/~knotinfo/.
"

{Reversible, FullyAmphicheiral, NegativeAmphicheiral, Chiral};

SymmetryType::usage = "
SymmetryType[K] returns the symmetry type of the knot K, if known to
KnotTheory`. The possible types are: Reversible, FullyAmphicheiral,
NegativeAmphicheiral and Chiral.
"

UnknottingNumber::usage = "
UnknottingNumber[K] returns the unknotting number of the knot K, if known
to KnotTheory`. If only a range of possible values is known, a list of the
form {min, max} is returned.
"

UnknottingNumber::about = "
The unknotting numbers of torus knots are due to ???. All other
unknotting numbers known to KnotTheory` are taken from Charles
Livingston's \"Table of Knot Invariants\",
http://www.indiana.edu/~knotinfo/.
"

ThreeGenus::usage = "
ThreeGenus[K] returns the 3-genus of the knot K, if known to
KnotTheory`.
"

ThreeGenus::about = "
The 3-genus data known to KnotTheory` is taken from Charles
Livingston's \"Table of Knot Invariants\",
http://www.indiana.edu/~knotinfo/.
"

BridgeIndex::usage = "
BridgeIndex[K] returns the bridge index of the knot K, if known to
KnotTheory`.
"

BridgeIndex::about = "
The bridge index data known to KnotTheory` is taken from Charles
Livingston's \"Table of Knot Invariants\",
http://www.indiana.edu/~knotinfo/.
"

SuperBridgeIndex::usage = "
SuperBridgeIndex[K] returns the super bridge index of the knot K, if
known to KnotTheory`. If only a range of possible values is known, a
list of the form {min, max} is returned.
"

SuperBridgeIndex::about = "
The super bridge index data known to KnotTheory` is taken from Charles
Livingston's \"Table of Knot Invariants\",
http://www.indiana.edu/~knotinfo/.
"

NakanishiIndex::usage = "
NakanishiIndex[K] returns the Nakanishi index of the knot K, if known to
KnotTheory`.
"

NakanishiIndex::about = "
The Nakanishi index data known to KnotTheory` is taken from Charles
Livingston's \"Table of Knot Invariants\",
http://www.indiana.edu/~knotinfo/.
"

SymmetryType::about = "
The symmetry type data known to KnotTheory` is taken from Charles
Livingston's \"Table of Knot Invariants\",
http://www.indiana.edu/~knotinfo/.
"

Begin["`Indiana`"]

BraidIndex[K_]  := (
  Needs["KnotTheory`IndianaData`"];
  Unset[BraidIndex[K1_]];
  BraidIndex[K]
)

BridgeIndex[K_]  := (
  Needs["KnotTheory`IndianaData`"];
  Unset[BridgeIndex[K1_]];
  BridgeIndex[K]
)

NakanishiIndex[K_]  := (
  Needs["KnotTheory`IndianaData`"];
  Unset[NakanishiIndex[K1_]];
  NakanishiIndex[K]
)

SuperBridgeIndex[K_]  := (
  Needs["KnotTheory`IndianaData`"];
  Unset[SuperBridgeIndex[K1_]];
  SuperBridgeIndex[K]
)

SymmetryType[K_]  := (
  Needs["KnotTheory`IndianaData`"];
  Unset[SymmetryType[K1_]];
  SymmetryType[K]
)

ThreeGenus[K_]  := (
  Needs["KnotTheory`IndianaData`"];
  Unset[ThreeGenus[K1_]];
  ThreeGenus[K]
)

UnknottingNumber[TorusKnot[p_, q_]] := (p-1)(q-1)/2;
UnknottingNumber[K_]  := (
  Needs["KnotTheory`IndianaData`"];
  Unset[UnknottingNumber[K1_]];
  UnknottingNumber[K]
)

End[];

EndPackage[];
