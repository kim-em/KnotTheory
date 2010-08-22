BeginPackage["KnotTheory`"];

HFKHat::usage = 
  "HFKHat[K][t,m] returns the Poincare polynomial of the Heegaard-Floer \
Knot Homology (hat version) of the knot K, in the Alexander variable t \
and the Maslov variable m.";

HFKHat::about = 
  "The Heegaard-Floer Knot Homology program was written by Jean-Marie \
Droz in 2007 at the University of Zurich, based on methods of Anna \
Beliakova's arXiv:07050669.";

Begin["`HFK`"];

HFKHat[Knot[n_, k_]] := (
  Needs["KnotTheory`HFKHat4KnotsTo11`"];
  Unset[HFKHat[Knot[n1_, k1_]]];
  HFKHat[Knot[n, k]]
)
HFKHat[Knot[11, t_, k_]] := (
  Needs["KnotTheory`HFKHat4KnotsTo11`"];
  Unset[HFKHat[Knot[11, t1_, k1_]]];
  HFKHat[Knot[11, t, k]]
)

HFKHat[K_] /; AlternatingQ[K] := Function @@ {Expand[
  Alexander[K][-#1 #2]*(-#2)^(KnotSignature[K]/2)
]};
HFKHat[K_] /; (!AlternatingQ[K] && Head[K] =!= ArcPresentation) :=
  HFKHat[ArcPresentation[K]];
HFKHat[ap_ArcPresentation] := 
  HFKHat[ap] = Module[{f, out, minA, maxA, minM, maxM, R},
    CreditMessage[
     "The HFKHat program was written by Jean-Marie Droz in 2007 at the \
University of Zurich, based on methods of Anna \
Beliakova's arXiv:07050669."];
    SetDirectory[ToFileName[{KnotTheoryDirectory[], "HFK-Zurich"}]];
    f = OpenWrite["in", PageWidth -> Infinity];
    WriteString[f, 
      StringDrop[ToString[ap], StringLength["ArcPresentation"]]];
    Close[f];
    f = OpenWrite["out", PageWidth -> Infinity];
    Write[f, "batchVersion.py running..."];
    Close[f];
    Run["batchVersion.py"];
    f = OpenRead["out"];
    out = Read[f, String];
    Close[f];
    ResetDirectory[];
    If[StringMatchQ[out, "*batchVersion.py running...*"],
      $Failed,
    (*Else*) {minA, maxA, minM, maxM, R} = 
      ToExpression[StringReplace[out, {"[" -> "{", "]" -> "}"}]];
      Function @@ {Expand[(#2^Range[minM, maxM]).R.(#1^Range[minA, maxA])]}
    ]
  ]

End[]; EndPackage[];

