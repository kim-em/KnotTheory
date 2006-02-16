BeginPackage["KnotTheory`"]

GaussCode::usage = "
  GaussCode[i1, i2, ...] represents a knot via its Gauss
  Code following the conventions used by the knotilus website,
  http://srankin.math.uwo.ca/cgi-bin/retrieve.cgi/html/start.html.
  Likewise GaussCode[l1, l2, ...] represents a link, where each of
  l1, l2,...  is a list describing the code read along one component
  of the link.  GaussCode also acts as a \"type caster\", so for
  example, GaussCode[K] where K is is a named knot (or link) returns
  the Gauss code of that knot.
"

KnotilusURL::usage = "
  KnotilusURL[K_] returns the URL of the knot/link K on the knotilus
  website,\n
  http://srankin.math.uwo.ca/cgi-bin/retrieve.cgi/html/start.html.
"

DTCode::usage = "
  DTCode[i1, i2, ...] represents a knot via its DT
  (Dowker-Thistlethwaite) code. DTCode also acts as a \"type caster\",
  so for example, DTCode[K] where K is is a named knot returns the DT
  code of that knot.
"
ConwayNotation::usage=""

Begin["`GaussCode`"]

GaussCode[K_] /; !MatchQ[Head[K], PD|DTCode|List|String|ConwayNotation] := GaussCode[PD[K]]
GaussCode[PD[_Loop]] = GaussCode[]
GaussCode[PD[l___, _Loop, r___]] := Append[
  GaussCode[PD[l,r]],
  {}
]
GaussCode[PD[Xs___X]] := (
  pd = PD[Xs]; c=0;
  kc = KC @@ (s = Skeleton[pd] /. Loop -> List);
  pd /. X[i_, j_, k_, l_] :> (
    kc[[Sequence @@ First@Position[s,
      If[j - l == 1 || l - j > 1, l, j]
    ]]] = ++c;
    kc[[Sequence @@ First@Position[s, i]]] = -c
  );
  If[Length[s]==1,
    GaussCode @@ First[kc],
    GaussCode @@ kc
  ]
)
GaussCode[HoldPattern[DTCode[is___Integer]]] := Module[
  {dtc={is}, gc, k},
  gc = GaussCode @@ Range[2Length[dtc]];
  Do[
    gc[[2k - 1]] = Sign[dtc[[k]]]*k;
    gc[[Abs[dtc[[k]]]]] = -Sign[dtc[[k]]]*k,
    {k, Length[dtc]}
  ];
  gc
]

(* This function translates the string representations of Gauss codes used in the Knot Atlas back to KnotTheory's standard representation of a Gauss code. *)
GaussCode[S_String]:=GaussCode@@ToExpression["{"<>S<>"}"]

KnotilusURL[HoldPattern[GaussCode[is__Integer]]] := StringJoin[
  "http://srankin.math.uwo.ca/cgi-bin/retrieve.cgi/",
  StringReplace[
    ToString[{is}],
    {" " -> "", "{" -> "", "}" -> ""}
  ],
  "/goTop.html"
]
KnotilusURL[HoldPattern[GaussCode[ls__List]]] := StringJoin[
  "http://srankin.math.uwo.ca/cgi-bin/retrieve.cgi/",
  StringReplace[
    ToString[{ls}],
    {"{{" -> "", "}, {" -> ":", " " -> "", "}}" -> ""}
  ],
  "/goTop.html"
]
KnotilusURL[K_] /; Head[K] =!= GaussCode := KnotilusURL[GaussCode[K]]

DTCode[GaussCode[]] = DTCode[]
DTCode[HoldPattern[GaussCode[is__Integer]]] := Module[
  {gc={is}, agc, inds, odds, evens, s},
  agc = Abs /@ gc;
  inds = Flatten[Flatten[Position[agc, #]] & /@ Range[Max @@ agc]];
  odds = Select[inds, OddQ];
  evens = Select[inds, EvenQ];
  s = Sign[gc[[1]]];
  DTCode @@ Last /@ Sort[MapThread[
    {#1, s*Sign[gc[[#1]]]*#2} &,
    {odds, evens}
  ]]
]
DTCode[K_] /; !MatchQ[Head[K], DTCode|GaussCode|String] := DTCode[GaussCode[K]]

(* This function translates the string representations of DT codes used in the Knot Atlas back to KnotTheory's standard representation of a DT code. *)
DTCode[S_String]:=
  DTCode@@ToExpression["{"<>StringReplace[S," "\[Rule]","]<>"}"]

End[]; EndPackage[]
