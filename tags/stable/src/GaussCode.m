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

DTCode::usage = "DTCode[i1, i2, ...] represents a knot via its DT (Dowker-Thistlethwaite) code, while DTCode[{i11,...}, {i21...}, ...] likewise represents a link. DTCode also acts as a \"type caster\", so for example, DTCode[K] where K is is a named knot or link returns the DT code of K."

ConwayNotation::usage=""

Begin["`GaussCode`"]

GaussCode[K_] /; !MatchQ[
  Head[K], PD|DTCode|List|String|ConwayNotation|GaussCode
] := GaussCode[PD[K]]
GaussCode[gc_GaussCode] := gc;
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
GaussCode[HoldPattern[DTCode[ls__List]]] := Module[
  {dtc = {ls}, gc, k},
  gc = GaussCode[DTCode @@ Flatten[dtc]];
  k = 0; gc = dtc /. i_Integer :> {gc[[++k]], gc[[++k]]};
  GaussCode @@ (Flatten /@ gc)
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

DTCode[dtc_DTCode] := dtc;
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
DTCode[HoldPattern[GaussCode[ls__List]]] := Module[
  {gc = {ls}, agc, c, lens, l, NeededShifts, i, c1, c2, p1, p2, dtc, k},
  agc = gc /. i_Integer :> Abs[i];
  c = Length[gc];  lens = (Length /@ gc)/2; l = Plus @@ lens;
  NeededShifts = Table[
    Position[agc, i] /.
      {{c1_, p1_}, {c2_, p2_}} :> {c1, c2, Mod[p1 + p2 + 1, 2]},
    {i, l}
  ];
  shifts = Table[0, {c}];
  decided = ReplacePart[Table[False, {c}], True, 1];
  While[
    (NeededShifts = DeleteCases[
      NeededShifts, {c1_, c2_, _} /; decided[[c1]] && decided[[c2]]
    ]) =!= {},
    {{c1, c2, s}} = Select[
      NeededShifts,
      (decided[[#[[1]]]] || decided[[#[[2]]]]) &,
      1
    ];
    If[decided[[c1]],
      shifts[[c2]] = shifts[[c1]] + s; decided[[c2]] = True,
      shifts[[c1]] = shifts[[c2]] + s; decided[[c1]] = True
    ]
  ];
  gc = MapThread[RotateLeft, {gc, shifts}];
  dtc = DTCode[GaussCode @@ Flatten[gc]];
  k = 0; dtc = Table[dtc[[++k]], {j, c}, {lens[[j]]}];
  DTCode @@ dtc
]
DTCode[K_] /; !MatchQ[Head[K], DTCode|GaussCode|String] := DTCode[GaussCode[K]]

(* This function translates the string representations of DT codes used in the Knot Atlas back to KnotTheory's standard representation of a DT code. *)
DTCode[S_String]:=
  DTCode@@ToExpression["{"<>StringReplace[S," "\[Rule]","]<>"}"]

End[]; EndPackage[]
