(*Subversion test *) 

BeginPackage["KnotTheory`"]

KnotTheoryVersion::usage = "
KnotTheoryVersion[] returns the date of the current version of the
package KnotTheory`. KnotTheoryVersion[k] returns the kth field in
KnotTheoryVersion[].
"

KnotTheoryVersionString::usage = "
KnotTheoryVersionString[] returns a string containing the date and
time of the current version of the package KnotTheory`. It is generated
from KnotTheoryVersion[].
"

KnotTheoryWelcomeMessage::usage = "
KnotTheoryWelcomeMessage[] returns a string containing the welcome message
printed when KnotTheory` is first loaded.
"

KnotTheoryDirectory::usage = "
KnotTheoryDirectory[] returns the best guess KnotTheory` has for its
location on the host computer. It can be reset by the user.
"

Begin["`System`"]

KnotTheoryVersion[] = {2005, 8, 29, 15, 33, 11};
KnotTheoryVersion[k_Integer] := KnotTheoryVersion[][[k]]

KnotTheoryVersionString[] = StringJoin[
  {
    "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"
  }[[KnotTheoryVersion[2]]],
  " ",
  ToString[KnotTheoryVersion[3]],
  ", ",
  ToString[KnotTheoryVersion[1]],
  ", ",
  ToString[KnotTheoryVersion[4]],
  ":",
  ToString[KnotTheoryVersion[5]],
  ":",
  ToString[KnotTheoryVersion[6]]
]

KnotTheoryDirectory[] = (
  File /. Flatten[FileInformation[ToFileName[#,"KnotTheory"]] & /@ $Path]
)

KnotTheoryWelcomeMessage[] = StringJoin[
  "Loading KnotTheory` (version of ",
  KnotTheoryVersionString[],
")..."
]

Print[KnotTheoryWelcomeMessage[]]

End[]; EndPackage[];

BeginPackage["KnotTheory`"]

Knot::usage = "
  Knot[n, k] denotes the kth knot with n crossings in the Rolfsen table.
  Knot[11, Alternating, k] denotes the kth alternating 11-crossing knot in
  the Hoste-Thistlethwaite table. Knot[11, NonAlternating, k] denotes the
  kth non alternating 11-crossing knot in the Hoste-Thistlethwaite table.
"

Link::usage = "
  Link[n, Alternating, k] denotes the kth alternating n-crossing link in
  the Thistlethwaite table. Link[n, NonAlternating, k] denotes the kth
  non alternating n-crossing link in the Thistlethwaite table.
"

TorusKnot::usage = "
  TorusKnot[m, n] represents the (m,n) torus knot.
"

PD::usage = "
  PD[v1, v2, ...] represents a planar diagram whose vertices are v1, v2,
  .... PD also acts as a \"type caster\", so for example, PD[K] where K is 
  is a named knot (or link) returns the PD presentation of that knot.
"

X::usage = "
  X[i,j,k,l] represents a crossing between the edges labeled i, j, k
  and l starting from the incoming lower strand i and going
  counterclockwise through j, k and l.  The (sometimes ambiguous)
  orientation of the upper strand is determined by the ordering of
  {j,l}.
"

Xp::usage = "
  Xp[i,j,k,l] represents a positive (right handed) crossing between the
  edges labeled i, j, k and l starting from the incoming lower strand i
  and going counter clockwise through j, k and l. The upper strand is
  therefore oriented from l to j regardless of the ordering of {j,l}.
  Presently Xp is only lightly supported.
"

Xm::usage = "
  Xm[i,j,k,l] represents a negative (left handed) crossing between the
  edges labeled i, j, k and l starting from the incoming lower strand i
  and going counter clockwise through j, k and l. The upper strand is
  therefore oriented from j to l regardless of the ordering of {j,l}.
  Presently Xm is only lightly supported.
"

PositiveQ::usage = "
  PositiveQ[xing] returns True if xing is a positive (right handed)
  crossing and False if it is negative (left handed).
"

NegativeQ::usage = "
  NegativeQ[xing] returns True if xing is a negative (left handed)
  crossing and False if it is positive (right handed).
"

P::usage = "
  P[i,j] represents a bivalent vertex whose adjacent edges are i and j
  (i.e., a \"Point\" between the segment i and the segment j). Presently P
  is only lightly supported.
"

Loop::usage = "
  Loop[i] represents a crossingsless loop labeled i.
"

Crossings::usage = "
  Crossings[L] returns the number of crossings of a knot/link L (in its
  given presentation).
"

PositiveCrossings::usage = "
  PositiveCrossings[L] returns the number of positive (right handed)
  crossings in a knot/link L (in its given presentation).
"

NegativeCrossings::usage = "
  NegativeCrossings[L] returns the number of negaitve (left handed)
  crossings in a knot/link L (in its given presentation).
"

ConnectedSum::usage = "
  ConnectedSum[K1, K2] represents the connected sum of the knots K1 and
  K2 (ConnectedSum may not work with links).
"

KnotTheory::loading = "Loading precomputed data in `1`."

(* Lightly documented features: *)

NumberOfKnots::usage = "NumberOfKnots[type] return the number of knots of a
given type.";

Skeleton; Orient; NumberOfLinks; Alternating; NonAlternating; BR;
Mirror;

Begin["`Private`"]

SetAttributes[P, Orderless]

PD[pd_PD] := pd

PD[BR[k_, {}]] := PD @@ (Loop /@ Range[k])
PD[BR[k_Integer, l_List]] := Module[
  {
    a, b, c, d, e = Range[k], m = k, j, j1, Xp, Xm, pd, ar, cycles = 1, 
    closurerule, indexes, len, loops
  },
  pd = PD @@ (l  /. j_Integer :> (
    j1 = Abs[j];
    a = e[[j1]]; b = e[[j1 + 1]]; c = e[[j1]] = ++m; d = e[[j1 + 1]] = ++m;
    cycles *= ar[a, d]*ar[b, c];
    If[j > 0, Xp[b, d, c, a], Xm[a, b, d, c]]
  ));
  closurerule = MapThread[Rule, {e, Range[k]}];
  cycles = cycles /. closurerule //.
    ar[a_, b___, c_]ar[c_, d___, e_] :> ar[a, b, c, d, e] /.
    a_ar :> Rest[a];
  pd = pd /. closurerule;
  len = Length[indexes = Flatten[List @@@ List @@ cycles]];
  loops = Length[Complement[Range[k], Abs[l], Abs[l] + 1]];
  Join[
    pd /. MapThread[Rule, {indexes, Range[len]}] /. Xp | Xm -> X,
    Loop /@ PD @@ Range[len + 1, len + loops]
  ]
]

BR[TorusKnot[m_, n_]] /; m > 0 && n > 0 :=
  BR[n, Flatten[Table[Range[n - 1], {m}]]]
PD[TorusKnot[m_, n_]] /; m > 0 && n > 0 := PD[BR[TorusKnot[m, n]]]

RotateToMinimal[l_] := Module[
  {bl=l,rl=RotateLeft[l]},
  While[rl=!=l,
    bl=First[Sort[{bl,rl}]];
    rl=RotateLeft[rl]
  ];
  bl
]

Skeleton[pd_PD] := Sort[RotateToMinimal /@ (
  c = Times @@ pd /. {
    X[i_, j_, k_, l_] /; (l-j==1 || j-l>1) :> path[i, k] path[j, l],
    X[i_, j_, k_, l_] /; (j-l==1 || l-j>1) :> path[i, k] path[l, j],
    P[i_, j_] :> path[i, j]
  } //. {
    path[a__, i_]path[i_, b__] :> path[a, i, b],
    path[a__, i_]path[b__, i_] :> Join[path[a, i], Reverse[path[b]]],
    path[i_, a__]path[i_, b__] :> Join[Reverse[path[b]], path[i, a]]
  } /. {
    path[i_, a___, i_] :> Loop[i, a],
    path[i_, a___, j_](j_ -> i_) :> DirectedLoop[j, i, a],
    path[i_, a___, j_](i_ -> j_) :> Reverse[DirectedLoop[a, j, i]]
  };
  If[Head[c] === Times, List @@ c, {c}]
)]
Skeleton[L_] := Skeleton[PD[L]]

Mirror[PD[Xs___X]] := PD[Xs] /. {
  X[i_,j_,k_,l_] /; j-l==1 || l-j>1 :> X[l,i,j,k],
  X[i_,j_,k_,l_] /; l-j==1 || j-l>1 :> X[j,k,l,i]
}

Crossings[pd_PD] := Count[pd, _X|_Xp|_Xm]
Crossings[Knot[n_,__]] := n
Crossings[Link[n_,__]] := n
Crossings[TorusKnot[m_, n_]] /; (m>0 && n>0) := m*(n-1)
Crossings[L_] := Crossings[PD[L]]

PositiveQ[X[i_,j_,k_,l_]] /; j-l==1 || l-j>1 = True;
PositiveQ[X[i_,j_,k_,l_]] /; l-j==1 || j-l>1 = False;
PositiveQ[_Xp] = True;
PositiveQ[_Xm] = False;

NegativeQ[X[i_,j_,k_,l_]] /; j-l==1 || l-j>1 = False;
NegativeQ[X[i_,j_,k_,l_]] /; l-j==1 || j-l>1 = True;
NegativeQ[_Xp] = False;
NegativeQ[_Xm] = True;

PositiveCrossings[pd_PD] := Count[pd, _?PositiveQ];
PositiveCrossings[L_] := PositiveCrossings[PD[L]];
NegativeCrossings[pd_PD] := Count[pd, _?NegativeQ];
NegativeCrossings[L_] := NegativeCrossings[PD[L]];

ConnectedSum[pd1_PD, pd2_PD] := Module[
  {c1, c2, l2, npd1, npd2},
  If[Head[First[pd1]] === Loop, Return[Join[Drop[pd1, 1], pd2]]];
  If[Head[First[pd2]] === Loop, Return[Join[pd1, Drop[pd2, 1]]]];
  c1 = pd1[[1, 1]];
  c2 = pd2[[1, 1]];
  l2 = Max @@ Max @@@ pd2;
  npd1 = Map[If[# > c1, # + l2, #] &, pd1, {2}];
  npd1[[1, 1]] += l2;
  npd2 = Map[If[# <= c2, # + c1 + l2 - c2, # + c1 - c2] &, pd2, {2}];
  npd2[[1, 1]] -= l2;
  Join[npd1, npd2]
];
PD[ConnectedSum[K1_, K2_]] := ConnectedSum[PD[K1], PD[K2]]

End[]; EndPackage[]

BeginPackage["KnotTheory`"]

Jones::usage = "
  Jones[L][q] computes the Jones polynomial of a knot or link L as a
  function of the variable q.
"

Vassiliev::usage = "
  Vassiliev[2][K] computes the (standardly normalized) type 2 Vassiliev
  invariant of the knot K, i.e., the coefficient of z^2 in Conway[K][z].
  Vassiliev[3][K] computes the (standardly normalized) type 3
  Vassiliev invariant of the knot K, i.e., 3J''(1)-(1/36)J'''(1) where
  J is the Jones polynomial of K.
"

A2Invariant::usage = "
  A2Invariant[L][q] computes the A2 (sl(3)) invariant of a knot or link L
  as a function of the variable q.
"

Conway;

Begin["`Private`"]

KB[PD[],_,web_] := Expand[web];
KB[PD[_Loop, x___], inside_, web_] := Expand[(-A^2-1/A^2)KB[PD[x], inside, web]]
KB[pd_PD, inside_, web_] := Module[
  {pos = First[Ordering[Length[Complement[List @@ #, inside]]& /@ List @@ pd]]},
  pd[[pos]] /. {
    X[a_,b_,c_,d_] :> KB[
      Delete[pd,pos],
      Union[inside, {a,b,c,d}],
      Expand[web*(A P[a,d] P[b,c]+1/A P[a,b] P[c,d])] //. {
        P[e_,f_]P[f_,g_] :> P[e,g],
        P[e_,e_] -> -A^2-1/A^2, P[__]^2 -> -A^2-1/A^2
      }
    ],
    P[a_,b_] :> KB[
      Delete[pd,pos],
      Union[inside, {a,b}],
      Expand[web*P[a,b]] //. {
        P[e_,f_]P[f_,g_] :> P[e,g],
        P[e_,e_] -> -A^2-1/A^2, P[__]^2 -> -A^2-1/A^2
      }
    ]
  }
]

Jones[Knot[n_, k_]] := (
  Needs["KnotTheory`Jones4Knots`"];
  Unset[Jones[Knot[n1_, k1_]]];
  Jones[Knot[n, k]]
)
Jones[Knot[11, t_, k_]] := (
  Needs["KnotTheory`Jones4Knots11`"];
  Unset[Jones[Knot[11, t1_, k1_]]];
  Jones[Knot[11, t, k]]
)
Jones[Link[n_, t_, k_]] := (
  Needs["KnotTheory`Jones4Links`"];
  Unset[Jones[Link[n1_, t1_, k1_]]];
  Jones[Link[n, t, k]]
)
Jones[TorusKnot[m_, n_]] := (
  Needs["KnotTheory`Jones4TorusKnots`"];
  Unset[Jones[TorusKnot[m1_, n1_]]];
  Jones[TorusKnot[m, n]]
)

Jones[pd_PD] := Jones[pd] = Function @@ {Expand[Together[
  KB[pd, {}, 1] * (-A^3)^(PositiveCrossings[pd]-NegativeCrossings[pd]) / (-A^2-1/A^2) /. A -> #^(1/4)
]]}
Jones[L_] := Jones[L] = Jones[PD[L]] 

Vassiliev[2][K_] := Module[{z},
  Coefficient[Conway[K][z], z, 2]
]

Vassiliev[3][K_] := Module[{q, J},
  J = Jones[K][q];
  -1/36(D[J, {q, 3}] + 3D[J, {q, 2}]) /. q -> 1
]

SetAttributes[{Yo, Yi}, Orderless]
A2Quick[PD[], _, web_] := Expand[web];
A2Quick[pd_PD, inside_, web_] := Module[
  {
    pos = Last[Ordering[
      (Length[Intersection[List @@ #, inside]])& /@ List @@ pd
    ]],
    h = Max[List @@ Union @@ pd, inside],
    a, b, c, d, i, j, k, l, m, n, o, r
  },
    pd[[pos]] /. X[a_, b_, c_, d_] :> A2Quick[
       Delete[pd, pos],
       Union[inside, {a, b, c, d, ++h}],
       FixedPoint[
         Expand[# //. {
           ar[i_, i_] :> q^2 + 1 + 1/q^2,
           ar[i_, j_]ar[j_, k_] :> ar[i, k],
           Yi[i_, j_, k_]ar[l_, k_] :> Yi[i, j, l],
           Yo[i_, j_, k_]ar[k_, l_] :> Yo[i, j, l],
           Yi[i_, j_, k_]Yo[i_, j_, l_] :> (q + 1/q)ar[k, l],
           Yo[i_, j_, k_]Yi[j_, l_, m_]Yo[m_, n_, o_]Yi[o_, r_, k_]
             :> ar[r, i]ar[l, n] + ar[l, i]ar[r, n]
         }]&,
         Expand[
           web * If[d - b == 1 || b - d > 1,
             1/q^2 ar[a, d]ar[b, c] - 1/q^3 Yi[a, b, h]Yo[c, d, h],
             q^2 ar[a, b]ar[d, c] - q^3 Yi[a, d, h]Yo[b, c, h]
           ]
         ]
      ]
   ]
]

A2Invariant[Knot[n_, k_]] := (
  Needs["KnotTheory`A2Invariant4Knots`"];
  Unset[A2Invariant[Knot[n1_, k1_]]];
  A2Invariant[Knot[n, k]]
)
A2Invariant[Knot[11, t_, k_]] := (
  Needs["KnotTheory`A2Invariant4Knots11`"];
  Unset[A2Invariant[Knot[11, t1_, k1_]]];
  A2Invariant[Knot[11, t, k]]
)
A2Invariant[Link[n_, t_, k_]] := (
  Needs["KnotTheory`A2Invariant4Links`"];
  Unset[A2Invariant[Link[n1_, t1_, k1_]]];
  A2Invariant[Link[n, t, k]]
)
A2Invariant[TorusKnot[m_, n_]] := (
  Needs["KnotTheory`A2Invariant4TorusKnots`"];
  Unset[A2Invariant[TorusKnot[m1_, n1_]]];
  A2Invariant[TorusKnot[m, n]]
)

A2Invariant[L_] := A2Invariant[L] = Module[
  {pd = PD[L], loops},
  loops = Position[pd, _Loop];
  Function @@ {
    Expand[
      (q^2 + 1 + 1/q^2)^Length[loops]
      * A2Quick[Delete[pd, loops], {}, 1]
    ] /. q -> #
  }
]

End[]; EndPackage[]

BeginPackage["KnotTheory`"]

KnotSignature::usage = "
  KnotSignature[K] returns the signature of a knot K.
"

Begin["`Private`"]

KnotSignature[PD[Loop[_]]] = 0
KnotSignature[pd_PD] := KnotSignature[pd] = Module[
  {spd, a, s = 0, c, cs, A, es},
  spd = (Times @@ pd) /. 
    X[i_, j_, k_, l_] :> If[j - l == 1 || l - j > 1 , Xp, Xm][i, j, k, l];
  cs = spd /. {
     Xp[i_, j_, k_, l_] :> a[j, ++s, i]a[k, ++s, -j]a[-l, ++s, -k]a[-i, ++s, l],
     Xm[i_, j_, k_, l_] :> a[-j, ++s, i]a[k, ++s, j]a[l, ++s, -k]a[-i, ++s, -l]
  } //.  a[i_, x__, j_]a[j_, y__, k_] :> a[i, x, y, k] /. 
    a[i_, x__, j_] :> a[x];
  A = Table[0, {Length[cs]}, {Length[cs]}];
  Do[
    indices = Position[cs, #][[1, 1]] & /@ (4i - 4 + {1, 2, 3, 4});
    A[[indices, indices]] += If[Head[spd[[i]]] === Xp,
      {{0, 0, 0, 0}, {1, -1, 0, 0}, {0, -1, 0, 1}, {-1, 2, 0, -1}},
      {{1, -1, 0, 0}, {0, 0, 0, 0}, {-2, 1, 1, 0}, {1, 0, -1, 0}}
    ],
    {i, Length[spd]}
  ];
  es = Re[Eigenvalues[N[A + Transpose[A]]]] /.  x_Real /; Abs[x] < 10^-9 -> 0;
  -Plus @@ Sign /@ es
]
KnotSignature[K_] := KnotSignature[PD[K]]

End[]; EndPackage[]
BeginPackage["KnotTheory`"]		(* Braids *)

BR::usage = "BR stands for Braid Representative. BR[k,l] represents a
braid on k strands with crossings l={i1,i2,...}, where a positive index
i within the list l indicates a right-handed crossing between strand
number i and strand number i+1 and a negative i indicates a left handed
crossing between strands numbers |i| and |i|+1. Each ij can also be a
list of non-adjacent (i.e., commuting) indices. BR also acts as a
\"type caster\": BR[K] will return a braid whose closure is K if K is
given in any format that KnotTheory` understands. BR[K] where K is is a
named knot with up to 10 crossings returns a minimum braid
representative for that knot."

BR::about = "
The minimum braids representing the knots with up to 10 crossings were
provided by Thomas Gittings. See his article on the subject at
arXiv:math.GT/0401051. Vogel's algorithm was implemented by Dan Carney in
the summer of 2005 at the University of Toronto.
"

BraidLength::usage = "
BraidLength[K] returns the braid length of the knot K, if known to
KnotTheory`.
"

Mirror::usage = "
  Mirror[br] return the mirror braid of br.
"

CollapseBraid::usage = "
  CollapseBraid[br] groups together commuting generators in the braid
  br. Useful in conjunction with BraidPlot to produce compact braid plots.
"

BraidPlot::usage = "
  BraidPlot[br, opts] produces a plot of the braid br. Possible options
  are Mode, HTMLOpts, WikiOpts and Images.
"

NotAvailable; Mode; HTMLOpts; Images; WikiOpts;

Begin["`Private`"]

BR[br_BR] := br;

BR[k_, s_String] := BR[
  k, ToCharacterCode[s] /. j_Integer :> If[j < 97, 64 - j, j - 96]
]

Mirror[BR[k_Integer, l_List]] := BR[k, -l]
BR[Mirror[K_]] := Mirror[BR[K]]

BraidLength[K_] := Crossings[BR[K]]

CollapseBraid[NotAvailable] = NotAvailable
CollapseBraid[BR[k_, l_List]] := Module[
  {
    queue = Flatten[List /@ l], collapsed = {}, footprints = {}, current,
    abscurr, j, len
  },
  While[queue =!= {},
    abscurr = Abs[current = First[queue]]; queue = Rest[queue];
    j = len = Length[collapsed];
    While[j > 0 && FreeQ[footprints[[j]], abscurr], --j];
    If[j == len, AppendTo[collapsed, {}]; AppendTo[footprints, {}]];
    AppendTo[collapsed[[j+1]], current];
    footprints[[j+1]] = Union[footprints[[j+1]], abscurr + {-1, 0, 1}]
  ];
  BR[k, collapsed]
]

BraidPlot[NotAvailable, ___] := NotAvailable

Options[BraidPlot] = {
  Mode -> "Graphics",
  Images -> {"0.gif", "1.gif", "2.gif", "3.gif", "4.gif"},
  HTMLOpts -> "",
  WikiOpts -> ""
}

BraidPlot[BR[k_Integer, l_List], opts___Rule] := Module[
  {
    mat, i, j, ll, g, t, x, y,
    mode = (Mode /. {opts} /. Options[BraidPlot]),
    images = (Images /. {opts} /. Options[BraidPlot]),
    htmlopts = (HTMLOpts /. {opts} /. Options[BraidPlot] /. "" -> " "),
    wikiopts = (WikiOpts /. {opts} /. Options[BraidPlot])
  },
  If[StringTake[htmlopts, 1]=!=" ", htmlopts=" "<>htmlopts];
  If[StringTake[htmlopts, -1]=!=" ", htmlopts=htmlopts<>" "];
  If[Length[l]>0, 
    mat = Table[0, {k}, {Length[l]}];
    Do[
      ll = Flatten[{l[[i]]}];
      Do[
        If[ll[[j]] > 0,
          mat[[ll[[j]], i]] = 1;  mat[[ll[[j]]+1, i]] = 2,
          mat[[-ll[[j]], i]] = 3;  mat[[-ll[[j]]+1, i]] = 4
        ],
        {j, Length[ll]}
      ],
      {i, Length[l]}
    ],
    mat = Table[{0}, {k}]
  ];
  Switch[mode,
    "Graphics", Graphics[MapIndexed[g, mat, {2}] /. g[t_, {j_, i_}] :> (
      x = i - 1; y = k - j;
      Switch[t,
        0, Line[{{x, y+0.5}, {x+1, y+0.5}}],
        1, {
          Line[{{x, y+0.5}, {x+0.5, y}}],
          Line[{{x+0.75, y+0.25}, {x+1, y+0.5}}]
        },
        2, {
          Line[{{x, y+0.5}, {x+0.25, y+0.75}}],
          Line[{{x+0.5, y+1}, {x+1, y+0.5}}]
        },
        3, {
          Line[{{x, y+0.5}, {x+0.25, y+0.25}}],
          Line[{{x+0.5, y}, {x+1, y+0.5}}]
        },
        4, {
           Line[{{x, y+0.5}, {x+0.5, y+1}}],
           Line[{{x+0.75, y+0.75}, {x+1, y+0.5}}]
        }
      ]
    )],
    "HTML", StringJoin[
      "<table cellspacing=0 cellpadding=0 border=0>\n",
      Table[
        {
          "<tr><td>",
          ("<img"<>htmlopts<>"src="<>images[[#+1]]<>">") & /@ mat[[j]],
          "</td></tr>\n"
        },
        {j, k}
      ],
      "</table>"
    ],
    "Wiki", StringJoin[
      "<table cellspacing=0 cellpadding=0 border=0>\n",
      Table[
        {
          "<tr><td>",
          ("[[Image:"<>images[[#+1]]<>wikiopts<>"]]") & /@ mat[[j]],
          "</td></tr>\n"
        },
        {j, k}
      ],
      "</table>"
    ],
    _, mat
  ]
]

End[]; EndPackage[]


BeginPackage["TubePlot`", {"Utilities`FilterOptions`"}]

TubePlot::usage = "
  TubePlot[gamma, {t, t0, t1}, r, opts] plots the space curve gamma
  with the variable t running from t0 to t1, as a tube of radius r. The
  available options are TubeSubdivision, TubeFraming and TubePlotPrelude.
  All other options are passed on to Graphics3D.
  TubePlot[TorusKnot[m, n], opts] produces a tube plot of the (m,n)
  torus knot.
"

TubeSubdivision::usage = "
  TubeSubdivision is an option for TubePlot. TubePlot[__, TubeSubdivision
  -> {l, m} draws the tube subdivided to l pieces lengthwise and m pieces
  around. The default is TubeSubdivision -> {50, 12}.
"

TubeFraming::usage = "
  TubeFraming is an option for TubePlot. TubePlot[gamma, {t, __},
  _, TubeFraming -> n] sets the framing of the tube (visible when
  TubeSubdivision -> {l, m} with small m) to be the vector n, which
  in itself may be a function of t.  Thus TubeFraming -> {0,0,1} is
  \"blackboard framing\". TubeFraming -> Normal (default) uses the normal
  vector of the curve gamma.
"

TubePlotPrelude::usage = "
  TubePlotPrelude is an option for TubePlot. Its value is passed to
  Graphics3D before the main part of the plot, allowing to set various
  graphics options. For example,  TubePlotPrelude -> EdgeForm[{}] will
  suppress the drawing of edges between the polygons making up the tube.
  The default is TubePlotPrelude -> {}.
"

Begin["`Private`"]

Options[TubePlot] = {
  TubeSubdivision -> {50, 12}, TubeFraming -> Normal, TubePlotPrelude -> {}
};
TubePlot[gamma_, {t_, t1_, t2_}, r_, opts___Rule] := Module[
  {
    l, m, framing, prelude, Normalize, ProjectOut, dt, ts, gs, Ts, Ns, Bs, 
    args, Cs, Ss, ring, tube
  },
  {{l, m}, framing, prelude} =
    {TubeSubdivision, TubeFraming, TubePlotPrelude} /.
    {opts} /.  Options[TubePlot];
  Normalize[v_] := v/Sqrt[v.v]; ProjectOut[v_, w_] := v - (v.w)w;
  dt = N[t2 - t1]/l;
  ts = t1 + Range[-1, l + 1]*dt;
  gs = (gamma /. (t -> #)) & /@ ts;
  Ts = (RotateLeft[gs] - gs)/dt;
  Ns = If[framing === Normal,
    (Ts - RotateRight[Ts])/dt,
    (framing /. (t -> #)) & /@  ts
  ];
  Ts = Normalize /@ (Ts + RotateRight[Ts]);
  Ns = Normalize /@ MapThread[ProjectOut, {Ns, Ts}];
  Bs = Normalize /@ MapThread[Cross, {Ts, Ns}];
  args = N[2Pi*Range[0, m]/m];
  {Cs, Ss} = {Cos /@ args, Sin /@ args};
  ring[g_, n_, b_] := 
    Transpose[g + r(Outer[Times, n , Cs] + Outer[Times, b, Ss])];
  tube = MapThread[ring, {gs, Ns, Bs}];
  Graphics3D[{prelude, Table[ 
    Polygon[{tube[[i, j]], tube[[i+1, j]], tube[[i+1, j+1]], tube[[i, j+1]]}],
    {i, 2, l + 1}, {j, m}
  ]}, FilterOptions[Graphics3D, opts]]
]

End[]; EndPackage[]

TubePlot[TorusKnot[m_, n_], opts___] := TubePlot[
  {Cos[n t], Sin[n t], 0} + 
    0.5{Cos[m t]Cos[n t], Cos[m t]Sin[n t], -Sin[m t]},
  {t, 0, 2Pi}, 1/Max[m, n], opts,
  TubeSubdivision -> {40(m + 2n), 12}, TubeFraming -> {0,0,1},
  TubePlotPrelude -> EdgeForm[{}], Boxed -> False, ViewPoint -> {0, 0, 1}
];
BeginPackage["KnotTheory`"]

PD; X; OuterFace; Gap; Colour; StrandColour

DrawPD::usage = "
  DrawPD[pd] takes the planar diagram description pd and creates a
  graphics object containing a picture of the knot.
  DrawPD[pd,options], where options is a list of rules, allows the user
  to control some of the parameters.  OuterFace->n sets the face at
  infinity to the face numbered n.  OuterFace->{e_1,e_2,...,e_n} sets
  the face at infinity to a face which has edges e_1, e_2, ..., e_n in
  the planar diagram description.  Gap->g sets the size of the gap
  around a crossing to length g.
"

DrawPD::about = "
  DrawPD was written by Emily Redelmeier at the University of Toronto in
  the summers of 2003 and 2004.
"

Begin["`DrawPD`"]

(* Representation Manipulation *)

(* Positions of the various fields *)
neighbours=1;
type=2;
r=3;
centre=4;
graphicsObjs=5;

FieldValues[triangulation_,field_]:=
  Table[triangulation[[i,field]],{i,
      Length[triangulation]}]

AddField[triangulation_,field_,values_]:=
  Table[ReplacePart[
      If[Length[triangulation[[i]]]<
          field,PadRight[
          triangulation[[i]],field],
        triangulation[[i]]],
      values[[i]],field],{i,
      Length[triangulation]}]

ChangeField[triangulation_,field_,f_]:=
  MapAt[f,triangulation,Table[{i,field},{i,Length[triangulation]}]]

DeriveField[triangulation_,field_,f_]:=
  AddField[triangulation,field,Map[f,triangulation]]

(* PD Graph Manipulation *)

OtherVertex[pd_,coordinates_]:=
  Complement[
      Position[pd,
        Extract[pd,
          coordinates]],{coordinates}][[1\
]]

Faces[pd_]:=
  Select[Flatten[
      Table[NestWhileList[
          Function[{coordinates},{OtherVertex[pd,
                  coordinates][[1]],
              Mod[OtherVertex[pd,
                      coordinates][[2]]-\
1,Length[pd[[OtherVertex[pd,
                        coordinates][[1]]\
]]],1]}],{i,j},Unequal,All,Infinity,-1],{i,
          Length[pd]},{j,
          Length[pd[[i]]]}],1],
    Function[{face},
      face[[1]]==
        Sort[face][[1]]]]

Triangulate[pd_]:=(facelist=Faces[pd];
    Join[Table[{Flatten[
            Table[{Length[pd]+
                  pd[[vertex,i]],
                Length[pd]+Length[Union[Flatten[pd,1,X]]]+
                  Position[facelist,face_/;MemberQ[face,{vertex,i}],
                      1][[1,1]]},{i,
                Length[pd[[vertex]]]}]],
          "X"},{vertex,Length[pd]}],
      Table[{Flatten[
            Table[{Length[pd]+Length[Union[Flatten[pd,1,X]]]+
                  Position[facelist,
                      face_/;MemberQ[face,
                          Position[pd,edge,2][[
                            i]]],2][[1,
                    1]],
                Position[pd,edge,2][[i,
                  1]]},{i,Length[Position[pd,edge,2]]}]],
          "e"},{edge,Length[Union[Flatten[pd,1,X]]]}],
      Table[{Flatten[
            Table[{facelist[[face,i,1]],
                Length[pd]+
                  Extract[pd,
                    facelist[[face,
                      i]]]},{i,
                Length[facelist[[
                    face]]]}]],"f"},{face,
          Length[facelist]}]])

GetOuterFace[triangulation_,edges_]:=
  Select[Range[Length[triangulation]],
      Function[v,
        triangulation[[v,type]]==
            "f"&&Union[
              Map[triangulation[[#]]&,
                triangulation[[v,
                  neighbours]]],
              Select[triangulation,#[[type\
]]=="e"&][[edges\
]]]==Union[
              Map[triangulation[[#]]&,
                triangulation[[v,
                  neighbours]]]]]][[1\
]]

NthOrderNeighbours[triangulation_,v_,0]:={v}
NthOrderNeighbours[triangulation_,v_,n_/;n>0]:=
  Apply[Union,
    Map[triangulation[[#,neighbours]]&,
      NthOrderNeighbours[triangulation,v,n-1]]]

DefaultOuterFace[triangulation_]:=
  Sort[Select[Range[Length[triangulation]],
        triangulation[[#,
              type]]=="f"&],
      Length[triangulation[[#1,
              neighbours]]]>=Length[
            triangulation[[#2,
              neighbours]]]&][[1\
]]

(* generalize to accept other types of vertices (v, etc.) *)

(* Circle Packing: Radii *)

CircleAngle[r_,r1_,r2_]:=
  ArcCos[((r1+r)^2+(r2+r)^2-(r1+r2)^2)/(2(r1+r)(r2+r))]

FlowerAngle[triangulation_,v_,radii_]:=
  Plus@@(CircleAngle[radii[[v]],#1,#2]&@@@
        Map[radii[[#]]&,
          Transpose[{triangulation[[v,
                neighbours]],
              RotateRight[
                triangulation[[v,
                  neighbours]]]}],{2}])

AdjustRadius[triangulation_,v_,targetAngle_,radii_]:=
  N[radii[[v]]((1-
              Cos[FlowerAngle[triangulation,v,radii]/
                  Length[triangulation[[v,
                      neighbours]]]]+
              Sqrt[2-2Cos[
                      FlowerAngle[triangulation,v,radii]/
                        Length[
                          triangulation[[v,
                            neighbours]]]]])/(1+
              Cos[FlowerAngle[triangulation,v,radii]/
                  Length[triangulation[[v,
                      neighbours]]]]))(Sqrt[
            2/(1-Cos[
                    targetAngle/
                      Length[triangulation[[v,
                          neighbours]]]])]-1)]

PackingStep[triangulation_,targetAngles_]:=(
    radii=Table[Unique[radius],{Length[triangulation]}];
    Compile[Evaluate[radii],
      Evaluate[Table[
          If[targetAngles[[v]]==0,
            radii[[v]],
            AdjustRadius[triangulation,v,
              targetAngles[[v]],
              radii]],{v,Length[triangulation]}]]]
    )

GetRadii[triangulation_,targetAngles_,radii_]:=(
    EvaluatedPackingStep=PackingStep[triangulation,targetAngles];
    NestWhile[EvaluatedPackingStep@@#&,radii,Unequal,2]
    )

DefaultDirichlet[triangulation_]:=
  AddField[triangulation,r,
    GetRadii[triangulation,
      ReplacePart[Table[2Pi,{Length[triangulation]}],
        0,{{1},{triangulation[[1,neighbours,
              1]]},{triangulation[[1,
              neighbours,2]]}}],
      Table[1,{Length[triangulation]}]]]

(* Circle Packing: Positions *)

PlaceFlower[triangulation_,v_,neighbour_]:=
  For[w=triangulation[[v,neighbours,
        neighbour]];
    theta=Arg[(z[w]-
              z[v])/(triangulation[[w,
                r]]+
              triangulation[[v,r]])];
    lastr=triangulation[[w,r]];i=1,
    i<Length[triangulation[[v,
          neighbours]]],i++,
    w=triangulation[[v,neighbours,
        Mod[neighbour+i,
          Length[triangulation[[v,
              neighbours]]],1]]];
    currentr=triangulation[[w,r]];
    theta+=CircleAngle[
        triangulation[[v,r]],lastr,
        currentr];lastr=currentr;
    z[w]=z[v]+(triangulation[[v,r]]+
              currentr)Exp[I*theta];placed=Union[placed,{w}]]

PackCircles[triangulation_]:=(Clear[z];placed={};surrounded={};v1=1;z[v1]=0;
    placed=Union[placed,{v1}];
    v2=triangulation[[v1,neighbours,1]];
    z[v2]=triangulation[[v1,r]]+
        triangulation[[v2,r]];
    placed=Union[placed,{v2}];
    While[Length[placed]!=Length[triangulation],
      v=Complement[placed,
            surrounded][[1]];
      PlaceFlower[triangulation,v,
        Position[
            triangulation[[v,
              neighbours]],
            w_/;MemberQ[placed,w]][[1,
          1]]];surrounded=Union[surrounded,{v}]];
    Table[z[i],{i,Length[triangulation]}])

PackCirclesBound[triangulation_]:=(Clear[z];placed={};surrounded={};
    v1=Select[Range[Length[triangulation]],
          FlowerAngle[triangulation,#,
                FieldValues[triangulation,
                  r]]==2Pi&][[1\
]];z[v1]=0;placed=Union[placed,{v1}];
    v2=triangulation[[v1,neighbours,1]];
    z[v2]=triangulation[[v1,r]]+
        triangulation[[v2,r]];
    placed=Union[placed,{v2}];
    While[Length[placed]!=Length[triangulation],
      v=Select[Complement[placed,surrounded],
            FlowerAngle[triangulation,#,
                  FieldValues[triangulation,
                    r]]==2Pi&][[1\
]];
      PlaceFlower[triangulation,v,
        Position[
            triangulation[[v,
              neighbours]],
            w_/;MemberQ[placed,w]][[1,
          1]]];surrounded=Union[surrounded,{v}]];
    Table[z[i],{i,Length[triangulation]}])

AddPositions[triangulation_]:=
  AddField[triangulation,centre,PackCircles[triangulation]]

AddPositionsBound[triangulation_]:=
  AddField[triangulation,centre,PackCirclesBound[triangulation]]

(* Fractional Linear Transformations *)

NewRadius[z_,radius_,{{a_,b_},{c_,d_}}]:=
  radius*Abs[a*d-b*c]/(Abs[c*z+d]^2-Abs[c]^2*radius^2)

NewPosition[z_,
    radius_,{{a_,b_},{c_,d_}}]:=((a*z+b)*Conjugate[c*z+d]-
        a*Conjugate[c]*radius^2)/((c*z+d)*Conjugate[c*z+d]-
        c*Conjugate[c]*radius^2)

ApplyFLMap[
    triangulation_,{{a_,b_},{c_,d_}}]:=(newRadii=
      Table[NewRadius[
          triangulation[[v,centre]],
          triangulation[[v,
            r]],{{a,b},{c,d}}],{v,Length[triangulation]}];
    newPositions=
      Table[NewPosition[
          triangulation[[v,centre]],
          triangulation[[v,
            r]],{{a,b},{c,d}}],{v,Length[triangulation]}];
    AddField[AddField[triangulation,r,newRadii],centre,newPositions])

Moebius[a_]:={{1,-a},{-Conjugate[a],1}}

ComposeMoebius[a_,b_]:=(a+b)/(1+a*Conjugate[b])

(* Inversion *)

PutInside[triangulation_,outerFace_]:=
  ApplyFLMap[
    triangulation,{{0,
        triangulation[[outerFace,
          r]]},{1,-triangulation[[
            outerFace,centre]]}}]

(* Balancing *)

BalanceStep[triangulation_,moebiusConst_]:=
  ComposeMoebius[
    Plus@@FieldValues[ApplyFLMap[triangulation,Moebius[moebiusConst]],centre]/
      Length[triangulation],moebiusConst]

BalanceMoebius[triangulation_]:=FixedPoint[BalanceStep[triangulation,#]&,0]

Balance[triangulation_]:=
  ApplyFLMap[triangulation,Moebius[BalanceMoebius[triangulation]]]

(* Graphics *)

xyCoords[z_]:={Re[z],Im[z]}

(* Graphics: Circle Packing *)

PackingGraphics[triangulation_]:=Graphics[Join[
      Map[
        Circle[xyCoords[#[[centre]]],
            Abs[#[[r]]]]&,triangulation],
      Table[Text[i,
          xyCoords[
            triangulation[[i,
              centre]]]],{i,Length[triangulation]}]],
    AspectRatio->1]

(* Knot Manipulation *)

ConnectedNeighbours[triangulation_,v_]:={{1,5},{3,7}}/;
    triangulation[[v,type]]=="X"
ConnectedNeighbours[triangulation_,v_]:={{2,4}}/;
    triangulation[[v,type]]=="e"
ConnectedNeighbours[triangulation_,v_]:={}/;
    triangulation[[v,type]]=="f"

Xunder=1;
Xover=2;

OtherEnd[triangulation_,v_,neighbour_]:=
  Select[Select[
          Map[triangulation[[v,
                neighbours,#]]&,
            ConnectedNeighbours[triangulation,v]],
          MemberQ[#,
              neighbour]&][[1]],#\
!=neighbour&][[1]]

AdjacentComponents[triangulation_,{v_,n_}]:=
  Select[Map[Take[#,2]&,
      Position[Table[
          Map[triangulation[[w,
                neighbours,#]]&,
            ConnectedNeighbours[triangulation,w]],{w,Length[triangulation]}],
        v]],Function[component,
      MemberQ[Map[
          triangulation[[v,
              neighbours,#]]&,
          ConnectedNeighbours[triangulation,v][[
            n]]],
        component[[1]]]]]

GetStrand[triangulation_,{v_,n_}]:=
  FixedPoint[
    Apply[Union,
        Append[Map[
            Function[component,
              AdjacentComponents[triangulation,component]],#],#]]&,{{v,n}}]

ListStrands[triangulation_]:=
  Union[Map[GetStrand[triangulation,#]&,
      Flatten[Table[{v,n},{v,Length[triangulation]},{n,
            Length[ConnectedNeighbours[triangulation,v]]}],1]]]

(* Graphics: Graphs *)

gapParam=1;

ArcCentre[z_,radius_,{z1_,z2_}]:=2*radius/Conjugate[Sign[z1-z]+Sign[z2-z]]

ArcRadius[z_,radius_,{z1_,z2_}]:=
  Sqrt[Abs[ArcCentre[z,radius,{z1,z2}]]^2-radius^2]

LinearProject[{v1_,v2_},w_]:=Re[v2-v1]*Cos[Arg[w]]+Im[v2-v1]*Sin[Arg[w]]

ArcProject[{v1_,v2_},o_]:=Mod[Arg[v2-o]-Arg[v1-o],2*Pi,-\[Pi]]

ArcOrientation[z_,radius_,{z1_,z2_}]:=
  Sign[ArcProject[{radius*Sign[z1-z],radius*Sign[z2-z]},
      ArcCentre[z,radius,{z1,z2}]]]

(*find a way to consolidate the crossing functions*)

ArcCrossing[
    radius_,{arc1_,
      arc2_}]:=(Re[arc1]*Im[arc2]-Im[arc1]*Re[arc2]-
          Sign[Re[arc1]*Im[arc2]-Im[arc1]*Re[arc2]]*
            Sqrt[(Re[arc1]*Im[arc2]-Im[arc1]*Re[arc2])^2-
                Abs[arc1-arc2]^2*radius^2])/
      Abs[arc1-arc2]^2*(arc1-arc2)*I

ArcLineCrossing[
    radius_,{arc_,
      line_}]:=((Re[arc]*Re[line]+Im[arc]*Im[line]-
            Sign[Re[arc]*Re[line]+Im[arc]*Im[line]]*
              Sqrt[(Re[arc]*Re[line]+Im[arc]*Im[line])^2-
                  Abs[line]^2*radius^2])/Abs[line]^2)*line

Crossing[z_,radius_,{{z1_,z2_},{z3_,z4_}}]:=
  If[Sign[z1-z]+Sign[z2-z]==0,
    If[Sign[z3-z]+Sign[z4-z]==0,0,
      ArcLineCrossing[radius,{ArcCentre[z,radius,{z3,z4}],z2-z1}]],
    If[Sign[z3-z]+Sign[z4-z]==0,
      ArcLineCrossing[radius,{ArcCentre[z,radius,{z1,z2}],z4-z3}],
      ArcCrossing[
        radius,{ArcCentre[z,radius,{z1,z2}],ArcCentre[z,radius,{z3,z4}]}]]]

arcConst=10^(-5);

ArcDistance[z_,radius_,{z1_,z2_},{w1_,w2_}]:=
  Which[Sign[z1-z]+Sign[z2-z]==0,LinearProject[{w1,w2},z2-z1],
    Abs[ArcProject[{w1,w2},ArcCentre[z,radius,{z1,z2}]]](*==0*)(**)<
      arcConst(**),LinearProject[{w1,w2},
      ArcOrientation[z,
          radius,{z1,z2}]*I*((w1+w2)/2-
            ArcCentre[z,radius,{z1,z2}])],True,
    ArcOrientation[z,radius,{z1,z2}]*ArcRadius[z,radius,{z1,z2}]*
      ArcProject[{w1,w2},ArcCentre[z,radius,{z1,z2}]]]

GetArc[z_,radius_,{z1_,z2_},{w1_,w2_},{l1_,l2_}]:=
  Which[ArcDistance[z,radius,{z1,z2},{w1,w2}]<
      l1+l2,{},(*Mod[
          Arg[w1-ArcCentre[z,radius,{z1,z2}]]+
            ArcOrientation[z,radius,{z1,z2}]*l1/ArcRadius[z,radius,{z1,z2}],
          2*Pi,Arg[w1-ArcCentre[z,radius,{z1,z2}]]]==
        Mod[Arg[z2-ArcCentre[z,radius,{z1,z2}]]-
            ArcOrientation[z,radius,{z1,z2}]*l2/ArcRadius[z,radius,{z1,z2}],
          2*Pi,Arg[w1-ArcCentre[z,radius,{z1,z2}]]]*)(**)
      Abs[ArcProject[{w1,w2},ArcCentre[z,radius,{z1,z2}]]-
          l1/ArcRadius[z,radius,{z1,z2}]-l2/ArcRadius[z,radius,{z1,z2}]]<
      arcConst(**),{Line[{xyCoords[
            ArcCentre[z,radius,{z1,z2}]+
              ArcRadius[z,radius,{z1,z2}]*
                Exp[I*(Arg[w1-ArcCentre[z,radius,{z1,z2}]]+
                        ArcOrientation[z,radius,{z1,z2}]*
                          l1/ArcRadius[z,radius,{z1,z2}])]],
          xyCoords[
            ArcCentre[z,radius,{z1,z2}]+
              ArcRadius[z,radius,{z1,z2}]*
                Exp[I*(Arg[w2-ArcCentre[z,radius,{z1,z2}]]-
                        ArcOrientation[z,radius,{z1,z2}]*
                          l2/ArcRadius[z,radius,{z1,z2}])]]}]},
    True,{Circle[xyCoords[z+ArcCentre[z,radius,{z1,z2}]],
        ArcRadius[z,radius,{z1,z2}],
        Sort[{Mod[
              Arg[w1-ArcCentre[z,radius,{z1,z2}]]+
                ArcOrientation[z,radius,{z1,z2}]*
                  l1/ArcRadius[z,radius,{z1,z2}],2*Pi,
              Which[ArcOrientation[z,radius,{z1,z2}]>0,
                Arg[radius*Sign[z1]-ArcCentre[z,radius,{z1,z2}]]-Pi/2,
                ArcOrientation[z,radius,{z1,z2}]<0,
                Arg[radius*Sign[z2]-ArcCentre[z,radius,{z1,z2}]]-Pi/2]],
            Mod[Arg[w2-ArcCentre[z,radius,{z1,z2}]]-
                ArcOrientation[z,radius,{z1,z2}]*
                  l2/ArcRadius[z,radius,{z1,z2}],2*Pi,
              Which[ArcOrientation[z,radius,{z1,z2}]>0,
                Arg[radius*Sign[z1]-ArcCentre[z,radius,{z1,z2}]]-Pi/2,
                ArcOrientation[z,radius,{z1,z2}]<0,
                Arg[radius*Sign[z2]-ArcCentre[z,radius,{z1,z2}]]-Pi/2]]},
          Less]]}]

CircleParams[triangulation_,v_,
    n_]:={triangulation[[v,centre]],
    triangulation[[v,r]],
    Map[triangulation[[triangulation[[v,
            neighbours,#]],centre]]&,
      Map[ConnectedNeighbours[triangulation,
              v][[#]]&,n,{-1}],{-1}]}

ExtraGap[triangulation_,v_,neighbour_,gap_]:=
  If[MemberQ[
        Map[triangulation[[v,
              neighbours,#]]&,
          ConnectedNeighbours[triangulation,v][[
            Xunder]]],neighbour],
      Max[0,gap-
          Abs[Apply[ArcDistance,
              Join[CircleParams[triangulation,v,
                  Xunder],{{Apply[Crossing,
                      CircleParams[triangulation,v,{Xunder,Xover}]],
                    triangulation[[v,r]]*
                      Sign[triangulation[[neighbour,
                            centre]]-
                          triangulation[[v,
                            centre]]]}}]]]],0]/;
    triangulation[[v,type]]=="X"
ExtraGap[triangulation_,v_,neighbour_,gap_]:=
  If[MemberQ[
        Map[triangulation[[v,
              neighbours,#]]&,
          ConnectedNeighbours[triangulation,
              v][[1]]],neighbour],
      Max[0,ExtraGap[triangulation,OtherEnd[triangulation,v,neighbour],v,gap]-
          Apply[ArcDistance,
            Join[CircleParams[triangulation,v,
                1],{Map[
                  triangulation[[v,r]]*
                      Sign[triangulation[[#,
                            centre]]-
                          triangulation[[v,
                            centre]]]&,
                  Map[triangulation[[v,
                        neighbours,#]]&,
                    ConnectedNeighbours[triangulation,
                        v][[1]]]]}]]],0]/;
    triangulation[[v,type]]=="e"

DefaultGap[triangulation_]:=
  
  Min[Table[
      If[triangulation[[v,
            type]]=="X",
        Map[Abs[Apply[ArcDistance,
                Join[CircleParams[triangulation,v,
                    Xunder],{{triangulation[[v,
                          r]]*
                        Sign[triangulation[[#,
                              centre]]-
                            triangulation[[v,
                              r]]],
                      Apply[Crossing,
                        CircleParams[triangulation,v,{Xunder,Xover}]]}}]]]&,
          Map[triangulation[[v,
                neighbours,#]]&,
            ConnectedNeighbours[triangulation,v][[
              Xunder]]]],Infinity],{v,
        Length[triangulation]}]]

GetGraphicsObjs[triangulation_,v_,
    graphicsParams_]:={Join[
        Apply[GetArc,
          Join[CircleParams[triangulation,v,
              Xunder],{{triangulation[[v,
                    r]]*
                  Sign[triangulation[[
                        triangulation[[v,neighbours,
                          ConnectedNeighbours[triangulation,
                              v][[Xunder,
                            1]]]],
                        centre]]-
                      triangulation[[v,
                        centre]]],
                Apply[Crossing,
                  CircleParams[triangulation,v,{Xunder,Xover}]]},{ExtraGap[
                  triangulation,
                  triangulation[[v,neighbours,
                    ConnectedNeighbours[triangulation,
                        v][[Xunder,
                      1]]]],v,
                  graphicsParams[[gapParam\
]]],
                graphicsParams[[gapParam\
]]}}]],
        Apply[GetArc,
          Join[CircleParams[triangulation,v,
              Xunder],{{Apply[Crossing,
                  CircleParams[triangulation,v,{Xunder,Xover}]],
                triangulation[[v,r]]*
                  Sign[triangulation[[triangulation\
[[v,neighbours,
                          ConnectedNeighbours[triangulation,
                              v][[Xunder,
                            2]]]],
                        centre]]-
                      triangulation[[v,
                        centre]]]},{graphicsParams\
[[gapParam]],
                ExtraGap[triangulation,
                  triangulation[[v,neighbours,
                    ConnectedNeighbours[triangulation,
                        v][[Xunder,
                      1]]]],v,
                  graphicsParams[[gapParam\
]]]}}]]],
      Apply[GetArc,
        Join[CircleParams[triangulation,v,
            Xover],{Map[
              triangulation[[v,r]]*
                  Sign[
                    triangulation[[#,
                        centre]]-
                      triangulation[[v,
                        centre]]]&,
              Map[triangulation[[v,
                    neighbours,#]]&,
                ConnectedNeighbours[triangulation,v][[
                  Xover]]]],
            Map[ExtraGap[triangulation,#,v,
                  graphicsParams[[
                    gapParam]]]&,
              Map[triangulation[[v,
                    neighbours,#]]&,
                ConnectedNeighbours[triangulation,v][[
                  Xover]]]]}]]}/;
    triangulation[[v,type]]=="X"
GetGraphicsObjs[triangulation_,v_,
    graphicsParams_]:={Apply[GetArc,
        Join[CircleParams[triangulation,v,
            1],{Map[triangulation[[v,r]]*
                  Sign[triangulation[[#,
                        centre]]-
                      triangulation[[v,
                        centre]]]&,
              Map[triangulation[[v,
                    neighbours,#]]&,
                ConnectedNeighbours[triangulation,
                    v][[1]]]],
            Map[ExtraGap[triangulation,#,v,
                  graphicsParams[[1]]]&,
              Map[triangulation[[v,
                    neighbours,#]]&,
                ConnectedNeighbours[triangulation,
                    v][[1]]]]}]]}/;
    triangulation[[v,type]]=="e"
GetGraphicsObjs[triangulation_,v_,graphicsParams_]:={}/;
    triangulation[[v,type]]=="f"

AddGraphicsObjs[triangulation_,graphicsParams_]:=
  AddField[triangulation,graphicsObjs,
    Table[GetGraphicsObjs[triangulation,v,graphicsParams],{v,
        Length[triangulation]}]]

Draw[triangulation_]:=
  Graphics[Flatten[
      FieldValues[triangulation,graphicsObjs]],{AspectRatio->1}]

(* Colours *)

colourList={RGBColor[0,0,0],RGBColor[1,0,0],RGBColor[0,1,0],RGBColor[1,1,0],
      RGBColor[0,0,1],RGBColor[0.5,0.25,0],RGBColor[1,0,1],
      RGBColor[1,0.5,0.5],RGBColor[1,0.5,0],RGBColor[0.5,0.5,0.5]};

AddColour[triangulation_,components_,colour_]:=
  Insert[Insert[triangulation,colour,
      Table[{components[[i,1]],
          graphicsObjs,components[[i,2]],
          1},{i,Length[components]}]],RGBColor[0,0,0],
    Table[{components[[i,1]],
        graphicsObjs,
        components[[i,2]],-1},{i,
        Length[components]}]]

ColourStrands[triangulation_,colouredStrands_]:=
  Fold[AddColour[#1,#2[[1]],#2\
[[2]]]&,triangulation,
    Transpose[{ListStrands[triangulation],
        Take[Fold[
            Insert[#1,#2[[2]],#2\
[[1]]]&,
            Select[colourList,
              FreeQ[If[Length[colouredStrands]==0,{},
                    Transpose[
                        colouredStrands][[2\
]]],#]&],colouredStrands],
          Length[ListStrands[triangulation]]]}]]

OuterFace="OuterFace";
(* Commented out by Dror: Gap="Gap"; *)
Colour="Colour";
StrandColour="StrandColour";

(* Dror: Add line and pd -> pd_PD *)
DrawPD[L_] := DrawPD[PD[L]]
DrawPD[L_,options_] := DrawPD[PD[L],options]
DrawPD[pd_PD]:=(t=AddPositionsBound[DefaultDirichlet[Triangulate[pd]]];
    t=PutInside[t,DefaultOuterFace[t]];t=Balance[t];
    t=AddGraphicsObjs[t,{DefaultGap[t]}];t=ColourStrands[t,{}];Draw[t])
DrawPD[pd_PD,options_]:=(optionsList=Map[Apply[List,#]&,options];
    t=AddPositionsBound[DefaultDirichlet[Triangulate[pd]]];
    t=PutInside[t,
        Which[Length[
              Select[optionsList,#[[1]]\
==OuterFace&]]\[Equal]0,DefaultOuterFace[t],
          Depth[Select[
                  optionsList,#[[1]]\
==OuterFace&][[1,2]]]\[Equal]1,
          Select[optionsList,#[[1]]\
==OuterFace&][[1,2]],True,
          GetOuterFace[t,
            Select[optionsList,#[[1]]\
==OuterFace&][[1,2]]]]];
    t=Balance[t];
    graphicsParams={If[
          Length[Select[
                optionsList,#[[1]]\
==Gap&]]\[Equal]0,DefaultGap[t],
          Select[optionsList,#[[1]]\
==Gap&][[1,2]]]};
    t=AddGraphicsObjs[t,graphicsParams];
    t=If[Length[
            Select[optionsList,#[[1]]\
==Colour&]]\[Equal]0,t,
        ColourStrands[t,
          If[Length[
                Select[optionsList,#[[1]]\
==StrandColour&]]\[Equal]0,{},
            MapAt[Position[
                    ListStrands[t],{#+Length[pd],1}][[1,
                  1]]&,
              Select[optionsList,#[[1]]\
==StrandColour&][[1,2]],
              Table[{1,i},{i,
                  Length[Select[
                        optionsList,#[[1\
]]==StrandColour&][[1,
                      2]]]}]]]]];Draw[t])

End[]; EndPackage[]

BeginPackage["KnotTheory`"]		(* Data *)

AllKnots::usage = "
  AllKnots[] return a list of all the named knots known to KnotTheory.m.
"

AllLinks::usage = "
  AllLinks[] return a list of all the named links known to KnotTheory.m.
"

DTCode;

Begin["`Private`"]

NumberOfKnots[0, Alternating] = 1
NumberOfKnots[1, Alternating] = 0
NumberOfKnots[2, Alternating] = 0
NumberOfKnots[3, Alternating] = 1
NumberOfKnots[4, Alternating] = 1
NumberOfKnots[5, Alternating] = 2
NumberOfKnots[6, Alternating] = 3
NumberOfKnots[7, Alternating] = 7
NumberOfKnots[8, Alternating] = 18
NumberOfKnots[9, Alternating] = 41
NumberOfKnots[10, Alternating] = 123
NumberOfKnots[11, Alternating] = 367
NumberOfKnots[12, Alternating] = 1288
NumberOfKnots[13, Alternating] = 4878
NumberOfKnots[14, Alternating] = 19536
NumberOfKnots[15, Alternating] = 85263
NumberOfKnots[16, Alternating] = 379799

NumberOfKnots[0, NonAlternating] = 0
NumberOfKnots[1, NonAlternating] = 0
NumberOfKnots[2, NonAlternating] = 0
NumberOfKnots[3, NonAlternating] = 0
NumberOfKnots[4, NonAlternating] = 0
NumberOfKnots[5, NonAlternating] = 0
NumberOfKnots[6, NonAlternating] = 0
NumberOfKnots[7, NonAlternating] = 0
NumberOfKnots[8, NonAlternating] = 3
NumberOfKnots[9, NonAlternating] = 8
NumberOfKnots[10, NonAlternating] = 42
NumberOfKnots[11, NonAlternating] = 185
NumberOfKnots[12, NonAlternating] = 888
NumberOfKnots[13, NonAlternating] = 5110
NumberOfKnots[14, NonAlternating] = 27436
NumberOfKnots[15, NonAlternating] = 168030
NumberOfKnots[16, NonAlternating] = 1008906

NumberOfKnots[n_] :=
  NumberOfKnots[n, Alternating] + NumberOfKnots[n, NonAlternating]

NumberOfLinks[2] = 1
NumberOfLinks[3] = 0
NumberOfLinks[4] = 1
NumberOfLinks[5] = 1
NumberOfLinks[6] = 6
NumberOfLinks[7] = 9
NumberOfLinks[8] = 29
NumberOfLinks[9] = 83
NumberOfLinks[10] = 287
NumberOfLinks[11] = 1007
NumberOfLinks[12] = 4276
NumberOfLinks[13] = 7539
NumberOfLinks[2, Alternating] = 1
NumberOfLinks[3, Alternating] = 0
NumberOfLinks[4, Alternating] = 1
NumberOfLinks[5, Alternating] = 1
NumberOfLinks[6, Alternating] = 5
NumberOfLinks[7, Alternating] = 7
NumberOfLinks[8, Alternating] = 21
NumberOfLinks[9, Alternating] = 55
NumberOfLinks[10, Alternating] = 174
NumberOfLinks[11, Alternating] = 548
NumberOfLinks[12, Alternating] = 2020
NumberOfLinks[2, NonAlternating] = 0
NumberOfLinks[3, NonAlternating] = 0
NumberOfLinks[4, NonAlternating] = 0
NumberOfLinks[5, NonAlternating] = 0
NumberOfLinks[6, NonAlternating] = 1
NumberOfLinks[7, NonAlternating] = 2
NumberOfLinks[8, NonAlternating] = 8
NumberOfLinks[9, NonAlternating] = 28
NumberOfLinks[10, NonAlternating] = 113
NumberOfLinks[11, NonAlternating] = 459
NumberOfLinks[12, NonAlternating] = 2256

(* These are ordered lists for the purpose of data loading! Do not mess! *)
AllKnots[] = Flatten[{
  Table[Knot[n,k], {n,0,10}, {k,NumberOfKnots[n]}],
  Table[Knot[11, Alternating, k], {k, NumberOfKnots[11, Alternating]}],
  Table[Knot[11, NonAlternating, k], {k, NumberOfKnots[11, NonAlternating]}]
}]
AllLinks[] = Flatten[Table[{
  Table[Link[n, Alternating, k], {k,NumberOfLinks[n, Alternating]}],
  Table[Link[n, NonAlternating, k], {k,NumberOfLinks[n, NonAlternating]}]
}, {n,2,11}]]

PD[Knot[n_, k_]] := (
  Needs["KnotTheory`PD4Knots`"];
  Unset[PD[Knot[n1_, k1_]]];
  PD[Knot[n, k]]
)

DTCode[Knot[n_, t_, k_]] /; (n<=11) := (
  Needs["KnotTheory`DTCode4KnotsTo11`"];
  Unset[DTCode[Knot[n1_, t1_, k1_]] /; (n1<=11)];
  DTCode[Knot[n, t, k]]
)

PD[Knot[n_, t_, k_]] := PD[DTCode[Knot[n, t, k]]]

PD[Link[n_, t_, k_]] := (
  Needs["KnotTheory`PD4Links`"];
  Unset[PD[Link[n1_, t1_, k1_]]];
  PD[Link[n, t, k]]
)

DT4Knots[n_, t_] /; (12<=n<=16) := DT4Knots[n, t] = Module[
  {ts, fn, f},
  ts = t /. {Alternating -> "A", NonAlternating -> "N"};
  fn = "KnotTheory/"<>ToString[n]<>ts<>".dts";
  Message[KnotTheory::loading, fn];
  f = OpenRead[fn];
  Table[
    StringTake[Read[f, String], n],
    {NumberOfKnots[n, t]}
  ]
]

DTCode[Knot[n_, t_, k_]] /; (12<=n<=16) := DTCode @@ (
  If[# >= 97, 2(#-96), -2(#-64)]& /@ ToCharacterCode[DT4Knots[n, t][[k]]] 
)

End[]; EndPackage[]
BeginPackage["KnotTheory`"]             (* Braid Data *)

BR;

Begin["`Private`"]

BR[Knot[0,1]] = BR[1, ""]
BR[Knot[3, 1]] = BR[2, "AAA"]
BR[Knot[4, 1]] = BR[3, "AbAb"]
BR[Knot[5, 1]] = BR[2, "AAAAA"]
BR[Knot[5, 2]] = BR[3, "AAABaB"]
BR[Knot[6, 1]] = BR[4, "AABacBc"]
BR[Knot[6, 2]] = BR[3, "AAAbAb"]
BR[Knot[6, 3]] = BR[3, "AAbAbb"]
BR[Knot[7, 1]] = BR[2, "AAAAAAA"]
BR[Knot[7, 2]] = BR[4, "AAABaBCbC"]
BR[Knot[7, 3]] = BR[3, "aaaaabAb"]
BR[Knot[7, 4]] = BR[4, "aabAbbcBc"]
BR[Knot[7, 5]] = BR[3, "AAAABaBB"]
BR[Knot[7, 6]] = BR[4, "AAbACbC"]
BR[Knot[7, 7]] = BR[4, "aBaBcBc"]
BR[Knot[8, 1]] = BR[5, "AABaBCbdCd"]
BR[Knot[8, 2]] = BR[3, "AAAAAbAb"]
BR[Knot[8, 3]] = BR[5, "AABacBcdCd"]
BR[Knot[8, 4]] = BR[4, "AAAbAbcBc"]
BR[Knot[8, 5]] = BR[3, "aaaBaaaB"]
BR[Knot[8, 6]] = BR[4, "AAAABacBc"]
BR[Knot[8, 7]] = BR[3, "aaaaBaBB"]
BR[Knot[8, 8]] = BR[4, "aaabACbCC"]
BR[Knot[8, 9]] = BR[3, "AAAbAbbb"]
BR[Knot[8, 10]] = BR[3, "aaaBaaBB"]
BR[Knot[8, 11]] = BR[4, "AABaBBcBc"]
BR[Knot[8, 12]] = BR[5, "AbACbdCd"]
BR[Knot[8, 13]] = BR[4, "AAbAbbcBc"]
BR[Knot[8, 14]] = BR[4, "AAABaBcBc"]
BR[Knot[8, 15]] = BR[4, "AAbACBBBC"]
BR[Knot[8, 16]] = BR[3, "AAbAAbAb"]
BR[Knot[8, 17]] = BR[3, "AAbAbAbb"]
BR[Knot[8, 18]] = BR[3, "AbAbAbAb"]
BR[Knot[8, 19]] = BR[3, "aaabaaab"]
BR[Knot[8, 20]] = BR[3, "aaaBAAAB"]
BR[Knot[8, 21]] = BR[3, "AAABaaBB"]
BR[Knot[9, 1]] = BR[2, "AAAAAAAAA"]
BR[Knot[9, 2]] = BR[5, "AAABaBCbCDcD"]
BR[Knot[9, 3]] = BR[3, "aaaaaaabAb"]
BR[Knot[9, 4]] = BR[4, "AAAAABaBCbC"]
BR[Knot[9, 5]] = BR[5, "aabAbbcBcdCd"]
BR[Knot[9, 6]] = BR[3, "AAAAAABaBB"]
BR[Knot[9, 7]] = BR[4, "AAAABaBCbCC"]
BR[Knot[9, 8]] = BR[5, "AAbAbcBDcD"]
BR[Knot[9, 9]] = BR[3, "AAAAABaBBB"]
BR[Knot[9, 10]] = BR[4, "aabAbbbbcBc"]
BR[Knot[9, 11]] = BR[4, "aaaaBacBc"]
BR[Knot[9, 12]] = BR[5, "AAbACbCDcD"]
BR[Knot[9, 13]] = BR[4, "aaaabAbbcBc"]
BR[Knot[9, 14]] = BR[5, "aabACbCdCd"]
BR[Knot[9, 15]] = BR[5, "aaabACbdCd"]
BR[Knot[9, 16]] = BR[3, "aaaabbAbbb"]
BR[Knot[9, 17]] = BR[4, "aBaBBBcBc"]
BR[Knot[9, 18]] = BR[4, "AAABaBBBCbC"]
BR[Knot[9, 19]] = BR[5, "aBaBBCbdCd"]
BR[Knot[9, 20]] = BR[4, "AAAbACbCC"]
BR[Knot[9, 21]] = BR[5, "aabAbCbdCd"]
BR[Knot[9, 22]] = BR[4, "AbAbCbbbC"]
BR[Knot[9, 23]] = BR[4, "AAABaBBCbCC"]
BR[Knot[9, 24]] = BR[4, "AAbACbbbC"]
BR[Knot[9, 25]] = BR[5, "AAbACBBdCd"]
BR[Knot[9, 26]] = BR[4, "aaaBaBcBc"]
BR[Knot[9, 27]] = BR[4, "AAbAbbCbC"]
BR[Knot[9, 28]] = BR[4, "AAbACbbCC"]
BR[Knot[9, 29]] = BR[4, "aBBcBaBcB"]
BR[Knot[9, 30]] = BR[4, "AAbbAbCbC"]
BR[Knot[9, 31]] = BR[4, "AAbAbCbCC"]
BR[Knot[9, 32]] = BR[4, "aaBaBacBc"]
BR[Knot[9, 33]] = BR[4, "AbAbbACbC"]
BR[Knot[9, 34]] = BR[4, "AbAbCbAbC"]
BR[Knot[9, 35]] = BR[5, "AABaBBCbbDcBDC"]
BR[Knot[9, 36]] = BR[4, "aaaBaacBc"]
BR[Knot[9, 37]] = BR[5, "AAbACbadCbCd"]
BR[Knot[9, 38]] = BR[4, "AABBcBaBCCB"]
BR[Knot[9, 39]] = BR[5, "aabACBadcBcd"]
BR[Knot[9, 40]] = BR[4, "AbACbACbC"]
BR[Knot[9, 41]] = BR[5, "AABacbbDCbCD"]
BR[Knot[9, 42]] = BR[4, "aaaBAAcBc"]
BR[Knot[9, 43]] = BR[4, "aaabaaCbC"]
BR[Knot[9, 44]] = BR[4, "AAABaacBc"]
BR[Knot[9, 45]] = BR[4, "AABaBACbC"]
BR[Knot[9, 46]] = BR[4, "AbAbCBaBC"]
BR[Knot[9, 47]] = BR[4, "AbAbcbAbc"]
BR[Knot[9, 48]] = BR[4, "aabAbaCbAbC"]
BR[Knot[9, 49]] = BR[4, "aabaaCbAbcc"]
BR[Knot[10, 1]] = BR[6, "AABaBCbCDceDe"]
BR[Knot[10, 2]] = BR[3, "AAAAAAAbAb"]
BR[Knot[10, 3]] = BR[6, "AABaBCbdCdeDe"]
BR[Knot[10, 4]] = BR[5, "AAAbAbcBcdCd"]
BR[Knot[10, 5]] = BR[3, "aaaaaaBaBB"]
BR[Knot[10, 6]] = BR[4, "AAAAAABacBc"]
BR[Knot[10, 7]] = BR[5, "AABaBCbCCdCd"]
BR[Knot[10, 8]] = BR[4, "AAAAAbAbcBc"]
BR[Knot[10, 9]] = BR[3, "aaaaaBaBBB"]
BR[Knot[10, 10]] = BR[5, "AAbAbbcBcdCd"]
BR[Knot[10, 11]] = BR[5, "AAAABacBcdCd"]
BR[Knot[10, 12]] = BR[4, "aaaaabACbCC"]
BR[Knot[10, 13]] = BR[6, "AABacBDceDe"]
BR[Knot[10, 14]] = BR[4, "AAAAABaBcBc"]
BR[Knot[10, 15]] = BR[4, "aaaaBaBCbCC"]
BR[Knot[10, 16]] = BR[5, "aabAbbCbCDcD"]
BR[Knot[10, 17]] = BR[3, "AAAAbAbbbb"]
BR[Knot[10, 18]] = BR[5, "AAABaBcBcdCd"]
BR[Knot[10, 19]] = BR[4, "AAAAbAbbcBc"]
BR[Knot[10, 20]] = BR[5, "AAAABaBCbdCd"]
BR[Knot[10, 21]] = BR[4, "AABaBBBBcBc"]
BR[Knot[10, 22]] = BR[4, "aaaabACbCCC"]
BR[Knot[10, 23]] = BR[4, "AAbAbbbbcBc"]
BR[Knot[10, 24]] = BR[5, "AABaBBBCbdCd"]
BR[Knot[10, 25]] = BR[4, "AAAABaBBcBc"]
BR[Knot[10, 26]] = BR[4, "AAAbAbbbcBc"]
BR[Knot[10, 27]] = BR[4, "AAAABaBcBcc"]
BR[Knot[10, 28]] = BR[5, "aabAbbcBDcDD"]
BR[Knot[10, 29]] = BR[5, "AAAbACbdCd"]
BR[Knot[10, 30]] = BR[5, "AABaBBCbCdCd"]
BR[Knot[10, 31]] = BR[5, "AAABacBccdCd"]
BR[Knot[10, 32]] = BR[4, "aaaBaBBCbCC"]
BR[Knot[10, 33]] = BR[5, "AABaBcBccdCd"]
BR[Knot[10, 34]] = BR[5, "aaabAbcBDcDD"]
BR[Knot[10, 35]] = BR[6, "AbAbcBDceDe"]
BR[Knot[10, 36]] = BR[5, "AAABaBCbCdCd"]
BR[Knot[10, 37]] = BR[5, "AAABacBcdCdd"]
BR[Knot[10, 38]] = BR[5, "AAABaBBCbdCd"]
BR[Knot[10, 39]] = BR[4, "AAABaBBBcBc"]
BR[Knot[10, 40]] = BR[4, "aaabAbbCbCC"]
BR[Knot[10, 41]] = BR[5, "aBaBBcBDcD"]
BR[Knot[10, 42]] = BR[5, "AAbAbCbdCd"]
BR[Knot[10, 43]] = BR[5, "AAbACbdCdd"]
BR[Knot[10, 44]] = BR[5, "AAbACbCdCd"]
BR[Knot[10, 45]] = BR[5, "AbAbCbCdCd"]
BR[Knot[10, 46]] = BR[3, "aaaaaBaaaB"]
BR[Knot[10, 47]] = BR[3, "aaaaaBaaBB"]
BR[Knot[10, 48]] = BR[3, "AAAAbbAbbb"]
BR[Knot[10, 49]] = BR[4, "AAAAbACBBBC"]
BR[Knot[10, 50]] = BR[4, "aabAbbCbbbC"]
BR[Knot[10, 51]] = BR[4, "aabAbbCbbCC"]
BR[Knot[10, 52]] = BR[4, "aaaBaaBBCbC"]
BR[Knot[10, 53]] = BR[5, "AABaBcBDCCCD"]
BR[Knot[10, 54]] = BR[4, "aaaBaaBCbCC"]
BR[Knot[10, 55]] = BR[5, "AAABacBDCCCD"]
BR[Knot[10, 56]] = BR[4, "aaabAbCbbbC"]
BR[Knot[10, 57]] = BR[4, "aaabAbCbbCC"]
BR[Knot[10, 58]] = BR[6, "aBacBDCCeDe"]
BR[Knot[10, 59]] = BR[5, "AbAbCbbdCd"]
BR[Knot[10, 60]] = BR[5, "AbAbbCbCBDcD"]
BR[Knot[10, 61]] = BR[4, "aaaBaaaBCbC"]
BR[Knot[10, 62]] = BR[3, "aaaaBaaaBB"]
BR[Knot[10, 63]] = BR[5, "AAbACBBBCDcD"]
BR[Knot[10, 64]] = BR[3, "aaaBaaaBBB"]
BR[Knot[10, 65]] = BR[4, "aabAbCbbbCC"]
BR[Knot[10, 66]] = BR[4, "AAAbACBBBCC"]
BR[Knot[10, 67]] = BR[5, "AAABaBCbbdCBdC"]
BR[Knot[10, 68]] = BR[5, "aaBaBBCbbDcBDC"]
BR[Knot[10, 69]] = BR[5, "aabACbadCbCd"]
BR[Knot[10, 70]] = BR[5, "AbACbbbdCd"]
BR[Knot[10, 71]] = BR[5, "AAbACbbdCd"]
BR[Knot[10, 72]] = BR[4, "aaaabbAbCbC"]
BR[Knot[10, 73]] = BR[5, "AABaBAcBcDcD"]
BR[Knot[10, 74]] = BR[5, "AABaBBCbbdCBdC"]
BR[Knot[10, 75]] = BR[5, "aBaBcBBdCbdc"]
BR[Knot[10, 76]] = BR[4, "aaaabACbbbC"]
BR[Knot[10, 77]] = BR[4, "aaaabACbbCC"]
BR[Knot[10, 78]] = BR[5, "AABaBAcBDcDD"]
BR[Knot[10, 79]] = BR[3, "AAAbbAAbbb"]
BR[Knot[10, 80]] = BR[4, "AAAbAACBBBC"]
BR[Knot[10, 81]] = BR[5, "aaBacbbDCCCD"]
BR[Knot[10, 82]] = BR[3, "AAAAbAbAbb"]
BR[Knot[10, 83]] = BR[4, "aabAbCbbCbC"]
BR[Knot[10, 84]] = BR[4, "aaabACbbCbC"]
BR[Knot[10, 85]] = BR[3, "AAAAbAAbAb"]
BR[Knot[10, 86]] = BR[4, "AAbAbAbbcBc"]
BR[Knot[10, 87]] = BR[4, "aaabACbCbCC"]
BR[Knot[10, 88]] = BR[5, "AbACbCbdCd"]
BR[Knot[10, 89]] = BR[5, "AbAbcBADCbCD"]
BR[Knot[10, 90]] = BR[4, "AAbAbcBAcbb"]
BR[Knot[10, 91]] = BR[3, "AAAbAbbAbb"]
BR[Knot[10, 92]] = BR[4, "aaabbCbAbCb"]
BR[Knot[10, 93]] = BR[4, "AAbAAbAbcBc"]
BR[Knot[10, 94]] = BR[3, "aaaBaaBBaB"]
BR[Knot[10, 95]] = BR[4, "AAbbCbAbccb"]
BR[Knot[10, 96]] = BR[5, "AbaCbaCdCbCd"]
BR[Knot[10, 97]] = BR[5, "aabAbaCbAbcDcD"]
BR[Knot[10, 98]] = BR[4, "AABBcBaBBcB"]
BR[Knot[10, 99]] = BR[3, "AAbAAbbAbb"]
BR[Knot[10, 100]] = BR[3, "AAAbAAbAAb"]
BR[Knot[10, 101]] = BR[5, "aaabAcBacbbdCd"]
BR[Knot[10, 102]] = BR[4, "AAbACbAbbcc"]
BR[Knot[10, 103]] = BR[4, "AABacBBcBBc"]
BR[Knot[10, 104]] = BR[3, "AAAbbAbAbb"]
BR[Knot[10, 105]] = BR[5, "aaBacbbDCbCD"]
BR[Knot[10, 106]] = BR[3, "aaaBaBaaBB"]
BR[Knot[10, 107]] = BR[5, "AAbAcbbDcBcD"]
BR[Knot[10, 108]] = BR[4, "aaBaacBaBCC"]
BR[Knot[10, 109]] = BR[3, "AAbAbbAAbb"]
BR[Knot[10, 110]] = BR[5, "AbACBBBdcBcd"]
BR[Knot[10, 111]] = BR[4, "aabbCbbAbCb"]
BR[Knot[10, 112]] = BR[3, "AAAbAbAbAb"]
BR[Knot[10, 113]] = BR[4, "aaabCbAbCbC"]
BR[Knot[10, 114]] = BR[4, "AABacBcBcBc"]
BR[Knot[10, 115]] = BR[5, "aBacbbDCbCCD"]
BR[Knot[10, 116]] = BR[3, "AAbAAbAbAb"]
BR[Knot[10, 117]] = BR[4, "aabbCbAbCbC"]
BR[Knot[10, 118]] = BR[3, "aaBaBaBBaB"]
BR[Knot[10, 119]] = BR[4, "AAbACbAbccb"]
BR[Knot[10, 120]] = BR[5, "AABacbADCBBCCD"]
BR[Knot[10, 121]] = BR[4, "AABcBaBcBcB"]
BR[Knot[10, 122]] = BR[4, "aabCbACbCbC"]
BR[Knot[10, 123]] = BR[3, "AbAbAbAbAb"]
BR[Knot[10, 124]] = BR[3, "aaaaabaaab"]
BR[Knot[10, 125]] = BR[3, "aaaaaBAAAB"]
BR[Knot[10, 126]] = BR[3, "AAAAABaaaB"]
BR[Knot[10, 127]] = BR[3, "AAAAABaaBB"]
BR[Knot[10, 128]] = BR[4, "aaabaabbcBc"]
BR[Knot[10, 129]] = BR[4, "aaaBAAcBAcB"]
BR[Knot[10, 130]] = BR[4, "aaaBAABBCbC"]
BR[Knot[10, 131]] = BR[4, "AAABaaBBCbC"]
BR[Knot[10, 132]] = BR[4, "aaaBAABCbCC"]
BR[Knot[10, 133]] = BR[4, "AAABaaBCbCC"]
BR[Knot[10, 134]] = BR[4, "aaabaabcBcc"]
BR[Knot[10, 135]] = BR[4, "aaabAbCBBBC"]
BR[Knot[10, 136]] = BR[5, "aBaBCbbdCd"]
BR[Knot[10, 137]] = BR[5, "AbAbCBBdCd"]
BR[Knot[10, 138]] = BR[5, "AbAbcbbDcD"]
BR[Knot[10, 139]] = BR[3, "aaaabaaabb"]
BR[Knot[10, 140]] = BR[4, "aaaBAAABCbC"]
BR[Knot[10, 141]] = BR[3, "aaaaBAAABB"]
BR[Knot[10, 142]] = BR[4, "aaabaaabcBc"]
BR[Knot[10, 143]] = BR[3, "AAAABaaaBB"]
BR[Knot[10, 144]] = BR[4, "AABaBAcBAcb"]
BR[Knot[10, 145]] = BR[4, "AABaBACBaBC"]
BR[Knot[10, 146]] = BR[4, "AAbAbaCbAbC"]
BR[Knot[10, 147]] = BR[4, "aaaBaBCbAbC"]
BR[Knot[10, 148]] = BR[3, "AAAABaaBaB"]
BR[Knot[10, 149]] = BR[3, "AAAABaBaBB"]
BR[Knot[10, 150]] = BR[4, "aaaBaacBAcb"]
BR[Knot[10, 151]] = BR[4, "aaabAAcBacB"]
BR[Knot[10, 152]] = BR[3, "AAABBAABBB"]
BR[Knot[10, 153]] = BR[4, "AAABAAcbbbc"]
BR[Knot[10, 154]] = BR[4, "aabAbacbbbc"]
BR[Knot[10, 155]] = BR[3, "aaabAAbAAb"]
BR[Knot[10, 156]] = BR[4, "AAAbaaCBaBC"]
BR[Knot[10, 157]] = BR[3, "aaabbAbAbb"]
BR[Knot[10, 158]] = BR[4, "AAABaacbAbc"]
BR[Knot[10, 159]] = BR[3, "AAABaBaaBB"]
BR[Knot[10, 160]] = BR[4, "aaabaaCbAbC"]
BR[Knot[10, 161]] = BR[3, "AAABaBAABB"]
BR[Knot[10, 162]] = BR[4, "AABaaBBAcBc"]
BR[Knot[10, 163]] = BR[4, "aaBAAcbAbbc"]
BR[Knot[10, 164]] = BR[4, "aaBaBBCbAbC"]
BR[Knot[10, 165]] = BR[4, "aabACbAbccb"]

End[]; EndPackage[]
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

Begin["`GaussCode`"]

GaussCode[K_] /; !MatchQ[Head[K], PD|DTCode|List] := GaussCode[PD[K]]
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
DTCode[K_] /; Head[K] =!= GaussCode := DTCode[GaussCode[K]]

End[]; EndPackage[]
BeginPackage["KnotTheory`"]

PD::about = "
  The PD to GaussCode and to MorseLink conversions were written by
  Siddarth Sankaran at the University of Toronto in the summer of
  2005.
"

Begin["`GaussCode`"]

PD[GaussCode[]] = PD[Loop[1]]

PD[in_GaussCode] := 
    Module[ {chords=List@@in,
        int = Range[Max[List@@in]] /. x_Integer \[Rule] {}, 
        dirlist = Table[0, {Max[List@@in]}], edgelist, output={}, ol={{}} },
      
      If[AtomQ[chords[[1]] ], 
        chords = {chords} ]; (*make a knot into a 1-
          component link for consistency *)
      
      (*compile edgelist *)
      Module[ {k, c=0},
        For[k = 1, k\[LessEqual] Length[chords], k++, 
          AppendTo[ol,Range[++c, c+=  Length[chords[[k]] ] -1]  ]  ;
          ];
        edgelist = ol = Delete[ol,1];
        ];
      
      (* relax the knot, so we get a one component, 
        and reverse the direction of travesal along each relaxation *)
      
      Module[ {c1, c2, k, temp, j, p1, p2, etemp},
        For[k = 1, k\[LessEqual] Max[chords], 
          k++, (*relax crossing by crossing *)
          temp = chords; 
          etemp = edgelist;
          {c1, c2} = {Position[chords, -k], Position[chords,k]};
          
          If[c1[[1,1]] \[Equal] 
              c2[[1,1]], (*same component *)
            {p1, 
                p2} = {Min[c1[[1,2]], c2[[1,2]] ], 
                Max[c1[[1,2]], c2[[1,2]] ] };
            c1 = c2 = c1[[1,1]];
            
            For[j=1, j< p2 - p1, j++, 
              chords[[c1, p1 + j]] = temp[[c1, p2 -j]]; 
              edgelist[[c1, p1+j]] = etemp[[c1,p2+1 - j]] ];
            edgelist[[c1,p2]] = etemp[[c1, p1+1]];
            , (*different components, 
              relaxation combines them *)
            {p1,p2} = {c1[[1,2]], 
                c2[[1,2]]};{c1,c2} = {c1[[1,1]], c2[[1,1]]};
            
            chords[[c1]] = 
              Flatten[Insert[temp[[c1]], 
                  RotateRight[Reverse[temp[[c2]]], p2-1], p1+1]];
            chords = Delete[chords, c2];
            
            edgelist[[c1]] = 
              Flatten[Insert[etemp[[c1]], 
                  RotateRight[Reverse[etemp[[c2]]], p2], p1+1]];
            edgelist = Delete[edgelist, c2];
            ];
          ];
        chords = Flatten[chords];
        AppendTo[edgelist[[1]], First[edgelist[[1]]]];
        edgelist = Partition[Flatten[edgelist], 2, 1];
        ];
      
      (* compile a list of which chords intersect: 
              int[k] = {list of crossings whose chords intersect crossing k} *)

            Module[ {k,j,a,b},
        For[k = 1, k \[LessEqual] Max[chords], k++,
            {a,b} = Flatten[Position[Abs[chords], k] ];
            For[j = 1, j \[LessEqual] Max[chords], j++,
              
              If[Count[Take[Abs[chords], {a+1, b-1}], j] \[Equal] 1, 
                  AppendTo[int[[k]],j] ];
              ];
            ];
        ];
       
      (*arrange dirlist so intersecting chords have opposite dirs *)
      
      Module[{s, l, mirror, p,d,ch},
        s[1] = -1;s[-1] = 1; s[0]=0;
        dirlist[[1]] = 
          If[Head[in[[1]]]=== Integer || Length[in] \[Equal] 1, -1,
            1]; (*1st edge up *)
        
        mirror = l = Table[{dirlist[[i]], i}, {i, Length[dirlist]}];
        
        l = l //. {x_Integer, i_Integer}/;x\[Equal] 0 \[RuleDelayed] (
                d = Table[ mirror[[n,1]], {n, Length[l]}];
                p = Position[ Abs[ d[[ int[[i]] ]] ] , 1];
                
                
                If[ Length[p] \[NotEqual] 0, ch = int[[i, p[[1,1]] ]]; 
                  mirror[[i]] = {s[d[[ ch ]] ],i};{s[d[[ch]] ],i},
                  mirror[[i]] = {0,i};{0,i}]  );
        
        dirlist = l /. {x_, y_} \[Rule] x;
        
        ];
      
      (* compile output from edgelist *)
      
      Module[ {k,p1,p2,a,b,x,y, inunder,l},
        For[k=1, k \[LessEqual] Max[chords], k++,
            {{x,y}} = 
              If[AtomQ[List@@in[[1]] ], Position[{List@@in}, -k], 
                Position[List@@in, -k]];
            inunder = ol[[x,y]];
            {p1, p2} = {Position[chords, -k][[1,1]], 
                Position[chords,k][[1,1]]};
            {{x,y}, {a,b}} = {edgelist[[p1]],edgelist[[p2]]};
            
            l=If[dirlist[[k]] \[Equal] 1, {x,b,a,y},{x,y,a,
                  b}];  (*in right or in left*) 
            l = RotateLeft[l, Position[l, inunder][[1,1]] -1 ];
            AppendTo[output, Apply[X, l]];
            ];
        ];
      output[[0]] = PD;
      Return[output];
      ] ;
 
PD[dt_DTCode] := PD[GaussCode[dt]]

End[]; EndPackage[]
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
BeginPackage["KnotTheory`"]

ColouredJones::usage = "
ColouredJones[br, n][q] computes the coloured Jones polynomial of the
closure of the braid br in colour n (i.e., in the (n+1)-dimensional 
representation) and with respect to the variable q. ColouredJones[K, n][q]
does the same for knots for which a braid representative is known to this
program.
"

ColouredJones::about = "
The ColouredJones program was written jointly with Stavros Garoufalidis,
based on formulas provided to us by Thang Le.
"

ColoredJones::usage = "
Type ColoredJones and see for yourself.
"

CJ`Summand::usage = "
CJ`Summand[br, n] returned a pair {s, vars} where s is the summand in the 
the big sum that makes up ColouredJones[br, n][q] and where vars is the
list of variables that need to be summed over (from 0 to n) to get
ColouredJones[br, n][q]. CJ`Summand[K, n] is the same for knots for
which a braid representative is known to this program.
"

qPochhammer::usage = "
qPochhammer[a, q, k] represents the q-shifted factorial of a in base
q with index k. See Eric Weisstein's\n
http://mathworld.wolfram.com/q-PochhammerSymbol.html
and Axel Riese's\n
www.risc.uni-linz.ac.at/research/combinat/risc/software/qMultiSum/
"

qBinomial::usage = "
qBinomial[n, k, q] represents the q-binomial coefficient of n and k in base
q. For k<0 it is 0; otherwise it is\n
qPochhammer[q^(n-k+1), q, k] / qPochhammer[q, q, k].
"

qExpand::usage = "
qExpand[expr_] replaces all occurences of qPochhammer and qBinomial in
expr by their definitions as products. See the documentation for
qPochhammer and for qBinomial for details.
"

CJ`k; CJ`q; NotAvailable; Compute

Begin["`CJ`"]

ColoredJones = ColouredJones;

ColouredJones[Knot[n_, k_], nn_] := (
  Needs["KnotTheory`ColouredJones4Knots`"];
  Unset[ColouredJones[Knot[n1_, k1_], nn1_]];
  ColouredJones[Knot[n, k], nn]
)

ColouredJones[TorusKnot[m_, n_], nn_] := (
  Needs["KnotTheory`ColouredJones4TorusKnots`"];
  Unset[ColouredJones[TorusKnot[m1_, n1_], nn1_]];
  ColouredJones[TorusKnot[m, n], nn]
)

qExpand[expr_] := expr /. {
  qBinomial[n_, k_Integer, q_] :> qBin[n, k, q],
  qPochhammer[a_, q_, k_Integer] :> qPoc[a, q, k]
}

qPoc[a_, q_, k_Integer] /; k > 0 := qPoc[a,q,k] =
  Simplify[Product[(1 - a q^i), {i, 0, k - 1}]];
qPoc[a_, q_, 0] = 1;
qPoc[a_, q_, k_Integer] /; k < 0 := qPoc[a,q,k] =
  Simplify[Product[1/(1 - a q^(-i)), {i, 1, -k}]];

qBin[n_, k_Integer, q_] /; k >= 0 := qBin[n,k,q] =
  Simplify[qPoc[q^(n - k + 1), q, k]/qPoc[q, q, k]];
qBin[n_, k_Integer, q_] /; k < 0 := qBin[n,k,q] = 0;

CJ`Summand[K_, n_] /; Head[K]=!=BR := CJ`Summand[BR[K], n]
CJ`Summand[BR[s_, l_List], n_] := Module[
  {i, eqns, v, vars, sol, nulls, a = Range[s], m = s, j, B, summand},
  B = Times[
    Times @@ (l  /. {
      j_Integer /; j>0 :> 
        Xp[a[[j]], a[[j + 1]], a[[j]] = ++m, a[[j + 1]] = ++m],
      j_Integer /; j<0 :>
        Xm[a[[-j]], a[[1 - j]], a[[-j]] = ++m, a[[1 - j]] = ++m]
    }),
    Product[cl[a[[j]], j], {j, 2, s}],
    bt[1]*tp[a[[1]]]
  ];
  i = 0;
  eqns = Flatten[
    (List @@ B) /. { 
      Xp[a_, b_, c_, d_] :> {v[a] - k[++i] == v[d], v[b] + k[i] == v[c]},
      Xm[a_, b_, c_, d_] :> {v[a] + k[++i] == v[d], v[b] - k[i] == v[c]},
      cl[a_, b_] :> {v[a] == v[b]},
      (bt | tp)[a_] :> {v[a] == 0}
    }
  ];
  vars = v /@ (Union @@ Apply[List, List @@ B, {1}]);
  sol = First@Solve[eqns, vars];
  nulls = Union[Cases[
    Cases[
      Last /@ sol, 
      HoldPattern[Times[-1, _] | Plus[Times[-1, _] ..]]
    ],
    _k, Infinity
  ]];
  i = 0;
  summand = B /. {
    Xp[a_, b_, _, _] :> (
      ++i;
      q^(n/2)*q^(v[b](k[i] - v[a]))*qBinomial[v[a], k[i], 1/q]*
        q^(v[b]*n)*qPochhammer[q^(n - v[b]), 1/q, k[i]]
    ),
    Xm[a_, b_, _, _] :> (
       ++i;
       q^(-n/2)*q^(-v[a](k[i] - v[b]))*qBinomial[v[b], k[i], q]*
         q^(-v[a]*n)*qPochhammer[q^(-n + v[a]), q, k[i]]
    ),
    cl[a_, b_] :> q^((2v[a] - n)/2),
    (_bt) | (_tp) :> 1
  } /. sol /. ((# -> 0) & /@ nulls);
  vars = Union[Cases[summand, _k, Infinity]];
  {summand /. q -> CJ`q, vars} /. Thread[Rule[vars, Array[CJ`k, Length[vars]]]]
]

Options[ColouredJones] = {Compute -> True};
ColouredJones[K_, n_Integer, opts___] /; Head[K]=!=BR :=
  ColouredJones[BR[K], n, opts]
ColouredJones[b_BR, n_Integer, opts___] := ColouredJones[b, n] = Module[
  {
    compute = Compute /. {opts} /. Options[ColouredJones],
    s1, vars1, s2, vars2, s, vars, rule, nv, out = 0, jj
  },
  If[!compute, NotAvailable,
    {s1, vars1} = CJ`Summand[b, n];
    {s2, vars2} = CJ`Summand[Mirror[b], n];
    If[Length[vars1] <= Length[vars2],
      {vars, s} = {vars1, s1}; rule = {CJ`q -> #},
      {vars, s} = {vars2, s2}; rule = {CJ`q -> 1/#}
    ];
    s = Simplify[qExpand[s]];
    nv = Length[vars];
    Do[
      out += Expand[qExpand[
        s /. Thread[Rule[vars, IntegerDigits[jj, n + 1, nv]]]
      ]],
      {jj, (n + 1)^nv}
    ];
    Function @@ {Expand[Simplify[out /. rule]]}
  ]
]

End[]; EndPackage[];
BeginPackage["KnotTheory`"]

HOMFLYPT::usage = "
HOMFLYPT[K][a, z] computes the HOMFLY-PT (Hoste, Ocneanu, Millett,
Freyd, Lickorish, Yetter, Przytycki and Traczyk) polynomial of a
knot/link K, in the variables a and z.
"

HOMFLYPT::about = "
The HOMFLYPT program was written by Scott Morrison.
"

Begin["`HOMFLYPT`"]

CrossingSignList[pd_PD]:=
  List @@ pd /. X[i_,j_,k_,l_] :> If[j-l == 1 || l-j>1, +1, -1]

SignedGaussCode[K_] /; Head[K]=!=PD && Head[K] =!= List :=
  SignedGaussCode[PD[K]]

SignedGaussCode[pd_PD] := Module[
  {csl=CrossingSignList[pd],sgc},
  sgc=GaussCode[pd]/.n_Integer :> {n, csl[[Abs[n]]]};
  If[Depth[sgc] == 3,SignedGaussCode[List@@sgc],SignedGaussCode@@sgc]
]

StateValuation[a_, z_][s_State] := Times[
  (-1)^Count[s, {_, -1, bullet}, 2],
  z^Count[s, {_, _, bullet}, 2],
  a^Writhe[s],
  ((a - 1/a)/z)^(Length[s]-1)
]

Writhe[s_State] := Plus @@ Cases[Flatten[List @@ s, 1], {_, n_, o} :> n]/2

Writhe[s_SignedGaussCode] := Plus @@ Flatten[
  List @@ s /.  {_, sign_Integer} :> sign
]/2

Decorate[code_DecoratedGaussCode]:=Module[
  {t1, t2, switch, splice},
  {t1,t2} = Position[code,_?(Length[#] == 2&), {2}, 1][[1]];
  If[code[[t1, t2]][[1]] > 0,
      (* an overcrossing. mark the crossing and move on *)
    code /. {
      code[[t1,t2]] -> Append[code[[t1, t2]], o],
      code[[t1,t2]]{-1,1} -> Append[code[[t1,t2]]{-1, 1}, o]
    },
      (* an undercrossing *)
    switch = code /. {
      code[[t1,t2]] -> Append[code[[t1, t2]]{-1,-1}, o],
      code[[t1,t2]]{-1,1} -> Append[code[[t1, t2]]{1,-1}, o]
    };
    splice= code /. {
      {z1___, code[[t1,t2]], z2__, code[[t1,t2]]{-1,1}, z3___} -> Sequence[
	{z1, Append[code[[t1, t2]], bullet], z3},
	{z2, Append[code[[t1, t2]]{-1,1}, tt]}
      ],
      DecoratedGaussCode[
	l1___, {z1___, code[[t1,t2]], z2___},
        l2___, {z3___, code[[t1,t2]]{-1,1}, z4___}, l3___
      ] -> DecoratedGaussCode[
        l1, {z1,Append[code[[t1, t2]],bullet],z4,z3,
        Append[code[[t1, t2]]{-1,1},tt],z2},l2,l3
      ]
    };
    {switch,splice}
  ]
]

Decorate[code_SignedGaussCode]:= Nest[
  Flatten[Decorate/@#]&,
  {DecoratedGaussCode@@code},
  Length[Flatten[List@@code]]/4
] /.DecoratedGaussCode -> State

HOMFLYPT[pd_PD] := HOMFLYPT[pd] = (
  loops = Position[pd, _Loop];
  L = Delete[pd, loops];
  Function[{a, z},
    Evaluate[Expand[
      If[L === PD[],
        ((-1 + a^2)/(a*z))^(Length[loops]-1),
        ((-1 + a^2)/(a*z))^Length[loops] *
        a^(-Writhe[SignedGaussCode[L]])*
          (Plus @@ (StateValuation[a, z] /@ Decorate[SignedGaussCode[L]]))
      ]
    ]]
  ]
)
HOMFLYPT[L_] := HOMFLYPT[PD[L]]

End[]; EndPackage[];
BeginPackage["KnotTheory`"]

Kauffman::usage = "
Kauffman[K][a, z] computes the Kauffman polynomial of a knot or link K,
in the variables a and z.
"

Kauffman::about = "
The Kauffman program was written by Scott Morrison.
"

Begin["`Kauffman`"]

CrossingSign[X[i_,j_,k_,l_]]:=If[j-l==1 || l-j>1,1,-1]

RotateToMinimal[l_] := Module[
  {bl=l,rl=RotateLeft[l]},
  While[rl=!=l,bl=First[Sort[{bl,rl}]]; rl=RotateLeft[rl]]; bl
]

LinkSkeleton[pd_PD]:=Sort[RotateToMinimal/@(
  c=Times@@pd/.{
    X[i_,j_,k_,l_] :>
      path[i,k] If[CrossingSign[X[i,j,k,l]]==-1,path[j,l], path[l,j]],
    P[i_,j_]:>path[i,j]
  } //.
    {path[a__,i_]path[i_,b__]:> path[a,i,b]}
  /. {path[i_,a___,i_]:>Loop[i,a]};
  If[Head[c]===Times,List@@c,{c}]
)]

LinkSkeleton[L_]:=LinkSkeleton[PD[L]]

SignedGaussCode[PD[_Loop]]=SignedGaussCode[];

SignedGaussCode[PD[l___,_Loop,r___]]:=Append[SignedGaussCode[PD[l,r]],{}]

SignedGaussCode[PD[Xs___X]]:=Module[
  {pd=PD[Xs],c=0,s,kc},
  kc=KC@@(s=LinkSkeleton[pd]/.Loop->List);
  pd/.X[i_,j_,k_, l_]:>(
    kc[[Sequence@@ First@Position[
      s, If[CrossingSign[X[i,j,k,l]]==1,l,j]
    ]]]={++c, CrossingSign[X[i,j,k,l]]};
    kc[[Sequence@@First@Position[s,i]]]={-c,CrossingSign[X[i,j,k,l]]}
  );
  If[Length[s]==1,SignedGaussCode@@First[kc],SignedGaussCode@@kc]
]

Writhe[s_SignedGaussCode] := 1/2 Plus @@ Flatten[
  List @@ s /. {_, sign_Integer} :> sign
]

Decorate[code_DecoratedGaussCode] := Module[
  {t1,t2,switch, hsplice,vsplice},
  {t1,t2} = Position[code,_?(Length[#]==2&),{2}, 1][[1]];
  If[code[[t1, t2]][[1]]> 0,
    (* an overcrossing. mark the crossing and move on *)
    code/. {
      code[[t1,t2]]-> Append[code[[t1, t2]],EmptyCircle],
      code[[t1,t2]]{-1,1}-> Append[code[[t1,t2]]{-1, 1},EmptyCircle]},
    (* an undercrossing *)
    switch = code /. {
      code[[t1,t2]]-> Append[code[[t1, t2]]{-1,-1},EmptyCircle],
      code[[t1,t2]]{-1,1}-> Append[code[[t1, t2]]{1,-1},EmptyCircle]
    };
    hsplice= code/.{
      {z1___,code[[t1,t2]],z2__, code[[t1,t2]]{-1,1}, z3___}->Sequence[
        {z1,{If[code[[t1,t2, 2]]==1,FilledCircle, SixPointedStar]},z3},
        {z2,{DoubleDagger}}
      ],
      DecoratedGaussCode[
        l1___,{z1___, code[[t1,t2]],z2___}, l2___,
        {z3___, code[[t1,t2]]{-1,1}, z4___}, l3___
      ] -> DecoratedGaussCode[
        l1,{z1,{
          If[code[[t1,t2, 2]]==1,FilledCircle,SixPointedStar]
        },z4,z3,{DoubleDagger},z2},l2,l3
      ]
    };
    vsplice = (code/. {
      {z1___, code[[t1,t2]],z2__, code[[t1,t2]]{-1,1}, z3___}
        -> {z1,{
          If[code[[t1,t2, 2]]==1,SixPointedStar, FilledCircle]
        },ReverseGaussString[z2],{DoubleDagger},z3},
      DecoratedGaussCode[
        l1___,{z1___, code[[t1,t2]], z2___},l2___,
        {z3___, code[[t1,t2]]{-1, 1},z4___},l3___
      ] -> DecoratedGaussCode[
        l1, {
          z1,
          {If[code[[t1,t2, 2]]==1, SixPointedStar,FilledCircle]},
          ReverseGaussString[z4,z3],{DoubleDagger},z2
        }, l2, l3
      ]
    }) //. {
      ReverseGaussString[]->Sequence[],
      ReverseGaussString[z___,{a_}]->Sequence[{a}, ReverseGaussString[z]],
      DecoratedGaussCode[
        l1___,{z1___,ReverseGaussString[z2___,{a_,b_,c___}],z3___}, l2___
      ] :> (DecoratedGaussCode[
        l1,{z1,{a,-b,c},ReverseGaussString[z2],z3}, l2
      ]/.{-a,b,c}->{-a,-b,c})
    };
    {switch,hsplice,vsplice}
  ]
]

Decorate[code_SignedGaussCode]:= Nest[
  Flatten[Decorate/@#]&,
  {If[Depth[code]==3,
    DecoratedGaussCode[List@@code],
    DecoratedGaussCode@@code
  ]},
  Length[Flatten[List@@code]]/4] /. DecoratedGaussCode->State

StateValuation[Alpha_, z_][s_State] := Module[
  {
    Mu = (Alpha - Alpha^(-1))/z + 1, 
    length = Length[s], 
    symbols = Flatten[
      List @@ s /. {{a_Integer, b_Integer, c_} :> b, DoubleDagger -> {}}
    ]
  },
  (-1)^Count[symbols, SixPointedStar] *
  z^Count[symbols, SixPointedStar | FilledCircle] *
  Alpha^((1/2)(Count[symbols, 1] - Count[symbols, -1])) *
  Mu^(length - 1)
]

Kauffman[Knot[n_, k_]] := (
  Needs["KnotTheory`Kauffman4Knots`"];
  Unset[Kauffman[Knot[n1_, k1_]]];
  Kauffman[Knot[n, k]]
)
Kauffman[Knot[11, t_, k_]] := (
  Needs["KnotTheory`Kauffman4Knots11`"];
  Unset[Kauffman[Knot[11, t1_, k1_]]];
  Kauffman[Knot[11, t, k]]
)
Kauffman[Link[n_, t_, k_]] := (
  Needs["KnotTheory`Kauffman4Links`"];
  Unset[Kauffman[Link[n1_, t1_, k1_]]];
  Kauffman[Link[n, t, k]]
)
Kauffman[TorusKnot[m_, n_]] := (
  Needs["KnotTheory`Kauffman4TorusKnots`"];
  Unset[Kauffman[TorusKnot[m1_, n1_]]];
  Kauffman[TorusKnot[m, n]]
)

Kauffman[pd_PD] := Kauffman[pd] = (
  loops = Position[pd, _Loop];
  L = Delete[pd, loops];
  Function[{a, z},
    Evaluate[Expand[
      If[L === PD[],
        (-((1 + a^2 - a*z)/(a*z)))^(Length[loops]-1),
        (-((1 + a^2 - a*z)/(a*z)))^Length[loops] *
        (I a)^(-Writhe[SignedGaussCode[L]]) *
        (Plus @@
          (StateValuation[I a, -I z] /@ Decorate[SignedGaussCode[L]])
        )
      ]
    ]]
  ]
)
Kauffman[L_] := Kauffman[PD[L]]

End[];

EndPackage[];
BeginPackage["KnotTheory`"]

$RecursionLimit = 65536;

Kh::usage = "Kh[L][q, t] returns the Poincare polynomial of the
Khovanov Homology of a knot/link L (over a field of characteristic 0)
in terms of the variables q and t. Kh[L, Program -> prog] uses the
program prog to perform the computation. The currently available
programs are \"FastKh\", written in Mathematica by Dror Bar-Natan in
the winter of 2005 and \"JavaKh\" (default), written in java (java 1.5
required!) by Jeremy Green in the summer of 2005. The java program is
several thousand times faster than the Mathematica program, though java
may not be available on some systems. \"JavaKh\" also takes the option
\"Modulus -> p\" which changes the characteristic of the ground field
to p. If p==0 JavaKh works over the rational numbers; if p==Null JavaKh
works over Z (see ?ZMod for the output format)."

JavaOptions::usage = "JavaOptions is an option to Kh. Kh[L, Program ->
\"JavaKh\", JavaOptions -> jopts] calls java with options jopts. Thus
for example, JavaOptions -> \"-Xmx256m\" sets the maximum java heap
size to 256MB - useful for large computations."

ZMod::usage = "ZMod[m] denotes the cyclic group Z/mZ. Thus if m=0 it is the
infinite cyclic group Z and if m>0 it is the finite cyclic group with m
elements. ZMod[m1, m2, ...] denotes the direct sum of ZMod[m1],
ZMod[m2], ... .";

ExpansionOrder; Program;

Begin["`FastKh`"]

bdot[_]^_ ^=0; tdot[_]^_ ^=0;

EquivalenceClasses[l_List] := Fold[
      (
          pos = First /@ Position[#1, #2];
          Append[Delete[#1, List /@ pos], Union@@(#1[[pos]])]
          )&,
      l, Union @@ l
];

DotRule[top_, bot_] := DotRule[top, bot] = Flatten[Cases[
  DeleteCases[
    EquivalenceClasses[Join[
      Cases[{top}, P[i_,j_][m_] :> {z@i,z@j,tdot@m}, Infinity],
      Cases[{bot}, P[i_,j_][m_] :> {z@i,z@j,bdot@m}, Infinity]
    ]],
    _z, {2}
  ],
  l_List :> ((# -> First[l])& /@ l)
]];

HCLaw[
        Cobordism[top1_Smoothing,bot1_Smoothing],
        Cobordism[top2_Smoothing,bot2_Smoothing]
        ] /; MemberQ[{top1, bot1, top2, bot2}, Q, Infinity] := MapAt[
      (Q^Exponent[Times@@bot1, Q]*Q^Exponent[Times@@bot2, Q])&,
      MapAt[
        (Q^Exponent[Times@@top1, Q]*Q^Exponent[Times@@top2, Q])&,
        HCLaw[Cobordism[top1, bot1] /. Q->1, 
          Cobordism[top2, bot2] /. Q->1],
        {1,1,1}
        ],
      {1,2,1}
      ];

(*
  Note: Gluing d disks along z zippers, the result has b boundaries and
  genus g with 2g=2+z-d-b.
*)
HCLaw[
  Cobordism[top1_Smoothing, bot1_Smoothing],
  Cobordism[top2_Smoothing, bot2_Smoothing]
] /; FreeQ[{top1, bot1, top2, bot2}, Q] := HCLaw[
  Cobordism[top1, bot1], Cobordism[top2, bot2]
] = Module[
  {dr, top, bot, dots, handles=1, h, g2, decors, law, to, cob},
  dr = DotRule[top1 top2, bot1 bot2];
  top = Smoothing[
    First@top1*First@top2 //. P[i_, j_][m_] P[j_, k_][n_] :> (
      P[i, k][Min[m, n]]
    ) /. {
      P[i_, j_][m_]^2 :> (handles /= (tdot[m] /. dr /. bdot -> h); Loop[m]),
      P[i_, i_][m_] :> (handles /= (tdot[m] /. dr /. bdot -> h); Loop[m])
    }
  ];
  bot=Smoothing[
    First@bot1*First@bot2 //. P[i_, j_][m_] P[j_, k_][n_] :> (
      handles *= (bdot[m] /. dr /. bdot -> h);
      P[i, k][Min[m, n]]
    ) /. {
      P[i_, j_][m_]^2 :> (handles *= (bdot[m] /. dr /. bdot -> h); Loop[m]),
      P[i_, i_][m_] :> Loop[m]
    }
  ];
  dots = Union[
    Last /@ DotRule[top, bot],
    Cases[{top}, Loop[m_] :> tdot[m], Infinity],
    Cases[{bot}, Loop[m_] :> bdot[m], Infinity]
  ];
  handles *= Times @@ (Union[Last /@ dr] /. bdot -> h)^2;
  handles /= Times @@ (
    Join[
      Union[Last /@ DotRule[top1, bot1]],
      Union[Last /@ DotRule[top2, bot2]],
      Union[Last /@ DotRule[top, bot]]
    ] /. dr /. bdot -> h
  );
  decors = Expand[(handles /. h[m_]^g2_ :> (2bdot[m])^(g2/2)) *
    Times @@ MapThread[
      If[#1===#2, 1, #1+#2]&,
      {dots, dots /. dr}
    ]
  ];
  law = Union[
    Last /@ DotRule[top1, bot1], Last /@ DotRule[top2, bot2]
  ];
  law = DeleteCases[
    Thread[to[law, law /. dr]],
    to[m_, m_]
  ] /. to -> Rule;
  {Cobordism[top, bot, decors], law}
];

HC[0, _] = HC[_, 0] = 0;
HC[Smoothing[s1_], Smoothing[s2_]]:= Smoothing[
  s1 s2 //. P[i_, j_][m_] P[j_, k_][n_]:> P[i, k][Min[m, n]]
    /. {P[i_, j_][m_]^2 :> Loop[m], P[i_, i_][m_] :> Loop[m]}
];

HC[n1_.*e[t1__]*s1_Smoothing, n2_.*e[t2__]*s2_Smoothing] :=
    n1 n2 e[t1,t2]HC[s1, s2];

HC[
      Cobordism[top1_Smoothing,bot1_Smoothing, ds1_],
      Cobordism[top2_Smoothing,bot2_Smoothing, ds2_]
      ] := Module[
      {cob, law},
      {cob,law} = HCLaw[
          Cobordism[top1, bot1], Cobordism[top2, bot2]
          ];
      cob = MapAt[Expand[(ds1 ds2 /. law)*#]&, cob, 3];
      cob
      ];

HC[a_Plus, b_] := HC[#, b]& /@ a;
HC[a_, b_Plus] := HC[a, #]& /@ b;

HC[Morphism[top_, bot_, a_+b_], s_] := Plus[
      HC[Morphism[top, bot, a],s],
      HC[Morphism[top, bot, b],s]
      ];
HC[Morphism[top_, bot_, MM[e[i___],e[j___], mat_]], e[k___] * s_Smoothing] :=
    Module[
      {cob, law},
      {cob, law} = HCLaw[
          Cobordism[Coefficient[top, e[i]], Coefficient[bot, e[j]]],
          Cobordism[s,s]
          ];
      MM[e[i,k], e[j,k], Expand[Last[cob]*(mat /. law)]]
      ];

HC[s_, Morphism[top_, bot_, a_Plus]] := HC[s, Morphism[top, bot, #]]& /@ a

HC[e[k___] * s_Smoothing, Morphism[top_, bot_, MM[e[i___],e[j___], mat_]]] :=
    Module[
      {cob, law},
      {cob, law} = HCLaw[
          Cobordism[s,s],
          Cobordism[Coefficient[top, e[i]], Coefficient[bot, e[j]]]
          ];
      MM[e[k,i], e[k,j], Expand[Last[cob]*(mat /. law)]]
      ];

HC[
      Kom[f1_, obs1_, mos1_],
      Kom[f2_, obs2_, mos2_]
      ] := Module[
      {l1, l2, k, j1, j2, obs, morph, mos, rule},
      l1=Length[obs1]-1; l2=Length[obs2]-1;
      obs=Objects @@ Table[
            Plus @@ Table[
                j2=k-j1;
                HC[obs1[[1+j1]], obs2[[1+j2]]] /. 
                  e[t__] :> e[t, j1],
                {j1,Max[0,k-l2],Min[l1, k]}
                ],
            {k,0,l1+l2}
            ];
      mos = Morphisms @@ Table[
            Plus @@ Table[
                j2=k-j1;
                Plus[
                  If[1+j1 > l1 || mos1[[1+j1]] === 0 || obs2[[1+j2]]===0,
                    0, 
                    HC[
                        Morphism[obs1[[1+j1]], obs1[[2+j1]], mos1[[1+j1]]],
                       obs2[[1+j2]]
                        ] /. 
                      MM[e[t1__], e[t2__], mm_] :> 
                        MM[e[t1, j1], e[t2, j1+1], mm]
                    ],
                  If[1+j2 > l2 || obs1[[1+j1]] === 0 || mos2[[1+j2]] === 0,
                   0, 
                    HC[
                        obs1[[1+j1]],
                        Morphism[obs2[[1+j2]], obs2[[2+j2]], mos2[[1+j2]]]
                       ] /. 
                      MM[e[t1__], e[t2__], mm_] :> 
                        MM[e[t1, j1], e[t2, j1], Expand[(-1)^j1*mm]]
                    ]
                  ],
                {j1,Max[0,k-l2],Min[l1, k]}
                ],
            {k, 0, l1+l2-1}
            ];
      ReTag[Kom[f1+f2, obs, mos]]
      ];

ReTag[kom_Kom] := Module[
    {f, obs, mos, l},
    {f, obs, mos} = List @@ kom;
    l=Length[obs]-1;
    Do[
      rule = Union[Cases[{obs[[1+k]]}, _e, Infinity]];
      rule = Thread[Rule[rule, e /@ Range[Length[rule]]]];
      obs[[1+k]] = obs[[1+k]] /. rule;
      If[k<l,
        mos[[1+k]] = 
          mos[[1+k]] /. 
            MM[e1_, e2_, mm_] :> MM[e1 /. rule, e2, mm]
        ];
      If[k>0,
        mos[[k]] = 
          mos[[k]] /. MM[e1_, e2_, mm_] :> MM[e1, e2 /. rule, mm]
        ],
      {k, 0, l}
      ];
    Kom[f, obs, mos]
    ]

(*
  Note: Gluing d disks along z zippers, the result has b boundaries and
  genus g with 2g=2+z-d-b.
*)
VCLaw[
  Cobordism[top_Smoothing,mid_Smoothing],
  Cobordism[mid_Smoothing,bot_Smoothing]
] := VCLaw[Cobordism[top, mid], Cobordism[mid, bot]] = Module[
  {decors, law1, law2, dots, dots1, dots2, dr1, dr2, dr, to, h, g2},
  {law1, law2} = {{}, {}};
  decors = Times @@ Cases[
    {mid},
    Loop[m_] :> (
      AppendTo[law1, bdot[m] -> mdot[m]];
      AppendTo[law2, tdot[m] -> mdot[m]];
      mdot[m]
    ),
    Infinity
  ];
  dots = Union[Last /@ DotRule[top, bot]];
  dots1 = Union[Last /@ (dr1 = DotRule[top, mid] /. bdot -> mdot)];
  dots2 = Union[Last /@ (dr2 = DotRule[mid, bot] /. tdot -> mdot)];
  dr = Flatten[Cases[
    EquivalenceClasses[Join[List @@@ dr1, List @@@ dr2]],
    l_List :> ((# -> First[l])& /@ Rest[l])
  ]];
  decors *= Times @@ (Union[Last /@ dr] /. bdot -> h)^2;
  decors *= Times @@ (
    Cases[mid, P[__][m_] :> mdot[m], Infinity] /. dr /. bdot -> h
  );
  decors /= Times @@ (Join[dots1, dots2, dots] /. dr /. bdot -> h);
  decors = decors /. h[m_]^g2_ :> (2bdot[m])^(g2/2);
  decors *= Expand[Times @@ MapThread[
    If[#1===#2, 1, #1+#2]&,
    {dots, dots /. dr}
  ]];
  law1 = Join[law1,
    DeleteCases[
        Thread[to[dots1, dots1 /. dr]] /. mdot -> bdot,
        to[m_, m_]
      ] /. to -> Rule
    ];
  law2 = Join[law2,
    DeleteCases[
        Thread[to[dots2, dots2 /. dr]],
        to[m_, m_]
      ] /. to -> Rule
    ];
  {law1, law2, decors}
];

VC[a_, b_, c__] := VC[a, VC[b,c]];
VC[
      Cobordism[top_Smoothing,mid_Smoothing, ds1_],
      Cobordism[mid_Smoothing,bot_Smoothing, ds2_]
      ] := Module[
      {law1, law2, decor, cob},
      {law1, law2, decor} = VCLaw[Cobordism[top, mid], Cobordism[mid, bot]];
      cob = Cobordism[top, bot,
          Expand[decor*(ds1 /. law1)*(ds2 /. law2)] /. (_mdot)^2 -> 
                1 /. (_mdot -> 0)
          ];
      cob
      ];

DeLoop[kom_Kom] := Module[
      {f, obs, mos, l, dot},
      {f, obs, mos} = List @@ kom;
      l=Length[obs]-1;
      Do[
        obs[[1+k]] = 
          obs[[1+k]] //.e[i___]Smoothing[Loop[j_]*rest_.] :> (
                If[k>0, 
                  mos[[k]] = 
                    mos[[k]] /. MM[e[l___], e[i], mat_] :> Plus[
                          MM[e[l], e[i,-1],
                            Expand[dot[j]*mat] /. bdot[j]dot[j] -> 1 /. 
                              dot[j] -> 0
                            ],
                          MM[e[l], e[i,1],
                            mat /. bdot[j] -> 0
                            ]
                          ]];
                If[k<l, 
                  mos[[1+k]] = 
                    mos[[1+k]] /. 
                      MM[e[i], e[l___], mat_] :> Plus[
                          MM[e[i,-1], e[l],
                            mat /. tdot[j] -> 0
                            ],
                          MM[e[i,1], e[l],
                            Expand[dot[j]*mat] /. tdot[j]dot[j] -> 1 /. 
                              dot[j] -> 0
                            ]
                          ]];
                e[i,-1]Smoothing[rest/Q] + e[i,1]Smoothing[rest*Q]
                ),
        {k, 0, l}
        ];
      ReTag[Kom[f, obs, mos] /. MM[_, _, {{0}}] -> 0]
      ];

Contract[kom_Kom] := Module[
      {
        f, obs, mos, l, k, e2s0, e2s1, s2b, b, e2b0, e2b1, killed0, killed1, 
        done, mok
        },
      {f, obs, mos} = List @@ kom;
      l=Length[obs]-1;
      Do[
        e2s0 = 
          Cases[{obs[[1+k]]}, i_e*s_Smoothing :> (i -> s), Infinity];
        e2s1 = 
          Cases[{obs[[1+k+1]]}, i_e*s_Smoothing :> (i -> s), Infinity];
        s2b = 
          Union[Union[Last /@ e2s0, Last /@ e2s1] /. 
              P[j__][m_] :> P[j]];
        s2b = Thread[Rule[s2b, b /@ Range[Length[s2b]]]];
        e2b0 = e2s0 /. P[j__][m_] :> P[j] /. s2b;
        e2b1 = e2s1 /. P[j__][m_] :> P[j] /. s2b;
        killed0 = killed1 = {}; done = False;
        While[!done,
          done = True;
          mok = mos[[1+k]];
          Cases[
            {mok},
            MM[i_e, j_e, {{r_?NumberQ}}] /;
                ((i /. e2b0) === (j /. e2b1))
              :> (
                mok = Plus[
                    mok /. {MM[i, _, _] -> 0, MM[_, j, _] -> 0},
                   Expand[-Plus @@ Flatten[Outer[
                              Function[{M1, M2},
                                MM[M1[[1]], M2[[2]], Last[VC[
                                      Cobordism[M1[[1]] /. e2s0, j /. e2s1,
                                        M1[[3,1,1]]],
                                      Cobordism[j /. e2s1, 
                                        i /. e2s0, {{1/r}}],
                                      Cobordism[i /. e2s0, M2[[2]] /. e2s1,
                                        M2[[3,1,1]]]
                                      ]]
                                  ]
                                ],
                              Cases[{mok}, MM[i1_e, j, mm1_] /; i1=!=i, 
                                Infinity],
                              Cases[{mok}, MM[i, j1_e, mm2_] /; j1=!= j, 
                                Infinity]
                              ]]]
                    ];
                mos[[1+k]] = (((mok //. 
                              a_*MM[i1_, j1_, mm_] :> 
                                MM[i1,j1, Expand[a*mm]]) //. 
                          MM[i1_, j1_, mm1_] + 
                              MM[i1_, j1_, mm2_] :> 
                            MM[i1, j1, mm1+mm2])
                      /. MM[_, _, {{0}}] -> 0);
                done = False;
                AppendTo[killed0, i]; AppendTo[killed1, j]
                ),
            Infinity, 1]
          ];
        obs[[1+k]] = obs[[1+k]] /. ((#->0)& /@ killed0);
        obs[[1+k+1]] = obs[[1+k+1]] /. ((#->0)& /@ killed1);;
        If[k>0,
          mos[[1+k-1]] = mos[[1+k-1]] /.
              MM[i_e, j_e, mm_] /; MemberQ[killed0, j] :> 0
          ];
        If[k<l-1,
          mos[[1+k+1]] = mos[[1+k+1]] /.
              MM[i_e, j_e, mm_] /; MemberQ[killed1, i] :> 0
          ],
        {k,0,l-1}
        ];
      ReTag[Kom[f, obs, mos]]
      ];

KhComplex[X[i_,j_,k_,l_]]/;(j-l==1||l-j>1):=Kom[0, (* + xing *)
      Objects[
          e[1]Smoothing[Q P[i,j] P[k,l]],
          e[1]Smoothing[Q^2 P[i,l] P[j,k]]
          ]/.P[m_,n_]:>P[m,n][Min[m,n]],
      Morphisms[MM[e[1],e[1],{{1}}]]
      ];
KhComplex[X[i_,j_,k_,l_]]/;(l-j==1||j-l>1):=Kom[-1, (* - xing *)
      Objects[
          e[1]Smoothing[Q^(-2) P[i,j] P[k,l]],
          e[1]Smoothing[Q^(-1) P[i,l] P[j,k]]
          ]/.P[m_,n_]:>P[m,n][Min[m,n]],
      Morphisms[MM[e[1],e[1],{{1}}]]
      ];
KhComplex[pd_PD] /; (Length[pd] > 1) := Module[
    {kom},
    kom = KhComplex[First@pd];
    Do[
      kom = HC[kom, KhComplex[pd[[i]]]];
      kom = DeLoop[kom];
      kom = Contract[kom],
      {i,2,Length[pd]}
      ];
    kom
    ]

KhPoly[kom_Kom] := Module[
      {f, obs, mos},
      {f, obs, mos} = List @@ kom;
      If[Union[List @@ mos] =!= {0}, Error,
        Plus @@ Expand[t^(f-1) * t^Range[Length[obs]] * (
                List @@ obs /. e[i_]Smoothing[s_] :> s /. Q -> q
                )]
        ]
      ];

Kh[Knot[n_, k_]] := (
  Needs["KnotTheory`Kh4Knots`"];
  Unset[Kh[Knot[n1_, k1_]]];
  Kh[Knot[n, k]]
)
Kh[Knot[11, t_, k_]] := (
  Needs["KnotTheory`Kh4Knots11`"];
  Unset[Kh[Knot[11, t1_, k1_]]];
  Kh[Knot[11, t, k]]
)
Kh[Link[n_, t_, k_]] := (
  Needs["KnotTheory`Kh4Links`"];
  Unset[Kh[Link[n1_, t1_, k1_]]];
  Kh[Link[n, t, k]]
)
Kh[TorusKnot[m_, n_]] := (
  Needs["KnotTheory`Kh4TorusKnots`"];
  Unset[Kh[TorusKnot[m1_, n1_]]];
  Kh[TorusKnot[m,n]]
)

Options[Kh] = {
  ExpansionOrder -> Automatic,
  Program -> "JavaKh",
  Modulus -> 0,
  JavaOptions -> ""
};

Kh[L_, opts___] := Kh[L, opts] = Module[
  {
    L1, pos, inside, L2, f, cl,
    eo = (ExpansionOrder /. {opts} /. Options[Kh]),
    prog = (Program /. {opts} /. Options[Kh]),
    modulus = (Modulus /. {opts} /. Options[Kh]),
    javaoptions = (JavaOptions /. {opts} /. Options[Kh])
  },
  L1 = PD[L];
  Switch[prog,
  "FastKh", (
    If[eo === Automatic,
      L2 = List @@ L1; L1 = PD[]; inside = {};
      While[Length[L2] > 0,
        pos = Last[Ordering[(Length[Intersection[List @@ #, inside]])& /@ L2]];
        AppendTo[L1, L2[[pos]]];
        inside = Union[inside, List @@ L2[[pos]]];
        L2 = Delete[L2, pos]
      ]
    ];
    Function @@ {KhPoly[KhComplex[L1]] /. {q -> #1, t -> #2}}
  ),
  "JavaKh", (
    dir = Directory[];
    SetDirectory[ToFileName[KnotTheoryDirectory[], "JavaKh"]];
    f = OpenWrite["pd", PageWidth -> Infinity];
    WriteString[f, ToString[L1]];
    Close[f];
    cl = StringJoin[
      "!java ", javaoptions, " JavaKh ",
      If[modulus === Null, "-Z", "-mod "<>ToString[modulus]],
      " < pd"
    ];
    f = OpenRead[cl];
    out = Read[f, Expression];
    Close[f];
    SetDirectory[dir];
    out = StringReplace[out, {
      "q" -> "#1", "t" -> "#2", "Z" -> "ZMod"
    }];
    ToExpression[out <> "&"]
  )
  ]
]

End[]; EndPackage[]
BeginPackage["KnotTheory`"];

MorseLink::usage =
    "MorseLink[K] returns a presentation of the oriented link K, composed, in \
successive order, of the following 'events':
    Cup[m,n] is a directed creation, starting at strand position n, towards \
position m, where m and n differ by 1.
    X[n,a = {Over/Under}, b = {Up/Down}, c={Up/Down}] is a crossing with \
lower-left edge at strand n, a determines whether the strand running \
bottom-left to top-right is over/under the crossing, b and c give the \
directions of the bottom-left and bottom-right strands respectively through \
the crossing.
    Cap[m,n] is a directed cap, from strand m to strand n.
    ";

MorseLink::about = "MorseLink was added to KnotTheory` by Siddarth Sankaran
at the University of Toronto in the summer of 2005."

Cup::usage=Cap::usage=Up::usage=Down::usage=Over::usage=Under::usage=MorseLink::usage;

Begin["`MorseLink`"]; 

GetDir[a_,b_] := 
    If[Max[a,b] \[Equal] (Min[a,b] +1), 
      If[a<b, Return[Up], Return[Down]],
      If[a<b, Return[Down], Return[Up]]];


s4[1]=2;s4[2]=3;s4[3]=4;s4[4]=1;  (*since a[[0]] is NOT the first element*)
MorseLink[PD[Loop[1]]] := MorseLink[Cup[1,2], Cap[2,1]];
MorseLink[input_] := MorseLink[PD[input]];
MorseLink[crossings_PD] := 
    Module[ {strands,  output={}, adjpos, found=0, in, dirlist,k=1} , 
      in = crossings;
      (* setup first crossing *)
      
      Module[{d1,d2},
        
        {d1,d2} = {GetDir[ in[[1,1]] , in[[1,3]] ], 
            GetDir[ in[[1,2]], in[[1,4]] ]};
        If[TrueQ[d1 \[Equal] Up], output={Cup[1,2]}, output = {Cup[2,1]}];
        If[TrueQ[d2 \[Equal] Up], AppendTo[output,Cup[4,3] ] , 
          AppendTo[output, Cup[3,4] ] ];
        AppendTo[output, X[ 2, Under, d1,d2 ] ] ;
        strands = {in[[1,1]], in[[1,4]], in[[1,3]], in[[1,2]]};
        Switch[{d1,d2},
          {Up,Up},
          	dirlist={Down,Up,Up,Down},
          {Up,Down},
          	dirlist = {Down,Down,Up,Up},
          {Down,Up},
          	dirlist = {Up,Up,Down,Down},
          {Down,Down},
          	dirlist = {Up,Down,Down,Up}
          ];
        
        in = Delete[in, 1];
        ];
      
      
      (* Main loop - caps things, uses crossings of adjacent strands, 
        adds cups if none of the above *) 
      While[ (Length[strands] \[NotEqual] 0) && (k \[LessEqual] 
              4*Length[crossings]) , 
        k++;
        If[Length[in]\[NotEqual] 0, found=0, found=1];
        
        (*find adjacent strands, cap 'em and remove them from strand list *)
        
        Module[{adjpos,dir},
          adjpos = Position[
                Partition[strands,2,1], {x_,x_}];
          If[Length[adjpos] \[NotEqual] 0,
            
            If[TrueQ[dirlist[[ adjpos[[1,1]] ]] \[Equal] Up],
              
              output = 
                Append[output,Cap[ adjpos[[1,1]], adjpos[[1,1]] + 1 ] ] ];
            If[TrueQ[dirlist[[ adjpos[[1,1]] ]] \[Equal] Down],
              
              output = 
                Append[output, Cap[ adjpos[[1,1]] + 1, adjpos[[1,1]] ] ] ];
            
            strands =Delete[strands, {{adjpos[[1,1]]}, {adjpos[[1,1]] + 1}}];
            dirlist =Delete[dirlist, {{adjpos[[1,1]]}, {adjpos[[1,1]] + 1}}];
            ];
          ];
        
        (* find a crossing whose edges involve adjacent strands, 
          if we can *)
        
        Module[ {m,n, a, b, x, y, pos,overunder,dx,dy},
          For[ m = 1, m \[LessEqual] Length[in], m++,
              For[ n=1, n\[LessEqual] 4, n++,
                  If[found==0,
                      {x,y} = {in[[m,n]], in[[m, s4[n] ]]};
                      {a,b} = {in[[m, s4[s4[s4[n]]]  ]], 
                          in[[m, s4[s4[n]] ]]};   (*very inelegant!!*)
                      
                      
                      If[Position[
                            Partition[strands, 2, 1], {x,y}] \[NotEqual] {},
                        (*found at least one crossing using adjacent strands, 
                          pick the first and do it *)
                        
                        found = 1;
                        
                        pos = Position[Partition[strands, 2, 1], {x,y}][[1,
                              1]];
                        
                        If[Mod[n,2] \[Equal] 1, overunder=Under, 
                          overunder=Over];
                        {dx,dy} = {GetDir[x,b], GetDir[y,a]};
                        output = Append[output, X[pos, overunder, dx, dy] ];
                        
                        strands = 
                          ReplacePart[ ReplacePart[strands, a, pos], b, 
                            pos+1];
                        
                        dirlist = 
                          ReplacePart[ReplacePart[dirlist, dy, pos], dx, 
                            pos + 1];
                        in = Delete[in, m];
                        
                        ];
                      ];
                  ];
               ];
          ];
        
        (* If there was no usable crossing, 
          we introduce new strands so that we can use one *)
        If[found==0,
          Module[ {cflag=0, pos, m, n,a,b,x,y, overunder,dx,dy, opdy},
              (*search the list of crossings for an edge that appears in the \
strand list, such that the adjacent edge does not *)
              For[m = 1, m \[LessEqual] Length[in], m++,
                  For[n=1, n\[LessEqual] 4, n++,
                      If[cflag\[Equal]0,
                          
                          If[    
                              Length[Position[strands, in[[m,n]] ] ] \[Equal] 
                                1 ,
                              If[ !MemberQ[strands, in[[m, s4[n] ]]], 
                                  cflag=1;
                                  {x,y}= {in[[m,n]], in[[m, s4[n] ]]};
                                  {a,b} = {in[[m, s4[s4[s4[n]]]  ]], 
                                      in[[m, s4[s4[n]] ]]};
                                  pos = Position[strands, x][[1,1]];
                                  
                                  If[Mod[n,2] \[Equal]1, overunder = Under, 
                                    overunder=Over];
                                  {dx,dy} = {GetDir[x,b], GetDir[y,a]};
                                  
                                  If[TrueQ[dy \[Equal] Up],
                                    output=Append[output, Cup[pos+2,pos+1]];
                                    opdy = Down;,
                                    output = Append[output,Cup[pos+1,pos+2]]; 
                                    opdy =Up;
                                    ];
                                  
                                  
                                  output=
                                    Append[output, 
                                      X[ pos, overunder, dx, dy] ];
                                  
                                  strands = 
                                    ReplacePart[
                                      Insert[Insert[strands,b, pos+1], y, 
                                        pos+2], a, pos];
                                  
                                  dirlist = 
                                    ReplacePart[
                                      Insert[Insert[dirlist,dx, pos+1], opdy, 
                                        pos+2], dy, pos];
                                  in=Delete[in,m];
                                  ];
                              ];
                          ];
                      ];
                  ];
              ];
          
          ];
        (* maybe there's more components, setup the next one *)
        (*If[(strands \[Equal] {}) && (in \[NotEqual]  {}),
              
              output =  
                Flatten[
                  Append[output,{"Next Component:",Cup[1], Cup[3], 
                      Cr[2, "u"]  }]]  ;
              strands = {in[[1,1]], in[[1,4]], in[[1,3]], in[[1,2]]};
              in = Delete[in, 1];
              ]; *)
        ]; 
      output[[0]]=MorseLink;
      If[k> 4*Length[crossings],Return["MorseLink::Error: bad input"],Return[output]];
      ];
End[];
EndPackage[];
BeginPackage["KnotTheory`"];
 
DrawMorseLink::usage = 
    "DrawMorseLink[L] returns a drawing of the knot or link L as a \"Morse Link\". \
For diagrams with a large number of crossings, it may be helpful \
to use one or both of the options as in
    DrawMorseLink[L, Gap -> g, ArrowSize -> as ], with 0 < as, g < 1, where g controls \
the amount of white space at each crossing, and as controls the size of the \
orientation arrows. ";

DrawMorseLink::about = "DrawMorseLink was written by Siddarth Sankaran
at the University of Toronto in the summer of 2005."

Options[DrawMorseLink] = {Gap \[Rule] 0.4, ArrowSize \[Rule] 0.5};

Begin["`DrawMorseLink`"]; 

DrawMorseLink[in_, opts___]/; Head[in] =!= MorseLink := DrawMorseLink[MorseLink[in], opts];

DrawMorseLink[ml_MorseLink, opts___] := 
    Module[ {in={{}}, output={}, ch=1, cw=1,  dline, dcup, dcap, dslant, 
        l, Edge, Mid, lc,
        as =(ArrowSize *0.25) /. {opts} /. If[Count[ml, _X] \[LessEqual] 4, ArrowSize \[Rule] 0.2, Options[DrawMorseLink] ],
        crgap=(0.5- Gap/2) /. {opts} /. Options[DrawMorseLink] },
      (*set parameters *)
      
      lc[1] = RGBColor[0., 0., 0.];
      lc[2] = RGBColor[1., 0., 0.];
      lc[3] = RGBColor[0., 0., 1.];
      lc[4] = RGBColor[1., 0., 1.];
      lc[5] = RGBColor[1., 0.5, 0.];
      lc[6] = RGBColor[0.5, 0.164693, 0.164693];
      lc[7] = RGBColor[1., 1., 0.];
      lc[n_] /; n>7 := lc[Mod[n,7,1]];
                
      (*drawing fns*)
      
      dline[{x_, y_}, col_] := {col,Line[{{x,y}, {x+cw, y}}]};
      dcup[{x_, y_}, {a_,b_},dir_, col_] := {col, 
          Circle[{x+cw, (y+b)/2}, {0.6*cw, (b-y)/2}, {\[Pi]/2, 3\[Pi]/2}],
          
          If[dir===D, 
            Line[{{x+(0.4-2*as)*cw, (y+b)/2 + as*cw}, {x+0.4*cw, (y+b)/
                    2}, {x+(0.4+2*as)*cw, (y+b)/2 + as*cw}}], 
            Line[{{x+(0.4-2*as)*cw, (y+b)/2 - as*cw}, {x+0.4*cw, (y+b)/
                    2}, {x+(0.4+2*as)*cw, (y+b)/2 - as*cw}}]]};
      
      dcap[{x_, y_}, {a_,b_}, dir_, col_]:= {col,
          Circle[{x, (y+b)/2}, {0.6*cw, (b-y)/2}, {-\[Pi]/2, \[Pi]/2}], 
          If[dir===D, 
            Line[{{x+(0.6 - 2*as)*cw, (y+b)/2 +as*cw}, {x+0.6*cw, (y+b)/
                    2}, {x+(0.6+2*as)*cw, (y+b)/2 + as*cw}}],
            
            Line[{{x+(0.6-2*as)*cw, (y+b)/2 - as*cw}, {x+0.6*cw, (y+b)/
                    2}, {x+(0.6+2*as)*cw, (y+b)/2 -as*cw}}]]};
      dcr[{x_, y_},{a_,b_}, t_, c1_, c2_]:= Module[ {dy = (b-y)},
          Switch[t, 
              Over, 
              Return[{c1,Line[{{x,y}, {a+cw, b}}],c2, 
                  Line[{{x+cw, y}, {x+(1-crgap)*cw,y+ crgap*dy}}], 
                  Line[{{x+crgap*cw, y+(1-crgap)*dy}, {a, b}}]}],
              Under, 
              Return[{c2, Line[{{a,b}, {x+cw, y}}],c1,  
                  Line[{{x,y}, {x+crgap*cw, y+crgap*dy}}], 
                  Line[{{x+(1-crgap)*cw, y+(1-crgap)*dy}, {a + cw, b}}]}]
              ];
          ];
      
      (*start doing something*)
      (*convert to absolute positions *)
     
 
      Module[{str={},x,y, pos, s1, s2,k, t},
        
        (*first pass - cups fixed*)
        l = List@@ml/.{
                X[a_,c___] \[RuleDelayed] X[str[[a]],str[[a+1]],c], 
                
                Cap[a_, b_] \[RuleDelayed] ({x,y} = {str[[a]], str[[b]]};
                    pos = Min[a,b];
                    str = Delete[str, {{a}, {b}}];
                    Cap[x,y]),
                
                Cup[a_, b_] \[RuleDelayed] 
                  (pos = Min[a,b]; 
                    
                    If[(Length[str] \[NotEqual]0) && (pos >  Length [str] || pos == 1), (*edge of diagram*)
                      
                      str = Flatten[If[pos == 1, Prepend[str, {First[str] - 2*cw, First[str] -cw}],
                      	Append[str, {{Last[str] + cw}, {Last[str] + 2*cw}}]]];
                      t = Edge;,
                      If[Length[str] \[NotEqual] 0,
                          	t = Mid;
                          	For[k=1, k \[LessEqual] Length[str], k++,
                            	If[k\[LessEqual] pos -1, 
                                	str[[k]] = str[[k]] -ch;,
                                	str[[k]] = str[[k]] + ch;
                                	];
                            	];
                          {s1, s2} = {str[[pos-1]]+ch, 
                              str[[pos-1]] + 2*ch};
                          str = Insert[ Insert[str, s2, pos], s1, pos];
                          ];
                      ]; 
                    If[Length[str] \[Equal] 0, str = {ch, 2*ch};t=Edge];
                    Cup[str[[a]], str[[b]],t ]
                    )
                };
        ];

      (*second pass, look ahead for lane changes and adjust accordingly, 
        and generate colours *)
      Module[ {t,caps,f, ac, pos, m},
        caps = Position[l, Cup[___, Mid, ___]];
        f[a_, {p1_, p2_}] := 
          If[a\[LessEqual] p1, a-1, If[a\[GreaterEqual] p2, a+1]];
        m=t = Table[{l[[i]], i}, {i, Length[l]}];
        If[Length[caps] \[NotEqual] 0,
          t = t /. {a_[b_, c_, d___], n_} \[RuleDelayed] ( 
                    ac = Cases[caps, {i_} /; i > n]; 
                    
                    pos = {Min[m[[#,1,1]], m[[#,1,2]]], 
                            Max[m[[#,1,1]], m[[#,1,2]]]} & /@ Flatten[ac];
                    m[[n]] = {a[Fold[f,b,pos], Fold[f,c,pos],d],n};
                    {a[Fold[f,b,pos], Fold[f,c,pos],d],n}
                    );
          ];
        (*generate colours *)

        Module[ {temp, k=0, ar, prod=1,prev, next, i, cur},
          
          t = t  /. {X[a_, b_, c_, ___] \[RuleDelayed]  
                  X[a,b,c,temp[++k], temp[++k] ], 
                Cup[a_, b_, c_] \[RuleDelayed]  Cup[a, b, temp[++k], c],
                Cap[a_, b_] \[RuleDelayed]  Cap[a,b, temp[++k]]};
          
          next[str_, pos_] := Module[ {p},
              
              p= First[
                    Cases[t, {_[a_, b_, ___], 
                          i_} /; (a== str || b\[Equal]str)&& i>pos]][[1]];
              
              Switch[Head[p],Cap, p[[3]],X, 
                If[p[[1]] === str, p[[4]], p[[5]] ]   ]    ];
          
          For[i=1, i\[LessEqual] Length[t], i++,
            cur = t[[i,1]];
            Switch[Head[cur],
              Cup,
              
              prod =prod*ar[cur[[3]],next[cur[[1]], i]]*
                    ar[cur[[3]],next[cur[[2]] ,i]];,
              X,
              
              prod = prod*ar[cur[[4]], next[cur[[2]], i]]*
                    ar[cur[[5]], next[cur[[1]], i]];
              ];
            ];
          
          prod = prod //. 
              ar[a___, b_, c___]*ar[d___, b_, f___] \[RuleDelayed] 
                DeleteCases[ar[a,b,c,d,f], {}];
          prod = List@@prod /. ar \[Rule] List;
          If[Head[prod[[1]]] =!= List, prod = {prod}];
          t= t /. Flatten[ 
                Table[prod[[i,j]] \[Rule] lc[i], {i, Length[prod]}, {j, 
                    Length[prod[[i]]]}]];
          ];
        l = Table[t[[i,1]] , {i, Length[t]}] ;
        ];

      (*play tetris*)
      Module[ {cur, k,j,i, p1, p2}, 
        For[k=1, k\[LessEqual] Length[l], k++, 
            cur = l[[k]];
            {p1, p2} = #[ cur[[1]], cur[[2]] ]& /@ {Min, Max};
            Switch[cur,
              _Cap,  
              i = Length[in];
              
              While[i>0 && 
                  Apply[And, FreeQ[in[[i]], #, 2]& /@ Range[p1, p2] ], --i];
              If[i\[Equal]Length[in], AppendTo[in, {}] ];
              AppendTo[in[[i+1]], cur];,
              _,
              i= Length[in];
              
              
              While[i>0&& 
                  FreeQ[Union@@ (Range[Min[#[[1]], #[[2]]], 
                              Max[#[[1]], #[[2]]] ]&/@in[[i]]), cur[[1]]] && 
                  FreeQ[ Union@@ (Range[Min[#[[1]], #[[2]]], 
                              Max[#[[1]], #[[2]]]]&/@in[[i]]), cur[[2]] ] && 
                  Apply[And, FreeQ[in[[i]], #, 2]& /@ Range[p1, p2]]  , i--];
              If[i \[Equal] Length[in] , AppendTo[in,{}]];
              AppendTo[in[[i+1]], cur];
              
              ];
            ];
        ];

      (*at this point:
            X[str1, str2, over/under, col1, col2]
          Cup[str1, str2, col, edge/mid]
          Cap[str1, str2, col] *)
      
      (*draw components*)
      Module[{n,m,cur, p=1},
        For[n=1, n\[LessEqual]Length[in], n++,
            For[m=1, m \[LessEqual] Length[in[[n]] ], m++,
                cur = in[[n,m]];
                Switch[ cur,
                  _Cup,
                  	Module[ {p1,p2,d},
                    	{p1, p2} = #[cur[[1]], cur[[2]] ]& /@ {Min, Max};
                    	If[cur[[1]] < cur[[2]],d=U, d=D];
                    	
                    output = 
                      Flatten[
                        Append[output, 
                          dcup[{n*cw, p1}, {n*cw, p2}, d, cur[[3]] ]]];
                    		],
                  _X,
                  	Module[ {pos},
                    	pos = cur[[1]];
                    	
                    output = 
                      Flatten[
                        Append[output, 
                          dcr[{n*ch, cur[[1]]}, {n*ch, cur[[2]]}, cur[[3]], 
                            cur[[4]], cur[[5]] ] ] ];
                    	],
                  _Cap,
                  	Module[ {p1, p2, d},
                    	{p1, p2} = {Min[ cur[[1]], cur[[2]] ], 
                        Max[ cur[[1]], cur[[2]] ]};
                    	If[cur[[1]] < cur[[2]],d=U, d=D];
                    	
                    output = 
                      Flatten[
                        Append[output, 
                          dcap[{n*cw, p1}, {n*cw, p2},d, cur[[3]] ] ] ];
                    	]
                  ];
                ];
            ];
        ];
      
      (*Draw lines to connect components  *)
      
      Module[ {strands, i,j, noninv, p1, p2},
        strands = 
          Flatten[ 
            Table[{in[[1,m,n]], in[[1,m,3]]}, {m, Length[in[[1]]]}, {n,2}] , 
            1];
        For[i=2, i\[LessEqual] Length[in], i++,
          
          noninv = 
            Cases[strands, {x_, _} /; FreeQ[Cases[in[[i]], _Integer, 2], x]];
          
          For[j=1, j \[LessEqual] Length[noninv], j++,
            
            output = 
                Flatten[
                  Append[output, 
                    dline[{cw*i, noninv[[j,1]]}, noninv[[j,2]] ] ] ];
            ];
          
          For[j=1, j \[LessEqual] Length[in[[i]] ], j++,
            {p1,p2} = #[ in[[i,j,1]], in[[i,j,2]]]& /@ {Min, Max};
            Switch[in[[i,j]],
              _Cup,
              
              strands = 
                  Union[strands,{{p1,in[[i,j,3]]}, {p2, in[[i,j,3]]}}]; 
              ,
              _Cap, 
              
              strands = 
                  DeleteCases[
                    DeleteCases[
                      strands, {in[[i,j,1]], _} ], {in[[i,j,2]], _} ];
              ,
              _X,
              strands = 
                  strands /. {{x_, c_} /; 
                          x \[Equal] in[[i,j,1]] \[RuleDelayed] {x, 
                          in[[i,j,5]]}, {x_, c_} /; 
                          x \[Equal] in[[i,j,2]] \[RuleDelayed] {x, 
                          in[[i,j,4]]}};
              ];
            ];
          ];
        ];
      Return[Graphics[output]];
      ];
End[];
EndPackage[];
BeginPackage["KnotTheory`"]; 

Begin["`MorseLink2PD`"]; 

PD[MorseLink[Cup[1,2], Cap[2 , 1] ] ] := PD[Loop[1]]; 

PD[in_MorseLink] := Module[ {pos, arrow, strands = {}, edgecount = 0, n, chains = 1, output = {}, a, b, x, y, i}, 
	For[n = 1, n <= Length[in], n++, 
		Switch[in[[n]], 
			_Cup, 
				pos = Min[in[[n,1]], in[[n,2]]]; 
				edgecount++; 
				strands = Insert[Insert[strands, edgecount, pos], edgecount, pos]; , 
			_Cap,
				chains *= arrow[strands[[ in[[n,1]] ]], strands[[ in[[n,2]] ]] ];
				pos = Min[ in[[n,1]], in[[n,2]] ]; 
				output = output /. strands[[ in[[n,1]] ]] -> strands[[ in[[n,2]] ]]; 
				chains = chains /. strands[[in[[n,1]]]] -> strands[[in[[n,2]]]]; 
				strands = Delete[strands, {{pos}, {pos + 1}}]; , 
			_X, 
				pos = in[[n,1]]; 
				{x, y} = {strands[[pos]], strands[[pos + 1]]}; 
				a = strands[[pos]] = ++edgecount; b = strands[[pos + 1]] = ++edgecount; 
				Switch[ {in[[n,2]], in[[n,3]], in[[n,4]]}, 
					{Under, Up, _}, AppendTo[output, X[x, y, b, a]], 
					{Under, Down, _},  AppendTo[output, X[b, a, x, y]], 
					{Over, _, Up}, AppendTo[output, X[y, b, a, x]], 
					{Over, _, Down}, AppendTo[output, X[a, x, y, b] ] 
				]; 
				If[TrueQ[in[[n,3]] == Up], chains *= arrow[x, b], chains *= arrow[b, x] ];
				If[TrueQ[in[[n,4]] == Up], chains *= arrow[y, a], chains *= arrow[a, y] ]; 
		]; 
	]; 
	chains = chains //. arrow[a_, b___, c_]*arrow[c_, d___, e_] :> arrow[a, b, c, d,  e] //. arrow[a___, x_, x_, b___] :> arrow[a, x, b]; chains = chains /.  a_arrow :> Rest[a]; 
	i = Flatten[Apply[List, DeleteCases[List @@ chains, arrow[]^(n_)], {1}]]; 
	output = output /. MapThread[Rule, {i, Range[Length[i]]}]; 
	Return[PD@@output]; 
]; 
End[]; 
EndPackage[]; 
BeginPackage["KnotTheory`"]

Alexander::usage = "Alexander[K][t] computes the Alexander polynomial of a knot K as a function of the variable t.  Alexander[K, r][t] computes a basis of the r'th Alexander ideal of K in Z[t].";

Alexander::about = "The program Alexander[K, r] to compute Alexander ideals was written by Jana Archibald at the University of Toronto in the summer of 2005."

Conway::usage = "Conway[K][z] computes the Conway polynomial of a knot K as a function of the variable z."

KnotDet::usage = "KnotDet[K] returns the determinant of a knot K."

Begin["`AlexanderConway`"]

Alexander[PD[Loop[_]]] = 1&
Alexander[pd_PD] := Alexander[pd] = Function @@ {(
  n = Length[pd];
  sints = List @@ Union @@ pd;
  tints = sints //. Cases[pd, X[i_, j_, k_, l_] :> (k -> i)];
  lints = tints /. Thread[Rule[Union[tints], Range[n]]];
  r = Thread[Rule[sints, lints]];
  e[i_] := ReplacePart[Table[0, {n}], 1, i];
  mat = (List @@ pd) /. 
    X[i_, j_, _, k_] :> (1 - t)e[i /. r] + t*e[k /. r] - e[j /. r] ;
  a = Det[Rest[Rest /@ mat]];
  Expand[
    a/t^((Exponent[a, t, Min] + Exponent[a, t, Max])/2)/(a /. t -> 1)
  ]
) /. t->#}
Alexander[K_] := Alexander[K] = Alexander[PD[K]]

Alexander[PD[Loop[_]], r_Integer] := {1}& 
Alexander[K_, r_] /; Head[K] =!= PD := Alexander[PD[K], r]
Alexander[K_PD, r_Integer] := (Alexander[K, r] = (
L = {};
For[i = 1, i <= Length[K], i++, Which[K[[i]][[4]] == 1,
      L = 
        Append[L, 
          ReplacePart[
            ReplacePart[
              ReplacePart[Table[0, {n, 2 Length[K]}], t, K[[i]][[1]]], -1, 
              K[[i]][[3]]], 1 - t, K[[i]][[4]]]],
      K[[i]][[2]] == 1,
      L = 
        Append[L, 
          ReplacePart[
            ReplacePart[
              ReplacePart[Table[0, {n, 2 Length[K]}], t, K[[i]][[3]]], -1, 
              K[[i]][[1]]], 1 - t, K[[i]][[4]]]],
      K[[i]][[2]] < K[[i]][[4]],
      L = 
        Append[L, 
          ReplacePart[
            ReplacePart[
              ReplacePart[Table[0, {n, 2 Length[K]}], t, K[[i]][[1]]], -1, 
              K[[i]][[3]]], 1 - t, K[[i]][[4]]]],
      K[[i]][[2]] > K[[i]][[4]],
      L = Append[L, ReplacePart[
            
            ReplacePart[
              ReplacePart[Table[0, {n, 2 Length[K]}], t, K[[i]][[3]]], -1, 
              K[[i]][[1]]], 1 - t, K[[i]][[4]]]]]];  



P1 = {};
For[i = 1, i <= Length[K], i++, P1 = Append[P1, Part[Part[K, i], 1]]];
F := Sort[P1];
G := Array[0, {2Length[K], Length[K]}];
For[i = 1, i <= Length[K], i++,  
  For[j = 1, j <= 2Length[K], j++, 
    If[i < 2, 
      G = ReplacePart[G, 
          Which[j > Part[F, Length[K]], 1, j <= Part[F, 1], 1, 
            Part[F, 1] < j <= Part[F, Length[K]], 0],  {j, i}], 
      G = ReplacePart[G, 
          If[Part[F, i - 1] < j <= Part[F, i], 1, 0],  {j, i}]]]];

Det[Rest[Transpose[Rest[L.G]]]];
A = Union[Flatten[Minors[Rest[Transpose[Rest[L.G]]], Length[K] - r]]];
A = DeleteCases[A, 0];
For[i = 1, i <= Length[A], i++, 
    A = ReplacePart[A, Expand[A[[i]]/t^(Exponent[A[[i]], t, Min])], i]];
A = Union[A];
B = A;
Block[{t}, Label[start]; A = B;
  For[i = 1, i <= Length[A], i++, 
    For[j = i + 1, j <= Length[A], j++, 
      If [Exponent[B[[i]], t, Max] < 1, 
        If[Exponent[B[[j]], t, Max] < 1, 
          B = ReplacePart[B, GCD[B[[i]], B[[j]]], {{i}, {j}}],  
          B = ReplacePart[B, PolynomialMod[B[[j]], B[[i]]], j]],
        If[Exponent[B[[j]], t, Max] < 1, 
          B = ReplacePart[B, PolynomialMod[B[[i]], B[[j]]], i],
          
          
          
          If[Abs[LCM[
                    Part[CoefficientList[B[[i]], t], 
                      Exponent[B[[i]], t, Max] + 1], 
                    Part[CoefficientList[B[[j]], t], 
                      Exponent[B[[j]], t, Max] + 1]]/(Part[
                      CoefficientList[B[[j]], t], 
                      Exponent[B[[j]], t, Max] + 1])] != 1,
            
            
            If[Abs[LCM[
                      Part[CoefficientList[B[[i]], t], 
                        Exponent[B[[i]], t, Max] + 1], 
                      Part[CoefficientList[B[[j]], t], 
                        Exponent[B[[j]], t, Max] + 1]]/(Part[
                        CoefficientList[B[[i]], t], 
                        Exponent[B[[i]], t, Max] + 1])] != 1,
              
              B = Append[B, 
                  Expand[LCM[
                          Part[CoefficientList[B[[i]], t], 
                            Exponent[B[[i]], t, Max] + 1], 
                          Part[CoefficientList[B[[j]], t], 
                            Exponent[B[[j]], t, Max] + 1]] *B[[i]]*
                        t^(Max[Exponent[B[[i]], t, Max], 
                                  Exponent[B[[j]], t, Max]] - 
                                Exponent[B[[i]], t, Max])/(Part[
                              CoefficientList[B[[i]], t], 
                              Exponent[B[[i]], t, Max] + 1])
                      - 
                      LCM[Part[CoefficientList[B[[i]], t], 
                            Exponent[B[[i]], t, Max] + 1], 
                          Part[CoefficientList[B[[j]], t], 
                            Exponent[B[[j]], t, Max] + 1]] *B[[j]]*
                        t^(Max[Exponent[B[[i]], t, Max], 
                                  Exponent[B[[j]], t, Max]] - 
                                Exponent[B[[j]], t, Max])/(Part[
                              CoefficientList[B[[j]], t], 
                              Exponent[B[[j]], t, Max] + 1])]],
              
              
              B = ReplacePart[B, 
                  Expand[LCM[
                          Part[CoefficientList[B[[i]], t], 
                            Exponent[B[[i]], t, Max] + 1], 
                          Part[CoefficientList[B[[j]], t], 
                            Exponent[B[[j]], t, Max] + 1]] *B[[i]]*
                        t^(Max[Exponent[B[[i]], t, Max], 
                                  Exponent[B[[j]], t, Max]] - 
                                Exponent[B[[i]], t, Max])/(Part[
                              CoefficientList[B[[i]], t], 
                              Exponent[B[[i]], t, Max] + 1])
                      - 
                      LCM[Part[CoefficientList[B[[i]], t], 
                            Exponent[B[[i]], t, Max] + 1], 
                          Part[CoefficientList[B[[j]], t], 
                            Exponent[B[[j]], t, Max] + 1]] *B[[j]]*
                        t^(Max[Exponent[B[[i]], t, Max], 
                                  Exponent[B[[j]], t, Max]] - 
                                Exponent[B[[j]], t, Max])/(Part[
                              CoefficientList[B[[j]], t], 
                              Exponent[B[[j]], t, Max] + 1])], i]],
            B = 
              ReplacePart[B, 
                Expand[LCM[
                        Part[CoefficientList[B[[i]], t], 
                          Exponent[B[[i]], t, Max] + 1], 
                        Part[CoefficientList[B[[j]], t], 
                          Exponent[B[[j]], t, Max] + 1]] *B[[i]]*
                      t^(Max[Exponent[B[[i]], t, Max], 
                                Exponent[B[[j]], t, Max]] - 
                              Exponent[B[[i]], t, Max])/(Part[
                            CoefficientList[B[[i]], t], 
                            Exponent[B[[i]], t, Max] + 1])
                    - 
                    LCM[Part[CoefficientList[B[[i]], t], 
                          Exponent[B[[i]], t, Max] + 1], 
                        Part[CoefficientList[B[[j]], t], 
                          Exponent[B[[j]], t, Max] + 1]] *B[[j]]*
                      t^(Max[Exponent[B[[i]], t, Max], 
                                Exponent[B[[j]], t, Max]] - 
                              Exponent[B[[j]], t, Max])/(Part[
                            CoefficientList[B[[j]], t], 
                            Exponent[B[[j]], t, Max] + 1])], j]
            ]
          ]]]];
  B = DeleteCases[B, 0];
  For[i = 1, i <= Length[B], i++, 
    B = ReplacePart[B, Expand[B[[i]]/t^(Exponent[B[[i]], t, Min])], i]];
  B = Union[B];
  If[B != A, Goto[start]]];
  Evaluate[B /. t->#]&
))

Conway[K_] := Conway[K] = Function @@ {Module[{t},
  a = Alexander[K][t];
  While[0 < (h=Exponent[a, t, Max]),
    a += Expand[Coefficient[a, t, h] * (z^h - (t+1/t-2)^h)]
  ];
  a /. z->z^2
] /. z -> #}

KnotDet[K_] := Abs[Alexander[K][-1]]

End[]
EndPackage[]
