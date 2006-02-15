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
