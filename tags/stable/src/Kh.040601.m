
BeginPackage["KnotTheory`"]

Kh::usage = "
  Kh[L][q, t] returns the Poincare polynomial of the Khovanov Homology of
  a knot/link L (over a field of characteristic 0) in terms of the
  variables q and t. Kh[L, Modulus -> p][q, t] does the same modulo p.
"

Kh`S; Kh`V; Kh`c; Kh`vp; Kh`vm; Kh`Bracket; Kh`v; Kh`d; Kh`CC;
Kh`Betti; Kh`qBetti;

Begin["`Kh`"]

SetAttributes[p, Orderless]

Kh`S[L_, s_String] := Kh`S[L, Characters[s] /. {"0"->0, "1"->1}]
Kh`S[L_, a_List] := Times @@ (Thread[{List @@ PD[L], a}] /. {
  {X[i_,j_,k_,l_], 0} :> p[i,j][Min[i,j]] p[k,l][Min[k,l]],
  {X[i_,j_,k_,l_], 1} :> p[i,l][Min[i,l]] p[j,k][Min[j,k]],
  {x_X, "*"} :> x
}) //. p[i_,j_][m_] p[j_,k_][n_] :> p[i,k][Min[m,n]] //. {
  X[i_,j_,k_,l_] p[i_,j_][m_] p[k_,l_][n_]
    :> (Kh`c[m] Kh`c[n] -> Kh`c[Min[m,n]]),
  X[i_,j_,k_,l_] p[i_,l_][m_] p[j_,k_][n_]
    :> (Kh`c[Min[m,n]] -> Kh`c[m] Kh`c[n])
} //. p[_,_][m_]^_. :> Kh`c[m]

Deg[expr_] := Count[expr, _Kh`vp, {0,1}] - Count[expr, _Kh`vm, {0,1}]

Kh`V[L_, s_String, deg___] := Kh`V[L, Characters[s] /. {"0"->0, "1"->1}, deg]
Kh`V[L_, a_List] := List @@ Expand[Kh`S[L, a] /.
  x_Kh`c :> ((Kh`vp @@ x) + (Kh`vm @@ x))]
Kh`V[L_, a_List, deg_Integer] :=
  Select[Kh`V[L, a], (deg == Deg[#] + (Plus @@ a))&]

Kh`d[L_, s_String] := Kh`d[L, Characters[s] /. {"0"->0, "1"->1}]
Kh`d[L_, a_List] := Kh`S[L, a] /. {
  (Kh`c[x_]Kh`c[y_]->Kh`c[z_])*_. :> {
    Kh`vp@x Kh`vp@y -> Kh`vp@z,
    Kh`vp@x Kh`vm@y -> Kh`vm@z,
    Kh`vm@x Kh`vp@y -> Kh`vm@z,
    Kh`vm@x Kh`vm@y -> 0
  },
  (Kh`c[z_]->Kh`c[x_]Kh`c[y_])*_. :>
    {Kh`vp@z -> Kh`vp@x Kh`vm@y + Kh`vm@x Kh`vp@y, Kh`vm@z -> Kh`vm@x Kh`vm@y}
}

Kh`Bracket[L_, r_Integer, deg___] := If[r<0 || r>Length[PD[L]], {0},
  Join @@ ( ((Kh`v @@ #) Kh`V[L, #, deg])& /@
    Permutations[Join[Table[0, {Length[PD[L]] - r}], Table[1, {r}]]]
  )
]

Kh`CC[L_, r_Integer, deg_Integer] := Kh`Bracket[L, r+NegativeCrossings[L], deg-PositiveCrossings[L]+2NegativeCrossings[L]]

Kh`d[L_][expr_] := Expand[expr] /. s_*a_Kh`v :> Expand[
  sign=1; Sum[ If[ a[[i]]==0,
    sign * ReplacePart[a, 1, i] * s /.  Kh`d[L, List @@ ReplacePart[a, "*", i]],
    sign *= -1; 0
  ], {i, Length[a]} ]
]

Options[Kh`Betti] = {Modulus -> Infinity}
Rank[L_, r_Integer, deg_Integer, opts___] := Rank[L,r,deg,opts] = (
  modulus = (Modulus /. {opts} /. Options[Kh`Betti]);
  Off[Solve::svars];
  b0 = Kh`CC[L, r, deg]; l1 = Length[b1 = Kh`CC[L, r+1, deg]];
  eqs = (#==0)& /@ Kh`d[L][b0] /. MapThread[Rule, {b1, vars = Array[b, l1]}];
  rk = Which[b0 == {} || l1 == 0, 0,
    modulus === Infinity, Length[First[Solve[eqs, vars]]],
    True, Length[First[Solve[
      Append[eqs, Modulus == modulus], vars, Mode -> Modular
    ]]] - 1
  ];
  On[Solve::svars]; rk
)
Kh`Betti[L_, r_Integer, deg_Integer, opts___] := Kh`Betti[L,r,deg,opts] =
  Length[Kh`CC[L,r,deg]]-Rank[L,r,deg,opts]-Rank[L,r-1,deg,opts];

Kh`qBetti[L_, r_Integer, opts___] := (
  degs = Union[Deg /@ Kh`Bracket[L, r+NegativeCrossings[L]]] + PositiveCrossings[L] - NegativeCrossings[L] + r;
  Function @@ {(Kh`Betti[L, r, #, opts]& /@ degs) . #^degs}
)

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

Kh[L_PD, opts___] := Kh[L, opts] = Function @@ {Expand[
  Sum[#2^r * Kh`qBetti[L, r, opts][#1], {r, -NegativeCrossings[L], Length[PD[L]]-NegativeCrossings[L]}]
]}
Kh[L_, opts___] := Kh[PD[L], opts]

End[]; EndPackage[]
