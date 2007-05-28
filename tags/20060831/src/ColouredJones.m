BeginPackage["KnotTheory`"]

ColouredJones::usage = "ColouredJones[K, n][q] returns the coloured
Jones polynomial of a knot in colour n (i.e., in the (n+1)-dimensional
representation) in the indeterminate q. Some of these polynomials have
been precomputed in KnotTheory`. To force computation, use
ColouredJones[K,n, Program -> \"prog\"][q], with \"prog\" replaced by
one of the two available programs, \"REngine\" or \"Braid\" (including
the quotes). \"REngine\" (default) computes the invariant for closed
knots (as well as links where all components are coloured by the same
integer) directly from the MorseLink presentation of the knot, while
\"Braid\" computes the invariant via a presentation of the knot as a
braid closure.  \"REngine\" will usually be faster, but it might be
better to use \"Braid\" when (roughly): 1) a \"good\" braid
representative is available for the knot, and 2) the length of this
braid is less than the maximum width of the MorseLink presentation of
the knot."

ColouredJones::about = "
The \"REngine\" algorithm was written by Siddarth Sankaran in the
summer of 2005, while the \"Braid\" algorithm was written jointly by
Dror Bar-Natan and Stavros Garoufalidis. Both are based on formulas by
Thang Le and Stavros Garoufalidis; see [Garoufalidis, S. and Le, T.
\"The coloured Jones function is q-holonomic.\" Geom. Top., v9, 2005
(1253-1293)]."

ColoredJones::usage = "Type ColoredJones and see for yourself."

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

qBinomial = QuantumGroups`qBinomial;

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

CJ`k; CJ`q; NotAvailable; Compute; Program

Begin["`CJBraid`"]

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

Options[ColouredJones] = {Compute -> True, Program -> "REngine"};

ColouredJones[K_, n_Integer, opts___] := ColouredJones[K, n, opts] = Module[
  {prog = Program /. {opts} /. Options[ColouredJones]},
  Switch[prog,
    "REngine", KnotTheory`CJREngine`CJ[K,n],
    "Braid", KnotTheory`CJBraid`CJ[BR[K],n,opts]
  ]
];


CJ[b_BR, n_Integer, opts___] := Module[
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
