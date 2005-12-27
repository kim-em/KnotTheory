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
  CreditMessage["The HOMFLYPT program was written by Scott Morrison."];
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
