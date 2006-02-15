BeginPackage["KnotTheory`"]

Kauffman::usage = "
Kauffman[K][a, z] computes the Kauffman polynomial of a knot or link K,
in the variables a and z.
"

Kauffman::about = "
The Kauffman polynomial program was written by Scott Morrison.
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
  CreditMessage["The Kauffman polynomial program was written by Scott Morrison."];
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
