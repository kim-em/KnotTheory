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
