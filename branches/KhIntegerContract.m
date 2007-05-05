twoInvertible=True;
invertibleQ[r_]:= If[(Abs[r]<=1)||(twoInvertible&&Abs[r]==2),True,Print["Refusing to invert ", r];False]

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
                ((i /. e2b0) === (j /. e2b1)) && invertibleQ[r]
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
