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

	CreditMessage["DrawMorseLink was written by Siddarth Sankaran
at the University of Toronto in the summer of 2005."];

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
