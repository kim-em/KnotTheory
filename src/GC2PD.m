BeginPackage["KnotTheory`"]

PD::about = "
  The GaussCode to PD conversion was written by Siddarth Sankaran at
  the University of Toronto in the summer of 2005.
"

Begin["`GaussCode`"]

PD[GaussCode[]] = PD[Loop[1]]

PD[in_GaussCode] := 
    Module[ {chords=List@@in,
        int = Range[Max[List@@in]] /. x_Integer \[Rule] {}, 
        dirlist = Table[0, {Max[List@@in]}], edgelist, output={}, ol={{}} },

      CreditMessage["The GaussCode to PD conversion was written by Siddarth Sankaran at the University of Toronto in the summer of 2005."];
      
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
