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
    CreditMessage["MorseLink was added to KnotTheory` by Siddarth Sankaran
at the University of Toronto in the summer of 2005."];
      
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
