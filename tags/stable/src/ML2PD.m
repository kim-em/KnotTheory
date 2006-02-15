BeginPackage["KnotTheory`"]; 

Begin["`MorseLink2PD`"]; 

PD[MorseLink[Cup[1,2], Cap[2 , 1] ] ] := PD[Loop[1]]; 

PD[in_MorseLink] := Module[ {pos, arrow, strands = {}, edgecount = 0, n, chains = 1, output = {}, a, b, x, y, i}, 
	For[n = 1, n <= Length[in], n++, 
		Switch[in[[n]], 
			_Cup, 
				pos = Min[in[[n,1]], in[[n,2]]]; 
				edgecount++; 
				strands = Insert[Insert[strands, edgecount, pos], edgecount, pos]; , 
			_Cap,
				chains *= arrow[strands[[ in[[n,1]] ]], strands[[ in[[n,2]] ]] ];
				pos = Min[ in[[n,1]], in[[n,2]] ]; 
				output = output /. strands[[ in[[n,1]] ]] -> strands[[ in[[n,2]] ]]; 
				chains = chains /. strands[[in[[n,1]]]] -> strands[[in[[n,2]]]]; 
				strands = Delete[strands, {{pos}, {pos + 1}}]; , 
			_X, 
				pos = in[[n,1]]; 
				{x, y} = {strands[[pos]], strands[[pos + 1]]}; 
				a = strands[[pos]] = ++edgecount; b = strands[[pos + 1]] = ++edgecount; 
				Switch[ {in[[n,2]], in[[n,3]], in[[n,4]]}, 
					{Under, Up, _}, AppendTo[output, X[x, y, b, a]], 
					{Under, Down, _},  AppendTo[output, X[b, a, x, y]], 
					{Over, _, Up}, AppendTo[output, X[y, b, a, x]], 
					{Over, _, Down}, AppendTo[output, X[a, x, y, b] ] 
				]; 
				If[TrueQ[in[[n,3]] == Up], chains *= arrow[x, b], chains *= arrow[b, x] ];
				If[TrueQ[in[[n,4]] == Up], chains *= arrow[y, a], chains *= arrow[a, y] ]; 
		]; 
	]; 
	chains = chains //. arrow[a_, b___, c_]*arrow[c_, d___, e_] :> arrow[a, b, c, d,  e] //. arrow[a___, x_, x_, b___] :> arrow[a, x, b]; chains = chains /.  a_arrow :> Rest[a]; 
	i = Flatten[Apply[List, DeleteCases[List @@ chains, arrow[]^(n_)], {1}]]; 
	output = output /. MapThread[Rule, {i, Range[Length[i]]}]; 
	Return[PD@@output]; 
]; 
End[]; 
EndPackage[]; 
