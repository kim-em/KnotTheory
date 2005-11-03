BeginPackage["KnotTheory`"]

REngine::usage = "REngine[K, Rp, Rn, Mcupl, Mcupr, Mcapl Mcapr] returns
the invariant associated with the given R-matrices (Rp for positive
crossings, Rn for negative crossings) and oriented creation and
annihilation M matrices, of the oriented knot or link K. See the Manual
for details of convention. Note that REngine does not verify that the
given matrices actually define an invariant, use TestRMatrix[..] for
this purpose."

REngine::about = "REngine was written by Siddarth Sankaran at the
University of Toronto, in the summer of 2005."

Begin["`REngine`"]

REngine[in_, rmatrix_, rbar_, mcupl_, mcupr_, mcapl_, mcapr_] /; Head[in]=!= MorseLink := REngine[MorseLink[in], rmatrix, rbar, mcupl, mcupr, mcapr, mcapr];

REngine[ml_MorseLink, rmatrix_, rbar_, mcupl_, mcupr_, mcapl_, mcapr_] := 
	Module[ {F, k, var, varl, varm, varr, preprule, cr, capruler, caprulel, R, Rbar, n=Length[mcupl], a,b} ,

		R[a_Integer, b_Integer, x_Integer, y_Integer] := (R[a,b,x,y] = rmatrix[[n*(x-1) + y, (a-1)n+b]]);
		Rbar[a_Integer, b_Integer, x_Integer, y_Integer] := (Rbar[a,b,x,y] = rbar[[ (x-1)n + y, (a-1)n + b]]);
		
		cr[Over, Down, Down] := (cr[Over, Down, Down] = Dispatch[Flatten[Table[varm[a,b] -> Sum[varm[x,y]*R[a,b,x,y], {x,n}, {y,n}], {a,n}, {b,n}]]]);
		cr[Under, Down, Down] := (cr[Under, Down, Down] = Dispatch[Flatten[Table[varm[a,b] -> Sum[varm[x,y]*Rbar[a,b,x,y], {x,n}, {y,n}], {a,n}, {b,n}]]]);

		cr[Over, Down, Up] := (cr[Over, Down, Up] = Dispatch[Flatten[Table[varm[a,b] -> Sum[varm[x,y]*mcupr[[x,c]]*Rbar[c,a,y,d]*mcapr[[d,b]], {c,n}, {d,n}, {x,n}, {y,n}], {a,n}, {b,n}]]]);
		cr[Under, Down, Up] := (cr[Under, Down, Up] = Dispatch[Flatten[Table[varm[a,b] -> Sum[varm[x,y]*mcupr[[x,c]]*R[c,a,y,d]*mcapr[[d,b]], {c,n}, {d,n}, {x,n}, {y,n}], {a,n}, {b,n}]]]);

	cr[Over, Up, Down] := (cr[Over, Up, Down] = Dispatch[Flatten[Table[varm[a,b] -> Sum[varm[x,y]*mcapl[[a,c]]*Rbar[b,d,c,x]*mcupl[[d,y]], {c,n}, {d,n}, {x,n}, {y,n} ], {a,n}, {b,n}]]]);
	cr[Under, Up, Down] := (cr[Under, Up, Down] = Dispatch[Flatten[Table[varm[a,b] -> Sum[varm[x,y]*mcapl[[a,c]]*R[b,d,c,x]*mcupl[[d,y]], {c,n}, {d,n}, {x,n}, {y,n} ], {a,n}, {b,n}]]]);
	
	cr[Over, Up, Up] := (cr[Over, Up,Up] = Dispatch[Flatten[Table[varm[a,b] -> Sum[varm[x,y]*mcupr[[x,f]]*mcupr[[y,e]]*R[e,f,d,c]*mcapr[[c,a]]*mcapr[[d,b]], {c,n}, {d,n}, {e,n}, {f,n}, {x,n}, {y,n}], {a,n}, {b,n}]]]);
	cr[Under, Up, Up] := (cr[Under, Up,Up] = Dispatch[Flatten[Table[varm[a,b] -> Sum[varm[x,y]*mcupr[[x,f]]*mcupr[[y,e]]*Rbar[e,f,d,c]*mcapr[[c,a]]*mcapr[[d,b]], {c,n}, {d,n}, {e,n}, {f,n}, {x,n}, {y,n}], {a,n}, {b,n}]]]);


	caprulel := (caprulel = Dispatch[Flatten[Table[varm[a,b] -> mcapl[[a,b]], {a,n}, {b,n}]]]);
	capruler := (capruler = Dispatch[Flatten[Table[varm[a,b] -> mcapr[[a,b]], {a,n}, {b,n}]]]);
	

	preprule[a_] := (preprule[a] = var[a0___, b0_, c0_, d0___]/; (Length[List[a0]] +1 == a) :> varl[a0]varm[b0,c0]varr[d0]);
	postrule := (varl[a0___]*varm[b0____]*varr[d0___] :> var[a0,b0,d0]);
	 
	F[0] = var[];	
	For[k = 1, k<= Length[ml], k++,
		(*Print[k, " ", ml[[k]]];*)
		Switch[ml[[k]],
		_Cup,
		a = Min[ml[[k,1]], ml[[k,2]]];
		If[ml[[k,1]] < ml[[k,2]],
			F[k] = F[k-1] /. var[a0___, b0___]/; (Length[List[a0]] + 1 == a) :> Sum[var[a0, x,y, b0]*mcupl[[x,y]], {x,n}, {y,n}],
			F[k] = F[k-1] /. var[a0___, b0___]/; (Length[List[a0]] + 1 == a) :> Sum[var[a0, x,y, b0]*mcupr[[x,y]], {x,n}, {y,n}]
		];,
		
		_Cap,		
		
		a = Min[ml[[k,1]], ml[[k,2]]];		
		If[ml[[k,1]] < ml[[k,2]],
			F[k] = Expand[F[k-1] /. preprule[a] /. caprulel]  /. varl[x___]varr[z___] :> var[x,z]/. var[] -> 1,	
			F[k] = Expand[F[k-1] /. preprule[a] /. capruler] /. varl[x___]varr[z___] :> var[x,z]/. var[] -> 1;
		];,
		_X,
		a = ml[[k,1]];
	
		(*Module[ {a1,a2,a3,a4}, 
		
		Print["a1"]; 
		a1 = F[k-1] /. preprule[a];
		Print["a2"];
		a2 = a1 /. cr[ml[[k,2]], ml[[k,3]], ml[[k,4]] ];
		Print["a3"];
		a3 = Expand[a2];
		Print["a4"];
		a4 = (a3 /. varl[x___]varm[y___]varr[z___] :> var[x,y,z] )/. var[] -> 1;
		Print["a5"];
		F[k] = a4; 
		]  
		*)
		F[k] = (Expand[F[k-1] /. preprule[a] /. cr[ml[[k,2]], ml[[k,3]], ml[[k,4]] ] ]) /. varl[x___]varm[y___]varr[z___] :> var[x,y,z] /. var[] -> 1;

		
		
		];
	(*Print["OUTPUT ",k,": ",F[k]];*) 
	];
	Return[F[Length[ml]]];


]

End[];EndPackage[];
