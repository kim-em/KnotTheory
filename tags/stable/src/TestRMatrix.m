BeginPackage["KnotTheory`"]

TestRMatrix::usage = "TestRMatrix[Rp, Rn, McupL, McupR, McapL, McapR]
checks if the invariant associated with the given R-matrices (Rp for
positive crossings, Rn for negative crossings) along with the directed
creation and annihilation M matrices, is indeed an invariant of regular
isotopy (which includes satisfying the Yang-Baxter Equation). See the
manual entry for REngine for notational conventions. You may skip a
test by using one or more of the options in TestRMatrix[Rp, Rn, McupL,
McupR, McapL, McapR, opts] : SlideTest -> False, R2Test -> False,
R3Test -> False."

SlideTest;R2Test;R3Test;
Options[TestRMatrix] = {SlideTest -> True, R3Test -> True, R2Test -> True};

Begin["`TestRMatrix`"]

TestRMatrix[r_, rb_, mcupl_, mcupr_, mcapl_, mcapr_, opts___] := 
	Module[ {i, a, b,t, n=Length[mcupl], Rengine, slidetest, r2test, r3test, r2flag, slideflag, r3flag},
	
	{slideflag, r2flag, r3flag} = {SlideTest, R2Test, R3Test} /. {opts} /. Options[TestRMatrix];

(*RENGINE*************************************************************)	
	Rengine[instr_, ml_MorseLink, rmatrix_, rbar_, mcupL_, mcupR_, mcapL_, mcapR_] :=

	Module[ { a,b,x,y, in,F, k, str, n=Length[mcupl], sumvars,v, instrands, outstrands,count },
		R[a_Integer, b_Integer, x_Integer, y_Integer] := rmatrix[[n*(x-1) + y, (a-1)n+b]];
		Rbar[a_Integer, b_Integer, x_Integer, y_Integer] := rbar[[ (x-1)n + y, (a-1)n + b]];
		Mcupl[a_Integer, b_Integer] := mcupL[[a,b]];
		Mcupr[a_Integer, b_Integer] := mcupR[[a,b]];
		Mcapl[a_Integer, b_Integer] := mcapL[[a,b]];
		Mcapr[a_Integer, b_Integer] := mcapR[[a,b]];
		
		in = ml /. {
			X[n_, u_, Up, Up] :> Sequence[Cup[n+1,n], Cup[n+2,n+1], X[n+2, If[u===Under, Under, Over], Down, Down], Cap[n+4, n+3], Cap[n+3, n+2] ],
		 	X[n_, u_, Up, Down] :> Sequence[Cup[n+2, n+3], X[n+1, If[u===Under,Over,Under], Down, Down], Cap[n, n+1] ],
			X[n_, u_, Down, Up] :> Sequence[Cup[n+1, n], X[n+1, If[u===Under,Over,Under], Down, Down], Cap[n+3, n+2]]
			};
		F[0]=1;
		instrands = str = Table[var[i], {i, instr}];
		(* Print["instr = ",instrands]; *)
		count = instr;
		For[k=1, k<= Length[in], k++,
			Switch[in[[k]],
				_Cup,
					x = var[++count]; y=var[++count];
					{a,b} = {in[[k,1]], in[[k,2]]};
					str = If[Length[str] != 0, Insert[ Insert[str, y, Min[a,b]], x, Min[a,b ] ], {x,y}];
					F[k] = F[k-1]*If[a<b, Mcupl[x, y], Mcupr[x, y]];
				,
				_X,
				{a,b,x,y} = {str[[ in[[k,1]] ]], str[[ in[[k,1]] + 1 ]],var[++count], var[++count]};
				str[[in[[k,1]] ]] = x;
				str[[in[[k,1]] + 1]] = y;
				sumvars = Cases[{a,b}, var[n_] /; n > instr];
				If[Length[sumvars] > 0,
					F[k] = Sum[F[k-1]*If[in[[k,2]] === Over, R[x,y,a,b], Rbar[x,y,a,b]] /. Table[sumvars[[i]] -> v[i], {i, Length[sumvars]} ], Evaluate[Sequence@@Table[{v[i], n}, {i, Length[sumvars]}]]];,
					F[k] = F[k-1]*If[in[[k,2]] === Over, R[x,y,a,b], Rbar[x,y,a,b]];
				];
				,
				_Cap,
				{a,b} = {str[[in[[k,1]]]], str[[in[[k,2]]]] };
				str = Delete[str, {{in[[k,1]]}, {in[[k,2]]}}];
				sumvars = Cases[{a,b}, var[n_] /; n > instr];
				If[Length[sumvars] > 0,
					F[k] = Sum[F[k-1]*If[in[[k,1]] < in[[k,2]], Mcapl[a,b], Mcapr[b,a]] /. Table[sumvars[[i]] -> v[i], {i, Length[sumvars]} ], Evaluate[Sequence@@Table[{v[i], n}, {i, Length[sumvars]} ] ] ];,
					F[k] = F[k-1]*If[in[[k,1]] < in[[k,2]], Mcapl[a,b], Mcapr[b,a]];
				];
			];
		];
		outstrands = str;
		(*Print["outstr = ", outstrands]; *)	
		Return[F[Length[in]] /. Table[instrands[[i]] -> sin[i], {i, Length[instrands]}] /. Table[outstrands[[i]] -> sout[i], {i, Length[outstrands]}]] ;

];
		
(*TEST DEF'NS***************************************************************)

(* slide move*)
slidetest[1,1] = MorseLink[X[1, Over, Up, Up], Cap[2,3]];
slidetest[1,2] = MorseLink[X[2, Under, Up, Down], Cap[1,2]];
slidetest[2,1] = MorseLink[X[1, Over, Up, Down], Cap[2,3]];
slidetest[2,2] = MorseLink[X[2, Under, Down, Down], Cap[1,2]];
slidetest[3,1] = MorseLink[X[1, Over, Down, Up], Cap[3,2]];
slidetest[3,2] = MorseLink[X[2, Under, Up, Up], Cap[2,1]];
slidetest[4,1] = MorseLink[X[1, Over, Down, Down], Cap[3,2]];
slidetest[4,2] = MorseLink[X[2, Under, Down, Up], Cap[2,1]];
slidetest[5,1] = MorseLink[X[1, Under, Up, Up], Cap[2,3]];
slidetest[5,2] = MorseLink[X[2, Over, Up, Down], Cap[1,2]];
slidetest[6,1] = MorseLink[X[1, Under, Up, Down], Cap[2,3]];
slidetest[6,2] = MorseLink[X[2, Over, Down, Down], Cap[1,2]];
slidetest[7,1] = MorseLink[X[1, Under, Down, Up], Cap[3,2]];
slidetest[7,2] = MorseLink[X[2, Over, Up, Up], Cap[2,1]];
slidetest[8,1] = MorseLink[X[1, Under, Down, Down], Cap[3,2]];
slidetest[8,2] = MorseLink[X[2, Over, Down, Up], Cap[2,1]];
(*cup*)
slidetest[9,1] = MorseLink[Cup[2,3], X[1, Over, Down, Down]];
slidetest[9,2] = MorseLink[Cup[1,2], X[2, Under, Up, Down]];
slidetest[10,1] = MorseLink[Cup[2,3], X[1, Over, Up, Down]];
slidetest[10,2] = MorseLink[Cup[1,2], X[2, Under, Up, Up]];
slidetest[11,1] = MorseLink[Cup[3,2], X[1, Over, Down, Up]];
slidetest[11,2] = MorseLink[Cup[2,1], X[2, Under, Down, Down]];
slidetest[12,1] = MorseLink[Cup[3,2], X[1, Over, Up, Up]];
slidetest[12,2] = MorseLink[Cup[2,1], X[2, Under, Down, Up]];
slidetest[13,1] = MorseLink[Cup[2,3], X[1, Under, Down, Down]];
slidetest[13,2] = MorseLink[Cup[1,2], X[2, Over, Up, Down]];
slidetest[14,1] = MorseLink[Cup[2,3], X[1, Under, Up, Down]];
slidetest[14,2] = MorseLink[Cup[1,2], X[2, Over, Up, Up]];
slidetest[15,1] = MorseLink[Cup[3,2], X[1, Under, Down, Up]];
slidetest[15,2] = MorseLink[Cup[2,1], X[2, Over, Down, Down]];
slidetest[16,1] = MorseLink[Cup[3,2], X[1, Under, Up, Up]];
slidetest[16,2] = MorseLink[Cup[2,1], X[2, Over, Down, Up]];

(* R3 *)
r3test[1,1] = 
    MorseLink[ X[2, Under, Up,Up], X[1, Under, Up, Up], X[2, Over, Up, Up]];
r3test[1,2] = 
    MorseLink[X[1, Over, Up, Up], X[2, Under, Up, Up], X[1, Under, Up, Up]];
r3test[2,1] = 
    MorseLink[ X[2, Under, Up,Down], X[1, Under, Up, Down], 
      X[2, Over, Up, Up]];
r3test[2,2] = 
    MorseLink[X[1, Over, Up, Up], X[2, Under, Up, Down], 
      X[1, Under, Up, Down]];
r3test[3,1] = 
    MorseLink[ X[2, Under, Down,Up], X[1, Under, Up, Up], 
      X[2, Over, Up, Down]];
r3test[3,2] = 
    MorseLink[X[1, Over, Up, Down], X[2, Under, Up, Up], 
      X[1, Under, Down, Up]];
r3test[4,1] = 
    MorseLink[ X[2, Under, Down,Down], X[1, Under, Up, Down], 
      X[2, Over, Up, Down]];
r3test[4,2] = 
    MorseLink[X[1, Over, Up, Down], X[2, Under, Up, Down], 
      X[1, Under, Down, Down]];
r3test[5,1] = 
    MorseLink[ X[2, Under, Up,Up], X[1, Under, Down, Up], 
      X[2, Over, Down, Up]];
r3test[5,2] = 
    MorseLink[X[1, Over, Down, Up], X[2, Under, Down, Up], 
      X[1, Under, Up, Up]];
r3test[6,1] = 
    MorseLink[ X[2, Under, Up,Down], X[1, Under, Down, Down], 
      X[2, Over, Down, Up]];
r3test[6,2] = 
    MorseLink[X[1, Over,Down, Up ], X[2, Under, Down,Down], 
      X[1, Under, Up, Down]];
r3test[7,1] = 
    MorseLink[ X[2, Under, Down,Up], X[1, Under, Down, Up], 
      X[2, Over, Down, Down]];
r3test[7,2] = 
    MorseLink[X[1, Over, Down, Down], X[2, Under, Down, Up], 
      X[1, Under, Down, Up]];
r3test[8,1] = 
    MorseLink[ X[2, Under, Down,Down], X[1, Under, Down, Down], 
      X[2, Over, Down, Down]];
r3test[8,2] = 
    MorseLink[X[1, Over, Down, Down], X[2, Under, Down, Down], 
      X[1, Under, Down, Down]];
r3test[9,1] = 
    MorseLink[ X[2, Over, Up,Up], X[1, Under, Up, Up], X[2, Under, Up, Up]];
r3test[9,2] = 
    MorseLink[X[1, Under, Up, Up], X[2, Under, Up, Up], X[1, Over, Up, Up]];
r3test[10,1] = 
    MorseLink[ X[2, Over, Up,Down], X[1, Under, Up, Down], 
      X[2, Under, Up, Up]];
r3test[10,2] = 
    MorseLink[X[1, Under, Up, Up], X[2, Under, Up, Down], 
      X[1, Over, Up, Down]];
r3test[11,1] = 
    MorseLink[ X[2, Over, Down,Up], X[1, Under, Up, Up], 
      X[2, Under, Up, Down]];
r3test[11,2] = 
    MorseLink[X[1, Under, Up, Down], X[2, Under, Up, Up], 
      X[1, Over, Down, Up]];
r3test[12,1] = 
    MorseLink[ X[2, Over, Down,Down], X[1, Under, Up, Down], 
      X[2, Under, Up, Down]];
r3test[12,2] = 
    MorseLink[X[1, Under, Up, Down], X[2, Under, Up, Down], 
      X[1, Over, Down, Down]];
r3test[13,1] = 
    MorseLink[ X[2, Over, Up,Up], X[1, Under, Down, Up], 
      X[2, Under, Down, Up]];
r3test[13,2] = 
    MorseLink[X[1, Under, Down, Up], X[2, Under, Down, Up], 
      X[1, Over, Up, Up]];
r3test[14,1] = 
    MorseLink[ X[2, Over, Up,Down], X[1, Under, Down, Down], 
      X[2, Under, Down, Up]];
r3test[14,2] = 
    MorseLink[X[1, Under,Down, Up ], X[2, Under, Down,Down], 
      X[1, Over, Up, Down]];
r3test[15,1] = 
    MorseLink[ X[2, Over, Down,Up], X[1, Under, Down, Up], 
      X[2, Under, Down, Down]];
r3test[15,2] = 
    MorseLink[X[1, Under, Down, Down], X[2, Under, Down, Up], 
      X[1, Over, Down, Up]];
r3test[16,1] = 
    MorseLink[ X[2, Over, Down,Down], X[1, Under, Down, Down], 
      X[2, Under, Down, Down]];
r3test[16,2] = 
    MorseLink[X[1, Under, Down, Down], X[2, Under, Down, Down], 
      X[1, Over, Down, Down]];
r3test[17,1] = 
    MorseLink[ X[2, Under, Up,Up], X[1, Over, Up, Up], X[2, Over, Up, Up]];
r3test[17,2] = 
    MorseLink[X[1, Over, Up, Up], X[2, Over, Up, Up], X[1, Under, Up, Up]];
r3test[18,1] = 
    MorseLink[ X[2, Under, Up,Down], X[1, Over, Up, Down], 
      X[2, Over, Up, Up]];
r3test[18,2] = 
    MorseLink[X[1, Over, Up, Up], X[2, Over, Up, Down], 
      X[1, Under, Up, Down]];
r3test[19,1] = 
    MorseLink[ X[2, Under, Down,Up], X[1, Over, Up, Up], 
      X[2, Over, Up, Down]];
r3test[19,2] = 
    MorseLink[X[1, Over, Up, Down], X[2, Over, Up, Up], 
      X[1, Under, Down, Up]];
r3test[20,1] = 
    MorseLink[ X[2, Under, Down,Down], X[1, Over, Up, Down], 
      X[2, Over, Up, Down]];
r3test[20,2] = 
    MorseLink[X[1, Over, Up, Down], X[2, Over, Up, Down], 
      X[1, Under, Down, Down]];
r3test[21,1] = 
    MorseLink[ X[2, Under, Up,Up], X[1, Over, Down, Up], 
      X[2, Over, Down, Up]];
r3test[21,2] = 
    MorseLink[X[1, Over, Down, Up], X[2, Over, Down, Up], 
      X[1, Under, Up, Up]];
r3test[22,1] = 
    MorseLink[ X[2, Under, Up,Down], X[1, Over, Down, Down], 
      X[2, Over, Down, Up]];
r3test[22,2] = 
    MorseLink[X[1, Over,Down, Up ], X[2, Over, Down,Down], 
      X[1, Under, Up, Down]];
r3test[23,1] = 
    MorseLink[ X[2, Under, Down,Up], X[1, Over, Down, Up], 
      X[2, Over, Down, Down]];
r3test[23,2] = 
    MorseLink[X[1, Over, Down, Down], X[2, Over, Down, Up], 
      X[1, Under, Down, Up]];
r3test[24,1] = 
    MorseLink[ X[2, Under, Down,Down], X[1, Over, Down, Down], 
      X[2, Over, Down, Down]];
r3test[24,2] = 
    MorseLink[X[1, Over, Down, Down], X[2, Over, Down, Down], 
      X[1, Under, Down, Down]];
r3test[25,1] = 
    MorseLink[ X[2, Over, Up,Up], X[1, Over, Up, Up], X[2, Under, Up, Up]];
r3test[25,2] = 
    MorseLink[X[1, Under, Up, Up], X[2, Over, Up, Up], X[1, Over, Up, Up]];
r3test[26,1] = 
    MorseLink[ X[2, Over, Up,Down], X[1, Over, Up, Down], 
      X[2, Under, Up, Up]];
r3test[26,2] = 
    MorseLink[X[1, Under, Up, Up], X[2, Over, Up, Down], 
      X[1, Over, Up, Down]];
r3test[27,1] = 
    MorseLink[ X[2, Over, Down,Up], X[1, Over, Up, Up], 
      X[2, Under, Up, Down]];
r3test[27,2] = 
    MorseLink[X[1, Under, Up, Down], X[2, Over, Up, Up], 
      X[1, Over, Down, Up]];
r3test[28,1] = 
    MorseLink[ X[2, Over, Down,Down], X[1, Over, Up, Down], 
      X[2, Under, Up, Down]];
r3test[28,2] = 
    MorseLink[X[1, Under, Up, Down], X[2, Over, Up, Down], 
      X[1, Over, Down, Down]];
r3test[29,1] = 
    MorseLink[ X[2, Over, Up,Up], X[1, Over, Down, Up], 
      X[2, Under, Down, Up]];
r3test[29,2] = 
    MorseLink[X[1, Under, Down, Up], X[2, Over, Down, Up], 
      X[1, Over, Up, Up]];
r3test[30,1] = 
    MorseLink[ X[2, Over, Up,Down], X[1, Over, Down, Down], 
      X[2, Under, Down, Up]];
r3test[30,2] = 
    MorseLink[X[1, Under,Down, Up ], X[2, Over, Down,Down], 
      X[1, Over, Up, Down]];
r3test[31,1] = 
    MorseLink[ X[2, Over, Down,Up], X[1, Over, Down, Up], 
      X[2, Under, Down, Down]];
r3test[31,2] = 
    MorseLink[X[1, Under, Down, Down], X[2, Over, Down, Up], 
      X[1, Over, Down, Up]];
r3test[32,1] = 
    MorseLink[ X[2, Over, Down,Down], X[1, Over, Down, Down], 
      X[2, Under, Down, Down]];
r3test[32,2] = 
    MorseLink[X[1, Under, Down, Down], X[2, Over, Down, Down], 
      X[1, Over, Down, Down]];

(*r2  Horizontal*)

r2test[1,1] = 
    MorseLink[Cup[2,3], X[1, Over, Up, Down], X[3, Under, Up, Down], 
      Cap[2,3]];
r2test[1,2] = MorseLink[Cap[1,2], Cup[1,2]]; 
r2test[2,1] = 
    MorseLink[Cup[3,2], X[1, Over, Up, Up], X[3, Under, Down, Down], 
      Cap[2,3]];
r2test[2,2] = MorseLink[Cap[1,2], Cup[2,1]];
r2test[3,1] = 
    MorseLink[Cup[2,3], X[1, Over, Down, Down], X[3, Under, Up, Up], 
      Cap[3,2]];
r2test[3,2] = MorseLink[Cap[2,1], Cup[1,2]];
r2test[4,1] = 
    MorseLink[Cup[3,2], X[1, Over, Down, Up], X[3, Under, Down, Up], 
      Cap[3,2]];
r2test[4,2] = MorseLink[Cap[2,1], Cup[2,1]];
r2test[5,1] = 
    MorseLink[Cup[2,3], X[1, Under, Up, Down], X[3, Over, Up, Down], 
      Cap[2,3]];
r2test[5,2] = MorseLink[Cap[1,2], Cup[1,2]]; 
r2test[6,1] = 
    MorseLink[Cup[3,2], X[1, Under, Up, Up], X[3, Over, Down, Down], 
      Cap[2,3]];
r2test[6,2] = MorseLink[Cap[1,2], Cup[2,1]];
r2test[7,1] = 
    MorseLink[Cup[2,3], X[1, Under, Down, Down], X[3, Over, Up, Up], 
      Cap[3,2]];
r2test[7,2] = MorseLink[Cap[2,1], Cup[1,2]];
r2test[8,1] = 
    MorseLink[Cup[3,2], X[1, Under, Down, Up], X[3, Over, Down, Up], 
      Cap[3,2]];
r2test[8,2] = MorseLink[Cap[2,1], Cup[2,1]];
	
	
	
		(*prelim *)
		Print["Cancel move: ", If[(mcupl.mcapl == IdentityMatrix[n]) && (mcupr.mcapr == IdentityMatrix[n]), "passed", "failed", "failed"]];
		Print["Loop value: ", If[Tr[mcupl.Transpose[mcapr]] == Tr[mcupr.Transpose[mcapl]], Tr[mcupl.Transpose[mcapr]], "failed", "failed"]];
		
		(*slide move*)
		If[slideflag,
		For[ i=1, i<= 8, i++,
			{a,b} = Rengine[3, slidetest[i,#], r, rb, mcupl, mcupr, mcapl, mcapr] &/@ {1,2};
			t= Flatten[Table[{i,j,k, l}, {i,n}, {j,n}, {k,n}, {l,n}],3];
		(*	Print[Table[Expand[{a,b}]/. {sin[1] -> t[[j,1]], sin[2] -> t[[j,2]], sin[3] -> t[[j,3]], sout[1] -> t[[j,4]]} , {j, Length[t]} ]];*)
			Print["Slide test ", i, ": ",
				If[Apply[ And, Table[TrueQ[Expand[a /. {sin[1] -> t[[j,1]], sin[2] -> t[[j,2]], sin[3] -> t[[j,3]], sout[1] -> t[[j,4]]}] == Expand[b /. {sin[1] -> t[[j,1]], sin[2] -> t[[j,2]], sin[3] -> t[[j,3]], sout[1] -> t[[j,4]]} ] ] , {j, Length[t]}]], "passed", "failed"]
			];
		];
		
		For[ i=9, i<= 16, i++,
			{a,b} = Rengine[3, slidetest[i,#], r, rb, mcupl, mcupr, mcapl, mcapr] &/@ {1,2};
			t= Flatten[Table[{i,j,k, l}, {i,n}, {j,n}, {k,n}, {l,n}],3];
		(*	Print[Table[Expand[{a,b}]/. {sin[1] -> t[[j,1]], sin[2] -> t[[j,2]], sin[3] -> t[[j,3]], sout[1] -> t[[j,4]]} , {j, Length[t]} ]];*)
			Print["Slide test ", i, ": ",
				If[Apply[ And, Table[TrueQ[Expand[a /. {sin[1] -> t[[j,1]], sout[1] -> t[[j,2]], sout[2] -> t[[j,3]], sout[3] -> t[[j,4]]}] == Expand[b /. {sin[1] -> t[[j,1]], sout[1] -> t[[j,2]], sout[2] -> t[[j,3]], sout[3] -> t[[j,4]]} ] ] , {j, Length[t]}]], "passed", "failed"]
			];
		];
	];
		
		(*R2*)
		If[r2flag,
		For[ i=1, i<= 8, i++,
			{a,b} = Rengine[2, r2test[i,#], r, rb, mcupl, mcupr, mcapl, mcapr] &/@ {1,2};
			t= Flatten[Table[{i,j,k, l}, {i,n}, {j,n}, {k,n}, {l,n}],3];
			Print["R2 test ", i, ": ",
				If[Apply[ And, Table[TrueQ[Expand[a/. {sin[1] -> t[[j,1]], sin[2] -> t[[j,2]], sout[1] -> t[[j,3]], sout[2] -> t[[j,4]]}] == Expand[b/. {sin[1] -> t[[j,1]], sin[2] -> t[[j,2]], sout[1] -> t[[j,3]], sout[2] -> t[[j,4]]}] ] , {j, Length[t]}] ], "passed", "failed"]
			];
		];
];

		(*R3*)
			If[r3flag,		
			For[ i=1, i<= 32, i++,
			{a,b} = Rengine[3, r3test[i,#], r, rb, mcupl, mcupr, mcapl, mcapr] &/@ {1,2};
			t= Flatten[Table[{i,j,k, l, m, p}, {i,n}, {j,n}, {k,n}, {l,n}, {m,n}, {p,n}],5];
			Print["R3 test ", i, ": ",
				If[Apply[ And, Table[TrueQ[Expand[a/. {sin[1] -> t[[j,1]], sin[2] -> t[[j,2]], sin[3]-> t[[j,3]], sout[1] -> t[[j,4]], sout[2] -> t[[j,5]], sout[3] -> t[[j,6]] }] == Expand[b/. {sin[1] -> t[[j,1]], sin[2] -> t[[j,2]], sin[3]-> t[[j,3]], sout[1] -> t[[j,4]], sout[2] -> t[[j,5]], sout[3] -> t[[j,6]] }] ] , {j, Length[t]}] ], "passed", "failed"]
			];
		];
		];
	];
	
	
End[];EndPackage[];

