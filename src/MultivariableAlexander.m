BeginPackage["KnotTheory`"];

MultivariableAlexander::usage = "
MultivariableAlexander[L][t] returns the multivariable Alexander polynomial
of a link L as a function of the variable t[1], t[2], ..., t[c], where c
is the number of components of L.
"

MultivariableAlexander::about = "The multivariable Alexander program was
written by Dan Carney at the University of Toronto in the summer of 2005."

Begin["`MultivariableAlexanderPrivate`"];

MultivariableAlexander[Link[n_, t_, k_]] := (
  Needs["KnotTheory`MultivariableAlexander4Links`"];
  Unset[MultivariableAlexander[Link[n1_, t1_, k1_]]];
  MultivariableAlexander[Link[n, t, k]]
)

MultivariableAlexander[K_] /; Head[K] =!= BR := ( MultivariableAlexander[BR[K]] )

MultivariableAlexander[ BR[ NotAvailable ] ] := ( error & )

MultivariableAlexander[ BR[ 1, {} ] ] := ( 1 & )

MultivariableAlexander[ BR[ 2, braidWord_List ] ] := 
	( MultivariableAlexander[ BR[ 3, Append[ braidWord, 2 ] ] ] )

MultivariableAlexander[BR[numberOfStrings_Integer, permutations_List]] /; numberOfStrings >= 3 := MultivariableAlexander[BR[numberOfStrings, permutations]] = Module[
{data},

    	If[ numberOfStrings > 2 , Null, Return[ error ];, Return[ error ] ];

	If[ 	!Scan[ If[ Abs[ #1 ] < numberOfStrings, Null, Return[ False ]; ]; & , permutations ], Return[ error ]; ];
	
	data[ braidWidth ] = numberOfStrings;
	data[ braidWord ] = permutations;
	data[ braidHead ] = Range[ numberOfStrings ];
	
	MultivariableAlexanderInner[ data ]
];

SetAttributes[ {
	braidWidth, braidHead, braidTail,
	braidWord, knotComponent, error,
	strandMapping, components, numberOfComponents,
	variableName, polynomial
	}, Protected
];

dbgPrint = False;
Dbg[ seq__ ]  := ( If[ dbgPrint, Print[ seq ] ]; )

MultivariableAlexanderInner[ data_ ] := Module[
	{temp}, 

Dbg[ Unevaluated[ "Braid Word ", data[ braidWord ] ] ];

	Scan[ data[ braidTail, # ] = #; &, data[ braidHead ] ];

Dbg[ Unevaluated[ "Braid Tail ", data[ braidTail, # ] & /@ data[ braidHead ] ] ];

	Scan[ (
		PermutationFunction[ data, braidTail, # ];
Dbg[ Unevaluated[ "Braid Tail ", data[ braidTail, # ] & /@ data[ braidHead ] ] ];
		) &, data[ braidWord ] ];	

	IdentifyElements[ data ];
Dbg[ Unevaluated[ "Strand Components ", data[ components ] ] ];
Dbg[ Unevaluated[ "Variables ", ReplaceAll[ data[ variableName, # ] & /@ data[ braidHead ], knotComponent->"T" ] ] ];

	FormColouredBurauMatrix[ data ];

Dbg[ Unevaluated[ "Burau ", ReplaceAll[ Expand[ data[ burau ] ], knotComponent->"T" ] // MatrixForm ] ]; 
Dbg[ Unevaluated[ "Divisor ", ReplaceAll[ data[ divisor ], knotComponent->"T" ] // MatrixForm ] ]; 

	temp = data[ burau ] - data[ divisor ]*IdentityMatrix[ data[ braidWidth ] - 1 ];

	temp = Expand[ temp ];

Dbg[ Unevaluated[ "Matrix ", ReplaceAll[ temp, knotComponent->"T" ] // MatrixForm ] ]; 
	
	temp = Det[ temp ];	
	
Dbg[ Unevaluated[ "Determinant ", ReplaceAll[ temp, knotComponent->"T" ] ] ]; 

	data[polynomial] = Expand[ Simplify[ Factor[temp]/Factor[CalculateDivisor[ data ] ] ] ];

Dbg[ Unevaluated[ "Polynomial ", ReplaceAll[ data[ polynomial ], knotComponent->"T" ] ] ]; 

	CalculateOutput[ data ]
];

PermutationFunction[ data_, list_, j_Integer ] := Module [
	{temp,i},  
	i = Abs[j];
	temp = data[ list, i ];
	data[ list, i ] = data[ list, i+1 ];
	data[ list, i+1 ] = temp;
];

IdentifyElements[ data_ ] := Module[
{ marked, strand, component },

	Scan[ ( data[ strandMapping, data[ braidTail, # ] ] = #; ) &, data[ braidHead ] ];

Dbg[ Unevaluated[ "Strand Mapping ", data[ strandMapping, # ] & /@ data[ braidHead ] ] ];

	data[ components ] = {};
	Scan[ ( 
		If[ marked[#] =!= True,
			component = {};
			strand = #;

			While[ marked[strand] =!= True,
				marked[strand] = True;
				component = Append[ component, strand ];
				strand = data[ strandMapping, strand ];
			];
		
			data[ components ] = Append[ data[ components ], component ];
		];	
	) &, data[ braidHead ] ];
	
	data[ numberOfComponents ] = Length[ data[ components ] ];

	For[ component = 1, component <= data[ numberOfComponents ], component++, 	
		Scan[ ( data[ variableName, # ] = knotComponent[component] ) &, data[ components ][[component]] ];	
	];

];

CalculateDivisor[ data_ ] := Module [ 
{temp = 1},

	Scan[ ( temp *= data[ variableName, # ] ) &, data[ braidHead ] ];

	temp = If[ data[ numberOfComponents ] == 1, (1-temp)/(1 - data[ variableName, 1 ]), 1 - temp ];

Dbg[ Unevaluated[ "Divisor ", ReplaceAll[ temp, knotComponent->"T" ] ] ];

	temp
];

CalculateOutput[ data_ ] := Module [
{temp=1,temp2, comps, term1},

	If[ data[polynomial] == 0, Return[ 0 & ] ];

	Scan[
		( temp2 = knotComponent[#]^Exponent[ data[polynomial], knotComponent[#], Min ];
		If[ temp2 =!= 0, temp *= temp2; ] )
	& , comps = Range[data[numberOfComponents]] ];
	temp = Expand[ data[polynomial]/temp ];
	comps = knotComponent /@ comps;
	temp = First[Sort[Flatten[
	  ({temp, -temp} /. Thread[Rule[comps, #]])& /@ Permutations[comps]
	]]];
	(*
	  If[Head[temp] === Plus, term1=First[temp], term1=temp];
	  If[(term1 /. _knotComponent -> 1) < 0, temp=Expand[-temp]];
	*)
	Function @@ {ReplaceAll[ temp, knotComponent-> # ]}
];

GetSubmatrix[ row_Integer, variableIndex_Integer, data_ ] := Module [
{output, variable},

	variable = data[ variableName, variableIndex ];
	
	output = IdentityMatrix[ data[ braidWidth ] - 1];
	
	output[[ row, row ]] = - variable;
	If[ row != data[ braidWidth ]-1, output[ [ row, row + 1 ] ] = 1, Null ];
	If[ row != 1, output[ [ row, row - 1 ] ] = variable, Null ];

Dbg[ Unevaluated[ "Submatrix ", ReplaceAll[ output, knotComponent->"T" ] // MatrixForm ] ]; 

	data[ burau ] = data[ burau ].output;
];

GetSubmatrixInverse[ row_Integer, variableIndex_Integer, data_ ] := Module [
{output, variable},

	variable = data[ variableName, variableIndex ];
	data[ divisor ] = variable*data[ divisor ];
	
	output = variable*IdentityMatrix[ data[ braidWidth ] - 1 ];
	
	output[[ row, row ]] = - 1;	
	If[ row != data[ braidWidth ] - 1, output[ [ row, row + 1 ] ] = 1, Null ];
	If[ row != 1, output[ [ row, row - 1 ] ] = variable, Null ];

Dbg[ Unevaluated[ "Submatrix ", ReplaceAll[ output, knotComponent->"T" ] // MatrixForm ] ]; 

	data[ burau ] = data[ burau ].output;
];

FormColouredBurauMatrix[ data_ ] := Module [
{tempArray},

	data[ divisor ] = 1;
	
	data[ burau ] = IdentityMatrix[ data[ braidWidth ] - 1 ];

	Scan[ ( data[ tempArray, # ] = #; ) &, data[ braidHead ] ];

	Scan[ (
		If[ # < 0, 
			GetSubmatrixInverse[ -1*#, data[ tempArray, -1*#+1 ], data ];,
			GetSubmatrix[ #, data[ tempArray, # ], data ];
		];
		PermutationFunction[ data, tempArray, # ];
		) &, data[ braidWord ]
	];

];

End[];
EndPackage[];
