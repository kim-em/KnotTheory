
(* VogelsAlgorithm.m by Dan Carney *)

BeginPackage["KnotTheory`"];

BR; Mirror; NumberOfKnots; PD;

Begin[ "`VogelsAlgorithm`" ];

BR[K_] /; !(
  Head[K] === Mirror
  || MatchQ[K,
    Knot[n_Integer, k_Integer] /; 0<=n<=10 && 1 <= k <= NumberOfKnots[n]
  ]
) := CalculateBraid[PD[K]]

CalculateBraid[K_] /; Head[K] =!= PD := ( CalculateBraid[PD[K]] )

CalculateBraid[PD[Loop[_]]] := ( BR[1,{}] )

CalculateBraid[ PD[ Xs__X ] ] := Module[
{ temp },
  CreditMessage["Vogel's algorithm was implemented by Dan Carney in the summer of 2005 at the University of Toronto."];
  temp = List @@@ {Xs};	
  CalculateBraid2[ temp, If[ #[[2]] - #[[4]] == 1 || #[[4]] - #[[2]] > 1, +1, -1 ] & /@ temp ]
];

error;

crossingIndex;
crossingSign;

edgeIndex;
edgeMark;
edgeCircle;
edgeEnd;
edgeStart;

circleIndex;
circleDescription;

left;
right;
clockwise;
counterClockwise;

crossingDescription = { 1, 2, 3, 4 };

dbgPrint = False;

Dbg[ seq__ ]  := ( If[ dbgPrint, Print[ seq ] ]; )

Append2[ a_, b_ ] := ( a = Append[ a, b ]; )

Mark[ a__ ] := ( Scan[ If[ # =!= True, Set[ #, True ] ] & , { a } ]; )

IsMarked[ a_ ] := ( If[ a === True, True, False, False ] )

CalculateBraid2[ crossingsList_List, crossingSigns_List ] := Module[
{next, current = {crossingsList, crossingSigns} },

	While[ True, 
		next = CalculateBraid3[ Sequence @@ current ];
		If[ Head[next] =!= List, Return[next] ];
		current = next;
	];		
];

CalculateBraid3[ crossingsList_List, crossingSigns_List ] := Module[
{data, pair},

	data[crossingIndex] = crossingsList;
Dbg[ Unevaluated[ "Crossings ", data[crossingIndex] ] ];

	MapThread[ data[ crossingSign, #1] = #2; & , { crossingsList, crossingSigns } ];
Dbg[ Unevaluated[ "Signs ", data[crossingSign, #] & /@ data[crossingIndex] ] ];
	
	data[ edgeIndex ] = Union[ Flatten[ data[crossingIndex] ] ];
Dbg[ Unevaluated[ "Edges ", data[ edgeIndex ] ] ];
		
	Scan[ 	data[ edgeStart, #[[3]] ] = data[ edgeEnd, #[[1]] ] = #;
		If[ data[ crossingSign, # ] == 1,
		 	data[ edgeStart, #[[2]] ] = data[ edgeEnd, #[[4]] ] = #;,
		 	data[ edgeStart, #[[4]] ] = data[ edgeEnd, #[[2]] ] = #;
		 ];
		 & , data[crossingIndex] ]; 			
(*
Dbg[ Unevaluated[ "Starting crossings ", data[ edgeStart, # ] & /@ data[ edgeIndex ] ] ];
Dbg[ Unevaluated[ "Ending crossings ", data[ edgeEnd, # ] & /@ data[ edgeIndex ] ] ];
*)

	CalculateSeifertCircles[ data ];
Dbg[ Unevaluated[ "Seifert Circles ", { #, data[circleDescription, #]} & /@ data[circleIndex] ] ];

	pair = CalculateSurfaces[ data ];
	If[ pair =!= Null, Return[ VogelMove[ data, pair ] ]; ];

	BuildReducedSeifertGraph[ data ];
	
(*	Return[ VerifyReducedSeifertGraph[ data ] ]; *)
	 
	ReadBraidWord[ data ]
];

GetEnds[ { a_, _, _, b_ }, 1 ] := {a,b}
GetEnds[ { a_, b_, _, _ }, -1 ] := {a,b}

GetStarts[ { _, b_, a_, _ }, 1 ] := {a,b}
GetStarts[ { _, _, a_, b_ }, -1 ] := {a,b}

PreviousStrandEdge[ edge_, { _, _, edge_, x_ }, 1 ] := (x)
PreviousStrandEdge[ edge_, { _, x_, edge_, _ }, -1 ] := (x)
PreviousStrandEdge[ edge_, { x_, _, _, edge_ }, -1 ] := (x)
PreviousStrandEdge[ edge_, { x_, edge_, _, _ }, 1 ] := (x)

NextStrandEdge[ edge_, { edge_, x_,    _,  _     },  1 ] := (x)
NextStrandEdge[ edge_, { edge_, _,     _,  x_    }, -1 ] := (x)
NextStrandEdge[ edge_, { _,     _,     x_, edge_ },  1 ] := (x)
NextStrandEdge[ edge_, { _,     edge_, x_, _     }, -1 ] := (x)

IsNextCrossingRight[ edge_, { _, edge_, _, _ }, -1 ] := (True)
IsNextCrossingRight[ edge_, { _, _, _, edge_ }, 1 ] := (False)
IsNextCrossingRight[ edge_, { edge_, _, _, _ }, 1 ] := (True)
IsNextCrossingRight[ edge_, { edge_, _, _, _ }, -1 ] := (False)

CalculateSeifertCircles[ data_ ] := Module[ 
{ currentCircleIndex=0, currentEdge, nextCrossing, currentCircle },

	Scan[ currentEdge = #;
		If[ !IsMarked[ data[ edgeMark, currentEdge ] ], 
			currentCircle = {};
			currentCircleIndex++;
			While[ !IsMarked[ data[ edgeMark, currentEdge ] ], 
				Mark[ data[ edgeMark, currentEdge ] ];
				data[ edgeCircle, currentEdge ] = currentCircleIndex;
				Append2[ Unevaluated[ currentCircle ], currentEdge ];
				nextCrossing = data[ edgeEnd, currentEdge ];
				currentEdge = NextStrandEdge[ currentEdge, nextCrossing, data[ crossingSign, nextCrossing ] ];
			];
			data[ circleDescription, currentCircleIndex ] = currentCircle;
		];
	&, data[ edgeIndex ] ];
	data[ circleIndex ] = Range[ currentCircleIndex ];
];

CalculateSurfaces[ data_ ] := Module[
{ surface, edgeDirection, pair },

	Scan[ 
		currentEdge = #1;
		edgeDirection = left;
		pair = AccumulateSurface[ Unevaluated[ currentEdge ], Unevaluated[ edgeDirection ], data ];		
		If[ pair =!= Null, Return[ pair ] ];	
		edgeDirection = right;
		pair = AccumulateSurface[ Unevaluated[ currentEdge ], Unevaluated[ edgeDirection ], data ];			
		If[ pair =!= Null, Return[ pair ] ];	
	&, data[ edgeIndex ] ]
	 
];

AccumulateSurface[ currentEdge_, edgeDirection_, data_ ] := Module[ 
{surface = {}, crossing },

	If[ IsMarked[ data[ edgeMark, edgeDirection, currentEdge ] ], Return[]; ];

	While[ !IsMarked[ data[ edgeMark, edgeDirection, currentEdge ] ], 
		
		Mark[ data[ edgeMark, edgeDirection, currentEdge ] ];		
		Append2[ Unevaluated[ surface ], If[ edgeDirection === left, currentEdge, -currentEdge ] ];

		crossing = data[ If[ edgeDirection === left, edgeEnd, edgeStart ], currentEdge ];
		Scan[ If[ crossing[[#]] === currentEdge, 
			currentEdge = crossing[[ If[ # == 4, 1, #+1 ] ]]; Return[Null];
			 ]; &, crossingDescription ];

		edgeDirection = If[ data[ edgeStart, currentEdge ] === crossing, left, right ];		
	];
	
(* Dbg[ "Surface ", surface ]; *)

	SearchSurfaceForAdmissiblePair[ surface, data ]
];

SearchSurfaceForAdmissiblePair[ surface_, data_ ] := Module[ 
{unorderedList, orderedList },
	
	unorderedList = Sign[#]*data[ edgeCircle, Abs[#] ] & /@ surface;
	orderedList = Union[ unorderedList ];

	If[ Length[ orderedList ] <= 1, Return[Null]; ];

	If[ Sign[ orderedList[[1]] ] == Sign[ orderedList[[2]] ],
		Return[ { 	surface[[ Position[ unorderedList, orderedList[[1]] ][[1]][[1]] ]] ,
					surface[[ Position[ unorderedList, orderedList[[2]] ][[1]][[1]] ]] } ];
	];

	If[ Sign[ orderedList[[-1]] ] == Sign[ orderedList[[-2]] ],
		Return[ { 	surface[[ Position[ unorderedList, orderedList[[-1]] ][[1]][[1]] ]] ,
					surface[[ Position[ unorderedList, orderedList[[-2]] ][[1]][[1]] ]] } ];
	];
	
	Null
];

VogelMove[ data_, pair_ ] := Module[
{ newCrossings, newSigns, direction, high, edgeA, edgeB },

Dbg[ Unevaluated[ "Found pair ", pair  ] ];	

	edgeA = Abs[ pair[[1]] ];
	edgeB = Abs[ pair[[2]] ];
	direction = If[ Sign[ pair[[1]] ] == 1, right, left ];

	newSigns = Join[
		data[ crossingSign, #] & /@ data[ crossingIndex ] , 
		If[ direction === right, {1,-1}, {-1,1} ]
	];
	
	high = Max[ Sequence[ data[ edgeIndex ] ] ];
	
	newCrossings = Join[ 
		ReplaceAll[ data[ crossingIndex ], {
			data[ edgeStart, edgeA ] -> Replace[ data[ edgeStart, edgeA ], edgeA->high+1, 2 ],
			data[ edgeEnd,   edgeA ] -> Replace[ data[ edgeEnd,   edgeA ], edgeA->high+3, 2 ],
			data[ edgeStart, edgeB ] -> Replace[ data[ edgeStart, edgeB ], edgeB->high+4, 2 ],
			data[ edgeEnd,   edgeB ] -> Replace[ data[ edgeEnd,   edgeB ], edgeB->high+6, 2 ]	
		 } ],
		If[ direction === right,
			{ { high+1, high+6, high+2, high+5 }, { high+2, high+4, high+3, high+5 } },
			{ { high+1, high+5, high+2, high+6 }, { high+2, high+5, high+3, high+4 } }
		]
	 ];
	 
	{newCrossings, newSigns}
];

BuildReducedSeifertGraph[ data_ ] := Module[
{ circleA, circleB },

	Scan[ data[ circleNeighbour, # ] = {}; &, data[ circleIndex ] ];

	Scan[
		circleA = data[ edgeCircle, #[[3]] ];
		circleB =  data[ edgeCircle, If[ data[ crossingSign, # ] == 1, #[[2]], #[[4]] ] ];
		Append2[ Unevaluated[ data[ circleNeighbour, circleA ] ], circleB ];
		Append2[ Unevaluated[ data[ circleNeighbour, circleB ] ], circleA ];
		&, data[ crossingIndex ] ];

	Scan[ data[ circleNeighbour, # ] = Union[ data[ circleNeighbour, # ] ]; &, data[ circleIndex ] ];
Dbg[ "Neighbours ", data[ circleNeighbour, # ] & /@ data[ circleIndex  ] ];

];

VerifyReducedSeifertGraph[ data_ ] := Module[
{ temp },
	temp = Union[ Length[ data[ circleNeighbour, # ] ] & /@ data[ circleIndex ] ];
	If[ MemberQ[ temp, 1 ] && Max[ Sequence[ temp ] ] <= 2, True, False, error ]
];

CalculateStrandChain[ data_ ] := Module[
{ chain, current, next, temp, initialCrossing, initialEdge },
	
	{current,next} = Scan[
			If[ Length[ data[ circleNeighbour, # ] ] == 1, Return[{#,data[ circleNeighbour, # ][[1]]} ] ] & , data[ circleIndex ] ];
		
	chain = {current, next};	
	While[ Length[ data[ circleNeighbour, next ] ] == 2,
		temp =  data[ circleNeighbour, next ][[ If[ data[ circleNeighbour, next ][[1]] === current, 2, 1 ] ]];
		current = next;
		next = temp;
		Append2[ Unevaluated[ chain ], next ];
	];
	
	initialEdge = First[data[ circleDescription, First[ chain ]]];
	initialCrossing = data[ edgeStart, initialEdge ];
	chain = If[ data[ crossingSign, initialCrossing ] == 1,
		If[ initialCrossing[[3]] == initialEdge, Reverse[ chain ], chain ],
		If[ initialCrossing[[3]] == initialEdge, chain, Reverse[ chain ] ]
	];
	
	Dbg[ Unevaluated[ "Chain ", chain ] ];
	data[ strands ] = chain;
];

MarkStrandNeighbours[ data_ ] := Module[ 
{ temp },
	temp = Null;
	Scan[ data[ leftStrand, # ] = temp; data[ rightStrand, temp ] = #; temp = #; &, data[ strands ] ];
	data[ rightStrand, Last[ data[ strands ] ] ] = Null;
];

GetRightInitialEdge[ { _, _, _, x_ }, 1 ] := (x)
GetRightInitialEdge[ { x_, _, _, _ }, -1 ] := (x)

FindNextRightCrossing[ edgeIn_, data_ ] := Module[
{crossing, edge = edgeIn},
	crossing = data[ edgeEnd, edge ];
	While[ True,
		If[ IsNextCrossingRight[ edge, crossing, data[ crossingSign, crossing ] ],
			Return[ crossing ], Null,
			Print[ "Error ", edge, " ", crossing, " ", data[ crossingSign, crossing ] ]
		];
		edge = NextStrandEdge[ edge, crossing, data[ crossingSign, crossing ] ];
		crossing = data[ edgeEnd, edge ];
	];
];

CalculateInitialEdges[ data_ ] := Module[
{ edge, crossing, currentStrand, nextStrand, temp },

	currentStrand = First[ data[ strands ] ];
	nextStrand = data[ rightStrand, currentStrand ];

	edge = First[ data[ circleDescription, currentStrand ] ];
	data[ strandInitialEdge, currentStrand ] = edge;

	While[ nextStrand =!= Null, 

		currentStrand = nextStrand;
		nextStrand = data[ rightStrand, nextStrand ];

		crossing = FindNextRightCrossing[ edge, data ];

		edge = GetRightInitialEdge[ crossing, data[ crossingSign, crossing ] ];
		data[ strandInitialEdge, currentStrand ] = edge;
	];
	
];

BraidSign[ leftEdge_, { leftEdge_, _, _, _ }, 1 ] := (1)
BraidSign[ leftEdge_, { _, leftEdge_, _, _ }, -1 ] := (-1)

VerifyBraidWord[ edgeFront_, data_ ] := Module[
{temp},

	temp = If[ edgeFront[ # ] ==  data[ strandInitialEdge, data[ strands][[#]] ], True, False, error ]  & /@
				Range[ Length[ data[ circleIndex ] ] ];
	temp = Union[ temp ];
	
	If[ Length[ temp ] != 1 && !temp[[1]], Return[ False ], Null, Return[ error ] ];

	temp = IsMarked[ data[ braidMark, # ] ] & /@ data[ crossingIndex ];
	temp = Union[ temp ];
	If[ Length[ temp ] != 1 && !temp[[1]], Return[ False ], Null, Return[ error ] ];

	True
];

ReadBraidWord[ data_ ] := Module[
{ edgeFront, braidWord, braidWidth, leftEdge, rightEdge, crossing, sign },

	CalculateStrandChain[ data ];
	
	MarkStrandNeighbours[ data ];
	
	CalculateInitialEdges[ data ];
Dbg[ Unevaluated[ "Start Edges ", data[ strandInitialEdge, # ] & /@ data[ strands ] ] ];

	braidWord = {};
	braidWidth = Length[ data[ circleIndex ] ];

	Scan[ ( edgeFront[ # ] = data[ strandInitialEdge, data[ strands][[#]] ] ) &, Range[ braidWidth ] ];

	While[ True,
	
		For[ offset = 1, offset < braidWidth,
		 
			leftEdge = edgeFront[offset];
			rightEdge = edgeFront[offset+1];
			crossing = data[ edgeEnd, leftEdge ];
			If[ 
				crossing == data[ edgeEnd, rightEdge ] 
				&& !IsMarked[ data[ braidMark, crossing ] ], Break[] ]; 
		
		offset++ ];
		
		If[ offset == braidWidth, 
			Return[ BR[ braidWidth, braidWord ] ]
		];
		
		Mark[ data[ braidMark, crossing ] ];
		
		sign =  data[ crossingSign, crossing ];
		
		edgeFront[offset] = NextStrandEdge[ leftEdge, crossing, sign ];
		edgeFront[offset+1] = NextStrandEdge[ rightEdge, crossing, sign ];		

		braidWord = Append[ braidWord, offset*BraidSign[ leftEdge, crossing, sign ] ];
	];
];


End[];
EndPackage[];

(* End of VogelsAlgorithm.m *)

