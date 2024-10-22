BeginPackage["KnotTheory`"]		(* Data *)

AllKnots::usage = "
  AllKnots[] return a list of all knots with up to 11 crossings. AllKnots[n_] returns
  a list of all knots with n crossings, up to 16. AllKnots[{n_,m_}] returns a list of
  all knots with between n and m crossings, and AllKnots[n_,Alternating|NonAlternating]
  returns all knots with n crossings of the specified type.
"

AllLinks::usage = "
  AllLinks[] return a list of all links with up to 11 crossings. AllLinks[n_] returns
  a list of all links with n crossings, up to 12.
"

DTCode;

Begin["`Private`"]

NumberOfKnots[0, Alternating] = 1
NumberOfKnots[1, Alternating] = 0
NumberOfKnots[2, Alternating] = 0
NumberOfKnots[3, Alternating] = 1
NumberOfKnots[4, Alternating] = 1
NumberOfKnots[5, Alternating] = 2
NumberOfKnots[6, Alternating] = 3
NumberOfKnots[7, Alternating] = 7
NumberOfKnots[8, Alternating] = 18
NumberOfKnots[9, Alternating] = 41
NumberOfKnots[10, Alternating] = 123
NumberOfKnots[11, Alternating] = 367
NumberOfKnots[12, Alternating] = 1288
NumberOfKnots[13, Alternating] = 4878
NumberOfKnots[14, Alternating] = 19536
NumberOfKnots[15, Alternating] = 85263
NumberOfKnots[16, Alternating] = 379799

NumberOfKnots[0, NonAlternating] = 0
NumberOfKnots[1, NonAlternating] = 0
NumberOfKnots[2, NonAlternating] = 0
NumberOfKnots[3, NonAlternating] = 0
NumberOfKnots[4, NonAlternating] = 0
NumberOfKnots[5, NonAlternating] = 0
NumberOfKnots[6, NonAlternating] = 0
NumberOfKnots[7, NonAlternating] = 0
NumberOfKnots[8, NonAlternating] = 3
NumberOfKnots[9, NonAlternating] = 8
NumberOfKnots[10, NonAlternating] = 42
NumberOfKnots[11, NonAlternating] = 185
NumberOfKnots[12, NonAlternating] = 888
NumberOfKnots[13, NonAlternating] = 5110
NumberOfKnots[14, NonAlternating] = 27436
NumberOfKnots[15, NonAlternating] = 168030
NumberOfKnots[16, NonAlternating] = 1008906

NumberOfKnots[n_] :=
  NumberOfKnots[n, Alternating] + NumberOfKnots[n, NonAlternating]

NumberOfKnots[{n_, m_}]:= Sum[NumberOfKnots[k], {k,n,m}]
NumberOfKnots[{n_, m_}, t_]:= Sum[NumberOfKnots[k, t], {k,n,m}]

NumberOfLinks[2] = 1
NumberOfLinks[3] = 0
NumberOfLinks[4] = 1
NumberOfLinks[5] = 1
NumberOfLinks[6] = 6
NumberOfLinks[7] = 9
NumberOfLinks[8] = 29
NumberOfLinks[9] = 83
NumberOfLinks[10] = 287
NumberOfLinks[11] = 1007
NumberOfLinks[12] = 4276
NumberOfLinks[13] = 7539
NumberOfLinks[2, Alternating] = 1
NumberOfLinks[3, Alternating] = 0
NumberOfLinks[4, Alternating] = 1
NumberOfLinks[5, Alternating] = 1
NumberOfLinks[6, Alternating] = 5
NumberOfLinks[7, Alternating] = 7
NumberOfLinks[8, Alternating] = 21
NumberOfLinks[9, Alternating] = 55
NumberOfLinks[10, Alternating] = 174
NumberOfLinks[11, Alternating] = 548
NumberOfLinks[12, Alternating] = 2020
NumberOfLinks[2, NonAlternating] = 0
NumberOfLinks[3, NonAlternating] = 0
NumberOfLinks[4, NonAlternating] = 0
NumberOfLinks[5, NonAlternating] = 0
NumberOfLinks[6, NonAlternating] = 1
NumberOfLinks[7, NonAlternating] = 2
NumberOfLinks[8, NonAlternating] = 8
NumberOfLinks[9, NonAlternating] = 28
NumberOfLinks[10, NonAlternating] = 113
NumberOfLinks[11, NonAlternating] = 459
NumberOfLinks[12, NonAlternating] = 2256

NumberOfLinks[{n_, m_}]:= Sum[NumberOfLinks[k], {k,n,m}]
NumberOfLinks[{n_, m_}, t_]:= Sum[NumberOfLinks[k, t], {k,n,m}]

(* These are ordered lists for the purpose of data loading! Do not mess! *)
AllKnots[] = Flatten[{
  Table[Knot[n,k], {n,0,10}, {k,NumberOfKnots[n]}],
  Table[Knot[11, Alternating, k], {k, NumberOfKnots[11, Alternating]}],
  Table[Knot[11, NonAlternating, k], {k, NumberOfKnots[11, NonAlternating]}]
}]
AllLinks[] = Flatten[Table[{
  Table[Link[n, Alternating, k], {k,NumberOfLinks[n, Alternating]}],
  Table[Link[n, NonAlternating, k], {k,NumberOfLinks[n, NonAlternating]}]
}, {n,2,11}]]

AllKnots[n_]/;n<=10:=Table[Knot[n,k],{k,1,NumberOfKnots[n]}]
AllKnots[n_]/;11<=n<=16:=AllKnots[n,Alternating]~Join~AllKnots[n,NonAlternating]
AllKnots[n_,t_]/;11<=n<=16:=Table[Knot[n,t,k],{k,1,NumberOfKnots[n,t]}]
AllKnots[n_,Alternating]/;n<=10:=Table[Knot[n,k],{k,1,NumberOfKnots[n,Alternating]}]
AllKnots[n_,NonAlternating]/;n<=10:=Table[Knot[n,NumberOfKnots[n,Alternating]+k],{k,1,NumberOfKnots[n,NonAlternating]}]
AllKnots[{n_,m_}]:=Join@@Table[AllKnots[i],{i,n,m}]
AllLinks[n_]/;2<=n<=12:=AllLinks[n,Alternating]~Join~AllLinks[n,NonAlternating]
AllLinks[n_,t_]/;2<=n<=12:=Table[Link[n,t,k],{k,1,NumberOfLinks[n,t]}]
AllLinks[{n_,m_}]:=Join@@Table[AllLinks[i],{i,n,m}]

PD[Knot[n_, k_]] := (
  Needs["KnotTheory`PD4Knots`"];
  Unset[PD[Knot[n1_, k1_]]];
  PD[Knot[n, k]]
)

DTCode[Knot[n_, t_, k_]] /; (n<=11) := (
  Needs["KnotTheory`DTCode4KnotsTo11`"];
  Unset[DTCode[Knot[n1_, t1_, k1_]] /; (n1<=11)];
  DTCode[Knot[n, t, k]]
)

PD[Knot[n_, t_, k_]] := PD[DTCode[Knot[n, t, k]]]

PD[Link[n_, t_, k_]] := (
  Needs["KnotTheory`PD4Links`"];
  Unset[PD[Link[n1_, t1_, k1_]]];
  PD[Link[n, t, k]]
)

DT4Knots[n_, t_] /; (12<=n<=16) := DT4Knots[n, t] = Module[
  {ts, fn, f},
  ts = t /. {Alternating -> "A", NonAlternating -> "N"};
  fn = "KnotTheory/"<>ToString[n]<>ts<>".dts";
  Message[KnotTheory::loading, fn];
  Import[fn, "Lines"]
]

DTCode[Knot[n_, t_, k_]] /; (12<=n<=16) := DTCode @@ (
  If[# >= 97, 2(#-96), -2(#-64)]& /@ ToCharacterCode[DT4Knots[n, t][[k]]] 
)

(* from http://mathematica.stackexchange.com/a/7512/35 *)

dPcore[L_, p : {q___, _}] := 
 Inner[L[[# ;; #2]] &, {0, q} + 1, p, Head@L]
dPcore[L_, p_, All] := dPcore[L, p]~Append~Drop[L, Last@p]
dPcore[L_, p_, n__] := dPcore[L, p]~Join~Partition[L~Drop~Last@p, n]
dynamicPartition[L_, p : {__Integer}, x___] := 
 dPcore[L, Accumulate@p, x] /; ! Negative@Min@p && Length@L >= Tr@p

AlphabeticDTCode[S_String] := 
 Module[{digits, crossings, components, sizes},
  digits = 
   ToCharacterCode[S] /. {n_ /; n >= 97 :> n - 96, 
     n_ /; n < 97 :> 64 - n};
  crossings = First[digits];
  components = digits[[2]];
  If[2 + crossings + components != StringLength[S],
   Print["Invalid alphabetic link Gauss code: ", S]; Abort[]];
  sizes = digits[[3 ;; 2 + components]];
  DTCode @@ (2 dynamicPartition[digits[[3 + components ;;]], sizes])
  ]

DT4Links[12, t : (Alternating | NonAlternating)] := 
 DT4Links[12, t] = Module[{},
   CreditMessage[
    "The table of links with 12 crossings was provided by Morwen \
Thistlethwaite, but it has not been independently confirmed and may \
contain omissions or duplications."];
   Import[
    FileNameJoin[{KnotTheoryDirectory[], 
      "L12" <> (Switch[t, Alternating, "A", NonAlternating, "N"])}], 
    "Lines"]
   ]

PD[Link[12, t_, k_]] := PD[AlphabeticDTCode[DT4Links[12, t][[k]]]]

End[]; EndPackage[]
