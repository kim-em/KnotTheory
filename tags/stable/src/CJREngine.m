BeginPackage["KnotTheory`"]

Begin["`CJREngine`"]

CJ[K_, M_] :=  Module[ {N=M+1,cu, sq, kd, fp, fn, bp, bn, tt, t,ttb,r, rb, mcupl, mcapl, mcupr, mcapr},

	(* generate matrices -- with only one colour!! (n=n1=n2)*)
	cu[n_] := t^(2*n) - t^(-2*n);
	cu[n_, k_] := (cu[n,k] = If[k >= 0, Product[cu[n-i+1], {i, k}], 0]);
	
	sq[n_, k_] := (sq[n,k] = If[k>=0, cu[n,k]/cu[k,k], 0]);
	
	kd[a_, b_] := If[a==b,1,0];
	
	fp[n1_, n2_, a_, b_, k_] := (fp[n1,n2,a,b,k]=(-1)^k * t^-((n1-1-2a)(n2-1-2b) + k*(k-1)) * sq[b+k,k]*cu[n1-1+k-a, k]);
	
	fn[n1_, n2_, a_, b_, k_] := (fn[n1,n2,a,b,k] =  t^((n1-1-2a-2k)*(n2-1-2b+2k) + k*(k-1)) * sq[a+k,k]*cu[n2-1+k-b,k]);
	
	bp[n1_, n2_, a_, b_, c_, d_] := t^(n1^2 - 1)*fp[n1,n2,a,b,c-b]*kd[c-b,a-d];
	
	bn[n1_, n2_, a_, b_, c_, d_] := t^(1 - n1^2)*fn[n1,n2,a,b,b-c]*kd[c-b,a-d];
	
	tt = Table[Simplify[bp[N,N,a,b,c,d]], {a,0,N-1}, {b,0,N-1}, {c,0,N-1}, {d,0,N-1}];
	
	ttb = Table[Simplify[bn[N,N,a,b,c,d]], {a,0,N-1}, {b,0,N-1}, {c,0,N-1}, {d,0,N-1}];

	r = Table[tt[[Ceiling[x/N], If[Mod[x,N] != 0, Mod[x,N],N], Ceiling[y/N], If[Mod[y,N] != 0, Mod[y,N], N] ]], {x, N^2}, {y, N^2}];
	
	rb = Table[ttb[[Ceiling[x/N], If[Mod[x,N] != 0, Mod[x,N],N], Ceiling[y/N], If[Mod[y,N] != 0, Mod[y,N], N] ]], {x, N^2}, {y, N^2}];	
	mcupl = Table[t^a * kd[a,-b], {a, -N+1, N-1, 2}, {b, -N+1, N-1, 2}];
	
	mcupr = Table[t^a * kd[a,-b], {a, -N+1, N-1, 2}, {b, -N+1, N-1, 2}];
	
	mcapr = Inverse[mcupr];
	mcapl = Inverse[mcupl];

Return[Function@@ {Apart[REngine[K, r, rb, mcupl, mcapl, mcupr, mcapr] / REngine[MorseLink[Knot[0,1]], r, rb, mcupl, mcapl, mcupr, mcapr] /. t -> #^(1/4)]}]
	
]

End[];EndPackage[];
