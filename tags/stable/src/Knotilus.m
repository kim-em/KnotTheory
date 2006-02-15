BeginPackage["KnotTheory`"]

KnotilusCode::usage = "
  KnotilusCode[i1, i2, ...] represents a knot via its Gauss
  Code following the conventions used by the knotilus website,
  http://srankin.math.uwo.ca/cgi-bin/retrieve.cgi/html/start.html.
  Likewise KnotilusCode[l1, l2, ...] represents a link, where each of
  l1, l2,...  is a list describing the code read along one component
  of the link.  KnotilusCode also acts as a \"type caster\", so for
  example, KnotilusCode[K] where K is is a named knot (or link) returns
  the knotilus code of that knot.
"

KnotilusURL::usage = "
  KnotilusURL[K_] returns the URL of the knot/link K on the knotilus
  website, http://srankin.math.uwo.ca/cgi-bin/retrieve.cgi/html/start.html.
"

Begin["`Knotilus`"]

KnotilusCode[K_] /; Head[K]=!=PD := KnotilusCode[PD[K]]
KnotilusCode[PD[Xs___X]] := (
  pd = PD[Xs]; c=0;
  kc = KC @@ (s = Skeleton[pd] /. Loop -> List);
  pd /. X[i_, j_, k_, l_] :> (
    kc[[Sequence @@ First@Position[s,
      If[j - l == 1 || l - j > 1, l, j]
    ]]] = ++c;
    kc[[Sequence @@ First@Position[s, i]]] = -c
  );
  If[Length[s]==1,
    KnotilusCode @@ First[kc],
    KnotilusCode @@ kc
  ]
)

KnotilusURL[HoldPattern[KnotilusCode[is__Integer]]] := StringJoin[
  "http://srankin.math.uwo.ca/cgi-bin/retrieve.cgi/",
  StringReplace[
    ToString[{is}],
    {" " -> "", "{" -> "", "}" -> ""}
  ],
  "/index.html"
]
KnotilusURL[HoldPattern[KnotilusCode[ls__List]]] := StringJoin[
  "http://srankin.math.uwo.ca/cgi-bin/retrieve.cgi/",
  StringReplace[
    ToString[{ls}],
    {"{{" -> "", "}, {" -> ":", " " -> "", "}}" -> ""}
  ],
  "/index.html"
]
KnotilusURL[K_] /; Head[K] =!= KnotilusCode := KnotilusURL[KnotilusCode[K]]

End[]; EndPackage[]
