
BeginPackage["TubePlot`", {"Utilities`FilterOptions`"}]

TubePlot::usage = "
  TubePlot[gamma, {t, t0, t1}, r, opts] plots the space curve gamma
  with the variable t running from t0 to t1, as a tube of radius r. The
  available options are TubeSubdivision, TubeFraming and TubePlotPrelude.
  All other options are passed on to Graphics3D.
  TubePlot[TorusKnot[m, n], opts] produces a tube plot of the (m,n)
  torus knot.
"

TubeSubdivision::usage = "
  TubeSubdivision is an option for TubePlot. TubePlot[__, TubeSubdivision
  -> {l, m} draws the tube subdivided to l pieces lengthwise and m pieces
  around. The default is TubeSubdivision -> {50, 12}.
"

TubeFraming::usage = "
  TubeFraming is an option for TubePlot. TubePlot[gamma, {t, __},
  _, TubeFraming -> n] sets the framing of the tube (visible when
  TubeSubdivision -> {l, m} with small m) to be the vector n, which
  in itself may be a function of t.  Thus TubeFraming -> {0,0,1} is
  \"blackboard framing\". TubeFraming -> Normal (default) uses the normal
  vector of the curve gamma.
"

TubePlotPrelude::usage = "
  TubePlotPrelude is an option for TubePlot. Its value is passed to
  Graphics3D before the main part of the plot, allowing to set various
  graphics options. For example,  TubePlotPrelude -> EdgeForm[{}] will
  suppress the drawing of edges between the polygons making up the tube.
  The default is TubePlotPrelude -> {}.
"

Begin["`Private`"]

Options[TubePlot] = {
  TubeSubdivision -> {50, 12}, TubeFraming -> Normal, TubePlotPrelude -> {}
};
TubePlot[gamma_, {t_, t1_, t2_}, r_, opts___Rule] := Module[
  {
    l, m, framing, prelude, Normalize, ProjectOut, dt, ts, gs, Ts, Ns, Bs, 
    args, Cs, Ss, ring, tube
  },
  {{l, m}, framing, prelude} =
    {TubeSubdivision, TubeFraming, TubePlotPrelude} /.
    {opts} /.  Options[TubePlot];
  Normalize[v_] := v/Sqrt[v.v]; ProjectOut[v_, w_] := v - (v.w)w;
  dt = N[t2 - t1]/l;
  ts = t1 + Range[-1, l + 1]*dt;
  gs = (gamma /. (t -> #)) & /@ ts;
  Ts = (RotateLeft[gs] - gs)/dt;
  Ns = If[framing === Normal,
    (Ts - RotateRight[Ts])/dt,
    (framing /. (t -> #)) & /@  ts
  ];
  Ts = Normalize /@ (Ts + RotateRight[Ts]);
  Ns = Normalize /@ MapThread[ProjectOut, {Ns, Ts}];
  Bs = Normalize /@ MapThread[Cross, {Ts, Ns}];
  args = N[2Pi*Range[0, m]/m];
  {Cs, Ss} = {Cos /@ args, Sin /@ args};
  ring[g_, n_, b_] := 
    Transpose[g + r(Outer[Times, n , Cs] + Outer[Times, b, Ss])];
  tube = MapThread[ring, {gs, Ns, Bs}];
  Graphics3D[{prelude, Table[ 
    Polygon[{tube[[i, j]], tube[[i+1, j]], tube[[i+1, j+1]], tube[[i, j+1]]}],
    {i, 2, l + 1}, {j, m}
  ]}, FilterOptions[Graphics3D, opts]]
]

End[]; EndPackage[]

BeginPackage["KnotTheory`", {"TubePlot`"}]

TorusKnot;

Begin["`Private`"]

TubePlot[TorusKnot[m_, n_], opts___] := TubePlot[
  {Cos[n t], Sin[n t], 0} + 
    0.5{Cos[m t]Cos[n t], Cos[m t]Sin[n t], -Sin[m t]},
  {t, 0, 2Pi}, 1/Max[m, n], opts,
  TubeSubdivision -> {40(m + 2n), 12}, TubeFraming -> {0,0,1},
  TubePlotPrelude -> EdgeForm[{}], Boxed -> False, ViewPoint -> {0, 0, 1}
];

End[]; EndPackage[]
