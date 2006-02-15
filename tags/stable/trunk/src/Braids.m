BeginPackage["KnotTheory`"]		(* Braids *)

BR::usage = "BR stands for Braid Representative. BR[k,l] represents a
braid on k strands with crossings l={i1,i2,...}, where a positive index
i within the list l indicates a right-handed crossing between strand
number i and strand number i+1 and a negative i indicates a left handed
crossing between strands numbers |i| and |i|+1. Each ij can also be a
list of non-adjacent (i.e., commuting) indices. BR also acts as a
\"type caster\": BR[K] will return a braid whose closure is K if K is
given in any format that KnotTheory` understands. BR[K] where K is is a
named knot with up to 10 crossings returns a minimum braid
representative for that knot."

BR::about = "
The minimum braids representing the knots with up to 10 crossings were
provided by Thomas Gittings. See his article on the subject at
arXiv:math.GT/0401051. Vogel's algorithm was implemented by Dan Carney in
the summer of 2005 at the University of Toronto.
"

BraidLength::usage = "
BraidLength[K] returns the braid length of the knot K, if known to
KnotTheory`.
"

Mirror::usage = "
  Mirror[br] return the mirror braid of br.
"

CollapseBraid::usage = "
  CollapseBraid[br] groups together commuting generators in the braid
  br. Useful in conjunction with BraidPlot to produce compact braid plots.
"

BraidPlot::usage = "
  BraidPlot[br, opts] produces a plot of the braid br. Possible options
  are Mode, HTMLOpts, WikiOpts and Images.
"

NotAvailable; Mode; HTMLOpts; Images; WikiOpts;

Begin["`Private`"]

BR[br_BR] := br;

BR[k_, s_String] := BR[
  k, ToCharacterCode[s] /. j_Integer :> If[j < 97, 64 - j, j - 96]
]

Mirror[BR[k_Integer, l_List]] := BR[k, -l]
BR[Mirror[K_]] := Mirror[BR[K]]

BraidLength[Knot[n_Integer, k_Integer]] /; 0<=n<=10 && 1<=k<=NumberOfKnots[n] := Crossings[BR[K]]

CollapseBraid[NotAvailable] = NotAvailable
CollapseBraid[BR[k_, l_List]] := Module[
  {
    queue = Flatten[List /@ l], collapsed = {}, footprints = {}, current,
    abscurr, j, len
  },
  While[queue =!= {},
    abscurr = Abs[current = First[queue]]; queue = Rest[queue];
    j = len = Length[collapsed];
    While[j > 0 && FreeQ[footprints[[j]], abscurr], --j];
    If[j == len, AppendTo[collapsed, {}]; AppendTo[footprints, {}]];
    AppendTo[collapsed[[j+1]], current];
    footprints[[j+1]] = Union[footprints[[j+1]], abscurr + {-1, 0, 1}]
  ];
  BR[k, collapsed]
]

BraidPlot[NotAvailable, ___] := NotAvailable

Options[BraidPlot] = {
  Mode -> "Graphics",
  Images -> {"0.gif", "1.gif", "2.gif", "3.gif", "4.gif"},
  HTMLOpts -> "",
  WikiOpts -> ""
}

BraidPlot[BR[k_Integer, l_List], opts___Rule] := Module[
  {
    mat, i, j, ll, g, t, x, y,
    mode = (Mode /. {opts} /. Options[BraidPlot]),
    images = (Images /. {opts} /. Options[BraidPlot]),
    htmlopts = (HTMLOpts /. {opts} /. Options[BraidPlot] /. "" -> " "),
    wikiopts = (WikiOpts /. {opts} /. Options[BraidPlot])
  },
  If[StringTake[htmlopts, 1]=!=" ", htmlopts=" "<>htmlopts];
  If[StringTake[htmlopts, -1]=!=" ", htmlopts=htmlopts<>" "];
  If[Length[l]>0, 
    mat = Table[0, {k}, {Length[l]}];
    Do[
      ll = Flatten[{l[[i]]}];
      Do[
        If[ll[[j]] > 0,
          mat[[ll[[j]], i]] = 1;  mat[[ll[[j]]+1, i]] = 2,
          mat[[-ll[[j]], i]] = 3;  mat[[-ll[[j]]+1, i]] = 4
        ],
        {j, Length[ll]}
      ],
      {i, Length[l]}
    ],
    mat = Table[{0}, {k}]
  ];
  Switch[mode,
    "Graphics", Graphics[MapIndexed[g, mat, {2}] /. g[t_, {j_, i_}] :> (
      x = i - 1; y = k - j;
      Switch[t,
        0, Line[{{x, y+0.5}, {x+1, y+0.5}}],
        1, {
          Line[{{x, y+0.5}, {x+0.5, y}}],
          Line[{{x+0.75, y+0.25}, {x+1, y+0.5}}]
        },
        2, {
          Line[{{x, y+0.5}, {x+0.25, y+0.75}}],
          Line[{{x+0.5, y+1}, {x+1, y+0.5}}]
        },
        3, {
          Line[{{x, y+0.5}, {x+0.25, y+0.25}}],
          Line[{{x+0.5, y}, {x+1, y+0.5}}]
        },
        4, {
           Line[{{x, y+0.5}, {x+0.5, y+1}}],
           Line[{{x+0.75, y+0.75}, {x+1, y+0.5}}]
        }
      ]
    )],
    "HTML", StringJoin[
      "<table cellspacing=0 cellpadding=0 border=0>\n",
      Table[
        {
          "<tr><td>",
          ("<img"<>htmlopts<>"src="<>images[[#+1]]<>">") & /@ mat[[j]],
          "</td></tr>\n"
        },
        {j, k}
      ],
      "</table>"
    ],
    "Wiki", StringJoin[
      "<table cellspacing=0 cellpadding=0 border=0 style=\"white-space: pre\">\n",
      Table[
        {
          "<tr><td>",
          ("[[Image:"<>images[[#+1]]<>wikiopts<>"]]") & /@ mat[[j]],
          "</td></tr>\n"
        },
        {j, k}
      ],
      "</table>"
    ],
    _, mat
  ]
]

End[]; EndPackage[]

