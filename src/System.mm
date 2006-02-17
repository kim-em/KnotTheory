BeginPackage["KnotTheory`"];

KnotTheoryVersion::usage = "
KnotTheoryVersion[] returns the date of the current version of the
package KnotTheory`. KnotTheoryVersion[k] returns the kth field in
KnotTheoryVersion[].
"

KnotTheoryVersionString::usage = "
KnotTheoryVersionString[] returns a string containing the date and
time of the current version of the package KnotTheory`. It is generated
from KnotTheoryVersion[].
"

KnotTheoryWelcomeMessage::usage = "
KnotTheoryWelcomeMessage[] returns a string containing the welcome message
printed when KnotTheory` is first loaded.
"

KnotTheoryDirectory::usage = "
KnotTheoryDirectory[] returns the best guess KnotTheory` has for its
location on the host computer. It can be reset by the user.
"

CreditMessage::usage = "CreditMessage[cm] is used to print the string cm as a 'credit message'. Every credit message is printed at most once."

KnotTheory::credits = "`1`";

Begin["`System`"]

KnotTheoryVersion[] = ---date---;
KnotTheoryVersion[k_Integer] := KnotTheoryVersion[][[k]]

KnotTheoryVersionString[] = StringJoin[
  {
    "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"
  }[[KnotTheoryVersion[2]]],
  " ",
  ToString[KnotTheoryVersion[3]],
  ", ",
  ToString[KnotTheoryVersion[1]],
  ", ",
  ToString[KnotTheoryVersion[4]],
  ":",
  ToString[KnotTheoryVersion[5]],
  ":",
  ToString[KnotTheoryVersion[6]]
]

KnotTheoryDirectory[] = (
  File /. Flatten[FileInformation[ToFileName[#,"KnotTheory"]] & /@ $Path]
)

(* might be dangerous if KnotTheoryDirectory[] is somehow incorrect! *)
If[!MemberQ[$Path, KnotTheoryDirectory[]],
	AppendTo[$Path, KnotTheoryDirectory[]]
]

KnotTheoryWelcomeMessage[] = StringJoin[
  "Loading KnotTheory` version of ",
  KnotTheoryVersionString[],
  ". Read more at http://katlas.math.toronto.edu/wiki/KnotTheory."
]

Print[KnotTheoryWelcomeMessage[]]

CreditMessage[cm_String] := Module[
  {l},
  l=Length[$MessageList];
  Message[KnotTheory::credits, cm];
  If[Length[$MessageList] > l, CreditMessage[cm] = Null];
]

End[]; EndPackage[];
