BeginPackage["KnotTheory`"]             (* Braid Data *)

BR;

Begin["`Private`"]

BR[Knot[0,1]] = BR[1, ""]
BR[Knot[3, 1]] = BR[2, "AAA"]
BR[Knot[4, 1]] = BR[3, "AbAb"]
BR[Knot[5, 1]] = BR[2, "AAAAA"]
BR[Knot[5, 2]] = BR[3, "AAABaB"]
BR[Knot[6, 1]] = BR[4, "AABacBc"]
BR[Knot[6, 2]] = BR[3, "AAAbAb"]
BR[Knot[6, 3]] = BR[3, "AAbAbb"]
BR[Knot[7, 1]] = BR[2, "AAAAAAA"]
BR[Knot[7, 2]] = BR[4, "AAABaBCbC"]
BR[Knot[7, 3]] = BR[3, "aaaaabAb"]
BR[Knot[7, 4]] = BR[4, "aabAbbcBc"]
BR[Knot[7, 5]] = BR[3, "AAAABaBB"]
BR[Knot[7, 6]] = BR[4, "AAbACbC"]
BR[Knot[7, 7]] = BR[4, "aBaBcBc"]
BR[Knot[8, 1]] = BR[5, "AABaBCbdCd"]
BR[Knot[8, 2]] = BR[3, "AAAAAbAb"]
BR[Knot[8, 3]] = BR[5, "AABacBcdCd"]
BR[Knot[8, 4]] = BR[4, "AAAbAbcBc"]
BR[Knot[8, 5]] = BR[3, "aaaBaaaB"]
BR[Knot[8, 6]] = BR[4, "AAAABacBc"]
BR[Knot[8, 7]] = BR[3, "aaaaBaBB"]
BR[Knot[8, 8]] = BR[4, "aaabACbCC"]
BR[Knot[8, 9]] = BR[3, "AAAbAbbb"]
BR[Knot[8, 10]] = BR[3, "aaaBaaBB"]
BR[Knot[8, 11]] = BR[4, "AABaBBcBc"]
BR[Knot[8, 12]] = BR[5, "AbACbdCd"]
BR[Knot[8, 13]] = BR[4, "AAbAbbcBc"]
BR[Knot[8, 14]] = BR[4, "AAABaBcBc"]
BR[Knot[8, 15]] = BR[4, "AAbACBBBC"]
BR[Knot[8, 16]] = BR[3, "AAbAAbAb"]
BR[Knot[8, 17]] = BR[3, "AAbAbAbb"]
BR[Knot[8, 18]] = BR[3, "AbAbAbAb"]
BR[Knot[8, 19]] = BR[3, "aaabaaab"]
BR[Knot[8, 20]] = BR[3, "aaaBAAAB"]
BR[Knot[8, 21]] = BR[3, "AAABaaBB"]
BR[Knot[9, 1]] = BR[2, "AAAAAAAAA"]
BR[Knot[9, 2]] = BR[5, "AAABaBCbCDcD"]
BR[Knot[9, 3]] = BR[3, "aaaaaaabAb"]
BR[Knot[9, 4]] = BR[4, "AAAAABaBCbC"]
BR[Knot[9, 5]] = BR[5, "aabAbbcBcdCd"]
BR[Knot[9, 6]] = BR[3, "AAAAAABaBB"]
BR[Knot[9, 7]] = BR[4, "AAAABaBCbCC"]
BR[Knot[9, 8]] = BR[5, "AAbAbcBDcD"]
BR[Knot[9, 9]] = BR[3, "AAAAABaBBB"]
BR[Knot[9, 10]] = BR[4, "aabAbbbbcBc"]
BR[Knot[9, 11]] = BR[4, "aaaaBacBc"]
BR[Knot[9, 12]] = BR[5, "AAbACbCDcD"]
BR[Knot[9, 13]] = BR[4, "aaaabAbbcBc"]
BR[Knot[9, 14]] = BR[5, "aabACbCdCd"]
BR[Knot[9, 15]] = BR[5, "aaabACbdCd"]
BR[Knot[9, 16]] = BR[3, "aaaabbAbbb"]
BR[Knot[9, 17]] = BR[4, "aBaBBBcBc"]
BR[Knot[9, 18]] = BR[4, "AAABaBBBCbC"]
BR[Knot[9, 19]] = BR[5, "aBaBBCbdCd"]
BR[Knot[9, 20]] = BR[4, "AAAbACbCC"]
BR[Knot[9, 21]] = BR[5, "aabAbCbdCd"]
BR[Knot[9, 22]] = BR[4, "AbAbCbbbC"]
BR[Knot[9, 23]] = BR[4, "AAABaBBCbCC"]
BR[Knot[9, 24]] = BR[4, "AAbACbbbC"]
BR[Knot[9, 25]] = BR[5, "AAbACBBdCd"]
BR[Knot[9, 26]] = BR[4, "aaaBaBcBc"]
BR[Knot[9, 27]] = BR[4, "AAbAbbCbC"]
BR[Knot[9, 28]] = BR[4, "AAbACbbCC"]
BR[Knot[9, 29]] = BR[4, "aBBcBaBcB"]
BR[Knot[9, 30]] = BR[4, "AAbbAbCbC"]
BR[Knot[9, 31]] = BR[4, "AAbAbCbCC"]
BR[Knot[9, 32]] = BR[4, "aaBaBacBc"]
BR[Knot[9, 33]] = BR[4, "AbAbbACbC"]
BR[Knot[9, 34]] = BR[4, "AbAbCbAbC"]
BR[Knot[9, 35]] = BR[5, "AABaBBCbbDcBDC"]
BR[Knot[9, 36]] = BR[4, "aaaBaacBc"]
BR[Knot[9, 37]] = BR[5, "AAbACbadCbCd"]
BR[Knot[9, 38]] = BR[4, "AABBcBaBCCB"]
BR[Knot[9, 39]] = BR[5, "aabACBadcBcd"]
BR[Knot[9, 40]] = BR[4, "AbACbACbC"]
BR[Knot[9, 41]] = BR[5, "AABacbbDCbCD"]
BR[Knot[9, 42]] = BR[4, "aaaBAAcBc"]
BR[Knot[9, 43]] = BR[4, "aaabaaCbC"]
BR[Knot[9, 44]] = BR[4, "AAABaacBc"]
BR[Knot[9, 45]] = BR[4, "AABaBACbC"]
BR[Knot[9, 46]] = BR[4, "AbAbCBaBC"]
BR[Knot[9, 47]] = BR[4, "AbAbcbAbc"]
BR[Knot[9, 48]] = BR[4, "aabAbaCbAbC"]
BR[Knot[9, 49]] = BR[4, "aabaaCbAbcc"]
BR[Knot[10, 1]] = BR[6, "AABaBCbCDceDe"]
BR[Knot[10, 2]] = BR[3, "AAAAAAAbAb"]
BR[Knot[10, 3]] = BR[6, "AABaBCbdCdeDe"]
BR[Knot[10, 4]] = BR[5, "AAAbAbcBcdCd"]
BR[Knot[10, 5]] = BR[3, "aaaaaaBaBB"]
BR[Knot[10, 6]] = BR[4, "AAAAAABacBc"]
BR[Knot[10, 7]] = BR[5, "AABaBCbCCdCd"]
BR[Knot[10, 8]] = BR[4, "AAAAAbAbcBc"]
BR[Knot[10, 9]] = BR[3, "aaaaaBaBBB"]
BR[Knot[10, 10]] = BR[5, "AAbAbbcBcdCd"]
BR[Knot[10, 11]] = BR[5, "AAAABacBcdCd"]
BR[Knot[10, 12]] = BR[4, "aaaaabACbCC"]
BR[Knot[10, 13]] = BR[6, "AABacBDceDe"]
BR[Knot[10, 14]] = BR[4, "AAAAABaBcBc"]
BR[Knot[10, 15]] = BR[4, "aaaaBaBCbCC"]
BR[Knot[10, 16]] = BR[5, "aabAbbCbCDcD"]
BR[Knot[10, 17]] = BR[3, "AAAAbAbbbb"]
BR[Knot[10, 18]] = BR[5, "AAABaBcBcdCd"]
BR[Knot[10, 19]] = BR[4, "AAAAbAbbcBc"]
BR[Knot[10, 20]] = BR[5, "AAAABaBCbdCd"]
BR[Knot[10, 21]] = BR[4, "AABaBBBBcBc"]
BR[Knot[10, 22]] = BR[4, "aaaabACbCCC"]
BR[Knot[10, 23]] = BR[4, "AAbAbbbbcBc"]
BR[Knot[10, 24]] = BR[5, "AABaBBBCbdCd"]
BR[Knot[10, 25]] = BR[4, "AAAABaBBcBc"]
BR[Knot[10, 26]] = BR[4, "AAAbAbbbcBc"]
BR[Knot[10, 27]] = BR[4, "AAAABaBcBcc"]
BR[Knot[10, 28]] = BR[5, "aabAbbcBDcDD"]
BR[Knot[10, 29]] = BR[5, "AAAbACbdCd"]
BR[Knot[10, 30]] = BR[5, "AABaBBCbCdCd"]
BR[Knot[10, 31]] = BR[5, "AAABacBccdCd"]
BR[Knot[10, 32]] = BR[4, "aaaBaBBCbCC"]
BR[Knot[10, 33]] = BR[5, "AABaBcBccdCd"]
BR[Knot[10, 34]] = BR[5, "aaabAbcBDcDD"]
BR[Knot[10, 35]] = BR[6, "AbAbcBDceDe"]
BR[Knot[10, 36]] = BR[5, "AAABaBCbCdCd"]
BR[Knot[10, 37]] = BR[5, "AAABacBcdCdd"]
BR[Knot[10, 38]] = BR[5, "AAABaBBCbdCd"]
BR[Knot[10, 39]] = BR[4, "AAABaBBBcBc"]
BR[Knot[10, 40]] = BR[4, "aaabAbbCbCC"]
BR[Knot[10, 41]] = BR[5, "aBaBBcBDcD"]
BR[Knot[10, 42]] = BR[5, "AAbAbCbdCd"]
BR[Knot[10, 43]] = BR[5, "AAbACbdCdd"]
BR[Knot[10, 44]] = BR[5, "AAbACbCdCd"]
BR[Knot[10, 45]] = BR[5, "AbAbCbCdCd"]
BR[Knot[10, 46]] = BR[3, "aaaaaBaaaB"]
BR[Knot[10, 47]] = BR[3, "aaaaaBaaBB"]
BR[Knot[10, 48]] = BR[3, "AAAAbbAbbb"]
BR[Knot[10, 49]] = BR[4, "AAAAbACBBBC"]
BR[Knot[10, 50]] = BR[4, "aabAbbCbbbC"]
BR[Knot[10, 51]] = BR[4, "aabAbbCbbCC"]
BR[Knot[10, 52]] = BR[4, "aaaBaaBBCbC"]
BR[Knot[10, 53]] = BR[5, "AABaBcBDCCCD"]
BR[Knot[10, 54]] = BR[4, "aaaBaaBCbCC"]
BR[Knot[10, 55]] = BR[5, "AAABacBDCCCD"]
BR[Knot[10, 56]] = BR[4, "aaabAbCbbbC"]
BR[Knot[10, 57]] = BR[4, "aaabAbCbbCC"]
BR[Knot[10, 58]] = BR[6, "aBacBDCCeDe"]
BR[Knot[10, 59]] = BR[5, "AbAbCbbdCd"]
BR[Knot[10, 60]] = BR[5, "AbAbbCbCBDcD"]
BR[Knot[10, 61]] = BR[4, "aaaBaaaBCbC"]
BR[Knot[10, 62]] = BR[3, "aaaaBaaaBB"]
BR[Knot[10, 63]] = BR[5, "AAbACBBBCDcD"]
BR[Knot[10, 64]] = BR[3, "aaaBaaaBBB"]
BR[Knot[10, 65]] = BR[4, "aabAbCbbbCC"]
BR[Knot[10, 66]] = BR[4, "AAAbACBBBCC"]
BR[Knot[10, 67]] = BR[5, "AAABaBCbbdCBdC"]
BR[Knot[10, 68]] = BR[5, "aaBaBBCbbDcBDC"]
BR[Knot[10, 69]] = BR[5, "aabACbadCbCd"]
BR[Knot[10, 70]] = BR[5, "AbACbbbdCd"]
BR[Knot[10, 71]] = BR[5, "AAbACbbdCd"]
BR[Knot[10, 72]] = BR[4, "aaaabbAbCbC"]
BR[Knot[10, 73]] = BR[5, "AABaBAcBcDcD"]
BR[Knot[10, 74]] = BR[5, "AABaBBCbbdCBdC"]
BR[Knot[10, 75]] = BR[5, "aBaBcBBdCbdc"]
BR[Knot[10, 76]] = BR[4, "aaaabACbbbC"]
BR[Knot[10, 77]] = BR[4, "aaaabACbbCC"]
BR[Knot[10, 78]] = BR[5, "AABaBAcBDcDD"]
BR[Knot[10, 79]] = BR[3, "AAAbbAAbbb"]
BR[Knot[10, 80]] = BR[4, "AAAbAACBBBC"]
BR[Knot[10, 81]] = BR[5, "aaBacbbDCCCD"]
BR[Knot[10, 82]] = BR[3, "AAAAbAbAbb"]
BR[Knot[10, 83]] = BR[4, "aabAbCbbCbC"]
BR[Knot[10, 84]] = BR[4, "aaabACbbCbC"]
BR[Knot[10, 85]] = BR[3, "AAAAbAAbAb"]
BR[Knot[10, 86]] = BR[4, "AAbAbAbbcBc"]
BR[Knot[10, 87]] = BR[4, "aaabACbCbCC"]
BR[Knot[10, 88]] = BR[5, "AbACbCbdCd"]
BR[Knot[10, 89]] = BR[5, "AbAbcBADCbCD"]
BR[Knot[10, 90]] = BR[4, "AAbAbcBAcbb"]
BR[Knot[10, 91]] = BR[3, "AAAbAbbAbb"]
BR[Knot[10, 92]] = BR[4, "aaabbCbAbCb"]
BR[Knot[10, 93]] = BR[4, "AAbAAbAbcBc"]
BR[Knot[10, 94]] = BR[3, "aaaBaaBBaB"]
BR[Knot[10, 95]] = BR[4, "AAbbCbAbccb"]
BR[Knot[10, 96]] = BR[5, "AbaCbaCdCbCd"]
BR[Knot[10, 97]] = BR[5, "aabAbaCbAbcDcD"]
BR[Knot[10, 98]] = BR[4, "AABBcBaBBcB"]
BR[Knot[10, 99]] = BR[3, "AAbAAbbAbb"]
BR[Knot[10, 100]] = BR[3, "AAAbAAbAAb"]
BR[Knot[10, 101]] = BR[5, "aaabAcBacbbdCd"]
BR[Knot[10, 102]] = BR[4, "AAbACbAbbcc"]
BR[Knot[10, 103]] = BR[4, "AABacBBcBBc"]
BR[Knot[10, 104]] = BR[3, "AAAbbAbAbb"]
BR[Knot[10, 105]] = BR[5, "aaBacbbDCbCD"]
BR[Knot[10, 106]] = BR[3, "aaaBaBaaBB"]
BR[Knot[10, 107]] = BR[5, "AAbAcbbDcBcD"]
BR[Knot[10, 108]] = BR[4, "aaBaacBaBCC"]
BR[Knot[10, 109]] = BR[3, "AAbAbbAAbb"]
BR[Knot[10, 110]] = BR[5, "AbACBBBdcBcd"]
BR[Knot[10, 111]] = BR[4, "aabbCbbAbCb"]
BR[Knot[10, 112]] = BR[3, "AAAbAbAbAb"]
BR[Knot[10, 113]] = BR[4, "aaabCbAbCbC"]
BR[Knot[10, 114]] = BR[4, "AABacBcBcBc"]
BR[Knot[10, 115]] = BR[5, "aBacbbDCbCCD"]
BR[Knot[10, 116]] = BR[3, "AAbAAbAbAb"]
BR[Knot[10, 117]] = BR[4, "aabbCbAbCbC"]
BR[Knot[10, 118]] = BR[3, "aaBaBaBBaB"]
BR[Knot[10, 119]] = BR[4, "AAbACbAbccb"]
BR[Knot[10, 120]] = BR[5, "AABacbADCBBCCD"]
BR[Knot[10, 121]] = BR[4, "AABcBaBcBcB"]
BR[Knot[10, 122]] = BR[4, "aabCbACbCbC"]
BR[Knot[10, 123]] = BR[3, "AbAbAbAbAb"]
BR[Knot[10, 124]] = BR[3, "aaaaabaaab"]
BR[Knot[10, 125]] = BR[3, "aaaaaBAAAB"]
BR[Knot[10, 126]] = BR[3, "AAAAABaaaB"]
BR[Knot[10, 127]] = BR[3, "AAAAABaaBB"]
BR[Knot[10, 128]] = BR[4, "aaabaabbcBc"]
BR[Knot[10, 129]] = BR[4, "aaaBAAcBAcB"]
BR[Knot[10, 130]] = BR[4, "aaaBAABBCbC"]
BR[Knot[10, 131]] = BR[4, "AAABaaBBCbC"]
BR[Knot[10, 132]] = BR[4, "aaaBAABCbCC"]
BR[Knot[10, 133]] = BR[4, "AAABaaBCbCC"]
BR[Knot[10, 134]] = BR[4, "aaabaabcBcc"]
BR[Knot[10, 135]] = BR[4, "aaabAbCBBBC"]
BR[Knot[10, 136]] = BR[5, "aBaBCbbdCd"]
BR[Knot[10, 137]] = BR[5, "AbAbCBBdCd"]
BR[Knot[10, 138]] = BR[5, "AbAbcbbDcD"]
BR[Knot[10, 139]] = BR[3, "aaaabaaabb"]
BR[Knot[10, 140]] = BR[4, "aaaBAAABCbC"]
BR[Knot[10, 141]] = BR[3, "aaaaBAAABB"]
BR[Knot[10, 142]] = BR[4, "aaabaaabcBc"]
BR[Knot[10, 143]] = BR[3, "AAAABaaaBB"]
BR[Knot[10, 144]] = BR[4, "AABaBAcBAcb"]
BR[Knot[10, 145]] = BR[4, "AABaBACBaBC"]
BR[Knot[10, 146]] = BR[4, "AAbAbaCbAbC"]
BR[Knot[10, 147]] = BR[4, "aaaBaBCbAbC"]
BR[Knot[10, 148]] = BR[3, "AAAABaaBaB"]
BR[Knot[10, 149]] = BR[3, "AAAABaBaBB"]
BR[Knot[10, 150]] = BR[4, "aaaBaacBAcb"]
BR[Knot[10, 151]] = BR[4, "aaabAAcBacB"]
BR[Knot[10, 152]] = BR[3, "AAABBAABBB"]
BR[Knot[10, 153]] = BR[4, "AAABAAcbbbc"]
BR[Knot[10, 154]] = BR[4, "aabAbacbbbc"]
BR[Knot[10, 155]] = BR[3, "aaabAAbAAb"]
BR[Knot[10, 156]] = BR[4, "AAAbaaCBaBC"]
BR[Knot[10, 157]] = BR[3, "aaabbAbAbb"]
BR[Knot[10, 158]] = BR[4, "AAABaacbAbc"]
BR[Knot[10, 159]] = BR[3, "AAABaBaaBB"]
BR[Knot[10, 160]] = BR[4, "aaabaaCbAbC"]
BR[Knot[10, 161]] = BR[3, "AAABaBAABB"]
BR[Knot[10, 162]] = BR[4, "AABaaBBAcBc"]
BR[Knot[10, 163]] = BR[4, "aaBAAcbAbbc"]
BR[Knot[10, 164]] = BR[4, "aaBaBBCbAbC"]
BR[Knot[10, 165]] = BR[4, "aabACbAbccb"]

End[]; EndPackage[]