This is the subversion repository for the Mathematica package KnotTheory`.

Comments:

* The subdirectory KnotTheory/ should contain precisely the "export"
  part of the package. It is packed to produce KnotTheory.tar.gz,
  KnotTheory.zip, DTCodes4Knots12To16.tar.gz and DTCodes4Knots12To16.zip
  and these files are downloaded by users.

* The subdirectory src/ contains the modules that make up the
  "computational" part of KnotTheory`, plus some support files and old
  leftovers. The modules are packed together to form the file init.m in
  KnotTheory/.

* "Make.nb" packs src/ to make init.m and KnotTheory/ to make
  KnotTheory.tar.gz and KnotTheory.zip. It should be run manually after
  making changes.

* All other files are not documented here. Sorry.

