This is the subversion repository for the Mathematica package KnotTheory`.

Comments:

* The subdirectory KnotTheory/ contains precisely the "export"
  part of the package. It is packed to produce KnotTheory.tar.gz,
  KnotTheory.zip, DTCodes4Knots12To16.tar.gz and DTCodes4Knots12To16.zip
  and these files are downloaded by users.

* The subdirectory src/ contains the modules that make up the
  "computational" part of KnotTheory`, plus some support files and old
  leftovers. The modules are packed together to form the file init.m in
  KnotTheory/.

* "makefile" packs src/ to make init.m and KnotTheory/ to make
  KnotTheory.tar.gz and KnotTheory.zip. It is run automatically every time
  changes to this repository are committed with a changed version of the file
  "post". The file "post" is never read; its only purpose is to signal a post
  command to the makefile.

* All other files are not documented here. Sorry.

