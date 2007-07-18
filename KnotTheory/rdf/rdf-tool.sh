#!/bin/sh
#
# Configuration variables
#
# JAVA_HOME
#   Home of Java installation.
#
# JAVA_OPTIONS
#   Extra options to pass to the JVM
#


# ----- Verify and Set Required Environment Variables -------------------------

if [ "$JAVA_HOME" = "" ] ; then
  echo You must set JAVA_HOME to point at your Java Development Kit installation
  exit 1
fi

if [ "$JAVA_OPTIONS" = "" ] ; then
  JAVA_OPTIONS='-Xms32M -Xmx512M -Djava.awt.headless=true'
fi

# ----- Set Local Variables ( used to minimize cut/paste) ---------------------

JAVA="$JAVA_HOME/bin/java"
PARSER="-Dorg.xml.sax.parser=org.apache.xerces.parsers.SAXParser"
LOADER="Loader"
LOADER_LIB="./tools/loader/classes"
MAIN="-Dloader.main.class=org.katlas.rdf.KnotAtlasRDFTool"
# and a horrible hack to avoid having to worry about whether the path separator
#   on this platform is ";" or ":"
LIBRARIES="-Dloader.jar.repositories=target/classes:target:lib:;target/classes;target;lib"

# ----- Do the action ----------------------------------------------------------

$JAVA $JAVA_OPTIONS -cp $LOADER_LIB $LIBRARIES $MAIN $PARSER $LOADER $*

exit 0
