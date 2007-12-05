#! /bin/env python
#
# The location of your version of python should replace the above.  On
# Unix, the command "which python" will tell you the correct location.
#
# Under Unix, you also need to set the file permissions so that it is
# executeable (e.g. chmod +x montesinos_bdry.py)
#
# For an explanation of what this program does see the help string below.
#
# Written by Nathan Dunfield <nathand@math.uchicago.edu>
#
# Version 1.0.  Dec 4, 1998.
# Version 1.1.  Jan 29, 1999. Added more graceful handling of improper input

import os, sys, re, getopt
from montesinos import compute_surfaces, compute_boundary_slopes,  essential_info, defines_knot, no_integer_tangles
from gcd_tools import string_to_frac

help = """usage: %s [-s -f -a] a/b c/d e/f ... 
    computes the incompressible surfaces in the complement of the given
   Montesinos knot K(a/b, c/d,...).  Prints the name of the knot followed
   by a list of boundary slopes, and then a some more detailed
   information about the incompressible surfaces.  If the -s option is
   given prints only the name and list of boundary slopes.  If the -f
   option is given it also prints full information about each
   incompressible surfaces (e.g. the edgepaths in the diagram).  If the
  -a option is given, it prints full information about all the candidate
   surfaces.
""" % sys.argv[0]

def main():
    tangles = []
    optlist = []
    for arg in sys.argv[1:]:
        if arg == "-s" or arg == "-f" or arg == "-a":
            optlist.append(arg)
        else:
            try:
                tangles.append( string_to_frac(arg))
            except ValueError:
                print "%s is not a fraction or an option\n" % arg
                print help
                sys.exit(0)
         
    if len(tangles) < 3:
        print "Need at least 3 tangles, else use two-bridge program\n"
        print help
        sys.exit(0)

    if not defines_knot(tangles):
        print "Tangles give a link, not a knot."
        sys.exit(0)
    if not no_integer_tangles(tangles):
        print "No integer tangles allowed."
        sys.exit(0)

    surfaces = compute_surfaces(tangles)
    str = "K%s\t%s\n" % (tuple(tangles), compute_boundary_slopes(surfaces) )
    sys.stdout.write(str)

    if not optlist.count("-s"):
        sys.stdout.write("More info: (slope, number of sheets, euler char)\n")
        sys.stdout.write( repr(essential_info(surfaces)) + "\n")
    if optlist.count("-f"):
        for surface in surfaces:
            if surface.carries_incompressible:
                sys.stdout.write(repr(surface) + "\n")
    if optlist.count("-a"):
        for surface in surfaces:
            sys.stdout.write(repr(surface) + "\n")

    sys.stdout.flush()

		    
main()
 

