#! /bin/env python
#
# The location of your version of python should replace the above.  On
# Unix, the command "which python" will tell you the correct location.
#
# Under Unix, you also need to set the file permissions so that it is
# executeable (e.g. chmod +x two_bridge_bdry.py)
#
# For an expanation of what this program does see the help string below. 
#
# Written by Nathan Dunfield <nathand@math.uchicago.edu>
#
# Version 1.0.  Dec 4, 1998.

from two_bridge import branched_surfaces, slopes, print_surfaces
import sys

def main():
    arg_list = []
    optlist = []
    for arg in sys.argv[1:]:
        if arg == "-f":
            optlist.append(arg)
        else:
            arg_list.append(arg)
	    
    try:
	p, q = map(int, arg_list)
    except:
      	print "Must supply two integers.\n"
      	help_then_exit()

    if not 0 < p < q:
        print "Need 0 < p < q, sorry"
        sys.exit(0)
       	
    if not optlist:
	str = "K(%d/%d) %s\n" % (p, q, slopes(branched_surfaces(p,q)))
	sys.stdout.write(str)
	sys.stdout.flush()
    else:
	surfaces = (branched_surfaces(p,q))
	str = "K(%d/%d) %s\n\n" % (p, q, slopes(branched_surfaces(p,q)))
	sys.stdout.write(str)
	print_surfaces(surfaces)
		    
def help_then_exit():
    print "usage: %s [-f] p q  " % sys.argv[0]
    print "   computes the boundary slopes of the 2-bridge knot p/q. If"
    print "   the -f option is not given, the program prints the name of"
    print "   the knot followed by a list of boundary slopes.  If -f is "
    print "   given, the program follows this information with a list of"
    print "   the continued fraction expansions corresponding to each"
    print "   branched surface which carries an incompressible surface,"
    print "   followed by the slope. "
    sys.exit(0)


main()
