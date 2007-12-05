# This program computes the boundary slopes of a Montesinos
# or two-bridge knot.
#
# You will need a Python interperator (version 1.5 or newer) to run
# these programs.  These are free and availible for virtually every
# platform from the Python home page <http://www.python.org>. If you're
# using UNIX, you can see if your system has python installed by
# typing which python. To run do
#
#    UNIX: While in the directory where you put all the files, type: python bdry_run.py
#    
#    MacOS: Run the Python program BuildApplet on bdry_run.py to
#     create an executable.  (Old versions of MacPython: Drag the file
#     bdry_run.py and drop it onto the main Python icon.)
#
#    Windoze: Double click the file bdry_run.py. If this doesn't
#     work, try the instructions for MacOS.
#
# Version 1.0   Dec 4, 1998
# Version 1.1.  Jan 29, 1999. Added more graceful handling of improper input.
# Version 1.1.1 Oct 16, 2000. Changed MacOS instructions. 
#
# Written by Nathan Dunfield <nathand@math.harvard.edu>

import os, re, sys
import two_bridge, montesinos, gcd_tools

out = sys.stdout.write
get_input = sys.stdin.readline

# ask user Question and get back a one character response which must be from Answers

def ask_question(Question, Answers):
	while 1:
		out(Question)
		inp = get_input()[0]    
		if re.search(inp, Answers):
	   		break
	   	out("Sorry, input not understood.\n")
	return inp

# begin main program

out("This program computes the boundary slopes of a Montesinos knot.\n")
out("Written by Nathan Dunfield <nathand@math.harvard.edu>\n\n")
while 1:
	inp = ask_question("Do you want to do a t)wo-bridge knot or a m)ontesinos knot?: ", "tm")
	if inp == 't':
		while 1:
			out("Enter a fraction describing the two-bridge knot, e.g. 5/7: ")
			inp = get_input()[:-1]
			try:
				f = gcd_tools.string_to_frac(inp)
			except:
				out("Sorry, input not understood.\n")
				continue
			if f <= 0:
				out("Sorry, need fraction to be > 0.\n")
				continue
			break
		surfaces = two_bridge.branched_surfaces(f.t, f.b)
		out("K(%s) %s\n" % (f, two_bridge.slopes(surfaces)))
		inp = ask_question("Do you want more information? (y/n): ", "yn")
		if inp == 'y':
			out("\nContinued fraction expansion corresponding to each branched surface\n")
			out("followed by the slope.\n")
			two_bridge.print_surfaces(surfaces)
	
	else: #montesinos knot
		while 1:
			out("Enter fractions describing a montesinos knot, e.g. 1/3 1/3 -1/3 : ")
			tangles = []
			inp = get_input()[:-1]
			try:
				for item in re.split("\s+", inp):
					tangles.append(gcd_tools.string_to_frac(item))
			except:
				out("Sorry, input not understood.\n")
				continue

			if not montesinos.defines_knot(tangles):
				out("Tangles give a link, not a knot.\n")
				continue
			if not montesinos.no_integer_tangles(tangles):
				out("No integer tangles allowed.\n")
				continue
			break
		
		surfaces = montesinos.compute_surfaces(tangles)
		out("K%s\t%s\n" % (tuple(tangles), montesinos.compute_boundary_slopes(surfaces) ))
		out("More info: (slope, number of sheets, euler char)\n")
		out(repr(montesinos.essential_info(surfaces)) + "\n")
		inp = ask_question("Do you want more information? (y/n): ", "yn")
		if inp == 'y':
			out("\n")
			for surface in surfaces:
				if surface.carries_incompressible:
					out(repr(surface) + "\n")			

	if ask_question("Another knot? (y/n): ", "yn") == "n":
		break
	
