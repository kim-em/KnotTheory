#! /pub/local/bin/python
#
# The location of your version of python should replace the above.  On
# Unix, the command "which python" will tell you the correct location.
#
# This program computes the boundary slopes of a 2-bridge knot using
# the algorithm of Hatcher and Thurston, Invent. Math. 79, (1983).
# Below, Fig X refers to that figure in their paper.
#
# Written by Nathan Dunfield <nathand@math.uchicago.edu>
#
# Version 1.0.  Dec 4, 1998.
#
# The first function returns a list of the branched surfaces in the
# complement of the two-bridge knot p/q which carry an incompressible
# surface.  Surfaces are stored as:

class surface:
    expansion = []  
    slope = 0

# where expansion is the corresponding continued fraction expansion of
# p/q:  [b1, b2, ... , bn] -> 1/b1 - 1/b2 + 1/b3 ... 

def branched_surfaces(p, q):
    # check input

    if not (0 < p < q): raise ValueError, "must have 0 < p < q"

    # Branched surfaces carrying incompressible surfaces correspond to
    # minimal paths in the heavy edges of the in Fig 5.  The union of
    # the heavy edges will be called D.  The a_i in Fig. 5 are given
    # by

    a = positive_continued_fraction_expansion(p, q)

    # Paths in D are kept track of by a list of the vertices.  The
    # vertices are labeled [-1, k] were i is vertex opposite a_i in
    # Fig. 5 (I start a_i at a_0, not a_1, to be consistent with how
    # python index arrays) .  Thus vertices on the top edge of D are
    # odd, and those on the bottom edge are even.

    paths = minimal_edge_paths(a)

    # Next, we compute the continued fraction expansion corresponding
    # to each edge path, and create the corresponding branched
    # surfaces.

    surfaces = []
    for path in paths:
	surfaces.append(surface_from_path(path, a))
	
    # We compute the slopes of the surfaces:

    twist = seifert_twist(surfaces)
    
    for s in surfaces:
   	compute_slope(s, twist)

    return surfaces
   
def print_surfaces(surfaces):
    for s in surfaces:
	print s.expansion, s.slope

# returns the list of boundary slopes of surfaces with no repeats

def slopes(surfaces):
    slopes = []
    for s in surfaces:
	slopes.append(s.slope)

    slopes.sort()
    unique_slopes = [slopes[0]]
    for slope in slopes[1:]:
	if slope != unique_slopes[-1]: 
	    unique_slopes.append(slope)

    return unique_slopes

# We will need the following

from gcd_tools import*    #gcd, positive_continued_fraction_expansion

# see above for description 

def minimal_edge_paths(a):
    if len(a) < 1: raise ValueError, "input trivial"

    k = len(a)

    # paths start at vertex -1 and end at vertex k
    # there are two possible initial paths

    paths = [ [-1, 1], [-1, 0] ]
    final_paths = []

    # create paths recursively

    while len(paths) != 0:
	new_paths = []
	for path in paths:
	    # check to see if the path has reached the last vertex
	    
	    if path[-1] == k:
		final_paths.append(path)
		continue

	    # otherwise, there are two possible continuations.   The first is:

	    path1 = path[:] + [path[-1] + 1]

	    # We check if it is minimal.  A path is minimal as long as
	    # it doesn't go from j to j + 1 to j + 2 with a_(j+1) = 1.

	    if not (path1[-3] + 1 == path1[-2] == path1[-1] - 1 and a[path1[-2]] == 1):
		new_paths.append(path1)

	    # the other poss. continuation isn't always possible.

	    if path[-1] + 2 <= k:
		path2 = path[:] + [path[-1] + 2]

		if not (path2[-3] + 1 == path2[-2] == path2[-1] - 1 and a[path2[-2]] == 1):
		    new_paths.append(path2)

	paths = new_paths[:]

    return final_paths



# This function computes the continued fraction expansion associated
# to a minimal edge path, and creates a surface with that frac. expansion

def surface_from_path(path, a):   # path, pos cont. frac. exp.
    b = []   # this is the expansion (b_i) in the paper. 

    for i in range (1, len(path)):    
	x, y = path[i - 1], path[i]

	# The associated cont. frac. exp. has one term for each vertex
	# in the full diagram that the path passes through.

	# First we add those b_i corresponding to vertices of the full
	# diagram in the interior of the edge from x to y.  Then we
	# add the b_i corresponding to y.

	# the sign of b_i is determined by whether the vertex is on
	# the top or bottom edge of D.
	
	if y  %2 == 0:
	    sign = 1
	else:
	    sign = -1

	# there are only vertices on the interior of [x,y] if it is
	# horizontal.  each such vertex contributes +/- 2
	
	if y - x == 2:
	    b = b + [sign*2]*(a[y - 1] - 1)
	    
	# now we consider the contribution of y, if y is not the last
	# vertex in the path

	if y != len(a):
	    z = path[i + 1]

	    # contrib of y is a[y] + c where c = 

	    if y - x == 1 and z - y == 1:
		c = 0
	    if y - x == 1 and z - y == 2:
		c = 1
	    if y - x == 2 and z - y == 1:
		c = 1
	    if y - x == 2 and z - y == 2:
		c = 2

	    b.append(sign*(a[y] + c))
        
    s = surface()
    s.expansion = b[:]
    return s

# for a surface s  computes the n_plus - n_minus of Prop. 2

def twist(s):
    n_plus, n_minus = 0, 0
    for b in s.expansion:
	if b > 0:
	    n_plus = n_plus + 1
	else:
	    n_minus = n_minus + 1
    return n_plus - n_minus


# The Seifert surfaces are carried by the unique branched surface where
# every term in the corresponding continued fraction expansion is even.
# this function returns the twist of that surface.

def seifert_twist(surfaces):
    for s in surfaces:
	all_even = 1
	for b in s.expansion:
	    if b % 2 == 1:
		all_even = 0
	if all_even:
	    return twist(s)

# this function computes the slope using Prop. 2

def compute_slope(s, twist_of_seifert):
    s.slope = 2 * (twist(s) - twist_of_seifert)
    
    
	
    



























