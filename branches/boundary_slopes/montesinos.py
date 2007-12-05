# This file contains a program to compute the boundary slopes of a
# Montesinos knot.  It uses the algorithm of Hatcher and Oertel
# The code follows their article very closely.  References to say Thm 2.5
# etc. below are to the Thm 2.5 below.  This file uses the base classes
# in montesinos_base.py and also gcd_tools.py.
#
# For an example of the use of the main function in this file
# see the file montesinos_bdry.py (which is what you would
# use just to get output).  
#
# Written by Nathan Dunfield <nathand@math.harvard.edu>
#
# Version 1.0. Dec 4, 1998.
# Version 1.1  Nov 24, 2002.  Made to work with Python 2.*


from montesinos_base import *

# Functions needed for calc_Seifert_twist()

# takes a path which is assumed to project to a single edge in the
# reduced diagram and continues it to the left edge of T.

def continue_path_wo_changing_in_reduced(path):
    while path[0].u() != 0:
        a, b = path[0].leftward_neighbors()
        if a.reduced() == path[1].reduced():
            path = [a] + path[ : ]
        else:
            path = [b] + path[ : ]
            
    return path
        
def paths_to_infinity_using_only_one_edge_of_reduced_diagram(tangle):
    cont = continue_path_wo_changing_in_reduced
    v = vertex_of_D(tangle)
    a, b = v.leftward_neighbors()
    if tangle.b % 2 == 0:
        a, b = v.leftward_neighbors()
        return [ cont( [a, v] ) ,  cont( [b, v] )  ]
    else:
        if a.reduced() == frac(1,0):
            return cont( [ a, v] )
        else:
            return cont( [ b, v] )

def to_left_edge_via_odd_denominators(tangle):
        v = vertex_of_D(tangle)
        a, b = v.leftward_neighbors()
        if a.reduced().b == 1:
            return  continue_path_wo_changing_in_reduced( [a, v] )
        else:
             return  continue_path_wo_changing_in_reduced( [b, v] )
        
# follows top of page 461

def comp_Seifert_twist(tangles):
    n = len(tangles)
    even_qi_exists = 0
    
    for i in range(0, n):
        if tangles[i].b % 2 == 0:
            even_qi_exists = 1
            break

    if even_qi_exists:
        # Seifert surface consists a edge paths which go all the way
        # to <1/0> and use only one edge of the reduced diagram.
        # There is a uniques such except of the ith tangle where qi is
        # even

        paths_to_infinity = []
        for j in range(0, i) + range(i+1, n):
            paths_to_infinity.append(
                paths_to_infinity_using_only_one_edge_of_reduced_diagram(tangles[j]))

        num_of_odd_penultimate_slopes = 0
        for path in paths_to_infinity:
            if path[0].p() % 2 == 1:
                 num_of_odd_penultimate_slopes = num_of_odd_penultimate_slopes + 1

        for path in paths_to_infinity_using_only_one_edge_of_reduced_diagram(tangles[i]):
            if (path[0].p() + num_of_odd_penultimate_slopes) % 2 == 0:
                temp_paths = paths_to_infinity[ : i] + [path] + paths_to_infinity[i : ]
                real_paths = []
                for j in range(0, n):
                    real_paths.append(edgepath(j, temp_paths[j]) )
                surface = branched_surface( real_paths, "III", frac(1,0) )

    else: # all qi are odd.
        paths = []
        for i in range(0,n):
            real_path = edgepath(i, to_left_edge_via_odd_denominators(tangles[i]))
            paths.append(real_path)
        surface = branched_surface(paths, "II", 0)
        sum_of_endpoints = 0
        for path in paths:
            sum_of_endpoints = sum_of_endpoints + path[0].p()
        surface.twist = surface.twist + 2* sum_of_endpoints
          
    return surface.twist

# returns the trees T_i.  Differs from paper in that if a tangle does
# not have maximum denominator, the root of the tree extends all the
# way back to u = 1.  When the function first creates a node, 

def build_trees(tangles, max_denominator):
    trees = []
    
    # create root and (sometimes) first edge
    for t in tangles:
        if t.b == max_denominator:
            v = vertex_of_D(t)
            n = node(v, None)
            trees.append(n)
            extend_tree(n)
        else:
            v = vertex_of_D(t)
            n1 = node(v, None)
            n2 = node(v, n1)
            n1.leftward[0] = n2
            trees.append(n1)
            extend_tree(n2)

    return trees

# takes node and extends tree recursively to left edge of T

def extend_tree(n):
    if n.vertex.u() == 0:  
        return   #allready to left edge of T
    
    left = n.vertex.leftward_neighbors()
    for i in [0,1]:
        v = left[i]
        
        #check for minimality
        if n.back and n.back.vertex != n.vertex:
            if joined_by_edge(v, n.back.vertex):
                continue

        # create new node
        new_node = node(v, n)
        n.leftward[i] = new_node
        extend_tree(new_node)

def print_tree(n, level = 0):
    if n:
        print level*"\t",  n
        for i in [0,1]:
            print_tree( n.leftward[i], level + 1)                    

# given a collection of edge created in create_type_I_surfaces,
# creates the associated linear equation:
#
#  (sum of v cor over edges)  = a * u + b = 0
#
#  and returns (a,b)

def create_equation(edges):
    a = b = 0
    for edge in edges:
        if edge[0].vertex == edge[1].vertex:  #edge flat
            b = b + edge[1].v()
        else:
            p , q = edge[0].p(), edge[0].q()
            r,  s = edge[1].p(), edge[1].q()
            a = a + frac(p*s - q*r, q - s)
            b = b + frac(p - r + q*r - p*s, q - s)
    return a, b

# given one edge in each tree, and a value of u, constructs the
# associated surface.

def create_type_I(edges, u):
    paths = []
    for t in range(0, len(edges)):
        edge = edges[t]
        if u == edge[0].u():
            path = [ edge[0].vertex ]
            cur_node = edge[0]
        elif u == edge[1].u():
            path = [ edge[1].vertex ]
            cur_node = edge[1]
        else:
            q = edge[0].q()
            s = edge[1].q()
            if edge[0].vertex == edge[1].vertex:  # on  a horizonal strip
                km = q*(1 - u)
                path = [ interior_of_edge_of_D( edge[0].v() , edge[1].v(), km ) ]
            else:
                # sometimes causes overflow, so use large_div
                km  =  (1 + s * ( -1 + u)) / ( (s - q)*(-1 + u) )
                path = [ interior_of_edge_of_D( edge[0].v() , edge[1].v(), km ), edge[1].vertex ]
            cur_node = edge[1]

        # add rest of path

        while cur_node.back and cur_node.vertex != cur_node.back.vertex:
            path.append(cur_node.back.vertex)
            cur_node = cur_node.back

        paths.append(edgepath(t, path))

    return branched_surface(paths, "I", u)            
     

def create_type_I_surfaces(tangles):
    surfaces = []
    
    max_denom = max( map( lambda x: x.b, tangles ))
    trees = build_trees(tangles, max_denom)
    
    # We look at systems whose ending point has u coordinate in
    # [(k-2)/(k - 1), (k-1)/k] for k in [1, max_denom].  We keep a list
    # edges, indexed by the tangles, so that edge[i] is a list of
    # edges of the form [node, node] where the projection that edge
    # onto the u axis contains [(k-2)/(k - 1), (k - 1)/k ]

    edges = []
    for tree in trees:
        edges_for_this_tangle = []
        if tree.q() == max_denom:
            for i in [0,1]:
                edges_for_this_tangle.append( [ tree.leftward[i], tree ] )
        else: # flat initial segment
            edges_for_this_tangle.append( [ tree.leftward[0], tree ] )
        edges.append(edges_for_this_tangle)

    for k in range(max_denom, 1, -1):   # [max_denom, ... , 2]
        for edge_choices in product_of_lists(edges):
            a, b = create_equation(edge_choices)   #(sum of v cor over edges)  = a * u + b = 0
            
            # if a == 0, there is either no solution, or a
            # non-isolated one.  In the latter case I will handle it
            # with as small of k as possible (i.e. at the leftmost
            # endpoint of the region of solution).  There is a small
            # point of concern here.  I believe that you should just
            # be able to ignore non-isolated solutions until they
            # become isolated, but I'm not sure this is the case.  At
            # worst I'm reporting multiple copies of the same surface, most
            # of which will be removed later.
            
            if a == 0 and b == 0 and k != 2:  # if k == 2, handle this as a type II solution
                u = frac (k-2,k-1)
                surface = create_type_I(edge_choices, u)
                surface.carries_incompressible = test_when_no_vertical_edges(surface)
                surfaces.append(surface)
                surface.from_non_iso_solution = 1
      
            if  a != 0:
                u = -b/a
                if frac(k-2,k-1) <  u <= frac(k - 1, k):   # have solution in desired interval
                    surface = create_type_I(edge_choices, u)
                    surface.carries_incompressible = test_when_no_vertical_edges(surface)
                    surfaces.append(surface)
      
        # change edges if necessary for decreasing k

        new_edges = []
        for edge_in_tangle in edges:
            new_edges_in_tangle = []
            for edge in edge_in_tangle:
                if edge[0].q() == k - 1:
                    for i in [0,1]:
                        if edge[0].leftward[i]:
                            new_edges_in_tangle.append([edge[0].leftward[i], edge[0] ])
                else:
                    new_edges_in_tangle.append(edge)
            new_edges.append(new_edges_in_tangle)
        edges = new_edges[:]

    # weed out duplicates
    final_surfaces = []
    for surface in surfaces:
        match_found = 0
        for other_surface in final_surfaces:
            if surface == other_surface:
                match_found = 1
                break
        if not match_found:
            final_surfaces.append(surface)
    return final_surfaces
                                       
def create_all_edgepaths_to_left_edge_of_T(tangle_num, tangles):
    paths = [ [ vertex_of_D( tangles[tangle_num] ) ] ]         
    completed_paths = []

    while len(paths) > 0:
        new_paths = []
        for path in paths:
            for v in path[0].leftward_neighbors():
                
                # check if adding v to path would make a minimal path

                if len(path) != 1 and  joined_by_edge(v, path[1]): continue

                if abs(v.q()) == 1:   #have we reached left edge of T?
                    completed_paths.append( [v] + path )
                else:
                    new_paths.append( [v] + path )

        paths = new_paths[:]
            
    final_paths = []
    for path in completed_paths:
        final_paths.append( edgepath(tangle_num, path))

    return final_paths

# Cor. 2.4: A candidate surface is incompressible unless the cycle of
# r-values for the final edges is one of the following types:
# (0, r1, ..., rn), (1, ..., 1, rn) or (1, ... , 1, 2, rn).
#
# returns 0 if the test inconclusive, 1 if incompressible.
#
# if cycle of r-values is of the latter 2 types, rearranges
# paths so that the cycle is in the form shown.  Note that
# the cycle is only determined up to rotations and reflection.
# Reflection may be needed in the last case.

def apply_cor_2_4(surface):
    number_of_ones = 0
    n = len(surface.edgepaths)
    for path in surface.edgepaths:
        r = path.r_value
        if r == 0: return 0
        if abs(r) == 1: number_of_ones = number_of_ones + 1

    if number_of_ones == n:
            return 0
        
    if number_of_ones ==  n - 1:
        for i  in range(0, n):
            if abs(surface[i].r_value ) != 1:
                surface.cycle_paths_so_last(i)
                return 0

    if number_of_ones == n - 2:
        for i in range(0,n):
            # because of def of surface, below valid even if i = n - 1
            if  abs(surface[i].r_value ) == 2 and abs(surface[ i + 1 ].r_value) != 1:  
                surface.cycle_paths_so_last(i + 1)
                return 0
            if  abs(surface[i].r_value ) == 2 and abs(surface[ i - 1 ].r_value) != 1:  
                surface.cycle_paths_so_last(i)  # (1, ... ,1, r, 2)
                surface.reflect()               # (2, r, 1, ...  1)
                surface.cycle_paths_so_last(1)  # (1,...,1,2,r)
                return 0
            

    return 1

# needed functions for test_when_no_vertical_edges
#
# takes two edge paths and determines if they have the same ending
# point and the final edges lie on the same triangle.

def same_ending_point_and_triangle(path0, path1):
    if path0[0] ==  path1[0]:  #have same ending point
        if joined_by_edge(path0[1],path1[1]) or path0[1] == path1[1]:   #lie on same triangle
            return 1
    return 0

def pairs_of_ones_have_opp_signs(surface):
    for i in range(0, len(surface.edgepaths)):
        if surface[i].r_value * surface[i + 1].r_value == 1:
            return 0
    return 1

def all_signs_alternate(surface):
    for i in range(0, len(surface.edgepaths)):
        if surface[i].r_value * surface[i + 1].r_value > 0:
            return 0
    return 1

def ones_have_opp_sign_from_neighbors(surface):
    for i in range(0, len(surface.edgepaths)):
        if abs(surface[i].r_value) == 1:
            if surface[i].r_value * surface[i + 1].r_value > 0:
                return 0
            if surface[i - 1].r_value * surface[i].r_value > 0:
                return 0
    return 1
                                          
# Uses Cor 2.4 and Prop. 2.6-7 to check compressiblity.  Applys to
# systems contained in T with no vertical edges (type I and II systems)

def test_when_no_vertical_edges(surface):
    n = len(surface.edgepaths)

    # test for constant edgepaths

    for i in range(0, n):
        if len(surface[i].path) == 1:
            return 1
        
    if apply_cor_2_4(surface):
        return 1

    # if r-cycle is (1,...,1, r) apply Prop. 2.6

    final_r = abs(surface[-1].r_value)
    if abs(surface[-2].r_value) == 1:
        if pairs_of_ones_have_opp_signs(surface):
            if n % 2 == 1:   # case (1)
                if final_r % 2 == 0:   # case (b)
                    if surface.num_reversible() >= n - 1 and surface[-1].completely_reversible:
                        return 0
            else:  # case(2)  n even
                # first check condition in (a) that the last edge of
                # surface[-1] lies in the same triangle of T and has
                # the same ending point, as an edge with r = 1.  There
                # is the poss that final_r ==1

                    
                if all_signs_alternate(surface):
                    if final_r == 1:
                        if  surface.num_reversible()  >= n - 2:
                                return 0
                    else:
                        for i in range(0, n-1):
                            if same_ending_point_and_triangle(surface[i], surface[-1] ):
                                # case 2(b).  Are n - 2 paths with r = 1 comp. reversible?  Yes -> comp.
                                if surface.num_reversible(range(0, n-1)) >= n - 2:
                                    return 0

                           

    else:    # r-values  are (1,...1,2,r) and we implement Prop 2.7
        if ones_have_opp_sign_from_neighbors(surface):
            if final_r == 2:  # case (1)
                if surface.num_reversible() >= n - 1: return 0  #(a)
                
            elif final_r == 4: # case (2)
                if surface.num_reversible() == n: return 0  # (b)
                
            else: #case(3)
                if (same_ending_point_and_triangle(surface[-2], surface[-1]) and
                    surface.num_reversible(range(0, n-1)) == n - 1):
                    return 0  # (b)

    return 1
        
# applys Prop 2.9 to the type II surface the sum of whose endpoints on
# the left edge of T is sum.  Returns 1 if system carries an
# incompressible surface, 0 otherwise.

def apply_proposition_2_9(surface, sum):
    e = sum/abs(sum)  # +/- 1
    
    # the system does not extend to one which carries an
    # incompressible surface if the cycle of r-values contains at
    # least one e and every e is separated from the next by e by
    # r-values all but possibly one of which is a completely
    # reversible e*2.

    e_found = 0
    for i in range(0, len(surface.edgepaths)):
        if surface[i].r_value == e:
            e_found = 1
            
    if not e_found: return 1
    for i in range(0, len(surface.edgepaths)):
        if  surface[i].r_value == e:
            j = i + 1
            non_comp_rev_e2s = 0
            while surface[j].r_value != e:
                if surface[j].r_value != e*2 or not surface[j].completely_reversible:
                    non_comp_rev_e2s = non_comp_rev_e2s + 1
                j = j + 1
            if non_comp_rev_e2s > 1:
                return 1

    return 0

def create_type_II_and_III_surfaces(tangles):
    path_poss = []
    for i in range(0, len(tangles)):
        path_poss.append( create_all_edgepaths_to_left_edge_of_T(i, tangles) )

    # loop over all possible curve systems

    surfaces = []
    
    for paths in product_of_lists(path_poss):
        # needed to determine incompressibility
        
        sum_of_endpoints = 0
        for path in paths:
            sum_of_endpoints = sum_of_endpoints + path[0].p()

        # Create corresponding type II surface

        surface = branched_surface(paths, "II", 0)

        # determine whether the branched surface carries an
        # incompressible surface.  
        
        if(sum_of_endpoints ==  0):
            surface.carries_incompressible = test_when_no_vertical_edges(surface)
        else:
            # have to adjust the twist in this case
            surface.twist = surface.twist + 2* sum_of_endpoints
            surface.carries_incompressible = apply_proposition_2_9(surface, sum_of_endpoints)

        surfaces.append(surface)
        #print surface
        
        # Create corresponding type III surface
        # (edgepaths continued to <1/0> )
        
        surface = branched_surface(paths, "III", frac(1,0))

        # apply proposition 2.5 to determine whether the branched
        # surface carries an incompressible surface

        if abs(sum_of_endpoints) <= 1 and surface.num_reversible() >= len(tangles) - 2 :
            surface.carries_incompressible = 0
        else:
            surface.carries_incompressible = 1

        surfaces.append(surface)
        
    return surfaces

# For checking if given tangle defines a knot

def count_even_qi(tangles):
    count  =  0
    for t in tangles:
        if t.b % 2== 0:
            count = count + 1
    return count

def count_odd_pi(tangles):
    count = 0
    for t in tangles:
        if t.t % 2 == 1:
            count = count + 1
    return count

def defines_knot(tangles):
    if count_even_qi(tangles) == 1:
        return 1
    if count_even_qi(tangles) == 0 and count_odd_pi(tangles) % 2 == 1:
        return 1
    return 0

# The program does not work with integer tangles.

def no_integer_tangles(tangles):
    for tangle in tangles:
        if tangle.b == 1:
            return 0
    return 1
            
# the main function:

def compute_surfaces(tangles):
    if not defines_knot(tangles):
        raise ValueError, "tangles give a link, not a knot"
    if not no_integer_tangles(tangles):
        raise ValueError, "no integer tangles allowed"
    surfaces = create_type_I_surfaces(tangles) + create_type_II_and_III_surfaces(tangles)
    seifert_twist = comp_Seifert_twist(tangles)
    for surface in surfaces:
        surface.comp_slope(seifert_twist)
    if len(tangles) > 3:
        surfaces.append(conway_sphere())
    return surfaces

# functions below for printing out information about the surfaces

# returns the boundary slopes of all incompressible surfaces

def compute_boundary_slopes(surfaces):
    slopes = []
    for surface in surfaces:
        if surface.carries_incompressible:
            slopes.append(surface.slope)
    slopes.sort()
    unique_slopes = [slopes[0]]
    for slope in slopes[1:]:
        if slope != unique_slopes[-1]: 
            unique_slopes.append(slope)
            
    return unique_slopes
            
# returns a list of tuples (slope, num_sheets, euler_char) about each
# incompressible surface.

def essential_info(surfaces):
    info = []
    for surface in surfaces:
        if surface.carries_incompressible:
            info.append((surface.slope, surface.num_sheets, surface.euler_char))
    info.sort()
    return info

if __name__ == "__main__":
    None

        
    







