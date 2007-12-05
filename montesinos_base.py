# Written by Nathan Dunfield <nathand@math.uchicago.edu>
#
# Version 1.0.  Dec 4, 1998.

from gcd_tools import *

# The following two classes are used to store the vertices of an edge
# path.  The first is denoted <p/q> in the paper.  I will try to
# insure that q is always > 0, and the gcd(p,q) = 1.

class vertex_of_D:
    # can take either a fraction or a pair of integers
    def __init__(self, p, q="noarg"):
        if q == "noarg":
            self.frac = p.copy()
        else:
            self.frac = frac(p,q)

    def __repr__(self):
        return "<%s>" % self.frac

    def __cmp__(self, other):
        if not isinstance(other, vertex_of_D): return 1
        return cmp(self.frac, other.frac)
    
    def p(self):
        return self.frac.t

    def q(self):
        return self.frac.b

    def u(self):
        return frac(self.frac.b - 1, self.frac.b)

    def v(self):
        return self.frac.copy()

    # min num of arcs needed to realize this diagram
    
    def num_arcs(self):
        return 1
    
    # returns the corresponding fraction in the collasped diagram with
    # only three vert.
    
    def reduced(self):
        return  frac(self.frac.t % 2, self.frac.b % 2)
        
    
    # finds the two leftward neighbors of a vertex of T

    def leftward_neighbors(self):
        # The two vertices to the left of <p/q> are <r/s>
        # where sp - rq = +/-1 and s < q

        p, q = self.frac.t, self.frac.b
        g, s, r = euclidean_algorithm(p, q)   # 1 = sp + rq
        if(g != 1) or q <= 1: raise ValueError, "bad vertex %i,%i" % (p,q)

        # change so sp - rq = +/-1 and r > 0

        r = -r
        if r < 0: r, s = -r, -s
        
        # all solutions of s'p - r'q = +/-1 are of form
        # s' = (s + aq), r = (r + aq)
        
        a = -s/q

        # return so that the vertex with larger v coordinate is second
        
        ret = [vertex_of_D (r + a*p, s + a*q),  vertex_of_D (r + (a + 1)*p, s + (a + 1)*q)]
        if ret[0] > ret[1]:
            ret.reverse()
        return ret

# the second kind of vertex in an edge path is  k/m <p/q> + (m - k)/m <r/s>.

class interior_of_edge_of_D:
    # takes three fractions to define -- k/m must be between 0 and 1
    def __init__(self, pq, rs, km):         
        self.pq = pq.copy()
        self.rs = rs.copy()
        self.km = km.copy()
        if not 0 <= km <= 1:
            raise ValueError, "need 0 <= km <=1"

    def p(self):
        return self.pq.t

    def q(self):
        return self.pq.b

    def r(self):
        return self.rs.t

    def s(self):
        return self.rs.b

    def __repr__(self):
        return "%s<%s> + %s<%s>" % (self.km, self.pq, 1 - self.km, self.rs)

    def __cmp__(self, o):
        if not isinstance(o, interior_of_edge_of_D): return 1
        if self.pq == o.pq and self.rs == o.rs and self.km == o.km:
            return 0
        return 1

    # min num arcs needed to realize system
    def num_arcs(self):
        if self.pq == self.rs:
            return self.km.t
        return self.km.b

        
# Decides if two vertices of D are joined by an edge

def joined_by_edge(v, w):
    if (not isinstance(v, vertex_of_D)) or (not isinstance(w, vertex_of_D)): raise TypeError
    return abs(v.p() * w.q() - v.q()*w.p()) == 1


class edgepath:
    def __init__(self, tangle_num, given_path):
        self.tangle = tangle_num   # which tangle this is an edge path for
        self.path =  given_path       # list of vertices from left to right
        self.r_value = self.compute_final_r_value()
        self.completely_reversible = self.decide_reversibility()

    def __getitem__(self, i): return self.path[i]

    def __len__(self): return len(self.path)
    
    def __repr__(self):
        return "tangle: %i,  r = %i, cr = %i, %s" % (self.tangle,
                                              self.r_value, self.completely_reversible, self.path)

    def compute_final_r_value(self):
        path = self.path
        if len(path) <= 1: return 0
    
        p, q = path[0].p(), path[0].q()
        r, s =  path[1].p(), path[1].q()
        
        # using that <p, q> has (u,v) coordinates ( (q - 1)/q, p/q ) its
        # easy to calculate that the intersection of the line through <p,
        # q> and <r, s> with the right edge of T (u = 1) is given by:
        # (p - r)/(q - s)

        rr = frac(p -r, q -s).b

        if p*s <  q*r: rr = -rr
        return rr
    
    def decide_reversibility(self):
        path = self.path
        
        # determine if path is completely reversible.  This is the case if
        # for each pair of successive segments of the path lie on
        # triangles of D sharing a common face.
        
        if len(path) <=2: return 1

        # change leftmost segment if necessary

        if isinstance(path[0], interior_of_edge_of_D):
            path = [vertex_of_D(path[0].p(), path[0].q())] + path[1:]
          
        for i in range(2, len(path)):
            for v in path[i].leftward_neighbors():
                if not joined_by_edge(v, path[i - 2]): return 0

        return 1

    def twist(self):

        # Formula for twist is tau = 2 ( down - up ) where down is the
        # number of edges which derease slope and up is the number
        # which increase slope.

        tau = 0
        path = self.path
        if len(path) <= 1: return 0

        # iterate over each edge [v, w]

        for i in range(0, len(path) - 1):
            v, w = path[i], path[i+1]

            # fractional twist possible for final edge
            
            if i == 0 and isinstance(v, interior_of_edge_of_D):
                if v.pq < v.rs:
                    tau = tau + 2*v.km
                else:
                    tau = tau - 2*v.km
            else:
                if v.frac < w.frac:
                    tau = tau + 2
                else:
                    tau = tau - 2

        return tau

    def __cmp__(self, o):
        if not isinstance(o, edgepath): raise TypeError
        if self.tangle == o.tangle and self.path == o.path:
            return 0
        return 1

# a branched surface is a collection of edgepaths one for each tangle.
# The tangles are regarded as cyclically ordered, and if b is
# branched_surface, b[i] is the (i % n)th edgepath, where n = num of
# tangles.

class branched_surface:
    def __init__(self, paths, type, u):
        self.edgepaths = paths      # a cyclically ordered list of edgepaths (edgepaths[0] not ness. for tangle 0)
        self.type = type   # I, II, or III        
        self.u = u              # u coordinate of ending point
        self.twist = self.compute_twist()
        self.slope = "?"
        self.carries_incompressible = 0
        self.num_sheets = self.comp_sheets()
        self.euler_char = self.comp_euler_char()
        self.from_non_iso_solution =  0   # used only in regression testing against Oretel's ver.
    
    def __repr__(self):
        if self.carries_incompressible:
            s = "type %s incompressible,  " % self.type
        else:
            s = "type %s compressible,  " % self.type
        s = s + "u = %s, slope: %s, twist %s, sheets: %i, euler: %i\n" % (self.u,
                                                                                self.slope, self.twist, self.num_sheets,
                                                                                self.euler_char)
        
        # we sometimes re-arrange the paths, so print them in the
        # standard order
        
        for i in range(0, len(self.edgepaths)):
            for path in self.edgepaths:
                if path.tangle == i:
                    s = s + "%s" % path + "\n"
                    
        return s

    # with no args, returns the number of edgepaths that are
    # completely reversible.  with arg, which should be a range of
    # numbers, returns the number of edgepaths in that range that are
    # comp. reversible.
    
    def num_reversible(self, r="noarg"):
        num = 0
        if r == "noarg":
            r = range(0, len(self.edgepaths) )
        for i in r:
            num = num + self[i].completely_reversible
        return num

    def compute_twist(self):
        twist = 0
        for path in self.edgepaths:
            twist = twist + path.twist()
        return twist

    def comp_slope(self, seifert_twist):
        self.slope = self.twist -  seifert_twist

    def comp_sheets(self):
        # If the endpoint of each path is written (a_i, b_i, c_i) then
        # the number of sheets is the lcm of the a_i.   a_i = minimal # of arcs
        # needed to represent that point in the diagram
        a = []
        for path in self.edgepaths:
            a.append( path[0].num_arcs() )
        return lcm(a)

    def comp_euler_char(self):
        n = len(self.edgepaths)
        sheets = self.num_sheets
        euler_char = 0
        
        #compute euler char of each piece seperately, add together
        
        for path in self.edgepaths:
            # base disks
            if len(path) == 1 and isinstance(path[0], interior_of_edge_of_D):
                k , m = path[0].km.t, path[0].km.b
                euler_char = euler_char + 2*sheets + (m-k)*(sheets/k)   # = 2m arcs + m - k circles
            else:
                euler_char = euler_char + 2*sheets

            #count saddles

            num_saddles = 0

            if len(path) != 1:
                # First edge is special case:
                if isinstance(path[0], interior_of_edge_of_D):
                    num_saddles = num_saddles + path[0].km.t * sheets / path[0].km.b
                else:
                    num_saddles = num_saddles + sheets
                # rest of edges
                num_saddles = num_saddles + sheets * (len(path) - 2)

            euler_char = euler_char - num_saddles
            
        # adjustments for additional saddles
        
        if self.type == "II":
            sum_of_endpoints = 0
            for path in self.edgepaths:
                sum_of_endpoints = sum_of_endpoints + path[0].p()
            euler_char = euler_char -  abs(sum_of_endpoints)*sheets  # adjustment for vert. edges
            
        if self.type  == "III":
            euler_char = euler_char -  sheets*n  # adjustment for additional saddles going to infinity

        # now glue together

        if self.type == "III":
            euler_char = euler_char  - sheets*n  # adjustment for glueing

        if self.type == "I":
            
            # we need to know how we glue components which end in arcs
            # together as we go from tangle to tangle (we can ignore
            # those ending in circles because there is no change in
            # euler characteristic when you attach via a circle).  Cut
            # the arcs via a vertical line running through the middle
            # of the punctures.  The number we want is the number of
            # pieces on one side of the line.  If a pair of arcs have
            # slope p/q it interects this line in 2*q places.  The
            # number of components C on one side of the line is then
            # (num end pts)/2 = q + 1.  This number is independant of
            # the tangle because it depends only on the intersection
            # of the surface with the axis (the line around which the
            # tangles are arranged.

            v = self.edgepaths[0][0]

            if isinstance(v, interior_of_edge_of_D):
                k, m = v.km.t, v.km.b
                # is on horizontal edge <p/q, p/q>
                if v.pq == v.rs:
                    C = (sheets/k)*(k*(v.q() + 1) + (m - k)*v.q())
                else:
                    C = k*sheets/m*(v.q() + 1) + (m - k)*sheets/m*(v.s() + 1)                    
            else:
                C = (v.q() + 1)*sheets

            # each time we attach a tangle to the next one, we
            # decrease euler_char by -C.  The exception is the last
            # glueing where we're gluing together different parts of
            # the same thing.  In this case, the euler char increases by
            # C - 2*sheets

            euler_char = euler_char - C*(n - 1) + (C - 2*sheets)

        if self.type == "II":
            euler_char = euler_char - 2*sheets*(n-1)   # see above
             
        return euler_char
            
    # cyclically permutes the paths so that the current ith one is
    # made the _last_ one
    
    def cycle_paths_so_last(self, i):
        i = i % len(self.edgepaths)
        self.edgepaths = self.edgepaths[i+1:] + self.edgepaths[:i] + [ self.edgepaths[i]]

    def reflect(self):
        self.edgepaths.reverse()
        
    # gets ith path with cycle ordering
    def __getitem__(self, i):
        return self.edgepaths[i % len(self.edgepaths)]

    def __cmp__(self, o):
        if self.type == o.type and self.u == o.u and self.edgepaths == o.edgepaths:
            return 0
        return 1
        
# For each tangle we will need a tree in order to determine the Type I
# solutions.  Each node consists of a vertex with links to its two
# leftward neighbors and a link rightward vertex it came from.

class node:
    def __init__(self, vertex, back):  
        self.vertex = vertex
        self.leftward = [None, None]
        self.back = back              # back to the right

    def p(self):
        return self.vertex.p()

    def q(self):
        return self.vertex.q()

    def u(self):
        return self.vertex.u()

    def v(self):
        return self.vertex.v()

    def __repr__(self):
        s = "%s" % self.vertex
        if self.leftward[0]:
            s = s + " up: " +  repr(self.leftward[0].vertex)
        if self.leftward[1]:
            s = s +  " down " +  repr(self.leftward[1].vertex)
        if self.back:
            s = s + " back:  " +  repr(self.back.vertex)
        return s

class conway_sphere:
    slope = frac(1,0)
    carries_incompressible = 1
    num_sheets = "Conway Sphere"
    euler_char = -2
    from_non_iso_solution = 0
        
# Takes a list of lists [L_1, ... , L_n] and outputs a list consisting
# of all points in the cartesian product of the L1, i.e. every list
# whose ith element is in L_i

def product_of_lists( lists, done=[[]]):
    if len(lists) == 0: return done

    new_list = []
    for list in done:
        for item in lists[0]:
            new_list.append( list + [item] )

    return product_of_lists( lists[1:], new_list )
