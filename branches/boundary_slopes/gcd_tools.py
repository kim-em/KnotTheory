# computes the gcd.  taken from snappea

def gcd(a, b):

    a = abs(a)
    b = abs(b)
    
    if a == 0:
	if b == 0: raise ValueError, "gcd(0,0) undefined."
	else: return b

    while 1:
	b = b % a
	if (b == 0): return a
	a = a % b
	if (a == 0): return b

# returns (gcd, a, b) where ap + bq = gcd.  taken from snappea

def euclidean_algorithm(m, n):

    # Given two long integers m and n, use the Euclidean algorithm to
    # find integers a and b such that a*m + b*n = g.c.d.(m,n).
    #
    # Recall the Euclidean algorithm is to keep subtracting the
    # smaller of {m, n} from the larger until one of them reaches 
    # zero.  At that point the other will equal the g.c.d.
    #
    #	As the algorithm progresses, we'll use the coefficients
    #	mm, mn, nm, and nn to express the current values of m and n
    #   in terms of the original values:
    #
    #       current m = mm*(original m) + mn*(original n)
    #       current n = nm*(original m) + nn*(original n)

    # Begin with a quick error check.

    if m == 0 and n == 0 : raise ValueError, "gcd(0,0) undefined."

    #	Initially we have
    #
    #		current m = 1 (original m) + 0 (original n)
    #		current n = 0 (original m) + 1 (original n)

    mm = nn = 1
    mn = nm = 0
    
    #     It will be convenient to work with nonnegative m and n.

    if m < 0:
        m = - m
        mm = -1

    if n < 0:
        n = - n
        nn = -1
        

    while 1:
        #	If m is zero, then n is the g.c.d. and we're done.
        
        if m == 0:
            return(n, nm, nn)

        #	Let n = n % m, and adjust the coefficients nm and nn accordingly.
        
        quotient = n / m
        nm = nm - quotient * mm
        nn = nn - quotient * mn
        n  = n - quotient * m

        #	If n is zero, then m is the g.c.d. and we're done.

        if n == 0:
            return(m, mm, mn)
		
        # Let m = m % n, and adjust the coefficients mm and mn accordingly.
    
        quotient = m / n
        mm = mm - quotient * nm
        mn = mn - quotient * nn
        m  = m - quotient * n
	
        # We never reach this point.



# computes the lcm of the given list of numbers:

def lcm( a ):
    cur_lcm = a[0]
    for n in a[1 : ]:
        cur_lcm = n * cur_lcm / gcd(n, cur_lcm)
    return abs(cur_lcm)

# computes the continued fraction expansion of the given
# number p/q which must satisfy 0 < a/b < 1, a, b > 0

def positive_continued_fraction_expansion(a, b):
    if not (0 < a < b): raise ValueError, "must have 0 < a < b"
    expansion = []
    while a > 0:
	# b = q a + r

	q = b/a
	r = b %  a 
	expansion.append(q)
	b, a = a, r
    return expansion

# A fraction class

from types import *

class frac:
    # the fraction is stored as self.t/self.b where
    # t and b are coprime and b > 0
    #
    # standard arithmatic and compareson ops implemented.
    # can add, etc. fracs and integers

    def __init__(self, p, q):
        if (type(p) is not IntType) or (type(q) is not IntType): raise TypeError
        g = gcd(p, q)
        if q == 0:
            self.t, self.b = 1, 0
        if q < 0:
            self.t, self.b = -p/g, -q/g
        else:
            self.t, self.b = p/g, q/g

    def copy(self):
        return frac(self.t, self.b)
            
    def __repr__(self):
        if self.b == 1: return "%i" % self.t
        return "%i/%i" % (self.t, self.b)

    def __abs__(self):
        return frac(abs(self.t), abs(self.b) )

    def __add__(self, o):
        if type(o) is IntType: o = frac(o, 1)
        if not isinstance(o, frac): raise TypeError
        return frac(self.t*o.b + self.b*o.t, self.b*o.b)

    def __radd__(self, o):
        return self.__add__(o)
    
    def __sub__(self, o):
        if type(o) is IntType: o = frac(o, 1)
        if not isinstance(o, frac): raise TypeError
        return frac(self.t*o.b -  self.b*o.t, self.b*o.b)

    def __rsub__(self, o):
        if type(o) is IntType: o = frac(o, 1)
        if not isinstance(o, frac): raise TypeError
        return frac(o.t*self.b -  o.b*self.t, self.b*o.b)

    def __mul__(self, o):
        if type(o) is IntType: o = frac(o, 1)
        if not isinstance(o, frac): raise TypeError
        return frac(self.t*o.t , self.b*o.b)

    def __rmul__(self, o):
        return self.__mul__(o)
    
    def __div__(self, o):
        if type(o) is IntType: o = frac(o, 1)
        if not isinstance(o, frac): raise TypeError
        if not o.__nonzero__(): raise ZeroDivisionError
        try:
            p, q =  self.t*o.b ,  self.b*o.t
            return frac( p, q )
        except OverflowError:
            gt = gcd(self.t, o.t)
            gb = gcd(self.b, o.b)        
            return frac( (self.t/gt)*(o.b/gb) , (self.b/gb)*(o.t/gt))

    def __neg__(self):
        return frac(-self.t, self.b)
    
    def __nonzero__(self):
        if self.t == 0: return 0
        return 1

    def __cmp__(self, o):
        if type(o) is IntType: o = frac(o, 1)
        if not isinstance(o, frac): raise TypeError
        return cmp(self.t*o.b, self.b*o.t)

import re

def string_to_frac(s):
    data = re.match("([+-]{0,1}\d+)/([+-]{0,1}\d+)$", s)
    if not data:
        data = re.match("[+-]{0,1}\d+$", s)
        if data:
            return frac(int(s), 1)
        else:
            raise ValueError, "need something of form a/b, a and b integers"
    p, q = map(int, data.groups())
    return frac(p, q)
