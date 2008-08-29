package org.katlas.JavaKh;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;

import org.katlas.JavaKh.algebra.LinearMorphism;
import org.katlas.JavaKh.algebra.Ring;
import org.katlas.JavaKh.algebra.Rings;


public class LCCC<R extends Ring<R>> implements LinearMorphism<R, Obj, LCCC<R>>, Serializable { // Linear Combination of Canned Cobordisms
    /**
	 * 
	 */
	private static final long serialVersionUID = 8539035436108747574L;
	//static int maxsz = 0; // is 10 for T(7.6)
    Cap top, bottom;
    // coefficients are BaseRing
    // List<CannedCobordism> cobordisms;
    // List<BaseRing> coefficients;
    // consider storing the hash codes?

    final SortedMap<CannedCobordism, R> coefficients;
    
    public R firstCoefficient() {
    	return coefficients.get(coefficients.firstKey());
    }
    
    public LCCC(Cap t, Cap b) {
	//if (t == null || b == null)
	//throw new IllegalArgumentException();
	top = t;
	bottom = b;
	// n = 0;
	// cobordisms = new LinkedList<CannedCobordism>();
	// coefficients = new LinkedList<BaseRing>();
	coefficients = new TreeMap<CannedCobordism, R>();
    }

    int size() {
    	return coefficients.size();
    }
    
    @SuppressWarnings("unchecked")
	public boolean equals(Object o) {
	if (o == null && size() == 0)
	    return true;
	if (!(o instanceof LCCC))
	    return false;
	LCCC<R> other = (LCCC<R>) o;
	if (size() > 1 || other.size() > 1)
	    // not working right now
	    throw new UnsupportedOperationException();
	if (size() == 0) {
	    if (other.size() != 0 && !other.firstCoefficient().isZero())
		return false;
	} else if (other.size() == 0 && !firstCoefficient().isZero())
	    return false;
	else if (!(coefficients.firstKey().equals(other.coefficients.firstKey())
		   && coefficients.get(coefficients.firstKey()).equals(other.coefficients.get(coefficients.firstKey()))))
	    return false;
	return true;
    }

//    public static void main(String args[]) {
//	// associativity check
//	Cap caps[] = Cap.generate(10);
//	for (int i = 0; i < caps.length; i++)
//	    for (int j = 0; j < caps.length; j++)
//		for (int k = 0; k < caps.length; k++)
//		    for (int l = 0; l < caps.length; l++) {
//			CannedCobordism
//			    a1=CannedCobordism.generateRandom(caps[i],caps[j]),
//			    a2=CannedCobordism.generateRandom(caps[i],caps[j]),
//			    a3=CannedCobordism.generateRandom(caps[i],caps[j]),
//			    b1=CannedCobordism.generateRandom(caps[j],caps[k]),
//			    b2=CannedCobordism.generateRandom(caps[j],caps[k]),
//			    b3=CannedCobordism.generateRandom(caps[j],caps[k]),
//			    c1=CannedCobordism.generateRandom(caps[k],caps[l]),
//			    c2=CannedCobordism.generateRandom(caps[k],caps[l]),
//			    c3=CannedCobordism.generateRandom(caps[k],caps[l]);
//			LCCC a = new LCCC(caps[i], caps[j]);
//			a.add(a1, (int) (Math.random() * 10));
//			a.add(a2, (int) (Math.random() * 10));
//			a.add(a3, (int) (Math.random() * 10));
//			LCCC b = new LCCC(caps[j], caps[k]);
//			b.add(b1, (int) (Math.random() * 10));
//			b.add(b2, (int) (Math.random() * 10));
//			b.add(b3, (int) (Math.random() * 10));
//			LCCC c = new LCCC(caps[k], caps[l]);
//			c.add(c1, (int) (Math.random() * 10));
//			c.add(c2, (int) (Math.random() * 10));
//			c.add(c3, (int) (Math.random() * 10));
//			LCCC d = (c.compose(b)).compose(a);
//			LCCC e = c.compose(b.compose(a));
//			if (!d.equals(e)) {
//			    System.out.println("Error in associativity check");
//			    return;
//			}
//			LCCC f = c.reduce().compose(b.reduce().compose(a.reduce()));
//			if (!f.reduce().equals(e.reduce())) {
//			    System.out.println("Error in reduction check");
//			    return;
//			}
//		    }
//	System.out.println("Associativity checks OK!");
//    }

    @SuppressWarnings("unchecked")
	public void add(CannedCobordism cc, int n) {
    	add(cc, (R)Rings.createInstance(n));
    }

    public void add(CannedCobordism cc, R num) {
    	
    if(coefficients.containsKey(cc)) {
		R newCoefficient = coefficients.get(cc).add(num);
		if(newCoefficient.isZero()) {
			coefficients.remove(cc);
		} else {
			coefficients.put(cc, newCoefficient);
		}
		return;
    }
    	
	if (num.isZero()) {
	    return;
	}
	
	coefficients.put(cc, num);
	// DEBUG
	/*if (entries.size() > maxsz)
	  maxsz = entries.size();*/
    }

    public LCCC<R> add(LCCC<R> other) {
	if (other != null)
	for (CannedCobordism cc : other.coefficients.keySet())
	    add(cc, other.coefficients.get(cc));
	return this;
    }

    public LCCC<R> multiply(R num) {
	if (num.isZero()) {
	    coefficients.clear();
	} else {
		for(CannedCobordism cc : coefficients.keySet()) {
			coefficients.put(cc, coefficients.get(cc).multiply(num));
		}
	}
	return this;
	}

    public LCCC<R> compose(LCCC<R> other) { // vertical composition
	if (other == null || size() == 0 || other.size() == 0)
	    return null;
	assert top.equals(other.bottom);
	LCCC<R> ret = new LCCC<R>(other.top, bottom);
	
	for(CannedCobordism cc : coefficients.keySet()) {
		for(CannedCobordism occ : other.coefficients.keySet()) {
			ret.add(cc.compose(occ), coefficients.get(cc).multiply(other.coefficients.get(occ)));
		}
	}
	
	return ret;
    }

    // horizontal composition
    public LCCC<R> compose(int start, CannedCobordism cc, int cstart, int nc,
			boolean reverse) {
	if (size() == 0)
	    return null;
	LCCC<R> ret = new LCCC<R>(null, null);
	if (reverse) {
		for(CannedCobordism occ : coefficients.keySet()) {
			ret.add(cc.compose(cstart, occ, start, nc),
					coefficients.get(occ));
		}	
//	    for (int i = 0; i < size(); i++) {
//		ret.add(cc.compose(cstart, cobordisms.get(i), start, nc),
//			coefficients.get(i));
//	    }
	} else {
		for(CannedCobordism occ : coefficients.keySet()) {
			ret.add(occ.compose(start, cc, cstart, nc),
					coefficients.get(occ));
		}	

//		for (int i = 0; i < size(); i++) {
//		ret.add(cobordisms.get(i).compose(start, cc, cstart, nc),
//			coefficients.get(i));
//	    }
	}
	if (ret.size() == 0)
	    return null;
	else {
	    ret.top = ret.coefficients.firstKey().top;
	    ret.bottom = ret.coefficients.firstKey().bottom;
	    return ret;
	}
    }

    public LCCC<R> reduce() {
	if (JavaKh.using_h)
	    return reduceWithH();
	if (size() == 0)
	    return null;
	LCCC<R> ret = new LCCC<R>(top, bottom);
	for (CannedCobordism cc : coefficients.keySet()) {
	    R num = coefficients.get(cc);
	    cc.reverseMaps();
	    byte dots[] = new byte[cc.nbc];
	    byte genus[] = CannedCobordism.zeros[cc.nbc];
	    int moreWork[] = new int[cc.ncc];
	    int nmoreWork = 0;
	    boolean kill = false;
	    for (int i = 0; i < cc.ncc; i++)
		if (cc.genus[i] + cc.dots[i] > 1) {
		    kill = true;
		} else if (cc.boundaryComponents[i].length == 0) {
		    if (cc.genus[i] == 1)
			num = num.multiply(2);
		    else if (cc.dots[i] == 0)
			kill = true;
		} else if (cc.boundaryComponents[i].length == 1) {
		    dots[cc.boundaryComponents[i][0]] = (byte)(cc.dots[i] + cc.genus[i]);
		    if (cc.genus[i] == 1)
			num = num.multiply(2);
		} else {
		    // neck cutting relation
		    if (cc.genus[i] + cc.dots[i] == 1) { // only one choice
			if (cc.genus[i] == 1) {
			    // there is a 2 from the torus
			    num = num.multiply(2);
			}
			// use dots to cancel out the other factors
			for (int j =0; j<cc.boundaryComponents[i].length;j++)
			    dots[cc.boundaryComponents[i][j]] = 1;
		    } else {
			// cc.bC[i].length choices
			// use dots to cancel out all the factors, except
			// one which is cancelled by the torus
			// do these last
			moreWork[nmoreWork++] = i;
		    }
		}
	    if (kill)
		continue;
	    byte neckCutting[][] = new byte[1][];
	    neckCutting[0] = dots;
	    for (int i = 0; i < nmoreWork; i++) {
		int concomp = moreWork[i];
		int nbc = cc.boundaryComponents[concomp].length;
		byte newarr[][] = new byte[neckCutting.length * nbc][cc.nbc];
		for (int j = 0; j < neckCutting.length; j++) {
		    System.arraycopy(neckCutting[j], 0, newarr[j * nbc], 0,
				     cc.nbc);
		    for (int k = 0; k < nbc; k++)
			newarr[j * nbc][cc.boundaryComponents[concomp][k]] = 1;
		    for (int k = 1; k < nbc; k++)
			System.arraycopy(newarr[j * nbc], 0,
					 newarr[j * nbc + k], 0, cc.nbc);
		    for (int k = 0; k < nbc; k++)
			newarr[j * nbc + k][cc.boundaryComponents[concomp][k]] = 0;
		}
		neckCutting = newarr;
	    }
	    byte connectedComponent[] = CannedCobordism.counting[cc.nbc];
	    for (int i = 0; i < neckCutting.length; i++) {
		CannedCobordism newcc = new CannedCobordism(top, bottom);
		// IMPORTANT!!! in order for them to safely share arrays
		// CannedCobordisms must be treated as immutable
		newcc.connectedComponent = connectedComponent;
		newcc.ncc = newcc.nbc;
		newcc.genus = genus;
		newcc.dots = neckCutting[i];
		ret.add(newcc, num);
	    }
	}
	if (ret.size() == 0)
	    return null;
	else
	    return ret;
    }

    @SuppressWarnings("unchecked")
	public LCCC<R> reduceWithH() {
	if (size() == 0)
	    return null;
	LCCC<R> ret = new LCCC<R>(top, bottom);
	for (CannedCobordism cc : coefficients.keySet()) {
	    R num = coefficients.get(cc);
	    cc.reverseMaps();
	    byte dots[] = new byte[cc.nbc];
	    int hpow = cc.hpower;
	    int moreWork[] = new int[cc.ncc];
	    int nmoreWork = 0;
	    boolean kill = false;
	    for (int i = 0; i < cc.ncc; i++)
		if (cc.boundaryComponents[i].length == 0) {
		    if (cc.dots[i] > 0)
			hpow += cc.dots[i] + cc.genus[i] - 1;
		    else if (cc.genus[i] % 2 == 0)
			kill = true;
		    else {
			num = num.multiply(2);
			hpow += cc.genus[i] - 1;
		    }
		} else if (cc.boundaryComponents[i].length == 1) {
		    if (cc.dots[i] > 0) {
			hpow += cc.dots[i] + cc.genus[i] - 1;
			dots[cc.boundaryComponents[i][0]] = 1;
		    } else if (cc.genus[i] % 2 == 0)
			hpow += cc.genus[i];
		    else
			moreWork[nmoreWork++] = i;
		} else {
		    if (cc.dots[i] > 0) {
			// the dot and the -h terms cancel since dot == h
			for (int j = 0; j<cc.boundaryComponents[i].length; j++)
			    dots[cc.boundaryComponents[i][j]] = 1;
			hpow += cc.dots[i] + cc.genus[i] - 1;
		    } else
			moreWork[nmoreWork++] = i;
		}
	    if (kill)
		continue;
	    byte nCdots[][] = new byte[1][];
	    int nChpow[] = new int[1];
	    List<R> nCnum = new ArrayList<R>(1);
	    nCdots[0] = dots;
	    nChpow[0] = hpow;
	    nCnum.set(0, num);
	    for (int i = 0; i < nmoreWork; i++) {
		int concomp = moreWork[i];
		int nbc = cc.boundaryComponents[concomp].length;
		assert cc.dots[concomp] == 0;
		byte newdots[][] = new byte[nCdots.length << nbc][cc.nbc];
		int newhpow[] = new int[nChpow.length << nbc];
		List<R> newnum = new ArrayList<R>(nCnum.size() << nbc);
		for (int j = 0; j < nCdots.length; j++) {
		    for (int k = 0; k < (1 << nbc); k++) {
			int idx = (j << nbc) + k;
			System.arraycopy(nCdots[j], 0,newdots[idx], 0, cc.nbc);
			newhpow[idx] = nChpow[j];
			newnum.set(idx, nCnum.get(j));
			int nzeros = 0;
			for (int l = 0; l < nbc; l++)
			    if ((k & (1 << l)) == 0) {
				newdots[idx][cc.boundaryComponents[concomp][l]] = 0;
				nzeros++;
			    } else
				newdots[idx][cc.boundaryComponents[concomp][l]] = 1;
			R nmul = (R)Rings.createInstance(0);
			int hmod = 0;
			boolean hset = false;
			for (int l = 0; l < (1 << nzeros); l++) {
			    int ndots = 0;
			    for (int m = 0; m < nzeros; m++)
				if ((l & (1 << m)) == 0)
				    ndots++;
			    if (ndots > 0) {
				if (hset) {
				    if (hmod != nzeros + cc.genus[concomp] - 1)
					throw new AssertionError();
				} else {
				    hmod = nzeros + cc.genus[concomp] - 1;
				    hset = true;
				}
				int n = 1;
				for (int o = 0; o < nzeros - ndots; o++)
				    n = -n;
				nmul = nmul.add((R)Rings.createInstance(n));
			    } else if (cc.genus[concomp] % 2 == 0)
				continue; // coefficient of zero
			    else {
				if (hset) {
				    if (hmod != nzeros + cc.genus[concomp] - 1)
					throw new AssertionError();
				} else {
				    hmod = nzeros + cc.genus[concomp] - 1;
				    hset = true;
				}
				int n = 2;
				for (int o = 0; o < nzeros; o++)
				    n = -n;
				nmul = nmul.add((R)Rings.createInstance(n));
			    }
			}
			newhpow[idx] += hmod;
			newnum.set(idx, newnum.get(idx).multiply(nmul));
		    }
		}
		nCdots = newdots;
		nChpow = newhpow;
		nCnum = newnum;
	    }
	    for (int i = 0; i < nCdots.length; i++) {
		CannedCobordism newcc = new CannedCobordism(top, bottom);
		newcc.connectedComponent = CannedCobordism.counting[newcc.nbc];
		newcc.ncc = newcc.nbc;
		newcc.genus = CannedCobordism.zeros[cc.nbc];
		newcc.dots = nCdots[i];
		newcc.hpower = nChpow[i];
		ret.add(newcc, nCnum.get(i));
	    }
	}
	if (ret.size() == 0)
	    return null;
	else
	    return ret;
    }

    public LCCC<R> finalizeH() {
	if (size() == 0)
	    return null;
	assert top.n == 2 && top.ncycles == 0
	    && bottom.n == 2 && bottom.ncycles == 0;
	LCCC<R> ret = new LCCC<R>(top, bottom);
	CannedCobordism cc = CannedCobordism.isomorphism(top);
	boolean hset = false;
	for (CannedCobordism occ : coefficients.keySet()) {
	    if (!coefficients.get(occ).isZero()) {
		if (!hset)
		    cc.hpower = occ.hpower + occ.dots[0]
			+ occ.genus[0];
		else if (cc.hpower != occ.hpower
			 + occ.dots[0] + occ.genus[0])
		    throw new AssertionError();
		ret.add(cc, coefficients.get(occ));
	    }
	}
	if (ret.size() == 0)
	    return null;
	else
	    return ret;
    }
}
