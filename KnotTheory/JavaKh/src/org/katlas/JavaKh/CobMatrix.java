package org.katlas.JavaKh;
import gnu.trove.TIntObjectHashMap;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import net.tqft.iterables.AbstractIterator;

import org.katlas.JavaKh.algebra.Ring;
import org.katlas.JavaKh.utils.RedBlackIntegerTree;


// sparse matrix
// based on http://www.ii.uib.no/~geirg/jaggedarrays.html
public class CobMatrix<R extends Ring<R>> implements Serializable{
    /**
	 * 
	 */
	private static final long serialVersionUID = 6928267083411895640L;
	SmoothingColumn source, target;
//    LCCC values[][];
//    int indices[][];
//    int rowsizes[];

//    final List<Map<Integer, LCCC>> entries;
    final ArrayList<CobMatrixRow<R>> entries;
    
    public CobMatrix(SmoothingColumn s, SmoothingColumn t) {
		source = new SmoothingColumn(s);
		target = new SmoothingColumn(t);
		entries = new ArrayList<CobMatrixRow<R>>(t.n);
		for (int i = 0; i < t.n; ++i) {
			entries.add(newRow());
		}
	}
    
    public CobMatrix(SmoothingColumn s, SmoothingColumn t, boolean shared) {
    	if(shared) {
    		source = s;
    		target = t;
    	} else {
    		source = new SmoothingColumn(s);
    		target = new SmoothingColumn(t);
    	}
		entries = new ArrayList<CobMatrixRow<R>>(t.n);
		for (int i = 0; i < t.n; ++i) {
			entries.add(newRow());
		}
	}
    
    void unshare() {
    	source = new SmoothingColumn(source);
    	target = new SmoothingColumn(target);
    }
    
    private CobMatrixRow<R> newRow() {
//    	return new TroveEntryMap();
//    	return new TreeEntryMap();
    	return new RedBlackEntryMap();
    }

    // assumes matrix[i][j] is not contained in this sparse matrix
	public void append(int i, int j, LCCC<R> lc) {
		assert check();
		if (lc == null || lc.size() == 0) {
			return;
		}
		entries.get(i).put(j, lc);
		assert check();
	}

    @SuppressWarnings("unchecked")
	public LCCC<R>[] unpackRow(int i) {
		LCCC<R> rowi[] = new LCCC[source.n];
		CobMatrixRow<R> rowEntries = entries.get(i);
		for (int j : rowEntries.keys()) {
			rowi[j] = rowEntries.get(j);
		}
		return rowi;
	}

    // not working right now
    @SuppressWarnings("unchecked")
	public boolean equals(Object o) {
	if (!(o instanceof CobMatrix))
	    return false;
	CobMatrix<R> cm = (CobMatrix<R>) o;
	if (!source.equals(cm.source))
	    return false;
	if (!target.equals(cm.target))
	    return false;
	// Arrays.equals() isn't adequate because null == 0 for LCCCs
	for (int i = 0; i < target.n; i++) {
	    LCCC<R> arowi[] = unpackRow(i);
	    LCCC<R> browi[] = cm.unpackRow(i);
	    for (int j = 0; j < source.n; j++)
		if (arowi[j] != null) {
		    if (!arowi[j].equals(browi[j]))
			return false;
		} else if (browi[j] != null && browi[j].size() != 0)
		    return false;
	}
	return true;
    }

    public CobMatrix<R> multiply(CobMatrix<R> cm) { // this * cm
	assert source.equals(cm.target);
	/*if (!source.equals(cm.target))
	  throw new IllegalArgumentException();*/
	CobMatrix<R> ret = new CobMatrix<R>(cm.source, target);

	for (int i = 0; i < target.n; ++i) {
			CobMatrixRow<R> rowEntriesI = entries.get(i);
			CobMatrixRow<R> result = newRow();
			for (int j : rowEntriesI.keys()) {
				for (int k : cm.entries.get(j).keys()) {
					LCCC<R> lc = rowEntriesI.get(j).compose(
							cm.entries.get(j).get(k));
					if (lc != null) {
						if (result.containsKey(k)) {
							result.get(k).add(lc);
						} else {
							result.put(k, lc);
						}
					}
				}
			}
			ret.entries.set(i, result);
		}
	
	return ret;
    }

    public void multiply(R n) { // modifies in place
    	for(CobMatrixRow<R> rowEntries : entries) {
    		for(int i : rowEntries.keys()) {
    			LCCC<R> lc = rowEntries.get(i);
    			if(lc != null) {
    				lc.multiply(n);
    			}
    		}
    	}
   	}

    public void add(CobMatrix<R> cm) { // edits in place
	assert source.equals(cm.source) && target.equals(cm.target);
	
	for(int i = 0; i < entries.size(); ++i) {
		CobMatrixRow<R> rowEntries = entries.get(i);
		CobMatrixRow<R> cmRowEntries = cm.entries.get(i);
		for(int j : cmRowEntries.keys()) {
			if(rowEntries.containsKey(j)) {
				rowEntries.get(j).add(cmRowEntries.get(j));
			} else {
				rowEntries.put(j, cmRowEntries.get(j));
			}
		}
	}
	
    }

    public void reduce() { // modifies this CobMatrix in place
    	for(CobMatrixRow<R> rowEntries : entries) {
    		Set<Integer> entriesToRemove = new HashSet<Integer>();
    		for(int i : rowEntries.keys()) {
    			LCCC<R> rlc = rowEntries.get(i).reduce();
    			if(rlc == null) {
    				entriesToRemove.add(i);
    			} else {
    				rowEntries.put(i, rlc);
    			}
    		}
    		for(int i : entriesToRemove) {
    			rowEntries.remove(i);
    		}
    	}
   	}

    public boolean isZero() {
    	for(CobMatrixRow<R> rowEntries : entries) {
    		for(int i : rowEntries.keys()) {
    			LCCC<R> lc = rowEntries.get(i);
    			if(lc != null && lc.size() > 0) {
    				return false;
    			}
    		}
    	}
		return true;
    }

    public boolean check() {
    	if(entries.size() != target.n) {
    		assert false;
    		return false;
    	}
		for (int i = 0; i < entries.size(); ++i) {
			CobMatrixRow<R> row = entries.get(i);
			for (int j : row.keys()) {
				if (!row.get(j).top.equals(source.smoothings.get(j))) {
					assert false;
					return false;
				}
				if (!row.get(j).bottom.equals(target.smoothings.get(i))) {
					assert false;
					return false;
				}
			}
		}
		return true;
	}

    public void print() {
	System.out.print("[");
	for (int i = 0; i < target.n; i++) {
	    LCCC<R> rowi[] = unpackRow(i);
	    for (int j = 0; j < source.n; j++) {
		String n;
		if (rowi[j] == null || rowi[j].size() == 0)
		    n = "0";
		else {
		    assert rowi[j].size() == 1;
		    n = rowi[j].firstCoefficient().toString();
		}
		System.out.print(n);
		if (j != source.n - 1)
		    System.out.print(",");
	    }
	    System.out.print("; ");
	}
	System.out.println("]");
    }

    public void printZeros() {
	//throw new UnsupportedOperationException();
	for (int i = 0; i < target.n; i++) {
	    LCCC<R> rowi[] = unpackRow(i);
	    for (int j = 0; j < source.n; j++)
		if (rowi[j] == null)
		    System.out.print("0");
		else {
		    int n = rowi[j].size();
		    if (n == 1) {
			CannedCobordism cc = rowi[j].coefficients.firstKey();
			if (cc.isIsomorphism())
			    System.out.print("i");
			else
			    System.out.print("1");
		    } else
			System.out.print(n);
		}
	    System.out.println();
	}
    }
        
    
    
    @SuppressWarnings("unused")
	private class TreeEntryMap implements CobMatrixRow<R>, Serializable {
    	/**
		 * 
		 */
		private static final long serialVersionUID = 7040588247299310395L;
		
		Map<Integer, LCCC<R>> map = new TreeMap<Integer, LCCC<R>>();
    	
		public void compact() {	}

		public boolean containsKey(int k) {
			return map.containsKey(k);
		}

		public LCCC<R> get(int j) {
			return map.get(j);
		}

		public Iterable<Integer> keys() {
			return map.keySet();
		}

		public void put(int j, LCCC<R> lc) {
			map.put(j, lc);
		}

		public void remove(int i) {
			map.remove(i);
		}

		public void decrementIndexesAbove(int key) {
			List<Integer> targetIndexes = new ArrayList<Integer>();
			for(int k : map.keySet()) {
				if(k > key) {
					targetIndexes.add(k);
				}
			}
			for(int k : targetIndexes) {
				map.put(k - 1, map.get(k));
				map.remove(k);
			}
		}
    	
    	
    }
  
    @SuppressWarnings("unused")
	private class RedBlackEntryMap extends RedBlackIntegerTree<LCCC<R>> implements CobMatrixRow<R> {

		/**
		 * 
		 */
		private static final long serialVersionUID = 2840043469285162678L;

		public void compact() {
			
		}

    };
    
    @SuppressWarnings("unused")
	private class TroveEntryMap implements CobMatrixRow<R>, Serializable {

    	/**
		 * 
		 */
		private static final long serialVersionUID = -230926858992553476L;
		
		TIntObjectHashMap<LCCC<R>> map = new TIntObjectHashMap<LCCC<R>>(2);
    	
		public void compact() {
			map.compact();
		}

		public boolean containsKey(int k) {
			return map.contains(k);
		}

		public LCCC<R> get(int j) {
			return map.get(j);
		}

		public Iterable<Integer> keys() {
			return new Iterable<Integer>() {
				final int[] keys = map.keys();

				public Iterator<Integer> iterator() {
					return new AbstractIterator<Integer>() {
						int i = 0;
						public boolean hasNext() {
							return i < keys.length;
						}
						public Integer returnNext() {
							return keys[i++];
						}
					};
				}
			};
		}

		public void put(int j, LCCC<R> lc) {
			map.put(j, lc);
		}

		public void remove(int i) {
			map.remove(i);
		}

		public void decrementIndexesAbove(int key) {
			List<Integer> targetIndexes = new ArrayList<Integer>();
			for(int k : map.keys()) {
				if(k > key) {
					targetIndexes.add(k);
				}
			}
			Collections.sort(targetIndexes);
			for(int k : targetIndexes) {
				map.put(k - 1, map.get(k));
				map.remove(k);
			}

		}
    	
    }

}
