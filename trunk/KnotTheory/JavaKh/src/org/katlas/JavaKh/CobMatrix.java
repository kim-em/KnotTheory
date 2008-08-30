package org.katlas.JavaKh;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.katlas.JavaKh.algebra.Ring;
import org.katlas.JavaKh.rows.LinkedListRow;
import org.katlas.JavaKh.rows.MatrixRow;
import org.katlas.JavaKh.rows.RedBlackIntegerMap;


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
    final ArrayList<MatrixRow<LCCC<R>>> entries;
    
    public CobMatrix(SmoothingColumn s, SmoothingColumn t) {
		source = new SmoothingColumn(s);
		target = new SmoothingColumn(t);
		entries = new ArrayList<MatrixRow<LCCC<R>>>(t.n);
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
		entries = new ArrayList<MatrixRow<LCCC<R>>>(t.n);
		for (int i = 0; i < t.n; ++i) {
			entries.add(newRow());
		}
	}
    
    void unshare() {
    	source = new SmoothingColumn(source);
    	target = new SmoothingColumn(target);
    }
    
    private MatrixRow<LCCC<R>> newRow() {
//    	return new TroveEntryMap<LCCC<R>>();
//    	return new TreeEntryMap<LCCC<R>>();
//    	return new RedBlackIntegerMap<LCCC<R>>();
    	return new LinkedListRow<LCCC<R>>();
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
		MatrixRow<LCCC<R>> rowEntries = entries.get(i);
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
			MatrixRow<LCCC<R>> rowEntriesI = entries.get(i);
			MatrixRow<LCCC<R>> result = newRow();
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
    	for(MatrixRow<LCCC<R>> rowEntries : entries) {
    		for(int i : rowEntries.keys()) {
    			LCCC<R> lc = rowEntries.get(i);
    			if(lc != null) {
    				lc.multiply(n);
    			}
    		}
    	}
   	}

    public void add(CobMatrix<R> that) { // edits in place
		assert source.equals(that.source) && target.equals(that.target);

		for (int i = 0; i < entries.size(); ++i) {
			MatrixRow<LCCC<R>> thisRowEntriesI = entries.get(i);
			MatrixRow<LCCC<R>> thatRowEntriesI = that.entries.get(i);

			Iterator<Integer> thisIterator = thisRowEntriesI.keys().iterator();
			Iterator<Integer> thatIterator = thatRowEntriesI.keys().iterator();

			if(!thatIterator.hasNext()) continue;
			else {
				if(!thisIterator.hasNext()) {
					while(thatIterator.hasNext()) {
						int j = thatIterator.next();
						thisRowEntriesI.put(j, thatRowEntriesI.get(j));
					}
				} else {
					// both rows are non-empty!
					int thisKey = thisIterator.next(), thatKey = thatIterator.next();
					while(true) {
						if(thisKey < thatKey) {
							if(thisIterator.hasNext()) {
								thisKey = thisIterator.next();
								continue;
							} else {
								thisRowEntriesI.put(thatKey, thatRowEntriesI.get(thatKey));								
								break;
							}
						} else if(thisKey == thatKey) {
							thisRowEntriesI.get(thisKey).add(thatRowEntriesI.get(thatKey));	
							if(!thisIterator.hasNext() || !thatIterator.hasNext()) {
								break;
							} else {
								thisKey = thisIterator.next();
								thatKey = thatIterator.next();
								continue;
							}
						} else {
							// thisKey > thatKey
							thisRowEntriesI.put(thatKey, thatRowEntriesI.get(thatKey));								
							if(!thatIterator.hasNext()) {
								break;
							} else {
								thatKey = thatIterator.next();
								continue;
							}
						}
					}

					while (thatIterator.hasNext()) {
						int j = thatIterator.next();
						thisRowEntriesI.put(j, thatRowEntriesI.get(j));
					}
					
				}
			}

//			for (int j : thatRowEntriesI.keys()) {
//				if (thisRowEntriesI.containsKey(j)) {
//					thisRowEntriesI.get(j).add(thatRowEntriesI.get(j));
//				} else {
//					thisRowEntriesI.put(j, thatRowEntriesI.get(j));
//				}
//			}
		}
	
    }

    public void reduce() { // modifies this CobMatrix in place
    	for(MatrixRow<LCCC<R>> rowEntries : entries) {
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
    	for(MatrixRow<LCCC<R>> rowEntries : entries) {
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
			MatrixRow<LCCC<R>> row = entries.get(i);
			for (int j : row.keys()) {
				assert row.get(j) != null;
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
        
    


}