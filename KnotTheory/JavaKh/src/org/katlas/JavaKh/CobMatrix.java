package org.katlas.JavaKh;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.katlas.JavaKh.algebra.AbstractMatrix;
import org.katlas.JavaKh.algebra.DirectSum;
import org.katlas.JavaKh.algebra.Matrix;
import org.katlas.JavaKh.algebra.MatrixEntry;
import org.katlas.JavaKh.algebra.Ring;
import org.katlas.JavaKh.rows.ArrayRow;
import org.katlas.JavaKh.rows.DoublyLinkedListRow;
import org.katlas.JavaKh.rows.LinkedListRow;
import org.katlas.JavaKh.rows.MaskedArrayRow;
import org.katlas.JavaKh.rows.MatrixRow;
import org.katlas.JavaKh.rows.RedBlackIntegerMap;


public class CobMatrix<R extends Ring<R>> extends AbstractMatrix<R, Cap, LCCC<R>> implements Matrix<R, Cap, LCCC<R>>, Serializable{
    /**
	 * 
	 */
	private static final long serialVersionUID = 6928267083411895640L;
	
	private static final Log log = LogFactory.getLog(CobMatrix.class);
	
	transient SmoothingColumn source, target;
	transient ArrayList<MatrixRow<LCCC<R>>> entries;
    
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
//    	return new PackedArrayRow<LCCC<R>>();
//    	return new ArrayRow<LCCC<R>>(source.n);
    	return new MaskedArrayRow<LCCC<R>>(source.n);
//    	return new TreeEntryMap<LCCC<R>>();
//    	return new RedBlackIntegerMap<LCCC<R>>();
//    	return new LinkedListRow<LCCC<R>>();
//    	return new DoublyLinkedListRow<LCCC<R>>();
    }

    // assumes matrix[i][j] is not contained in this sparse matrix
	public void putEntry(int i, int j, LCCC<R> lc) {
		assert check();
		if (lc == null || lc.numberOfTerms() == 0) {
			return;
		}
		entries.get(i).put(j, lc);
		assert check();
	}

	public void addEntry(int row, int column, LCCC<R> t) {
		if (t == null || t.numberOfTerms() == 0) {
			return;
		}
		LCCC<R> entry = entries.get(row).get(column);
		if(entry == null) {
			entries.get(row).put(column, t);			
		} else {
			entries.get(row).put(column, entry.add(t));						
		}
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
		} else if (browi[j] != null && browi[j].numberOfTerms() != 0)
		    return false;
	}
	return true;
    }

    
    public CobMatrix<R> compose(Matrix<R, Cap, LCCC<R>> matrix) { // this * cm

		CobMatrix<R> cm;

		if (matrix instanceof CobMatrix) {
			cm = ((CobMatrix<R>) matrix);
		} else {
			throw new UnsupportedOperationException();
		}

		assert check();
		assert cm.check();

		assert source.equals(cm.target);
		/*
		 * if (!source.equals(cm.target)) throw new IllegalArgumentException();
		 */
		CobMatrix<R> ret = new CobMatrix<R>(cm.source, target);

		for (int i = 0; i < target.n; ++i) {
			MatrixRow<LCCC<R>> rowEntriesI = entries.get(i);
			MatrixRow<LCCC<R>> result = ret.newRow();
			for (int j : rowEntriesI.keys()) {
				for (int k : cm.entries.get(j).keys()) {
					LCCC<R> lc = rowEntriesI.get(j).compose(
							cm.entries.get(j).get(k));
					if (lc != null) {
						if (result.containsKey(k)) {
							result.get(k).add(lc);
						} else {
							result.put(k, lc); // stopping to think, sadly we
												// can't use putLast here.
						}
					}
				}
			}
			ret.entries.set(i, result);
		}

		return ret;
	}


    public CobMatrix<R> multiply(R n) { // modifies in place
    	for(MatrixRow<LCCC<R>> rowEntries : entries) {
    		for(int i : rowEntries.keys()) {
    			LCCC<R> lc = rowEntries.get(i);
    			if(lc != null) {
    				lc.multiply(n);
    			}
    		}
    	}
    	return this;
   	}
    
    public CobMatrix<R> add(Matrix<R, Cap, LCCC<R>> m) { // edits in place
    	
    	CobMatrix<R> that;
    	
    	if(m instanceof CobMatrix) {
			that =((CobMatrix<R>) m);
    	} else {
			throw new UnsupportedOperationException();
		}
    	
    	assert check();
    	assert that.check();
    	
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
						thisRowEntriesI.putLast(j, thatRowEntriesI.get(j));
					}
				} else {
					// both rows are non-empty!
					int thisKey = thisIterator.next(), thatKey = thatIterator.next();
					LCCC<R> thisRowEntry = thisRowEntriesI.get(thisKey);
					while(true) {
						if(thisKey < thatKey) {
							if(thisIterator.hasNext()) {
								thisKey = thisIterator.next();
								thisRowEntry = thisRowEntriesI.get(thisKey);
								continue;
							} else {
								thisRowEntriesI.putLast(thatKey, thatRowEntriesI.get(thatKey));								
								break;
							}
						} else if(thisKey == thatKey) {
							thisRowEntry.add(thatRowEntriesI.get(thatKey));	
							if(!thisIterator.hasNext() || !thatIterator.hasNext()) {
								break;
							} else {
								thisKey = thisIterator.next();
								thisRowEntry = thisRowEntriesI.get(thisKey);
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
						thisRowEntriesI.putLast(j, thatRowEntriesI.get(j));
					}
					
				}
			}

//			thisRowEntriesI.compact();
			
//			for (int j : thatRowEntriesI.keys()) {
//				if (thisRowEntriesI.containsKey(j)) {
//					thisRowEntriesI.get(j).add(thatRowEntriesI.get(j));
//				} else {
//					thisRowEntriesI.put(j, thatRowEntriesI.get(j));
//				}
//			}
		}
	
		return this;
    }

    public CobMatrix<R> reduce() { // modifies this CobMatrix in place
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
    	return this;
   	}

    public boolean isZero() {
    	for(MatrixRow<LCCC<R>> rowEntries : entries) {
    		for(int i : rowEntries.keys()) {
    			LCCC<R> lc = rowEntries.get(i);
    			if(lc != null && lc.numberOfTerms() > 0) {
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
				if (!row.get(j).source().equals(source.smoothings.get(j))) {
					assert false;
					return false;
				}
				if (!row.get(j).target().equals(target.smoothings.get(i))) {
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
		if (rowi[j] == null || rowi[j].numberOfTerms() == 0)
		    n = "0";
		else {
		    assert rowi[j].numberOfTerms() == 1;
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
		    int n = rowi[j].numberOfTerms();
		    if (n == 1) {
			CannedCobordism cc = rowi[j].firstTerm();
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
        
	private void writeObject(ObjectOutputStream s) throws IOException {
		s.defaultWriteObject();
		s.writeInt(1); // serialization version
		s.writeInt(target.n); // number of rows
		s.writeInt(source.n); // number of columns
		s.writeObject(source);
		s.writeObject(target);
		for(int i = 0; i < entries.size(); ++i) {
			for(int j : entries.get(i).keys()) {
				s.writeInt(i);
				s.writeInt(j);
				s.writeObject(entries.get(i).get(j));
			}
		}
		s.writeInt(-1);
		s.writeInt(-1);
		s.writeObject(null);
	}

	@SuppressWarnings("unchecked")
	private void readObject(ObjectInputStream s) throws IOException,
			ClassNotFoundException {
		s.defaultReadObject();
    	int serializationVersion = s.readInt();
    	if(serializationVersion < 1 || serializationVersion > 1) {
    		log.warn("Serialization version looks wrong...");
    	}
    	int rows = s.readInt();
    	@SuppressWarnings("unused")
		int columns = s.readInt();
    	source = (SmoothingColumn)s.readObject();
    	target = (SmoothingColumn)s.readObject();
    	entries = new ArrayList<MatrixRow<LCCC<R>>>(rows);
		for (int i = 0; i < rows; ++i) {
			entries.add(newRow());
		}
    	while(true) {
    		int i = s.readInt();
    		int j = s.readInt();
    		LCCC<R> lc = (LCCC<R>) s.readObject();
    		if(i == -1 && j == -1) {
    			break;
    		} else {
    			entries.get(i).putLast(j, lc);
    		}
    	}
	}

	public CobMatrix<R> extractColumn(int column) {
		SmoothingColumn sc = new SmoothingColumn(1);
		sc.smoothings.set(0, source.smoothings.get(column));
		sc.numbers.set(0, source.numbers.get(column));
		
		source.numbers.remove(column);
		source.smoothings.remove(column);
		source.n--;
		
		CobMatrix<R> result = new CobMatrix<R>(sc, target);
		
		for (int a = 0; a < entries.size(); a++) {
			MatrixRow<LCCC<R>> row = entries.get(a);
			if(row.containsKey(column)) {
				result.entries.get(a).put(0, row.get(column));
				row.remove(column);
			}
			row.decrementIndexesAbove(column);
		}
		
		return result;
	}

	public CobMatrix<R> extractRow(int row) {
		SmoothingColumn sc = new SmoothingColumn(1);
		sc.smoothings.set(0, target.smoothings.get(row));
		sc.numbers.set(0, target.numbers.get(row));
		
		target.numbers.remove(row);
		target.smoothings.remove(row);
		target.n--;
		
		CobMatrix<R> result = new CobMatrix<R>(source, sc);
		result.entries.set(0, entries.remove(row));
		
		return result;
	}
	
	/* (non-Javadoc)
	 * @see org.katlas.JavaKh.algebra.AbstractMatrix#extractColumns(java.util.List)
	 */
	public CobMatrix<R> extractColumns(List<Integer> columns) {
		List<Integer> reverseSortedColumns = new ArrayList<Integer>(columns);
		Collections.sort(reverseSortedColumns);
		Collections.reverse(reverseSortedColumns);
		
		List<Integer> reducedColumns = new ArrayList<Integer>(columns);
		for(int i = 0; i < columns.size(); ++i) {
			int reducedColumn = columns.get(i);
			for(int j = 0; j < i; ++j) {
				if(columns.get(j) < columns.get(i)) --reducedColumn;
			}
			reducedColumns.set(i, reducedColumn);
		}
		
		SmoothingColumn sc = new SmoothingColumn(columns.size());
		int i = 0;
		for(int column : columns) {
			sc.smoothings.set(i, source.smoothings.get(column));
			sc.numbers.set(i, source.numbers.get(column));
			++i;
		}
		
		for(int column : reverseSortedColumns) {
			source.numbers.remove(column);
			source.smoothings.remove(column);
		}
		
		source.n -= columns.size();
		
		CobMatrix<R> result = new CobMatrix<R>(sc, target);
		for (int j = 0; j < entries.size(); j++) {
			MatrixRow<LCCC<R>> row = entries.get(j);
			i = 0;	
			for (int reducedColumn : reducedColumns) {
				if (row.containsKey(reducedColumn)) {
					result.entries.get(j).put(i, row.get(reducedColumn));
					row.remove(reducedColumn);
				}
				row.decrementIndexesAbove(reducedColumn);
				++i;
			}
		}

		assert result.check();
		assert check();
		
		return result;
	}

	public CobMatrix<R> extractRows(List<Integer> rows) {
		List<Integer> reverseSortedRows = new ArrayList<Integer>(rows);
		Collections.sort(reverseSortedRows);
		Collections.reverse(reverseSortedRows);
		
		List<Integer> reducedRows = new ArrayList<Integer>(rows);
		for(int i = 0; i < rows.size(); ++i) {
			int reducedRow = rows.get(i);
			for(int j = 0; j < i; ++j) {
				if(rows.get(j) < rows.get(i)) --reducedRow;
			}
			reducedRows.set(i, reducedRow);
		}
		
		SmoothingColumn sc = new SmoothingColumn(rows.size());
		int i = 0;
		for(int row : rows) {
			sc.smoothings.set(i, target.smoothings.get(row));
			sc.numbers.set(i, target.numbers.get(row));
			++i;
		}
		
		for(int row : reverseSortedRows) {
			target.numbers.remove(row);
			target.smoothings.remove(row);
		}
		
		target.n -= rows.size();
		
		CobMatrix<R> result = new CobMatrix<R>(source, sc);
		i = 0;
		for(int reducedRow : reducedRows) {
			result.entries.set(i, entries.remove(reducedRow));
			++i;
		}

		assert result.check();
		assert check();
		
		return result;
	}

	
	public void insertAfterColumn(int column,
			Matrix<R, Cap, LCCC<R>> extraColumns) {
		// TODO
		throw new UnsupportedOperationException();
	}

	public void insertAfterRow(int row, Matrix<R, Cap, LCCC<R>> extraRows) {
		// TODO
		throw new UnsupportedOperationException();
	}

	public Iterable<? extends MatrixEntry<LCCC<R>>> matrixColumnEntries(
			int column) {
		// TODO Auto-generated method stub
		return null;
	}

	public Iterable<? extends MatrixEntry<LCCC<R>>> matrixRowEntries(int row) {
		// TODO Auto-generated method stub
		return null;
	}

	public int numberOfColumns() {
		return source.n;
	}

	public int numberOfRows() {
		return target.n;
	}


	public Iterator<MatrixEntry<LCCC<R>>> iterator() {
		// TODO Auto-generated method stub
		return null;
	}

	public LCCC<R> getEntry(int row, int column) {
		return entries.get(row).get(column);
	}

	public DirectSum<Cap> source() {
		// TODO Auto-generated method stub
		return null;
	}

	public DirectSum<Cap> target() {
		// TODO Auto-generated method stub
		return null;
	}


}
