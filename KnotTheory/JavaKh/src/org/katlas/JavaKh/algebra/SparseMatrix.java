package org.katlas.JavaKh.algebra;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.TreeMap;


import net.tqft.iterables.AbstractIterator;
import net.tqft.iterables.IterableBundle;

public class SparseMatrix<R, T extends Algebra<R, T>> implements
		Matrix<L, R, T> {

	List<L> rows, columns;
	Map<L, SparseMatrixEntry<T>> initialRowEntries, initialColumnEntries;

	public SparseMatrix(List<L> rows, List<L> columns) {
		this.rows = rows;
		this.columns = columns;
		initialRowEntries = new TreeMap<L, SparseMatrixEntry<T>>();
		initialColumnEntries = new TreeMap<L, SparseMatrixEntry<T>>();
	}

	public Matrix<L, R, T> compose(Matrix<L, R, T> matrix) {
		List<L> resultRows = rows();
		List<L> resultColumns = matrix.columns();
		Matrix<L, R, T> result = new SparseMatrix<R, T>(resultRows, resultColumns);
		for(L row : resultRows) {
			for(L column : resultColumns) {
				T t = null;
				Iterator<MatrixEntry<L,T>> columnIterator = matrix.matrixColumnEntries(column).iterator();
				if(!columnIterator.hasNext()) continue;
				MatrixEntry<L,T> columnEntry = columnIterator.next();
				for(MatrixEntry<L,T> rowEntry : matrixRowEntries(row)) {
					while(columnEntry.row.compareTo(rowEntry.column) < 0) {
						if(columnIterator.hasNext()) {
							columnEntry = columnIterator.next();
						} else {
							columnEntry = null;
						}
					}
					if(columnEntry == null) break;
					if(columnEntry.row.compareTo(rowEntry.column) == 0) {
						// found something!
						T product = rowEntry.value.compose(columnEntry.value);
						if(t == null) {
							t = product;
						} else {
							t = t.add(product);
						}
					}
				}
				if(t != null) {
					result.putEntry(row, column, t);
				}
			}
		}
		
		return result;
	}

	public Matrix<L, R, T> deleteColumn(L column) {
		if (!columns.contains(column)) {
			throw new NoSuchElementException();
		}

		SparseMatrix<R, T> result = new SparseMatrix<R, T>(rows,
				Collections.singletonList(column));
		SparseMatrixEntry<T> columnEntry = initialColumnEntries.get(column);
		columns.remove(column);

		if (columnEntry != null) {
			result.initialColumnEntries.put(column, columnEntry);
			while (columnEntry != null) {
				if (initialRowEntries.get(columnEntry.row) == columnEntry) {
					initialRowEntries.put(columnEntry.row, columnEntry.right);
				}
				if (columnEntry.left != null) {
					columnEntry.left.right = columnEntry.right;
				}
				if (columnEntry.right != null) {
					columnEntry.right.left = columnEntry.left;
				}
				columnEntry.left = null;
				columnEntry.right = null;
				result.initialRowEntries.put(columnEntry.row, columnEntry);
				columnEntry = columnEntry.down;
			}
		}

		return result;
	}

	public Matrix<L, R, T> deleteRow(L row) {
		if (!rows.contains(row)) {
			throw new NoSuchElementException();
		}

		SparseMatrix<R, T> result = new SparseMatrix<R, T>(columns,
				Collections.singletonList(row));
		SparseMatrixEntry<T> rowEntry = initialRowEntries.get(row);
		rows.remove(row);

		if (rowEntry != null) {
			result.initialRowEntries.put(row, rowEntry);
			while (rowEntry != null) {
				if (initialColumnEntries.get(rowEntry.column) == rowEntry) {
					initialColumnEntries.put(rowEntry.column, rowEntry.down);
				}
				if (rowEntry.up != null) {
					rowEntry.up.down = rowEntry.down;
				}
				if (rowEntry.down != null) {
					rowEntry.down.up = rowEntry.up;
				}
				rowEntry.up = null;
				rowEntry.down = null;
				result.initialColumnEntries.put(rowEntry.column, rowEntry);
				rowEntry = rowEntry.right;
			}
		}

		return result;
	}

	public void insertAfterColumn(L column,
			Matrix<L, R, T> extraColumns) {
		// TODO Auto-generated method stub

	}

	public void insertAfterRow(L row, Matrix<L, R, T> extraRows) {
		// TODO Auto-generated method stub

	}

	public int numberOfColumns() {
		return columns.size();
	}

	public int numberOfRows() {
		return rows.size();
	}

	public void putEntry(L row, L column, T t) {
		put(row, column, t, false);
	}

	public void addEntry(L row, L column, T t) {
		put(row, column, t, true);
	}

	private void put(L row, L column, T t, boolean addition) {
		SparseMatrixEntry<T> newMatrixEntry = new SparseMatrixEntry<T>(row, column);

		boolean columnRequiresLinking = true;

		{
			SparseMatrixEntry<T> currentRowEntry = initialRowEntries.get(row);
			if (currentRowEntry == null) {
				// the whole row is empty
				initialRowEntries.put(row, newMatrixEntry);
			} else {
				while (currentRowEntry.right != null
						&& currentRowEntry.column.compareTo(column) < 0) {
					currentRowEntry = currentRowEntry.right;
				}
				if (currentRowEntry.column.compareTo(column) == 0) {
					// the matrix entry already exists
					columnRequiresLinking = false;
					if (addition) {
						currentRowEntry.value.add(t);
					} else {
						currentRowEntry.value = t;
					}
				} else {
					// the matrix entry doesn't exist; we need to link it in
					// both the row and the column
					newMatrixEntry.value = t;
					newMatrixEntry.left = currentRowEntry;
					newMatrixEntry.right = currentRowEntry.right;
					if (currentRowEntry.right != null) {
						currentRowEntry.right.left = newMatrixEntry;
					}
					currentRowEntry.right = newMatrixEntry;
				}
			}
		}
		if (columnRequiresLinking) {
			SparseMatrixEntry<T> currentColumnEntry = initialColumnEntries
					.get(column);
			if (currentColumnEntry == null) {
				initialColumnEntries.put(column, newMatrixEntry);
			} else {
				while (currentColumnEntry.down != null
						&& currentColumnEntry.row.compareTo(row) < 0) {
					currentColumnEntry = currentColumnEntry.down;
				}
				if (currentColumnEntry.row.compareTo(row) == 0) {
					throw new AssertionError(
							"Failed to find the matrix entry in a row, but it later turned up in a column!");
				} else {
					newMatrixEntry.up = currentColumnEntry;
					newMatrixEntry.down = currentColumnEntry.down;
					if (currentColumnEntry.down != null) {
						currentColumnEntry.down.up = newMatrixEntry;
					}
					currentColumnEntry.down = newMatrixEntry;
				}
			}
		}

	}

	// WARNING: adds in place.
	public Matrix<L, R, T> add(Matrix<L, R, T> m) {
		// this is a pretty crappy implementation!
		for(MatrixEntry<L, T> entry : m.matrixEntries()) {
			addEntry(entry.row, entry.column, entry.value);
		}
		return this;
	}

	// WARNING: multiplies in place.
	public Matrix<L, R, T> multiply(R r) {
		for(L row : rows) {
			SparseMatrixEntry<T> entry = initialRowEntries.get(row);
			while(entry != null) {
				entry.value = entry.value.multiply(r);
				entry = entry.right;
			}
		}
		return this;
	}

	public Iterable<MatrixEntry<L, T>> matrixEntries() {
		return new IterableBundle<L, MatrixEntry<L, T>>(rows) {
			@Override
			protected Iterable<MatrixEntry<L, T>> buildNewFibreIterable(
					L row) {
				return matrixRowEntries(row);
			}
		};
	}

	public Iterable<MatrixEntry<L, T>> matrixColumnEntries(final L column) {
		return new Iterable<MatrixEntry<L, T>>() {
			public Iterator<MatrixEntry<L, T>> iterator() {
				return new AbstractIterator<MatrixEntry<L, T>>() {
					SparseMatrixEntry<T> entry = initialColumnEntries.get(column);
					public boolean hasNext() {
						return entry != null;
					}
					protected MatrixEntry<L,T> returnNext() {
						MatrixEntry<L,T> result = entry;
						entry = entry.down;
						return result;
					}		
				};
			}
		};
	}

	public Iterable<MatrixEntry<L, T>> matrixRowEntries(final L row) {
		return new Iterable<MatrixEntry<L, T>>() {
			public Iterator<MatrixEntry<L,T>> iterator() {
				return new AbstractIterator<MatrixEntry<L,T>>() {
					SparseMatrixEntry<T> entry = initialRowEntries.get(row);
					public boolean hasNext() {
						return entry != null;
					}
					protected MatrixEntry<L,T> returnNext() {
						MatrixEntry<L,T> result = entry;
						entry = entry.right;
						return result;
					}		
				};
			}
		};
	}

	public List<L> rows() {
		return new ArrayList<L>(rows);
	}
	
	public List<L> columns() {
		return new ArrayList<L>(columns);
	}


	
}

class L implements Comparable<L> {
	
	static int counter = 0;
	private final int index;

	L() {
		index = ++counter;
	}

	public int compareTo(L label) {
		return index - label.index;
	}
}


class SparseMatrixEntry<T> extends MatrixEntry<L, T> {
	SparseMatrixEntry(L row, L column) {
		super(row, column);
	}
		
	SparseMatrixEntry<T> up, down, left, right;
}