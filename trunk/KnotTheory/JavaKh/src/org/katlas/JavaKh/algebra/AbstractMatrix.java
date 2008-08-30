package org.katlas.JavaKh.algebra;

public abstract class AbstractMatrix<R extends Ring<R>, Obj, Mor extends LinearMorphism<R, Obj, Mor>> implements Matrix<R, Obj, Mor> {

	public void addEntry(int row, int column, Mor t) {
		Mor existingEntry = getEntry(row, column);
		if(existingEntry == null) {
			putEntry(row, column, t);
		} else {
			putEntry(row, column, existingEntry.add(t));
		}
	}

	public Matrix<R, Obj, Mor> extractColumns(Iterable<Integer> columns) {
		Matrix<R, Obj, Mor> result = null;
		for(int column : columns) {
			if(result == null) {
				result = extractColumn(column);
			} else {
				result.insertAfterColumn(result.numberOfColumns(), extractColumn(column));
			}
		}
		return result;
	}

	public Matrix<R, Obj, Mor> extractRows(Iterable<Integer> rows) {
		Matrix<R, Obj, Mor> result = null;
		for(int row : rows) {
			if(result == null) {
				result = extractRow(row);
			} else {
				result.insertAfterRow(result.numberOfRows(), extractRow(row));
			}
		}
		return result;
	}

	
	
}
