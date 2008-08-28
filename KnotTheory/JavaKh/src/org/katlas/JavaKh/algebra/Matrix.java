package org.katlas.JavaKh.algebra;

import java.util.List;


public interface Matrix<L extends Comparable<L>, R, T extends Algebra<R, T>> extends Algebra<R, Matrix<L, R, T>> {

	public int numberOfRows();
	public int numberOfColumns();
	
	public List<L> rows();
	public List<L> columns();
	
	public Matrix<L, R, T> deleteRow(L row);
	public Matrix<L, R, T> deleteColumn(L row);
	
	public void insertAfterRow(L row, Matrix<L, R, T> extraRows);
	public void insertAfterColumn(L column, Matrix<L, R, T> extraColumns);
	
	public Matrix<L, R, T> compose(Matrix<L, R, T> matrix);
	
	public void putEntry(L row, L column, T t);
	public void addEntry(L row, L column, T t);
	
	public Iterable<MatrixEntry<L, T>> matrixEntries();
	public Iterable<MatrixEntry<L, T>> matrixRowEntries(L row);
	public Iterable<MatrixEntry<L, T>> matrixColumnEntries(L column);
	
}

