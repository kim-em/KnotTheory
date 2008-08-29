package org.katlas.JavaKh.algebra;


public interface Matrix<R extends Ring<R>, Obj, Mor extends LinearMorphism<R, Obj, Mor>> extends LinearMorphism<R, DirectSum<Obj>, Matrix<R, Obj, Mor>> {

	public int numberOfRows();
	public int numberOfColumns();
	
	public Matrix<R, Obj, Mor> deleteRow(int row);
	public Matrix<R, Obj, Mor> deleteColumn(int column);
	
	public void insertAfterRow(int row, Matrix<R, Obj, Mor> extraRows);
	public void insertAfterColumn(int column, Matrix<R, Obj, Mor> extraColumns);
	
	public Matrix<R, Obj, Mor> compose(Matrix<R, Obj, Mor> matrix);
	
	public void putEntry(int row, int column, Mor t);
	public void addEntry(int row, int column, Mor t);
	
	public Iterable<? extends MatrixEntry<Mor>> matrixEntries();
	public Iterable<? extends MatrixEntry<Mor>> matrixRowEntries(int row);
	public Iterable<? extends MatrixEntry<Mor>> matrixColumnEntries(int column);
	
}


interface MatrixEntry<T> {
	T getValue();
	int getRow();
	int getColumn();
}

