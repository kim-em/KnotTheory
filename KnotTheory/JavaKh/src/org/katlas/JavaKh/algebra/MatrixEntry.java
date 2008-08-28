package org.katlas.JavaKh.algebra;

@SuppressWarnings("hiding")
public abstract class MatrixEntry<L, T> {
	MatrixEntry(L row, L column) {
		this.row = row;
		this.column = column;
	}

	T value;
	L row, column;
}
