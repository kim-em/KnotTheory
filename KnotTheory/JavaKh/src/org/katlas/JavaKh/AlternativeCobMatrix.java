package org.katlas.JavaKh;

import org.katlas.JavaKh.algebra.MatrixEntry;
import org.katlas.JavaKh.algebra.Ring;
import org.katlas.JavaKh.algebra.SparseMatrix;
import org.katlas.JavaKh.rows.MatrixRow;

public class AlternativeCobMatrix<R extends Ring<R>> extends SparseMatrix<R, Obj, LCCC<R>> {

	final SmoothingColumn source;
	final SmoothingColumn target;
	
	public AlternativeCobMatrix(SmoothingColumn source, SmoothingColumn target) {
		super(target.n, source.n);
		this.source = source;
		this.target = target;
	}

	public AlternativeCobMatrix(CobMatrix<R> matrix) {
		super(matrix.target.n, matrix.source.n);
		this.source = matrix.source;
		this.target = matrix.target;
		
		for(int i = 0; i < matrix.target.n; ++i) {
			MatrixRow<LCCC<R>> row = matrix.entries.get(i);
			for(int j : row.keys()) {
				putEntry(i, j, row.get(j));
			}
		}
	}
	
	public CobMatrix<R> asCobMatrix() {
		CobMatrix<R> result = new CobMatrix<R>(source, target);
		for(MatrixEntry<LCCC<R>> entry : this) {
			result.putEntry(entry.getRow(), entry.getColumn(), entry.getValue());
		}
		return result;
	}

	public void reduce() {
		for(MatrixEntry<LCCC<R>> entry : this) {
			entry.setValue(entry.getValue().reduce());
		}
	}
}
