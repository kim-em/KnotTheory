package org.katlas.JavaKh;

import org.katlas.JavaKh.algebra.LinearCombo;
import org.katlas.JavaKh.algebra.Ring;

public interface LCCC<R extends Ring<R>> extends LinearCombo<R, Cap, ICannedCobordism, LCCC<R>> {

	public LCCC<R> compose(int start, ICannedCobordism cc, int cstart, int nc,
			boolean reverse);

	public LCCC<R> reduce();

	public LCCC<R> reduceWithH();

	public LCCC<R> finalizeH();

	
}
