package org.katlas.JavaKh;

import java.io.Serializable;

import org.katlas.JavaKh.algebra.Ring;
import org.katlas.JavaKh.algebra.implementations.ZeroLinearCombo;
import org.katlas.JavaKh.interfaces.CannedCobordism;
import org.katlas.JavaKh.interfaces.LCCC;

public class ZeroLCCC<R extends Ring<R>> extends
		ZeroLinearCombo<R, Cap, CannedCobordism, LCCC<R>> implements
		LCCC<R>, Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = -985131448758667387L;

	ZeroLCCC(Cap source, Cap target) {
		super(source, target);
	}

	public LCCC<R> finalizeH() {
		return this;
	}

	public LCCC<R> reduce() {
		return this;
	}

	public LCCC<R> reduceWithH() {
		return this;
	}
	
	public LCCC<R> compose(int start, CannedCobordism cc, int cstart, int nc,
			boolean reverse) {
		assert false;
		throw new UnsupportedOperationException();
//		return new ZeroLCCC<R>(source().compose(start, cc.source(),
//				cstart, nc), target().compose(start, cc.target(), cstart, nc));
	}

	@Override
	public LCCC<R> fixedZeroLinearCombo(Cap source, Cap target) {
		return new ZeroLCCC<R>(source, target);
	}

	@Override
	public LCCC<R> flexibleZeroLinearCombo(Cap source, Cap target) {
		return new LCCCMap<R>(source, target);
	}

	@Override
	public LCCC<R> singleTermLinearCombo(CannedCobordism mor, R r) {
		/*
		 * WARNING
		 * SingleTermLCCC is somehow horribly broken. Don't use it.
		 * You've been warned.
		 */

//		return new SingleTermLCCC<R>(mor, r);
		LCCC<R> result = new LCCCMap<R>(mor.source(), mor.target());
		return result.add(mor, r);
	}

}