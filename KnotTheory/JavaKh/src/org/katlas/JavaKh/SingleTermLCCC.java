package org.katlas.JavaKh;

import java.io.Serializable;

import org.katlas.JavaKh.algebra.Ring;
import org.katlas.JavaKh.algebra.implementations.SingleTermLinearCombo;
import org.katlas.JavaKh.interfaces.CannedCobordism;
import org.katlas.JavaKh.interfaces.LCCC;

/*
 * WARNING
 * SingleTermLCCC is somehow horribly broken. Don't use it.
 * You've been warned.
 */


public class SingleTermLCCC<R extends Ring<R>> extends
		SingleTermLinearCombo<R, Cap, CannedCobordism, LCCC<R>> implements
		LCCC<R>, Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = -28865833270013691L;
	
	transient boolean alreadyReduced;

	public SingleTermLCCC(CannedCobordism cc, R coefficient) {
		super(cc, coefficient);
		assert false;
	}

	public LCCC<R> compose(int start, CannedCobordism cc, int cstart, int nc,
			boolean reverse) {
		CannedCobordism resultCC;
		if (reverse) {
			resultCC = cc.compose(cstart, mor, start, nc);
		} else {
			resultCC = mor.compose(start, cc, cstart, nc);
		}
		LCCC<R> result = singleTermLinearCombo(resultCC, coefficient);
		return result;
	}


	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append(coefficient);
		sb.append(" * ");
		sb.append(mor);
		return sb.toString();
	}

	public LCCC<R> compact() {
		if (coefficient.isZero()) {
			return fixedZeroLinearCombo(source(), target());
		} else {
			return this;
		}
	}

	@Override
	public LCCC<R> fixedZeroLinearCombo(Cap source, Cap target) {
		return new ZeroLCCC<R>(source, target);
	}

	public LCCC<R> singleTermLinearCombo(CannedCobordism cc, R r) {
		return new SingleTermLCCC<R>(cc, r);
	}

	@Override
	public LCCC<R> flexibleZeroLinearCombo(Cap source, Cap target) {
		return new LCCCMap<R>(source, target);
	}

	public LCCC<R> finalizeH() {
		return new LCCCMap<R>(this).finalizeH();
	}

	public LCCC<R> reduce() {
		if(alreadyReduced) return this;
		else return new LCCCMap<R>(this).reduce();
	}

	public LCCC<R> reduceWithH() {
		if(alreadyReduced) return this;
		else return new LCCCMap<R>(this).reduceWithH();
	}

}
