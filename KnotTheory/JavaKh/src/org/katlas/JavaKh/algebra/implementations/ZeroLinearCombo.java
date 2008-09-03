package org.katlas.JavaKh.algebra.implementations;

import java.util.Collections;
import java.util.Set;

import org.katlas.JavaKh.algebra.LinearCombo;
import org.katlas.JavaKh.algebra.Morphism;
import org.katlas.JavaKh.algebra.Ring;
import org.katlas.JavaKh.algebra.rings.Rings;

public abstract class ZeroLinearCombo<R extends Ring<R>, O, Mor extends Morphism<O, Mor>, LinearMor extends LinearCombo<R, O, Mor, LinearMor>>
extends	AbstractLinearCombo<R, O, Mor, LinearMor>
implements LinearCombo<R, O, Mor, LinearMor> {
	
	protected ZeroLinearCombo(O source, O target) {
		super(source, target);
	}

	public R firstCoefficient() {
		return null;
	}

	public Mor firstTerm() {
		return null;
	}

	public int numberOfTerms() {
		return 0;
	}

	public boolean isZero() {
		return true;
	}
	
	public LinearMor add(LinearMor m) {
		return m;
	}

	@SuppressWarnings("unchecked")
	public LinearMor multiply(int r) {
		return (LinearMor)this;
	}

	@SuppressWarnings("unchecked")
	public LinearMor multiply(R r) {
		return (LinearMor)this;
	}

	public LinearMor compose(LinearMor m) {
		return fixedZeroLinearCombo(m.source(), target);
	}

	public O source() {
		return source;
	}

	public O target() {
		return target;
	}

	public LinearMor add(Mor mor, int r) {
		Rings<R> ring = Rings.current();
		return add(mor, ring.createInstance(r));
	}

	public LinearMor add(Mor mor, R r) {
		return singleTermLinearCombo(mor, r);
	}

	public R getCoefficient(Mor term) {
		Rings<R> ring = Rings.current();
		return ring.ZERO;
	}

	public Set<Mor> terms() {
		return Collections.emptySet();
	}
	
	public abstract LinearMor singleTermLinearCombo(Mor mor, R r);
	public abstract LinearMor fixedZeroLinearCombo(O source, O target);
	public abstract LinearMor flexibleZeroLinearCombo(O source, O target);

	@SuppressWarnings("unchecked")
	@Override
	public LinearMor compact() {
		return (LinearMor)this;
	}

}