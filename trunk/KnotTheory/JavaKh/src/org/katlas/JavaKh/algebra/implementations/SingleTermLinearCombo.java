package org.katlas.JavaKh.algebra.implementations;

import java.util.Collections;
import java.util.Set;

import org.katlas.JavaKh.algebra.LinearCombo;
import org.katlas.JavaKh.algebra.Morphism;
import org.katlas.JavaKh.algebra.Ring;
import org.katlas.JavaKh.algebra.rings.Rings;

public abstract class SingleTermLinearCombo<R extends Ring<R>, O, Mor extends Morphism<O, Mor>, LinearMor extends LinearCombo<R, O, Mor, LinearMor>>
extends	AbstractLinearMorphism<R, O, LinearMor>
implements LinearCombo<R, O, Mor, LinearMor> {

	protected R coefficient;
	protected Mor mor;
	
	public SingleTermLinearCombo(Mor cc, R coefficient) {
		this.mor = cc;
		this.coefficient = coefficient;
	}

	public LinearMor add(Mor m, int num) {
		Rings<R> ring = Rings.current();
		return add(m, ring.createInstance(num));
	}

	@SuppressWarnings("unchecked")
	public LinearMor add(Mor m, R num) {
		if(mor.equals(m)) {
			coefficient = coefficient.add(num);
			return (LinearMor)this;
		} else {
			assert source().equals(m.source());
			assert target().equals(m.target());
			LinearMor result = flexibleZeroLinearCombo(source(), target());
			result = result.add(mor, coefficient);
			result = result.add(m, num);
			return result;
		}
	}

	public R firstCoefficient() {
		return coefficient;
	}

	public Mor firstTerm() {
		return mor;
	}

	public int numberOfTerms() {
		return coefficient.isZero() ? 0 : 1;
	}

	@SuppressWarnings("unchecked")
	public LinearMor add(LinearMor m) {
		if(m.numberOfTerms() == 0) {
			return (LinearMor)this;
		} else if (m.numberOfTerms() == 1) {
			if(mor.equals(m.firstTerm())) {
				R sum = coefficient.add(m.firstCoefficient());
				return (sum.isZero()) ? fixedZeroLinearCombo(source(), target()) : (LinearMor)this;
			} 
		}
		// otherwise, create a new LCCC
		LinearMor result = flexibleZeroLinearCombo(source(), target());
		result = result.add(m);
		return result.add(mor, coefficient);
	}

	public LinearMor multiply(int r) {
		Rings<R> ring = Rings.current();
		return multiply(ring.createInstance(r));
	}

	@SuppressWarnings("unchecked")
	public LinearMor multiply(R r) {
		coefficient = coefficient.multiply(r);
		return (LinearMor)this;
	}

	@SuppressWarnings("unchecked")
	public LinearMor compose(LinearMor m) {
		if(m.numberOfTerms() == 0) {
			return fixedZeroLinearCombo(m.source(), target());
		} else if(m.numberOfTerms() == 1) {
			mor = mor.compose(m.firstTerm());
			coefficient = coefficient.multiply(m.firstCoefficient());
			return (LinearMor)this;
		} else {
			LinearMor result = flexibleZeroLinearCombo(m.source(), target());
			for(Mor mor : m.terms()) {
				result.add(this.mor.compose(mor), coefficient.multiply(m.getCoefficient(mor)));
			}
			return result;
		}
	}

	public R getCoefficient(Mor term) {
		if(term.equals(mor)) {
			return coefficient;
		} else {
			Rings<R> ring = Rings.current();
			return ring.ZERO;
		}
	}


	public Set<Mor> terms() {
		return Collections.singleton(mor);
	}
	
	public O source() {
		return mor.source();
	}

	public O target() {
		return mor.target();
	}

	abstract public LinearMor fixedZeroLinearCombo(O source, O target);
	abstract public LinearMor singleTermLinearCombo(Mor mor, R r);
	abstract public LinearMor flexibleZeroLinearCombo(O source, O target);

	@SuppressWarnings("unchecked")
	public LinearMor compact() {
		if(coefficient.isZero()) {
			return fixedZeroLinearCombo(source(), target());
		} else {
			return (LinearMor)this;
		}
	}

	
}
