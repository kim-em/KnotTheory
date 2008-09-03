package org.katlas.JavaKh.algebra.implementations;

import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;

import org.katlas.JavaKh.algebra.LinearCombo;
import org.katlas.JavaKh.algebra.Morphism;
import org.katlas.JavaKh.algebra.Ring;
import org.katlas.JavaKh.algebra.rings.Rings;

public abstract class LinearComboMap<R extends Ring<R>, O, Mor extends Morphism<O, Mor>, LinearMor extends LinearCombo<R, O, Mor, LinearMor>>
		extends AbstractLinearCombo<R, O, Mor, LinearMor> implements
		LinearCombo<R, O, Mor, LinearMor> {

	protected final SortedMap<Mor, R> coefficients;

	public LinearComboMap(O source, O target) {
		super(source, target);
		coefficients = new TreeMap<Mor, R>();
	}

	public LinearComboMap(LinearComboMap<R, O, Mor, LinearMor> lc) {
		super(lc.source, lc.target);
		coefficients = new TreeMap<Mor, R>(lc.coefficients);
	}

	public Mor firstTerm() {
		return coefficients.firstKey();
	}

	public R firstCoefficient() {
		return coefficients.get(firstTerm());
	}

	public R getCoefficient(Mor term) {
		return coefficients.get(term);
	}

	public Set<Mor> terms() {
		return coefficients.keySet();
	}

	public int numberOfTerms() {
		return coefficients.size();
	}

	public LinearMor add(Mor m, int num) {
		Rings<R> ring = Rings.current();
		return add(m, ring.createInstance(num));
	}

	@SuppressWarnings("unchecked")
	public LinearMor add(Mor cc, R num) {

		if (coefficients.containsKey(cc)) {
			R newCoefficient = coefficients.get(cc).add(num);
			if (newCoefficient.isZero()) {
				coefficients.remove(cc);
				if (numberOfTerms() == 0) {
					return fixedZeroLinearCombo(source(), target());
				} else {
					return (LinearMor) this;
				}
			} else {
				coefficients.put(cc, newCoefficient);
				return (LinearMor) this;
			}
		} else {

			if (!num.isZero()) {
				coefficients.put(cc, num);
			}
			return (LinearMor) this;
		}
	}

	public LinearMor add(LinearMor other) {
		if (other != null) {
			for (Mor cc : other.terms()) {
				/* this = */ add(cc, other.getCoefficient(cc));
			}
		}
		return (LinearMor) this.compact();
	}

	@SuppressWarnings("unchecked")
	public LinearMor multiply(R num) {
		if (num.isZero()) {
			coefficients.clear();
			return null;
		} else {
			for (Mor cc : coefficients.keySet()) {
				coefficients.put(cc, coefficients.get(cc).multiply(num));
			}
			return (LinearMor) this;
		}
	}

	public LinearMor compose(LinearMor other) { // vertical composition
		if (other == null || isZero() || other.isZero()) {
			return null;
		}

		assert source().equals(other.target());
		LinearMor ret = flexibleZeroLinearCombo(other.source(), target());

		for (Mor cc : terms()) {
			for (Mor occ : other.terms()) {
				ret = ret.add(cc.compose(occ), getCoefficient(cc).multiply(
						other.getCoefficient(occ)));
			}
		}

		return ret.compact();
	}

	public O source() {
		return source;
	}

	public O target() {
		return target;
	}

	abstract public LinearMor singleTermLinearCombo(Mor mor, R r);

	abstract public LinearMor fixedZeroLinearCombo(O source, O target);

	abstract public LinearMor flexibleZeroLinearCombo(O source, O target);

}