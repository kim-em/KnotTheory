package org.katlas.JavaKh.algebra;

import java.util.SortedMap;
import java.util.TreeMap;


public class LinearComboMap<R extends Ring<R>, O, Mor extends Morphism<O, Mor>, LinearMor extends LinearCombo<R, O, Mor, LinearMor>>
	extends	AbstractLinearMorphism<R, O, LinearMor>
	implements LinearCombo<R, O, Mor, LinearMor> {

	protected final SortedMap<Mor, R> coefficients;
	protected final O source, target;

	public LinearComboMap(O source, O target) {
		this.source = source;
		this.target = target;
		coefficients = new TreeMap<Mor, R>();
	}

	public Mor firstTerm() {
		return coefficients.firstKey();
	}

	public R firstCoefficient() {
		return coefficients.get(firstTerm());
	}

	public int numberOfTerms() {
		return coefficients.size();
	}

	@SuppressWarnings("unchecked")
	public void add(Mor m, int num) {
		add(m, (R)Rings.createInstance(num));
	}
	
	public void add(Mor cc, R num) {
	
		if (coefficients.containsKey(cc)) {
			R newCoefficient = coefficients.get(cc).add(num);
			if (newCoefficient.isZero()) {
				coefficients.remove(cc);
			} else {
				coefficients.put(cc, newCoefficient);
			}
			return;
		}
	
		if (num.isZero()) {
			return;
		}
	
		coefficients.put(cc, num);
	}

	@SuppressWarnings("unchecked")
	public LinearMor add(LinearMor other) {
		if (other != null) {
			if(other instanceof LinearComboMap) {
				LinearComboMap<R, O, Mor, LinearMor> otherMap = (LinearComboMap<R, O, Mor, LinearMor>) other;
				for (Mor cc : otherMap.coefficients.keySet()) {
					add(cc, otherMap.coefficients.get(cc));
				}
			} else {
				throw new UnsupportedOperationException();
			}
		}
		return (LinearMor) this;
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
		if (other == null || numberOfTerms() == 0 || other.numberOfTerms() == 0) {
			return null;
		}
	
		if (other instanceof LinearComboMap) {
			LinearComboMap<R, O, Mor, LinearMor> otherMap = (LinearComboMap<R, O, Mor, LinearMor>) other;
			assert source().equals(otherMap.target());
			LinearMor ret = zeroLinearCombo(otherMap.source(), target());
	
			for (Mor cc : coefficients.keySet()) {
				for (Mor occ : otherMap.coefficients.keySet()) {
					ret.add(cc.compose(occ), coefficients.get(cc).multiply(
							otherMap.coefficients.get(occ)));
				}
			}
	
			return ret;
		} else {
			throw new UnsupportedOperationException();
		}
	}

	public O source() {
		return source;
	}

	public O target() {
		return target;
	}

	@SuppressWarnings("unchecked")
	public LinearMor zeroLinearCombo(O source, O target) {
		// You have to override this method if you want to subclass.
		// Unless LinearMor is just LinearComboMap, this cast really will fail!
		return (LinearMor)(new LinearComboMap<R, O, Mor, LinearMor>(source, target));
	}


}