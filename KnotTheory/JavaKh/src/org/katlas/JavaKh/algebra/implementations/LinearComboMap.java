package org.katlas.JavaKh.algebra.implementations;

import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;

import org.katlas.JavaKh.algebra.LinearCombo;
import org.katlas.JavaKh.algebra.Morphism;
import org.katlas.JavaKh.algebra.Ring;
import org.katlas.JavaKh.algebra.implementations.AbstractLinearCombo;
import org.katlas.JavaKh.algebra.rings.Rings;

public abstract class LinearComboMap<R extends Ring<R>, O, Mor extends Morphism<O, Mor>, LinearMor extends LinearCombo<R, O, Mor, LinearMor>>
		extends AbstractLinearCombo<R, O, Mor, LinearMor> implements
		LinearCombo<R, O, Mor, LinearMor> {

	public final SortedMap<Mor, R> coefficients;

	public LinearComboMap(O source, O target) {
		super(source, target);
		coefficients = emptyMap();
	}

	public LinearComboMap(O source, O target, Map<Mor, R> coefficients) {
		super(source, target);
		this.coefficients = unmodifiableMap(new TreeMap<Mor, R>(coefficients));
	}

	public LinearComboMap(Mor mor, R r) {
		super(mor.source(), mor.target());
		SortedMap<Mor, R> coefficients_ = new TreeMap<Mor, R>();
		coefficients_.put(mor, r);
		coefficients = unmodifiableMap(coefficients_);
	}
	
	private SortedMap<Mor, R> emptyMap() {
		return new TreeMap<Mor, R>();
//		return EmptySortedMap.instance();
	}
	
	private SortedMap<Mor, R> unmodifiableMap(SortedMap<Mor, R> map) {
//		return (SortedMap<Mor, R>) Collections.unmodifiableMap(map);
		return map;
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
		assert source().equals(cc.source());
		assert target().equals(cc.target());

		if (coefficients.containsKey(cc)) {
			R newCoefficient = coefficients.get(cc).add(num);
			if (newCoefficient.isZero()) {
				coefficients.remove(cc);
				return compact();
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
		assert source().equals(other.source());
		assert target().equals(other.target());

//		LinearMor result = flexibleZeroLinearCombo(source(), target());
//		assert result instanceof LinearComboMap;
//		LinearComboMap<R, O, Mor, LinearMor> map = (LinearComboMap<R, O, Mor, LinearMor>) result;
//
//		map.coefficients.putAll(coefficients);
//
//		for (Mor cc : other.terms()) {
//			R num = other.getCoefficient(cc);
//			if (map.coefficients.containsKey(cc)) {
//				R newCoefficient = map.coefficients.get(cc).add(num);
//				if (newCoefficient.isZero()) {
//					map.coefficients.remove(cc);
//				} else {
//					coefficients.put(cc, newCoefficient);
//				}
//			} else {
//				if (!num.isZero()) {
//					map.coefficients.put(cc, num);
//				}
//			}
//		}
//		return result.compact();
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

		return ret == null ? null : ret.compact();
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
//
//class EmptySortedMap<K,V> implements SortedMap<K,V> {
//
//	@SuppressWarnings("unchecked")
//	private static final SortedMap INSTANCE = new EmptySortedMap();
//	
//	@SuppressWarnings("unchecked")
//	public static <K,V> SortedMap<K,V> instance() {
//		return INSTANCE;
//	}
//	
//	private EmptySortedMap() {
//		
//	}
//	
//	private Object readResolve() {
//		return instance();
//	}
//	
//	public Comparator<? super K> comparator() {
//		return null;
//	}
//
//	public Set<java.util.Map.Entry<K, V>> entrySet() {
//		return Collections.emptySet();
//	}
//
//	public K firstKey() {
//		throw new NoSuchElementException();
//	}
//
//	public SortedMap<K, V> headMap(K toKey) {
//		return instance();
//	}
//
//	public Set<K> keySet() {
//		return Collections.emptySet();
//	}
//
//	public K lastKey() {
//		throw new NoSuchElementException();
//	}
//
//	public SortedMap<K, V> subMap(K fromKey, K toKey) {
//		return instance();
//	}
//
//	public SortedMap<K, V> tailMap(K fromKey) {
//		return instance();
//	}
//
//	public Collection<V> values() {
//		return Collections.emptySet();
//	}
//
//	public void clear() {
//		return;
//	}
//
//	public boolean containsKey(Object key) {
//		return false;
//	}
//
//	public boolean containsValue(Object value) {
//		return false;
//	}
//
//	public V get(Object key) {
//		return null;
//	}
//
//	public boolean isEmpty() {
//		return true;
//	}
//
//	public V put(K key, V value) {
//		throw new UnsupportedOperationException();
//	}
//
//	public void putAll(Map<? extends K, ? extends V> m) {
//		if(!m.isEmpty()) throw new UnsupportedOperationException();
//	}
//
//	public V remove(Object key) {
//		return null;
//	}
//
//	public int size() {
//		return 0;
//	}
//	
//}