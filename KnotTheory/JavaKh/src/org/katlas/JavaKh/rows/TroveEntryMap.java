package org.katlas.JavaKh.rows;

import gnu.trove.TIntObjectHashMap;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import net.tqft.iterables.AbstractIterator;

public class TroveEntryMap<F> implements MatrixRow<F>, Serializable {

	/**
		 * 
		 */
	private static final long serialVersionUID = -230926858992553476L;

	TIntObjectHashMap<F> map = new TIntObjectHashMap<F>(2);

	public void compact() {
		map.compact();
	}

	public boolean containsKey(int key) {
		return map.contains(key);
	}

	public F get(int key) {
		return map.get(key);
	}

	public Iterable<Integer> keys() {
		return new Iterable<Integer>() {
			final int[] keys = map.keys();

			public Iterator<Integer> iterator() {
				return new AbstractIterator<Integer>() {
					int i = 0;

					public boolean hasNext() {
						return i < keys.length;
					}

					public Integer returnNext() {
						return keys[i++];
					}
				};
			}
		};
	}

	public void put(int key, F f) {
		map.put(key, f);
	}

	public void remove(int key) {
		map.remove(key);
	}

	public void decrementIndexesAbove(int key) {
		List<Integer> targetIndexes = new ArrayList<Integer>();
		for (int k : map.keys()) {
			if (k > key) {
				targetIndexes.add(k);
			}
		}
		Collections.sort(targetIndexes);
		for (int k : targetIndexes) {
			map.put(k - 1, map.get(k));
			map.remove(k);
		}

	}

	public void clear() {
		map.clear();
	}

}