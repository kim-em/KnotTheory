package org.katlas.JavaKh.LCCC;

import java.util.AbstractMap;
import java.util.Collections;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

public class SizeRarelyMoreThanOneMap<K, V> extends AbstractMap<K, V>
		implements Map<K, V> {
	/* extends AbstractMap just for the implementation of values() */
	
	private Map<K, V> map;
	private K firstKey;
	private V firstValue;

	protected Map<K, V> newMap() {
		return new TreeMap<K, V>();
	}

	public void clear() {
		map = null;
		firstKey = null;
		firstValue = null;
	}

	public boolean containsKey(Object key) {
		return (firstKey != null && firstKey.equals(key))
				|| (map != null && map.containsKey(key));
	}

	public boolean containsValue(Object value) {
		return (firstValue != null && firstValue.equals(value))
				|| (map != null && map.containsValue(value));
	}

	public V get(Object key) {
		if (firstKey != null && firstKey.equals(key)) {
			return firstValue;
		} else if (map != null) {
			return map.get(key);
		} else {
			return null;
		}
	}

	public boolean isEmpty() {
		return firstKey == null && (map == null || map.isEmpty());
	}

	public V put(K key, V value) {
		if (value == null) {
			return remove(key);
		}

		if (firstKey == null) {
			if (map == null) {
				firstKey = key;
				firstValue = value;
				return null;
			} else {
				if (map.containsKey(key)) {
					return map.put(key, value);
				} else {
					firstKey = key;
					firstValue = value;
					return null;
				}
			}
			// unreachable
		} else {
			if (firstKey.equals(key)) {
				V result = firstValue;
				firstValue = value;
				return result;
			} else {
				if (map == null) {
					map = newMap();
				}
				return map.put(key, value);
			}
			// unreachable
		}
	}

	public void putAll(Map<? extends K, ? extends V> otherMap) {
		if (otherMap.size() > 1) {
			if (map == null) {
				map = newMap();
			}
			map.putAll(otherMap);
			return;
		} else {
			for (Map.Entry<? extends K, ? extends V> entry : otherMap
					.entrySet()) {
				put(entry.getKey(), entry.getValue());
			}
			return;
		}
	}

	public V remove(Object key) {
		if (firstKey != null && firstKey.equals(key)) {
			firstKey = null;
			V result = firstValue;
			firstValue = null;
			return result;
		} else if (map != null) {
			V result = map.remove(key);
			if (map.isEmpty()) {
				map = null;
			}
			return result;
		} else {
			return null;
		}
	}

	public int size() {
		return (firstKey == null ? 0 : 1) + (map == null ? 0 : map.size());
	}

	public Set<K> keySet() {
		if (firstKey == null) {
			if (map == null) {
				return Collections.emptySet();
			} else {
				return map.keySet();
			}
		} else {
			if (map == null) {
				return Collections.singleton(firstKey);
			} else {
				Set<K> keys = new TreeSet<K>(map.keySet());
				keys.add(firstKey);
				return keys;
			}
		}
	}

	public Set<Map.Entry<K, V>> entrySet() {
		assert false; // this code sucks, hope that it never actually runs.

		if (firstKey == null) {
			if (map == null) {
				return Collections.emptySet();
			} else {
				return map.entrySet();
			}
		} else {
			if (map == null) {
				return Collections.singletonMap(firstKey, firstValue)
						.entrySet();
			} else {
				Set<Map.Entry<K, V>> entries = new TreeSet<Map.Entry<K, V>>(map
						.entrySet());
				entries.addAll(Collections.singletonMap(firstKey, firstValue)
						.entrySet());
				return entries;
			}
		}
	}

}
