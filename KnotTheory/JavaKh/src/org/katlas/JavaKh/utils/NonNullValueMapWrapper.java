package org.katlas.JavaKh.utils;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

public class NonNullValueMapWrapper<K, V> implements Map<K, V> {

	private final Map<K, V> internalMap;

	public NonNullValueMapWrapper(Map<K, V> internalMap) {
		for(V v : internalMap.values()) {
			if(v == null) {
				throw new IllegalArgumentException("Cannot wrap a map which already contains null values.");
			}
		}
		this.internalMap = internalMap;
	}
	
	@Override
	public void clear() {
		internalMap.clear();
	}

	@Override
	public boolean containsKey(Object key) {
		return internalMap.containsKey(key);
	}

	@Override
	public boolean containsValue(Object value) {
		return internalMap.containsValue(value);
	}

	@Override
	public Set<java.util.Map.Entry<K, V>> entrySet() {
		return internalMap.entrySet();
	}

	@Override
	public V get(Object key) {
		return internalMap.get(key);
	}

	@Override
	public boolean isEmpty() {
		return internalMap.isEmpty();
	}

	@Override
	public Set<K> keySet() {
		return internalMap.keySet();
	}

	@Override
	public V put(K key, V value) {
		if(value == null) {
			throw new IllegalArgumentException("Tried to add a null value.");
		}
		return internalMap.put(key, value);
	}

	@Override
	public void putAll(Map<? extends K, ? extends V> m) {
		for(K k : m.keySet()) {
			put(k, m.get(k));
		}
	}

	@Override
	public V remove(Object key) {
		return internalMap.remove(key);
	}

	@Override
	public int size() {
		return internalMap.size();
	}

	@Override
	public Collection<V> values() {
		return internalMap.values();
	}

}
