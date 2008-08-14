package org.katlas.JavaKh.utils;
import java.io.Serializable;
import java.util.Collection;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

/**
 * 
 */

public class AlwaysEmptyMap<K,V> implements Map<K,V>, Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 2680627961266068327L;

	@Override
	public void clear() {
		return;
	}

	@Override
	public boolean containsKey(Object key) {
		return false;
	}

	@Override
	public boolean containsValue(Object value) {
		return false;
	}

	@Override
	public Set<Entry<K, V>> entrySet() {
		return new AlwaysEmptySet<Entry<K,V>>();
	}

	@Override
	public V get(Object key) {
		return null;
	}

	@Override
	public boolean isEmpty() {
		return true;
	}

	@Override
	public Set<K> keySet() {
		return new AlwaysEmptySet<K>();
	}

	@Override
	public V put(K key, V value) {
		return null;
	}

	@Override
	public void putAll(Map<? extends K, ? extends V> m) {
		
	}

	@Override
	public V remove(Object key) {
		return null;
	}

	@Override
	public int size() {
		return 0;
	}

	@Override
	public Collection<V> values() {
		return new AlwaysEmptySet<V>();
	}
	
}