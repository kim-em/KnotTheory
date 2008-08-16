package org.katlas.JavaKh.utils;

import java.lang.ref.WeakReference;
import java.util.HashMap;
import java.util.Map;

public class HashCodeCache<E> implements Cache<E> {

	private final Map<Integer, WeakReference<E>> hashmap = new HashMap<Integer, WeakReference<E>>();
	
	@Override
	public E cache(E e) {
		int hash = e.hashCode();
		if(hashmap.containsKey(hash)) {
			E result = hashmap.get(hash).get();
			if(result != null) {
				return result;
			}
		}
		hashmap.put(hash, new WeakReference<E>(e));
		return e;
	}
	
	@Override
	public void flush() {
		hashmap.clear();
	}

	
	
}
