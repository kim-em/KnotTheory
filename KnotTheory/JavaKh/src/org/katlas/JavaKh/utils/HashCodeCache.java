package org.katlas.JavaKh.utils;

import java.lang.ref.WeakReference;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.katlas.JavaKh.Komplex;

public class HashCodeCache<E> implements Cache<E> {

	private static final Log log = LogFactory.getLog(HashCodeCache.class);
	
	private final Map<Integer, WeakReference<E>> hashmap = new HashMap<Integer, WeakReference<E>>();
	
	@Override
	public E cache(E e) {
		int hash = e.hashCode();
		if(hashmap.containsKey(hash)) {
			E result = hashmap.get(hash).get();
			if(result != null) {
//				log.info("Returning object from HashCodeCache. (requested hash: " + hash + ", returned hash: " + result.hashCode() + ", objects " + (e.equals(result) ? "" : "not ") + "equal, objects " + (e == result ? "" : "not ") + "==");
				if(hash != result.hashCode()) {
					log.warn("Hashcode has mysteriously changed.");
				} else if(!e.equals(result)) {
					log.warn("Hash collision!");
//					e.equals(result);
//					e.hashCode();
//					result.hashCode();
					return e;
				}
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
