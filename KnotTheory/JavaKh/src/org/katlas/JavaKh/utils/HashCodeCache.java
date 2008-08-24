package org.katlas.JavaKh.utils;

import gnu.trove.TIntObjectHashMap;

import java.lang.ref.WeakReference;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class HashCodeCache<E> implements Cache<E> {

	private static final Log log = LogFactory.getLog(HashCodeCache.class);
	
	private final TIntObjectHashMap<WeakReference<E>> hashmap = new TIntObjectHashMap<WeakReference<E>>();
	
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
				} else {
					return result;
				}
			} else {
				hashmap.remove(hash);
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
