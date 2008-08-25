package org.katlas.JavaKh.utils;

import gnu.trove.TIntObjectHashMap;

import java.lang.ref.WeakReference;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class HashCodeCache<E> implements Cache<E> {

	private static final Log log = LogFactory.getLog(HashCodeCache.class);
	
	private final TIntObjectHashMap<WeakReference<E>> hashmap = new TIntObjectHashMap<WeakReference<E>>();
	private transient int hits = 0;
	private transient int checks = 0;
	
	@Override
	public E cache(E e) {
		++checks;
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
					++hits;
					return result;
				}
			} else {
//				log.debug("Weak reference lost object");
				hashmap.remove(hash);
			}
		}
//		if(e instanceof Cap) {
//			log.info("Caching cap: " + Arrays.toString(((Cap)e).pairings) + " " + ((Cap)e).ncycles);
//		}
		hashmap.put(hash, new WeakReference<E>(e));
		return e;
	}
	
	@Override
	public void flush() {
		hashmap.clear();
	}

	@Override
	public int size() {
		return hashmap.size();
	}

	public int getNumberOfChecks() { return checks; }
	public int getNumberOfHits() { return hits; }
	
}
