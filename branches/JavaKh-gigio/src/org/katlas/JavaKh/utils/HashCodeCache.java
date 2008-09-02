package org.katlas.JavaKh.utils;

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class HashCodeCache<E> implements Cache<E> {

	private static final Log log = LogFactory.getLog(HashCodeCache.class);
	
//	private final TIntObjectHashMap<WeakReference<E>> hashmap = new TIntObjectHashMap<WeakReference<E>>();
//	private final TIntObjectHashMap<E> hashmap = new TIntObjectHashMap<E>();
//	private final RedBlackIntegerTree<E> hashmap = new RedBlackIntegerTree<E>();
	private final Map<Integer, E> hashmap = new HashMap<Integer, E>();
	private transient long hits = 0;
	private transient long checks = 0;
	
	public synchronized E cache(E e) {
		++checks;
		int hash = e.hashCode();
		if(hashmap.containsKey(hash)) {
			E result = hashmap.get(hash);
			if(result != null) {
				if(hash != result.hashCode()) {
					log.info("Hashcode has mysteriously changed.");
				} else if(!e.equals(result)) {
//					log.warn("Hash collision!");
				} else {
					++hits;
					return result;
				}
			} else {
//				log.debug("Weak reference lost object");
				hashmap.remove(hash);
			}
		}
		hashmap.put(hash, e);
		return e;
	}
	
	public synchronized void flush() {
		hashmap.clear();
	}

	public int size() {
		return hashmap.size();
	}

	public long getNumberOfChecks() { return checks; }
	public long getNumberOfHits() { return hits; }
	
}
